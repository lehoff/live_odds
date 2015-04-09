-module(lo_odds).

% -define(LOG(LEVEL, REPORT), io:format("~p: ~p~n", [LEVEL, REPORT]) ).
-define(LOG(LEVEL, REPORT), LEVEL=LEVEL, REPORT=REPORT ).  % don't log, also don't warn about unused symbols.

-behaviour(gen_server).

-export([start_link/0, start_link/2, position/2, stop/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).


start_link() ->
	start_link([], 0).

start_link(Java_Options, Tracelevel) when is_list(Java_Options), Tracelevel >= 0 ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Java_Options, Tracelevel], []).

position(Match, Position) ->
	gen_server:call(?MODULE, {new_position, Match, Position}, infinity).

stop() ->
	gen_server:call(?MODULE, stop, infinity).


% Here follow the gen_server callback functions.

-record(state, {
	port,
	mbox, % The Odds Node gets messages sent to this Mbox.
	from, % The client where results need to be sent back to.
	infotext = [], % Stores up any info text coming from the Odds Node.
	infoline = [] % Builds up complete lines of info text.
}).
-define(MAX_INFOTEXT_LINES, 1000).

init([Java_Options, Tracelevel]) ->
	process_flag(trap_exit, true),
	{Id, Host, Odds_Node_Name} = mk_node_name(),
	{Result, Cmd_or_Error} = case os:find_executable("java") of
		false ->
			{stop, java_not_found};
		Java ->
			{ok, mk_cmdline(Java, Id, Host, Tracelevel, Java_Options)}
	end,
	case {Result, Cmd_or_Error} of
		{stop, Error} ->
			{stop, Error};
		{ok, Cmd} ->
			?LOG(info, [{start, Cmd}]),
			Port = open_port({spawn, Cmd}, [stream, {line, 100}, stderr_to_stdout, exit_status]),
			wait_for_startup(#state{port=Port, mbox={odds, Odds_Node_Name}})
	end.

mk_cmdline(Java, Id, Host, Tracelevel, Java_Options) ->
	Extra_CP = proplists:get_all_values(classpath, Java_Options),
	Extra_Options = proplists:delete(classpath, Java_Options),
	Class_Path = string:join([
				filename:join([code:priv_dir(jinterface), "OtpErlang.jar"]),
				filename:join([code:priv_dir(?MODULE), "Odds.jar"])
			| Extra_CP ], separator()),
	lists:flatten([
		Java, " -cp", quote(Class_Path),
		[quote(Option) || Option <- Extra_Options],
		" Odds.Odds",
		quote(Id),
		quote(Host),
		quote(atom_to_list(node())),
		quote(atom_to_list(erlang:get_cookie())),
		quote(integer_to_list(Tracelevel))
	]).

% Wait for the READY signal before confirming that our Lua Server is
% up and running.  Just echo out some of the chit chat coming from the
% Node program.
wait_for_startup(#state{port=Port} = State) ->
	receive
		{Port, {exit_status, N}} ->
			?LOG(error, [{startup_failure, {exit_status, N}}, State]),
			{stop, {exit_status, N}};
		{Port, {data, {eol, "READY"}}} ->
			?LOG(info, [ready, State]),
			{ok, State};
		{Port, {data, {eol, "."}}} ->
			wait_for_startup(State);
		{Port, {data, {eol, S}}} ->
			?LOG(debug, [{startup, S}, State]),
			wait_for_startup(State)
	end.


handle_call({new_position, Match, Position}, From, #state{mbox=Mbox, from=undefined} = State) ->
	?LOG(debug, [{position, Position}, State]),
	Mbox ! {position, self(), {Match, Position}},
	{noreply, State#state{from=From}};
handle_call(stop, _From, #state{from=undefined} = State) ->
	?LOG(debug, [stop, State]),
	{stop, normal, ok, State};
handle_call(Request, _From, #state{from=Id} = State) when Id =/= undefined ->
	?LOG(debug, [{busy, Request}, State]),
	{reply, {error, busy}, State}.

handle_cast(_Request, State) ->
	{noreply, State}.


% We're going to receive a number of different kinds of messages from
% the Odds Node program:
% termination messages, stdout messages, and proper execution replies.

% The first three messages mean that the Odds program is no longer running.
% So we stop.
% The first is normal termination, the other two are abnormal.
handle_info({Port, {exit_status, 0}}, #state{port=Port} = State) ->
	?LOG(info, [{'EXIT', {exit_status, 0}}, State]),
	{stop, normal, State#state{port=undefined, mbox=undefined}};
handle_info({Port, {exit_status, N}}, #state{port=Port} = State) ->
	?LOG(error, [{'EXIT', {exit_status, N}}, State]),
	{stop, {port_status, N}, State#state{port=undefined, mbox=undefined}};
handle_info({'EXIT', Port, Reason}, #state{port=Port} = State) ->
	?LOG(error, [{'EXIT', Reason}, State]),
	{stop, {port_exit, Reason}, State#state{port=undefined, mbox=undefined}};

% Stdout data messages come from the standard output of the Odds Node program.
% Unfinished output lines are tagged with noeol.
handle_info({Port, {data, {noeol, S}}}, #state{port=Port} = State) ->
	{noreply, noeol_port_data(S, State)};
% Finished lines are tagged with eol.
% The convention in the Odds Node program is to send a solitary "." line to signal
% that this particular bit of output is complete; we flush in this case.
handle_info({Port, {data, {eol, "."}}}, #state{port=Port, infoline = []} = State) ->
	{noreply, flush_port_data(State)};
% Otherwise, we handle the complete line.
handle_info({Port, {data, {eol, S}}}, #state{port=Port} = State) ->
	{noreply, eol_port_data(S, State)};

% Finally, we can get proper returns coming from the Odds Node:
% error message or return value message.
handle_info({error, _Reason} = Error, #state{from=From} = State) when From =/= undefined ->
	gen_server:reply(From, Error),
	{noreply, State#state{from=undefined}};
handle_info({odds, _Result} = Reply, #state{from=From} = State) when From =/= undefined ->
	gen_server:reply(From, Reply),
	{noreply, State#state{from=undefined}};

% Anything else is weird and should, at least, be logged.
handle_info(Info, State) ->
	?LOG(debug, [{info, Info}, State]),
	{noreply, State}.


% A termination request when the Odds Node is already down,
% we simply acknowledge.
terminate(Reason, #state{mbox=undefined} = State) ->
	?LOG(debug, [{terminate, Reason}, State]),
	ok;
% Any termination while the Odds Node is up and running,
% we try and stop the Odds Node.
% This could be an explicit call to stop() (Reason=normal),
% or a supervisor shutting us down (Reason=shutdown),
% or an out of band termination (Reason=?)
terminate(Reason, #state{mbox=Mbox} = State) ->
	?LOG(info, [{terminate, Reason}, State]),
	Mbox ! {stop, self(), []},
	wait_for_exit(State).

wait_for_exit(#state{port=Port} = State) ->
	receive
		{Port, {exit_status, 0}} ->
			?LOG(info, [{'EXIT', {exit_status, 0}}, State]),
			ok;
		{Port, {exit_status, N}} ->
			?LOG(error, [{'EXIT', {exit_status, N}}, State]),
			ok;
		{'EXIT', Port, Reason} ->
			?LOG(error, [{'EXIT', Reason}, State]),
			ok;
		{Port, {data, {eol, "."}}} ->
			wait_for_exit(flush_port_data(State));
		{Port, {data, {noeol, S}}} ->
			wait_for_exit(noeol_port_data(S, State));
		{Port, {data, {eol, S}}} ->
			wait_for_exit(eol_port_data(S, State));
		Other ->
			?LOG(debug, [{info, Other}, State]),
			wait_for_exit(State)
	end.

code_change(_Old, State, _Extra) ->
	{ok, State}.


% Helper functions.

% Messages from the Odds Node program are accumulated and finally
% logged as info messages.

% We accumulate the output line by line; potentially having to assemble
% each line from pieces. Everything is accumulated through list cons'ing.
% Thus results have to be reversed before use.
% We don't accumulate forever, flushing regularly.

% We accumulate the line pieces.
noeol_port_data(S, #state{infotext = Text, infoline = []} = State)
		when length(Text) >= ?MAX_INFOTEXT_LINES ->
	noeol_port_data(S, flush_port_data(State));
noeol_port_data(S, #state{infoline = Line} = State) ->
	State#state{infoline = [S | Line]}.

% We accumulate the completed line into the text.
eol_port_data(S, #state{infotext = Text, infoline = []} = State)
		when length(Text) >= ?MAX_INFOTEXT_LINES ->
	eol_port_data(S, flush_port_data(State));
eol_port_data(S, #state{infotext = Text, infoline = Line} = State) ->
	Full_Line = lists:flatten(lists:reverse([S | Line])),
	State#state{infotext = [Full_Line | Text], infoline = []}.

% We write any info report of the completed text.
% If there's any half accumulated line, then process that first.
flush_port_data(#state{infotext = [], infoline = []} = State) ->
	State;
flush_port_data(#state{infoline = [_ | _]} = State) ->
	flush_port_data(eol_port_data("", State));
flush_port_data(#state{infotext = Text} = State) ->
	case lists:reverse(Text) of
		["FATAL: " ++ S | Rest] -> 
			?LOG(fatal, [{stdout, [S | Rest]}, State]);
		["ERROR: " ++ S | Rest] -> 
			?LOG(error, [{stdout, [S | Rest]}, State]);
		["WARN: " ++ S | Rest] -> 
			?LOG(warn, [{stdout, [S | Rest]}, State]);
		["INFO: " ++ S | Rest] -> 
			?LOG(info, [{stdout, [S | Rest]}, State]);
		["DEBUG: " ++ S | Rest] -> 
			?LOG(debug, [{stdout, [S | Rest]}, State]);
		Other ->
			?LOG(info, [{stdout, Other}, State])
	end,
	State#state{infotext = [], infoline = []}.

mk_node_name() ->
	This_Id = "odds",
	This_Host = string:sub_word(atom_to_list(node()), 2, $@),
	{This_Id, This_Host, list_to_atom(lists:flatten([This_Id, "@", This_Host]))}.

quote(S) ->
	case ostype() of
		win32 -> [" \"", S, "\""];
		unix -> [" '", S, "'"]
	end.

separator() ->
	case ostype() of
		win32 -> ";";
		unix -> ":"
	end.

ostype() ->
	case os:type() of
		{Type, _} -> Type;
		Type -> Type
	end.
