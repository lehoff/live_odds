-module(lo_ps).


%% For use as naming mechanism with OTP components.
%% See http://www.erlang.org/doc/man/gen_server.html#start_link-3
%% When starting a gen_server use {via, lo_ps, Name} to name the process. 
%% When sending a message to a gen_server use {via, lo_ps, Name} to name the process. 
%% This just calls through to gproc, but only with local names.
-export([register_name/2,
         unregister_name/1, 
         whereis_name/1,
         send/2]).

%% If lo_ps is used with names of the form {Tag, Name} then lookup(Tag) will return a
%% list of all the names of these processes in the form [{Tag, Name}]. 
%% Then whereis_name can be used to retrive the pid, if necessary.
-export([lookup/1]).

%% Simple pub/sub based on gproc. Only local properties used.
-export([subscribe/1,
         unsubscribe/1,
         publish/2]).

%% pub/sub functions
subscribe(Topic) ->
  gproc:reg(gproc_local_property(Topic)).

unsubscribe(Topic) ->
  gproc:unreg(gproc_local_property(Topic)).

publish(Topic, Msg) ->
  gproc:send(gproc_local_property(Topic), Msg).

gproc_local_property(Topic) ->
  {p, l, Topic}.


lookup(Tag) ->
  Key = {Tag, '$1'},
  GProcKey = {n, l, Key},
  MatchHead = {GProcKey, '_', '_'},
  Guard = [],
  Result = [{{Tag, '$1'}}],
  gproc:select([{MatchHead, Guard, Result}]).


%% via naming callbacks
register_name(Name, Pid) ->
  gproc:register_name(gproc_local_name(Name), Pid).

unregister_name(Name) ->
  gproc:unregister_name(gproc_local_name(Name)).

whereis_name(Name) ->
  gproc:whereis_name(gproc_local_name(Name)).

send(Name, Msg) ->
  gproc:send(gproc_local_name(Name), Msg).


gproc_local_name(Name) ->
  {n, l, Name}.
