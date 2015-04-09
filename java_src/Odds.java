package Odds;

import com.ericsson.otp.erlang.OtpErlangException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangBoolean;
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

public class Odds
{

	public static final void main(String[] args)
	{
		if (args.length != 5) {
			print("FATAL: Invalid arguments.");
			System.exit(10);
		}
		String odds_node = args[0];
		String odds_host = args[1];
		String erlang_node = args[2];
		String cookie = args[3];
		System.setProperty("OtpConnection.trace", args[4]);

		try {
			print("Starting Odds Node.");
			OtpNode node = new OtpNode(odds_node + "@" + odds_host, cookie);
			print("Started Odds Node '" + node.toString() + "'.");

			print("Creating mailbox.");
			OtpMbox mbox = node.createMbox("odds");

			Odds odds = new Odds(node, mbox);
			print("READY\n");

			print("DEBUG: Entering loop.");
			odds.loop();
			print("DEBUG: Exiting loop.");

			print("DEBUG: Closing mailbox.");
			node.closeMbox(mbox);
			print("DEBUG: Closing node.");
			node.close();
		} catch (Exception e) {
			print_exception("FATAL: Odds Node exception:", e);
			System.exit(11);
		}
		print("INFO: Odds Node stopped.");
	}

	OtpNode Node;
	OtpMbox Mbox;

	// Dummy at the moment, but would contain real position - odds handling
	private class Position {
		double odds;

		Position(double x) {
			odds = x;
		}

		private final double calculate_odds()
		{
			return odds;
		}
	}

	Odds(OtpNode node, OtpMbox mbox)
	{
		Node = node;
		Mbox = mbox;
	}

	private final void loop()
	{
		print("DEBUG: Starting Session loop.");
		OtpErlangObject msg, elem;
		OtpErlangTuple tuple;
		String request;
		OtpErlangPid from;

		while (true) {

			/* Incoming message is
				{ stop, Caller_Pid, [] }
				{ position, Caller_Pid, Position }
			   with
				stop - the atom 'stop'
				position - the atom 'position'
				Caller_Pid - the Pid of the Erlang process that sent the message
				Position - a tuple, ignored in this sample code
			*/
			try {
				msg = Mbox.receive();
			} catch (OtpErlangException e) {
				print_exception("ERROR: Odds Node stopping abnormally:", e);
				break;
			}

			if (! (msg instanceof OtpErlangTuple)) {
				print("WARNING: Ignoring malformed message (not tuple).");
				continue;
			}

			tuple = (OtpErlangTuple) msg;
			if (tuple.arity() != 3) {
				print("WARNING: Ignoring malformed message (not 3-arity tuple).");
				continue;
			}

			elem = tuple.elementAt(0);
			if (elem instanceof OtpErlangAtom) {
				request = ((OtpErlangAtom) elem).atomValue();
			} else {
				print("WARNING: Ignoring malformed message (first tuple element not atom).");
				continue;
			}

			elem = tuple.elementAt(1);
			if (elem instanceof OtpErlangPid) {
				from = (OtpErlangPid) elem;
			} else {
				print("WARNING: Ignoring malformed message (second tuple element not pid).");
				continue;
			}

			if ("stop".equals(request)) {
				print("INFO: Odds Node stopping normally.");
				break;
			}

			if ("position".equals(request)) {
				Position position;
				elem = tuple.elementAt(2);
				if (elem instanceof OtpErlangTuple) {
					// deconstruct the position info here
					// currently hardcoded
					position = new Position(4.0/3.0);
				} else {
					print("WARNING: Ignoring malformed message (third tuple element not binary).");
					Mbox.send(from, new OtpErlangTuple(new OtpErlangObject[] {
						new OtpErlangAtom("error"),
						new OtpErlangString("Third tuple element is not a binary.")
					}));
					continue;
				}

				double new_odds = position.calculate_odds();

				print("DEBUG: New odds for " + elem + ": " + new_odds);
				Mbox.send(from, new OtpErlangTuple(new OtpErlangObject[] {
						new OtpErlangAtom("odds"),
						new OtpErlangDouble(new_odds)
					})
				);

			} else {
				print("WARNING: Ignoring malformed message (first tuple element not atom 'stop' or 'position').");
				Mbox.send(from, new OtpErlangTuple(new OtpErlangObject[] {
					new OtpErlangAtom("error"),
					new OtpErlangString("First tuple element is not the atom 'stop' or 'position'.")
				}));
			}

		}
	}


	private static final void print(String s)
	{
		System.out.println(s);
		System.out.println(".");
		System.out.flush();
	}

	private static final void print_exception(String s, Exception e)
	{
		System.out.println(s);
		String msg = e.getMessage();
		if (msg != null) System.out.println(msg);
		e.printStackTrace(System.out);
		System.out.println(".");
		System.out.flush();
	}

}
