
A template to allow the odds calculation to happen in Java land.

The lo_odds.erl module contains the code to start an external Java
program that we will be communicating with. It also contains a
position() function that will forward match and position data to
the Jave program.

Copy lo_odds.erl to the src folder.

The Odds.java program is started from lo_odds.erl and will establish
itself as an Elrang-lookalike Node. It can then receive messages
from Elrang and send replies back to Erlang. There's a very silly
odds calculation in the Java code at the moment.

The actual content of the new position message and reply needs to
be adapted to your example.

Build the jar file and copy it over to the priv folder.

mkdir Odds
javac -cp "C:/Program Files/erl6.3/lib/jinterface-1.5.12/priv/OtpErlang.jar" -d Odds Odds.java
jar cf Odds.jar -C Odds .
copy Odds.jar ..\priv
