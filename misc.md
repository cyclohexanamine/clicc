-group PMs - can be an anonymous channel if only authed users are in it
-XMPP? 
-message content types
-public key crypto for auth? 

As BNC
- 1 IRC user per auth
- distribute connections to IRC between different servers, so channels are redundantly received
- could have local chans/msg as well as remote (on IRC)

Consensus for BASE
-Nodes broadcast received messages to all others
-Consensus checks made periodically between all node pairs
-ECC may be able to tell which blocks disagree all at once, prevents needing to recheck whole log in case new lines are added in the meantime
-If a node gets new lines thanks to a consensus check, broadcast them
-other servers will ignore duplicates (if line is from IRC, duplicates may differ in timestamp - soft duplicate)
-Nodes continue functioning under any connectivity

Better BASE consensus proto
1. A sends B a time range to sync (messages have canonical timestamps so this is OK) and hash of that history
2. B compares, and if agreeing, done; otherwise B sends code of recent history to A that will let A find time periods that are out of sync
3. A finds these periods and sends them, along with its own versions of their line hashes (if server), to B
4. B sends lines that A is missing, and sends them. If A is a server, it will also request lines it's missing from A.
5. Both merge the new additions locally, and check for any soft duplicates, keeping B's version. History is now considered synced

Hashes/codes
- Each line has canonical checksum (should be reproducible?), probably 64bit
- History checksums are fixed width, same as line checksum - because should be order independent. XOR all line checksums.
- Time code just means one node divides the history up into time blocks as it sees fit, and hashes each history within them


Consensus for ACID
-Use 2PC
-Ignore unresponsive nodes unless they're half the network or more
-If half the network or more is unresponsive, reject updates. When connection restored, accept any changes made by majority

Connection wrapper
Class with 
-callback for message received
-function to send message
-up/down status
-reconnect etc
Message wraps a line sent over the socket. May also wrap TLS etc, or binary protocol

Global queue for messages in - server processes these in parallel - queue may also be used for connection status updates etc

Global out queue - worker will get every connection to send a message once

ACID and BASE processors with queues for writes, and callback when they happen. Should process reads with the appropriate semantics. May also have timeout callback/expiry

Database
-should have a transactional DB of some kind, and wrap for the processors
-various backends, e.g. RAM, Lisp serial, SQL