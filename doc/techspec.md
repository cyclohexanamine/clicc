# Technical specification of ICC

1. [Introduction](#introduction)
2. [Network architecture](#network-architecture)
3. [Global State](#global-state)
    1. [ACID](#acid)
    2. [Lightweight 2PC](#lightweight-2pc)
    3. [BASE](#base)
    4. [Checksums](#checksums)
    5. [History consensus protocol](#history-consensus-protocol)
    6. [Duplicates](#duplicates)

## Introduction

ICC consists of a network of servers, and users who connect clients to those servers. The servers maintain a global state, which contains chat history and the metadata required to add to it. A server serves this state to clients, and receives requests to update the state from clients, whereupon it may co-ordinate the state update with the rest of the network.


## Network architecture

A server or node is some running software implementing ICC that can communicate two-way via some network. It is assumed that servers implement ICC correctly and without malice.

It's assumed that servers are connected to a network such that all servers are visible by all others during normal function, and that data sent to an address will really go there. This is most likely a friendly network connected to the Internet.

All servers keep an agreed-upon list of servers in the network (as part of the global state), consisting of an address (and port), and a status. A server-server connection is considered active if one server may reliably and sequentially send messages to the other; it should involve some kind of handshake for authentication.

Servers may be added to and removed from the network arbitrarily; any current server may indicate that any change ought to be made to the server list. It's assumed that this is only done manually, when servers are permanently added or removed. Adding a server to the network is an act of trust.

Similarly, status can be changed by any server, although this will typically be self-reported; servers which are known to be offline are marked as such, and not counted as part the network for consensus purposes until they are signalled as online. Servers which do not respond are not assumed to be offline.


## Global state

ICC is built around the notion of a global state. This state, considered as a distributed database across the network, consists of more or less everything that clients and servers want and need to know. Crucially, different aspects of the state have different semantics - ACID or BASE - as usage dictates.

Note that both schemes have atomicity and durability, as any good database should. Only the differences between the two are really relevant.


### ACID

The key here is immediate consistency, meaning that the entire network agrees on this part of the state. For things like user identities, this is useful, since a conflict ultimately can't be resolved peaceably. Consensus is achieved via a spin on 2PC.

Regular two-phase commit protocol relies on all servers responding promptly, but this means that the whole network is unable to process state changes if a single node goes offline. Even in good circumstances, it still means that a change is held up by the slowest node.

Consensus therefore only requires a majority of nodes to agree. This works because if a majority of nodes agree on the most recent transaction, when an outdated node tries to initiate a new transaction it will need at least one up-to-date node to verify that it does not conflict. After transactions are verified to be independent, they can function using BASE protocols, as below. This means that many independent changes can happen in parallel.


#### Lightweight 2PC

The protocol works under the assumption that a valid transaction on any node is valid on any other. In case of machine failure, such as disk errors, nodes are expected to not report success or failure.

Each transaction is proposed, and then co-ordinated, by a single server. The creator gives a new transaction a canonical time and checksum, and then broadcasts it to every server in the network. Upon receipt of a transaction, another node needs to either reversibly commit it and confirm validity to the creator, or report inconsistency and initiate a consensus check. If the creator was wrong, the transaction is cancelled, otherwise the receiver then confirms validity.

If any node cancels the transaction, the creator informs all servers to roll it back. If the majority of the network, including the creator, agree on validity, then the creator informs all servers that the transaction is now consensus, and should be considered part of history.

History based on temporary commits shouldn't be served to users or sent via consensus protocol. Nodes should check against canonical state and temporary state independently when conflict testing; if a transaction's validity is changed by a temporary transaction, the node should wait until this is not the case to respond. Since any temporary transaction may be reverted independently of any other, the network must not allow the state to become inconsistent by agreeing on a transaction that relies on a reverted change.

In the case of offline nodes, or a three-way deadlock (two minority groups have conflicting transactions and a third is unresponsive, or three groups have mutually conflicting transactions), the protocol won't converge. For this reason, both consensus requests and temporary transactions should expire after a certain amount of time, with the creator cancelling a transaction if it can't get consensus in time, and nodes rolling back old temporary transactions.


### BASE

The difference here is eventual consistency, meaning that different servers can have arbitrarily different state and still converge safely. This is true for ICC with message histories, because it's just a log with timestamped entries, and two such logs can simply be merged. BASE means that state changes happen as fast as possible - as soon as any served gets the news, instant locally - so this is appropriate for chat messages themselves.

Each entry is given a canonical timestamp and checksum by the server that creates it. Knowledge of this can spread in a few different ways:
1. A server proactively informs other servers or clients. In an IRC-like model, the creator would inform all other servers, and all servers would inform their connected clients.
2. A server or client requests some part of the history from a server. The server then serves it in full. This is only likely to be useful when the requester has no prior knowledge of the time period, otherwise it's bandwidth-inefficient.
3. A client or server requests consensus from another server. This is a potentially two-way merge that tries to only send data that one side is missing, utilising checksums to determine where there are discrepancies. The protocol for this is described below.


### Checksums
Each line has a canonical checksum, likely a 64-bit integer, that's generated by the creator. Checksums should be randomly distributed with collisions being unlikely.

The checksum of a block of lines is created by XORing all the line checksums together. This allows lines to be 'added' and 'removed' from the checksum easily.


### History consensus protocol

Here, A is a client or server requesting a history update, and B is a server providing it. It may be one way, A <- B, or two-way if A is also a server, A <-> B.

1. A sends B a time range to sync records from, and a checksum of that history.
2. B compares, and if agreeing, it responds so to A, and we're done; otherwise B sends a code of recent history to A that lets A find time periods that are out of sync. 
3. A finds these periods and sends them, along with its own versions of their record checksums (if 2-way), to B. 
4. B sends records that A is missing. If 2-way, it will also request lines it's missing from A.
5. Both merge the new additions locally, and check for any soft duplicates, keeping B's version. History is now considered synced. 

In the case of ACID consensus, a record will be an arbitrary database transaction, and this protocol will only be used to merge records which have been previously verified to be non-conflicting. For BASE, a record is usually a line of chat history.


### Duplicates

If a node receives two identical records, it will ignore one of them. This means that records can be rebroadcast at any time.

If ICC is connected to an IRC network, incoming lines and status updates will not have canonical timestamps or checksums. This means that if a message enters the network in multiple places, it will be duplicated into non-unique records, referred to as 'soft duplicates'.

Messages which could possibly have soft duplicates should be identified as such by the creator. Nodes are expected to implement their own duplicate detection, which should be consistent across every node. When a node receives a soft duplicate, it will discard it. 

When consensus is exchanged, soft duplicates should be discarded in a way that's agreed upon by both parties. The simple solution for this is just to accept B's version in all cases.


## Service semantics

This defines what an ICC service can do for users, and what the state is in relation to the service.

### Messages

Messages are, well, messages, the items of communication that the network ultimately exists to serve. 

A message history is a list of 'recent' messages, where recency is some criteria consistent across the server. It may be recency in time, or number of lines, but if a message is recent then all messages after it must also be recent. Recent history should be large enough to use the service seeing only those messages on the client.

Messages have the following format:
* Checksum
* Timestamp
* From - sender name
* To - recipient or channel name
* Content type - plain text, or otherwise
* Content - body of message
* Soft - whether the message could be a soft duplicate

None of these fields can be changed, and messages cannot be removed unless they are not unique or 'recent' enough. They can otherwise only be added. 

Checksums should be reasonably unique, but the consequences of a collision are not dire, especially when the messages are far apart in time or otherwise distinct. They should be reproducible from the rest of the message. They must be (uniformly) randomly distributed.

Timestamps are given by the server that creates the record. They ought to be millisecond-precise. The network should roughly agree on times.

Messages that may enter the network in multiple places are soft; timestamp and hence checksum may differ between the different points of entry.


### Users

Users are the users of the network, who send and receive messages. A user is entitled to messages addressed to their name sent while they owned that name, and addressed to channels they are in sent at any time.

A user has the following ACID fields:
* Name - name that sends and receives messages
* Ident, host, realname - like IRC
* Modes
Optionally:
* Auth - information to authenticate a connection as this user
    * Auth ID
    * Password
A connection can announce itself with a name, as with an IRC connection; if a user with the name does not already exist, one will be created, without auth, and the connection will be associated with it. Valid user names are limited, and do not overlap with valid channel names.

Alternatively, a connection may identify itself with either a name or auth ID, and a password. If the user does not exist, one will be created with those properties. If the user already exists, the credentials will be checked before associating the connection with the user.

These semantics are intended to allow persistent identity without much overhead, and integrated with transient users. Transient users, which IRC clients would receive, have no auth field, and are removed on disconnection. It may be that persistent users are removed after being disconnected for a while, unless, e.g., they provide an email address.

Users also have BASE information:
* Private message history - all private messages sent to and from this user.

Users can request that servers create messages (the client sends a message to the network). This request is valid if the recipient is valid - an extant user, or a channel that the sender is in - and the message body is valid.

Users can request history that they are entitled to. By default they will be served all messages to them - this is a property of their connection, and not the user itself. A user may issue commands to modify this behaviour and similar, as well as their own modes.

Users can also request to join or leave channels, and change channel modes, the validity of which is detailed below. 


### Channels

Channels are repositories of messages. Access to them can be controlled, and is public by default.

A channel has the following ACID properties:
* Name
* Modes
* Metadata - may include things like a description/topic

Modes define access controls for a channel. A mode has a grammar that necessarily includes a mode type, and optional additional specifiers, which typically indicate who and when to apply that mode to.

No modes are compulsory. Modes may include, among others, 
* Ownership - who owns the channel
* Modifiability - who can change modes, etc.
* Visibility - who can see the channel in listings
* Joinability - who can join, and by extension see the history of, the channel
* Messagability - who can send messages at all, and what messages are valid for whom

These may also have specifiers based on time, etc.

A channel also has the following BASE properties:
* Message history


### Network

The network itself has ACID state, mostly used to implement the service in the first place:
* Name
* Servers - a list of:
    * Address - enough information to connect to the server
    * Status - the server may be known to be offline
* Misc protocol information - things such as
    * Recentness criteria 
    * Soft duplicate criteria
    * Timezone - needed for timestamps to make sense
    * etc.


## Communication

### IRC-interoperable

### ICC-specific