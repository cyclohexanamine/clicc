# Technical specification of ICC

1. [Introduction](#introduction)
2. [Platform backend](#platform-backend)
    1. [ACID](#acid)
    2. [BASE](#base)
        1. [Duplicates](#duplicates)
        2. [Message removal](#message-removal)
3. [Service semantics](#service-semantics)
    1. [Messages](#messages)
    2. [Users](#users)
    3. [Channels](#channels)
    4. [Network](#network)

## Introduction

ICC consists of a network of servers, and users who connect clients to those servers. The servers maintain a global state, which contains chat history and the metadata required to add to it. A server serves this state to clients, and receives requests to update the state from clients, whereupon it may co-ordinate the state update with the rest of the network.


## Platform backend

ICC relies on a distributed database being implemented by a network of servers. How exactly these are implemented is somewhat arbitrary, but in practice any such platform will need to prioritise speed of service. See `docs/platform-techspec.md` for details of such a platform.

The platform should implement database semantics as follows:

### ACID

ACID should provide immediate consistency across the network. Consistency may not mean that every node serves the same thing at the same time, but that inconsistent changes will never be made in two different places and then propagated. For things like user identities, this is useful, since a conflict ultimately can't be resolved peaceably; at the same time, it allows legitimate changes to move quickly.

### BASE

The difference here is eventual consistency, meaning that different servers can have arbitrarily different state and still converge safely. This is true for ICC with message histories, because it's just a log with timestamped entries, and two such logs can simply be merged. BASE means that state changes happen as fast as possible - as soon as any served gets the news, instant locally - so this is appropriate for chat messages themselves.

#### Duplicates

BASE records may be received multiple times, and will be merged, so the platform should check for duplicates. Normally, a record has a canonical checksum, in which case uniqueness can compare this.

If ICC is connected to an IRC network, incoming lines and status updates will not have canonical timestamps or checksums. This means that if a message enters the network in multiple places, it will be duplicated into non-unique records, referred to as 'soft duplicates'. Messages which could possibly have soft duplicates should be identified as such by the creator. They will then have different uniqueness criteria applied to them.

#### Message removal

Messages cannot be unsent, but may be removed from a node's history. This will typically be if they're a duplicate, or not recent: the service might only keep recent history, in which case messages that don't meet recentness criteria will be discarded.


## Service semantics

This defines what an ICC service can do for users, and what the state is in relation to the service.

### Messages

Messages are, well, messages, the items of communication that the network ultimately exists to serve. 

A message history is a list of 'recent' messages, where recency is some criteria consistent across the server. It may be recency in time, or number of lines, but if a message is recent then all messages after it must also be recent. Recent history should be large enough to use the service seeing only those messages on the client.

Messages have the following format:
* Timestamp
* From - sender name
* To - recipient or channel name
* Content type - plain text, or otherwise
* Content - body of message

None of these fields can be changed, and messages cannot be removed unless they are not unique or 'recent' enough. They can otherwise only be added. 

Timestamps are given by the server that creates the record. They ought to be millisecond-precise. The network should roughly agree on times.


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
