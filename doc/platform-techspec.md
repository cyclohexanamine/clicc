# Technical specification of the generic service platform

1. [Introduction](#introduction)
2. [Network architecture](#network-architecture)
3. [Global State](#global-state)
    1. [ACID](#acid)
    2. [Lightweight 2PC](#lightweight-2pc)
    3. [Network](#network)
    4. [BASE](#base)
    5. [Checksums](#checksums)
    6. [History consensus protocol](#history-consensus-protocol)
    7. [Duplicates](#duplicates)

## Introduction

The GSP consists of a network of servers, maintaining a global state co-ordinated with the rest of the network. It's intended to provide a platform to implement an arbitrary distributed service, with the following constraints:
* A global state is wanted with some semantics.
* Servers are all equally able to serve the generic state to the rest of the network, when online.
* Servers may have poor uptime.

Each individual server has the following properties:
* Can serve client connections (hopefully scalably).
* Has a local database.

The service to be implemented may make use of abstractions provided by the platform to ignore networking and database details.


## Network architecture

A server or node is some running software implementing the platform that can communicate two-way via some network. It is assumed that servers implement the platform correctly and without malice.

It's assumed that servers are connected to a network such that all servers are visible by all others during normal function, and that data sent to an address will really go there. This is most likely a friendly network connected to the Internet.

All servers keep an agreed-upon list of servers in the network (as part of the global state), consisting of an address (and port), and a status. A server-server connection is considered active if one server may reliably and sequentially send messages to the other; it should involve some kind of handshake for authentication.

Servers may be added to and removed from the network arbitrarily; any current server may indicate that any change ought to be made to the server list. It's assumed that this is only done manually, when servers are permanently added or removed. Adding a server to the network is an act of trust.

Similarly, status can be changed by any server, although this will typically be self-reported; servers which are known to be offline are marked as such, and not counted as part the network for consensus purposes until they are signalled as online. Servers which do not respond are not assumed to be offline.


## Global state

The platform is built around the notion of a global state. This state, considered as a distributed database across the network, consists of more or less everything that clients and servers want and need to know. Crucially, different aspects of the state have different semantics - e.g., ACID or BASE - as usage dictates.

Note that all schemes tend to have have atomicity and durability, as any good database should. Only the differences between them are really relevant below.


### ACID

The key here is immediate consistency, meaning that the entire network agrees on this part of the state. This means that for arbitrary changes which may conflict with other changes, a conflict will never be reached by the network. Consensus is achieved via a spin on 2PC.

Regular two-phase commit protocol relies on all servers responding promptly, but this means that the whole network is unable to process state changes if a single node goes offline. Even in good circumstances, it still means that a change is held up by the slowest node.

Consensus therefore only requires a majority of nodes to agree. This works because if a majority of nodes agree on the most recent transaction, when an outdated node tries to initiate a new transaction it will need at least one up-to-date node to verify that it does not conflict. After transactions are verified to be independent, they can function using BASE protocols, as below. This means that many independent changes can happen in parallel.


#### Lightweight 2PC

The protocol works under the assumption that a valid transaction on any node is valid on any other. In case of machine failure, such as disk errors, nodes are expected to not report success or failure.

Each transaction is proposed, and then co-ordinated, by a single server. The creator gives a new transaction a canonical time and checksum, and then broadcasts it to every server in the network. Upon receipt of a transaction, another node needs to either reversibly commit it and confirm validity to the creator, or report inconsistency and initiate a consensus check. If the creator was wrong, the transaction is cancelled, otherwise the receiver then confirms validity.

If any node cancels the transaction, the creator informs all servers to roll it back. If the majority of the network, including the creator, agree on validity, then the creator informs all servers that the transaction is now consensus, and should be considered part of history.

History based on temporary commits shouldn't be served to users or sent via consensus protocol. Nodes should check against canonical state and temporary state independently when conflict testing; if a transaction's validity is changed by a temporary transaction, the node should wait until this is not the case to respond. Since any temporary transaction may be reverted independently of any other, the network must not allow the state to become inconsistent by agreeing on a transaction that relies on a reverted change.

In the case of offline nodes, or a three-way deadlock (two minority groups have conflicting transactions and a third is unresponsive, or three groups have mutually conflicting transactions), the protocol won't converge. For this reason, both consensus requests and temporary transactions should expire after a certain amount of time, with the creator cancelling a transaction if it can't get consensus in time, and nodes rolling back old temporary transactions.


#### Network

The network itself has some necessary ACID state, used to implement the platform itself:
* Servers - a list of:
    * Address - enough information to connect to the server
    * Status - the server may be known to be offline
* Misc protocol information - things such as
    * Recentness criteria 
    * Soft duplicate criteria
    * etc.



### BASE

The difference here is eventual consistency, meaning that different servers can have arbitrarily different state and still converge safely. In the case of, e.g., a log with timestamped entries, two such logs can simply be merged. BASE means that state changes happen as fast as possible - as soon as any server gets the news, instant locally.

Each entry is given a canonical checksum by the server that creates it. Knowledge of this can spread in a few different ways:
1. A server proactively informs other servers or clients. In an IRC-like model, the creator would inform all other servers, and all servers would inform their connected clients.
2. A server or client requests some part of the history from a server. The server then serves it in full. This is only likely to be useful when the requester has no prior knowledge of the selection, otherwise it's bandwidth-inefficient.
3. A client or server requests consensus from another server. This is a potentially two-way merge that tries to only send data that one side is missing, utilising checksums to determine where there are discrepancies. The protocol for this is described below.


### Checksums
Each line has a canonical checksum, likely a 64-bit integer, that's generated by the creator. Checksums should be randomly distributed with collisions being unlikely.

The checksum of a block of lines is created by XORing all the line checksums together. This allows lines to be 'added' and 'removed' from the checksum easily.


### History consensus protocol

Here, A is a client or server requesting a history update, and B is a server providing it. It may be one way, A <- B, or two-way if A is also a server, A <-> B.

1. A sends B some selection to sync records from, and a checksum of that history.
2. B compares, and if agreeing, it responds so to A, and we're done; otherwise B sends a code of the history to A that lets A find ranges that are out of sync. 
3. A finds these ranges and sends them, along with its own versions of their record checksums (if 2-way), to B.*
4. B sends records that A is missing. If 2-way, it will also request lines it's missing from A.
5. Both merge the new additions locally, and check for any soft duplicates, keeping B's version. History is now considered synced. 

* It is also possible to subdivide these ranges and request more codes from them, if they are potentially large.

In the case of ACID consensus, a record will be an arbitrary database transaction, and this protocol will only be used to merge records which have been previously verified to be non-conflicting. For BASE, a record is the atomic item to be kept.


### Selections, ranges and codes

When a node requests records from a peer, it will use some selection criteria which depend on the service being implemented. It's expected that records be orderded in some way, although ordering by checksum can work.

A range is the response to a selection - a list of records that match the selection. 

A code is intended to make comparison of record lists efficient by comparing blocks of records instead of individually. Given this, a code is a list of selections, with the checksums for the block of records in each one. How this code is generated depends on the service. 

In an example where a service has timestamped entries, selections will typically be by time range. In this case, it will likely make sense to generate codes that have smaller intervals for more recent history - e.g., 1900-2000, 2000-2015, Q1&2 2016, ... - because nodes are more likely to be out of sync for recent history.


### Duplicates

If a node receives two identical records, it will ignore one of them. This means that records can be rebroadcast at any time.

For some reasons, a service might want different duplicate criteria. It will provide these criteria and mark records that should use them - 'soft' records. When consensus is exchanged, soft duplicates should be discarded in a way that's agreed upon by both parties. The simple solution for this is just to accept B's version in all cases.



## Communication
