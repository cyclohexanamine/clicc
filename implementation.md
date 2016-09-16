# CLICC Implementation

CLICC is Common Lisp ICC, an ICC server implementation.


## Components

The server has three main components that work in parallel. Remote connections and local state have their own managers, while the processor implements all ICC semantics and orchestrates the managers.

Typically, an interface that needs to wait for thread safety should provide a blocking method to carry out the operation; methods which might block for a long time will be indicated by an asterisk. In some cases these might be augmented by asynchronous methods. The processor contains various abstractions that allow logic spanning many connections and messages to be implemented easily.


### Connection manager

The connection manager is responsible for accepting new connections and instantiating connection objects. Internally, it lives in its own thread. It provides a thread-safe interface for tracking connections, selecting them by metadata, and sending messages through them:
* Methods to initiate a connection.
* Handler registration.
    * Connection creation.
    * Default message handler.
* Selector for lists of connections based on metadata.
* Batch operations on lists of connections.
    * Send message*.
    * Modify metadata.
    * Register new handler.
    * Query status*.
    * Remove*.

#### Connection

Low-level connections are abstracted by a connection object. The high-level interface is as follows:
* Accessors for address, status.
* Method to send messages*.
* Handler registration for message reception.
* Getters and setters for metadata.
* Destructor*.
Internally, the connection should have its own thread for managing communication, with the send-message method adding the message to a queue to be sent when the internal thread processes it. When it receives a message, it should call the registered handler, passing a reference to itself as well as the message. 

Metadata is an arbitrary plist, and is used by processors to inform the connection manager about the connection. This will typically be whether it's a client or server, user identity, client capabilities, and so on.


### State manager

The state manager wraps the local database to provide state reads and changes with the required semantics. It consists of a layer around two threads, for ACID and BASE, that each reads and writes in a thread-safe manner. The interface is:
* Read/write operations for ACID
    * Read query*
    * Commit update (temporary)*
    * Canonicalise (make permanent) update*
* Read/write operations for BASE
    * Read query*
    * Add record*

#### Local database

The local database backend is abstracted into an intermediate interface. Unlike the connection object, this is never seen outside the state manager. The database may be, for example, a SQL database on disk.


### Processor

The processor implements the ICC-aware logic on the server. It is responsible for managing the managers, registering callbacks with them for the appropriate events, and in general orchestrating the protocol. The sub-components of the processor aren't necessarily distinct as threads, but processor logic itself may be threaded arbitrarily.

#### Message handler

The message handler provides a translation between raw messages, which the connection manager will pass, and various command constructs. It is the dispatch control for any messages received, and the point of contact for sending messages. When a message is received, there are typically one of three things that can happen:
1. The message is unexpected, and kicks off a new process. In this case the handler initiates the necessary process.
2. The message is expected as part of some ongoing process, in which case the handler passes it off to this process, most likely via the coordination manager.
3. The message is invalid. It may be sent to a special process, or just ignored.

#### Coordination manager

ICC logic that is easily expressible in terms of a conversation between nodes/clients would require complex state and programming to implement in terms of callbacks alone. The coordination manager provides abstractions for co-ordinating remote nodes, such as constructs for
* Blocking until a node sends a relevant response
* Gathering responses from the network

#### State protocol

This implements ICC protocol for coordinating the global state, built on top of the coordination manager. It will handle all updates to ACID state, some to BASE where necessary, and perform duplicate detection, as well as announcing changes to clients. Most messages between servers, and read requests from clients, are handled here.

#### Request protocol

This implements handlers for some new processes from other nodes, typically requests from clients to initiate state changes. If a message requires a semantic validity check before it can be implemented, it will be handled here.

#### Authentication

Handlers for client authentication, including creating new identities and identifying connections as identities.

## Details