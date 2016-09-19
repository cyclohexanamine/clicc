# CLICC Implementation

CLICC is Common Lisp ICC, an ICC server implementation.

**Components**:
   1. [Threads](#threads)
   2. [Connection manager](#connection-manager)
      1. [Connection](#connection)
      2. [Multi connection manager](#multi-connection-manager)
   3. [State manager](#state-manager)
      1. [Local database](#local-database)
      2. [Database processor](#database-processor)
   4. [Processor](#processor)
      1. [Message handler](#message-handler)
      2. [Coordination manager](#coordination-manager)
      3. [State protocol](#state-protocol)
      4. [Service protocol](#service-protocol)


## Components

The server has three main components that work in parallel. Remote connections and local state have their own managers, while the processor implements all protocol semantics and orchestrates the managers. There is also a thread interface available that provides abstractions for threaded and thread-safe objects.

Typically, an interface that needs to wait for thread safety should provide a blocking method to carry out the operation; methods which might block for a long time will be indicated by an asterisk. In some cases these might be augmented by asynchronous methods. The processor contains various abstractions that allow logic spanning many connections and messages to be implemented easily.


### Threads

The thread modules relies on something else - current Bordeaux Threads - to provide a basic interface for threads and locks. From there, it provides threaded-object - a class which is intended to be accessed from. multiple threads, and may have internal threads for processing.

A threaded-object has locks for its slots that may be accessed, and exposes readers and modifiers for these. There is also the option to define processors for the object, with a loop body that can be instantiated, perhaps multiple times, in its own thread. As a worker thread, these processors will likely read from the object's internal queue.

The interface for a threaded-object is:
* Definition - defclass-threaded, inheritance, defslotints, defprocessors.
* Creation - make-instance
* Processor handling - start-processor
* Queueing - push-queue, pop-queue


### Connection manager

The connection manager is responsible for accepting new connections and instantiating connection objects. Internally, it lives in its own thread. It provides a thread-safe interface for tracking connections, selecting them by metadata, and sending messages through them:
* Methods to initiate a connection.
* Handler registration.
    * Connection creation? 
    * Message handler.
* Selector for lists of connections based on metadata.
* Batch operations on lists of connections.
    * Send message*.
    * Modify metadata.
    * Register new handler.
    * Query status*.
    * Remove*.
    
It will assign connections with a unique ID that can be used by higher-ups to refer to the connection. 
    
It's intended to be specialised on a specific kind of connection. For multiple connection types or listener addresses, use multi-connection-manager.

#### Connection

Low-level connections are abstracted by a connection object. The high-level interface is as follows:
* Accessors for address, status.
* Method to send messages*.
* Getters and setters for metadata.

Metadata is an arbitrary plist, and is used by processors to inform the connection manager about the connection. This will typically be whether it's a client or server, user identity, client capabilities, and so on.

#### Multi connection manager

In the case of handling connections across multiple connection managers - for multiple connection types or addresses - the multi connection manager wraps around a collection of collection managers and exposes an interface like a single one.


### State manager

The state manager wraps the local database to provide state reads and changes with the required semantics. It consists of a layer around multiple database processors - e.g., one for each of ACID and BASE -  that each read and write in a thread-safe manner. 

The interface may look like:
* Read/write operations for ACID
    * Read query*
    * Commit update (temporary)*
    * Canonicalise (make permanent) update*
* Read/write operations for BASE
    * Read query*
    * Add record*

#### Local database

The local database backend is abstracted into an intermediate interface. Like the connection object, this is never directly seen outside the state manager. The database may be, for example, a SQL database on disk.

#### Database processors

Each part of the database with different semantics is handled by a different processor. A database processor is a threaded object that interfaces with the local database, shepherded by the state manager.


### Processor

The processor implements the protocol-aware logic on the server. It is responsible for managing the managers, registering callbacks with them for the appropriate events, and in general orchestrating the protocol. The sub-components of the processor aren't necessarily distinct as threads, but processor logic itself may be threaded arbitrarily.

#### Message handler

The message handler provides a translation between raw messages, which the connection manager will pass, and various command constructs. It is the dispatch control for any messages received, and the point of contact for sending messages. When a message is received, there are typically one of three things that can happen:
1. The message is unexpected, and kicks off a new process. In this case the handler initiates the necessary process.
2. The message is expected as part of some ongoing process, in which case the handler passes it off to this process, most likely via the coordination manager.
3. The message is invalid. It may be sent to a special process, or just ignored.

#### Coordination manager

Logic that is easily expressible in terms of a conversation between nodes/clients would require complex state and programming to implement in terms of callbacks alone. The coordination manager provides abstractions for co-ordinating remote nodes, such as constructs for
* Blocking until a node sends a relevant response
* Gathering responses from the network

#### State protocol

This implements protocol for coordinating the global state, built on top of the coordination manager. It will handle all updates to ACID state, some to BASE where necessary, and perform duplicate detection, as well as announcing changes to clients.

#### Service protocol

This implements handlers for messages pertaining to the overlying service built on top of the network, e.g., a request from a client to modify the global state.

