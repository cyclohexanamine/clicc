# CLICC Implementation

CLICC is Common Lisp ICC, an ICC server implementation.

## Components

### Low-level details

#### Threads

#### Local database

### Connection

Low-level connections are abstracted by a connection object. The high-level interface is as follows:
* Accessors for address, status.
* Method to send messages.
* Handler registration for message reception.
* Getters and setters for metadata.
Details such as 'connecting' and 'disconnecting', as well as the raw interface to whatever implements the connection, are abstracted.

Internally, the connection should have its own thread for managing communication, with the send-message method adding the message to a queue to be sent when the internal thread processes it. When it receives a message, it should call the registered handle on it. 

Metadata is an arbitrary plist, and is used by processors to inform the connection manager about the connection. This will typically be whether it's a client or server, user identity, client capabilities, and so on.

### Connection manager

The connection manager is responsible for accepting new connections and instantiating connection objects. It will keep track of these connections and delete them when they're no longer needed, and provide an interface for selecting connections by their metadata.

### Message parser

The parser is the ICC-aware layer that translates raw messages 

### State manager

The state manager unifies the local database and the connection manager to implement ICC's distributed database semantics.

### Processors


## Details