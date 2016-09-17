# Motivation and overview of ICC

ICC is superficially like IRC in many ways, but with some enhancements that improve usability. It should be compatible with RFC-compliant IRC clients and servers.

1. [Brief overview of IRC](#brief-overview-of-irc)
2. [Goals of ICC](#goals-of-icc)
3. [Brief overview of ICC](#brief-overview-of-icc)

## Brief overview of IRC

IRC is a simple real-time text-based chat protocol where servers are networked together, and clients connect to a server. Clients send messages to the server, and the network relays them to recipient clients. 

Clients/users may send textual messages directly to other users, or to a channel. Users can join channels, so that they'll receive all messages sent to that channel.

Users are ephemeral and effectively exist as a name for a client-server connection. Persistent 'identity' is bootstrapped by authentication mechanisms on services, and keep-alive intermediate servers between the client and the server (bouncers/BNC).

Connections exist as long-lived TCP sockets. This creates hell for client-server connections under inconsistent network conditions, such as spotty WiFi or mobile data, or a poor consumer network connection. The server network is connected together in a spanning tree, which effectively splits the network in two (netsplit) every time a server disappears.

The client-server protocol defines communication as straightforward and text-based. This tends to make it exceptionally easy to write IRC-compliant software, such as clients, bots, services. Server-server communication is similarly simple, which can lead to inconsistent state and rudimentary conflict resolution.

Servers maintain a global state comprising of all servers, channels and users. This more or less just defines connectivity; the only persistent information is channel metadata such as modes.

Greater functionality is provided via services; client-like automated software that may receive arbitrary data and implement arbitrary local functionality. They're limited to sending data in more or less the same way as clients.


## Goals of ICC
1. Integrate BNC-like functionality into the network itself.
    * A globally consistent recent chat history should be a part of the server state, available to clients.
    * Users should be able to have a persistent identity, with an arbitrary number of client connections as the same user.
    
2. Improve resilience to poor network conditions.
    * Servers should be linked in a mesh such that partition is less likely.
    * The network should be able to maintain a consistent state despite partition.
    * Clients should not need to maintain a single TCP connection to use the service.
    
   These two points allow for a chat experience more akin to Slack or Discord, with clients not having to keep track of history or being limited to one connection at a time. Private message history, and group private messaging, become possible. Users also don't feel the effects of server outages very much.
    
3. Interoperate with IRC.
    * An IRC client, and the user behind it, should be able to connect to and use an ICC service without any technical issues, and few violated expectations.
    * An ICC network should be able to federate with an IRC network in some way, acting as either clients or servers to an unknowing IRC network.
    
   There's a rich ecosystem of IRC clients that should be usable. In addition, being able to connect an ICC network to an IRC network makes it immediately useful, even when everyone uses IRC.
    
   This does have a number of consequences. Servers must be multilingual, keeping track of which connections speak IRC and which ICC. Ephemeral users still need to be supported. The ICC protocol must account for messages originating from IRC servers. And the most useful ICC features ought to integrate into the IRC client protocol somehow, likely in the same way as BNCs work now. 


## Brief overview of ICC

ICC is a real-time chat protocol. The service consists of a network of servers implementing a global state, and each server serving connections from clients.

The core difference compared to IRC is that this network is not really a relay. The network holds a canonical chat history for channels and users, and a database of metadata. Clients are notionally stateless, maintaining a local version of some of the network's state. This will typically be recent chat history for some users and channels, and some user and channel metadata.

An ICC network can be thought of as a network of nodes that implement two distributed databases. One, for network, user and channel data, has ACID semantics; the other, for history, has BASE semantics.

Clients then connect to servers and receive the state they request (and are entitled to), e.g., history and metadata for the user it identifies as and a few channels that user is in. Servers may proactively send state updates to a connected client, e.g., new messages and mode changes. Clients also send requests to servers to modify the state in some way, e.g., to add (send) a new message or join a new channel. The server coordinates this state change with the network, before reporting back to the client.

The connection that a client uses is more or less arbitrary, as long as it is capable of delivering and receiving messages reliably. It need not be a single stream socket. The same is true of server-server connections, although in practice keeping a socket for as long as possible will reduce overhead somewhat.
