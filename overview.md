ICC is like IRC in many ways, but with some enhancements that improve usability. It should be compatible with RFC-compliant IRC clients and servers.


## Brief overview of IRC

IRC is a simple real-time chat protocol where servers are networked together, and clients connect to a server. Clients send messages to the server, and the network relays them to recipient clients. 

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
    * Servers should be able to maintain a consistent state despite partition.
    * Clients should not need to maintain a single TCP connection to use the service.
    
3. Interoperate with IRC