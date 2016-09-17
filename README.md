# ICC and CLICC

**ICC** is a real-time chat protocol, superficially similar to IRC but with semantics intended for greater convenience. These include
* A global chat history
* Persistent user identities supporting multiple connections
* Better tolerance for poor connections, and network partitioning

ICC is also designed to be easy to implement for clients, and to be interoperable with the IRC protocol. 

See `docs/overview.md` for a broad overview of the ICC protocol, and `docs/techspec.md` for its technical details. The protocol is currently under active revision.

**CLICC** is a Common Lisp ICC server implementation. It's intended to be usable in practice. See `docs/implementation.md` for details. It is currently incomplete.
