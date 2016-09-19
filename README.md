# ICC and CLICC

**ICC** is a real-time chat protocol, superficially similar to IRC but with semantics intended for greater convenience. These include
* A global chat history
* Persistent user identities supporting multiple connections
* Better tolerance for poor connections, and network partitioning

ICC is also designed to be easy to implement for clients, and to be interoperable with the IRC protocol. 

See `docs/icc-overview.md` for a broad overview of the ICC protocol, and `docs/icc-techspec.md` for its technical details. The protocol is currently under active revision.

**CLICC** is a Common Lisp ICC server implementation, intended to be usable in practice. It uses a generic distributed service platform, details of which are in `docs/platform-implementation.md`. See `docs/icc-implementation.md` for details of the ICC implementation itself. It is currently incomplete.
