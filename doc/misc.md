-group PMs - can be an anonymous channel if only authed users are in it
-XMPP? 
-public key crypto for auth? 

As BNC
- 1 IRC user per auth
- distribute connections to IRC between different servers, so channels are redundantly received
- could have local chans/msg as well as remote (on IRC)


Database management should be separable enough from chat things - clear distinction between state management protocol and chat-specific protocol. Server should be usable as just a distributed DB.
^ State processor should only implement state protocol, and register callbacks for generic events so processor can handle the rest of the protocol. 
