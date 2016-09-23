- group PMs - can be an anonymous channel if only authed users are in it
- XMPP? 
- public key crypto for auth? 

As BNC
- 1 IRC user per auth
- distribute connections to IRC between different servers, so channels are redundantly received
- could have local chans/msg as well as remote (on IRC)

L2PC: 
* on conflict, immediately tell leaders to abort both
* if timeout happens on non-leader, it will try to co-ordinate the transaction itself
* multiple leaders is OK
* can this lead to inconsistent states, if one leader aborts and another commits? 
* maybe aborts should be co-ordinated too

Connection types
* Plain TCP
* TLS wrapping ^
* HTTP
* Email - accessing external SMTP server, or self-hosted
* UDP/SCTP

Platform would be interesting to use as an email service
