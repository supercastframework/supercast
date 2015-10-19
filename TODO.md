TODO
====
1. delayed sync included in supercast, require that every channel event is
recognized by supercast,
2. remove **supercast_mpd.erl** (everything done by the **supercast_channel** processes),
3. remove **supercast_server.erl** (everything done by the **(tcp|ssl|ws|wss)_client** processes),
4. channel live update of their permissions change: unsubscribe connected client not satisfying to the new permission,
5. websocket/supercast javascript library,
6. include simple http server.


In the future
-------------
Behaviour in a distributed environment with multiple supercast servers
running (see TODO 2 and 3).

Usages and examples
------------------
1. ROR like project builder
2. Example apps (risk game, chat like forum, sysmo)
3. Dynamic website with page has channel.
