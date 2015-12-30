Supercast
=========
[![Build Status](https://travis-ci.org/supercastframework/supercast.svg)](https://travis-ci.org/supercastframework/supercast)

Actually undocumented Supercast is not meant to be used as such. It isdevelopped and kept in sync from the [Sysmo-Core] repository.

Supercast is an application framework offering authentication and access control for applications behaving "asynchronous". It provide his functionnalities throught the concept of channels and access control around it. Originaly developped to facilitate the creation of full asynchronous websocket application, it is actually used by [Sysmo] to provide asynchronous behaviour to his Qt interface throught raw TCP.

TODO
====
1. delayed sync included in supercast, require that every channel event is
recognized by supercast,
2. remove **supercast_mpd.erl** (everything done by **supercast_channel** processes),
3. channel live update of their permissions change: unsubscribe connected client not satisfying to the new permission,
4. supercast javascript library,


In the future
-------------
Behaviour in a distributed environment with multiple supercast servers
running (see **TODO 2**).

Usages and examples
------------------
1. ROR like project builder
2. Example apps (risk game, chat like forum, sysmo)
3. Dynamic website with page has channel.

[Sysmo-Core]: https://github.com/sysmo-nms/sysmo-core
[Sysmo]: http://www.sysmo.io/
[QtSupercast]: https://github.com/sysmo-nms/sysmo-operator/tree/master/networkTODO
