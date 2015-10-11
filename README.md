Supercast
=========


Supercast is an application framework offering authentication and access control for applications behaving "asynchronous". It provide his functionnalities throught the concept of stream and access control around it. Originaly developped to facilitate the creation of full asynchronous websocket application, it is actually used by [Sysmo] to provide asynchronous behaviour to his Qt interface throught raw TCP.


TODO:
* use the Erlang http_server module,
* rewrite tcp_client to websocket_client,
* implement a SupercastClient library in Javascript (see [QtSupercast]),
* make a websocket tutorial demo (dice war game).
* doc,

Actually undocumented it is not meant to be used as such. It is developped and kept in sync from the [Sysmo-Core] repository.
[Sysmo-Core]: https://github.com/sysmo-nms/sysmo-core
[Sysmo]: http://www.sysmo.io/
[QtSupercast]: https://github.com/sysmo-nms/sysmo-operator/tree/master/network