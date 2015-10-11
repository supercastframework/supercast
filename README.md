Supercast
=========

Supercast is actually undocumented. It is developped and kept in sync from the [Sysmo-Core] repository.

Supercast is an application framework offering authentication and access control for applications behaving "asynchronous". Originaly developped to facilitate the creation of full asynchronous websocket application, it is actually used by [Sysmo] to provide asynchronous behaviour to his Qt interface throught raw TCP.


TODO:
* use the Erlang http_server module,
* replace tcp_client by websocket_client,
* rewrite Qt/Supercast in Javascript,
* doc,
* make a websocket demo.

[Sysmo-Core]: https://github.com/sysmo-nms/sysmo-core
[Sysmo]: http://www.sysmo.io/