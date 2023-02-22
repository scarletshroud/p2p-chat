-define(max_secret_key, 4294967296).
-define(id_size, 160).
-define(refresh_time, 5*60*60*1000).
-define(client_timeout, 2000).
-define(max_hops, 8).

-record(peer,{id, username, server_pid, client_pid}).