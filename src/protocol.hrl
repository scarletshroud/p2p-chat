-record(packet, {username, message}).

-record(update_peers_request, {peers, secret_key}).
-record(update_keys_request, {new_keys, secret_key}).

-record(get_peers_request, {mypid, peer=#peer{}}).
-record(get_peers_response, {peers}).

-record(find_closest_request, {mypid, peer=#peer{}, id}).
-record(find_closest_response, {result}).

-record(handshake_request, {username, public_key}).
-record(handshake_response, {public_key}).

-record(find_peer_request, {id}).
-record(send_message, {message}).
-record(get_keys_request, {secret_key}).

-record(ping_request, {sender}).