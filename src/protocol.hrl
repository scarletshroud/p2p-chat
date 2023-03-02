% Протокол передачи данных: username - имя пользователя, message - пользовательское сообщение, broadcast_list - список, клиентов, которые уже получили сообщение
-record(packet, {username, message, broadcast_list}).

% Запрос для обновления таблицы маршрутизации на сервере. peers - таблица маршрутизации, secret_key - ключ, для обращения к серверу
-record(update_peers_request, {peers, secret_key}).

% Запрос для обновления публичных ключей шифрования на сервере. new_keys - Обновленные ключа шифрования, secret_key - ключ, для обращения к серверу
-record(update_keys_request, {new_keys, secret_key}).

% Запрос/ответ для получения таблицы маршрутизации от сервера.
-record(get_peers_request, {mypid, peer = #peer{}}).
-record(get_peers_response, {peers}).

% peers - таблица маршрутизации.
% Запрос/ответ для получения ближайшего пира
-record(find_closest_request, {mypid, peer = #peer{}, id}).
-record(find_closest_response, {result}).

% result - ближайший пир
% Записи запрос/ответ для обмена публичными ключами шифрования между двумя пирами.
-record(handshake_request, {username, public_key}).
-record(handshake_response, {public_key}).

% Запись запроса для получения ближайшего пира
-record(find_peer_request, {id}).
-record(send_message, {message}).

% Запись для получения ключей шифрования от сервера
-record(get_keys_request, {secret_key}).

% secret_key - ключ, для обращения к серверу
% Запись запроса для проверки соединения с пиром
-record(ping_request, {sender}).
