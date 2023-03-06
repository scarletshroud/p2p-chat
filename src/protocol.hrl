% Протокол передачи данных: username - имя пользователя, message - пользовательское сообщение, broadcast_list - список, клиентов, которые уже получили сообщение
-record(packet, {username, message, broadcast_list}).

% Запрос для обновления дерева подключенных узлов на сервере. peers - новое дерево подключенных узлов, secret_key - ключ, для обращения к серверу
-record(update_peers_request, {peers, secret_key}).

% Запрос для обновления публичных ключей шифрования на сервере. new_keys - Обновленные ключа шифрования, secret_key - ключ, для обращения к серверу
-record(update_keys_request, {new_keys, secret_key}).

% Запрос на добавление публичного ключа шифрования.
% new_key - публичный ключ шифрования, secret_key - ключ, для обращения к серверу
-record(add_key_request, {username, new_key, secret_key}).

% Запрос/ответ для получения дерева подключенных узлов от сервера.
-record(get_peers_request, {peer = #peer{}}).
-record(get_peers_response, {peers}).

% Запрос/ответ для получения ближайшего узла
-record(find_closest_request, {peer = #peer{}, id}).
% result - ближайший пир
-record(find_closest_response, {result}). 

% Записи запрос/ответ для обмена публичными ключами шифрования между двумя узлами.
-record(handshake_request, {username, public_key}).
-record(handshake_response, {public_key}).

% Запись запроса для получения ближайшего узла
-record(find_peer_request, {id}).
-record(send_message, {message}).

% Запись для получения ключей шифрования от сервера
-record(get_keys_request, {secret_key}).

% secret_key - ключ, для обращения к серверу
% Запись запроса для проверки соединения с узлом
-record(ping_request, {sender}).
