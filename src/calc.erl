-module(calc).

-export([id/0, best_peers/1, distance/2]).

-include("definitions.hrl").

% Функция для расчета id пользователя
id() ->
  <<Id:?id_size/integer>> = crypto:hash(sha, crypto:strong_rand_bytes(20)),
  Id.

% Функция для расчета дистанции между двумя узлами. Использует идентификаторы узлов. Алгоритм kademlia. 
distance(Peer1_id, Peer2_id) -> Peer1_id bxor Peer2_id.

% Функция для поиска наилучших узлов
best_peers(Id) -> best_peers(Id, 1, [Id], ?id_size - 1).

best_peers(_, _, Ids, 0) -> Ids;
best_peers(Id, Distance, Ids, N) -> best_peers(Id, Distance * 2, [Id bxor Distance | Ids], N - 1).
