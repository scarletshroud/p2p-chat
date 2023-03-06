-module(server).

-export(
  [
    start/3,
    stop/1,
    update_peers/2,
    get_peers/2,
    find_closest/2,
    get_keys/2,
    update_keys/2,
    add_key/2, 
    ping/2,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
  ]
).

-include("definitions.hrl").
-include("protocol.hrl").

-behaviour(gen_server).

-define(SERVER, ?MODULE).

% Описание полей записи State
% peer - запись, которая описывает текущий узел (id, username, server_pid, client_pid)
% peers - список узлов, с которыми установлено соединение
% secret_key - секретный ключ, для обращения к клиенту
% keys - публичные ключи шифрования узлов, с которыми уже было установлено соединение
% public_key - публичный ключ шифрования.
% private_key - приватный ключ шифрования
-record(state, {peer, peers, secret_key, keys, public_key, private_key}).

%%%
%%% Public API
%%%

update_peers(ServerPid, UpdatePeersRequest) -> gen_server:cast(ServerPid, UpdatePeersRequest).

get_peers(ServerPid, GetPeersRequest) -> gen_server:call(ServerPid, GetPeersRequest).

find_closest(ServerPid, FindClosestRequest) -> gen_server:call(ServerPid, FindClosestRequest).

update_keys(ServerPid, UpdateKeysRequest) -> gen_server:cast(ServerPid, UpdateKeysRequest).

add_key(ServerPid, AddKeyRequest) -> gen_server:cast(ServerPid, AddKeyRequest).

get_keys(ServerPid, GetKeysRequest) -> gen_server:call(ServerPid, GetKeysRequest).

ping(ClientPid, PingRequest) -> gen_server:call(ClientPid, PingRequest).

%%%
%%% GenServer Implementation
%%%

start(Peers, SecretKey, Keys) ->
  {PublicKey, PrivateKey} = Keys,
  gen_server:start_link(?MODULE, {Peers, SecretKey, PublicKey, PrivateKey}, []).

stop(ServerPid) -> gen_server:cast(ServerPid, stop).

init({Peers, SecretKey, PublicKey, PrivateKey}) ->
  {
    ok,
    #state{
      peers = Peers,
      secret_key = SecretKey,
      keys = maps:new(),
      public_key = PublicKey,
      private_key = PrivateKey
    }
  }.

handle_cast(stop, State) -> {stop, normal, State};

handle_cast(#update_peers_request{peers = ReceivedPeers, secret_key = SecretKey}, State) ->
  case State#state.secret_key =:= SecretKey of
    true ->
      {
        noreply,
        #state{
          peers = ReceivedPeers,
          secret_key = State#state.secret_key,
          keys = State#state.keys,
          public_key = State#state.public_key,
          private_key = State#state.private_key
        }
      }
  end;

handle_cast(#update_keys_request{new_keys = NewKeys, secret_key = SecretKey}, State) ->
  case State#state.secret_key =:= SecretKey of
    true ->
      {
        noreply,
        #state{
          peers = State#state.peers,
          secret_key = State#state.secret_key,
          keys = NewKeys,
          public_key = State#state.public_key,
          private_key = State#state.private_key
        }
      }
  end;

handle_cast(#add_key_request{username = Username, new_key = NewKey, secret_key = SecretKey}, State) ->
  case State#state.secret_key =:= SecretKey of
    true ->
      {
        noreply,
        #state{
          peers = State#state.peers,
          secret_key = State#state.secret_key,
          keys = maps:put(Username, NewKey, State#state.keys),
          public_key = State#state.public_key,
          private_key = State#state.private_key
        }
      }
  end.

handle_call(#get_peers_request{peer = Peer}, _From, State) ->
  {
    reply,
    #get_peers_response{peers = State#state.peers},
    #state{
      peers = mpeer:add_peer(Peer, State#state.peers),
      secret_key = State#state.secret_key,
      keys = State#state.keys,
      public_key = State#state.public_key,
      private_key = State#state.private_key
    }
  };

handle_call(#get_keys_request{secret_key = SecretKey}, _From, State) ->
  case State#state.secret_key =:= SecretKey of
    true -> {reply, State#state.keys, State};
    false -> {reply, {error, "Invalid key"}, State}
  end;

handle_call(
  #find_closest_request{peer = Peer, id = Id},
  _From,
  State
) ->
  Result = mpeer:closest_peer(Id, State#state.peers),
  {
    reply,
    #find_closest_response{result = Result},
    #state{
      peers = mpeer:add_peer(Peer, State#state.peers),
      secret_key = State#state.secret_key,
      keys = State#state.keys,
      public_key = State#state.public_key,
      private_key = State#state.private_key
    }
  };

handle_call(#ping_request{sender = _}, _From, State) ->
  {reply, {ok, "Connection can be established"}, State}.


handle_info(Info, State) ->
  error_logger:info_msg("~p~n", [Info]),
  {noreply, State}.


terminate(_Reason, _State) ->
  error_logger:info_msg("Stoping the server..~n"),
  ok.


code_change(_OldVsn, State, _Extra) -> {ok, State}.
