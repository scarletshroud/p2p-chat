-module(client).

-behaviour(gen_server).

% Описание записи State
% peer - запись, которая описывает текущий узел (id, username, server_pid, client_pid)
% secret_key - ключ, для обращения к серверу.
% public_key - публичный ключ шифрования.
% private_key - приватный ключ шифрования
-record(state, {peer, secret_key, public_key, private_key}).

-define(CLIENT, ?MODULE).

-include("definitions.hrl").
-include("protocol.hrl").

-export(
  [
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    start/5,
    stop/1,
    get_peers/2,
    handshake/2,
    accept_message/2,
    send_message/2,
    refresh/0,
    close_connection/2
  ]
).

%%%
%%% Public API
%%%

send_message(ClientPid, Message) -> gen_server:cast(ClientPid, #send_message{message = Message}).

handshake(ClientPid, HandshakeRequest) -> gen_server:call(ClientPid, HandshakeRequest).

accept_message(ClientPid, Packet) -> gen_server:call(ClientPid, Packet).

close_connection(ClientPid, Peer) -> gen_server:cast(ClientPid, Peer).

refresh() -> gen_server:cast(refresh).

%%%
%%% GenServer Implementation
%%%

start(Id, Username, SecretKey, ServerPid, Keys) ->
  {PublicKey, PrivateKey} = Keys,
  gen_server:start_link(
    {local, ?MODULE},
    ?MODULE,
    {#peer{id = Id, username = Username, server_pid = ServerPid}, SecretKey, PublicKey, PrivateKey},
    []
  ).

stop(ClientPid) -> gen_server:cast(ClientPid, stop).

init(
  {
    #peer{id = Id, username = Username, server_pid = ServerPid, client_pid = _},
    SecretKey,
    PublicKey,
    PrivateKey
  }
) ->
  erlang:send_after(?refresh_time, self(), trigger),
  {
    ok,
    #state{
      peer = #peer{id = Id, username = Username, server_pid = ServerPid, client_pid = self()},
      secret_key = SecretKey,
      public_key = PublicKey,
      private_key = PrivateKey
    }
  }.

handle_cast(
  refresh,
  #state{peer = Peer, secret_key = SecretKey, public_key = PublicKey, private_key = PrivateKey}
) ->
  Peers = get_peers(Peer, Peer#peer.server_pid),
  NewPeers = mpeer:create_peers(Peer, Peers),
  server:update_peers(
    Peer#peer.server_pid,
    #update_peers_request{peers = NewPeers, secret_key = SecretKey}
  ),
  {
    noreply,
    #state{peer = Peer, secret_key = SecretKey, public_key = PublicKey, private_key = PrivateKey}
  };

handle_cast(
  #peer{id = Id, username = Username, server_pid = _, client_pid = _},
  State
) ->
  Peer = State#state.peer,
  Peers = get_peers(Peer, Peer#peer.server_pid),
  NewPeers = gb_trees:delete(Id, Peers),
  case server:get_keys(Peer#peer.server_pid, #get_keys_request{secret_key = State#state.secret_key}) of
    {error, Message} -> io:format("~s~n", [Message]);

    Keys ->
      server:update_keys(
        Peer#peer.server_pid,
        #update_keys_request{new_keys = maps:remove(Username, Keys), secret_key = State#state.secret_key}
      )
  end,
  server:update_peers(
    Peer#peer.server_pid,
    #update_peers_request{peers = NewPeers, secret_key = State#state.secret_key}
  ),
  {
    noreply,
    State
  };

handle_cast(#send_message{message = Message}, State) ->
  Peers = get_peers(State#state.peer, State#state.peer #peer.server_pid),
  case Peers of
    [] -> [];

    _ ->
      NewTree = gb_trees:delete(State#state.peer #peer.id, Peers),
      case
      server:get_keys(
        State#state.peer #peer.server_pid,
        #get_keys_request{secret_key = State#state.secret_key}
      ) of
        {error, Message} -> io:format("~s~n", [Message]);

        Keys ->
          {RefreshedKeys, _} =
            send(
              State#state.peer #peer.username,
              State#state.public_key,
              gb_trees:values(NewTree),
              Message,
              Keys,
              gb_trees:keys(Peers)
            ),
          server:update_keys(
            State#state.peer #peer.server_pid,
            #update_keys_request{new_keys = RefreshedKeys, secret_key = State#state.secret_key}
          )
      end
  end,
  {noreply, State};

handle_cast(stop, State) ->
  Peer = State#state.peer,
  save_data(Peer),
  Response =
    server:get_peers(Peer#peer.server_pid, #get_peers_request{peer = Peer}),
  case Response of
    #get_peers_response{peers = Peers} ->
      PeersList = gb_trees:values(gb_trees:delete(Peer#peer.id, Peers)),
      lists:foreach(
        fun ({_, _, ClientPid}) -> client:close_connection(ClientPid, Peer) end,
        PeersList
      );

    _ -> io:format("Unable to get peers from server ~w", [Peer#peer.server_pid])
  end,
  server:stop(Peer#peer.server_pid),
  {stop, normal, State}.

handle_call(
  #packet{username = EncryptedUsername, message = EncryptedMessage, broadcast_list = BroadcastList},
  _From,
  State
) ->
  try
    Peer = State#state.peer,
    DecryptedUsername =
      binary_to_list(crypto:private_decrypt(rsa, EncryptedUsername, State#state.private_key, [])),
    DecryptedMesssage =
      binary_to_list(crypto:private_decrypt(rsa, EncryptedMessage, State#state.private_key, [])),
    io:fwrite(
      "~nReceiver: ~s~nSender: ~s~nMessage: ~s~n",
      [Peer#peer.username, DecryptedUsername, DecryptedMesssage]
    ),
    #get_peers_response{peers = Peers} = server:get_peers(Peer#peer.server_pid, #get_peers_request{peer = Peer}), 
    Receivers =
      lists:foldl(
        fun
          (Key, Acc) ->
            case gb_trees:is_empty(Acc) of
              true -> Acc;
              false -> try gb_trees:delete(Key, Acc) catch _:_ -> Acc end
            end
        end,
        Peers,
        BroadcastList
      ),
    case gb_trees:is_empty(Receivers) of
      true ->
        {
          reply,
          BroadcastList,
          State
        };

      false ->
        NewBroadcastList = lists:append(BroadcastList, gb_trees:keys(Receivers)),
        {RefreshedKeys, _} =
          send(
            DecryptedUsername,
            State#state.public_key,
            gb_trees:values(Receivers),
            DecryptedMesssage,
            server:get_keys(Peer#peer.server_pid, #get_keys_request{secret_key = State#state.secret_key}),
            NewBroadcastList
          ),
        server:update_keys(Peer#peer.server_pid, #update_keys_request{new_keys = RefreshedKeys, secret_key = State#state.secret_key}),
        {
          reply,
          NewBroadcastList,
          State
        }
    end
  catch
    _:_ ->
      {
        reply,
        BroadcastList,
        State
      }
  end;

handle_call(#handshake_request{username = Username, public_key = NewPublicKey}, _From, State) ->
  server:add_key(State#state.peer#peer.server_pid, #add_key_request{username = Username, new_key = NewPublicKey, secret_key = State#state.secret_key}),
  {
    reply,
    State#state.public_key,
    State
  };

handle_call(#find_peer_request{id = Id}, _From, State) ->
  Peer = State#state.peer,
  Peers = get_peers(Peer, Peer#peer.server_pid),
  ClosestPeer = mpeer:closest_peer(Id, Peers),
  {
    reply,
    ClosestPeer,
    State
  }.

handle_info(trigger, State) ->
  Peer = State#state.peer,
  Peers = get_peers(Peer, Peer#peer.server_pid),
  NewPeers = mpeer:create_peers(Peer, Peers),
  server:update_peers(
    Peer#peer.server_pid,
    #update_peers_request{peers = NewPeers, secret_key = State#state.secret_key}
  ),
  erlang:send_after(?refresh_time, self(), trigger),
  {noreply, State};

handle_info(Info, State) ->
  error_logger:info_msg("~p~n", [Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  server:stop(_State#state.peer #peer.server_pid),
  error_logger:info_msg("Stoping the client..~n"),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%
%%% Private Functions
%%%

send(SenderUsername, PublicKey, Receivers, Message, Keys, Broadcast) ->
  lists:foldl(
    fun
      ({Username, _, ClientPid}, Params) ->
        {NewKeys, BroadcastList} = Params,
        try maps:get(Username, NewKeys) of
          Key ->
            EncryptedUsername = crypto:public_encrypt(rsa, list_to_binary(SenderUsername), Key, []),
            EncryptedMessage = crypto:public_encrypt(rsa, list_to_binary(Message), Key, []),
            NewBroadcastList =
              client:accept_message(
                ClientPid,
                #packet{
                  username = EncryptedUsername,
                  message = EncryptedMessage,
                  broadcast_list = BroadcastList
                }
              ),
            {NewKeys, NewBroadcastList}
        catch
          _:_ ->
            HandshakeResponse =
              client:handshake(
                ClientPid,
                #handshake_request{username = SenderUsername, public_key = PublicKey}
              ),
            RefreshedKeys = maps:put(Username, HandshakeResponse, NewKeys),
            EncryptedUsername = crypto:public_encrypt(rsa, list_to_binary(SenderUsername), HandshakeResponse, []),
            EncryptedMessage = crypto:public_encrypt(rsa, list_to_binary(Message), HandshakeResponse, []),
            NewBroadcastList =
              client:accept_message(
                ClientPid,
                #packet{
                  username = EncryptedUsername,
                  message = EncryptedMessage,
                  broadcast_list = BroadcastList
                }
              ),
            {RefreshedKeys, NewBroadcastList}
        end
    end,
    {Keys, Broadcast},
    Receivers
  ).

save_to_file(Peers) ->
  {ok, Fd} = file:open("peers", [write]),
  lists:foreach(
    fun ({_, ServerPid, _}) -> file:write(Fd, io_lib:fwrite("~p\n", [ServerPid])) end,
    Peers
  ).

save_data(Peer) ->
  Response =
    server:get_peers(Peer#peer.server_pid, #get_peers_request{peer = Peer}),
  case Response of
    #get_peers_response{peers = Peers} ->
      save_to_file(gb_trees:values(gb_trees:delete(Peer#peer.id, Peers)));

    _ -> io:format("")
  end.


get_peers(Peer, ServerPid) ->
  case server:get_peers(ServerPid, #get_peers_request{peer = Peer}) of
    #get_peers_response{peers = Peers} -> Peers;
    _ -> io:format("Unable to get peers. Timeout.")
  end.