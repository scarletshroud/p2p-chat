-module(client).

-behaviour(gen_server).

% peer - непосредственно запись, которая описывает текущий пир.
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
    start_link/5,
    stop/1,
    get_peers/1,
    get_peers/2,
    find_peer/2,
    refresh/0,
    send_message/2,
    close_connection/2,
    send/6
  ]
).

start_link(Id, Username, SecretKey, ServerPid, Keys) ->
  {PublicKey, PrivateKey} = Keys,
  gen_server:start_link(
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


get_peers(ClientPid) -> gen_server:call(ClientPid, get_peers).

find_peer(ClientPid, FindPeerRequest) -> gen_server:call(ClientPid, FindPeerRequest).

refresh() -> gen_server:cast(refresh).

close_connection(ClientPid, Peer) -> gen_server:cast(ClientPid, Peer).

send_message(ClientPid, Message) -> gen_server:cast(ClientPid, #send_message{message = Message}).

%% Private Functions %%

send(SenderUsername, PublicKey, Receivers, Message, Keys, Broadcast) ->
  lists:foldl(
    fun
      ({Username, ServerPid, _}, Params) ->
        {NewKeys, BroadcastList} = Params,
        try maps:get(Username, NewKeys) of
          Key ->
            EncryptedUsername = crypto:public_encrypt(rsa, list_to_binary(SenderUsername), Key, []),
            EncryptedMessage = crypto:public_encrypt(rsa, list_to_binary(Message), Key, []),
            NewBroadcastList =
              server:accept_message(
                ServerPid,
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
              server:handshake(
                ServerPid,
                #handshake_request{username = SenderUsername, public_key = PublicKey}
              ),
            RefreshedKeys = maps:put(Username, HandshakeResponse, NewKeys),
            EncryptedUsername =
              crypto:public_encrypt(rsa, list_to_binary(SenderUsername), HandshakeResponse, []),
            EncryptedMessage =
              crypto:public_encrypt(rsa, list_to_binary(Message), HandshakeResponse, []),
            NewBroadcastList =
              server:accept_message(
                ServerPid,
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
    server:get_peers(Peer#peer.server_pid, #get_peers_request{mypid = self(), peer = Peer}),
  case Response of
    #get_peers_response{peers = Peers} ->
      save_to_file(gb_trees:values(gb_trees:delete(Peer#peer.id, Peers)));

    _ -> io:format("")
  end.


get_peers(Peer, ServerPid) ->
  case server:get_peers(ServerPid, #get_peers_request{mypid = self(), peer = Peer}) of
    #get_peers_response{peers = Peers} -> Peers;
    _ -> io:format("Unable to get peers. Timeout.")
  end.

%% OTP Handlers %%

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
  #state{peer = Peer, secret_key = SecretKey, public_key = PublicKey, private_key = PrivateKey}
) ->
  Peers = get_peers(Peer, Peer#peer.server_pid),
  NewPeers = gb_trees:delete(Id, Peers),
  case server:get_keys(Peer#peer.server_pid, #get_keys_request{secret_key = SecretKey}) of
    {error, Message} -> io:format("~s~n", [Message]);

    Keys ->
      server:update_keys(
        Peer#peer.server_pid,
        #update_keys_request{new_keys = maps:remove(Username, Keys), secret_key = SecretKey}
      )
  end,
  server:update_peers(
    Peer#peer.server_pid,
    #update_peers_request{peers = NewPeers, secret_key = SecretKey}
  ),
  {
    noreply,
    #state{peer = Peer, secret_key = SecretKey, public_key = PublicKey, private_key = PrivateKey}
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
    server:get_peers(Peer#peer.server_pid, #get_peers_request{mypid = self(), peer = Peer}),
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
  get_peers,
  _From,
  #state{peer = Peer, secret_key = SecretKey, public_key = PublicKey, private_key = PrivateKey}
) ->
  Peers = get_peers(Peer, Peer#peer.server_pid),
  {
    reply,
    Peers,
    #state{peer = Peer, secret_key = SecretKey, public_key = PublicKey, private_key = PrivateKey}
  };

handle_call(
  #find_peer_request{id = Id},
  _From,
  #state{peer = Peer, secret_key = SecretKey, public_key = PublicKey, private_key = PrivateKey}
) ->
  Peers = get_peers(Peer, Peer#peer.server_pid),
  ClosestPeer = mpeer:closest_peer(Id, Peers),
  {
    reply,
    ClosestPeer,
    #state{peer = Peer, secret_key = SecretKey, public_key = PublicKey, private_key = PrivateKey}
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
