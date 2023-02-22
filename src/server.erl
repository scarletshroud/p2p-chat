-module(server).

-export(
[                      
	start_link/3,                
	stop/1,
	update_peers/2,
	accept_message/2,
	get_peers/2, 
	find_closest/2,
	handshake/2,
	get_keys/2,
	update_keys/2,
	ping/2,
	init/1,                    
	handle_call/3,              
	handle_cast/2,               
	handle_info/2,               
	terminate/2,                 
	code_change/3
]). 
           
-include("definitions.hrl").
-include("protocol.hrl").

-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {peer, peers, secret_key, keys, public_key, private_key}). 

start_link(Peers, SecretKey, Keys) ->
	{PublicKey, PrivateKey} = Keys,
	gen_server:start_link(
		?MODULE,
		{Peers, SecretKey, PublicKey, PrivateKey},
		[]
	).

stop(ServerPid) ->
	gen_server:cast(ServerPid, stop).

update_peers(ServerPid, UpdatePeersRequest) -> 
	gen_server:cast(ServerPid, UpdatePeersRequest). 

get_peers(ServerPid, GetPeersRequest) ->
	gen_server:call(ServerPid, GetPeersRequest).

find_closest(ServerPid, FindClosestRequest) -> 
	gen_server:call(ServerPid, FindClosestRequest).	

accept_message(ServerPid, #packet{username = Username, message = Message}) ->
	gen_server:cast(ServerPid, #packet{username = Username, message = Message}).

update_keys(ServerPid, UpdateKeysRequest) ->
	gen_server:cast(ServerPid, UpdateKeysRequest). 

handshake(ServerPid, HandshakeRequest) ->
	gen_server:call(ServerPid, HandshakeRequest).

get_keys(ServerPid, GetKeysRequest) ->
	gen_server:call(ServerPid, GetKeysRequest).

ping(ClientPid, PingRequest) ->
	gen_server:call(ClientPid, PingRequest).

init({Peers, SecretKey, PublicKey, PrivateKey}) ->
	{ok, #state{peers = Peers, secret_key = SecretKey, keys = maps:new(), public_key = PublicKey, private_key = PrivateKey}}.

handle_cast(stop, State) ->
	{
		stop, 
		normal, 
		State
	};

handle_cast(#update_peers_request{peers=ReceivedPeers, secret_key=SecretKey}, #state{peers = _, secret_key = ServerSecretKey, keys = Keys, public_key = PublicKey, private_key = PrivateKey}) ->
	case ServerSecretKey =:= SecretKey of 
		true ->
			{
				noreply, 
				#state{peers = ReceivedPeers, secret_key = ServerSecretKey, keys = Keys, public_key = PublicKey, private_key = PrivateKey}
			}
	end;

handle_cast(#update_keys_request{new_keys = NewKeys, secret_key = SecretKey}, #state{peers = Peers, secret_key = ServerSecretKey, keys = _, public_key = PublicKey, private_key = PrivateKey}) ->
	case ServerSecretKey =:= SecretKey of 
		true ->	
			{
				noreply, 
				#state{peers = Peers, secret_key = ServerSecretKey, keys = NewKeys, public_key = PublicKey, private_key = PrivateKey}
			}
	end;

handle_cast(#packet{username = EncryptedUsername, message = EncryptedMessage}, State) ->

	DecryptedUsername = crypto:private_decrypt(rsa, EncryptedUsername, State#state.private_key, []),
	DecryptedMesssage = crypto:private_decrypt(rsa, EncryptedMessage, State#state.private_key, []),

	io:fwrite("~nReceiver Pid: ~w~nSender: ~s~nMessage: ~s~n", [self(), DecryptedUsername, DecryptedMesssage]),
	{
		noreply, 
		State
	}.

handle_call(#get_peers_request{mypid=_, peer=Peer}, _From, #state{peers = Peers, secret_key = SecretKey, keys = Keys, public_key = PublicKey, private_key = PrivateKey}) ->
	{
		reply,
		#get_peers_response{peers = Peers},
		#state{peers = mpeer:add_peer(Peer, Peers), secret_key = SecretKey, keys = Keys, public_key = PublicKey, private_key = PrivateKey}
	};

handle_call(#get_keys_request{secret_key = SecretKey}, _From, State) ->
	case State#state.secret_key =:= SecretKey of 
		true ->
			{
				reply,
				State#state.keys,
				State
			};

		false ->
			{
				reply,
				{error, "Invalid key"},
				State
			}
	end;

handle_call(#find_closest_request{mypid=_, peer=Peer, id=Id}, _From, #state{peers = Peers, secret_key = SecretKey, keys = Keys, public_key = PublicKey, private_key = PrivateKey}) ->	
	Result = mpeer:closest_peer(Id, Peers),
	{
		reply, 
		#find_closest_response{result=Result},
		#state{peers = mpeer:add_peer(Peer, Peers), secret_key = SecretKey, keys = Keys, public_key = PublicKey, private_key = PrivateKey}
	};

handle_call(#handshake_request{username = Username, public_key = NewPublicKey}, _From, #state{peers = Peers, secret_key = SecretKey, keys = Keys, public_key = PublicKey, private_key = PrivateKey}) -> 
	{
		reply, 
		PublicKey,
		#state{peers = Peers, secret_key = SecretKey, keys = maps:put(Username, NewPublicKey, Keys), public_key = PublicKey, private_key = PrivateKey}
	};

handle_call(#ping_request{sender = _}, _From, State) ->
	{
		reply,
		{ok, "Connection can be established"}, 
		State
	}.

handle_info(Info, State) ->
	error_logger:info_msg("~p~n", [Info]),
	{noreply, State}. 

terminate(_Reason, _State) ->
	error_logger:info_msg("Stoping the server..~n"),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
