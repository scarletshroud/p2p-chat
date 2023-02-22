-module(mpeer).
-export([add_peer/2, closest_peer/2, closest_peer_in_network/3, create_peers/2, join/0]).

-include("definitions.hrl").
-include("protocol.hrl").

add_peer(_, {Size, Tree}) when Size >= ?id_size ->
	{Size, Tree};
add_peer(Peer, Peers) ->
	gb_trees:enter(Peer#peer.id, {Peer#peer.username, Peer#peer.server_pid, Peer#peer.client_pid}, Peers).

closest_peer(SearchKey, {_, Tree}) ->
	closest_peer(SearchKey, Tree);
closest_peer(SearchKey, {Key, {Username, ServerPid, ClientPid}, _, _}) when SearchKey == Key ->
	#peer{id=Key, username = Username, server_pid = ServerPid, client_pid = ClientPid};
closest_peer(SearchKey, {Key, {Username, ServerPid, ClientPid}, nil, _}) when SearchKey < Key ->
	#peer{id=Key, username = Username, server_pid = ServerPid, client_pid = ClientPid};
closest_peer(SearchKey, {Key, {Username, ServerPid, ClientPid}, _, nil}) when SearchKey > Key ->
	#peer{id=Key, username = Username, server_pid = ServerPid, client_pid = ClientPid};
closest_peer(SearchKey, {Key, _, Smaller, _}) when SearchKey < Key ->
	closest_peer(SearchKey, Smaller); 
closest_peer(SearchKey, {Key, _, _, Bigger}) when SearchKey > Key ->
	closest_peer(SearchKey, Bigger).

closest_peer_in_network(Peer, _, Id) when Peer#peer.id == Id ->
	Peer;
closest_peer_in_network(Peer, Peers, Id) ->
	Result = closest_peer(Id, Peers),
	case Result#peer.id == Id of
		true ->
			Result;
		false ->
			recursive_find_closest(Peer, Id, nil, Result, ?max_hops)
	end.

recursive_find_closest(_, _, _, Result, 0) ->
	Result;
recursive_find_closest(_, _, Result, Result, _) ->
	Result;
recursive_find_closest(Result, _, _, Result, _) ->
	Result;
recursive_find_closest(Peer, Id, _, Result, RemainingHops) ->
	case server:find_closest(Result#peer.server_pid, #find_closest_request{mypid=Peer#peer.client_pid, peer=Peer, id=Id}) of 
		#find_closest_response{result = NewResult} ->
			recursive_find_closest(Peer, Id, Result, NewResult, RemainingHops - 1);
		_ ->
			{error, "Unable to find the closest peer."}
	end.

create_peers(Peer, Peers) ->
	BestPeers = calc:best_peers(Peer#peer.id),
	BestNetworkPeers = lists:map(fun(Id) -> closest_peer_in_network(Peer, Peers, Id) end, BestPeers),
	NewPeers = lists:foldl(fun(NetworkPeer, NewPeers) -> add_peer(NetworkPeer, NewPeers) end, gb_trees:empty(), BestNetworkPeers),
	PeersList = lists:foldl(fun({Id, {Username, ServerId, ClientId}}, PeersList) -> [#peer{id=Id, username=Username, server_pid=ServerId, client_pid=ClientId}|PeersList] end, [], gb_trees:to_list(Peers)),
	lists:foldl(fun(OldPeer, UpdatedPeers) -> add_peer(OldPeer, UpdatedPeers) end, NewPeers, PeersList).

join() ->
	io:fwrite("~s~n", ["Enter your username"]),
	case io:fread("", "~s") of 
		{ok, Username} -> 
			SecretKey = rand:uniform(?max_secret_key),
			Peer = init_peer(Username, SecretKey),

			try discovery_server:join_network(Peer) of 
				Response ->
					case Response of 
						{discover, RemotePid} -> 
							case RemotePid of 
								nil ->
									io:format("To connect to the client use the following PID: ~w~n", [Peer#peer.client_pid]);
								_ ->
									case server:get_peers(RemotePid, #get_peers_request{mypid=self(), peer=Peer}) of 
										#get_peers_response{peers=RemotePeers} ->
											NewPeers = mpeer:create_peers(Peer, RemotePeers),
											server:update_peers(Peer#peer.server_pid, #update_peers_request{peers=NewPeers, secret_key = SecretKey}),
											io:format("To connect to the client use the following PID: ~w~n", [Peer#peer.client_pid]);
										_ ->
											io:format("Unable to get peers. Timeout.")
									end
							end;

						_ -> 
							io:format("~p Discovery (~p) timed out~n", [self(), Peer])
					end
		
			catch 
				_:_ ->
					Pids = read_pids(),
					Pid = find_available_peer(Pids),
					NewPeers = mpeer:create_peers(Peer, client:get_peers(Peer, Pid)),
					server:update_peers(Peer#peer.server_pid, #update_peers_request{peers=NewPeers, secret_key = SecretKey}),
					io:format("To connect to the client use the following PID: ~w~n", [Peer#peer.client_pid])
			end;

		{error, Error} -> Error
	end.

find_available_peer([]) ->
	{error, "Unable to find avaible peer"};

find_available_peer(Pids) ->
	StringPid = lists:nth(rand:uniform(length(Pids)), Pids), 
	Pid = erlang:list_to_pid(StringPid),
	try server:ping(Pid, #ping_request{sender = self()}) of 
		{ok, _} ->
			Pid
	catch 
		_:_ ->
			find_available_peer(lists:delete(StringPid, Pids))
	end. 

read_pids() ->
	{ok, Fd} = file:open("peers", [read]),
	try get_all_lines(Fd, [])
		after file:close(Fd)
	end.

get_all_lines(Device, Lines) ->
    case io:get_line(Device, "") of
        eof  -> Lines;
        Line -> get_all_lines(Device, [string:chomp(Line)| Lines])
    end.

init_peer(Username, SecretKey) ->
	Id = calc:id(),
	EmptyPeers = gb_trees:empty(),

	Keys = crypto:generate_key(rsa, {1024, 3}),

	case server:start_link(EmptyPeers, SecretKey, Keys) of
		{ok, ServerPid} ->

			case client:start_link(Id, Username, SecretKey, ServerPid, Keys) of 
				
				{ok, ClientPid} -> 
					Peers = add_peer(#peer{id = Id, username = Username, server_pid = ServerPid, client_pid = ClientPid}, EmptyPeers),
					server:update_peers(ServerPid, #update_peers_request{peers=Peers, secret_key = SecretKey}),
					Peer = #peer{id=Id, username=Username, server_pid=ServerPid, client_pid=ClientPid},
					Peer; 

				{error, Error} -> Error
			end;
			
		{error, Error} -> Error
	end.

