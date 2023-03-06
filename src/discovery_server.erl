-module(discovery_server).

-behaviour(gen_server).

-include("definitions.hrl").
-include("protocol.hrl").

% Описание полей записи State
% connected_peers - список подключенных пиров к discovery server
-record(state, {connected_peers}).

-export(
  [
	  join_network/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    stop/0,
    start/0,
    ping/0
  ]
).

%%%
%%% Public API
%%%

join_network(Peer) -> gen_server:call({?MODULE, 'discovery@127.0.0.1'}, Peer).

ping() -> gen_server:call({?MODULE, 'discovery@127.0.0.1'}, ping).

stop() -> gen_server:cast({?MODULE, 'discovery@127.0.0.1'}, stop).

%%%
%%% GenServer Implementation
%%%

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, #state{connected_peers = []}, []).

init(State) -> {ok, State}.

handle_cast(stop, State) -> {stop, normal, State}.

handle_call(ping, _From, State) ->
  {reply, {ok, "Connection can be established"}, State};

handle_call(Peer, _From, #state{connected_peers = ConnectedPeers}) ->
  Response =
    case ConnectedPeers of
      [] -> {discover, nil};

      _ ->
        RandomPeer = lists:nth(rand:uniform(length(ConnectedPeers)), ConnectedPeers),
        {discover, RandomPeer#peer.server_pid}
    end,
  {reply, Response, #state{connected_peers = [Peer | ConnectedPeers]}}.

handle_info(Info, State) ->
  error_logger:info_msg("~p~n", [Info]),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) ->
  error_logger:info_msg("Terminating..~n"),
  ok.
