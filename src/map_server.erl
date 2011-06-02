%%% -------------------------------------------------------------------
%%% Author  : Stan Reshatnyk
%%% Description :
%%%
%%% Created : 30.03.2011
%%% -------------------------------------------------------------------
-module(map_server).
-version("0").

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, call/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {map, players}).

-define(SERVER, ?MODULE).
-define(MAP_WIDTH, 20).
-define(MAP_HEIGHT, 20).

% OOO
%OOOOO
%OOPOO
%OOOOO
% OOO
-define(VISIBLE_AREA, [{-1,-1},{1,-1},{-1,1},{-1,0},{0,-1},{0,0},{1,0},{0,1},{1,1}]).

%% ====================================================================
%% External functions
%% ====================================================================


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	log:log("Map is starting..."),
	process_flag(trap_exit, true),
	State = #state{map=populate(), players=dict:new()},
	log:log("Map server started ~px~p", [?MAP_WIDTH, ?MAP_HEIGHT]),
    {ok, State}.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% MOVE
handle_call({move, Player, {X,Y}}, _From, State) ->
	{TileX, TileY} = fix_tile_location({X,Y}),
	%TileX = X,
	%TileY = Y,
	Map = State#state.map,
	Players= State#state.players,
	OldLocation = dict:fetch(Player, Players),
	case dict:is_key({TileX,TileY}, Map) of
        true ->
			OldPlayersInTile = dict:fetch(OldLocation, Map),
			case lists:member(Player, OldPlayersInTile) of
				true ->
					NewPlayersInTile = lists:delete(Player, OldPlayersInTile),
					Map1 = dict:store(OldLocation, NewPlayersInTile, Map),
					Map2 = dict:append({TileX,TileY}, Player, Map1),
					NewPlayers = dict:store(Player, {TileX,TileY}, Players),
					OldTileTrace = dict:fetch(OldLocation, Map2),
					NewTileTrace = dict:fetch({TileX,TileY}, Map2),
					log:log("Map after move ~p~p -> ~p~p", [OldLocation, OldTileTrace, {TileX,TileY}, NewTileTrace]),
					{reply, {TileX,TileY}, State#state{map=Map2, players=NewPlayers}};
				false ->
					{reply, error_player_not_found, State}
			end;
        false ->
            {reply, error_unknown_tile, State}
    end;

%% GET MAP
handle_call({get_map, Player}, _From, State) ->
	VisibleList = lists:map(
	  	fun({DX,DY}) ->
			{PlayerX, PlayerY} = dict:fetch(Player,State#state.players),
			Location = fix_tile_location({PlayerX + DX,PlayerY + DY}),
			Data = dict:fetch(Location, State#state.map),
			{Location,Data}
    	end, ?VISIBLE_AREA),
	%DDD = dict:from_list(VisibleList),
	{reply, VisibleList, State};

%% ADD player if not exist
handle_call({add, Player}, _From, State) ->
	case dict:find(Player, State#state.players) of
		{ok, Location} ->
			log:log("Player found ~p ~p", [Player, Location]),
			{reply, Location, State};
		error ->
			Location = {1, 1}, % initial position
			log:log("Player added ~p ~p", [Player, Location]),
			NewMap = dict:append(Location, Player, State#state.map),
			NewPlayers = dict:store(Player, Location, State#state.players),
			{reply, Location, State#state{map=NewMap,players=NewPlayers}}
	end;

handle_call(Request, _From, Map) ->
	log:log("handle_call~p", [Request]),
    Reply = ok,
    {reply, Reply, Map}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	log:log("Code change in map server"),
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% API %%

call(Request) ->
	Answer = gen_server:call(?SERVER, Request),
	Answer.

%% UTILS %%

populate() -> 
	populate(?MAP_WIDTH, ?MAP_HEIGHT, dict:new()).
populate(0, 0, Map) -> 
	dict:store({0, 0}, [], Map);
populate(Width, 0, Map) -> 
	populate(Width - 1, ?MAP_HEIGHT, dict:store({Width, 0}, [], Map));
populate(Width, Height, Map) -> 
	populate(Width, Height - 1, dict:store({Width, Height}, [], Map)).

fix_tile_location({X,Y}) ->
	{(X + ?MAP_WIDTH) rem ?MAP_WIDTH, (Y + ?MAP_HEIGHT) rem ?MAP_HEIGHT}.

get_visible_map([], VisibleMap, _Map) ->
	VisibleMap;
get_visible_map(Tiles, VisibleMap, Map) ->
	[Location|T] = Tiles,
	get_visible_map(T, dict:store(Location, dict:fetch(Location, Map), VisibleMap), Map).
	