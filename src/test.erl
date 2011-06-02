%% Author: iam
%% Created: 24.04.2011
%% Description: TODO: Add description to test_map_server
-module(test).


-define(TEST_PLAYER, 111).
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0]).

%%
%% API Functions
%%

start() ->
	map_server:call({add, ?TEST_PLAYER}),
	map_server:call({get_map, ?TEST_PLAYER}),
	map_server:call({move, ?TEST_PLAYER, {2,2}}),
	ok.

%%
%% Local Functions
%%

