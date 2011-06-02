%% Author: Stan Reshetnyk
%% Created: 10.04.2011
%% Description: Collection helper
-module(collections).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([index_of/2, get_value_by_prop/2]).

%%
%% API Functions
%%
index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).


get_value_by_prop(Property, List) ->
	[{Property, Value}] = lists:filter(fun(V)->
						case V of
							{Property,_} ->
								true;
							_ ->
								false
						end
					end, List),
	Value.
%%
%% Local Functions
%%

