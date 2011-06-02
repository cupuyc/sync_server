%% Author: Stan Reshetnyk
%% Created: 23.04.2011
%% Description: TODO: Add description to log
-module(log).

-export([log/1,log/2]).

log(Msg) ->
    log(Msg,[]).
log(Msg, Args) when is_list(Args) ->
	io:format("Map: " ++ Msg ++ "~n", Args);
log(Msg, Args) ->
	io:format("Map: " ++ Msg ++ "~n", [Args]).