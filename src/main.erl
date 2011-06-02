%% Author: iam
%% Created: 26.02.2011
%% Description: TODO: Add description to main
-module(main).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0, stop/0]).

%%
%% API Functions
%%
start() -> application:start(tcp_server).

stop() -> application:stop().


%%
%% Local Functions
%%

