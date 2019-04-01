-module(stats).
-include("erlmemfs.hrl").

-export([count/1]).

count(#file{}) ->
    #{file => 1, dir => 0};
count(Node=#dir{parent=none}) ->
    count(Node, 0);
count(Node=#dir{}) ->
    count(Node, 1).

%%------------------------------------------------------------------------------
%% Private
%%------------------------------------------------------------------------------

combine(#{file := F0, dir := D0}, #{file := F1, dir := D1}) ->
    #{file => F0 + F1, dir => D0 + D1}.

count(#dir{content=Content}, Dir) ->
    %% root node should not count towards
    %% count, hence Dir=0. Otherwise Dir=1
    lists:foldl(fun combine/2,
		#{file => 0, dir => Dir},
		lists:map(fun count/1, maps:values(Content))).
