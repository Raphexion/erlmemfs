-module(misc_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([test_erlmemfs_debug/1]).

all() -> [test_erlmemfs_debug].

test_erlmemfs_debug(_Config) ->
    erlmemfs_sup:start_link(),
    {ok, Fs} = erlmemfs_sup:create_erlmemfs(),
    {ok, erlmemfs_support:root()} =:= erlmemfs:debug(Fs).
