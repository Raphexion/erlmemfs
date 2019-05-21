-module(misc_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([test_erlmemfs_debug/1,
	 test_close_file_with_bad_reference/1]).

all() -> [test_erlmemfs_debug,
	  test_close_file_with_bad_reference].

test_erlmemfs_debug(_Config) ->
    application:ensure_started(erlmemfs),
    {ok, Fs} = erlmemfs_sup:create_erlmemfs(),
    {ok, erlmemfs_support:root()} =:= erlmemfs:debug(Fs).

test_close_file_with_bad_reference(_Config) ->
    application:ensure_started(erlmemfs),
    {ok, Fp} = erlmemfs_file_sup:create_erlmemfs_file(<<1,2,3,4>>),
    BadRef = erlang:make_ref(),
    {error, missing} = erlmemfs_file:close(Fp, BadRef).
