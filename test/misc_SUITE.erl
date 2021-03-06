-module(misc_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([test_erlmemfs_debug/1,
	 test_close_file_with_bad_reference/1,
	 test_list_of_missing_file/1,
	 test_list_of_single_file/1,
	 test_dir_file_collision/1,
	 test_gen_call_fs/1]).

all() -> [test_erlmemfs_debug,
	  test_close_file_with_bad_reference,
	  test_list_of_missing_file,
	  test_list_of_single_file,
	  test_dir_file_collision,
	  test_gen_call_fs].

test_erlmemfs_debug(_Config) ->
    application:ensure_started(erlmemfs),
    {ok, Fs} = erlmemfs_sup:create_erlmemfs(),
    Root = erlmemfs_support:root(),
    {ok, Root} = erlmemfs:debug(Fs).

test_close_file_with_bad_reference(_Config) ->
    application:ensure_started(erlmemfs),
    {ok, Fp} = erlmemfs_file_sup:create_erlmemfs_file(<<1,2,3,4>>),
    BadRef = erlang:make_ref(),
    {error, missing} = erlmemfs_file:close(Fp, BadRef).

test_list_of_missing_file(_Config) ->
    application:ensure_started(erlmemfs),
    {ok, Fs} = erlmemfs_sup:create_erlmemfs(),
    MissingFile = "abc",
    {error, missing} = erlmemfs:list_files(Fs, MissingFile).

test_list_of_single_file(_Config) ->
    application:ensure_started(erlmemfs),
    {ok, Fs} = erlmemfs_sup:create_erlmemfs(),
    Name = "my_file.txt",
    Data = <<1,2,3,4>>,
    {ok, Name} = erlmemfs:put_file(Fs, Name, Data),
    {ok, [Name]} = erlmemfs:list_files(Fs, Name).

test_dir_file_collision(_Config) ->
    application:ensure_started(erlmemfs),
    {ok, Fs} = erlmemfs_sup:create_erlmemfs(),
    Name = "abc",
    Data = <<1,2,3,4>>,
    {ok, Name} = erlmemfs:put_file(Fs, Name, Data),
    {error, file_collision} = erlmemfs:make_directory(Fs, Name),
    {error, file_collision} = erlmemfs:make_directory(Fs, "/"++Name).

test_gen_call_fs(_Config) ->
    application:ensure_started(erlmemfs),
    {ok, Fs} = erlmemfs_sup:create_erlmemfs(),
    {error, bad_call} = gen_server:call(Fs, bad_call).
