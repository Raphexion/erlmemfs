[![Build Status](https://travis-ci.org/Raphexion/erlmemfs.svg?branch=master)](https://travis-ci.org/Raphexion/erlmemfs)
[![codecov.io](https://codecov.io/gh/Raphexion/erlmemfs/coverage.svg?branch=master)](https://codecov.io/gh/Raphexion/erlmemfs?branch=master)

erlmemfs
========

A simple in-memory filesystem inspirered and intended to be used
with Bifrost.

Use property testing to test functionality.

[Property based testing](https://pragprog.com/book/fhproper/property-based-testing-with-proper-erlang-and-elixir)

Getting Started
---------------

```sh
rebar3 shell

{ok, Fs} = erlmemfs_sup:create_erlmemfs().

erlmemfs:make_directory(Fs, "my_folder").
erlmemfs:change_directory(Fs, "my_folder").

erlmemfs:put_file(Fs, "my_file", <<1,2,3,4>>).

erlmemfs:list_files(Fs).
```

Testing and cover
-----------------

```sh
rebar3 proper --cover
rebar3 cover
```
