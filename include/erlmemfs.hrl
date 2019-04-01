-record(dir, {
	  parent :: term(),
	  name :: string(),
	  content = #{} :: map()
	 }).

-record(file, {
	  name :: string(),
	  content :: binary()
	 }).

-type dir() :: #dir{}.
-type file() :: #file{}.
