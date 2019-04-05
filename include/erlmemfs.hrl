-record(dir, {
	      parent :: term(),
	      name :: string(),
	      content = #{} :: map()
	     }).

-record(file, {
	       name :: string(),
	       fp :: binary()
	      }).

-define(ROOT, #dir{name="/", parent=none}).

-type dir() :: #dir{}.
-type file() :: #file{}.
