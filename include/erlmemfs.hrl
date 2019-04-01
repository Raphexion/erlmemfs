-record(dir, {
	  parent :: any(),
	  name :: string(),
	  content = #{}:: map()
	 }).

-record(file, {
	  name :: string(),
	  content :: binary()
	 }).
