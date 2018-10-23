-module(matrix_quantlib).
-export([init/0,
		version/0,
		svd/1,
		matrix/2,
		transpose/1,
		inverse/1,
		multiply/2
		]).
-on_load(init/0).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


init() ->
	Directory=filename:dirname(code:which(matrix_quantlib)),
    erlang:load_nif(Directory++"/../priv/ql_nif", 0).

version() -> 
	exit(nif_library_not_loaded).

multiply(_,_)->
	exit(nif_library_not_loaded).

transpose(_)->
	exit(nif_library_not_loaded).

inverse(_)->
	exit(nif_library_not_loaded).

svd(_)->
	exit(nif_library_not_loaded).

matrix(Matrix, rc)->
	lists:map(fun(Row) -> lists:map(fun(El) -> float(El) end, Row) end, Matrix).

-ifdef(TEST).

transpose_test() ->
    transpose([[1.0,2.0],[3.0,4.0]])==[[1.0,3.0],[2.0,4.0]].

multiply_test()->
    multiply([[1.0,2.0],[3.0,4.0]],[[1.0,3.0],[2.0,4.0]])==[[5.0,11.0],[11.0,25.0]].

-endif.


