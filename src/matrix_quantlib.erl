-module(matrix_quantlib).
-export([init/0,
		version/0,
		svd/1,
		matrix/2,
		transpose/1,
		inv/1,
		matmul/2
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

matmul(_,_)->
	exit(nif_library_not_loaded).

transpose(_)->
	exit(nif_library_not_loaded).

inv(_)->
	exit(nif_library_not_loaded).

svd(_)->
	exit(nif_library_not_loaded).

matrix(Matrix, rc)->
	lists:map(fun(Row) -> lists:map(fun(El) -> float(El) end, Row) end, Matrix).
