-module(linalg_quantlib_tests). 
-import(linalg_quantlib,[transpose/1,inv/1,matmul/2]).
-include_lib("eunit/include/eunit.hrl").

transpose_1_test() ->
	?assert(transpose([[8.0]])=:=[[8.0]]).

transpose_2_test() ->
	?assert(transpose([[1.0,2.0],[3.0,4.0]])==[[1.0,3.0],[2.0,4.0]]).

transpose_3_test() ->
	?assert(transpose([[1.0,2.0,3.0],[4.0,5.0,6.0]])==[[1.0,4.0],[2.0,5.0],[3.0,6.0]]).

inv_1_test()->
	?assert(inv([[8.0]])==[[0.125]]).

inv_2_test()->
	?assert(inv([[1.0,0.0],[0.0,2.0]])==[[1.0,0.0],[0.0,0.5]]).

inv_3_test()->
	?assert(inv([[1.0,0.0,1.0],[0.0,2.0,1.0],[1.0,1.0,1.0]])==[[-1.0,-1.0,2.0],[-1.0,0.0,1.0],[2.0,1.0,-2.0]]).

matmul_test()->
    [
    ?assert(matmul([[2.0]], [[3.0]])==[[6.0]]),
    ?assert(matmul([[1.0,2.0],[3.0,4.0]], [[1.0,3.0],[2.0,4.0]])==[[5.0,11.0],[11.0,25.0]])
    ].

