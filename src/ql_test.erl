-module(ql_test).
-vsn('2016.0324').
-author('simon.klassen@adyne.com').
-export([start/0,test_mmultiply/0,test_svd/0,matrix/2,test_get_sabr_params/0]).

% From R
% SVD<-matrix(c(c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0),c(-5.0,-4.0,-3.0,-2.0,-1.0,0.0,1.0,2.0,3.0,4.0)),ncol=2)
% CHG<-matrix(c(NA,NA,3.0,NA,NA,NA,NA,6.0,NA,NA))
% 
% M1<-SVD[!is.na(CHG),]
% M2<-CHG[!is.na(CHG)]
% 
% A1<-solve(M1)%*%M2
% 
% SVD%*%A1
% 

start() ->
	ql:version().

test_transpose()->
	ql:transpose([[1.0,2.0,3.0],[4.0,5.0,6.0]]).

test_mmultiply()->
	M1 = matrix([[2,1,-1], [2,2,0]], rc),
	M2 = matrix([[1,0,2,1], [0,0,1,-1], [2, -1, 0, 2] ], rc),
	ql:mmultiply([[2.0,1.0,-1.0],[2.0,2.0,0.0]],[[1.0,0.0,2.0,1.0],[0.0,0.0,1.0,-1.0],[2.0,-1.0,0.0,2.0]]). 

test_svd()->
	ql:svd([[1.0, 0.9, 0.7],[0.9, 1.0, 0.4], [0.7, 0.4, 1.0]]).

test_inverse()->
	ql:inverse([[1.0, 0.9, 0.7],[0.9, 1.0, 0.4], [0.7, 0.4, 1.0]]).

matrix(Matrix, rc)->
	lists:map(fun(Row) -> 
					  lists:map(fun(El) -> float(El) end, Row) end, Matrix).

svd_interp()->
	SVD=[[0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0],[-5.0,-4.0,-3.0,-2.0,-1.0,0.0,1.0,2.0,3.0,4.0],[-5.0,-4.0,-3.0,-2.0,-1.0,0.0,-1.0,-2.0,-3.0,-4.0]],
	CHG=[na,na,3.0,na,na,na,na,6.0,na,na],
	N=length([X||X<-CHG,X/=na]),
	M0=lists:sublist(SVD,N),
	M1=[ element(2,lists:unzip(element(1,lists:partition(fun({X,Y})->X/=na end,lists:zip(CHG,M))))) || M<-M0 ],
	M2=ql:transpose([[X||X<-CHG,X/=na]]),
	A1=ql:mmultiply(ql:transpose(ql:inverse(M1)),M2),
	ql:mmultiply(ql:transpose(M0),A1).
	

test_get_sabr_params()->
	ql:get_sabr_params(
	  0.000000001,
	  [0.0802,0.3302,0.5802,0.8302,1.0802,1.3302,1.5802,1.8302,2.0802,2.3302,2.5802,2.8302,3.0802,3.3302,3.5802],
	  [123.05,98.21,73.37,48.53,17.28,55.25,84.62,113.99,143.35,172.72,202.09,231.45,260.82,290.19,319.55],
	  1.0802,
	  0.02,
	  0.0,
	  0.2,
	  6.0,
	  0.2).
