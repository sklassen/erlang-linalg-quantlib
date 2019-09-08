/* ql_nif.c */

#include <ql/qldefines.hpp>
#include <ql/cashflow.hpp>
#include <boost/function.hpp>
#include <ql/math/matrix.hpp>
#include <ql/math/matrixutilities/svd.hpp>
#include "ql_linalg.h"
#include <iostream>
#include <iomanip>

using namespace QuantLib;


ERL_NIF_TERM matrix_transpose(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	std::vector<std::vector<Real>> matrixA;

	try {
		nifpp::get_throws(env, argv[0], matrixA);
	} catch (std::exception &e) {
    	return enif_make_tuple2(env,enif_make_atom(env,"error"),enif_make_string(env, e.what(), ERL_NIF_LATIN1));
	}    

	size_t rA, cA;
	rA = matrixA.size();
	cA = matrixA[0].size();

	Matrix A(rA,cA);
	for(size_t i=0; i<rA; ++i)
		for(size_t j=0; j<cA; ++j)
			A[i][j] = (float) matrixA[i][j];

	try {
		Matrix tA = transpose(A);

		std::vector<std::vector<Real>> M;
		M = nifutils::matrix_to_vector(tA);

		return nifpp::make(env,M);
	} catch (std::exception &e) {
		return enif_make_tuple2(env,enif_make_atom(env,"error"),enif_make_string(env, e.what(), ERL_NIF_LATIN1));
    }

}


ERL_NIF_TERM matrix_inv(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	std::vector<std::vector<Real>> matrixA;

	try {
		nifpp::get_throws(env, argv[0], matrixA);
	} catch (std::exception &e) {
		return enif_make_tuple2(env,enif_make_atom(env,"error"),enif_make_string(env, e.what(), ERL_NIF_LATIN1));
	}    

	size_t rA, cA;
	rA = matrixA.size();
	cA = matrixA[0].size();

	Matrix A(rA,cA);
	for(size_t i=0; i<rA; ++i)
		for(size_t j=0; j<cA; ++j)
			A[i][j] = (float) matrixA[i][j];
	
	try {
		Real det = determinant (A);
		QL_REQUIRE (! close (det ,0.0) , " Non invertible matrix !");
		Matrix invA = inverse (A);

		std::vector<std::vector<Real>> M;
		M = nifutils::matrix_to_vector(invA);

		return nifpp::make(env,M);
	} catch (std::exception &e) {
		return enif_make_tuple2(env,enif_make_atom(env,"error"),enif_make_string(env, e.what(), ERL_NIF_LATIN1));
    }

}

ERL_NIF_TERM matrix_matmul(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	std::vector<std::vector<Real>> matrixA;
	std::vector<std::vector<Real>> matrixB;
	try {
		nifpp::get_throws(env, argv[0], matrixA);
		nifpp::get_throws(env, argv[1], matrixB);
	} catch (std::exception &e) {
		return enif_make_tuple2(env,enif_make_atom(env,"error"),enif_make_string(env, e.what(), ERL_NIF_LATIN1));
	}    

	size_t rA, rB, cA, cB;
	rA = matrixA.size();
	cA = matrixA[0].size();

	rB = matrixB.size();
	cB = matrixB[0].size();

	// Matrix declaration and population
	Matrix A1(rA,cA);
	for(size_t i=0; i<rA; ++i)
		for(size_t j=0; j<cA; ++j)
			A1[i][j] = (float) matrixA[i][j];

	Matrix B1(rB,cB);
	for(size_t i=0; i<rB; ++i)
		for(size_t j=0; j<cB; ++j)
			B1[i][j] = (float) matrixB[i][j];

	try {
		Matrix res = A1 * B1;

		std::vector<std::vector<Real>> M;
		M = nifutils::matrix_to_vector(res);

		return nifpp::make(env,M);
	} catch (std::exception &e) {
		return enif_make_tuple2(env,enif_make_atom(env,"error"),enif_make_string(env, e.what(), ERL_NIF_LATIN1));
	}    
	
}


// svd
/// returns a tuple with {{u, Umatrix}, {v, Vmatrix}, {diag, SVD_Diag}}
ERL_NIF_TERM svd(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

	std::vector<std::vector<Real>> matrix_for_svd;

	try {
		nifpp::get_throws(env, argv[0], matrix_for_svd);
	} catch (std::exception &e) {
		return enif_make_tuple2(env,enif_make_atom(env,"error"),enif_make_string(env, e.what(), ERL_NIF_LATIN1));
	}    

	size_t row, col;
	// for some reason I alwasy get wrong rows (one more) so changing the code to reduce it by one
	row = matrix_for_svd.size();
	col = matrix_for_svd[0].size();

	// Matrix declaration and population
	Matrix A(row,col);
	for(size_t i=0; i<row; ++i)
		for(size_t j=0; j<col; ++j)
			A[i][j] = (float) matrix_for_svd[i][j];

	try {
		// Check if the matrix is invertible
		Real det = determinant (A);
		QL_REQUIRE (! close (det ,0.0) , " Non invertible matrix !");
		Matrix invA = inverse (A);
		SVD svdDec (A);
		Matrix UMatrix = svdDec.U();
		Matrix VMatrix = svdDec.V();

		Array DiagMatrix = svdDec.singularValues();
		Size Array_size = DiagMatrix.size();
		std::vector<Real> DiagMatrix_vector(Array_size);

		for(Size x =0; x<Array_size; x++)
			DiagMatrix_vector[x] = DiagMatrix[x];
	
		std::vector<std::vector<Real>> UMatrix_erl, VMatrix_erl, DiagMatrix_erl;
	
		UMatrix_erl = nifutils::matrix_to_vector(UMatrix);
		VMatrix_erl = nifutils::matrix_to_vector(UMatrix);
	
		ERL_NIF_TERM U_erl = enif_make_tuple2(env, enif_make_atom(env, "u"), nifpp::make(env, UMatrix_erl));
		ERL_NIF_TERM V_erl = enif_make_tuple2(env, enif_make_atom(env, "v"), nifpp::make(env, VMatrix_erl));
		ERL_NIF_TERM Diag_erl =	enif_make_tuple2(env, enif_make_atom(env, "d"), nifpp::make(env, DiagMatrix_vector));
		return enif_make_tuple3(env, U_erl, V_erl, Diag_erl);
	} catch (std::exception &e) {
		return enif_make_tuple2(env,enif_make_atom(env,"error"),enif_make_string(env, e.what(), ERL_NIF_LATIN1));
	}    
}
