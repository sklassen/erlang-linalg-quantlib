#include"nifpp.h"
#include"ql_linalg.h"
#include "ql/version.hpp"

// ql:version().
static ERL_NIF_TERM version_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	try {
		return enif_make_string(env, QL_LIB_VERSION, ERL_NIF_LATIN1);
	} catch (...) {
		return enif_make_tuple2(env,enif_make_atom(env,"error"),enif_make_string(env, "FAILED", ERL_NIF_LATIN1));
	}
}


static ErlNifFunc nif_funcs[] = {
  {"version", 0, version_nif},
  {"matmul", 2, matrix_matmul},
  {"transpose", 1, matrix_transpose},
  {"inv", 1, matrix_inv},
  {"svd", 1, svd},
};

  
ERL_NIF_INIT(matrix_quantlib, nif_funcs, NULL, NULL, NULL, NULL)
