#include"ql_svd.h"
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
  {"multiply", 2, matrix_mult},
  {"transpose", 1, matrix_transpose},
  {"inverse", 1, matrix_inverse},
  {"svd", 1, svd},
};

  
ERL_NIF_INIT(matrix_quantlib, nif_funcs, NULL, NULL, NULL, NULL)
