#ifndef ql_svd_hpp_
#define ql_svd_hpp_
#include "nifpp.h"
#include "nifutils.h"
ERL_NIF_TERM matrix_transpose(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM matrix_inverse(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM matrix_mult(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM svd(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
#endif
