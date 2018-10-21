#ifndef NIFUTILS_H
#define NIFUTILS_H
#include<ql/math/matrix.hpp>
using namespace QuantLib;
namespace nifutils
{
  
  std::vector<std::vector<Real>> matrix_to_vector(Matrix ql_matrix); 

}

#endif
