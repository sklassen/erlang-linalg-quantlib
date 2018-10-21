#include<nifutils.h>
namespace nifutils{

  
  std::vector<std::vector<Real>> matrix_to_vector(Matrix ql_matrix) {

	int rows, cols;
	rows = ql_matrix.rows();
	cols = ql_matrix.columns();
	std::vector<std::vector<Real>> result(rows, std::vector<Real>(cols));
	for (int r=0; r<rows; r++)
	  for(int c=0; c<cols; c++)
		result[r][c] = ql_matrix[r][c];
	return result;
  }


}
