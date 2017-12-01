// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <unordered_set>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector symDiff(IntegerVector v1, IntegerVector v2) {

  std::sort(v1.begin(), v1.end());
  std::sort(v2.begin(), v2.end());

  std::vector<int> out;
  std::set_symmetric_difference(v1.begin(), v1.end(), v2.begin(), v2.end(), std::back_inserter(out));

  return wrap(out);
}

// [[Rcpp::export]]
IntegerVector setDiff(IntegerVector v1, IntegerVector v2) {

  std::sort(v1.begin(), v1.end());
  std::sort(v2.begin(), v2.end());

  std::vector<int> out;
  std::set_difference(v1.begin(), v1.end(), v2.begin(), v2.end(), std::back_inserter(out));

  return wrap(out);
}

// [[Rcpp::export]]
List processNodes(int j, List nodes) {

  int length_nodes = nodes.size();
  List out_list(length_nodes-(j+1));

  IntegerVector node_check = nodes[j];

  for (int i = j+1; i < length_nodes; i++) {
    IntegerVector nodes_i = nodes[i];
    IntegerVector out_item = symDiff(nodes_i, node_check);
    out_list[i-(j+1)] = out_item;
  }

  return out_list;
}

// [[Rcpp::export]]
IntegerVector finalizeMatrix(IntegerMatrix x, int i) {

  int ncol = x.ncol();

  // make nodes: start
  List nodes(i);

  for (int j = 0; j < i; j++) {
    IntegerVector vector2_1 = x( _, j);
    IntegerVector vector2_2(vector2_1.end() - (ncol - i), vector2_1.end());
    vector2_2.insert( vector2_2.end(), x(j,j) );
    nodes[j] = vector2_2;
  }
  // make nodes: end

  IntegerVector result(ncol);

  for (int j = 0; j < i; j++) {
    if (x(i-1,j) == 0) {
      List step_1(i-j);
      step_1 = processNodes(j, nodes);
      int length_step_1 = step_1.size();
      for (int l = 0; l < length_step_1; l++) {
        IntegerVector item_step_1 = step_1[l];
        int size_step_1 = item_step_1.size();
        if (size_step_1 == 2) {
          IntegerVector possible_entry;
          IntegerVector nodes_y;
          nodes_y = nodes[j];
          possible_entry = setDiff(item_step_1, nodes_y);
          result[j] = possible_entry[0];
          break;
        }
      }
    } else {
      result[j] = x(i-1,j);
    }
  }
  return result;
}

// [[Rcpp::export]]
IntegerMatrix finalizeMatrixOuter(IntegerMatrix x, int i) {

  for (int j = i; j > 0; j--) {
    IntegerVector fillin;
    fillin = finalizeMatrix(x, j);
    x(j-1, _) = fillin;
  }
  return x;
}
