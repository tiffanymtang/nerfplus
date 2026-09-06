#include <Rcpp.h>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>
using namespace Rcpp;


// Convert Rcpp::String to int
int rcpp_string_to_int(String rcpp_string) {
  std::string std_string = rcpp_string.get_cstring();
  return std::stoi(std_string);
}


// Convert Rcpp::CharacterVector to Rcpp::IntegerVector
IntegerVector char_to_int(CharacterVector char_vector) {
  int n = char_vector.size();
  IntegerVector int_vector(n);
  for (int i = 0; i < n; ++i) {
    std::string str = as<std::string>(char_vector(i));
    int_vector(i) = std::stoi(str);
  }
  return int_vector;
}


// Check if integer is in an Rcpp::IntegerVector
bool isin_int(int element, IntegerVector vector) {
  IntegerVector::iterator it = std::find(vector.begin(), vector.end(), element);
  return it != vector.end();
}


// Split Rcpp::String by comma
CharacterVector split_string_by_comma(String rcpp_string) {
  std::string std_string = rcpp_string.get_cstring();
  std::istringstream ss(std_string);
  std::string token;
  CharacterVector result;
  while (std::getline(ss, token, ',')) {
    result.push_back(token);
  }
  return result;
}


std::unordered_map<int, int> make_index_map(IntegerVector values) {
  std::unordered_map<int, int> out;
  out.reserve(values.size());
  for (int i = 0; i < values.size(); ++i) {
    out[values[i]] = i;
  }
  return out;
}


std::unordered_map<int, int> make_path_map(List tree_paths) {
  CharacterVector path_names = tree_paths.names();
  std::unordered_map<int, int> out;
  out.reserve(tree_paths.size());
  for (int i = 0; i < tree_paths.size(); ++i) {
    out[std::stoi(as<std::string>(path_names[i]))] = i;
  }
  return out;
}


std::unordered_set<int> make_int_set(IntegerVector values) {
  std::unordered_set<int> out;
  out.reserve(values.size());
  for (int i = 0; i < values.size(); ++i) {
    out.insert(values[i]);
  }
  return out;
}


std::unordered_set<int> parse_int_set(String rcpp_string) {
  std::string std_string = rcpp_string.get_cstring();
  std::istringstream ss(std_string);
  std::string token;
  std::unordered_set<int> out;
  while (std::getline(ss, token, ',')) {
    out.insert(std::stoi(token));
  }
  return out;
}


// [[Rcpp::export]]
NumericMatrix extract_psi_cpp(NumericMatrix x,
                              IntegerVector node_preds, 
                              List tree_paths,
                              IntegerVector node_ids, 
                              IntegerVector split_vars,
                              NumericVector split_vals) {
  int n = node_preds.size();
  int n_nodes = node_ids.size();
  std::unordered_map<int, int> node_id_to_idx = make_index_map(node_ids);
  std::unordered_map<int, int> terminal_to_path_idx = make_path_map(tree_paths);
  
  NumericMatrix psi(n, n_nodes);
  for (int i = 0; i < n; ++i) {
    int terminal_node = node_preds[i];
    IntegerVector tree_path = as<IntegerVector>(tree_paths[terminal_to_path_idx[terminal_node]]);
    for (IntegerVector::iterator path_node = tree_path.begin(); path_node != tree_path.end(); ++path_node) {
      int j = node_id_to_idx[*path_node];
      if (x(i, split_vars[j]) > split_vals[j]) {
        psi(i, j) = 1;
      } else {
        psi(i, j) = -1;
      }
    }
  }
  
  return psi;
}


// [[Rcpp::export]]
NumericMatrix extract_psi_chr_cpp(NumericMatrix x,
                                  IntegerVector node_preds, 
                                  List tree_paths,
                                  IntegerVector node_ids, 
                                  IntegerVector split_vars,
                                  CharacterVector split_vals,
                                  IntegerVector unordered_factors) {
  int n = node_preds.size();
  int n_nodes = node_ids.size();
  std::unordered_map<int, int> node_id_to_idx = make_index_map(node_ids);
  std::unordered_map<int, int> terminal_to_path_idx = make_path_map(tree_paths);
  std::unordered_set<int> unordered_factor_set = make_int_set(unordered_factors);
  std::vector<int> ordered_split_vals(n_nodes);
  std::vector<std::unordered_set<int> > unordered_split_vals(n_nodes);
  std::vector<bool> is_unordered_node(n_nodes, false);
  for (int j = 0; j < n_nodes; ++j) {
    if (unordered_factor_set.find(split_vars[j]) != unordered_factor_set.end()) {
      is_unordered_node[j] = true;
      unordered_split_vals[j] = parse_int_set(split_vals[j]);
    } else {
      ordered_split_vals[j] = std::stoi(as<std::string>(split_vals[j]));
    }
  }
  
  NumericMatrix psi(n, n_nodes);
  for (int i = 0; i < n; ++i) {
    int terminal_node = node_preds[i];
    IntegerVector tree_path = as<IntegerVector>(tree_paths[terminal_to_path_idx[terminal_node]]);
    for (IntegerVector::iterator path_node = tree_path.begin(); path_node != tree_path.end(); ++path_node) {
      int j = node_id_to_idx[*path_node];
      int split_var = split_vars[j];
      if (is_unordered_node[j]) {
        int x_val = static_cast<int>(x(i, split_var));
        if (unordered_split_vals[j].find(x_val) != unordered_split_vals[j].end()) {
          psi(i, j) = 1;
        } else {
          psi(i, j) = -1;
        }
      } else {
        if (x(i, split_var) > ordered_split_vals[j]) {
          psi(i, j) = 1;
        } else {
          psi(i, j) = -1;
        }
      }
    }
  }
  
  return psi;
}

