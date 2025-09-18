#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

const double SMOOTH = 0.00000001;


// function to solve basic Least square with network cohesion, when covariates are provided.
arma::mat RNC_LS(arma::mat X, arma::mat Y, arma::sp_mat L, arma::sp_mat H,
                 arma::sp_mat W, double lambda_netcoh) {

  int n = X.n_rows;
  arma::sp_mat I_sp = arma::speye<arma::sp_mat>(n, n);
  arma::mat I = arma::eye<arma::mat>(n, n);

  //arma::superlu_opts settings;
  //settings.symmetric = true;
  //arma::mat inverse = spsolve(I_sp + lambda_netcoh * L, I, "superlu", settings);
  // uncomment above if superlu is available. We use the less efficient but by default available lapack.
  arma::mat inverse = arma::spsolve(W + lambda_netcoh * L, I, "lapack");
  arma::mat b = X.t() * (I - inverse) * Y;
  arma::mat A = X.t() * (I - W * inverse) * W * X + H;
  arma::mat beta = arma::solve(A, b);
  arma::mat alpha = inverse * W * (Y - X * beta);
  arma::mat result = arma::join_cols(alpha, beta);
  return result;
}

// The currently available 'as' functions from R sparse matrix to sp_mat all have some numerical issues.
// So we currently only support using the normal matrix as the format for Laplacian. Notice that this
// sacrifices some speed.
// [[Rcpp::export]]
List rnc_solver(NumericMatrix X, NumericMatrix Y,
                NumericMatrix L, NumericMatrix H, NumericMatrix W,
                double lambda_netcoh) {

  arma::mat Xmat(X.begin(), X.nrow(), X.ncol(), false); // false to avoid memory copy
  arma::mat Ymat(Y.begin(), Y.nrow(), Y.ncol(), false);
  arma::mat Lmat(L.begin(), L.nrow(), L.ncol(), false);
  arma::mat Hmat(H.begin(), H.nrow(), H.ncol(), false);
  arma::mat Wmat(W.begin(), W.nrow(), W.ncol(), false);

  arma::sp_mat L_sp = arma::sp_mat(Lmat);
  arma::sp_mat H_sp = arma::sp_mat(Hmat);
  arma::sp_mat W_sp = arma::sp_mat(Wmat);

  arma::mat result = RNC_LS(Xmat, Ymat, L_sp, H_sp, W_sp, lambda_netcoh);
  return Rcpp::List::create(
    Rcpp::Named("alpha") = result.head_rows(Xmat.n_rows),
    Rcpp::Named("beta") = result.tail_rows(result.n_rows - Xmat.n_rows)
  );
}


// function to solve basic Least square with network cohesion, when covariates are provided.
arma::mat RNC_LS_Naive(arma::mat X, arma::mat Y, arma::sp_mat M, arma::sp_mat W) {

  int n = X.n_rows;
  arma::mat I = arma::eye<arma::mat>(n, n);
  arma::mat X_tilde = arma::join_rows(I, X);
  arma::sp_mat X_tilde_sp = arma::sp_mat(X_tilde);
  arma::sp_mat A = X_tilde_sp.t() * W * X_tilde_sp + M;
  arma::mat result = spsolve(A, X_tilde.t() * W * Y, "lapack");
  return result;
}

// [[Rcpp::export]]
List rnc_solver_naive(NumericMatrix X, NumericMatrix Y,
                      NumericMatrix L, NumericMatrix H, NumericMatrix W,
                      double lambda_netcoh) {

  arma::mat Xmat(X.begin(), X.nrow(), X.ncol(), false); // false to avoid memory copy
  arma::mat Ymat(Y.begin(), Y.nrow(), Y.ncol(), false);
  arma::mat Lmat(L.begin(), L.nrow(), L.ncol(), false);
  arma::mat Hmat(H.begin(), H.nrow(), H.ncol(), false);
  arma::mat Wmat(W.begin(), W.nrow(), W.ncol(), false);
  arma::sp_mat W_sp = arma::sp_mat(Wmat);

  int n = Xmat.n_rows;
  int p = Xmat.n_cols;

  arma::mat Omega = arma::zeros<arma::mat>(n + p, n + p);
  Omega(arma::span(0, n - 1), arma::span(0, n - 1)) = lambda_netcoh * Lmat;
  Omega(arma::span(n, n + p - 1), arma::span(n, n + p - 1)) = Hmat;
  arma::sp_mat M_sp = arma::sp_mat(Omega);

  arma::mat result = RNC_LS_Naive(Xmat, Ymat, M_sp, W_sp);
  return Rcpp::List::create(
    Rcpp::Named("alpha") = result.head_rows(n),
    Rcpp::Named("beta") = result.tail_rows(result.n_rows - n)
  );
}


// function to solve basic Least square with network cohesion, when covariates are provided.
arma::mat RNC_nodeids_LS(arma::mat X, arma::mat Y, arma::sp_mat L,
                         arma::sp_mat H, arma::sp_mat W,
                         arma::mat X_alpha, arma::sp_mat X_alpha_sp,
                         double lambda_netcoh) {

  int n = X.n_rows;
  int n_alpha = L.n_rows;
  arma::mat I_n = arma::eye<arma::mat>(n, n);
  arma::mat I = arma::eye<arma::mat>(n_alpha, n_alpha);

  //arma::superlu_opts settings;
  //settings.symmetric = true;
  //arma::mat inverse = spsolve(I_sp + lambda_netcoh * L, I, "superlu", settings);
  // uncomment above if superlu is available. We use the less efficient but by default available lapack.
  arma::mat inverse = arma::spsolve(
    X_alpha_sp.t() * W * X_alpha_sp + lambda_netcoh * L, I, "lapack"
  );
  arma::mat b = X.t() * (I_n - X_alpha * inverse * X_alpha.t()) * Y;
  // TODO: fix this for non identity W matrix
  arma::mat A = X.t() * (W - X_alpha * inverse * X_alpha.t()) * X + H;
  // arma::mat A = X.t() * (W - X_alpha * W * inverse * W * X_alpha.t()) * X + H;
  arma::mat beta = arma::solve(A, b);
  // arma::mat alpha = inverse * W * X_alpha.t() * (Y - X * beta);
  arma::mat alpha = inverse * X_alpha.t() * (Y - X * beta);
  arma::mat result = arma::join_cols(alpha, beta);
  return result;
}

// The currently available 'as' functions from R sparse matrix to sp_mat all have some numerical issues.
// So we currently only support using the normal matrix as the format for Laplacian. Notice that this
// sacrifices some speed.
// [[Rcpp::export]]
List rnc_nodeids_solver(NumericMatrix X, NumericMatrix Y,
                        NumericMatrix L, NumericMatrix H, NumericMatrix W,
                        NumericMatrix X_alpha, double lambda_netcoh) {

  arma::mat Xmat(X.begin(), X.nrow(), X.ncol(), false); // false to avoid memory copy
  arma::mat Ymat(Y.begin(), Y.nrow(), Y.ncol(), false);
  arma::mat Lmat(L.begin(), L.nrow(), L.ncol(), false);
  arma::mat Hmat(H.begin(), H.nrow(), H.ncol(), false);
  arma::mat Wmat(W.begin(), W.nrow(), W.ncol(), false);
  arma::mat X_alphamat(X_alpha.begin(), X_alpha.nrow(), X_alpha.ncol(), false);

  arma::sp_mat L_sp = arma::sp_mat(Lmat);
  arma::sp_mat H_sp = arma::sp_mat(Hmat);
  arma::sp_mat W_sp = arma::sp_mat(Wmat);
  arma::sp_mat X_alpha_sp = arma::sp_mat(X_alphamat);

  arma::mat result = RNC_nodeids_LS(
    Xmat, Ymat, L_sp, H_sp, W_sp, X_alphamat, X_alpha_sp, lambda_netcoh
  );
  return Rcpp::List::create(
    Rcpp::Named("alpha") = result.head_rows(Lmat.n_rows),
    Rcpp::Named("beta") = result.tail_rows(result.n_rows - Lmat.n_rows)
  );
}


// function to solve basic Least square with network cohesion, when covariates are provided.
arma::mat RNC_nodeids_LS_Naive(arma::mat X, arma::mat Y,
                               arma::sp_mat M, arma::sp_mat W,
                               arma::mat X_alpha) {

  int n = X.n_rows;
  arma::mat X_tilde = arma::join_rows(X_alpha, X);
  arma::sp_mat X_tilde_sp = arma::sp_mat(X_tilde);
  arma::sp_mat A = X_tilde_sp.t() * W * X_tilde_sp + M;
  arma::mat result = spsolve(A, X_tilde.t() * W * Y, "lapack");
  return result;
}

// [[Rcpp::export]]
List rnc_nodeids_solver_naive(NumericMatrix X, NumericMatrix Y,
                              NumericMatrix L, NumericMatrix H, NumericMatrix W,
                              NumericMatrix X_alpha, double lambda_netcoh) {

  arma::mat Xmat(X.begin(), X.nrow(), X.ncol(), false); // false to avoid memory copy
  arma::mat Ymat(Y.begin(), Y.nrow(), Y.ncol(), false);
  arma::mat Lmat(L.begin(), L.nrow(), L.ncol(), false);
  arma::mat Hmat(H.begin(), H.nrow(), H.ncol(), false);
  arma::mat Wmat(W.begin(), W.nrow(), W.ncol(), false);
  arma::mat X_alphamat(X_alpha.begin(), X_alpha.nrow(), X_alpha.ncol(), false);
  arma::sp_mat W_sp = arma::sp_mat(Wmat);

  int n = Xmat.n_rows;
  int n_nodes = Lmat.n_rows;
  int p = Xmat.n_cols;

  arma::mat Omega = arma::zeros<arma::mat>(n_nodes + p, n_nodes + p);
  Omega(arma::span(0, n_nodes - 1), arma::span(0, n_nodes - 1)) = lambda_netcoh * Lmat;
  Omega(arma::span(n_nodes, n_nodes + p - 1), arma::span(n_nodes, n_nodes + p - 1)) = Hmat;
  arma::sp_mat M_sp = arma::sp_mat(Omega);

  arma::mat result = RNC_nodeids_LS_Naive(Xmat, Ymat, M_sp, W_sp, X_alphamat);
  return Rcpp::List::create(
    Rcpp::Named("alpha") = result.head_rows(n_nodes),
    Rcpp::Named("beta") = result.tail_rows(result.n_rows - n_nodes)
  );
}


// Create sparse diagonal matrix from a vector of values
arma::sp_mat create_sparse_diagonal(arma::vec values) {
  int n = values.size();
  arma::sp_mat sparse_diag_matrix(n, n);
  for (int i = 0; i < n; ++i) {
    sparse_diag_matrix(i, i) = values(i);
  }
  return sparse_diag_matrix;
}

// [[Rcpp::export]]
List rnc_solver_path(NumericMatrix X, NumericMatrix Y, NumericMatrix L,
                     double lambda_netcoh, List lambdas_x) {

  arma::mat Xmat(X.begin(), X.nrow(), X.ncol(), false); // false to avoid memory copy
  arma::mat Ymat(Y.begin(), Y.nrow(), Y.ncol(), false);
  arma::mat Lmat(L.begin(), L.nrow(), L.ncol(), false);
  arma::sp_mat L_sp = arma::sp_mat(Lmat);

  int n = Xmat.n_rows;
  int p = Xmat.n_cols;
  arma::sp_mat I_sp = arma::speye<arma::sp_mat>(n, n);
  arma::mat I = arma::eye<arma::mat>(n, n);

  arma::mat inverse = spsolve(I_sp + lambda_netcoh * L_sp, I, "lapack");
  arma::mat Xxinverse = Xmat.t() * (I - inverse);
  arma::mat A = Xxinverse * Xmat;
  arma::mat b = Xxinverse * Ymat;

  // Compute eigendecomposition
  arma::vec A_evalues;
  arma::mat A_evectors;
  arma::eig_sym(A_evalues, A_evectors, A);

  // Save some repeated computations
  arma::mat Utb = A_evectors.t() * b;
  arma::vec alpha1 = inverse * Ymat;
  arma::mat alpha2 = inverse * Xmat;

  int n_lambdas_x = lambdas_x.size();
  arma::mat betas = arma::zeros<arma::mat>(p, n_lambdas_x);
  arma::mat alphas = arma::zeros<arma::mat>(n, n_lambdas_x);
  for (int i = 0; i < n_lambdas_x; ++i) {
    NumericVector lambda_x = lambdas_x[i];
    arma::vec lambda_x_vec(lambda_x.begin(), lambda_x.size(), false);
    arma::vec diag_values = 1 / (A_evalues + lambda_x_vec);
    arma::sp_mat diag_mat = create_sparse_diagonal(diag_values);
    arma::vec beta = A_evectors * diag_mat * Utb;
    betas.col(i) = beta;
    arma::vec alpha = alpha1 - alpha2 * beta;
    alphas.col(i) = alpha;
  }

  return Rcpp::List::create(
    Rcpp::Named("alpha") = alphas,
    Rcpp::Named("beta") = betas
  );
}

double calculate_mse(arma::mat y, arma::vec yhat) {
  int n = yhat.size();
  double mse = 0.0;
  for (int i = 0; i < n; ++i) {
    double error = yhat[i] - y[i];
    mse += error * error;
  }
  mse /= n;
  return mse;
}

// [[Rcpp::export]]
List rnc_solver_path_predict(NumericMatrix X, NumericMatrix Y, NumericMatrix L,
                             NumericMatrix X_test, NumericMatrix Y_test,
                             NumericMatrix L22, NumericMatrix L21,
                             double lambda_netcoh, List lambdas_x, bool return_fit) {

  List fit_out = rnc_solver_path(X, Y, L, lambda_netcoh, lambdas_x);
  arma::mat alphas = fit_out["alpha"];
  arma::mat betas = fit_out["beta"];

  arma::mat X_test_mat(X_test.begin(), X_test.nrow(), X_test.ncol(), false); // false to avoid memory copy
  arma::mat Y_test_mat(Y_test.begin(), Y_test.nrow(), Y_test.ncol(), false);
  arma::mat L22_mat(L22.begin(), L22.nrow(), L22.ncol(), false);
  arma::mat L21_mat(L21.begin(), L21.nrow(), L21.ncol(), false);
  arma::sp_mat L22_sp = arma::sp_mat(L22_mat);

  arma::vec errs(alphas.n_cols, arma::fill::zeros);
  for (int j = 0; j < alphas.n_cols; ++j) {
    arma::vec test_alpha = spsolve(L22_sp, -L21_mat * alphas.col(j), "lapack");
    arma::vec prediction = test_alpha + X_test_mat * betas.col(j);
    errs[j] = calculate_mse(Y_test_mat, prediction);
  }

  if (return_fit) {
    return Rcpp::List::create(
      Rcpp::Named("alpha") = alphas,
      Rcpp::Named("beta") = betas,
      Rcpp::Named("errs") = errs
    );
  } else {
    return Rcpp::List::create(Rcpp::Named("errs") = errs);
  }
}


// [[Rcpp::export]]
List rnc_nodeids_solver_path(NumericMatrix X, NumericMatrix Y, NumericMatrix L,
                             double lambda_netcoh, List lambdas_x,
                             NumericMatrix X_alpha) {

  arma::mat Xmat(X.begin(), X.nrow(), X.ncol(), false); // false to avoid memory copy
  arma::mat Ymat(Y.begin(), Y.nrow(), Y.ncol(), false);
  arma::mat Lmat(L.begin(), L.nrow(), L.ncol(), false);
  arma::mat X_alphamat(X_alpha.begin(), X_alpha.nrow(), X_alpha.ncol(), false);
  arma::sp_mat L_sp = arma::sp_mat(Lmat);
  arma::sp_mat X_alpha_sp = arma::sp_mat(X_alphamat);

  int n = Xmat.n_rows;
  int n_alpha = Lmat.n_rows;
  int p = Xmat.n_cols;
  arma::mat I_n = arma::eye<arma::mat>(n, n);
  arma::mat I = arma::eye<arma::mat>(n_alpha, n_alpha);

  arma::mat inverse = arma::spsolve(
    X_alpha_sp.t() * X_alpha_sp + lambda_netcoh * L_sp, I, "lapack"
  );
  arma::mat Xxinverse = Xmat.t() * (I_n - X_alphamat * inverse * X_alphamat.t());
  arma::mat A = Xxinverse * Xmat;
  arma::mat b = Xxinverse * Ymat;

  // Compute eigendecomposition
  arma::vec A_evalues;
  arma::mat A_evectors;
  arma::eig_sym(A_evalues, A_evectors, A);

  // Save some repeated computations
  arma::mat Utb = A_evectors.t() * b;
  arma::vec alpha1 = inverse * X_alphamat.t() * Ymat;
  arma::mat alpha2 = inverse * X_alphamat.t() * Xmat;

  int n_lambdas_x = lambdas_x.size();
  arma::mat betas = arma::zeros<arma::mat>(p, n_lambdas_x);
  arma::mat alphas = arma::zeros<arma::mat>(n_alpha, n_lambdas_x);
  for (int i = 0; i < n_lambdas_x; ++i) {
    NumericVector lambda_x = lambdas_x[i];
    arma::vec lambda_x_vec(lambda_x.begin(), lambda_x.size(), false);
    arma::vec diag_values = 1 / (A_evalues + lambda_x_vec);
    arma::sp_mat diag_mat = create_sparse_diagonal(diag_values);
    arma::vec beta = A_evectors * diag_mat * Utb;
    betas.col(i) = beta;
    arma::vec alpha = alpha1 - alpha2 * beta;
    alphas.col(i) = alpha;
  }
  arma::mat alphas_all = X_alpha_sp * alphas;

  return Rcpp::List::create(
    Rcpp::Named("alpha_nodeids") = alphas,
    Rcpp::Named("alpha") = alphas_all,
    Rcpp::Named("beta") = betas
  );
}


double loglike_bernoulli(arma::mat Y, arma::mat P){
  double result = 0;
  int n = Y.n_rows;
  for (int i = 0; i<n; i++) {
    if (P(i, 0) < SMOOTH) {
      P(i, 0) = SMOOTH;
    }
    if (P(i, 0) > (1 - SMOOTH)) {
      P(i, 0) = 1 - SMOOTH;
    }
    result += Y(i, 0) * log(P(i, 0)) + (1 - Y(i, 0)) * log(1 - P(i, 0));
  }
  return result;
}

arma::mat logit_p(arma::mat eta){
  int n = eta.n_rows;
  arma::mat expeta = exp(eta);
  arma::mat ones = arma::ones(n,1);
  arma::mat result = expeta / (expeta + ones);
  return result;
}

// function to solve logistic regression with network cohesion, when covariates are provided.
arma::mat RNC_Logit(arma::mat X, arma::mat Y,
                    arma::sp_mat M, arma::mat theta_init,
                    int maxit, double tol, bool verbose){

  int n = X.n_rows;
  int iter = 0;
  double err = 0;
  arma::mat I = arma::eye<arma::mat>(n, n);
  arma::mat X_tilde = arma::join_rows(I, X);
  arma::mat eta, theta_old, theta_new;
  double ell;

  // Do Newton Method
  theta_old = theta_init;
  eta = X_tilde * theta_old;
  arma::mat one_n = arma::ones(n, 1);
  arma::mat P = logit_p(eta);
  ell = loglike_bernoulli(Y, P);
  arma::mat residual = Y - P;
  arma::mat w_vec = P % (one_n - P);
  arma::mat z = eta + residual / w_vec;
  arma::sp_mat W = arma::sp_mat(n, n);
  W.diag() = w_vec;
  bool converge = false;
  while (!converge) {
    iter += 1;
    theta_new = RNC_LS_Naive(X, z, M, W);
    err = arma::norm(theta_new - theta_old, 2) /
      (arma::norm(theta_old, 2) + SMOOTH);
    if (err < tol) {
      converge = true;
    }
    theta_old = theta_new;
    eta = X_tilde * theta_old;
    P = logit_p(eta);
    ell = loglike_bernoulli(Y, P);
    residual = Y - P;
    w_vec = P % (one_n - P);
    z = eta + residual / w_vec;
    W.diag() = w_vec;
    if (iter == maxit){
      if (verbose) {
        Rcout << "Maximum iteraction reached before converge!" << std::endl;
      }
      break;
    }
    if (verbose) {
      Rcout << "Finish iteration " << iter << " with loglikelihood " << ell << std::endl;
    }
    if ((P.max() > (1 - SMOOTH)) || (P.min() < SMOOTH)) {
      converge = true;
    }
  }

  return theta_old;
}


// [[Rcpp::export]]
List rnc_logistic_solver(NumericMatrix X, NumericMatrix Y,
                         NumericMatrix L, NumericMatrix H,
                         double lambda_netcoh, NumericMatrix theta_init,
                         int maxit, double tol, bool verbose) {

  arma::mat Xmat(X.begin(), X.nrow(), X.ncol(), false); // false to avoid memory copy
  arma::mat Ymat(Y.begin(), Y.nrow(), Y.ncol(), false);
  arma::mat Lmat(L.begin(), L.nrow(), L.ncol(), false);
  arma::mat Hmat(H.begin(), H.nrow(), H.ncol(), false);
  arma::mat thetamat(theta_init.begin(), theta_init.nrow(), theta_init.ncol(), false);

  int n = Xmat.n_rows;
  int p = Xmat.n_cols;

  arma::mat Omega = arma::zeros<arma::mat>(n + p, n + p);
  Omega(arma::span(0, n - 1), arma::span(0, n - 1)) = lambda_netcoh * Lmat;
  Omega(arma::span(n, n + p - 1), arma::span(n, n + p - 1)) = Hmat;
  arma::sp_mat M_sp = arma::sp_mat(Omega);

  arma::mat result = RNC_Logit(Xmat, Ymat, M_sp, thetamat, maxit, tol, verbose);
  return Rcpp::List::create(
    Rcpp::Named("alpha") = result.head_rows(n),
    Rcpp::Named("beta") = result.tail_rows(result.n_rows - n)
  );
}
