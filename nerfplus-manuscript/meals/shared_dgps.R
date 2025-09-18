# Main Simulations
linear_additive_blockwise_network_dgp <- create_dgp(
  .dgp_fun = additive_blockwise_network_dgp_fun,
  .name = "Linear Additive Blockwise Network DGP",
  n = num_samples, p = num_features,
  f = f_linear, s = s, beta = beta_linear,
  network_fun = network_fun,
  network_args = list(K = K, pw = pw, pb = pb),
  centroids_scale = centroids_scale,
  pve = pve
)

lss_additive_blockwise_network_dgp <- create_dgp(
  .dgp_fun = additive_blockwise_network_dgp_fun,
  .name = "LSS Additive Blockwise Network DGP",
  n = num_samples, p = num_features,
  f = f_lss, m = m_lss, k = k_lss, beta = beta_lss,
  network_fun = network_fun,
  network_args = list(K = K, pw = pw, pb = pb),
  centroids_scale = centroids_scale,
  pve = pve
)

poly_additive_blockwise_network_dgp <- create_dgp(
  .dgp_fun = additive_blockwise_network_dgp_fun,
  .name = "Polynomial Additive Blockwise Network DGP",
  n = num_samples, p = num_features,
  f = f_hier_poly, m = m_poly, k = k_poly, beta = beta_poly,
  network_fun = network_fun,
  network_args = list(K = K, pw = pw, pb = pb),
  centroids_scale = centroids_scale,
  pve = pve
)

linear_network_autocorrelation_dgp <- create_dgp(
  .dgp_fun = network_autocorrelation_dgp_fun,
  .name = "Linear Network Autocorrelation DGP",
  n = num_samples, p = num_features,
  f = f_linear, s = s, beta = beta_linear,
  network_fun = network_fun,
  network_args = list(K = K, pw = pw, pb = pb),
  omega = omega,
  pve = pve
)

lss_network_autocorrelation_dgp <- create_dgp(
  .dgp_fun = network_autocorrelation_dgp_fun,
  .name = "LSS Network Autocorrelation DGP",
  n = num_samples, p = num_features,
  f = f_lss, m = m_lss, k = k_lss, beta = beta_lss,
  network_fun = network_fun,
  network_args = list(K = K, pw = pw, pb = pb),
  omega = omega,
  pve = pve
)

poly_network_autocorrelation_dgp <- create_dgp(
  .dgp_fun = network_autocorrelation_dgp_fun,
  .name = "Polynomial Network Autocorrelation DGP",
  n = num_samples, p = num_features,
  f = f_hier_poly, m = m_poly, k = k_poly, beta = beta_poly,
  network_fun = network_fun,
  network_args = list(K = K, pw = pw, pb = pb),
  omega = omega,
  pve = pve
)

# Main Simulations with Real Data for X
linear_additive_blockwise_network_real_data_dgp <- create_dgp(
  .dgp_fun = additive_blockwise_network_dgp_fun,
  .name = "Linear Additive Blockwise Network with Real Data DGP",
  n = 370, p = 214,
  x_fun = load_real_data,
  f = f_linear, s = s, beta = beta_linear,
  network_fun = network_fun,
  network_args = list(K = K, pw = pw, pb = pb),
  centroids_scale = centroids_scale,
  pve = pve
)

lss_additive_blockwise_network_real_data_dgp <- create_dgp(
  .dgp_fun = additive_blockwise_network_dgp_fun,
  .name = "LSS Additive Blockwise Network with Real Data DGP",
  n = 370, p = 214,
  x_fun = load_real_data,
  f = f_lss, m = m_lss, k = k_lss, beta = beta_lss,
  network_fun = network_fun,
  network_args = list(K = K, pw = pw, pb = pb),
  centroids_scale = centroids_scale,
  pve = pve
)

poly_additive_blockwise_network_real_data_dgp <- create_dgp(
  .dgp_fun = additive_blockwise_network_dgp_fun,
  .name = "Polynomial Additive Blockwise Network with Real Data DGP",
  n = 370, p = 214,
  x_fun = load_real_data,
  f = f_hier_poly, m = m_poly, k = k_poly, beta = beta_poly,
  network_fun = network_fun,
  network_args = list(K = K, pw = pw, pb = pb),
  centroids_scale = centroids_scale,
  pve = pve
)

linear_network_autocorrelation_real_data_dgp <- create_dgp(
  .dgp_fun = network_autocorrelation_dgp_fun,
  .name = "Linear Network Autocorrelation with Real Data DGP",
  n = 370, p = 214,
  x_fun = load_real_data,
  f = f_linear, s = s, beta = beta_linear,
  network_fun = network_fun,
  network_args = list(K = K, pw = pw, pb = pb),
  omega = omega,
  pve = pve
)

lss_network_autocorrelation_real_data_dgp <- create_dgp(
  .dgp_fun = network_autocorrelation_dgp_fun,
  .name = "LSS Network Autocorrelation with Real Data DGP",
  n = 370, p = 214,
  x_fun = load_real_data,
  f = f_lss, m = m_lss, k = k_lss, beta = beta_lss,
  network_fun = network_fun,
  network_args = list(K = K, pw = pw, pb = pb),
  omega = omega,
  pve = pve
)

poly_network_autocorrelation_real_data_dgp <- create_dgp(
  .dgp_fun = network_autocorrelation_dgp_fun,
  .name = "Polynomial Network Autocorrelation with Real Data DGP",
  n = 370, p = 214,
  x_fun = load_real_data,
  f = f_hier_poly, m = m_poly, k = k_poly, beta = beta_poly,
  network_fun = network_fun,
  network_args = list(K = K, pw = pw, pb = pb),
  omega = omega,
  pve = pve
)

# Main Simulations with Real Data for X (small version)
linear_additive_blockwise_network_real_data_small_dgp <- create_dgp(
  .dgp_fun = additive_blockwise_network_dgp_fun,
  .name = "Linear Additive Blockwise Network with Small Real Data DGP",
  n = 370, p = 100,
  x_fun = load_real_data,
  f = f_linear, s = s, beta = beta_linear,
  network_fun = network_fun,
  network_args = list(K = K, pw = pw, pb = pb),
  centroids_scale = centroids_scale,
  pve = pve
)

lss_additive_blockwise_network_real_data_small_dgp <- create_dgp(
  .dgp_fun = additive_blockwise_network_dgp_fun,
  .name = "LSS Additive Blockwise Network with Small Real Data DGP",
  n = 370, p = 100,
  x_fun = load_real_data,
  f = f_lss, m = m_lss, k = k_lss, beta = beta_lss,
  network_fun = network_fun,
  network_args = list(K = K, pw = pw, pb = pb),
  centroids_scale = centroids_scale,
  pve = pve
)

poly_additive_blockwise_network_real_data_small_dgp <- create_dgp(
  .dgp_fun = additive_blockwise_network_dgp_fun,
  .name = "Polynomial Additive Blockwise Network with Small Real Data DGP",
  n = 370, p = 100,
  x_fun = load_real_data,
  f = f_hier_poly, m = m_poly, k = k_poly, beta = beta_poly,
  network_fun = network_fun,
  network_args = list(K = K, pw = pw, pb = pb),
  centroids_scale = centroids_scale,
  pve = pve
)

linear_network_autocorrelation_real_data_small_dgp <- create_dgp(
  .dgp_fun = network_autocorrelation_dgp_fun,
  .name = "Linear Network Autocorrelation with Small Real Data DGP",
  n = 370, p = 100,
  x_fun = load_real_data,
  f = f_linear, s = s, beta = beta_linear,
  network_fun = network_fun,
  network_args = list(K = K, pw = pw, pb = pb),
  omega = omega,
  pve = pve
)

lss_network_autocorrelation_real_data_small_dgp <- create_dgp(
  .dgp_fun = network_autocorrelation_dgp_fun,
  .name = "LSS Network Autocorrelation with Small Real Data DGP",
  n = 370, p = 100,
  x_fun = load_real_data,
  f = f_lss, m = m_lss, k = k_lss, beta = beta_lss,
  network_fun = network_fun,
  network_args = list(K = K, pw = pw, pb = pb),
  omega = omega,
  pve = pve
)

poly_network_autocorrelation_real_data_small_dgp <- create_dgp(
  .dgp_fun = network_autocorrelation_dgp_fun,
  .name = "Polynomial Network Autocorrelation with Small Real Data DGP",
  n = 370, p = 100,
  x_fun = load_real_data,
  f = f_hier_poly, m = m_poly, k = k_poly, beta = beta_poly,
  network_fun = network_fun,
  network_args = list(K = K, pw = pw, pb = pb),
  omega = omega,
  pve = pve
)

# Main Simulations with Outliers
linear_additive_blockwise_network_outliers_dgp <- create_dgp(
  .dgp_fun = additive_blockwise_network_dgp_fun,
  .name = "Linear Additive Blockwise Network with Outliers DGP",
  n = num_samples, p = num_features,
  f = f_linear, s = s, beta = beta_linear,
  network_fun = network_fun,
  network_args = list(K = K, pw = pw, pb = pb),
  centroids_scale = centroids_scale,
  pve = pve,
  n_outliers = n_outliers, outliers_scale = outliers_scale
)

lss_additive_blockwise_network_outliers_dgp <- create_dgp(
  .dgp_fun = additive_blockwise_network_dgp_fun,
  .name = "LSS Additive Blockwise Network with Outliers DGP",
  n = num_samples, p = num_features,
  f = f_lss, m = m_lss, k = k_lss, beta = beta_lss,
  network_fun = network_fun,
  network_args = list(K = K, pw = pw, pb = pb),
  centroids_scale = centroids_scale,
  pve = pve,
  n_outliers = n_outliers, outliers_scale = outliers_scale
)

poly_additive_blockwise_network_outliers_dgp <- create_dgp(
  .dgp_fun = additive_blockwise_network_dgp_fun,
  .name = "Polynomial Additive Blockwise Network with Outliers DGP",
  n = num_samples, p = num_features,
  f = f_hier_poly, m = m_poly, k = k_poly, beta = beta_poly,
  network_fun = network_fun,
  network_args = list(K = K, pw = pw, pb = pb),
  centroids_scale = centroids_scale,
  pve = pve,
  n_outliers = n_outliers, outliers_scale = outliers_scale
)

linear_network_autocorrelation_outliers_dgp <- create_dgp(
  .dgp_fun = network_autocorrelation_dgp_fun,
  .name = "Linear Network Autocorrelation with Outliers DGP",
  n = num_samples, p = num_features,
  f = f_linear, s = s, beta = beta_linear,
  network_fun = network_fun,
  network_args = list(K = K, pw = pw, pb = pb),
  omega = omega,
  pve = pve,
  n_outliers = n_outliers, outliers_scale = outliers_scale
)

lss_network_autocorrelation_outliers_dgp <- create_dgp(
  .dgp_fun = network_autocorrelation_dgp_fun,
  .name = "LSS Network Autocorrelation with Outliers DGP",
  n = num_samples, p = num_features,
  f = f_lss, m = m_lss, k = k_lss, beta = beta_lss,
  network_fun = network_fun,
  network_args = list(K = K, pw = pw, pb = pb),
  omega = omega,
  pve = pve,
  n_outliers = n_outliers, outliers_scale = outliers_scale
)

poly_network_autocorrelation_outliers_dgp <- create_dgp(
  .dgp_fun = network_autocorrelation_dgp_fun,
  .name = "Polynomial Network Autocorrelation with Outliers DGP",
  n = num_samples, p = num_features,
  f = f_hier_poly, m = m_poly, k = k_poly, beta = beta_poly,
  network_fun = network_fun,
  network_args = list(K = K, pw = pw, pb = pb),
  omega = omega,
  pve = pve,
  n_outliers = n_outliers, outliers_scale = outliers_scale
)