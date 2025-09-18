# Non-network assisted baseline methods
lm_method <- create_method(
  .method_fun = lm_method_fun,
  .name = "Linear Regression",
  classification = classification,
  importance_modes = importance_modes,
  importance_options = importance_options,
  return_features = return_features,
  return_fit = return_fit,
  return_data = return_data
)

rf_method <- create_method(
  .method_fun = rf_method_fun,
  .name = "RF",
  classification = classification,
  importance_modes = importance_modes,
  importance_options = importance_options,
  return_features = return_features,
  return_fit = return_fit,
  return_data = return_data
)

rfplus_method <- create_method(
  .method_fun = nerfplus_cv_method_fun,
  .name = "RF+",
  classification = classification,
  include_netcoh = FALSE,
  ntrees_cv = ntrees_cv,
  lambdas_netcoh = 0,
  lambdas_raw = lambdas_raw,
  lambdas_stump = lambdas_stump,
  lambdas_l = lambdas_l,
  importance_modes = importance_modes,
  importance_options = importance_options,
  return_features = return_features,
  return_alpha = return_alpha,
  return_fit = return_fit,
  return_data = return_data
)

bart_method <- create_method(
  .method_fun = bart_method_fun,
  .name = "BART",
  ndpost = bart_ndpost,
  ntree = bart_ntrees,
  classification = classification,
  return_features = return_features,
  return_fit = return_fit,
  return_data = return_data
)

# NeRF+ Variants
nerfplus_method <- create_method(
  .method_fun = nerfplus_cv_method_fun,
  .name = "NeRF+",
  classification = classification,
  include_netcoh = TRUE,
  ntrees_cv = ntrees_cv,
  lambdas_netcoh = lambdas_netcoh,
  lambdas_embed = lambdas_embed,
  lambdas_raw = lambdas_raw,
  lambdas_stump = lambdas_stump,
  lambdas_l = lambdas_l,
  embedding = embedding,
  embedding_options = embedding_options,
  importance_modes = importance_modes,
  importance_options = importance_options,
  loo = loo,
  return_features = return_features,
  return_alpha = return_alpha,
  return_fit = return_fit,
  return_data = return_data
)

nerfplus_embedding_only_method <- create_method(
  .method_fun = nerfplus_cv_method_fun,
  .name = "NeRF+ (Embedding Only)",
  classification = classification,
  include_netcoh = FALSE,
  ntrees_cv = ntrees_cv,
  lambdas_netcoh = 0,
  lambdas_embed = lambdas_embed,
  lambdas_raw = lambdas_raw,
  lambdas_stump = lambdas_stump,
  lambdas_l = lambdas_l,
  embedding = embedding,
  embedding_options = embedding_options,
  importance_modes = importance_modes,
  importance_options = importance_options,
  loo = loo,
  return_features = return_features,
  return_alpha = return_alpha,
  return_fit = return_fit,
  return_data = return_data
)

nerfplus_cohesion_only_method <- create_method(
  .method_fun = nerfplus_cv_method_fun,
  .name = "NeRF+ (Cohesion Only)",
  classification = classification,
  include_netcoh = TRUE,
  ntrees_cv = ntrees_cv,
  lambdas_netcoh = lambdas_netcoh,
  lambdas_embed = 0,
  lambdas_raw = lambdas_raw,
  lambdas_stump = lambdas_stump,
  lambdas_l = lambdas_l,
  importance_modes = importance_modes,
  importance_options = importance_options,
  loo = loo,
  return_features = return_features,
  return_alpha = return_alpha,
  return_fit = return_fit,
  return_data = return_data
)

# Other Network-assisted Models
rnc_method <- create_method(
  .method_fun = rnc_cv_method_fun,
  .name = "RNC",
  classification = classification,
  lambdas_netcoh = lambdas_netcoh,
  lambdas_raw = lambdas_raw,
  lambdas_l = lambdas_l,
  importance_modes = importance_modes,
  importance_options = importance_options,
  return_features = return_features,
  return_alpha = return_alpha,
  return_fit = return_fit,
  return_data = return_data
)

network_bart_method <- create_method(
  .method_fun = network_bart_fun,
  .name = "Network BART",
  classification = classification,
  return_features = return_features,
  return_fit = return_fit,
  return_data = return_data
)
