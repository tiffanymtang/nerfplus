classification <- FALSE

# embedding parameters
embedding <- "laplacian"
embedding_options <- list(
  ndim = 2,
  regularization = 0.5,
  varimax = FALSE,
  center = TRUE,
  scale = TRUE
)

# importance parameters
importance_modes <- c("permute", "mdi+", "local")
importance_options <- list(
  B = 50,
  metric = NULL
)

# influence options
loo <- FALSE

# return parameters
return_features <- NULL
return_alpha <- FALSE
return_fit <- FALSE
return_data <- FALSE

# bart parameters
bart_ndpost <- 2000
bart_ntrees <- 500

# nerf+ parameters
ntrees_cv <- 10

# regularization parameters
nlams <- 10
lambdas_netcoh <- exp(seq(log(1000), log(0.0001), length.out = nlams)) * num_samples^2
lambdas_embed <- exp(seq(log(1000), log(0.0001), length.out = nlams)) * num_features
lambdas_raw <- exp(seq(log(1000), log(0.0001), length.out = nlams)) * num_features
lambdas_stump <- exp(seq(log(1000), log(0.0001), length.out = nlams)) * num_features
lambdas_l <- 0.05
