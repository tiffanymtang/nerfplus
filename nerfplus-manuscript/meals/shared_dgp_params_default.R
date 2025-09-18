num_samples <- 300  # Number of samples
num_features <- 20  # Number of features
s <- 2  # Number of non-zero coefficients in linear f
beta_linear <- 1  # Coefficient for linear f
m_lss <- 3  # Number of interactions in lss f
k_lss <- 2  # Number of features per interaction term in lss f
beta_lss <- 1  # Coefficient for lss f
m_poly <- 3  # Number of interactions in polynomial f
k_poly <- 2  # Number of features per interaction term in polynomial f
beta_poly <- 1  # Coefficient for polynomial f
network_fun <- sbm_network  # Function to generate the network
K <- 3  # Number of blocks in the network
pw <- 0.2  # Probability of edges within the same block
pb <- 0.02  # Probability of edges between different blocks
centroids_scale <- 1  # Network effect strength in additive blockwise network dgp
omega <- 0.5  # Network effect strength in network autocorrelation dgp
pve <- 0.6  # Proportion of variance explained
n_outliers <- 1  # Number of outliers
outliers_scale <- 3  # Scale of outliers
