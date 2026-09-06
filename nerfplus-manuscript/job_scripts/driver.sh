# run main simulations
qsub -N linear_additive submit_job.sh 01a_main_simulations --nreps 100 --save --dgp linear_additive_block
qsub -N linear_netar submit_job.sh 01a_main_simulations --nreps 100 --save --dgp linear_autocorrelation
qsub -N poly_additive submit_job.sh 01a_main_simulations --nreps 100 --save --dgp poly_additive_block
qsub -N poly_netar submit_job.sh 01a_main_simulations --nreps 100 --save --dgp poly_autocorrelation
qsub -N lss_additive submit_job.sh 01a_main_simulations --nreps 100 --save --dgp lss_additive_block
qsub -N lss_netar submit_job.sh 01a_main_simulations --nreps 100 --save --dgp lss_autocorrelation

# run logistic regression sim
qsub -N logistic_additive submit_job.sh 01b_logistic_simulations --nreps 100 --save --dgp logistic_additive_block --classification
qsub -N logistic_poly_additive submit_job.sh 01b_logistic_simulations --nreps 100 --save --dgp logistic_poly_additive_block --classification
qsub -N logistic_lss_additive submit_job.sh 01b_logistic_simulations --nreps 100 --save --dgp logistic_lss_additive_block --classification

# run main simulations with multiple observations per node
qsub -N linear_additive_multiple submit_job.sh 01c_repeated_node_simulations --nreps 100 --save --dgp linear_additive_block
qsub -N poly_additive_multiple submit_job.sh 01c_repeated_node_simulations --nreps 100 --save --dgp poly_additive_block
qsub -N lss_additive_multiple submit_job.sh 01c_repeated_node_simulations --nreps 100 --save --dgp lss_additive_block

# run main simulations with real data
qsub -N real_linear_additive submit_job.sh 02_real_data_simulations --nreps 100 --save --dgp linear_additive_block_real
qsub -N real_linear_netar submit_job.sh 02_real_data_simulations --nreps 100 --save --dgp linear_autocorrelation_real
qsub -N real_poly_additive submit_job.sh 02_real_data_simulations --nreps 100 --save --dgp poly_additive_block_real
qsub -N real_poly_netar submit_job.sh 02_real_data_simulations --nreps 100 --save --dgp poly_autocorrelation_real

# run loo simulation
qsub -N loo_linear_additive -pe smp 1 submit_job.sh 03_loo_simulations --nreps 1 --save --dgp linear_additive_block
qsub -N loo_linear_netar -pe smp 1 submit_job.sh 03_loo_simulations --nreps 1 --save --dgp linear_autocorrelation
qsub -N loo_poly_additive -pe smp 1 submit_job.sh 03_loo_simulations --nreps 1 --save --dgp poly_additive_block
qsub -N loo_poly_netar -pe smp 1 submit_job.sh 03_loo_simulations --nreps 1 --save --dgp poly_autocorrelation
qsub -N loo_lss_additive -pe smp 1 submit_job.sh 03_loo_simulations --nreps 1 --save --dgp lss_additive_block
qsub -N loo_lss_netar -pe smp 1 submit_job.sh 03_loo_simulations --nreps 1 --save --dgp lss_autocorrelation

# run main simulations with outliers
qsub -N outliers_linear_additive submit_job.sh 04_outlier_simulations --nreps 100 --save --dgp linear_additive_block_outliers
qsub -N outliers_linear_netar submit_job.sh 04_outlier_simulations --nreps 100 --save --dgp linear_autocorrelation_outliers
qsub -N outliers_poly_additive submit_job.sh 04_outlier_simulations --nreps 100 --save --dgp poly_additive_block_outliers
qsub -N outliers_poly_netar submit_job.sh 04_outlier_simulations --nreps 100 --save --dgp poly_autocorrelation_outliers
qsub -N outliers_lss_additive submit_job.sh 04_outlier_simulations --nreps 100 --save --dgp lss_additive_block_outliers
qsub -N outliers_lss_netar submit_job.sh 04_outlier_simulations --nreps 100 --save --dgp lss_autocorrelation_outliers

# run school conflict case study
qsub -N school_conflict submit_school_conflict_job.sh 05a_school_conflict --nreps 100 --save --include_w1 --connected --impute_mode none --embedding_ndim 4
qsub -N school_conflict_conformal submit_school_conflict_job.sh 05b_school_conflict_conformal --include_w1 --connected --impute_mode none --embedding_ndim 4

# run philadelphia crime case study
qsub -N philly_crime_predictions -t 1-4 submit_philly_crime_job.sh 06a_philly_crime_predictions --nreps 100 --save --include_weather
qsub -N philly_crime_predictions -t 5-5 -pe smp 12 submit_philly_crime_job.sh 06a_philly_crime_predictions --nreps 100 --save --include_weather
qsub -N philly_crime_global_importance -t 3-3 submit_philly_crime_job.sh 06b_philly_crime_global_importance --nreps 100 --save --include_weather
qsub -N philly_crime_local_importance -pe smp 1 -t 3-3 submit_philly_crime_job.sh 06c_philly_crime_local_importance --nreps 1 --save --include_weather
qsub -N philly_crime_conformal -t 3-3 submit_philly_crime_job.sh 06d_philly_crime_conformal --include_weather

# run conformal simulations
qsub -N conformal_linear_additive submit_job.sh 07_conformal_prediction --nreps 100 --save --dgp linear_additive_block
qsub -N conformal_linear_netar submit_job.sh 07_conformal_prediction --nreps 100 --save --dgp linear_autocorrelation
qsub -N conformal_poly_additive submit_job.sh 07_conformal_prediction --nreps 100 --save --dgp poly_additive_block
qsub -N conformal_poly_netar submit_job.sh 07_conformal_prediction --nreps 100 --save --dgp poly_autocorrelation
qsub -N conformal_lss_additive submit_job.sh 07_conformal_prediction --nreps 100 --save --dgp lss_additive_block
qsub -N conformal_lss_netar submit_job.sh 07_conformal_prediction --nreps 100 --save --dgp lss_autocorrelation

# run main simulations with different network embeddings
qsub -N linear_additive_ase submit_job.sh 01a_main_simulations --nreps 100 --save --dgp linear_additive_block --embedding_type adjacency --nerfplus_only
qsub -N linear_netar_ase submit_job.sh 01a_main_simulations --nreps 100 --save --dgp linear_autocorrelation --embedding_type adjacency --nerfplus_only
qsub -N poly_additive_ase submit_job.sh 01a_main_simulations --nreps 100 --save --dgp poly_additive_block --embedding_type adjacency --nerfplus_only
qsub -N poly_netar_ase submit_job.sh 01a_main_simulations --nreps 100 --save --dgp poly_autocorrelation --embedding_type adjacency --nerfplus_only
qsub -N lss_additive_ase submit_job.sh 01a_main_simulations --nreps 100 --save --dgp lss_additive_block --embedding_type adjacency --nerfplus_only
qsub -N lss_netar_ase submit_job.sh 01a_main_simulations --nreps 100 --save --dgp lss_autocorrelation --embedding_type adjacency --nerfplus_only

qsub -N linear_additive_lse_ase submit_job.sh 01a_main_simulations --nreps 100 --save --dgp linear_additive_block --embedding_type laplacian_adjacency --nerfplus_only
qsub -N linear_netar_lse_ase submit_job.sh 01a_main_simulations --nreps 100 --save --dgp linear_autocorrelation --embedding_type laplacian_adjacency --nerfplus_only
qsub -N poly_additive_lse_ase submit_job.sh 01a_main_simulations --nreps 100 --save --dgp poly_additive_block --embedding_type laplacian_adjacency --nerfplus_only
qsub -N poly_netar_lse_ase submit_job.sh 01a_main_simulations --nreps 100 --save --dgp poly_autocorrelation --embedding_type laplacian_adjacency --nerfplus_only
qsub -N lss_additive_lse_ase submit_job.sh 01a_main_simulations --nreps 100 --save --dgp lss_additive_block --embedding_type laplacian_adjacency --nerfplus_only
qsub -N lss_netar_lse_ase submit_job.sh 01a_main_simulations --nreps 100 --save --dgp lss_autocorrelation --embedding_type laplacian_adjacency --nerfplus_only

# run main simulations with different network ndim
qsub -N linear_additive_lse_ndim submit_ndim_job.sh 01a_main_simulations --nreps 100 --save --dgp linear_additive_block --nerfplus_only
qsub -N linear_netar_lse_ndim submit_ndim_job.sh 01a_main_simulations --nreps 100 --save --dgp linear_autocorrelation --nerfplus_only
qsub -N poly_additive_lse_ndim submit_ndim_job.sh 01a_main_simulations --nreps 100 --save --dgp poly_additive_block --nerfplus_only
qsub -N poly_netar_lse_ndim submit_ndim_job.sh 01a_main_simulations --nreps 100 --save --dgp poly_autocorrelation --nerfplus_only
qsub -N lss_additive_lse_ndim submit_ndim_job.sh 01a_main_simulations --nreps 100 --save --dgp lss_additive_block --nerfplus_only
qsub -N lss_netar_lse_ndim submit_ndim_job.sh 01a_main_simulations --nreps 100 --save --dgp lss_autocorrelation --nerfplus_only

# run main simulations with different network regularization
qsub -N linear_additive_lse_reg submit_reg_job.sh 01a_main_simulations --nreps 100 --save --dgp linear_additive_block --nerfplus_only
qsub -N linear_netar_lse_reg submit_reg_job.sh 01a_main_simulations --nreps 100 --save --dgp linear_autocorrelation --nerfplus_only
qsub -N poly_additive_lse_reg submit_reg_job.sh 01a_main_simulations --nreps 100 --save --dgp poly_additive_block --nerfplus_only
qsub -N poly_netar_lse_reg submit_reg_job.sh 01a_main_simulations --nreps 100 --save --dgp poly_autocorrelation --nerfplus_only
qsub -N lss_additive_lse_reg submit_reg_job.sh 01a_main_simulations --nreps 100 --save --dgp lss_additive_block --nerfplus_only
qsub -N lss_netar_lse_reg submit_reg_job.sh 01a_main_simulations --nreps 100 --save --dgp lss_autocorrelation --nerfplus_only

# timing simulations
qsub -N timing submit_job.sh 08_timing_simulations --nreps 100 --save --dgp linear_additive_block

# bamdt simulations
qsub -N bamdt_linear_additive submit_job.sh 09_bamdt_simulations --nreps 100 --save --dgp linear_additive_block
qsub -N bamdt_poly_additive submit_job.sh 09_bamdt_simulations --nreps 100 --save --dgp poly_additive_block
qsub -N bamdt_lss_additive submit_job.sh 09_bamdt_simulations --nreps 100 --save --dgp lss_additive_block

# generate figures
qsub -N figures -pe smp 1 submit_job.sh ../scripts/figures
