# run main simulations
qsub -N linear_additive submit_job.sh 01a_linear_additive_blockwise_network_effect
qsub -N linear_netar submit_job.sh 01b_linear_network_autocorrelation_effect
qsub -N poly_additive submit_job.sh 01c_poly_additive_blockwise_network_effect
qsub -N poly_netar submit_job.sh 01d_poly_network_autocorrelation_effect
qsub -N lss_additive submit_job.sh 01e_lss_additive_blockwise_network_effect
qsub -N lss_netar submit_job.sh 01f_lss_network_autocorrelation_effect

# run main simulations with real data
qsub -N real_linear_additive submit_job.sh 02a_linear_additive_blockwise_network_effect_real_data
qsub -N real_linear_netar submit_job.sh 02b_linear_network_autocorrelation_effect_real_data
qsub -N real_poly_additive submit_job.sh 02c_poly_additive_blockwise_network_effect_real_data
qsub -N real_poly_netar submit_job.sh 02d_poly_network_autocorrelation_effect_real_data

# run loo simulation
qsub -N loo_linear_additive -pe smp 1 submit_job.sh 03a_loo_linear_additive_blockwise_network_effect
qsub -N loo_linear_netar -pe smp 1 submit_job.sh 03b_loo_linear_network_autocorrelation_effect
qsub -N loo_poly_additive -pe smp 1 submit_job.sh 03c_loo_poly_additive_blockwise_network_effect
qsub -N loo_poly_netar -pe smp 1 submit_job.sh 03d_loo_poly_network_autocorrelation_effect
qsub -N loo_lss_additive -pe smp 1 submit_job.sh 03e_loo_lss_additive_blockwise_network_effect
qsub -N loo_lss_netar -pe smp 1 submit_job.sh 03f_loo_lss_network_autocorrelation_effect

# run main simulations with outliers
qsub -N outliers_linear_additive submit_job.sh 04a_linear_additive_blockwise_network_effect_outliers
qsub -N outliers_linear_netar submit_job.sh 04b_linear_network_autocorrelation_effect_outliers
qsub -N outliers_poly_additive submit_job.sh 04c_poly_additive_blockwise_network_effect_outliers
qsub -N outliers_poly_netar submit_job.sh 04d_poly_network_autocorrelation_effect_outliers
qsub -N outliers_lss_additive submit_job.sh 04e_lss_additive_blockwise_network_effect_outliers
qsub -N outliers_lss_netar submit_job.sh 04f_lss_network_autocorrelation_effect_outliers

# run school conflict case study
qsub -N school_conflict submit_school_conflict_job.sh none

# run philadelphia crime case study
qsub -N philly_crime_predictions -pe smp 24 submit_philly_crime_job.sh 06a_philly_crime_predictions
qsub -N philly_crime_global_importance -t 3-3 submit_philly_crime_job.sh 06b_philly_crime_global_importance
qsub -N philly_crime_local_importance -pe smp 1 -t 3-3 submit_philly_crime_job.sh 06c_philly_crime_local_importance

# generate figures
qsub -N figures -pe smp 1 submit_job.sh ../scripts/figures
