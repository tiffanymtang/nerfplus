#!/bin/bash

#$ -pe smp 24
#$ -N philly_crime
#$ -t 1-5

module load R
module load gdal
module load geos
module load udunits

SUBSAMPLES=(0.001 0.005 0.01 0.05 0.1)

cd ../
Rscript meals/${1}.R "${@:2}" --subsample ${SUBSAMPLES[$SGE_TASK_ID-1]}
