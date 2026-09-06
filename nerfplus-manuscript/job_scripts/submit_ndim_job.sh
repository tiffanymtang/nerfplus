#!/bin/bash

#$ -pe smp 24
#$ -N job_name
#$ -t 1-3

module load R
module load gdal
module load geos
module load udunits

NDIMS=(1 3 5)

cd ../
Rscript meals/${1}.R "${@:2}" --embedding_ndim ${NDIMS[$SGE_TASK_ID-1]}
