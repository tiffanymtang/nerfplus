#!/bin/bash

#$ -pe smp 24
#$ -N job_name
#$ -t 1-2

module load R
module load gdal
module load geos
module load udunits

REGS=(0.01 0.1)

cd ../
Rscript meals/${1}.R "${@:2}" --embedding_reg ${REGS[$SGE_TASK_ID-1]}
