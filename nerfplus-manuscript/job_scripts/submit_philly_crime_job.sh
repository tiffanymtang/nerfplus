#!/bin/bash

#$ -pe smp 32
#$ -N philly_crime
#$ -t 1-5

module load R

SUBSAMPLES=(0.001 0.005 0.01 0.05 0.1)

cd ../
Rscript meals/${1}.R --subsample ${SUBSAMPLES[$SGE_TASK_ID-1]} --include_weather
