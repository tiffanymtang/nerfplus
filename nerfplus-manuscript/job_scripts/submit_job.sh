#!/bin/bash

#$ -pe smp 32
#$ -N job_name

module load R
module load gdal
module load geos
module load udunits

cd ../
Rscript meals/${1}.R
