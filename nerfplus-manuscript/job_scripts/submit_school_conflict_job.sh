#!/bin/bash

#$ -pe smp 24
#$ -N school_conflict
#$ -t 1-28

module load R

SCHIDS=(
    "1" "10" "13" "19"
    "20" "21" "22" "24" "26" "27" "29"
    "3" "31" "33" "34" "35"
    "40" "42" "44" "45" "48" "49"
    "51" "56" "58"
    "6" "60"
    "9"
)

cd ../
Rscript meals/05_school_conflict.R --schid ${SCHIDS[$SGE_TASK_ID-1]} --include_w1 --connected --impute_mode ${1}
