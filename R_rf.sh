#!/usr/bin/env bash
#SBATCH --job-name=RF
#SBATCH --output=RF-%j.out 
#SBATCH --partition=standard
#SBATCH --mem=64G
#SBATCH --cpus-per-task=36 
#SBATCH --time=04:00:00

# print SLURM envirionment variables
echo "Job ID: ${SLURM_JOB_ID}"
echo "Node: ${SLURMD_NODENAME}" 
echo "Starting: "`date +"%D %T"` 
# Your calculations here 
Rscript /storage/tgause/iScience_tom/iScience_Project/rf_single_cell.R
# End of job info 
echo "Ending: "`date +"%D %T"`