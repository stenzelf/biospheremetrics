#!/bin/bash 
#SBATCH --qos=short
#SBATCH --ntasks=1
#SBATCH --mem=80000
#SBATCH --partition=standard
#SBATCH --account=open
#SBATCH --job-name=R_analysis_script
#SBATCH --workdir=./Metrics/
#SBATCH --output=outfile.%j.out
#SBATCH --error=outfile.%j.err
#SBATCH --mail-type=ALL 
#SBATCH --time=60

# call R
module load piam/1.24
Rscript read_in_biocol_data.R
