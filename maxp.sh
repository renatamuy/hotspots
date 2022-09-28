#!/bin/bash -e
#SBATCH -A massey03262
#SBATCH -J maxp # job name 
#SBATCH --time 100:00:00 # walltime
#SBATCH --mem 256GB # Memory in MB or GB
#SBATCH --ntasks 1
#SBATCH --cpus-per-task 4
#SBATCH -e maxp.err
#SBATCH -o maxp.out
#SBATCH --export NONE
#SBATCH --mail-type=end          # send email when job ends
#SBATCH --mail-type=fail         # send email if job fails
#SBATCH --mail-user=r.delaramuylaert@massey.ac.nz

cd /scale_wlg_persistent/filesets/project/massey03262/perturbation

module load UDUNITS/2.2.26-GCCcore-9.2.0 
module load GDAL/3.0.4-gimkl-2020a
module load R/4.0.1-gimkl-2020a

echo "Executing R ..."

Rscript 10_maxp_greedy.R

echo "Completed"
