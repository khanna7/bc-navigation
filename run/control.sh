#!/bin/bash
#SBATCH --job-name=datagen_06_14_2020          # Job name
#SBATCH --ntasks=30                 # Number of Simultaneous tasks
#SBATCH --cpus-per-task=1
#SBATCH --time=01:30:00             # Time limit hrs:min:sec
#SBATCH --output=%A-%a.log          # Std output and error log
#SBATCH --array=1-30                # Number of Runs

        ##Code to execute

echo date

echo "CONTROL RUN"

module load R
Rscript ../TimeSimulationControl.R

echo "END CONTROL RUN"

