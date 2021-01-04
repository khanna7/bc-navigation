#!/bin/bash
#SBATCH --job-name=control          # Job name
#SBATCH --ntasks=30                 # Number of Simultaneous tasks
#SBATCH --cpus-per-task=1
#SBATCH --time=01:50:00             # Time limit hrs:min:sec
#SBATCH --output=%A-%a.log          # Std output and error log
#SBATCH --array=1-30                # Number of Runs

        ##Code to execute

echo date

echo "CONTROL RUN"

module load R
Rscript model-scripts/TimeSimulationControl.R

echo "END CONTROL RUN"

