#!/bin/bash
#SBATCH --job-name=burnin          # Job name
#SBATCH --ntasks=30                 # Number of Simultaneous tasks
#SBATCH --cpus-per-task=1
#SBATCH --time=01:30:00             # Time limit hrs:min:sec
#SBATCH --output=%A-%a.log          # Std output and error log
#SBATCH --array=1-30                # Number of Runs
#SBATCH --mail-user=$USER@uchicago.edu # setup email notification
#SBATCH --mail-type=FAIL
#SBATCH --mail-type=END

        ##Code to execute

module load R
Rscript model-scripts/TimeSimulationBurnin.R