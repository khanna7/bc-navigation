#!/bin/bash
#SBATCH --job-name=datagen_06_08_2020          # Job name
#SBATCH --ntasks=30                 # Number of Simultaneous tasks
#SBATCH --cpus-per-task=1
#SBATCH --time=00:30:00             # Time limit hrs:min:sec
#SBATCH --output=%A-%a.log          # Std output and error log
#SBATCH --array=1-30                # Number of Runs

        ##Code to execute

echo date

module load R
Rscript Burnin_Time_simulation.R
echo This is task $SLURM_ARRAY_TASK_ID

echo date
##Organize (to-do: Move all output files into a folder)






