# bc-navigation
Breast Cancer R21: UIC-UChicago collaboration

##ABOUT
This is a collaboration between UIC and the Center for HIV Elimination (CCHE) at the University of Chicago. 
This research project uses an ABNM (agent based network model) to simulate the effects of patient navigation on breast cancer diagnosis
in a population of Black American women in Chicago.
The model is calibrated using empirical data listed in ###cookbook?###, along with our content expert's best estimates where no data exists. 

The model is written in R, and slurm is used to manage the high performance computing using the Midway2 cluster (rcc.uchicago.edu).

##Link to paper

##Model Structure

##File Descriptions

##How to use

Burnin- create or specify

log in to midway (or cluster with slurm)

sbatch run/{preferred mode}.sh
(control, intervention, interventionNoSocial)

data will output

sort data into directory structure: RunName > data rdata logs

move data into directory

e.g.
mv *.data RunName/data
mv *.RData RunName/rdata
mv *.log RunName/logs

load rstudio and run analysis code from data-analysis (multiplot.R)

e.g.
module load rstudio
rstudio
open analysis file and update paths to data

generate plots
