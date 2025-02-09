# Agent-Based Modeling of Patient Navigation to Improve Breast Cancer Outcomes 
Breast Cancer R21: UIC-UChicago collaboration

# ABOUT
This is a collaboration between UIC and the Center for HIV Elimination (CCHE) at the University of Chicago. 

This research project uses an ABNM (agent based network model) to simulate the effects of patient navigation on breast cancer diagnosis in a population of Black American women in Chicago.

The model is calibrated using empirical data that is listed in ###cookbook?###. For parameters without empirical data, our content expert's best estimates are specified. 

The model is written in R. The slurm scheduler is used to submit jobs on the Midway2 cluster (rcc.uchicago.edu).

![GitHub last commit](https://img.shields.io/github/last-commit/khanna7/bc-navigation)

# Table of Contents (#TODO links for easy navigation, alongside "back to top")

- [Link to Paper]([#link-to-paper](https://link.springer.com/article/10.1007/s11524-022-00669-9))

Burnin- create or specify

log in to midway (or cluster with slurm)

```sbatch run/{preferred mode}.sh```
(control, intervention, interventionNoSocial)

data will output

sort data into directory structure: RunName > data rdata logs

move data into directory

e.g.
```mv *.data RunName/data```
```mv *.RData RunName/rdata```
```mv *.log RunName/logs```

load rstudio and run analysis code from data-analysis (multiplot.R)

e.g.
```module load rstudio```
```rstudio```
open analysis file and update paths to data

## Funding
R21 CA 215252
