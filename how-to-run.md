There are two intended ways to run the model: locally and remotely on a computing cluster via SLURM.

## Running on a Computing Cluster

In the case of a replication, the model can be run in a straightforward manner by invoking `full_run.sh`. This will submit SLURM jobs for a run that contains the control, clinical navigation, and network navigation scenarios.
Each scenario will generate 30 runs to balance out the stochasticity of the model when analyzed together.

Before running `full_run.sh` you will need to tailor the third line of the R scripts `TimeSimulationCotrol.R`, `TimeSimulationNoSocial.R`, and `TimeSimulation.R` to match your cloned directory.
e.g `setwd("<path-to-cloned-bc-nav-code>")`

###### NOTE: you may also run any one scenario of the model by using `bash model-scripts/<scenario-name>.sh`. Under the hood `full_run.sh` operates by automating the execution of these scenario scripts (and collating the outputs).


## Running on a local machine

Running the model locally is very useful for small tests and debugging. This mode of operation runs only one instance of the model, whereas remote operation by default runs 90 instances.

In order to allow local operation the R script of choice (from `TimeSimulationCotrol.R`, `TimeSimulationNoSocial.R`, and `TimeSimulation.R`) will need its mode changed by setting the variable `slurm <- FALSE`.

Note that you will also need to tailor the third line of the R script of choice to match your cloned directory. (e.g `setwd("<path-to-cloned-bc-nav-code>")`)

Once this has been done, you may run the model in Rstudio or in the command line with `rscript <path-to-cloned-bc-nav-code>/model-scripts/<desired-time-simulation-version>.R`.

The code will begin to execute, outputting data logs in the main `bc-navigation` directory. You will need to keep track of these and organize them, as any subsequent run will overwrite these files if they are not moved or renamed.

> #### NOTE: The three versions of TimeSimulation.R correspond to the different scenarios in the paper:

  >* `TimeSimulationCotrol.R` is the control scenario. There is no navigation of any type. This scenario models cancer onset, progression, and testing/diagnosis.

  >* `TimeSimulationNoSocial.R` is the clinical navigation scenario. Clinical navigation is added in as a component of the clinical processes for some women.

  >* `TimeSimulation.R` is the network navigation scenario. Clinical navigation takes place, and women who are exposed to clinical navigation can have an effect on their network neighbors. 


