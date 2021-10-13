#!/bin/bash

#Setup output directories
start_time=`date +%T_%F`
control_dir=${start_time}_burnins

mkdir $control_dir
mkdir ${control_dir}/data $control_dir/rdata $control_dir/logs $control_dir/diagnostic_event_logs

#control
sbatch --wait sbatch-scripts/control.sh
wait
mv *.data ${control_dir}/data
mv *.events ${control_dir}/diagnostic_event_logs
mv *.RData ${control_dir}/rdata
mv *.log ${control_dir}/logs

full_run_dir=${start_time}_burnin_runs
mkdir ${full_run_dir}
mv ${control_dir} ${full_run_dir}

echo "############ Burnin Generation Complete ############"
