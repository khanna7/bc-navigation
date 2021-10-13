#!/bin/bash

#Setup output directories
start_time=`date +%T_%F`
control_dir=${start_time}_control
intervention_dir=${start_time}_intervention
interventionNoSocial_dir=${start_time}_interventionNoSocial

mkdir $control_dir
mkdir ${control_dir}/data $control_dir/rdata $control_dir/logs $control_dir/diagnostic_event_logs
mkdir $intervention_dir
mkdir ${intervention_dir}/data $intervention_dir/rdata $intervention_dir/logs $intervention_dir/diagnostic_event_logs
mkdir $interventionNoSocial_dir
mkdir ${interventionNoSocial_dir}/data $interventionNoSocial_dir/rdata $interventionNoSocial_dir/logs $interventionNoSocial_dir/diagnostic_event_logs

#control
sbatch --wait sbatch-scripts/control.sh
wait
mv *.data ${control_dir}/data
mv *.events ${control_dir}/diagnostic_event_logs
mv *.RData ${control_dir}/rdata
mv *.log ${control_dir}/logs

#add a check for full run?

#intervention
sbatch --wait sbatch-scripts/intervention.sh
wait
mv *.data ${intervention_dir}/data
mv *.events ${intervention_dir}/diagnostic_event_logs
mv *.RData ${intervention_dir}/rdata
mv *.log ${intervention_dir}/logs

#interventionNoSocial
sbatch --wait sbatch-scripts/interventionNoSocial.sh
wait
mv *.data ${interventionNoSocial_dir}/data
mv *.events ${interventionNoSocial_dir}/diagnostic_event_logs
mv *.RData ${interventionNoSocial_dir}/rdata
mv *.log ${interventionNoSocial_dir}/logs

full_run_dir=${start_time}_full_run
mkdir ${full_run_dir}
mv ${control_dir} ${full_run_dir}
mv ${intervention_dir} ${full_run_dir}
mv ${interventionNoSocial_dir} ${full_run_dir} 

#TODO add code to verify output 


echo "############ Job Complete ############"
