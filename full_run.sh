#!/bin/bash

#Setup output directories
start_time=`date +%T_%F`
control_dir=${start_time}_control
intervention_dir=${start_time}_intervention
interventionNoSocial_dir=${start_time}_interventionNoSocial

mkdir $control_dir
mkdir ${control_dir}/data $control_dir/rdata $control_dir/logs
mkdir $intervention_dir
mkdir ${intervention_dir}/data $intervention_dir/rdata $intervention_dir/logs
mkdir $interventionNoSocial_dir
mkdir ${interventionNoSocial_dir}/data $interventionNoSocial_dir/rdata $interventionNoSocial_dir/logs

#control
sbatch --wait sbatch-scripts/control.sh
wait
mv *.data ${control_dir}/data
mv *.dtdata ${control_dir}/data
mv *.RData ${control_dir}/rdata
mv *.log ${control_dir}/logs

#add a check for full run?

#intervention
sbatch --wait sbatch-scripts/intervention.sh
wait
mv *.data ${intervention_dir}/data
mv *.dtdata ${intervention_dir}/data
mv *.RData ${intervention_dir}/rdata
mv *.log ${intervention_dir}/logs

#interventionNoSocial
sbatch --wait sbatch-scripts/interventionNoSocial.sh
wait
mv *.data ${interventionNoSocial_dir}/data
mv *.dtdata ${interventionNoSocial_dir}/data
mv *.RData ${interventionNoSocial_dir}/rdata
mv *.log ${interventionNoSocial_dir}/logs

full_run_dir=${start_time}_full_run
mkdir ${full_run_dir}
mv ${control_dir} ${full_run_dir}
mv ${intervention_dir} ${full_run_dir}
mv ${interventionNoSocial_dir} ${full_run_dir} 

#TODO add code to verify output 


echo "############ Job Complete ############"
