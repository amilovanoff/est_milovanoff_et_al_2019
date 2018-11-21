#Script to write outputs
source("outputs/functions/write_f.R")


#Default output in default scenario. Do for all except functions with implicit attributes
# function_attributes <- read.csv("architecture/function_attributes.csv",stringsAsFactors = FALSE)
# function_tbc <- setdiff(unique(function_attributes$Function),unique(subset(function_attributes,Type=="Implicit")$Function))
# do.call(write_def_outputs_f,list(function_tbc=function_tbc,scen_tbc="def"))
# do.call(write_def_outputs_f,list(function_tbc="fleet_fc_hist_f",scen_tbc="def"))

#Run and write simulations

# do.call(write_simulation_f,list(function_tbc="fleet_i_ev_bat_f",sim_tbc="sim1"))

# sim_paper1<-c(2)
# for (sim in paste0("sim",sim_paper1)){
#   function_tbc<-"vehicle_lca_f"
#   do.call(write_simulation_f,list(function_tbc=function_tbc,sim_tbc=sim))
# }

sim_paper1<-c(1,2,6,8,11,12,13,16,17,20,21,22,23,24,25,26,28)
for (sim in paste0("sim",sim_paper1)){
  function_tbc<-"fleet_lca_f"
  do.call(write_simulation_f,list(function_tbc=function_tbc,sim_tbc=sim))
}

sim_paper1<-c(7,8,9,10)
for (sim in paste0("sim",sim_paper1)){
  function_tbc<-"fleet_lca_vkmt_f"
  do.call(write_simulation_f,list(function_tbc=function_tbc,sim_tbc=sim))
}

sim_paper1<-c(3,4)
for (sim in paste0("sim",sim_paper1)){
  function_tbc<-"fleet_fc_proj_f"
  do.call(write_simulation_f,list(function_tbc=function_tbc,sim_tbc=sim))
}

sim_paper1<-c(1,2,3)
for (sim in paste0("sim",sim_paper1)){
  function_tbc<-"fleet_mc_proj_f"
  do.call(write_simulation_f,list(function_tbc=function_tbc,sim_tbc=sim))
}

sim_paper1<-c(1)
for (sim in paste0("sim",sim_paper1)){
  function_tbc<-"lca_ef_prim_alu_f"
  do.call(write_simulation_f,list(function_tbc=function_tbc,sim_tbc=sim))
}

sim_paper1<-c(1)
for (sim in paste0("sim",sim_paper1)){
  function_tbc<-"fleet_mfa_f"
  do.call(write_simulation_f,list(function_tbc=function_tbc,sim_tbc=sim))
}

#Simulations including continuous values

sim_paper1<-c(5)
for (sim in paste0("sim",sim_paper1)){
  function_tbc<-"fleet_lca_vkmt_f"
  do.call(fun_sens_lw_timing,list(function_tbc=function_tbc,sim_tbc=sim))
}

sim_paper1<-c(2,3)
for (sim in paste0("sim",sim_paper1)){
  function_tbc<-"fleet_lca_f"
  do.call(fun_sens_lw_timing,list(function_tbc=function_tbc,sim_tbc=sim))
}

#Run and write sensitivity analyses
#Complete analysis
# function_tbc <- c("fleet_lca_f")
# for (fct in function_tbc){
#   do.call(write_sens_analysis_f,list(sens_tbc="complete",function_tbc=fct,scen_tbc="def"))
# }

#Partial analysis
sim_paper1<-c(1:4)
sens_tbc<-paste0("sens",sim_paper1)
for (sens in sens_tbc){
  do.call(write_sens_analysis_f,list(sens_tbc=sens,function_tbc="fleet_lca_f",scen_tbc="def"))
}

#Simulate min and max results from sensitivty analysis

do.call(write_min_max_f,list(function_tbc="fleet_lca_f",scen_tbc="def",sens_tbc=paste0("sens",c(1:4)),dts_names="dyn_LCI_tot",y_axis="GWP100",x_axis="Year",x_val=2050,var_tbc = "LWscen",case_ref="BAU",case_tbs="Aluminum Maximum",var_to_sum="n",cumulative="y"))
