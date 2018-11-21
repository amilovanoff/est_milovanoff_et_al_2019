fleet_lca_vkmt_f<-function(lcia_cat=NA,
                           lw_first_yr=NA,
                           fast_mode="n"){
  #Assigne default arguments
  source("architecture/attribute_f.R",local=TRUE)
  attribute_f("fleet_lca_vkmt_f")
  #Functions
  source("functions/plots/cum_long_dtf_f.R")
  #Functions' Outpus
  fleet_lca_f_res<-do.call(fun_res_f,list(fun_name="fleet_lca_f",fast_mode=fast_mode))
  dyn_LCI_tot<-fleet_lca_f_res[["dyn_LCI_tot"]]
  fleet_vkmt_f_res<-do.call(fun_res_f,list(fun_name="fleet_vkmt_f",fast_mode=fast_mode))
  fleet_vkmt<-fleet_vkmt_f_res[["fleet_vkmt"]]
  #Other parameters
  emi_inv<-lcia_cat
  cum_dyn_LCI_tot<-cum_long_dtf_f(dtf=dyn_LCI_tot,cum_variable=emi_inv,cum_param="Year")
  #Output files
  dyn_LCI_tot_vkt<-dyn_LCI_tot
  cum_dyn_LCI_tot_vkt<-subset(cum_dyn_LCI_tot,Year>=lw_first_yr)
  #Calculate annual fleet GHGs emissions per km traveled
  for (year in unique(dyn_LCI_tot$Year)){
    dyn_LCI_tot_vkt[dyn_LCI_tot_vkt$Year==year,emi_inv] <- dyn_LCI_tot[dyn_LCI_tot$Year==year,emi_inv]/sum(subset(fleet_vkmt,Year==year,Value))
  }
  #Calculate cumulative fleet GHGs emissions per km traveled starting in lw_first_yr. Cumulative emissions are also estimated starting in lw_first_l
  for (year in unique(cum_dyn_LCI_tot_vkt$Year)){
    cum_dyn_LCI_tot_vkt[cum_dyn_LCI_tot_vkt$Year==year,emi_inv] <- (cum_dyn_LCI_tot[cum_dyn_LCI_tot$Year==year,emi_inv]-cum_dyn_LCI_tot[cum_dyn_LCI_tot$Year==(lw_first_yr-1),emi_inv])/sum(subset(fleet_vkmt,Year%in%(lw_first_yr:year),Value))
  }
  return(list(dyn_LCI_tot_vkt=dyn_LCI_tot_vkt,cum_dyn_LCI_tot_vkt=cum_dyn_LCI_tot_vkt))
  }
