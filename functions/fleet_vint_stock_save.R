###>Script: Store the vintaging stock of light-duty vehicles per technology, size, and age for all projection scenarios in .csv files
#Called function
source("functions/fleet_vint_stock_f.R",local = TRUE)
#Input files
#Other parameters
first_yr = 1970
last_yr = 2050
adj_stock="y"
survival_rate_mdl=1
aeo_scen_tbc<-c("REF","REF_BAU","REF_EV","LOP","LOP_BAU","LOP_EV","HOP","HOP_BAU","HOP_EV")
#Loop for aeo_scen
for (aeo_scen in aeo_scen_tbc){
  fleet_vint_stock_f_res <- do.call(fleet_vint_stock_f,
                                    list(aeo_scen=aeo_scen,survival_rate_mdl=survival_rate_mdl,first_yr=first_yr,last_yr=last_yr,adj_stock=adj_stock))
  fleet_stock <- fleet_vint_stock_f_res[["fleet_vint_stock"]]
  fleet_stock[,"Adj_stock"] <- adj_stock
  fleet_stock[,"Survival_rate_mdl"] <- survival_rate_mdl
  write.csv(fleet_stock,paste0("inputs/fleet_vint_stock_",aeo_scen,".csv"), row.names = FALSE)
}
