###>Function: Creates stock data of on-road vehicle, new vehicles and scrapped vehicles
fleet_stock_f<-function (first_yr=2015,
                         last_yr=NA,
                         adj_stock=NA,
                         survival_rate_mdl=NA,
                         aeo_scen=NA){
  #Source
  source("architecture/attribute_f.R",local=TRUE)
  attribute_f("fleet_stock_f")
  #Input
  vh_techno <-read.csv("inputs/vehicle_technology.csv",stringsAsFactors = FALSE,check.names = FALSE)
  stock_dt <-read.csv(paste0("inputs/fleet_vint_stock_",aeo_scen,".csv"),stringsAsFactors = FALSE,check.names = FALSE)
  #Cretae output files. fleet_stock contains the total number of vehicles by technology, size and year, fleet_new contains the total number of new vehicles by technology, size and year, fleet_scrap contains the total number of scrapped vehicles by year
  agg.formula<-reformulate(termlabels = setdiff(colnames(stock_dt),c("Age","Value")),response = "Value")
  fleet_stock <- aggregate(data = subset(stock_dt,Adj_stock==adj_stock & Survival_rate_mdl==survival_rate_mdl & Year %in% (first_yr:last_yr)),agg.formula,FUN=sum)
  fleet_new <- subset(stock_dt,Adj_stock==adj_stock & Survival_rate_mdl==survival_rate_mdl & Year %in% (first_yr:last_yr) & Age==0)
  dt_col<-c("Year","Value")
  fleet_scrap<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  for (y in first_yr:last_yr){
    #scrap_veh is the total number of vehicles at previous year minus the number of vehicles currently in the fleet except new vehicles. Total number of scrapped vehicles between y-1 and y
    scrap_veh <- sum(subset(stock_dt,subset = Adj_stock==adj_stock & Survival_rate_mdl==survival_rate_mdl & Year==y-1 ,Value)) - sum(subset(stock_dt,subset = Adj_stock==adj_stock & Survival_rate_mdl==survival_rate_mdl & Year==y & Age!=0,Value))
    fleet_scrap[nrow(fleet_scrap)+1,c("Year","Value")]<-c(y,scrap_veh)
  }
  results<-list(fleet_stock=fleet_stock,
                fleet_scrap=fleet_scrap,
                fleet_new=fleet_new)
  return(results)
}
# data_f_res<-do.call(data_f,list())
# save(list="data_f_res",file="interm_results/data_f_def.RData")
