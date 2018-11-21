###>Function: Calculates the distance traveled by technology, size, age and year in kilometers of the entire fleet.
fleet_vkmt_f<-function (last_yr=NA,
                        adj_stock=NA,
                        survival_rate_mdl=NA,
                        aeo_scen=NA,
                        vkmt_model=NA){
  #Arguments
  source("architecture/attribute_f.R",local=TRUE)
  attribute_f("fleet_vkmt_f")
  #Input
  vh_techno <-read.csv("inputs/vehicle_technology.csv",stringsAsFactors = FALSE,check.names = FALSE)
  stock_dt <-read.csv(paste0("inputs/fleet_vint_stock_",aeo_scen,".csv"),stringsAsFactors = FALSE,check.names = FALSE)
  conv<-read.csv("inputs/conversion_units.csv", stringsAsFactors = FALSE, check.names = FALSE,row.names = 1)
  annual_mileage<-read.csv("inputs/annual_mileage_TEDB.csv", stringsAsFactors = FALSE, check.names = FALSE)
  #Other parameters
  first_yr=2015
  #vkmt_growth_f is a function that returns the growth of VKMT of a given year
  if (vkmt_model == "Growth"){
    #Create VKMT growth function: Based on VISION's model
    vkmt_growth_f<-function(y){
      growth_rate=0.008
      years_to_zero_growth=200
      discount_rate=-growth_rate/(years_to_zero_growth*0.8)
      ref_y=2015
      t=ifelse(ref_y>y,0,y-ref_y)
      return(exp(growth_rate*(t)+discount_rate*(t)^2))
    }
  } else if (vkmt_model == "Constant"){
    #No growth in VKMT
    vkmt_growth_f<-function(y){
      return(1)
    }
  }
  #Output files: fleet_vint_vkmt contains the distance traveled by technology, size, age and year in kilometers
  fleet_vint_vkmt <- subset(stock_dt,subset = Adj_stock==adj_stock & Survival_rate_mdl==survival_rate_mdl & Year%in%(first_yr:last_yr))
  for (size in c("Car", "Light truck")) {
    for (techno in unique(vh_techno[, "own"])) {
      stock <- subset(stock_dt,subset = Adj_stock==adj_stock & Survival_rate_mdl==survival_rate_mdl & Size==size & Technology == techno & Year%in%(first_yr:last_yr))
      #Loop for model years
      for (y in unique(stock$Year)){
        for (age in unique(stock$Age)){
          fleet_vint_vkmt[fleet_vint_vkmt$Year==y & fleet_vint_vkmt$Age==age & fleet_vint_vkmt$Technology==techno & fleet_vint_vkmt$Size==size,"Value"] <- 
            as.numeric(subset(stock,subset = Year==y & Age==age,Value))*annual_mileage[annual_mileage$`Vehicle age`==age,size]*conv["km","1 mile"]*vkmt_growth_f(as.numeric(y))
        }
      }
    }
  }
  #Output files: fleet_vkmt contains the total distance traveled by technology, size and year in km
  agg.formula<-reformulate(termlabels = setdiff(colnames(fleet_vint_vkmt),c("Age","Value")),response = "Value")
  fleet_vkmt <- aggregate(data = subset(fleet_vint_vkmt,Adj_stock==adj_stock & Survival_rate_mdl==survival_rate_mdl & Year %in% (first_yr:last_yr)),agg.formula,FUN=sum)
  #Output file: fleet_vkmt_new contains the total distance traveled by technology, size and year for new vehicles in km
  fleet_vkmt_new <- subset(fleet_vint_vkmt,Adj_stock==adj_stock & Survival_rate_mdl==survival_rate_mdl & Year %in% (first_yr:last_yr) & Age==0)
  results<-list(fleet_vint_vkmt=fleet_vint_vkmt,
                fleet_vkmt=fleet_vkmt,
                fleet_vkmt_new=fleet_vkmt_new)
  return(results)
}
#fleet_vkmt_f_res<-do.call(fleet_vkmt_f,list())
#save(list="fleet_vkmt_f_res",file="interm_results/fleet_vkmt_f_def.RData")
