###>Function: Gives the VKMT share on the fuel type specified for a specific technology
fleet_vkmt_share_f<-function(last_yr = NA,
                             fast_mode="n"){
  #function's creation tool's set
  source("architecture/attribute_f.R",local = TRUE)
  attribute_f("fleet_vkmt_share_f")
  #Input files
  vh_techno <- read.csv("inputs/vehicle_technology.csv", stringsAsFactors = FALSE, check.names = FALSE)
  ev_range_f_res<-do.call(fun_res_f,list(fun_name="ev_range_f",fast_mode=fast_mode))
  LWscen_l<-unique(ev_range_f_res$Scenario)
  conv<-read.csv("inputs/conversion_units.csv", stringsAsFactors = FALSE, check.names = FALSE,row.names = 1)
  #Function that calculate the vkmt share from range
  uf_f<-function(range){
    #Range is in miles
    range_miles<-range*conv["mile","1 km"]
    uf=-7.73E-09*range_miles^4+2.63E-06*range_miles^3-3.7E-04*range_miles^2+2.66E-02*range_miles
    return(uf)
  }
  #Output files
  dt_col<-c("Scenario","Size","Technology","Fuel type",2015:last_yr)
  fleet_vkmt_share<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  #Loop for lightweighting scenarios
  for (LWscen in LWscen_l){
    for (size in c("Car","Light truck")){
      for (techno in unique(vh_techno$own)){
        fuel_l <-unlist(strsplit(vh_techno$`Fuel type`[which(vh_techno$own == techno)][1], ";"))
        #If only one fuel type used by technology, the VKMT share is 1.
        if (length(fuel_l)==1){
          fleet_vkmt_share[nrow(fleet_vkmt_share)+1,]<-c(LWscen,size,techno,fuel_l,seq(from = 1,to = 1,length.out = last_yr-2015+1))
          } else {
            for (fuel_type in fuel_l){
              fleet_vkmt_share[nrow(fleet_vkmt_share)+1,c("Scenario","Size","Technology","Fuel type")]<-c(LWscen,size,techno,fuel_type)
              #Loop for model years starting in 2015 (Assumption: prior values are the same as 2015)
              for (y in 2015:last_yr){
              range=as.numeric(subset(ev_range_f_res,subset = Scenario == LWscen & Size == size & Technology == techno & Data == "range", select = as.character(y)))
              UF=uf_f(range=range)
              if (fuel_type=="Electricity"){
                fleet_vkmt_share[nrow(fleet_vkmt_share),as.character(y)]=UF
              } else {
                fleet_vkmt_share[nrow(fleet_vkmt_share),as.character(y)]=1-UF
              }
            }
          }
        }
      }
    }
  }
  return(list(fleet_vkmt_share=fleet_vkmt_share))
}
