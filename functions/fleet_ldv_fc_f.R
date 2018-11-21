###>Function: Compute fuel consumption values for entire light-duty fleet in gasoline equivalent values
fleet_ldv_fc_f<-function(last_yr=NA,
                         fast_mode="n"){
  #Source
  source("architecture/attribute_f.R",local = TRUE)
  #Assign default value
  attribute_f(fun_name = "fleet_ldv_fc_f")
  #Library
  library(reshape2)
  #Input files
  vehicle_technology <- read.csv("inputs/vehicle_technology.csv", stringsAsFactors = FALSE, check.names = FALSE)
  conv<-read.csv("inputs/conversion_units.csv", stringsAsFactors = FALSE, check.names = FALSE,row.names = 1)
  fuel_conv<-read.csv("inputs/fuel_conversion.csv", stringsAsFactors = FALSE, check.names = FALSE)
  #Functions' Outputs
  fleet_fc_proj_f_res <- do.call(fun_res_f,list(fun_name="fleet_fc_proj_f",fast_mode=fast_mode))
  fleet_FC_dt <- fleet_fc_proj_f_res[["fleet_FC_dt"]]
  fleet_vkmt_share_f_res <- do.call(fun_res_f,list(fun_name="fleet_vkmt_share_f",fast_mode=fast_mode))
  fleet_vkmt_share <- fleet_vkmt_share_f_res[["fleet_vkmt_share"]]
  fleet_stock_f_res <- do.call(fun_res_f,list(fun_name="fleet_stock_f",fast_mode=fast_mode))
  fleet_new <- fleet_stock_f_res[["fleet_new"]]
  LWscen_l <- unique(fleet_FC_dt$Scenario)
  #Compile FC value per size.
  #Format temp_fc_dt
  temp_fc_dt <- subset(fleet_FC_dt,subset=Year>=2015)
  temp_fc_dt$Value <- as.numeric(temp_fc_dt$Value)
  #Convert temp_fc_dt in gasoline equivalent
  for (r in 1:nrow(temp_fc_dt)){
    fuel <- temp_fc_dt[r,"Fuel type"]
    fuel_conv_fact <- fuel_conv[fuel_conv$Fuel==fuel&fuel_conv$Data=="Conversion factor","value"]/fuel_conv[fuel_conv$Fuel=="Gasoline"&fuel_conv$Data=="Conversion factor","value"]
    temp_fc_dt[r,"Value"] <- temp_fc_dt[r,"Value"]*fuel_conv_fact
  }
  temp_fc_dt[,"Unit"]<-"Le/100km"
  if (exists("ldv_fc_dt",inherits = FALSE)){remove(ldv_fc_dt)}
  for (LWscen in LWscen_l){
    for (size in c("Car","Light truck")){
      #Create temporary dataset
      dt_col<-c("Scenario","Size","Year","Value")
      temp_dt <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
      #Loop for years
      for (y in 2015:last_yr){
        #Loop for technologies
        for (techno in unique(vehicle_technology$own)){
          #List of fuel types per technology
          fuel_l <-unlist(strsplit(vehicle_technology$`Fuel type`[which(vehicle_technology$own == techno)][1], ";"))
          #Loop on fuel_l
          for (fuel_type in fuel_l) {
            #vkmt_share is the share of distance traveled on fuel_type. If car older than 2015 models, consider VKMT share of 2015 models.
            vkmt_share <- as.numeric(subset(fleet_vkmt_share,
                                          subset= Scenario == LWscen & Size == size & Technology == techno & `Fuel type` == fuel_type,
                                          select = as.character(y)))
            #Number of new vehicles of technology techno
            new_vh <- as.numeric(subset(fleet_new,subset=Size==size & Technology==techno & Year==y & Age==0,select = Value))
            #Adjusted FC value for this technology at year y
            fc <- subset(temp_fc_dt,Size==size & Technology==techno & Scenario==LWscen & Year==y &`Fuel type`==fuel_type)[,"Value"]
            if (LWscen%in%temp_dt$Scenario & y%in%temp_dt$Year & size%in%temp_dt$Size){
              temp_dt[temp_dt$Scenario==LWscen & temp_dt$Year==y & temp_dt$Size==size,"Value"]<-as.numeric(temp_dt[temp_dt$Scenario==LWscen & temp_dt$Year==y & temp_dt$Size==size,"Value"])+new_vh*fc*vkmt_share
              } else {
                temp_dt[nrow(temp_dt)+1,] <- c(LWscen,size,y,new_vh*fc*vkmt_share)
                }
            }
        }
        #Divide equivalent energy use per 100 km by number of vehicles. Give average FC per size.
        temp_dt[temp_dt$Scenario==LWscen & temp_dt$Year==y & temp_dt$Size==size,"Value"] <- as.numeric(temp_dt[temp_dt$Scenario==LWscen & temp_dt$Year==y & temp_dt$Size==size,"Value"])/sum(subset(fleet_new, Size==size & Year==y & Age==0,Value))
        #Format temporary dataset (before merging)
        }
      if (exists("ldv_fc_dt")){
        ldv_fc_dt <- rbind(ldv_fc_dt,temp_dt)
        } else {
          ldv_fc_dt <- temp_dt
        }
    }
  }
  dt_col <- c("Year","Value")
  ldv_fc_dt[,dt_col] <- sapply(dt_col,function(x) as.numeric(ldv_fc_dt[,x]))
  results<-list(ldv_fc_dt=ldv_fc_dt)
  return(results)
}
