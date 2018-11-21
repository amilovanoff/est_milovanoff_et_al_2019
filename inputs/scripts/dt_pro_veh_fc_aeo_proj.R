###>Creates prospective electricity mixes from AEO projections.
library(tidyr)
source("inputs/scripts/api_eia_data.R")
#Inputs
size_eq <- read.csv("inputs/size_equivalency.csv",stringsAsFactors = FALSE)
vh_techno <- read.csv("inputs/vehicle_technology.csv", stringsAsFactors = FALSE, check.names = FALSE)
#key is the personnal key to access EIA API
key<-"fd6b6196a799f52d34de4ebdb4e99d4e"
aeo_case_list <- c("REF","HOP","LOP")
aeo_year=2018
aeo_fc_dt <- NULL
aeo_fc_impro_dt <- NULL
for (aeo_case in aeo_case_list){
  veh_fc_dt <- getAEOVehicleFC(aeo_year=aeo_year,aeo_case=aeo_case,key=key)
  #Copy extracted data in file
  aeo_fc_dt <- rbind(aeo_fc_dt,veh_fc_dt)
  fc_dt <- veh_fc_dt %>% spread(Year,Value)
  #Delete rows with only zero
  fc_dt[fc_dt==0] <- NA
  #Convert mpg in gpm
  fc_dt[,as.character(2016:2050)] <- 1/fc_dt[,as.character(2016:2050)]
  for (r in 1:nrow(fc_dt)){
    #If no value in 2016. No calculation
    if(any(!is.na(fc_dt[r,as.character(2016)]))){
      first_col <- as.numeric(colnames(fc_dt[r,as.character(2016:2026)])[min(which(!is.na(fc_dt[r,as.character(2016:2026)])))])
      #Change represent the change in gpm from the first value and the minimum value
      fc_dt[r,"Change"]<-fc_dt[r,as.character(2026)]-fc_dt[r,as.character(first_col)]
      fc_dt[r,"Change%"]<-(fc_dt[r,as.character(2026)]-fc_dt[r,as.character(first_col)])/fc_dt[r,as.character(first_col)]*100
    } else {
      fc_dt[r,"Change"] <- NA
      fc_dt[r,"Change%"] <- NA
    }
  }
  #Delete rows with NA value in changes
  fc_dt <- subset(fc_dt, !is.na(Change))
  #fc_scenario contains the %changes associated with weight savings. 
  #In the High scenario, we assume that no changes were associated with weight savings
  #In the Med scenario, we assume that weight savings represented 5.4% of the changes (5.4% of MPG)
  fc_scenario<-data.frame(Low=(1/1.116-1)*100,Med=(1/1.054-1)*100,High=0,check.names = FALSE, stringsAsFactors = FALSE)
  fc_dt[,"WFR_Low"]<-fc_scenario[,"Low"]/fc_dt[,"Change%"]
  fc_dt[,"WFR_Med"]<-fc_scenario[,"Med"]/fc_dt[,"Change%"]
  fc_dt[,"WFR_High"]<-fc_scenario[,"High"]/fc_dt[,"Change%"]
  #If values above 1 in WFR, means that mass reduction technologies not properly implemented.
  #Assumption: Consider 8.4 and 5.4 reduction
  if (any(fc_dt[,c("WFR_Low","WFR_Med")]>1,na.rm=TRUE)){
    rows<-which(fc_dt[,c("WFR_Low","WFR_Med")]>1)
    fc_dt[rows,"WFR_Low"]<-(1/1.054-1)*100/fc_dt[rows,"Change%"]
    fc_dt[rows,"WFR_Med"]<-(1/1.026-1)*100/fc_dt[rows,"Change%"]
  }
   #Calculate the annual improvement in %
  #fc_impro_dt: Contains the annual fuel improvement per vehicle type and scenario
  fc_impro_dt <- NULL
  for (fc_impro_scen in colnames(fc_scenario)){
    temp_dt<-fc_dt
    temp_dt[,"2016"] <- 0
    for (r in 1:nrow(fc_dt)){
      #Change technology name
      temp_dt[r,"Technology"] <- vh_techno$own[sapply(1:nrow(vh_techno),function(x) fc_dt[r,"Technology"] %in% unlist(strsplit(vh_techno$aeo[x],";")))]
      #Change size name
      temp_dt[r,"Size"] <- size_eq$Own[sapply(1:nrow(vh_techno),function(x) fc_dt[r,"Class"] %in% unlist(strsplit(size_eq$AEO[x],";")))]
      temp_dt[r,as.character(2017:2050)]<-(fc_dt[r,as.character(2017:2050)]-fc_dt[r,as.character(2016:(2050-1))])/
        fc_dt[r,as.character(2016:(2050-1))]*100*(1-fc_dt[r,paste0("WFR_",fc_impro_scen)])
      #Check annual improvement after 2030. If above 1 (absolute value) then zero.
      if (any(abs(temp_dt[r,as.character(2035:2050)])>1)){
        col<-as.character(2035:2050)[which(abs(temp_dt[r,as.character(2035:2050)])>1)]
        temp_dt[r,col]<-0
      }
      }
  temp_dt[,"Improvement scenario"]<-fc_impro_scen
  fc_impro_dt<-rbind(fc_impro_dt,temp_dt)
  }
  fc_impro_dt[,c("Change","Change%","WFR_Low","WFR_Med","WFR_High")] <- NULL
  #Calculate cumulative fuel consumption relative changes
  fc_impro_dt[,"cum_changes"] <- rowSums(fc_impro_dt[,as.character(2016:2050)])
  #Output files
  dt_col<-c("Size","Technology","Aeo_case","Fc_impro",as.character(2016:2050))
  fc_impro_output<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  for (size in unique(size_eq$Own)){
    for (techno in unique(vh_techno$own)){
      if (nrow(subset(fc_impro_dt,Size==size & Technology==techno))!=0){
        size_tbc <- size
      }else{
        size_tbc <- setdiff(unique(size_eq$Own),size)
      }
      #Fill medium value
      med_row_tbc <- which.max(subset(fc_impro_dt,Size==size_tbc & Technology==techno & `Improvement scenario`=="Med")$cum_changes)
      fc_impro_output[nrow(fc_impro_output)+1,as.character(2016:2050)] <- subset(fc_impro_dt,Size==size_tbc & Technology==techno & `Improvement scenario`=="Med")[med_row_tbc,as.character(2016:2050)]
      fc_impro_output[nrow(fc_impro_output),c("Size","Technology","Aeo_case","Fc_impro")] <- c(size,techno,aeo_case,"Med")
      #Fill high value
      high_row_tbc <- which.min(subset(fc_impro_dt,Size==size_tbc & Technology==techno & `Improvement scenario`=="High")$cum_changes)
      fc_impro_output[nrow(fc_impro_output)+1,as.character(2016:2050)] <- subset(fc_impro_dt,Size==size_tbc & Technology==techno & `Improvement scenario`=="High")[high_row_tbc,as.character(2016:2050)]
      fc_impro_output[nrow(fc_impro_output),c("Size","Technology","Aeo_case","Fc_impro")] <- c(size,techno,aeo_case,"High")
    }
  }
  aeo_fc_impro_dt <- rbind(aeo_fc_impro_dt,fc_impro_output)
}
aeo_fc_impro_dt[,"2015"] <- 0
write.csv(aeo_fc_impro_dt,"inputs/fleet_fc_impro_aeo.csv", row.names = FALSE)
write.csv(aeo_fc_dt,paste0("inputs/data/veh_fuel_economy_aeo",aeo_year,".csv"), row.names = FALSE)
