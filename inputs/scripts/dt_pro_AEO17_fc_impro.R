#This script extract the data of fuel consumption, horsepower and weight changes in the AEO17
library(readxl)
library(reshape2)
library(ggplot2)
#Input files
conv<-read.csv("data/conversion_units.csv", stringsAsFactors = FALSE, check.names = FALSE,row.names = 1)
match_techno<-read.csv("C:/Users/Alexandre/Dropbox/UofT/Ford/EIA-AEO/technology_equivalency.csv", header = FALSE, stringsAsFactors = FALSE)
#Other parameters
lf_aeo_fc<-list.files("C:/Users/Alexandre/Dropbox/UofT/Ford/EIA-AEO/AEO17",
                     pattern="Light-Duty_Vehicle_Miles_per_Gallon_by_Technology_Type_", 
                     full.names=TRUE)
aeo_scen_tbc<-c("REF","LOP","HOP")
last_yr<-2050
#Create output file
dt_col<-c("AEO","Size","Technology","Data","units",2015:2050)
data<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
for (scen in aeo_scen_tbc){
  #Read input files and format
  dt<-read.csv(lf_aeo_fc[grep(scen,lf_aeo_fc)], header = TRUE,skip = 4, stringsAsFactors = FALSE, check.names = FALSE)
  dt$`api key`<-NULL
  dt$`full name`<-NULL
  #Loop on size
  for (size in c("Car","Light truck")){
    rows_size<-grep(paste("Conventional",size),dt[,1],ignore.case = TRUE)+0:18
    rows_techno<-which(dt[,1] %in% match_techno[3:nrow(match_techno),match_techno[1,]=="AEO"&match_techno[2,]==size])
    rows_tbc<-intersect(rows_size,rows_techno)
    data<-rbind(data,
                data.frame(AEO=scen,
                           Size=size,
                           Technology=dt[rows_tbc,1],
                           Data="FC",
                           dt[rows_tbc,c("units",as.character(2015:2050))],
                           check.names = FALSE,stringsAsFactors = FALSE))
  }
}
#Convert FC in GPM (Gal per Miles)
data[,as.character(2015:2050)]<-1/data[,as.character(2015:2050)]
data$units<-"gpm"
#Build Scenarios with WFR contribution to fc improvement
fc_impro_tot<-subset(data,select = setdiff(colnames(data),as.character(2015:2050)))
for (r in 1:nrow(data)){
  if(any(!is.na(data[r,as.character(2015:2050)]))){
    first_col<-as.numeric(colnames(data[,as.character(2015:2050)])[min(which(!is.na(data[r,as.character(2015:2050)])))])
    #Check if discrepancy in FE of 200 mile EV for light truck (significant gap between two first years). If issue, consider second value
    if (data$Technology[r]=="200 Mile Electric Vehicle" & data[r,as.character(first_col+1)]/data[r,as.character(first_col)]>1.3){
      first_col<-first_col+1
    }
    #Change represent the change in gpm from the first value and the minimum value
    fc_impro_tot[r,"Change"]<-data[r,as.character(2026)]-data[r,as.character(first_col)]
    fc_impro_tot[r,"Change%"]<-(data[r,as.character(2026)]-data[r,as.character(first_col)])/data[r,as.character(first_col)]*100
  }
}
#fc_scenario contains the %changes associated with weight savings. 
#In the High scenario, we assume that no changes were associated with weight savings
#In the Med scenario, we assume that weight savings represented 5.4% of the changes (5.4% of MPG)
fc_scenario<-data.frame(Low=(1/1.116-1)*100,Med=(1/1.054-1)*100,High=0,check.names = FALSE, stringsAsFactors = FALSE)
fc_impro_tot[,"WFR_Low"]<-fc_scenario[,"Low"]/fc_impro_tot[,"Change%"]
fc_impro_tot[,"WFR_Med"]<-fc_scenario[,"Med"]/fc_impro_tot[,"Change%"]
fc_impro_tot[,"WFR_High"]<-fc_scenario[,"High"]/fc_impro_tot[,"Change%"]
#If values above 1 in WFR, means that mass reduction technologies not properly implemented.
#Assumption: Consider 8.4 and 5.4 reduction
if (any(fc_impro_tot[,c("WFR_Low","WFR_Med")]>1,na.rm=TRUE)){
  rows<-which(fc_impro_tot[,c("WFR_Low","WFR_Med")]>1)
  fc_impro_tot[rows,"WFR_Low"]<-(1/1.054-1)*100/fc_impro_tot[rows,"Change%"]
  fc_impro_tot[rows,"WFR_Med"]<-(1/1.026-1)*100/fc_impro_tot[rows,"Change%"]
}
#fc_impro_dt: Contains the annual fuel improvement per vehicle type and scenario
dt_col<-c("Improvement scenario",colnames(data))
fc_impro_dt<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
for (fc_impro_scen in colnames(fc_scenario)){
temp_dt<-data
for (r in 1:nrow(data)){
  if(any(!is.na(data[r,as.character(2015:2050)]))){
    first_col<-as.numeric(colnames(data[,as.character(2015:2050)])[min(which(!is.na(data[r,as.character(2015:2050)])))])
    #Check if discrepancy in FE of 200 mile EV for light truck (significant gap between two first years). If issue, consider second value
    if (data$Technology[r]%in%c("200 Mile Electric Vehicle","Fuel Cell Hydrogen")&
        abs((data[r,as.character(first_col+1)]-data[r,as.character(first_col)])/data[r,as.character(first_col)])>0.1){
      first_col<-first_col+1
    }
    temp_dt[r,as.character(2015:first_col)]<-0
    temp_dt[r,as.character((first_col+1):2050)]<-(data[r,as.character((first_col+1):2050)]-data[r,as.character(first_col:(2050-1))])/
      data[r,as.character(first_col:(2050-1))]*100*(1-fc_impro_tot[r,paste0("WFR_",fc_impro_scen)])
    #Check annual improvement after 2030. If above 1 (absolute value) then zero.
    if (any(abs(temp_dt[r,as.character(2035:2050)])>1)){
      col<-as.character(2035:2050)[which(abs(temp_dt[r,as.character(2035:2050)])>1)]
      temp_dt[r,col]<-0
    }
  }
}
temp_dt[,"Improvement scenario"]<-fc_impro_scen
fc_impro_dt<-rbind(fc_impro_dt,temp_dt)
}

#write.csv(fc_impro_dt,"data/fleet_fc_impro_aeo.csv",row.names = FALSE)
