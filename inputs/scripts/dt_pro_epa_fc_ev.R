###>Script: Compile fuel consumption values from sales-weighted values and fuel economy per model.
library(readxl)
#Input files
fe_dt<-read.csv("inputs/data/epa_fe_sales_EV.csv", stringsAsFactors = FALSE, check.names = FALSE)
vh_techno <- read.csv("inputs/vehicle_technology.csv", stringsAsFactors = FALSE, check.names = FALSE)
conv<-read.csv("inputs/conversion_units.csv", stringsAsFactors = FALSE, check.names = FALSE,row.names = 1)
#Format input files
fe_dt<-fe_dt[,order(colnames(fe_dt))]
#Fill FC per model from EPA fueleconomy
data_tbc<-c("comb08","city08","highway08")
#Format dataset of FE. Convert fuel economy (MPG) in fuel consumption (L/100km)
for (d in data_tbc){
  fe_dt[,paste0(d,"_ed")]<-1/fe_dt[,d]*conv["L","1 gal"]*conv["mile","1 km"]*100
}
col_tbc<-c("combE","cityE","highwayE")
for (d in col_tbc){
  fe_dt[,paste0(d,"_ed")]<-fe_dt[,d]*conv["mile","1 km"]
}

#veh_class links vehicle class and our size
veh_class<-data.frame("Class" = unique(fe_dt$VClass), stringsAsFactors = FALSE)
car<-c(1,3,5,8,10,12,13,14)
veh_class[car,"Size"]<-"Car"
veh_class[-car,"Size"]<-"Light truck"
#veh_class links vehicle type and our technologies
veh_type<-data.frame("Vehicle type", "Fuel type","Technology", stringsAsFactors = FALSE, check.names = FALSE)
colnames(veh_type)<-veh_type[1,]
veh_type[1,]<-0
EPA_type<-unique(fe_dt$atvType)
for (a in 1:length(EPA_type)){
  fuel_type<-unique(fe_dt$fuelType[which(fe_dt$atvType==EPA_type[a])])
  for (b in 1:length(fuel_type)){
    veh_type[nrow(veh_type)+1,c("Vehicle type", "Fuel type")]<-c(EPA_type[a],fuel_type[b])
  }
}
veh_type<-veh_type[-1,]
veh_type$Technology[1] <- "BEV"
veh_type$Technology[2:5] <- "PHEV"
#Assume that without sale data, no sale
fe_dt$Sales[is.na(fe_dt$Sales)]<-0

years_tbc<-2012:2015
#Output file
dt_col<-c("Year","Size","Technology","Fuel type","Range Class","Data","Unit","Source","value")
dtf_FE<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
#Create temporary data frame with only BEV data and sales
techno<-"BEV"
rows_bev<-which(fe_dt$atvType %in% veh_type$`Vehicle type`[which(veh_type$Technology==techno)]
                &fe_dt$fuelType %in% veh_type$`Fuel type`[which(veh_type$Technology==techno)]
                &fe_dt$Sales!=0)
bev_fe_dt<-fe_dt[rows_bev,]
#Convert electricity consumption in /100km
databev_tbc<-c("combE","cityE","highwayE")
#Estimate the battery capacity
bev_fe_dt[,"Battery_capacity"]<-bev_fe_dt[,"range"]*0.8*bev_fe_dt[,"combE"]/100/0.9
#Range classes considered
range_inter<-seq(0,400,200)
range_class<-setdiff(range_inter,0)-100
bev_fe_dt[,"range_class"]<-cut(bev_fe_dt$range,range_inter,include.lowest = TRUE)
levels(bev_fe_dt[,"range_class"])<-range_class
range_l<-c(range_class,"all")
for (size in unique(veh_class$Size)){
  for (range in range_l){
    if (range == "all"){
      rows_range<-which(bev_fe_dt$atvType %in% veh_type$`Vehicle type`[which(veh_type$Technology==techno)]
                        &bev_fe_dt$fuelType %in% veh_type$`Fuel type`[which(veh_type$Technology==techno)]
                        &bev_fe_dt$VClass %in% veh_class$Class[which(veh_class$Size==size)])
      
      
    } else {
      rows_range<-which(bev_fe_dt$atvType %in% veh_type$`Vehicle type`[which(veh_type$Technology==techno)]
                        &bev_fe_dt$fuelType %in% veh_type$`Fuel type`[which(veh_type$Technology==techno)]
                        &bev_fe_dt$VClass %in% veh_class$Class[which(veh_class$Size==size)]
                        &bev_fe_dt$range_class==range)
    }
    model_yr<-sort(unique(bev_fe_dt$year[rows_range]))
    for (y in years_tbc){
      if (y%in%model_yr){
        rows<-intersect(rows_range,which(bev_fe_dt$year == y))
        for (d in databev_tbc){
          #Sales weighted electricity consumption
          fe<-sum(bev_fe_dt[rows,"Sales"]*bev_fe_dt[rows,paste0(d,"_ed")])/sum(bev_fe_dt[rows,"Sales"])
          dtf_FE[nrow(dtf_FE)+1,c("Year","Size","Technology","Fuel type","Range Class","Data","Unit","Source","value")]<-
            c(y,size,techno,"Electricity",range,d,"kWh/100km","EPA - FE",fe)
        }
        #Sales weighted range
        sl_range<-sum(bev_fe_dt[rows,"Sales"]*bev_fe_dt[rows,"range"])/sum(bev_fe_dt[rows,"Sales"])
        dtf_FE[nrow(dtf_FE)+1,c("Year","Size","Technology","Fuel type","Range Class","Data","Unit","Source","value")]<-
          c(y,size,techno,"Electricity",range,"Sales weighted Range","Miles","EPA - FE",sl_range)
        #Sales weighted battery capacity
        bat_cap<-sum(bev_fe_dt[rows,"Sales"]*bev_fe_dt[rows,"Battery_capacity"])/sum(bev_fe_dt[rows,"Sales"])
        dtf_FE[nrow(dtf_FE)+1,c("Year","Size","Technology","Fuel type","Range Class","Data","Unit","Source","value")]<-
          c(y,size,techno,"Electricity",range,"Sales weighted battery size","kWh","EPA - FE",bat_cap)
        
      }
    }
  }
}

#Fill for PHEV
techno="PHEV"
rows_phev<-which(fe_dt$atvType %in% veh_type$`Vehicle type`[which(veh_type$Technology==techno)]
                 &fe_dt$fuelType %in% veh_type$`Fuel type`[which(veh_type$Technology==techno)]
                 &fe_dt$Sales!=0)
phev_fe_dt<-fe_dt[rows_phev,]
dataphev_tbc_l<-list(
  c("comb08","highway08","city08"),
  c("combE","cityE","highwayE"))
fuel_l<-c("Gasoline","Electricity")
unit_l<-c("L/100km","kWh/100km")

#Range classes considered
range_inter<-seq(0,40,20)
range_class<-setdiff(range_inter,0)
phev_fe_dt[,"range_class"]<-cut(phev_fe_dt$rangeA,range_inter,include.lowest = TRUE)
levels(phev_fe_dt[,"range_class"])<-range_class
phev_fe_dt[,"Battery_capacity"]<-as.numeric(phev_fe_dt[,"rangeA"])*0.8*phev_fe_dt[,"combE"]/100/0.9
range_l<-c(range_class,"all")

for (size in unique(veh_class$Size)){
  for (range in range_l){
    if (range=="all"){
      rows_range<-which(phev_fe_dt$atvType %in% veh_type$`Vehicle type`[which(veh_type$Technology==techno)]
                        &phev_fe_dt$fuelType %in% veh_type$`Fuel type`[which(veh_type$Technology==techno)]
                        &phev_fe_dt$VClass %in% veh_class$Class[which(veh_class$Size==size)])
    } else {
      rows_range<-which(phev_fe_dt$atvType %in% veh_type$`Vehicle type`[which(veh_type$Technology==techno)]
                        &phev_fe_dt$fuelType %in% veh_type$`Fuel type`[which(veh_type$Technology==techno)]
                        &phev_fe_dt$VClass %in% veh_class$Class[which(veh_class$Size==size)]
                        &phev_fe_dt$range_class==range)
    }
  model_yr<-sort(unique(phev_fe_dt$year[rows_range]))
  for (y in years_tbc){
    if (y%in%model_yr){
      rows<-intersect(rows_range,which(phev_fe_dt$year == y))
      
      for (l in 1:length(dataphev_tbc_l)){
        fuel_type<-fuel_l[l]
        unit<-unit_l[l]
        for (d in dataphev_tbc_l[[l]]){
          #Sales weighted electricity consumption
          fe<-sum(phev_fe_dt[rows,"Sales"]*phev_fe_dt[rows,paste0(d,"_ed")])/sum(phev_fe_dt[rows,"Sales"])
          dtf_FE[nrow(dtf_FE)+1,c("Year","Size","Technology","Fuel type","Range Class","Data","Unit","Source","value")]<-
            c(y,size,techno,fuel_type,range,d,unit,"EPA - FE",fe)
        }
      }
      #Sales weighted range
      sl_range<-sum(phev_fe_dt[rows,"Sales"]*phev_fe_dt[rows,"rangeA"])/sum(phev_fe_dt[rows,"Sales"])
      dtf_FE[nrow(dtf_FE)+1,c("Year","Size","Technology","Fuel type","Range Class","Data","Unit","Source","value")]<-
        c(y,size,techno,"Electricity",range,"Sales weighted Range","Miles","EPA - FE",sl_range)
      #Sales weighted battery capacity
      bat_cap<-sum(phev_fe_dt[rows,"Sales"]*phev_fe_dt[rows,"Battery_capacity"])/sum(phev_fe_dt[rows,"Sales"])
      dtf_FE[nrow(dtf_FE)+1,c("Year","Size","Technology","Fuel type","Range Class","Data","Unit","Source","value")]<-
        c(y,size,techno,"Electricity",range,"Sales weighted battery size","kWh","EPA - FE",bat_cap)
      }
    }
  }
}
  
  
dtf_FE$`Data`[dtf_FE$`Data`%in%c("Adj City","city08","cityE")]<-"City"
dtf_FE$`Data`[dtf_FE$`Data`%in%c("Adj Hwy","highway08","highwayE")]<-"Highway"
dtf_FE$`Data`[dtf_FE$`Data`%in%c("Adj Comb","comb08","combE")]<-"Combined"
#write.csv(dtf_FE,"inputs/epa_fc_ev.csv",row.names = FALSE)
