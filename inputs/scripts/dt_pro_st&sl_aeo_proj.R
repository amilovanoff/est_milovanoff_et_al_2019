###>Creates dataset of stock and sales projections directly taken from AEO projections.

source("inputs/scripts/api_eia_data.R")
#Inputs
match_techno<-read.csv("inputs/vehicle_technology.csv", header = TRUE, stringsAsFactors = FALSE)
#key is the personnal key to access EIA API
key<-"fd6b6196a799f52d34de4ebdb4e99d4e"
aeo_case_list <- c("REF","HOP","LOP")
aeo_year=2018

for (aeo_case in aeo_case_list){
  sales_dt<-getAEOTransportation(aeo_year=aeo_year,aeo_case=aeo_case,aeo_data="sales",key=key)
  stock_dt<-getAEOTransportation(aeo_year=aeo_year,aeo_case=aeo_case,aeo_data="stock",key=key)
  proj_dt <- rbind(sales_dt,stock_dt)
  #Format
  for (i in 1:nrow(proj_dt)){
    if (!proj_dt[i,"Technology"] %in% match_techno$own){
      proj_dt[i,"Technology"]<-match_techno$own[sapply(1:nrow(match_techno),function(x)proj_dt[i,"Technology"] %in% unlist(strsplit(match_techno$aeo[x],";")))]
    }
  }
  agg.formula<-reformulate(termlabels = c("Technology","Size","Year","Data_type","Aeo_case","Aeo_year","Unit"),response = "Value")
  proj_dt<-aggregate(data = proj_dt,agg.formula,FUN=sum)
  proj_dt$Value<-trunc(proj_dt$Value)
  #Output
  write.csv(proj_dt,paste0("inputs/fleet_st&sl_proj_",aeo_case,".csv"), row.names = FALSE)
}




