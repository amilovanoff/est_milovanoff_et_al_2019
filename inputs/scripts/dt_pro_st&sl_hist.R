###>Creates the historical fleet stock and sales derived from the Annual Energy Outlook 2007 to 2018 versions and VISION.
#Inputs
lf_stock<-list.files("inputs/data/aeo_stock_sales",pattern="Stock_by_Technology", full.names=TRUE)
lf_sales<-list.files("inputs/data/aeo_stock_sales",pattern="Sales_by_Technology", full.names=TRUE)
match_techno<-read.csv("inputs/vehicle_technology.csv", header = TRUE, stringsAsFactors = FALSE)
#List of technologies in AEO data
aeo_technology_list <- unique(unlist(strsplit(match_techno$aeo,";")))
#Output files
dt_col<-c("Data_type","Year","Size","Technology","Value")
aeo_hist_dt<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
#Read stock files
for (l in 1:length(lf_stock)){
  #Read file
  dt<-read.csv(lf_stock[l], header = TRUE,skip = 4, stringsAsFactors = FALSE, check.names = FALSE)
  #Clean and delete columns
  dt$`api key`<-NULL
  dt$`full name`<-NULL
  dt[,5:ncol(dt)]<-NULL
  #Read values per Year (issues: AEO11 data not available and AEO16 starts at 2014 so 2013 not available. In two cases, consider data from previous AEO versions)
  for (y in 3:ifelse(colnames(dt)[3]==2012|colnames(dt)[3]==2007,4,3)){
    #yr is Year of data
    yr=colnames(dt)[y]
    #Get Cars data
    cars_rows <- intersect(1:which(dt[,1]=="Light Truck Stock"),which(dt[,1] %in% aeo_technology_list))
    for (r in cars_rows){
      #veh_techno is equivalent vehicle technology
      veh_techno<-match_techno$own[sapply(1:nrow(match_techno),function(x)dt[r,1] %in% unlist(strsplit(match_techno$aeo[x],";")))]
      #unit_conv is the conversion ratio of unit
      unit_conv = switch(dt[r,"units"],
                         "millions"=10^6,
                         "thousands"=10^3)
      #Get value and update table. If na then 0.
      aeo_hist_dt[nrow(aeo_hist_dt)+1,c("Data_type","Year","Size","Technology","Value")]<-c("stock",yr,"Car",veh_techno,ifelse(is.na(dt[r,yr]),0,as.numeric(dt[r,yr])*unit_conv))
    }
    #Get Light-trucks data
    lt_rows <- intersect(which(dt[,1]=="Light Truck Stock"):which(dt[,1]=="Total Stock"),which(dt[,1] %in% aeo_technology_list))
    for (r in lt_rows){
      #veh_techno is equivalent vehicle technology
      veh_techno<-match_techno$own[sapply(1:nrow(match_techno),function(x)dt[r,1] %in% unlist(strsplit(match_techno$aeo[x],";")))]
      #unit_conv is the conversion ratio of unit
      unit_conv = switch(dt[r,"units"],
                         "millions"=10^6,
                         "thousands"=10^3)
      #Get value and update table. If na then 0.
      aeo_hist_dt[nrow(aeo_hist_dt)+1,c("Data_type","Year","Size","Technology","Value")]<-c("stock",yr,"Light truck",veh_techno,ifelse(is.na(dt[r,yr]),0,as.numeric(dt[r,yr])*unit_conv))
    }
  }
}
#Read sales files
for (l in 1:length(lf_sales)){
  #Read file
  dt<-read.csv(lf_sales[l], header = TRUE,skip = 4, stringsAsFactors = FALSE, check.names = FALSE)
  #Clean and delete columns
  dt$`api key`<-NULL
  dt$`full name`<-NULL
  dt[,5:ncol(dt)]<-NULL
  #Read values per Year (issues: AEO11 data not available and AEO16 starts at 2014 so 2013 not available. In two cases, consider data from previous AEO versions)
  for (y in 3:ifelse(colnames(dt)[3]==2012|colnames(dt)[3]==2007,4,3)){
    #yr is Year of data
    yr=colnames(dt)[y]
    #Get Cars data
    cars_rows <- intersect(1:which(dt[,1]=="New Light Truck Sales"),which(dt[,1] %in% aeo_technology_list))
    for (r in cars_rows){
      #veh_techno is equivalent vehicle technology
      veh_techno<-match_techno$own[sapply(1:nrow(match_techno),function(x)dt[r,1] %in% unlist(strsplit(match_techno$aeo[x],";")))]
      #unit_conv is the conversion ratio of unit
      unit_conv = switch(dt[r,"units"],
                         "millions"=10^6,
                         "thousands"=10^3)
      #Get value and update table. If na then 0.
      aeo_hist_dt[nrow(aeo_hist_dt)+1,c("Data_type","Year","Size","Technology","Value")]<-c("sales",yr,"Car",veh_techno,ifelse(is.na(dt[r,yr]),0,as.numeric(dt[r,yr])*unit_conv))
    }
    #Get Light-trucks data
    lt_rows <- intersect(which(dt[,1]=="New Light Truck Sales"):which(dt[,1]=="Total New Light Truck Sales"),which(dt[,1] %in% aeo_technology_list))
    for (r in lt_rows){
      #veh_techno is equivalent vehicle technology
      veh_techno<-match_techno$own[sapply(1:nrow(match_techno),function(x)dt[r,1] %in% unlist(strsplit(match_techno$aeo[x],";")))]
      #unit_conv is the conversion ratio of unit
      unit_conv = switch(dt[r,"units"],
                         "millions"=10^6,
                         "thousands"=10^3)
      #Get value and update table. If na then 0.
      aeo_hist_dt[nrow(aeo_hist_dt)+1,c("Data_type","Year","Size","Technology","Value")]<-c("sales",yr,"Light truck",veh_techno,ifelse(is.na(dt[r,yr]),0,as.numeric(dt[r,yr])*unit_conv))
    }
  }
}
#Format and aggregate multiple technologies
aeo_hist_dt$Year<-as.numeric(aeo_hist_dt$Year)
aeo_hist_dt$Value<-as.numeric(aeo_hist_dt$Value)
agg.formula<-reformulate(termlabels = c("Technology","Size","Year","Data_type"),response = "Value")
aeo_hist_dt<-aggregate(data = aeo_hist_dt,agg.formula,FUN=sum)
aeo_hist_dt[,"Unit"]<-"vehicle"

#Convert vision data
#Inputs
vis_dts<-read.csv("inputs/data/fleet_st&sl_VISION_70-100.csv",header = FALSE,stringsAsFactors = FALSE,check.names = FALSE,row.names = 1)
#List of technologies to consider
technology_list <- unique(as.character(vis_dts[3,vis_dts[1,]=="Technology Market Shares"]))
yr_list <- 1970:2006
#Output files
dt_col<-c("Data_type","Year","Size","Technology","Value")
vision_hist_dt<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
#Get stock and sales data
for (yr in yr_list){
  #Obtain total sales
  total_sales <- as.numeric(vis_dts[as.character(yr),vis_dts[1,]=="Light Vehicle Sales"&vis_dts[2,]=="LDV"])
  for (sz in c("Car","Light truck")){
    size_market_share <- as.numeric(vis_dts[as.character(yr),vis_dts[1,]=="Light Vehicle Sales"&vis_dts[2,]==sz])
    for (techno in technology_list){
      #veh_techno is equivalent vehicle technology
      veh_techno<-match_techno$own[sapply(1:nrow(match_techno),function(x)techno %in% unlist(strsplit(match_techno$vision[x],";")))]
      techno_market_share <- as.numeric(vis_dts[as.character(yr),vis_dts[1,]=="Technology Market Shares"&vis_dts[2,]==sz&vis_dts[3,]==techno])
      #Update data output with sales
      vision_hist_dt[nrow(vision_hist_dt)+1,c("Data_type","Year","Size","Technology","Value")]<-c("sales",yr,sz,veh_techno,total_sales*size_market_share*techno_market_share)
      #Update data ouput with stock
      techo_stock<-as.numeric(vis_dts[as.character(yr),vis_dts[1,]=="Light Duty Vehicle Stock, million"&vis_dts[2,]==sz&vis_dts[3,]==techno])*10^6
      vision_hist_dt[nrow(vision_hist_dt)+1,c("Data_type","Year","Size","Technology","Value")]<-c("stock",yr,sz,veh_techno,techo_stock)
    }
  }
}
#Format and aggregate multiple technologies
vision_hist_dt$Year<-as.numeric(vision_hist_dt$Year)
vision_hist_dt$Value<-as.numeric(vision_hist_dt$Value)
agg.formula<-reformulate(termlabels = c("Technology","Size","Year","Data_type"),response = "Value")
vision_hist_dt<-aggregate(data = vision_hist_dt,agg.formula,FUN=sum)
vision_hist_dt[,"Unit"]<-"vehicle"

#Combine historical data
hist_dt<-rbind(vision_hist_dt,aeo_hist_dt)

#ISSUE: Important gap in total car and light truck stockes between 2013 and 2014 in data
#Assumption: Discrepancies are due to changes in size standards. Some cars became LT in 2014.
#We adjust car and light trucks stock in 2013 and previous data to limit impact of discrepancies on vintage stock
adj_vector <- data.frame(Year=2013:1970)
for (sz in c("Car","Light truck")){
  #diff is total stock different from 2013 to 2014
  diff<-sum(subset(hist_dt,Size==sz&Year==2014&Data_type=="stock",select=Value))-sum(subset(hist_dt,Size==sz&Year==2013&Data_type=="stock",select=Value))
  #Fill adj_vector with initial adjustement (initial difference)
  adj_vector[adj_vector$Year==2013,sz]<-diff
  #Assumption: The stock adjustement follows the same distribution than the light truck vehicle development from initial diff data.
  adj_vector[which(adj_vector$Year%in%(2012:1970)),sz]<-diff*
    cumprod(sapply(2012:1970, function(x)sum(subset(hist_dt,Size=="Light truck"&Year==x&Data_type=="stock",select=Value)))/
              sapply(2013:1971, function(x)sum(subset(hist_dt,Size=="Light truck"&Year==x&Data_type=="stock",select=Value))))
}
#We adjust stock data with previous adjustement factors
#Assumption: We do not change stock share. We assume 2014 and 2013 total stock to be the same.
adj_hist_dt <- hist_dt
for (sz in c("Car","Light truck")){
  for (y in 2013:1970){
    #i_stock is the initial stock
    i_stock<-sum(subset(hist_dt,Size==sz&Year==y&Data_type=="stock",select=Value))
    #f_stock is the final stock
    f_stock<-i_stock+adj_vector[adj_vector$Year==y,sz]
    #Adjust data
    adj_hist_dt[adj_hist_dt$Size==sz&adj_hist_dt$Year==y&adj_hist_dt$Data_type=="stock","Value"] <- (subset(hist_dt,Size==sz&Year==y&Data_type=="stock")[,"Value"]/i_stock)*f_stock
  }
}
#Format
adj_hist_dt$Value<-trunc(adj_hist_dt$Value)

#Check data consistencies
agg.formula<-reformulate(termlabels = setdiff(colnames(adj_hist_dt),c("Technology","Unit","Value")),response = "Value")
check_dt<-aggregate(data = adj_hist_dt,agg.formula,FUN=sum)
#Output
write.csv(adj_hist_dt,"inputs/fleet_st&sl_hist.csv", row.names = FALSE)
