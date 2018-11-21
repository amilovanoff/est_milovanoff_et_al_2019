###>Creates the vintaged stocks for the alternative scenarios to AEO projections: BAU and BNEF scenarios
#Input files
vh_techno <-read.csv("inputs/vehicle_technology.csv",stringsAsFactors = FALSE,check.names = FALSE)
bnef_ev<-read.csv("inputs/bnef_ev.csv",stringsAsFactors = FALSE,check.names = FALSE,row.names = 1)
#Source
source("functions/fleet_vint_stock_f.R",local = TRUE)

#Format bnef_ev. Assumption: Same EV sales after 2040 until 2050.
for (y in (max(as.numeric(rownames(bnef_ev)))+1):2050){
bnef_ev[as.character(y),]<-bnef_ev[as.character(y-1),]
}
#Loop for aeo_scen: Create the fleet sales for EV and BAU from aeo_scen. DO NOT CHANGE FLEET STOCK values
aeo_scen_tbc<-c("REF","LOP","HOP")
bev_techno_l <- c("BEV100","BEV300")
#aeo_scen_tbc<-unique(aeo_scen_dt$Scenario)
for (aeo_scen in aeo_scen_tbc){
  fleet_dt<-read.csv(paste0("inputs/fleet_st&sl_proj_",aeo_scen,".csv"),header = TRUE,stringsAsFactors = FALSE)
  ev_fleet_dt<-fleet_dt
  bau_fleet_dt<-fleet_dt
  for (y in as.character(2017:2050)){
    #tot_sales, car_Sales, lt_sales are the total, Car and light truck number of sales according to AEO projections. 
    ##Assumptions: these Values stay constant.
    tot_sales <- sum(subset(fleet_dt,Year==y&Data_type=="sales",Value))
    car_sales <- sum(subset(fleet_dt,Year==y&Size=="Car"&Data_type=="sales",Value))
    lt_sales <- sum(subset(fleet_dt,Year==y&Size=="Light truck"&Data_type=="sales",Value))
    #car_ev_sales and lt_ev_sales are the Car and Light truck EV sales according to AEO projections
    car_ev_sales <- sum(subset(fleet_dt,Year==y & grepl("BEV",Technology) & Size=="Car" & Data_type=="sales",Value))
    lt_ev_sales <- sum(subset(fleet_dt,Year==y & grepl("BEV",Technology) & Size=="Light truck" & Data_type=="sales",Value))
    #tot_bnef_ev_sales is the total number of BEV sales according to BNEF market share projections assuming total AEO sales projections
    tot_bnef_ev_sales <- bnef_ev[as.character(y),2]/100*tot_sales
    #car_bnef_ev_sales and lt_bnef_ev_sales are the Car and Light truck EV sales according to BNEF market share projections asuming total AEO sales projections
    car_bnef_ev_sales <- tot_bnef_ev_sales*bnef_ev[as.character(y),"Car"]/100
    lt_bnef_ev_sales <- tot_bnef_ev_sales*bnef_ev[as.character(y),"Light truck"]/100
    #Assumption: If AEO project higher EV sales than BNEF, keep AEO data
    if (car_ev_sales+lt_ev_sales < tot_bnef_ev_sales){
      #If AEO EV light truck sales are higher than BNEF EV light truck sales, do not change EV Light truck sales but reduce EV Car sales
      if (lt_ev_sales > lt_bnef_ev_sales){
        for (bev_techno in bev_techno_l){
          bev_techno_ratio <- subset(fleet_dt,Year==y & Technology==bev_techno & Size=="Car" & Data_type=="sales")$Value/car_ev_sales
          ev_fleet_dt[ev_fleet_dt$Technology==bev_techno & ev_fleet_dt$Size=="Car" & ev_fleet_dt$Year==y & ev_fleet_dt$Data_type=="sales","Value"] <- (tot_bnef_ev_sales - lt_ev_sales)*bev_techno_ratio
        }
      #If BNEF EV Car sales are higher than AEO Car sales, then increase BNEF Light truck sales to match
      } else if (car_bnef_ev_sales > car_sales){
        for (bev_techno in bev_techno_l){
          bev_techno_ratio_car <- subset(fleet_dt,Year==y & Technology==bev_techno & Size=="Car" & Data_type=="sales")$Value/car_ev_sales
          bev_techno_ratio_lt <- subset(fleet_dt,Year==y & Technology==bev_techno & Size=="Light truck" & Data_type=="sales")$Value/lt_ev_sales
          #All cars are EV
          ev_fleet_dt[ev_fleet_dt$Technology==bev_techno & ev_fleet_dt$Size=="Car" & ev_fleet_dt$Year==y & ev_fleet_dt$Data_type=="sales","Value"] <- car_sales*bev_techno_ratio_car
          #Light truck sales are remaining BNEF EV sales projections
          ev_fleet_dt[ev_fleet_dt$Technology==bev_techno & ev_fleet_dt$Size=="Light truck" & ev_fleet_dt$Year==y & ev_fleet_dt$Data_type=="sales","Value"] <- (tot_bnef_ev_sales - car_sales)*bev_techno_ratio_lt
        }
      #Else, adjust Car and Light truck sales according to BNEF projections
      } else {
        for (bev_techno in bev_techno_l){
          bev_techno_ratio_car <- subset(fleet_dt,Year==y & Technology==bev_techno & Size=="Car" & Data_type=="sales")$Value/car_ev_sales
          bev_techno_ratio_lt <- subset(fleet_dt,Year==y & Technology==bev_techno & Size=="Light truck" & Data_type=="sales")$Value/lt_ev_sales
          ev_fleet_dt[ev_fleet_dt$Technology==bev_techno & ev_fleet_dt$Size=="Car" & ev_fleet_dt$Year==y & ev_fleet_dt$Data_type=="sales","Value"] <- car_bnef_ev_sales*bev_techno_ratio_car
          ev_fleet_dt[ev_fleet_dt$Technology==bev_techno & ev_fleet_dt$Size=="Light truck" & ev_fleet_dt$Year==y & ev_fleet_dt$Data_type=="sales","Value"] <- lt_bnef_ev_sales*bev_techno_ratio_lt
        }
        }
    #Adjust other Technology sales
    for (sz in c("Car", "Light truck")) {
      new_conv_sales <- sum(subset(fleet_dt,Year==y & Size==sz & Data_type=="sales",Value)) - sum(subset(ev_fleet_dt,Year==y & Size==sz & grepl("BEV",Technology) & Data_type=="sales",Value))
      old_conv_sales <- sum(subset(fleet_dt,Year==y & Size==sz & !grepl("BEV",Technology) & Data_type=="sales",Value))
      #Assumption: All conventional vehicles (HEV included) are proportionally reduced to macth increase of EV vehicles.
      ev_fleet_dt[!grepl("BEV",ev_fleet_dt$Technology) & ev_fleet_dt$Size==sz & ev_fleet_dt$Year==y & ev_fleet_dt$Data_type=="sales","Value"] <-
        fleet_dt[!grepl("BEV",ev_fleet_dt$Technology) & fleet_dt$Size==sz & fleet_dt$Year==y & fleet_dt$Data_type=="sales","Value"]/old_conv_sales*new_conv_sales
    }
    }
    #Fill bau_fleet_dt
    #Assumption: The market shares are constant after 2017 (including size market share and Technology market share)
      bau_fleet_dt[bau_fleet_dt$Year==y&bau_fleet_dt$Data_type=="sales","Value"] <- 
        fleet_dt[fleet_dt$Year==2017&fleet_dt$Data_type=="sales","Value"]/sum(subset(fleet_dt,Year==2017&Data_type=="sales",Value))*tot_sales
  }
  #Write csv files
  write.csv(bau_fleet_dt,file=paste0("inputs/fleet_st&sl_proj_",aeo_scen,"_BAU_temp.csv"),row.names = FALSE)
  write.csv(ev_fleet_dt,file=paste0("inputs/fleet_st&sl_proj_",aeo_scen,"_EV_temp.csv"),row.names = FALSE)
}

#Loop for aeo_scen: Update the fleet stock for EV from aeo_scen. Final output
#unique(aeo_scen_dt$Scenario)
for (aeo_scen in aeo_scen_tbc){
  for (alt_scen in c("BAU","EV")){
    #temp_fleet_dt: Temporary stock and sales dataset
  temp_fleet_dt<-read.csv(paste0("inputs/fleet_st&sl_proj_",aeo_scen,"_",alt_scen,"_temp.csv"),header = TRUE,stringsAsFactors = FALSE)
  #fin_fleet_dt: Final dataset
  fin_fleet_dt<-temp_fleet_dt
  #Calculate stock based on sales projections without stock adjustments
  fleet_stock <- do.call(fleet_vint_stock_f,
                         list(aeo_scen=paste0(aeo_scen,"_",alt_scen,"_temp"),adj_stock="n"))
  agg.formula<-reformulate(termlabels = setdiff(colnames(fleet_stock),c("Age","Value")),response = "Value")
  tot_stock <- aggregate(data = subset(fleet_stock,Year %in% (2016:2050)),agg.formula,FUN=sum)
  #Update fleet stock in final file
  for (y in 2016:2050){
  for (sz in c("Car", "Light truck")) {
    #aeo_size_st: Total number of vehicles of size to respect (in the temporary file)
    aeo_size_st <- sum(subset(temp_fleet_dt,Size==sz&Year==y&Data_type=="stock",Value))
    #temp_size_st: total number of vehicles of size based on model computations
    temp_size_st <- sum(subset(tot_stock,subset = Size==sz&Year==y,Value))
    #adj_ratio: Ratio to adjust the current number of vehicles of size
    adj_ratio <- aeo_size_st/temp_size_st
    for (techno in unique(vh_techno[, "own"])) {
      #temp_techno_st: total number of vehicles of size sz and Technology techno based on model computations
      #Assumption: We adjust this number based on the adj_ratio previously computed
      temp_techno_st<-sum(subset(tot_stock,subset = Size==sz & Technology == techno & Year==y,select = Value))
      #Adjust the techno stock
      fin_fleet_dt[fin_fleet_dt$Technology==techno & fin_fleet_dt$Size==sz & fin_fleet_dt$Year==y & fin_fleet_dt$Data_type=="stock","Value"]<-
        temp_techno_st*adj_ratio
    }
  }
  }
  #Format
  fin_fleet_dt$Value <- trunc(fin_fleet_dt$Value)
  write.csv(fin_fleet_dt,file=paste0("inputs/fleet_st&sl_proj_",aeo_scen,"_",alt_scen,".csv"),row.names = FALSE)
  file.remove(paste0("inputs/fleet_st&sl_proj_",aeo_scen,"_",alt_scen,"_temp.csv"))
  }
}
