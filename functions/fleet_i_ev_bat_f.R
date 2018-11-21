###>Function: Return the initial electric range[miles], battery capacity [kWh], and battery weight[kg] for all technologies with ev batteries
fleet_i_ev_bat_f<-function(FCV_bat_t = NA,
                        BEV_bat_t = NA,
                        PHEV_bat_t = NA,
                        HEV_bat_t = NA,
                        wgt_scen_GREET=NA,
                        ev_bat_size_mdl=NA){
  #Assign default values
  source("architecture/attribute_f.R",local = TRUE)
  attribute_f("fleet_i_ev_bat_f")
  #Inputs
  vh_techno <- read.csv("inputs/vehicle_technology.csv", stringsAsFactors = FALSE, check.names = FALSE)
  wt_subcomp <-read.csv("inputs/C2G_rel_subcpt_wgt.csv",stringsAsFactors = FALSE,check.names = FALSE)
  ev_bat_size_dt <- read.csv("inputs/ev_bat_size.csv", stringsAsFactors = FALSE, check.names = FALSE)
  bat_fc_dt<-read.csv("inputs/GREET_bat&FC_2017.csv", stringsAsFactors = FALSE, check.names = FALSE)
  conv<-read.csv("inputs/conversion_units.csv", stringsAsFactors = FALSE, check.names = FALSE,row.names = 1)
  #Output
  dt_col <- c("Size","Technology","Bat_type","Range","Capacity","Weight")
  ev_bat_dt <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  #Loop size
  for (size in c("Car","Light truck")){
    #Size_GREET:Model size to use in GREET
    if (size=="Light truck" & wgt_scen_GREET%in%c(1,4)|size=="Car" & wgt_scen_GREET%in%c(3,4)){ size_greet = "SUV"
    }else if (size=="Light truck" & wgt_scen_GREET%in%c(2,3)) { size_greet = "PUT"
    }else { size_greet="Car"}
    #Loop technology
    for (techno in unique(vh_techno$own)){
      techno_greet <- subset(vh_techno,own==techno)[,"GREET1"]
      component <- unique(subset(wt_subcomp,sapply(1:nrow(wt_subcomp),function(x)techno_greet%in%unlist(strsplit(Technology,",")[x])))[,"Component"])
      #If EV battery included in vehicle's component
      if ("EV Battery"%in%component){
        #bat_type is the cathode type of eletric battery
        if (grepl("BEV",techno)){
          bat_type=BEV_bat_t
        } else if (grepl("PHEV",techno)){
          bat_type=PHEV_bat_t
        } else if (techno=="HEV"){
          bat_type=HEV_bat_t
        } else if (techno=="FCV"){
          bat_type=FCV_bat_t
        }
        #range is the range of vehicle (on ev battery)
        #tmp_techno is the techno without range value
        if (grepl("BEV",techno) | grepl("PHEV",techno)){
          range <- subset(vh_techno,own==techno)$Range
          tmp_techno <- substring(techno,0,as.numeric(regexpr(pattern="[[:digit:]]{1}",techno))-1)
        } else {
          range<-NA
          tmp_techno <- techno
        }
        #bat_cap is the battery capacity in kWh
        if (ev_bat_size_mdl=="def"){
          #Method: First, EPA data are considered. IF does not exist, then GREET data are considered
          if (any(ev_bat_size_dt$Source=="EPA" & ev_bat_size_dt$Size==size & ev_bat_size_dt$Technology==tmp_techno & ev_bat_size_dt$Range==range)){
            bat_cap <- as.numeric(subset(ev_bat_size_dt,Source=="EPA" & Year==2015 & Size==size & Technology==tmp_techno & Range%in%range,value))
          } else {
            bat_cap <- as.numeric(subset(ev_bat_size_dt,Source=="GREET" & Size==size_greet & Technology==tmp_techno & Range%in%range & Data=="Conventional",value))
          }
        } else{
          if (ev_bat_size_mdl=="low"){
            bat_cap_fun<-min
          } else if (ev_bat_size_mdl=="high"){
            bat_cap_fun<-max
          }
          bat_cap <- bat_cap_fun(subset(ev_bat_size_dt,Size%in%c(size_greet,size) & Technology==tmp_techno & Range%in%range,value))
        }
        #bat_wgt is the battery weight in lb
        bat_wgt <- bat_cap/
          subset(bat_fc_dt,Subcomponent=="EV Battery" & sapply(1:nrow(bat_fc_dt),function(x)tmp_techno %in% unlist(strsplit(Technology[x],","))) & `Battery type`==bat_type)[,"2015"]*
          conv["lb","1 kg"]
        #Fill output data
        ev_bat_dt[nrow(ev_bat_dt)+1,c("Size","Technology","Bat_type","Range","Capacity","Weight")] <- c(size,techno,bat_type,range,bat_cap,bat_wgt)
      }
    }
  }
  #Format
  ev_bat_dt[,c("Range","Capacity","Weight")] <- sapply(c("Range","Capacity","Weight"),function(x) as.numeric(ev_bat_dt[,x]))
  return(list(fleet_i_ev_bat=ev_bat_dt))
}
