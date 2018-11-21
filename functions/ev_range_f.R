###>Function: Returns the range of EV (BEV and PHEV) for the different scenarios
ev_range_f<-function( BEV_bat_t=NA,
                      PHEV_bat_t=NA,
                      fast_mode="n"){
  #Source
  source("architecture/attribute_f.R",local = TRUE)
  #Assign default value
  attribute_f(fun_name = "ev_range_f")
  #Inputs
  bat_fc_dt<-read.csv("inputs/GREET_bat&FC_2017.csv", stringsAsFactors = FALSE, check.names = FALSE)
  #Functions' outputs
  fleet_mc_proj_f_res <- do.call(fun_res_f,list(fun_name="fleet_mc_proj_f",fast_mode=fast_mode))
  comp_wgt_dt <- fleet_mc_proj_f_res[["comp_wgt_dt"]]
  fleet_fc_proj_f_res <- do.call(fun_res_f,list(fun_name="fleet_fc_proj_f",fast_mode=fast_mode))
  fleet_FC_dt <- fleet_fc_proj_f_res[["fleet_FC_dt"]]
  #Other parameters
  LWscen_l <- unique(comp_wgt_dt$Scenario)
  year_l <- as.numeric(colnames(comp_wgt_dt)[grep(pattern="[[:digit:]]{1}",colnames(comp_wgt_dt))])
  cpt <- "EV Battery"
  #Create output file
  dt_col <- c(colnames(comp_wgt_dt),"Data","Unit")
  fleet_range_dt <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  for (LWscen in LWscen_l){
    for (size in c("Car","Light truck")){
      for (techno in c("BEV100","BEV300","PHEV20","PHEV40")){
        #tmp_techno is the techno without range value
        tmp_techno <- substring(techno,0,as.numeric(regexpr(pattern="[[:digit:]]{1}",techno))-1)
        #bat_type
        bat_type <- ifelse(tmp_techno=="BEV",BEV_bat_t,PHEV_bat_t)
        temp_dt <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 4),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
        temp_dt[,"Data"]<-c("density","weight","electricity consumption","range")
        temp_dt[,"Unit"]<-c("kWh/kg","kg","kWh/100km","km")
        for (y in year_l){
          #Get the specific energy of the battery in y
          goal_yr<-2030
          #goal_dens: Final battery specific to be considered
          goal_dens<-bat_fc_dt[which(bat_fc_dt$Subcomponent==cpt
                                     &sapply(1:nrow(bat_fc_dt),function(x)tmp_techno %in% unlist(strsplit(bat_fc_dt$Technology[x],",")))
                                     &bat_fc_dt$`Battery type`%in%c(bat_type,NA)
                                     &bat_fc_dt$Data=="Energy density"),as.character(goal_yr)]
          #first_dens: Initial battery specific
          first_dens<-bat_fc_dt[which(bat_fc_dt$Subcomponent==cpt
                                      &sapply(1:nrow(bat_fc_dt),function(x)tmp_techno %in% unlist(strsplit(bat_fc_dt$Technology[x],",")))
                                      &bat_fc_dt$`Battery type`%in%c(bat_type,NA)
                                      &bat_fc_dt$Data=="Energy density"),"2015"]
          if (y <= goal_yr){
            bat_dens<-first_dens+(y-2015)*(goal_dens-first_dens)/(goal_yr-2015)
          } else {
            bat_dens<-goal_dens
          }
          temp_dt[temp_dt$Data=="density",as.character(y)] <- bat_dens
          #Obtain the battery weight
          cpt_wgt <- as.numeric(subset(comp_wgt_dt,subset=Scenario==LWscen & Size==size & Technology==techno & Subcomponent==cpt,select=as.character(y)))
          temp_dt[temp_dt$Data=="weight",as.character(y)] <- cpt_wgt
          #Obtain the electricity consumption
          elec_cons <- subset(fleet_FC_dt,subset=Scenario==LWscen & Size==size & Technology==techno & `Fuel type`=="Electricity" & Year==y)[,"Value"]
          temp_dt[temp_dt$Data=="electricity consumption",as.character(y)] <- elec_cons
          #Estimate range
          range<-bat_dens*cpt_wgt/elec_cons*100*
            bat_fc_dt[bat_fc_dt$Subcomponent==cpt
                             &sapply(1:nrow(bat_fc_dt), function(x)tmp_techno %in% unlist(strsplit(bat_fc_dt$Technology[x], ",")))
                             &bat_fc_dt$`Battery type` %in% c(bat_type, NA)
                             &bat_fc_dt$Data == "Usable Energy","2015"]
          temp_dt[temp_dt$Data=="range",as.character(y)]<-range
        }
        temp_dt[,"Technology"]<-techno
        temp_dt[,"Size"]<-size
        temp_dt[,"Scenario"]<-LWscen
        temp_dt[,"Subcomponent"]<-cpt
        fleet_range_dt<-rbind(fleet_range_dt,temp_dt)
      }
    }
  }
  return(fleet_range_dt)
}
 # ev_range_f_res<-do.call(ev_range_f,list(fast_mode="y"))
 # save(list="ev_range_f_res",file=paste0("interm_results/ev_range_f_def.RData"))
