###>Function: Resize the EV batteries according to other resizments and changes in FC
battery_r_f<-function(year,
                      size,
                      techno,
                      mat_cont_LW,
                      mat_cont,
                      bat_resz_ratio=NA,
                      bat_impro=NA,
                      BEV_bat_t=NA,
                      PHEV_bat_t=NA,
                      FC_dt){
  #Source
  source("architecture/attribute_f.R",local = TRUE)
  #Assign default value
  attribute_f("battery_r_f")
  #input files
  #Source
  source("functions/fc_upgrade_f.R",local = TRUE)
  #Inputs
  bat_fc_dt<-read.csv("inputs/GREET_bat&FC_2017.csv", stringsAsFactors = FALSE, check.names = FALSE)
  #Output file
  mat_cont_LW_sms<-mat_cont_LW
  #Adjust EV Battery for BEV and PHEV.
  if (grepl("BEV",techno) | grepl("PHEV",techno)){
    tmp_techno <- substring(techno,0,as.numeric(regexpr(pattern="[[:digit:]]{1}",techno))-1)
    bat_type<-ifelse(techno==tmp_techno,BEV_bat_t,PHEV_bat_t)
    cpt<-"EV Battery"
    #bat_dt is the data frame with battery attributes
    bat_dt <- data.frame(Component = cpt, stringsAsFactors = FALSE)
    #first_dens: Initial battery specific
    first_dens<-bat_fc_dt[which(bat_fc_dt$Subcomponent==cpt
                                &sapply(1:nrow(bat_fc_dt),function(x)tmp_techno %in% unlist(strsplit(bat_fc_dt$Technology[x],",")))
                                &bat_fc_dt$`Battery type`%in%c(bat_type,NA)
                                &bat_fc_dt$Data=="Energy density"),"2015"]
    #Consider battery specific energy improvement
    goal_yr<-2030
    if(bat_impro=="n"){
      goal_dens<-first_dens
      } else if (bat_impro=="y"){
    #goal_dens: Final battery specific to be considered
    goal_dens<-bat_fc_dt[which(bat_fc_dt$Subcomponent==cpt
                               &sapply(1:nrow(bat_fc_dt),function(x)tmp_techno %in% unlist(strsplit(bat_fc_dt$Technology[x],",")))
                               &bat_fc_dt$`Battery type`%in%c(bat_type,NA)
                               &bat_fc_dt$Data=="Energy density"),as.character(goal_yr)]
    }
    if (year <= goal_yr){
      i_dens<-first_dens+(year-1-2015)*(goal_dens-first_dens)/(goal_yr-2015)
      f_dens<-i_dens+(goal_dens-first_dens)/(goal_yr-2015)
    } else {
      i_dens<-goal_dens
      f_dens<-goal_dens
    }
    #If year does not belong in columns of FC_dt, first battery resizing.
    if (!as.character(year) %in% FC_dt$Year){
      FC_dt[nrow(FC_dt)+1,] <- subset(FC_dt,Year==(year-1))
      FC_dt[nrow(FC_dt),"Year"] <- year
      impro_scen=NA
      bat_dt[bat_dt$Component==cpt,"i_density"] <- i_dens
      bat_dt[bat_dt$Component==cpt,"f_density"] <- f_dens
    } else {
      impro_scen="n"
      bat_dt[bat_dt$Component==cpt,"i_density"]<-f_dens
      bat_dt[bat_dt$Component==cpt,"f_density"]<-f_dens
    }
    #Extract usable energy
    usable_en<-subset(bat_fc_dt,Subcomponent==cpt&sapply(1:nrow(bat_fc_dt),function(x)tmp_techno%in%unlist(strsplit(Technology, ",")[x]))&`Battery type`%in%c(bat_type, NA)&Data == "Usable Energy")[,"2015"]
    #i_wgt is the initial battery weight
    bat_dt[bat_dt$Component==cpt,"i_wgt"] <- mat_cont["Total",cpt]
    #i_cap is the battery capacity in kWh
    bat_dt[bat_dt$Component==cpt,"i_cap"] <- bat_dt[bat_dt$Component==cpt,"i_wgt"]*bat_dt[bat_dt$Component==cpt,"i_density"]
    #i_range is the range in km from the battery capacity, the FC and the battery usable energy.
    bat_dt[bat_dt$Component==cpt,"i_range"] <- bat_dt[bat_dt$Component==cpt,"i_cap"] / subset(FC_dt,Year==(year))[,"Value"]*100*usable_en
    #Upgrade fuel consumption values with weigt savings, improvement if applicable (new year only)
    FC_dt[FC_dt$Year==year,"Value"] <- do.call(fc_upgrade_f,
                                               list(
                                                 size = size,
                                                 techno = techno,
                                                 fuel_type = "Electricity",
                                                 year = year,
                                                 i_FC = subset(FC_dt,Year==(year))[,"Value"],
                                                 wgt_svg = mat_cont_LW["Total", "Total"] - mat_cont["Total", "Total"],
                                                 i_wgt = mat_cont["Total", "Total"],
                                                 fc_impro = impro_scen
                                               ))[[1]]
    
    #f_range is the final range once the weight savings are applied
    bat_dt[bat_dt$Component==cpt,"f_range"]<-
      bat_dt[bat_dt$Component==cpt,"i_cap"]*bat_dt[bat_dt$Component==cpt,"f_density"]/bat_dt[bat_dt$Component==cpt,"i_density"]/
      subset(FC_dt,Year==(year))[,"Value"]*100*usable_en
    #f_cap if the battery size considering the battery resizing to keep a certain range.
    bat_dt[bat_dt$Component==cpt,"f_cap"]<-
      (bat_dt[bat_dt$Component==cpt,"f_range"] + 
      bat_resz_ratio*(bat_dt[bat_dt$Component==cpt,"i_range"] - bat_dt[bat_dt$Component==cpt,"f_range"]))*
      subset(FC_dt,Year==(year))[,"Value"]/(100*usable_en)
    #f_wgt is the final weight of the battery
    bat_dt[bat_dt$Component==cpt,"f_wgt"]<-bat_dt[bat_dt$Component==cpt,"f_cap"]/bat_dt[bat_dt$Component==cpt,"f_density"]
    #Adjust EV Battery weight and material composition in mat_cont_LW_sms
    mat_cont_LW_sms[,cpt] <- mat_cont_LW[,cpt]*bat_dt[bat_dt$Component==cpt,"f_wgt"]/mat_cont_LW["Total",cpt]
  }
  #Update total
  mat_cont_LW_sms["Total", colnames(mat_cont_LW_sms)!="Total"] <- colSums(mat_cont_LW_sms[rownames(mat_cont_LW_sms)!="Total", colnames(mat_cont_LW_sms)!="Total"])
  mat_cont_LW_sms[,"Total"] <- rowSums(mat_cont_LW_sms[, colnames(mat_cont_LW_sms)!="Total"])
  results<-list(mat_cont_LW_sms,FC_dt)
  return(results)
}
