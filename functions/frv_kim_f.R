###>Function: Calculates the fuel reduction value (FRV) of a vehicle type (i.e., size and technology) at a particular year based on Kim and Wallington's model
frv_kim_f <-function(year,
                     size,
                     techno,
                     fuel_type, 
                     i_wgt,
                     i_FC,
                     frv_impro=NA,
                     frv_pwt_r=NA,
                     fc_dr_city=NA){
  #Attribute arguments
  source("architecture/attribute_f.R",local=TRUE)
  attribute_f("frv_kim_f")
  if (grepl("BEV",techno) | grepl("PHEV",techno)){
    tmp_techno <- substring(techno,0,as.numeric(regexpr(pattern="[[:digit:]]{1}",techno))-1)
  } else {
    tmp_techno <- techno
  }
  if (frv_impro=="n"){
    fuel_conv<-read.csv("inputs/fuel_conversion.csv", stringsAsFactors = FALSE, check.names = FALSE)
    FRV_dt <- read.csv("inputs/fleet_frv_stat.csv",stringsAsFactors = FALSE,check.names = FALSE)
    FRV<-as.numeric(subset(FRV_dt,subset= Size==size & Technology==techno & `Fuel type`==fuel_type &`Powertrain resizing`==frv_pwt_r,select=FRV))
  } else {
    #Input
    param <- read.csv("inputs/frv_param.csv",stringsAsFactors = FALSE,check.names = FALSE)
    dyn_param <- read.csv("inputs/frv_param_dyn.csv",stringsAsFactors = FALSE,check.names = FALSE)
    conv<-read.csv("inputs/conversion_units.csv", stringsAsFactors = FALSE, check.names = FALSE,row.names = 1)
    #On_road adjustment factor is the ratio of adjusted combined to unadjusted combined fuel consumption (l/100km)
    ##Source: EPA fuel trends report with 2016 value
    ##Assumption: Fuel consumption values are already adjusted. Fuel use calcualtions are based on 2-cycle unadjusted
    on_road_adj_factor=1/0.782
    #Format param: erase rows that are not associated with the technology nor size nor fuel type
    techno_row <- which(sapply(1:nrow(param),function(x)tmp_techno %in% unlist(strsplit(param$Technology[x],",")))
                         |param$Technology=="Glo")
    size_row <- which(sapply(1:nrow(param),function(x)size %in% unlist(strsplit(param$Size[x],",")))
                        |param$Size=="Glo")
    fuel_row <- which(sapply(1:nrow(param),function(x)fuel_type %in% unlist(strsplit(param$Fuel[x],",")))
                        |param$Fuel=="Glo")
    rows<-Reduce(intersect,list(techno_row,size_row,fuel_row))
    param<-param[rows,]
    #Format param: Combine city and highway values
    for (par in unique(param$Parameter[which(param$Data!="Glo")])){
      param[nrow(param)+1,]<-param[which(param$Parameter==par)[1],]
      param[nrow(param),"Data"]<-"Combined"
      param[nrow(param),"Value"]<-param[which(param$Parameter==par&param$Data=="City"),"Value"]*fc_dr_city+
        param[which(param$Parameter==par&param$Data=="Highway"),"Value"]*(1-fc_dr_city)
    }
    param<-param[which(param$Data%in%c("Combined","Glo")),]
    #Format dyn_param: erase rows that are not associated with the technology nor size nor fuel type
    if ( frv_impro == "n"){
      impro_row<-which(sapply(1:nrow(dyn_param),function(x)"Med" %in% unlist(strsplit(dyn_param$Improvement[x],",")))
                       |dyn_param$Improvement=="Glo")
    } else {
      impro_row<-which(sapply(1:nrow(dyn_param),function(x)frv_impro %in% unlist(strsplit(dyn_param$Improvement[x],",")))
                       |dyn_param$Improvement=="Glo")
    }
    techno_row <- which(sapply(1:nrow(dyn_param),function(x)tmp_techno %in% unlist(strsplit(dyn_param$Technology[x],",")))
                        |dyn_param$Technology=="Glo")
    size_row <- which(sapply(1:nrow(dyn_param),function(x)size %in% unlist(strsplit(dyn_param$Size[x],",")))
                      |dyn_param$Size=="Glo")
    fuel_row <- which(sapply(1:nrow(dyn_param),function(x)fuel_type %in% unlist(strsplit(dyn_param$Fuel[x],",")))
                      |dyn_param$Fuel=="Glo")
    rows<-Reduce(intersect,list(impro_row,techno_row,size_row,fuel_row))
    dyn_param<-dyn_param[rows,]
    #Fill param with year-specific data
    if (frv_impro=="n"){
      y=2015
    } else {
      y=year
    }
    param_tbf<-param$Description[is.na(param$Value)]
    for (par in param_tbf){
      param[param$Description==par,"Value"]<-dyn_param[dyn_param$Data==par,as.character(y)]
    }
    #Update frontal area
    param[param$Parameter=="front_area","Value"] <- param[param$Parameter=="front_area","Value"]*dyn_param[grepl("Frontal area",dyn_param$Data),as.character(y)]
    #Create the variable for the equations
    for (i in 1:nrow(param)){
      assign(param$Parameter[i],param$Value[i])
    }
    #I1 is integrale(dt) ; I2 is integrale(vdt) ; I3 is integrale(v^2dt) ; I4 is i,",ntegrale(v^3dt) ; I5 is integrale(avdt)
    #Coef_rolling: Coefficient of rolling
    #Fw is mass-depend load fuel consumption
    if (tmp_techno %in% c("HEV","PHEV")){
      eta_c<-1/(theta_elec/eta_e+(1-theta_elec)/eta_i)
    } else if ((!tmp_techno %in% c("HEV","PHEV","BEV"))){
      eta_c<-eta_i
    } else if (tmp_techno %in% c("BEV")){
      eta_c<-eta_e
    }
    Fw<-i_wgt/(Hf*eta_t*eta_c)*((1-theta*mu+rot_mass_factor)*I5+rol_resistance*grav_acc*I2)
    #Calculate adjusted value
    Fw_adj<-Fw*on_road_adj_factor
    #Fx is mass-independent load fuel consumption
    Fx<-1/(Hf*eta_c)*(0.5*air_density*drag_coef*front_area*I4/eta_t + alpha*I1)
    Fx_adj<-Fx*on_road_adj_factor
    #MIF
    mif<-(Fw/(Fw+Fx))*(i_FC/i_wgt)*100
    #If electricity consumed, Ft is the sum of Fw and Fx. Otherwise, total fuel consumption
    if (fuel_type %in% c("Electricity","Hydrogen")){
      #Ft<-Fw+Fx=
      Ft<-i_FC/100*I2/10^3
      #FRV<-(Fw/Ft)*(i_FC/i_wgt)*100
      FRV<-(Fw_adj/Ft)*(i_FC/i_wgt)*100
    } else {
      #Ff<-1/(Hf*eta_c)*0.5*fmep*disp*gear_ratio*I2
      #const_ratio<-fmep*disp/1800*gear_ratio
      const_ratio=0.3325
      Ff<-i_wgt/(Hf*eta_c)*0.5*const_ratio*I2
      #Calculated adjusted value
      Ff_adj<-Ff*on_road_adj_factor
      #If Fuel Consumption is in Lper 100km, we divide per 100. I2 is in m.
      #Ft<-Fw+Fx+Ff
      Ft<-i_FC/100*I2/10^3
      #Unit FRV: L per km and kg. 
      #FRV_wo_pwt_res is the FRV without powertrain resizing
      FRV_wo_pwt_res<-(Fw_adj/Ft)*(i_FC/i_wgt)*100
      #FRV_with_pwt_res is the FRV with powertrain resizing
      FRV_with_pwt_res<-(Fw_adj+Ff_adj)/Ft*(i_FC/i_wgt)*100
      #FRV does not consider powertrain resizing if frv_pwt_r=0, consider full powertrain resizing if frv_pwt_r=1.
      FRV=FRV_wo_pwt_res+frv_pwt_r*(FRV_with_pwt_res-FRV_wo_pwt_res)
    }
  }
  return(FRV)
}
