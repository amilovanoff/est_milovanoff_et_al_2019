###>Function: Resize the powertrain components according to other resizments and MacKenzie equation
mackenzie_pr_f<-function(year,
                         size,
                         techno,
                         mat_cont_LW,
                         mat_cont,
                         pwtr_resz_ratio=NA,
                         HEV_bat_t=NA){
  #Source
  source("architecture/attribute_f.R",local = TRUE)
  #Assign default value
  attribute_f("mackenzie_pr_f")
  #input files
  MK_coef_techno <- read.csv("inputs/MacKenzie_coef_techno.csv",stringsAsFactors = FALSE,  check.names = FALSE)
  bat_fc_dt<-read.csv("inputs/GREET_bat&FC_2017.csv", stringsAsFactors = FALSE, check.names = FALSE)
  pw_specs <- read.csv("inputs/MacKenzie_specs.csv",stringsAsFactors = FALSE,  check.names = FALSE)
  #Other parameters
  if (grepl("BEV",techno) | grepl("PHEV",techno)){
    tmp_techno <- substring(techno,0,as.numeric(regexpr(pattern="[[:digit:]]{1}",techno))-1)
  }else{
    tmp_techno <- techno
  }
  
  pw_specs<-pw_specs[pw_specs$Size==size&sapply(1:nrow(pw_specs),function(x)tmp_techno %in% unlist(strsplit(pw_specs$Technology[x],","))),]
  #Format input files
  MK_coef_techno<-MK_coef_techno[MK_coef_techno$Size==size&(sapply(1:nrow(MK_coef_techno),function(x)tmp_techno %in% unlist(strsplit(MK_coef_techno$Technology[x],",")))),]
  #Assign the parameters estimates
  for (col in colnames(MK_coef_techno[,-(1:4)])){
    assign(col,MK_coef_techno[1,col])
  }
  
  #Create the component data frame
  cpt_l<-unlist(strsplit(pw_specs$Component,","))
  cpt_dt<-data.frame(Component = cpt_l, stringsAsFactors = FALSE)
  #Loop for components
  for ( cpt in cpt_dt$Component){
    #Extract initial weight. Use mat_cont
    cpt_dt[cpt_dt$Component==cpt,"i_wgt"]<-mat_cont["Total",cpt]
    #Extract fixed mass if applicable
    cpt_dt[cpt_dt$Component==cpt,"f_mass"]<-ifelse(any(bat_fc_dt$Subcomponent==cpt
                                                       &sapply(1:nrow(bat_fc_dt),function(x)tmp_techno %in% unlist(strsplit(bat_fc_dt$Technology[x],",")))
                                                       &bat_fc_dt$`Battery type`%in%c(HEV_bat_t,NA)
                                                       &bat_fc_dt$Data=="Fixed mass"),
                                                   bat_fc_dt[bat_fc_dt$Subcomponent==cpt
                                                                   &sapply(1:nrow(bat_fc_dt),function(x)tmp_techno %in% unlist(strsplit(bat_fc_dt$Technology[x],",")))
                                                                   &bat_fc_dt$`Battery type`%in%c(HEV_bat_t,NA)
                                                                   &bat_fc_dt$Data=="Fixed mass","2015"],0)
    if (cpt %in% c("Engine","Traction Motor") & tmp_techno != "HEV" |
        cpt %in% c("Engine","EV Battery") & tmp_techno == "HEV"){
    #Extract density of the component (in kW or kWh / kg)
      cpt_dt[cpt_dt$Component==cpt,"density"]<-bat_fc_dt[which(bat_fc_dt$Subcomponent==cpt
                                                                 &sapply(1:nrow(bat_fc_dt),function(x)tmp_techno %in% unlist(strsplit(bat_fc_dt$Technology[x],",")))
                                                                 &bat_fc_dt$`Battery type`%in%c(HEV_bat_t,NA)
                                                                 &bat_fc_dt$Data=="Energy density"),"2015"]
    #Initial component's peak power based on density
    cpt_dt[cpt_dt$Component==cpt,"i_ppw"]<-
      (cpt_dt[cpt_dt$Component==cpt,"i_wgt"]-cpt_dt[cpt_dt$Component==cpt,"f_mass"])*
      cpt_dt[cpt_dt$Component==cpt,"density"]
    } else {
      cpt_dt[cpt_dt$Component==cpt,"density"]<-0
      cpt_dt[cpt_dt$Component==cpt,"i_ppw"]<-0
      }
    #Extract usable energy. 1 otherwise
    cpt_dt[cpt_dt$Component==cpt,"usable_e"]<-
      ifelse(any(bat_fc_dt$Subcomponent==cpt
                 &sapply(1:nrow(bat_fc_dt),function(x)tmp_techno %in% unlist(strsplit(bat_fc_dt$Technology[x],",")))
                 &bat_fc_dt$`Battery type`%in%c(HEV_bat_t,NA)
                 &bat_fc_dt$Data=="Usable Energy"),
             bat_fc_dt[bat_fc_dt$Subcomponent==cpt
                             &sapply(1:nrow(bat_fc_dt),function(x)tmp_techno %in% unlist(strsplit(bat_fc_dt$Technology[x],",")))
                             &bat_fc_dt$`Battery type`%in%c(HEV_bat_t,NA)
                             &bat_fc_dt$Data=="Usable Energy","2015"],1)
  }
  #Total peak power
  i_ppw<-sum(cpt_dt$i_ppw)
  #Final peak power with MacKenzie relation
  f_ppw<-min(exp(Re(polyroot(c(beta2*log(mat_cont_LW["Total","Total"]/mat_cont["Total","Total"])-beta1*log(i_ppw)-beta4*log(i_ppw)^2
             ,beta1
             ,beta4)))))
  #Final component weight based on density
  cpt_dt[,"f_wgt"]<-cpt_dt[,"f_mass"]+(cpt_dt[,"i_wgt"]-cpt_dt[,"f_mass"])*(1-pwtr_resz_ratio*(1-f_ppw/i_ppw)*cpt_dt[,"usable_e"])
  #Output file
  mat_cont_LW_sms<-mat_cont_LW
  #Adapt output file with weight ratio and powertrain resizing ratio
  for ( cpt in cpt_dt$Component){
    mat_cont_LW_sms[,cpt]<-mat_cont_LW[,cpt]*
      cpt_dt[cpt_dt$Component==cpt,"f_wgt"]/mat_cont_LW["Total",cpt]
  }
  #Update mat_cont_LW_sms
  mat_cont_LW_sms["Total", colnames(mat_cont_LW_sms)!="Total"] <-
    colSums(mat_cont_LW_sms[rownames(mat_cont_LW_sms)!="Total", colnames(mat_cont_LW_sms)!="Total"])
  mat_cont_LW_sms[,"Total"] <-
    rowSums(mat_cont_LW_sms[, colnames(mat_cont_LW_sms)!="Total"])
  results<-list(mat_cont_LW_sms)
  return(results)
}
