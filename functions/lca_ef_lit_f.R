lca_ef_lit_f<-function (techno="ns",
                        lcia_method = NA,
                        lcia_cat = NA,
                        FCV_bat_t=NA,
                        HEV_bat_t=NA,
                        BEV_bat_t = NA,
                        PHEV_bat_t = NA,
                        ef_bat_mdl = NA,
                        ef_steel_mdl = NA,
                        ef_hss_mdl = NA,
                        fast_mode="n"){
  #Assign arguments default values
  source("architecture/attribute_f.R",local=TRUE)
  attribute_f("lca_ef_lit_f")
  #Input files
  LCA_process <- read.csv("inputs/LCA_process.csv", stringsAsFactors = FALSE, check.names = FALSE)
  conv<-read.csv("inputs/conversion_units.csv", stringsAsFactors = FALSE, check.names = FALSE,row.names = 1)
  fuel_specs<-read.csv("inputs/fuel_specs.csv", stringsAsFactors = FALSE, check.names = FALSE,row.names = 1)
  EF_lit <- read.csv("inputs/EF_lit_review.csv", stringsAsFactors = FALSE, check.names = FALSE)
  #Other parameters
  greet_cat<-"GHGs"
  LCA_process<-subset(LCA_process,Source=="lit")
  #Get emission factors for battery production and assembly
  techno_tbc<-c("BEV","PHEV","HEV","FCV")
  for (techno in techno_tbc){
    bat_t_l<-grep("bat_t",ls(),value=TRUE)
    if (any(grepl(techno,bat_t_l))){
      bat_name<-get(grep(techno,bat_t_l,value=TRUE))
      if (any(grepl(pattern=" ",bat_name))){
        bat_type<-substring(bat_name,1,as.numeric(regexpr(pattern=" ",bat_name))-1)
        anode_type<-substring(bat_name,as.numeric(regexpr(pattern=" ",bat_name))+1,nchar(bat_name))
      } else {
        bat_type<-bat_name
        anode_type<-""
      }
    } else {
      bat_name=NA
    }
    temp_dt<-LCA_process[grepl("Battery",LCA_process$Phase,ignore.case = TRUE),c("Unit","Phase","Process","Source")]
    temp_dt[,lcia_cat]<-0
    temp_dt[,"Technology"]<-techno
    #Fill the emission factors from GREET
    for (cat in greet_cat){
      #Fill EF for battery production and assembly
      if (!is.na(bat_name)){
        #EF for Battery production
        #The default model uses Kim et al values data
        if (ef_bat_mdl=="def"){
          temp_dt[temp_dt$Phase=="Battery production and assembly"&temp_dt$Process=="Battery production",lcia_cat]<-as.numeric(subset(EF_lit,Modele=="def"&Process=="Battery production",select=LCI))
          temp_dt[temp_dt$Phase=="Battery production and assembly"&temp_dt$Process=="Battery Assembly",lcia_cat]<-as.numeric(subset(EF_lit,Modele=="def"&Process=="Battery Assembly",select=LCI))
          #If GREET values:
          } else if (ef_bat_mdl=="greet"){
          #EF for Battery Assembly
          temp_dt[temp_dt$Phase=="Battery production and assembly"&temp_dt$Process=="Battery Assembly",lcia_cat]<-as.numeric(subset(EF_lit,Reference=="GREET"&Process=="Battery Assembly"&grepl(bat_type,`Battery type`),select=LCI))
          #EF for battery production
          #Check if the exact battery name is available
          if (bat_name%in%EF_lit$`Battery type`[EF_lit$Reference=="GREET"]){
            temp_dt[temp_dt$Phase=="Battery production and assembly"&temp_dt$Process=="Battery production",lcia_cat]<-as.numeric(subset(EF_lit,Reference=="GREET"&Process=="Battery production"&`Functional Unit`=="kg"&Technology%in%c(NA,techno)&`Battery type`==bat_name,LCI))
          } else { #If not exact battery name, consider battery type
            temp_dt[temp_dt$Phase=="Battery production and assembly"&temp_dt$Process=="Battery production",lcia_cat]<-as.numeric(subset(EF_lit,Reference=="GREET"&Process=="Battery production"&`Functional Unit`=="kg"&Technology%in%c(NA,techno)&grepl(bat_type,`Battery type`),LCI))
          }
          #Extreme values for battery production. Assume assembly is included
          } else {
          #EF for Battery Assembly
          temp_dt[temp_dt$Phase=="Battery production and assembly"&temp_dt$Process=="Battery Assembly",lcia_cat]<-0
          if (ef_bat_mdl=="low"){ef_bat_fun=min
          } else if (ef_bat_mdl=="high"){ef_bat_fun=max}
          temp_dt[temp_dt$Phase=="Battery production and assembly"&temp_dt$Process=="Battery production",lcia_cat]<-ef_bat_fun(subset(EF_lit,Process=="Battery production"&`Functional Unit`=="kg"&grepl(bat_type,`Battery type`)&Technology%in%c(NA,techno),LCI))
          }
        }
      }
    if (exists("lca_ef_lit",inherits = FALSE)){
      lca_ef_lit<-rbind(lca_ef_lit,temp_dt)
    } else {
      lca_ef_lit<-temp_dt
    }
  }
  #Get emission factors for material production, manufacturing
  temp_dt<-LCA_process[!grepl("Battery",LCA_process$Phase,ignore.case = TRUE),c("Unit","Phase","Process","Source")]
  #First conventional steel EF
  steel_mat_tc="Mild steel and other steels"
  phase_list=subset(temp_dt, Process==steel_mat_tc,select=Phase)
  for (phase in phase_list$Phase){
    #Consider default values
    if (ef_steel_mdl=="def"){
    temp_dt[temp_dt$Phase==phase&temp_dt$Process==steel_mat_tc,lcia_cat]<-as.numeric(subset(EF_lit,Modele=="def"&Phase==phase&Process==steel_mat_tc,select=LCI))
    #Consider high or low values
    } else if (ef_steel_mdl=="low"){
      temp_dt[temp_dt$Phase==phase&temp_dt$Process==steel_mat_tc,lcia_cat]<-min(subset(EF_lit,Phase==phase&Process==steel_mat_tc,select=LCI))
    } else if (ef_steel_mdl=="high"){
      temp_dt[temp_dt$Phase==phase&temp_dt$Process==steel_mat_tc,lcia_cat]<-max(subset(EF_lit,Phase==phase&Process==steel_mat_tc,select=LCI))
    }
  }
  #Second hss EF
  hss_mat_tc="HSS/AHSS"
  #Assumption: Manufacturing phase for HSS same than steel
  temp_dt[temp_dt$Phase=="Manufacturing"&temp_dt$Process==hss_mat_tc,lcia_cat]<-temp_dt[temp_dt$Phase=="Manufacturing"&temp_dt$Process==steel_mat_tc,lcia_cat]
  #Assumption: Secondary material production for HSS same than steel
  temp_dt[temp_dt$Phase=="Secondary Material Production"&temp_dt$Process==hss_mat_tc,lcia_cat]<-temp_dt[temp_dt$Phase=="Secondary Material Production"&temp_dt$Process==steel_mat_tc,lcia_cat]
  #Three scenarios for Primary material production
  if (ef_hss_mdl=="def"){
    ratio_hss_steel=1
  }  else if (ef_hss_mdl=="high"){
    ratio_hss_steel=1.10
  }
  temp_dt[temp_dt$Phase=="Primary Material Production"&temp_dt$Process==hss_mat_tc,lcia_cat]<-ratio_hss_steel*temp_dt[temp_dt$Phase=="Primary Material Production"&temp_dt$Process==steel_mat_tc,lcia_cat]
  #Format
  temp_dt[,"Technology"]<-NA
  #Aggregate EF
  lca_ef_lit<-rbind(lca_ef_lit,temp_dt)
  
  return(lca_ef_lit)
}
