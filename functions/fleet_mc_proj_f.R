###>Function: Creates the material composition of prospective LDV based on scenarios
fleet_mc_proj_f <- function(LWscen_tbc=NA,
                            last_yr = NA,
                            lw_first_yr = NA,
                            lw_last_yr = NA,
                            mackenzie_svg = NA,
                            alonso_svg = NA,
                            sms_cutoff = NA,
                            feature_wgt = NA,
                            fast_mode="n") {
  #Source
  source("architecture/attribute_f.R",local = TRUE)
  #Assign default value
  attribute_f(fun_name = "fleet_mc_proj_f")
  #Library
  library(tidyr)
  #Called functions
  source("functions/sub_fac_f.R",local = TRUE)
  source("functions/alonso_sms_f.R",local = TRUE)
  source("functions/mackenzie_pr_f.R",local = TRUE)
  source("functions/battery_r_f.R",local = TRUE)
  #Input files
  comp_dt = read.csv("inputs/component_equivalency.csv",stringsAsFactors = FALSE,check.names = FALSE)
  LW_sc_dt <-read.csv("inputs/LW_scenario_substitution.csv",stringsAsFactors = FALSE,check.names = FALSE)
  vh_techno <- read.csv("inputs/vehicle_technology.csv", stringsAsFactors = FALSE, check.names = FALSE)
  #Functions called
  fleet_hist_fc_f_res <- do.call(fun_res_f,list(fun_name="fleet_fc_hist_f",fast_mode=fast_mode))
  fleet_hist_fc <- fleet_hist_fc_f_res[["fleet_fc_hist"]]
  fleet_i_mat_cont_f_res <- do.call(fun_res_f,list(fun_name="fleet_i_mat_cont_f",fast_mode=fast_mode))
  fleet_mat_cont <- fleet_i_mat_cont_f_res[["fleet_mat_cont"]]
  #Other parameters
  if (LWscen_tbc %in% c("all","own")){
    LWscen_l<-append(unique(LW_sc_dt$LW_scenario),"BAU")
  } else {
    LWscen_l<-LWscen_tbc
  }
  #Creation output files
  #fleet_mat_comp is the dynamic material composition per size and techno
  dt_col<-c("Scenario","Size", "Technology", "Material",t(2015:last_yr))
  fleet_mat_comp<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  #sub_fac_used contains the substitution factors used in the different components
  dt_col<-c("Year","Component","Replaced material", "Replacing material", "Substitution factor")
  sub_fac_used<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  dt_col<-c("Scenario","Size", "Technology","Subcomponent",t(2015:last_yr))
  #comp_wgt contains the subcomponent weight per scenario, technology and size
  comp_wgt<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  #pr_svg contains the primary mass savings per subcomponent and scenario
  pr_svg<-comp_wgt
  #sec_svg contains the secondary mass savings per subcompoent and scenario based on Alonso's model
  sec_svg<-pr_svg
  #pwt_svg contains the secondary mass savings for powertrain reziging based on MacKenzie equations
  pwt_svg<-pr_svg
  #bat_svg contains the mass savings associated with the battery resizing
  bat_svg<-pr_svg
  #Loop for ligthweight scenarios
  for (LWscen in LWscen_l) {
    #Loop for size
    for (size in c("Car","Light truck")){
      #Loop for technology
      for (techno in unique(vh_techno$own)){
        if (grepl("BEV",techno) | grepl("PHEV",techno)){
          FC_dt <- subset(fleet_hist_fc,Technology==techno & Size==size & `Fuel type`=="Electricity")
        } else {FC_dt=""}
        #i_mat_cont (1st version) is the initial material composition per component (in matrix)
        i_mat_cont <- spread(data=subset(fleet_mat_cont,Size==size & Technology==techno,select=-c(Size,Technology,Component)),key=Subcomponent,value=Weight,fill=0)
        rownames(i_mat_cont) <- i_mat_cont$Material
        i_mat_cont[,"Material"] <- NULL
        i_mat_cont <- rbind(i_mat_cont,Total=colSums(i_mat_cont))
        i_mat_cont <- cbind(i_mat_cont,Total=rowSums(i_mat_cont))
        #Format fleet_mt_comp: rw_mt the row numbers to fill.
        rw_mt<-(nrow(fleet_mat_comp)+1):(nrow(fleet_mat_comp)+nrow(i_mat_cont))
        #temp_l: list of colnames to update with the values
        temp_l<-list(Size=size,Technology=techno,Scenario=LWscen,Material=rownames(i_mat_cont),"2015"=i_mat_cont[,"Total"])
        for (col in names(temp_l)){
        fleet_mat_comp[rw_mt,col]<-temp_l[[col]]
        }
        #Format pr_svg and sec_svg: rw_svg the row numbers to fill
        rw_svg<-(nrow(pr_svg)+1):(nrow(pr_svg)+ncol(i_mat_cont))
        #temp_l: list of colnames to update with the values
        temp_l<-list(Size=size,Technology=techno,Scenario=LWscen,Subcomponent=colnames(i_mat_cont),"2015"=0)
        for (col in names(temp_l)){
          comp_wgt[rw_svg,col]<-temp_l[[col]]
          pr_svg[rw_svg,col]<-temp_l[[col]]
          sec_svg[rw_svg,col]<-temp_l[[col]]
          pwt_svg[rw_svg,col]<-temp_l[[col]]
          bat_svg[rw_svg,col]<-temp_l[[col]]
        }
        comp_wgt[rw_svg,"2015"]<-t(i_mat_cont["Total",comp_wgt$Subcomponent[rw_svg]])
        mat_cont<-i_mat_cont
        #Loop for all the years
        for (y in 2016:last_yr){
          mat_cont_LW <- mat_cont
          #Apply lightweighting
          if (y%in%(lw_first_yr:lw_last_yr) & any(LW_sc_dt$LW_scenario==LWscen&(sapply(1:nrow(LW_sc_dt),function(x)techno %in% unlist(strsplit(LW_sc_dt$Technology[x],",")))|LW_sc_dt$Technology=="Glo"))){
          #rpl_rates it the data frame with the replacement rates of the components
          rpl_rates<-LW_sc_dt[LW_sc_dt$LW_scenario==LWscen&(sapply(1:nrow(LW_sc_dt),function(x)techno %in% unlist(strsplit(LW_sc_dt$Technology[x],",")))|LW_sc_dt$Technology=="Glo"),]
          #mat_comp_obj contains the relative material content in the subcomponent of the different replaced material to achieve by lw_last_yr
          dt_col<-c("Subcomponent","Replaced material","Objective")
          mat_comp_obj<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
          #rpl_subs_l contains the list of subcomponents to be edited
          rpl_subc_l<-unique(rpl_rates$Subcomponent[rpl_rates$Subcomponent %in% colnames(mat_cont)])
          #Fill mat_comp_obj
          for (subc in rpl_subc_l){
            for (rpd_mat in unique(rpl_rates$`Replaced material`[rpl_rates$Subcomponent==subc])){
              mat_comp_obj[nrow(mat_comp_obj)+1,c("Subcomponent","Replaced material")]<-c(subc,rpd_mat)
              #The objective is the initial material content minus the percent of this content to be replaced
              mat_comp_obj[nrow(mat_comp_obj),"Objective"]<-i_mat_cont[rpd_mat,subc]/i_mat_cont["Total",subc]*(1-sum(rpl_rates$`Replacement rates`[rpl_rates$Subcomponent==subc&rpl_rates$`Replaced material`==rpd_mat]))
            }
          }
          for (i in 1:nrow(mat_comp_obj)){
            #subcomponent: Subcomponent to be edited
            subcomponent <- mat_comp_obj[i,"Subcomponent"]
            #rpd_mat: Replaced material in the subcomponent
            rpd_mat <- mat_comp_obj[i,"Replaced material"]
            #rpl_obj: is the final material content objective of the replaced material in subcomponent
            rpl_obj<-mat_comp_obj[i,"Objective"]
            #rpd_wgt is the weight of replaced material to substitute at year y
            #It is based on curent relative material content of rpd in subcomponent and the replacing objective.
            rpd_wgt<-(mat_cont_LW[rpd_mat, subcomponent]/mat_cont_LW["Total", subcomponent]-rpl_obj)/
              (lw_last_yr-y+1)*mat_cont_LW["Total", subcomponent]
            #rpg_mat_l is the list of replacing material
            rpg_mat_l <- rpl_rates$`Replacing material`[rpl_rates$Subcomponent==subcomponent&rpl_rates$`Replaced material`==rpd_mat]
            for (rpg_mat in rpg_mat_l){
              #sub_fac: Substitution factor of rpd_mat by rpg_mat in subcomponent
              sub_fac<-do.call(sub_fac_f,
                               list(rpd_mat = rpd_mat, rpg_mat = rpg_mat, component = subcomponent))
              #rpg_mat_ratio: rpg_mat_ratio is the ratio of rpg_mat to substitute rpd_mat compared to the other replacing materials
              rpg_mat_ratio<-rpl_rates$`Replacement rates`[rpl_rates$Subcomponent==subcomponent
                                                           &rpl_rates$`Replaced material`==rpd_mat
                                                           &rpl_rates$`Replacing material`==rpg_mat]/
                sum(rpl_rates$`Replacement rates`[rpl_rates$Subcomponent==subcomponent
                                                            &rpl_rates$`Replaced material`==rpd_mat])
              #rpd_wgt_rpg is the weight of rpd subsitued by rpg
              rpd_wgt_rpg<-rpd_wgt*rpg_mat_ratio
              #Replacement: rpl_ratio is a ratio against the total subcomponent weight
              mat_cont_LW[rpd_mat, subcomponent] <-mat_cont_LW[rpd_mat, subcomponent] - rpd_wgt_rpg
              #Increases the replacing material content
              mat_cont_LW[rpg_mat, subcomponent] <-mat_cont_LW[rpg_mat, subcomponent] + rpd_wgt_rpg*sub_fac
              #Extracts the subsitution factors used
              sub_fac_used[nrow(sub_fac_used)+ 1,]<-c(y,subcomponent,rpd_mat,rpg_mat,sub_fac)
            }
          }
        #Update totals in mat_cont_LW
        mat_cont_LW["Total", colnames(mat_cont_LW)!="Total"] <- colSums(mat_cont_LW[rownames(mat_cont_LW)!="Total", colnames(mat_cont_LW)!="Total"])
        mat_cont_LW[,"Total"] <- rowSums(mat_cont_LW[, colnames(mat_cont_LW)!="Total"])
        }
        #Apply annual weight increase due to feature content. 
        #Scenarios built from MacKenzie et al. 2014
        if (feature_wgt=="def"){
          feat_ann_wgt_inc <- 3.1
        } else if(feature_wgt=="n"){
          feat_ann_wgt_inc <- 0
        } else if(feature_wgt=="high"){
          feat_ann_wgt_inc <- 6.4
        }
        #Assumption: Weight increase due to features only changes "Other" in "Interior"
        mat_cont_LW["Other","Interior"] <- mat_cont["Other","Interior"] + feat_ann_wgt_inc
        #Fill pr_svg
        pr_svg[rw_svg,as.character(y)] <- t(mat_cont_LW["Total",]-mat_cont["Total",])
          #Battery resizing (First iteration)
        battery_r<-do.call(battery_r_f,
                           list(year=y, size=size, techno=techno, mat_cont_LW=mat_cont_LW, mat_cont=mat_cont, FC_dt=FC_dt))
        mat_cont_LW_bat<-battery_r[[1]]
        FC_dt<-battery_r[[2]]
        #fill bat_svg
        bat_svg[rw_svg,as.character(y)]<-t(mat_cont_LW_bat["Total",]-mat_cont_LW["Total",])
        #Powertrain resizing (First iteration)
        if (mackenzie_svg=="y"){
          #Powertrain resizing(First iteration)
          MK_results<-do.call(mackenzie_pr_f,
                              list(year=y, size=size, techno=techno, mat_cont_LW=mat_cont_LW_bat, mat_cont=mat_cont))
          mat_cont_LW_pwt<-MK_results[[1]]
          #Fill pwt_svg
          pwt_svg[rw_svg,as.character(y)]<-t(mat_cont_LW_pwt["Total",]-mat_cont_LW_bat["Total",])
        } else (mat_cont_LW_pwt<-mat_cont_LW_bat)
        #Secondary mass savings without powertrain and battery resizing (Alonso et al. model)(First iteration)
        if (alonso_svg=="y"){
        mat_cont_LW_sms <- do.call(alonso_sms_f,
                                   list(mat_cont = mat_cont, mat_cont_LW = mat_cont_LW_pwt))
        #Fill sec_svg
        sec_svg[rw_svg,as.character(y)]<-t(mat_cont_LW_sms["Total",]-mat_cont_LW_pwt["Total",])
        } else {mat_cont_LW_sms<-mat_cont_LW_pwt}
        while (mat_cont_LW_bat["Total","Total"]-mat_cont_LW_sms["Total","Total"]>sms_cutoff){
          #Battery resizing (Iteration): The latests weight is represented in mat_cont_LW_sms. The battery has not been resized since mat_cont_LW_bat
          battery_r<-do.call(battery_r_f,
                             list(year=y, size=size, techno=techno, mat_cont_LW=mat_cont_LW_sms, mat_cont=mat_cont_LW_bat, FC_dt=FC_dt))
          mat_cont_LW_bat<-battery_r[[1]]
          FC_dt<-battery_r[[2]]
          #fill bat_svg
          bat_svg[rw_svg,as.character(y)]<-bat_svg[rw_svg,as.character(y)]+t(mat_cont_LW_bat["Total",]-mat_cont_LW_sms["Total",])
          #Powertrain resizing (Iteration)
          if (mackenzie_svg=="y"){
            #The latest weight: mat_cont_bat. The powertrain has not been resized since mat_cont_LW_pwt
            MK_results<-do.call(mackenzie_pr_f,
                                list(year=y, size=size, techno=techno, mat_cont_LW=mat_cont_LW_bat, mat_cont=mat_cont_LW_pwt))
            mat_cont_LW_pwt<-MK_results[[1]]
            #Fill pwt_svg
            pwt_svg[rw_svg,as.character(y)]<-pwt_svg[rw_svg,as.character(y)]+t(mat_cont_LW_pwt["Total",]-mat_cont_LW_bat["Total",])
          } else (mat_cont_LW_pwt<-mat_cont_LW_bat)
          #Secondary mass savings without powertrain and battery resizing (Alonso et al. model)(First iteration)
          if (alonso_svg=="y"){
            #The latest weight: mat_cont_LW_pwt. Components not resized since mat_cont_LW_sms
            mat_cont_LW_sms <- do.call(alonso_sms_f,
                                       list(mat_cont = mat_cont_LW_sms, mat_cont_LW = mat_cont_LW_pwt))
            #Fill sec_svg
            sec_svg[rw_svg,as.character(y)]<-sec_svg[rw_svg,as.character(y)]+t(mat_cont_LW_sms["Total",]-mat_cont_LW_pwt["Total",])
          } else {mat_cont_LW_sms<-mat_cont_LW_pwt}
        }
        #Fill comp_wgt
        comp_wgt[rw_svg,as.character(y)]<-t(mat_cont_LW_sms["Total",comp_wgt$Subcomponent[rw_svg]])
        #Fill fleet_mat_comp
        fleet_mat_comp[rw_mt,as.character(y)]<-mat_cont_LW_sms[fleet_mat_comp$Material[rw_mt],"Total"]
        mat_cont<-mat_cont_LW_sms
      }
    }
    }
  }
  fleet_wgt_dt <- subset(fleet_mat_comp,Material=="Total")
  fleet_wgt_svg_dt<-fleet_wgt_dt
  for (y in 2016:last_yr){
    fleet_wgt_svg_dt[,as.character(y)]<-fleet_wgt_dt[,as.character(y)]-fleet_wgt_dt[,as.character(y-1)]
  }
  return(list(fleet_mt_comp_proj=fleet_mat_comp,
              comp_wgt_dt=comp_wgt,
              fleet_wgt_dt=fleet_wgt_dt,
              fleet_wgt_svg_dt=fleet_wgt_svg_dt,
              sub_fac_used=sub_fac_used,
              pr_svg=pr_svg,
              sec_svg=sec_svg,
              pwt_svg=pwt_svg,
              bat_svg=bat_svg))
}
