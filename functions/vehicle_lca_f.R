#Build the classic data of the vehicles included in the model
vehicle_lca_f<-function(aeo_scen = NA,
                        lcia_method=NA,
                        lcia_cat=NA,
                        vh_lca_yrs=NA,
                        lw_first_yr=NA,
                        lw_last_yr=NA,
                        fast_mode="n"){
  #Default values of arguments
  source("architecture/attribute_f.R",local=TRUE)
  attribute_f("vehicle_lca_f")
  #Source
  source("functions/lca_ef_greet_f.R",local=TRUE)
  source("functions/lca_ef_elec_f.R",local=TRUE)
  source("functions/lca_ef_lit_f.R",local=TRUE)
  source("functions/lca_ef_prim_alu_f.R",local=TRUE)
  #Input files
  conv<-read.csv("inputs/conversion_units.csv", stringsAsFactors = FALSE, check.names = FALSE,row.names = 1)
  LCA_process <- read.csv("inputs/LCA_process.csv", stringsAsFactors = FALSE, check.names = FALSE)
  vh_techno <- read.csv("inputs/vehicle_technology.csv", stringsAsFactors = FALSE, check.names = FALSE)
  nw_scrap_rt <-read.csv("inputs/new_scrap_rate.csv",stringsAsFactors = FALSE,check.names = FALSE)
  vkmt_lifetime <- read.csv("inputs/vkmt_lifetime.csv", stringsAsFactors = FALSE, check.names = FALSE)
  annual_mileage<-read.csv("inputs/annual_mileage_TEDB.csv", stringsAsFactors = FALSE, check.names = FALSE)
  #Functions' Outputs
  lca_ef_greet<-do.call(lca_ef_greet_f,list())
  lca_ef_elec<-do.call(lca_ef_elec_f,list())
  lca_ef_lit<-do.call(lca_ef_lit_f,list())
  lca_ef_prim_alu_f_res<-do.call(lca_ef_prim_alu_f,list(fast_mode=fast_mode))
  lca_ef_prim_alu<-lca_ef_prim_alu_f_res[["lca_ef_prim_alu"]]
  fleet_mc_proj_f_res<-do.call(fun_res_f,list(fun_name="fleet_mc_proj_f",fast_mode=fast_mode))
  mat_comp_15to50<-fleet_mc_proj_f_res[["fleet_mt_comp_proj"]]
  comp_wgt_dt<-fleet_mc_proj_f_res[["comp_wgt_dt"]]
  fleet_fc_proj_f_res<-do.call(fun_res_f,list(fun_name="fleet_fc_proj_f",fast_mode=fast_mode))
  fleet_FC_dt<-fleet_fc_proj_f_res[["fleet_FC_dt"]]
  fleet_vkmt_share_f_res<-do.call(fun_res_f,list(fun_name="fleet_vkmt_share_f",fast_mode=fast_mode))
  fleet_vkmt_share<-fleet_vkmt_share_f_res[["fleet_vkmt_share"]]
  fleet_mfa_f_res<-do.call(fun_res_f,list(fun_name="fleet_mfa_f",fast_mode=fast_mode))
  #Other parameters
  LWscen_tbc<-sort(unique(mat_comp_15to50$Scenario))
  lcia_tbc<-lcia_cat
  years_tbc<-as.numeric(unlist(strsplit(vh_lca_yrs,split=" ")))
  #Output file
  dt_col<-c("LWscen","i_year","Age","Size","Technology","Phase","Process","LCIA","value")
  dyn_LCI<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  for (LWscen in LWscen_tbc){
    #mat_dem is the Material Demand matrix. To be used for Manufacturing
    mat_prim_share<-subset(fleet_mfa_f_res[["mat_prim_share_dt"]],Scenario==LWscen)
    rownames(mat_prim_share)<-mat_prim_share$Material
    #mat_sec is the share of Secondary material production (internal and external).
    mat_sec_share<-subset(fleet_mfa_f_res[["mat_sec_share_dt"]],Scenario==LWscen)
    rownames(mat_sec_share)<-mat_sec_share$Material
    for (year in years_tbc){
      for (size in c("Car","Light truck")){
        for (techno in unique(vh_techno$own)){
          if (grepl("BEV",techno) | grepl("PHEV",techno)){
            tmp_techno <- substring(techno,0,as.numeric(regexpr(pattern="[[:digit:]]{1}",techno))-1)
          } else {
            tmp_techno <- techno
          }
          
          for (lcia in lcia_tbc){
            #Create the environmental matrix
            lca_env_mat<-LCA_process[,c("Unit","Source","Phase","Process")]
            lca_env_mat[,lcia]<-0
            #Fill lca_env_mat with GREET emission factors
            for (i in which(lca_env_mat$Source=="GREET")){
              lca_env_mat[i,lcia]<-as.numeric(subset(lca_ef_greet,Phase==lca_env_mat[i,"Phase"]&Process==lca_env_mat[i,"Process"],lcia))
            }
            #Fill lca_env_mat with literature emission factors except battery
            for (i in which(lca_env_mat$Source=="lit"&lca_env_mat$Phase!="Battery production and assembly")){
              lca_env_mat[i,lcia]<-as.numeric(subset(lca_ef_lit,Phase==lca_env_mat[i,"Phase"]&Process==lca_env_mat[i,"Process"],lcia))
            }
            #Fill lca_env_mat with own emission factors for primary aluminum
            lca_env_mat[lca_env_mat$Source=="own" & lca_env_mat$Phase=="Primary Material Production" & grepl("Aluminum",lca_env_mat$Process),lcia]<-
              lca_ef_prim_alu[lca_ef_prim_alu$Year == year,"value"]
            
            
            #Fill lca_env_mat with technology specific battery emission factors
            for (i in which(lca_env_mat$Source=="lit"&lca_env_mat$Phase=="Battery production and assembly")){
              lca_env_mat[i,lcia]<-as.numeric(subset(lca_ef_lit,Phase==lca_env_mat[i,"Phase"]&Process==lca_env_mat[i,"Process"]&Technology%in%c(tmp_techno,NA),lcia))
            }
            
            #lc_vkmt is the VKMT over the lifetime of the technology
            lc_vkmt <- subset(vkmt_lifetime,subset = Size == size & Technology == techno)[,"Value"]
            #lifetime is the lifetime of the vehicle to achieve lc_vkmt (in year). We add one the annual mileage because not an age, but years
            lifetime <- annual_mileage[min(which(cumsum(annual_mileage[,size]*conv["km","1 mile"])>lc_vkmt)),"Vehicle age"]+1
            #annual_vkt_dt contains the annual kilometer traveled
            dt_col<-c("Age","Annual VKT")
            annual_vkt_dt<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = lifetime),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
            annual_vkt_dt[,"Age"]<-1:lifetime
            annual_vkt_dt[annual_vkt_dt$Age%in%1:(lifetime-1),"Annual VKT"]<-annual_mileage[annual_mileage$`Vehicle age`%in%0:(lifetime-2),size]*conv["km","1 mile"]
            annual_vkt_dt[annual_vkt_dt$Age == lifetime,"Annual VKT"]<-lc_vkmt-sum(annual_vkt_dt[annual_vkt_dt$Age%in%1:(lifetime-1),"Annual VKT"])
            #List of fuels used by the technology
            fuel_l <-unlist(strsplit(vh_techno$`Fuel type`[which(vh_techno$own == techno)][1], ";"))
            #Material composition of the technology, size in the LW scenario at year
            mat_comp<-as.matrix(mat_comp_15to50[which(mat_comp_15to50$Size==size
                                                      &mat_comp_15to50$Technology==techno
                                                      &mat_comp_15to50$Scenario==LWscen),as.character(year)])
            rownames(mat_comp)<-mat_comp_15to50[which(mat_comp_15to50$Size==size
                                                      &mat_comp_15to50$Technology==techno
                                                      &mat_comp_15to50$Scenario==LWscen),"Material"]
            mat_comp<-as.matrix(mat_comp[-which(rownames(mat_comp)%in%c("Other","Total")),])
            #mat_dem is Material demand. Higher than the material composition because of new scrap losses.
            mat_dem<-mat_comp
            #mat_prim is primary material demand.
            mat_prim<-mat_dem
            #mat_sec is the secondary material demand
            mat_sec<-mat_dem
            #Fill mat_dem, mat_prim, mat_sec
            for (mat in rownames(mat_dem)) {
              mat_dem[mat,] <-mat_comp[mat,] /
                (1 - nw_scrap_rt$Rate[nw_scrap_rt$Material == mat])
              mat_prim[mat,]<-mat_dem[mat,]*mat_prim_share[mat,as.character(year)]
              mat_sec[mat,]<-mat_dem[mat,]*mat_sec_share[mat,as.character(year)]
            }
            #Fill dyn_LCI with age 0 (Material production, vehicle manufacturing and assembly)
            rows<-(nrow(dyn_LCI)+1):(nrow(dyn_LCI)+nrow(lca_env_mat))
            dyn_LCI[rows,c("LWscen","i_year","Age","Size","Technology","Phase","Process","LCIA","value")]<-
              c(LWscen,as.character(year),0,size,techno,lca_env_mat[,c("Phase","Process")],lcia,0)
            #Fill dyn_LCI with Primary Material Production
            for (mat in unique(dyn_LCI$Process[intersect(which(dyn_LCI$Phase=="Primary Material Production"),rows)])){
              dyn_LCI[intersect(which(dyn_LCI$Phase=="Primary Material Production"&dyn_LCI$Process==mat),rows),"value"]<-
                lca_env_mat[which(lca_env_mat$Phase=="Primary Material Production"&lca_env_mat$Process==mat),lcia]*mat_prim[mat,1]
            }
            #Fill dyn_LCI with Secondary Material Production
            for (mat in unique(dyn_LCI$Process[intersect(which(dyn_LCI$Phase=="Secondary Material Production"),rows)])){
              dyn_LCI[intersect(which(dyn_LCI$Phase=="Secondary Material Production"&dyn_LCI$Process==mat),rows),"value"]<-
                lca_env_mat[which(lca_env_mat$Phase=="Secondary Material Production"&lca_env_mat$Process==mat),lcia]*mat_sec[mat,1]
            }
            #Fill dyn_LCI with Vehicle Assembly
            dyn_LCI[intersect(which(dyn_LCI$Phase=="Manufacturing"&dyn_LCI$Process=="Vehicle Assembly"),rows),"value"]<-
              lca_env_mat[which(lca_env_mat$Phase=="Manufacturing"&lca_env_mat$Process=="Vehicle Assembly"),lcia]*1
            #Fill dyn_LCI with material manufacturing
            for (mat in unique(dyn_LCI$Process[intersect(which(dyn_LCI$Phase=="Manufacturing"&dyn_LCI$Process!="Vehicle Assembly"),rows)])){
              dyn_LCI[intersect(which(dyn_LCI$Phase=="Manufacturing"&dyn_LCI$Process==mat),rows),"value"]<-
                lca_env_mat[which(lca_env_mat$Phase=="Manufacturing"&lca_env_mat$Process==mat),lcia]*mat_dem[mat,1]
            }
            #Fill dyn_LCI with Battery Assembly and Production
            if ("EV Battery"%in%unique(comp_wgt_dt$Subcomponent[comp_wgt_dt$Technology==techno])){
              dyn_LCI[intersect(which(dyn_LCI$Phase=="Battery production and assembly"),rows),"value"]<-lca_env_mat[lca_env_mat$Phase=="Battery production and assembly",lcia]*
                as.numeric(subset(comp_wgt_dt,subset=Scenario==LWscen&Size==size&Technology==techno&Subcomponent=="EV Battery",select=as.character(year)))
            } else {
              dyn_LCI[intersect(which(dyn_LCI$Phase=="Battery production and assembly"),rows),"value"]<-0
            }
            #Loop over vehicle age
            for (age in 1:lifetime){
              #Fill dyn_LCI with age (Fuel production and use)
              rows<-(nrow(dyn_LCI)+1):(nrow(dyn_LCI)+nrow(lca_env_mat))
              dyn_LCI[rows,c("LWscen","i_year","Age","Size","Technology","Phase","Process","LCIA","value")]<-
                c(LWscen,as.character(year),age,size,techno,lca_env_mat[,c("Phase","Process")],lcia,0)
              #Fill lca_env_mat with ecoInvent emissions factors for electricity production
              lca_env_mat[lca_env_mat$Source=="ecoInvent" & lca_env_mat$Phase=="Fuel Production" & lca_env_mat$Process=="Electricity",lcia]<-
                lca_ef_elec[lca_ef_elec$Year == (year+age-1),"value"]
              #annual_vkt is the annual vehicle kilometer traveled by the technology
              annual_vkt<-annual_vkt_dt[annual_vkt_dt$Age==age,"Annual VKT"]
              #Fill dyn_LCI with Fuel Production and use
              for (fuel in fuel_l){
                #Consider FC value of the lightweighted car (in lw_last_yr)
                fc_value <- subset(fleet_FC_dt,Size==size & Technology==techno & `Fuel type`==fuel & Scenario==LWscen & Year==year)[,"Value"]
                #vkmt_share is the share of distance traveled on fuel_type. If car older than 2015 models, consider VKMT share of 2015 models.
                vkmt_share<-as.numeric(subset(fleet_vkmt_share,
                                              subset= Scenario == LWscen & Size == size & Technology == techno & `Fuel type` == fuel,
                                              select = as.character(year)))
                fuel_used<-fc_value/100*vkmt_share*annual_vkt
                #Fill fuel production phase with fuel
                dyn_LCI[intersect(which(dyn_LCI$Phase=="Fuel Production"&dyn_LCI$Process==fuel),rows),"value"]<-
                  lca_env_mat[which(lca_env_mat$Phase=="Fuel Production"&lca_env_mat$Process==fuel),lcia]*fuel_used
                #Fill use phase with fuel
                dyn_LCI[intersect(which(dyn_LCI$Phase=="Fuel Use"&dyn_LCI$Process==fuel),rows),"value"]<-
                  lca_env_mat[which(lca_env_mat$Phase=="Fuel Use"&lca_env_mat$Process==fuel),lcia]*fuel_used
                }
            }
            #Fill dyn_LCI with Vehicle Disposal at the lifetime of the vehicle
            dyn_LCI[intersect(which(dyn_LCI$Phase=="End of Life"&dyn_LCI$Process=="Vehicle Disposal"),rows),"value"]<-
              lca_env_mat[which(lca_env_mat$Phase=="End of Life"&lca_env_mat$Process=="Vehicle Disposal"),lcia]*1
          }
        }
      }
    }
  }
  #Aggregate the dynamic Inventory to create the aggregated LCI
  agg.formula <- reformulate(termlabels = setdiff(colnames(dyn_LCI),c("Age","value")),response = "value")
  stat_LCI <- aggregate(data = dyn_LCI,agg.formula,FUN=sum)
  return(list(stat_LCI=stat_LCI,dyn_LCI=dyn_LCI))
}
#vehicle_lca_f_res<-do.call(vehicle_lca_f,list(fast_mode="n"))
#save(list="vehicle_lca_f_res",file="interm_results/vehicle_lca_f_def.RData")
