###>Function: Calculate the life cycle GHG emissions of the light-duty fleet by life cycle processes, stages and total.
fleet_lca_f<-function(last_yr=NA,
                      lcia_method=NA,
                      lcia_cat=NA,
                      fast_mode="n"){
  #Source
  source("architecture/attribute_f.R",local=TRUE)
  attribute_f("fleet_lca_f")
  #Sources
  source("functions/fleet_mfa_f.R",local=TRUE)
  source("functions/lca_ef_greet_f.R",local=TRUE)
  source("functions/lca_ef_elec_f.R",local=TRUE)
  source("functions/lca_ef_lit_f.R",local=TRUE)
  source("functions/lca_ef_prim_alu_f.R",local=TRUE)
  #Input files
  LCA_process <- read.csv("inputs/LCA_process.csv", stringsAsFactors = FALSE, check.names = FALSE)
  recovery_rate <- read.csv("inputs/recovery_rate_scen.csv", stringsAsFactors = FALSE, check.names = FALSE)
  fuel_conv<-read.csv("inputs/fuel_conversion.csv", stringsAsFactors = FALSE, check.names = FALSE)
  #Functions' Outpus
  fleet_mc_proj_f_res<-do.call(fun_res_f,list(fun_name="fleet_mc_proj_f",fast_mode=fast_mode))
  comp_wgt_dt<-fleet_mc_proj_f_res[["comp_wgt_dt"]]
  fleet_fuel_u_f_res<-do.call(fun_res_f,list(fun_name="fleet_fuel_u_f",fast_mode=fast_mode))
  fleet_fuel_use_dt<- fleet_fuel_u_f_res[["fuel_use"]]
  fleet_stock_f_res<-do.call(fun_res_f,list(fun_name="fleet_stock_f",fast_mode=fast_mode))
  fleet_scrap<-fleet_stock_f_res[["fleet_scrap"]]
  fleet_new<-fleet_stock_f_res[["fleet_new"]]
  fleet_mfa_f_res<-do.call(fun_res_f,list(fun_name="fleet_mfa_f",fast_mode=fast_mode))
  lca_ef_greet<-do.call(lca_ef_greet_f,list())
  lca_ef_elec<-do.call(lca_ef_elec_f,list())
  lca_ef_lit<-do.call(lca_ef_lit_f,list())
  lca_ef_prim_alu_f_res<-do.call(lca_ef_prim_alu_f,list(fast_mode=fast_mode))
  lca_ef_prim_alu<-lca_ef_prim_alu_f_res[["lca_ef_prim_alu"]]
  #Other parameters
  emi_inv<-lcia_cat
  LWscen_l<-unique(fleet_fuel_use_dt$Scenario)
  #Create the environmental matrix
  lca_env_mat<-LCA_process[,c("Unit","Source","Phase","Process")]
  lca_env_mat[,emi_inv]<-0
  #Output files
  dt_col<-c("LWscen","Year","Phase","Process",emi_inv)
  dyn_LCI<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  for (LWscen in LWscen_l){
      #mat_dem is the Material Demand matrix. To be used for Manufacturing
      mat_dem<-subset(fleet_mfa_f_res[["mat_dem_dt"]],Scenario==LWscen)
      rownames(mat_dem)<-mat_dem$Material
      #mat_prim is the Primary material production matrix. To be used for Primary Material Production
      mat_prim<-subset(fleet_mfa_f_res[["mat_prim_dt"]],Scenario==LWscen)
      rownames(mat_prim)<-mat_prim$Material
      #mat_sec is the Secondary material production matrix (internal and external). To be used for Primary Material Production.
      mat_sec<-subset(fleet_mfa_f_res[["mat_sec_dt"]],Scenario==LWscen)
      rownames(mat_sec)<-mat_sec$Material
      #fuel_use if the fuel use matrix. To be used for Fuel production and use.
      fuel_use<-fleet_fuel_use_dt[fleet_fuel_use_dt$Scenario==LWscen,]
      for (y in 2015:2050){
        #Fill lca_env_mat with GREET emission factors
        for (i in which(lca_env_mat$Source=="GREET")){
          lca_env_mat[i,emi_inv]<-as.numeric(subset(lca_ef_greet,Phase==lca_env_mat[i,"Phase"]&Process==lca_env_mat[i,"Process"],emi_inv))
        }
        #Fill lca_env_mat with literature emission factors except battery
        for (i in which(lca_env_mat$Source=="lit"&lca_env_mat$Phase!="Battery production and assembly")){
          lca_env_mat[i,emi_inv]<-as.numeric(subset(lca_ef_lit,Phase==lca_env_mat[i,"Phase"]&Process==lca_env_mat[i,"Process"],emi_inv))
        }
        #Fill lca_env_mat with own emission factors for primary aluminum
        lca_env_mat[lca_env_mat$Source=="own" & lca_env_mat$Phase=="Primary Material Production" & grepl("Aluminum",lca_env_mat$Process),emi_inv]<-
          lca_ef_prim_alu[lca_ef_prim_alu$Year == y,"value"]
        #Fill lca_env_mat with ecoInvent emissions factors for electricity production
        lca_env_mat[lca_env_mat$Source=="ecoInvent" & lca_env_mat$Phase=="Fuel Production" & lca_env_mat$Process=="Electricity",emi_inv]<-
          lca_ef_elec[lca_ef_elec$Year == y,"value"]
        rows<-(nrow(dyn_LCI)+1):(nrow(dyn_LCI)+nrow(lca_env_mat))
        #new_veh is the amount of new vehicles sold in year y
        new_veh<-sum(subset(fleet_new,Year==y,Value))
        #scrap_veh is the amount of scraped vehicles in year y regardless of the technology or age.
        scrap_veh <-sum(subset(fleet_scrap,Year==y,Value))
        dyn_LCI[rows,c("LWscen","Year","Phase","Process")]<-c(LWscen,y,lca_env_mat[,c("Phase","Process")])
        for (col in emi_inv){
          #Fill dyn LCI with Primary Material Production
          for (mat in unique(dyn_LCI$Process[intersect(which(dyn_LCI$Phase=="Primary Material Production"),rows)])){
            dyn_LCI[intersect(which(dyn_LCI$Phase=="Primary Material Production"&dyn_LCI$Process==mat),rows),col]<-
              lca_env_mat[which(lca_env_mat$Phase=="Primary Material Production"&lca_env_mat$Process==mat),col]*mat_prim[mat,as.character(y)]
          }
          #Fill dyn LCI with Secondary Material Production
          for (mat in unique(dyn_LCI$Process[intersect(which(dyn_LCI$Phase=="Secondary Material Production"),rows)])){
            dyn_LCI[intersect(which(dyn_LCI$Phase=="Secondary Material Production"&dyn_LCI$Process==mat),rows),col]<-
              lca_env_mat[which(lca_env_mat$Phase=="Secondary Material Production"&lca_env_mat$Process==mat),col]*mat_sec[mat,as.character(y)]
          }
          #Fill dyn_LCI with Vehicle Assembly
          dyn_LCI[intersect(which(dyn_LCI$Phase=="Manufacturing"&dyn_LCI$Process=="Vehicle Assembly"),rows),col]<-
            lca_env_mat[which(lca_env_mat$Phase=="Manufacturing"&lca_env_mat$Process=="Vehicle Assembly"),col]*new_veh
          #Fill dyn LCI with material manufacturing
          for (mat in unique(dyn_LCI$Process[intersect(which(dyn_LCI$Phase=="Manufacturing"&dyn_LCI$Process!="Vehicle Assembly"),rows)])){
            dyn_LCI[intersect(which(dyn_LCI$Phase=="Manufacturing"&dyn_LCI$Process==mat),rows),col]<-
              lca_env_mat[which(lca_env_mat$Phase=="Manufacturing"&lca_env_mat$Process==mat),col]*mat_dem[mat,as.character(y)]
          }
          #Fill dyn LCI with battery production and assembly
          techno_bat<-unique(comp_wgt_dt$Technology[comp_wgt_dt$Subcomponent=="EV Battery"])
          lcia_bat<-c(0,0)
          for (size in c("Car","Light truck")){
            for (techno in techno_bat){
              #Fill lca_env_mat with technology specific battery emission factors
              for (i in which(lca_env_mat$Source=="lit"&lca_env_mat$Phase=="Battery production and assembly")){
                lca_env_mat[i,emi_inv]<-as.numeric(subset(lca_ef_lit,Phase==lca_env_mat[i,"Phase"]&Process==lca_env_mat[i,"Process"]&Technology%in%c(techno,NA),emi_inv))
              }
              #Battery weight
              bat_wgt<-comp_wgt_dt[comp_wgt_dt$Scenario==LWscen&comp_wgt_dt$Size==size&comp_wgt_dt$Technology==techno&comp_wgt_dt$Subcomponent=="EV Battery",as.character(y)]
              #Number of vehicles
              techno_num<-as.numeric(subset(fleet_new,Size==size & Technology==techno & Year==y,Value))
              #Emissions of battery production
              lcia_bat<-lcia_bat+lca_env_mat[lca_env_mat$Phase=="Battery production and assembly",col]*bat_wgt*techno_num
            }
          }
          dyn_LCI[intersect(which(dyn_LCI$Phase=="Battery production and assembly"),rows),col]<-lcia_bat
          #Fill dyn_LCI with Fuel Production
          for (fuel in unique(dyn_LCI$Process[intersect(which(dyn_LCI$Phase=="Fuel Production"),rows)])){
            #fuel_dist_eff is the fuel distribution efficiency for fuel. This represents the lossess of the production and distribution chains
            fuel_dist_eff<-as.numeric(subset(fuel_conv,subset = Data == "Distribution efficiency" & Fuel == fuel,select = value))
            dyn_LCI[intersect(which(dyn_LCI$Phase=="Fuel Production"&dyn_LCI$Process==fuel),rows),col]<-
              lca_env_mat[which(lca_env_mat$Phase=="Fuel Production"&lca_env_mat$Process==fuel),col]*
              fuel_use[fuel_use$Fuel==fuel,as.character(y)]/
              fuel_dist_eff
          }
          #Fill dyn_LCI with Fuel Use
          for (fuel in unique(dyn_LCI$Process[intersect(which(dyn_LCI$Phase=="Fuel Use"),rows)])){
            dyn_LCI[intersect(which(dyn_LCI$Phase=="Fuel Use"&dyn_LCI$Process==fuel),rows),col]<-
              lca_env_mat[which(lca_env_mat$Phase=="Fuel Use"&lca_env_mat$Process==fuel),col]*fuel_use[fuel_use$Fuel==fuel,as.character(y)]
          }
          #Fill dyn_LCI with Vehicle Disposal
          dyn_LCI[intersect(which(dyn_LCI$Phase=="End of Life"&dyn_LCI$Process=="Vehicle Disposal"),rows),col]<-
            lca_env_mat[which(lca_env_mat$Phase=="End of Life"&lca_env_mat$Process=="Vehicle Disposal"),col]*scrap_veh
      }
    }
  }
  #Create dyn_LCI by aggregating by process
  agg.formula<-reformulate(termlabels = setdiff(colnames(dyn_LCI),c("Process",emi_inv)),response = emi_inv)
  dyn_LCI_phase<-aggregate(data = dyn_LCI,
                           agg.formula,
                           FUN=sum)
  #Create dyn_LCI by aggregating by phase
  agg.formula<-reformulate(termlabels = setdiff(colnames(dyn_LCI),c("Process","Phase",emi_inv)),response = emi_inv)
  dyn_LCI_tot<-aggregate(data = dyn_LCI,
                           agg.formula,
                           FUN=sum)
  return(list(dyn_LCI=dyn_LCI,
              dyn_LCI_phase=dyn_LCI_phase,
              dyn_LCI_tot=dyn_LCI_tot))
}
#fleet_lca_f_res<-do.call(fleet_lca_f,list(fast_mode="y"))
#save(list="fleet_lca_f_res",file="interm_results/fleet_lca_f_def.RData")
