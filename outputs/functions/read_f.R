#Function: 
fun_res_f<-function(fun_name=NA,fast_mode="n"){
  #Environmental setup
  ##If the environment which contains the functions' results does not exist, it creates it.
  if (!exists("fun.res.env",where = .GlobalEnv)){
    assign("fun.res.env",new.env(),envir = .GlobalEnv)
  }
  #Check if the results of the function are not alread in fun.res.env
  if (exists(paste0(fun_name,"_res"),where = fun.res.env)){
    #Get the results of the function from fun.res.env
    fun_res <- get(paste0(fun_name,"_res"), envir = fun.res.env)
  } else {
    if (fast_mode=="y"){
      #Load default outputs. It is not in good format, extract the format and assign to good names in fun.res.env
      load(paste0("outputs/out_def/",fun_name,"_def.RData"),envir = fun.res.env)
      temp_res <- get("results_l",envir = fun.res.env)[["Scenario:Default"]]
      remove("results_l",envir = fun.res.env)
      assign(paste0(fun_name,"_res"),temp_res[which(names(temp_res)!="Scenario")],envir = fun.res.env)
      fun_res <- get(paste0(fun_name,"_res"), envir = fun.res.env)
    } else {
      #Read the function, generate the results and assign it in fun.res.env
      source(paste0("functions/",fun_name,".R"),local=TRUE)
      fun_res <- do.call(get(fun_name),list())
      assign(paste0(fun_name,"_res"),fun_res,envir = fun.res.env)
    }
  }
  return(fun_res)
}

#Function:Read the output of function "function_tbc" simulated under scenario "scen_tbc" with default values for explicit parameters
read_def_outputs_f<-function(function_tbc="n",scen_tbc="def"){
  load(file=paste0("outputs/out_def/",function_tbc,"_",scen_tbc,".RData"))
  #Output
  def_output_l <- list()
  dts_names_l <- setdiff(names(results_l[[1]]),"Scenario")
  for (dts_name in dts_names_l){
    #Clean the environment
    if (exists("dts1",inherits = FALSE)){remove(list="dts1")}
    for (i in 1:length(results_l)){
      temp_dt<-results_l[[i]][[dts_name]]
      #Get name of scenario
      scen_name<-results_l[[i]][["Scenario"]]
      #Update temp_dt
      temp_dt[,"Scenario"] <- scen_name
      #Merge results
      if (exists("dts1")){
        dts1<-rbind(dts1,temp_dt)
      } else {
        dts1<-temp_dt
      }
    }
    def_output_l[[length(def_output_l) + 1]] <- dts1
    #Rename new element with par and par_value
    names(def_output_l)[length(def_output_l)] <- dts_name
  }
  return(def_output_l)
}

#Function that read the output "dts_names" of simulation "sim_tbc" of function "function_tbc"
read_simulation_f<-function(function_tbc,sim_tbc,dts_names){
  library(readxl)
  simulations <- as.data.frame(read_excel("outputs/simulations.xlsx",sheet = function_tbc))
  #Delete rows not associated with sim_tbc
  sim_attribute_value<-simulations[simulations$Simulation==sim_tbc,]
  #Delete columns with NA
  sim_attribute_value<-sim_attribute_value[,!is.na(sim_attribute_value)[1,]]
  #Rdata to load
  load(paste0("outputs/out_sim/",function_tbc,"_",sim_tbc,".RData"))
  #Other parameters
  param_tbc<-sim_attribute_value[sim_attribute_value$Type!="par","Attribute"]
  param_tbk<-sim_attribute_value[sim_attribute_value$Type=="attr_tk","Attribute"]
  sim_cols<-which(!colnames(sim_attribute_value)%in%c("Simulation","Attribute","Type"))
  if (exists("dts1",inherits = FALSE)){remove(list="dts1")}
  for (i in 1:length(results_l)){
    temp_dt<-cbind(results_l[[i]][[dts_names]],
                   as.data.frame(results_l[[i]][param_tbc],stringsAsFactors = FALSE,check.names = FALSE))
    if (exists("dts1")){
      dts1<-rbind(dts1,temp_dt)
    } else {
      dts1<-temp_dt
    }
  }
  #
  case_l<-as.character(sim_attribute_value[sim_attribute_value$Attribute=="name",sim_cols])
  temp_dt<-dts1
  for (r in 1:nrow(temp_dt)){
    case_test<-sapply(sim_cols,function(x)identical(as.character(temp_dt[r,param_tbc]),as.character(sim_attribute_value[sim_attribute_value$Attribute%in%param_tbc,x])))
    if (any(case_test)){
      col_n<-sim_cols[case_test]
      temp_dt[r,"Case"]<-sim_attribute_value[sim_attribute_value$Attribute=="name",col_n]
    } else {
      temp_dt[r,"Case"]<-NA
    }
  }
  #Delete the rows without case
  dts2<-subset(temp_dt,subset=!is.na(Case))[,which(!colnames(temp_dt)%in%setdiff(param_tbc,param_tbk))]
  return(dts2)
}

#Function that read the simulation results
read_sens_lw_timing<-function(function_tbc,sim_tbc,dts_names){
  library(readxl)
  simulations <- as.data.frame(read_excel("outputs/simulation_sens_lw_timing.xlsx",sheet = function_tbc))
  #Delete rows not associated with sim_tbc
  sim_args_value<-simulations[simulations$Simulation==sim_tbc,]
  #Delete columns with NA
  sim_args_value<-sim_args_value[,!is.na(sim_args_value)[1,]]
  #Rdata to load
  load(paste0("outputs/out_sim/",function_tbc,"_lw_timing_",sim_tbc,".RData"))
  #Other parameters
  param_case<-sim_args_value[sim_args_value$Type%in%c("sim_par_tk","attr_tk","attr"),"Attribute"]
  param_tbc<-c(param_case,"lw_first_yr")
  param_tbk<-c(sim_args_value[sim_args_value$Type%in%c("sim_par_tk","attr_tk"),"Attribute"],"lw_first_yr")
  sim_cols<-which(!colnames(sim_args_value)%in%c("Simulation","Attribute","Type"))
  if (exists("dts1",inherits = FALSE)){remove(list="dts1")}
  for (i in 1:length(results_l)){
    temp_dt<-cbind(results_l[[i]][[dts_names]],
                   as.data.frame(results_l[[i]][param_tbc],stringsAsFactors = FALSE,check.names = FALSE))
    if (exists("dts1")){
      dts1<-rbind(dts1,temp_dt)
    } else {
      dts1<-temp_dt
    }
  }
  #
  case_l<-as.character(sim_args_value[sim_args_value$Attribute=="name",sim_cols])
  temp_dt<-dts1
  for (r in 1:nrow(temp_dt)){
    case_test<-sapply(sim_cols,function(x)identical(as.character(temp_dt[r,param_case]),as.character(sim_args_value[sim_args_value$Attribute%in%param_case,x])))
    if (any(case_test)){
      col_n<-sim_cols[case_test]
      temp_dt[r,"Case"]<-sim_args_value[sim_args_value$Attribute=="name",col_n]
    } else {
      temp_dt[r,"Case"]<-NA
    }
  }
  #Delete the rows without case
  dts2<-subset(temp_dt,subset=!is.na(Case))[,which(!colnames(temp_dt)%in%setdiff(param_tbc,param_tbk))]
  return(dts2)
}

#Function that read the outputs "dts_names" of sensitivty analysis "sens_tbc" or on complete sensitivity analysis.
read_sens_analysis_f<-function(function_tbc,sens_tbc="complete",scen_tbc="def",dts_names){
  #Read output files
  load(file=paste0("outputs/out_sens_a/",function_tbc,"_",sens_tbc,"_",scen_tbc,".RData"))
  #Clean the environment
  if (exists("dts1",inherits = FALSE)){remove(list="dts1")}
  #Get default table
  results <- do.call(read_def_outputs_f,list(function_tbc=function_tbc,scen_tbc=scen_tbc))
  dts1 <- results[[dts_names]]
  dts1[,"Sens_attribute"] <- "all"
  dts1[,"Attribute_value"] <- "Default"
  for (i in 1:length(results_l)){
    temp_dt <- results_l[[i]][[dts_names]]
    #Get name of the sensitivity analysis
    temp_name <- names(results_l)[i]
    #Extract parameter names from name
    par <- substring(temp_name, 0,as.numeric(regexpr(pattern=":",temp_name))-1)
    #Extract parameter value from name
    par_value <- substring(names(results_l)[i], as.numeric(regexpr(pattern=":",names(results_l)[i]))+1)
    #scen_name
    scen_name <- results_l[[i]][["Scenario"]]
    #Update temp_dt
    temp_dt[,"Scenario"] <- scen_name
    temp_dt[,"Sens_attribute"] <- par
    temp_dt[,"Attribute_value"] <- par_value
    #Merge results
    dts1<-rbind(dts1,temp_dt)
  }
  return(dts1)
}

read_min_max_f <- function(function_tbc="fleet_lca_f",
                            scen_tbc="def",
                            sens_tbc="complete",
                            dts_names="dyn_LCI_tot",
                            y_axis="GWP100",
                            x_axis="Year",
                            x_val=2050,
                            var_tbc = "LWscen",
                            case_ref="BAU",
                            case_tbs="Aluminum Maximum",
                            var_to_sum="n",
                            cumulative="y"){
  source("outputs/functions/read_f.R")
  if ("Complete"%in%sens_tbc){
    sens_tbc_name <- sens_tbc
  } else {
    sens_tbc_name <- paste0("sens",paste(gsub("sens","",sens_tbc),collapse = "-"))
  }
  #Write results
  load(file=paste0("outputs/out_sens_a/",function_tbc,"_min_max_",sens_tbc_name,"_",scen_tbc,".RData"))
  #Clean the environment
  if (exists("dts",inherits = FALSE)){remove(list="dts")}
  dts <- do.call(read_def_outputs_f,list(function_tbc=function_tbc,scen_tbc=scen_tbc))[[dts_names]]
  dts[,"Case"] <- "def"
  for (i in 1:length(results_l)){
    temp_dt <- results_l[[i]][[dts_names]]
    #Get name of the results simulation
    temp_name <- names(results_l)[i]
    #scen_name
    scen_name <- results_l[[i]][["Scenario"]]
    simulation_name <- results_l[[i]][["Case"]]
    #Update temp_dt
    temp_dt[,"Scenario"] <- scen_name
    temp_dt[,"Case"] <- simulation_name
    #Merge results
    dts<-rbind(dts,temp_dt)
  }
  return(dts)
}