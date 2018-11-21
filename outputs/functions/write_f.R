#Function: Run and write outputs of list of functions ("function_tbc") under scenario "scen_tbc" with default values for all explicit attributes
write_def_outputs_f <- function(function_tbc,scen_tbc="def"){
  #Input files
  attribute_value <- read.csv("architecture/attribute_value.csv",stringsAsFactors = FALSE, check.names = FALSE)
  scenarios <- read.csv("outputs/scenarios.csv",stringsAsFactors = FALSE, check.names = FALSE)
  #Fill empty spaces with NA
  scenarios[scenarios==""] <- NA
  #Delete rows not associated with sim_tbc
  scen_attribute_value<- subset(scenarios, Scenario==scen_tbc)
  #Delete columns with NA
  scen_attribute_value <- scen_attribute_value[,!is.na(scen_attribute_value)[1,]]
  #scen_attr_l is the list of parameters to be updated in the simulation
  scen_attr_l <- scen_attribute_value[scen_attribute_value$Type!="par","Attribute"]
  #sim_cols contains the columns that contains different simulation arguments
  scen_cols<-which(!colnames(scen_attribute_value)%in%c("Scenario","Attribute","Type"))[!duplicated(t(scen_attribute_value[scen_attribute_value$Attribute %in% scen_attr_l,!colnames(scen_attribute_value)%in%c("Scenario","Attribute","Type")]))]
  for (i in 1:length(function_tbc)){
    #Output files
    results_l<-list()
    fct <- function_tbc[i]
    #Update scenario attributes for all scenarios
    for (col in scen_cols){
      scen_name <- scen_attribute_value[scen_attribute_value$Attribute == "name",col]
      #Update values to use in functions by default values
      attribute_value$Value <- attribute_value$Default
      #Change the arguments values to be used
      for (scen_attr in scen_attr_l) {
        attribute_value[attribute_value$Attribute == scen_attr, "Value"] <- scen_attribute_value[scen_attribute_value$Attribute == scen_attr,col]
      }
      write.csv(attribute_value, "architecture/attribute_value.csv", row.names = FALSE)
      #Call function
      source(paste0("functions/",fct,".R"),local=TRUE)
      #Get function results
      results <- append(do.call(get(fct), list()), setNames(scen_name,"Scenario"))
      #Store results in a new list element
      results_l[[length(results_l) + 1]] <- results
      #Rename new element with par and par_value
      names(results_l)[length(results_l)]<-paste0("Scenario",":",scen_name)
      #Clear the environment that contains functions results
      if (exists(x="fun.res.env",envir = .GlobalEnv)){
        remove(list="fun.res.env",envir = .GlobalEnv)
      }
    }
    save(list="results_l",file=paste0("outputs/out_def/",fct,"_",scen_tbc,".RData"))
  }
  #Update attribute values to use by default values
  attribute_value$Value <- attribute_value$Default
  write.csv(attribute_value, "architecture/attribute_value.csv", row.names = FALSE)
}

#Function that launch simulation "sim_tbc" of function "function_tbc" from simulations.xlsx. Store the results in outputs/out_sim.
write_simulation_f<-function(function_tbc,sim_tbc){
  library(readxl)
  #Input files
  attribute_value<-read.csv("architecture/attribute_value.csv",stringsAsFactors = FALSE, check.names = FALSE)
  #Update the values to use in attribute_value by default values
  attribute_value$Value <- attribute_value$Default
  simulations <- as.data.frame(read_excel("outputs/simulations.xlsx",sheet = function_tbc))
  #Delete rows not associated with sim_tbc
  sim_attribute_value<-simulations[simulations$Simulation==sim_tbc,]
  #Delete columns with NA
  sim_attribute_value<-sim_attribute_value[,!is.na(sim_attribute_value)[1,]]
  #param_tbc is the list of parameters to be updated in the simulation
  param_tbc<-sim_attribute_value[sim_attribute_value$Type!="par","Attribute"]
  #sim_cols contains the columns that contains different simulation arguments
  sim_cols<-which(!colnames(sim_attribute_value)%in%c("Simulation","Attribute","Type"))[!duplicated(t(sim_attribute_value[sim_attribute_value$Attribute %in% param_tbc,!colnames(sim_attribute_value)%in%c("Simulation","Attribute","Type")]))]
  #Output files
  results_l<-list()
  for (col in sim_cols){
    #Update values to use in functions by default values
    attribute_value$Value <- attribute_value$Default
    #Change the arguments values to be used
    for (par in param_tbc) {
      attribute_value[attribute_value$Attribute == par, "Value"] <- sim_attribute_value[sim_attribute_value$Attribute == par,col]
    }
    #Update the external file of the arguments values
    write.csv(attribute_value, "architecture/attribute_value.csv", row.names = FALSE)  
    #Source the function
    source(paste0("functions/",function_tbc,".R"),local=TRUE)
    #Clear the environment that contains functions results
    if (exists(x="fun.res.env",envir = .GlobalEnv)){
      remove(list="fun.res.env",envir = .GlobalEnv)
    }
    #Get results and store it in a list with arguments
    results <- append(do.call(get(function_tbc), list()), setNames(sim_attribute_value[sim_attribute_value$Attribute%in%param_tbc,col],param_tbc))
    #Store results in a new list element
    results_l[[length(results_l) + 1]] <- results
  }
  save(list="results_l",file=paste0("outputs/out_sim/",function_tbc,"_",sim_tbc,".RData"))
  #Update attribute values to use by default values
  attribute_value$Value <- attribute_value$Default
  write.csv(attribute_value, "architecture/attribute_value.csv", row.names = FALSE)
  #Clear the environment that contains functions 
  if (exists(x="fun.res.env",envir = .GlobalEnv)){
    remove(list="fun.res.env",envir = .GlobalEnv)
  }
}

#Function that launch simulations
fun_sens_lw_timing<-function(function_tbc,sim_tbc){
  library(readxl)
  #Input files
  attribute_value<-read.csv("architecture/attribute_value.csv",stringsAsFactors = FALSE, check.names = FALSE)
  #Update the values to use in attribute_value by default values
  attribute_value$Value <- attribute_value$Default
  simulations <- as.data.frame(read_excel("outputs/simulation_sens_lw_timing.xlsx",sheet = function_tbc))
  #Delete rows not associated with sim_tbc
  sim_attribute_value<-simulations[simulations$Simulation==sim_tbc,]
  #Delete columns with NA
  sim_attribute_value<-sim_attribute_value[,!is.na(sim_attribute_value)[1,]]
  #param_tbc is the list of parameters to be updated in the simulation
  param_tbc<-sim_attribute_value[grep("attr",sim_attribute_value$Type),"Attribute"]
  #sim_cols contains the columns that contains different simulation arguments
  sim_cols<-which(!colnames(sim_attribute_value)%in%c("Simulation","Attribute","Type"))[!duplicated(t(sim_attribute_value[sim_attribute_value$Attribute %in% param_tbc,!colnames(sim_attribute_value)%in%c("Simulation","Attribute","Type")]))]
  #Output files
  results_l<-list()
  for (col in sim_cols){
    #Update values to use in functions by default values
    attribute_value$Value <- attribute_value$Default
    #assess_period is the assessment period for the cumulative emissions
    assess_period <- as.numeric(sim_attribute_value[sim_attribute_value$Attribute=="assess_period",col])
    #lw_period is the period of the lightweighting penetration
    lw_period <- as.numeric(sim_attribute_value[sim_attribute_value$Attribute=="lw_period",col])
    #sim_yr contains the simulations years
    sim_yr <- sim_attribute_value[sim_attribute_value$Attribute=="sim_yr",col]
    if (grepl(":NA",sim_yr)){
      lw_first_yr_tbc<-as.numeric(substring(sim_yr,0,as.numeric(regexpr(pattern=":",sim_yr))-1)):(2050-max(c(assess_period,lw_period)))
    } else if(grepl(";",sim_yr)){
      lw_first_yr_tbc<-as.numeric(unlist(strsplit(sim_yr,split=";")))
    } else if(!is.na(as.numeric(sim_yr))){
      lw_first_yr_tbc<-as.numeric(sim_yr)
    }
    for (lw_first_yr in lw_first_yr_tbc){
      lw_last_yr<-lw_first_yr+lw_period
      #Change the arguments values to be used
      for (par in param_tbc) {
        attribute_value[attribute_value$Attribute == par, "Value"] <- sim_attribute_value[sim_attribute_value$Attribute == par,col]
      }
      #Update lw_first_yr and lw_last_yr
      attribute_value[attribute_value$Attribute == "lw_first_yr", "Value"] <- lw_first_yr
      attribute_value[attribute_value$Attribute == "lw_last_yr", "Value"] <- lw_last_yr
      #Update the external file of the arguments values
      write.csv(attribute_value, "architecture/attribute_value.csv", row.names = FALSE) 
      #Source the function
      source(paste0("functions/",function_tbc,".R"))
      #Create data frame to be merged in results
      dt_col<-c(param_tbc,"lw_first_yr","assess_period","lw_period")
      arguments_dt_tmp<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
      for (par in c(param_tbc,"assess_period","lw_period")){
        arguments_dt_tmp[1,par]<-sim_attribute_value[sim_attribute_value$Attribute==par,col]
      }
      arguments_dt_tmp[1,"lw_first_yr"]<-lw_first_yr
      #Get results and store it in a list with arguments
      results <- append(do.call(get(function_tbc), list()), arguments_dt_tmp)
      #Store results in a new list element
      results_l[[length(results_l) + 1]] <- results
      #Clear the environment that contains functions results
      #Clear the environment that contains functions 
      if (exists(x="fun.res.env",envir = .GlobalEnv)){
        remove(list="fun.res.env",envir = .GlobalEnv)
      }
    }
  }
  save(list="results_l",file=paste0("outputs/out_sim/",function_tbc,"_lw_timing_",sim_tbc,".RData"))
  #Update attribute values to use by default values
  attribute_value$Value <- attribute_value$Default
  write.csv(attribute_value, "architecture/attribute_value.csv", row.names = FALSE)
}

#Function that generate, aggregate and write outputs of sensitivty anaysis for function "function_tbc" using sensitivity analysis "sens_tbc" on scenario "scen_tbc"
write_sens_analysis_f<-function(function_tbc,sens_tbc="complete",scen_tbc="def"){
  #Input files
  attribute_value <- read.csv("architecture/attribute_value.csv",stringsAsFactors = FALSE, check.names = FALSE)
  scenarios <- read.csv("outputs/scenarios.csv",stringsAsFactors = FALSE, check.names = FALSE)
  #Fill empty spaces with NA
  scenarios[scenarios==""] <- NA
  #Delete rows not associated with sim_tbc
  scen_attribute_value<- subset(scenarios, Scenario==scen_tbc)
  #Delete columns with NA
  scen_attribute_value <- scen_attribute_value[,!is.na(scen_attribute_value)[1,]]
  #scen_attr_l is the list of parameters to be updated in the simulation
  scen_attr_l <- scen_attribute_value[scen_attribute_value$Type!="par","Attribute"]
  #sim_cols contains the columns that contains different simulation arguments
  scen_cols<-which(!colnames(scen_attribute_value)%in%c("Scenario","Attribute","Type"))[!duplicated(t(scen_attribute_value[scen_attribute_value$Attribute %in% scen_attr_l,!colnames(scen_attribute_value)%in%c("Scenario","Attribute","Type")]))]
  if (sens_tbc!="complete"){
    library(readxl)
    sens_analysis <- as.data.frame(read_excel("outputs/sens_analysis.xlsx",sheet = "sens_analysis"))
    param_tbc<-unlist(strsplit(sens_analysis[sens_analysis$Sensitivity==sens_tbc,"Attributes"],split=";"))
  } else if (sens_tbc=="complete"){
    function_attributes<-read.csv("architecture/function_attributes.csv",stringsAsFactors = FALSE,check.names = FALSE,na.strings = "")
    matrix_sources <- read.csv("architecture/function_sources_matrix.csv",stringsAsFactors = FALSE, check.names = FALSE,row.names = 1)
    #inv_matrix_sources show the depedencies of the matrix functions. Columns are the functions and rows are the dependent inputs
    inv_matrix_sources <- t(solve(diag(ncol(matrix_sources))-t(matrix_sources)))
    #source_list contains the list of sources that are directly or indirectly used by function_tbc
    source_list <- rownames(inv_matrix_sources)[which(inv_matrix_sources[,function_tbc]!=0)]
    #param_tbc is the list of attributes of the source_list functions
    param_tbc <- unique(subset(function_attributes,Function%in%source_list & Type=="Explicit")$Attribute)
  }
  #Output files
  results_l<-list()
  #Loop on param_tbc
  for(par in param_tbc){
    #Update values to use in functions by default values
    attribute_value$Value <- attribute_value$Default
    def_par <- subset(attribute_value,Attribute==par)$Default
    #Extract all values of par except def. Only simulation alternative scenarios.
    par_value_l <- setdiff(unlist(strsplit(subset(attribute_value,Attribute==par)$All,split=";")),def_par)
    if (length(par_value_l)>=1){
      for (par_value in par_value_l){
        #Update attribute_values
        attribute_value[attribute_value$Attribute == par, "Value"] <- par_value
        #Update scenario attributes for all scenarios
        for (col in scen_cols){
          scen_name <- scen_attribute_value[scen_attribute_value$Attribute == "name",col]
          #Change the arguments values to be used
          for (scen_attr in scen_attr_l) {
            attribute_value[attribute_value$Attribute == scen_attr, "Value"] <- scen_attribute_value[scen_attribute_value$Attribute == scen_attr,col]
          }
          write.csv(attribute_value, "architecture/attribute_value.csv", row.names = FALSE)
          #Call function
          source(paste0("functions/",function_tbc,".R"),local=TRUE)
          #Clear the environment that contains functions results
          if (exists(x="fun.res.env",envir = .GlobalEnv)){
            remove(list="fun.res.env",envir = .GlobalEnv)
          }
          #Get function results
          results <- append(do.call(get(function_tbc), list()), setNames(c(scen_name,par_value),c("Scenario",par)))
          #Store results in a new list element
          results_l[[length(results_l) + 1]] <- results
          #Rename new element with par and par_value
          names(results_l)[length(results_l)]<-paste0(par,":",par_value)
        }
      }
    }
  }
  #Write results
  save(list="results_l",file=paste0("outputs/out_sens_a/",function_tbc,"_",sens_tbc,"_",scen_tbc,".RData"))
  #Update attribute values to use by default values
  attribute_value$Value <- attribute_value$Default
  write.csv(attribute_value, "architecture/attribute_value.csv", row.names = FALSE)
  #Clear the environment that contains functions results
  if (exists(x="fun.res.env",envir = .GlobalEnv)){
    remove(list="fun.res.env",envir = .GlobalEnv)
  }
}

write_min_max_f <- function(function_tbc="fleet_lca_f",
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
  #Inputs
  source("functions/plots/tornado_f.R")
  tornado_f_res <- do.call(tornado_f,list(function_tbc=function_tbc,scen_tbc=scen_tbc,sens_tbc=sens_tbc,dts_names=dts_names,y_axis=y_axis,x_axis=x_axis,x_val=x_val,var_tbc = var_tbc,case_ref=case_ref,var_to_sum=var_to_sum,cumulative=cumulative))
  attribute_value_min <- tornado_f_res[["attribute_value_min"]]
  attribute_value_max <- tornado_f_res[["attribute_value_max"]]
  attribute_value<-read.csv("architecture/attribute_value.csv",stringsAsFactors = FALSE, check.names = FALSE)
  scenarios <- read.csv("outputs/scenarios.csv",stringsAsFactors = FALSE, check.names = FALSE)
  #Fill empty spaces with NA
  scenarios[scenarios==""] <- NA
  #Delete rows not associated with sim_tbc
  scen_attribute_value<- subset(scenarios, Scenario==scen_tbc)
  #Delete columns with NA
  scen_attribute_value <- scen_attribute_value[,!is.na(scen_attribute_value)[1,]]
  #scen_attr_l is the list of parameters to be updated in the simulation
  scen_attr_l <- scen_attribute_value[scen_attribute_value$Type!="par","Attribute"]
  #sim_cols contains the columns that contains different simulation arguments
  scen_cols<-which(!colnames(scen_attribute_value)%in%c("Scenario","Attribute","Type"))[!duplicated(t(scen_attribute_value[scen_attribute_value$Attribute %in% scen_attr_l,!colnames(scen_attribute_value)%in%c("Scenario","Attribute","Type")]))]
  #Output files
  results_l<-list()
  for (i in 1:2){
    tmp_attr_name <- c("attribute_value_min","attribute_value_max")[i]
    tmp_attr <- get(tmp_attr_name)
    #Update values to use in functions by default values
    attribute_value$Value <- attribute_value$Default
    for (attr in subset(tmp_attr,!is.na(get(case_tbs)))[,"Attribute"]){
      attribute_value[attribute_value$Attribute==attr,"Value"] <- subset(tmp_attr,Attribute==attr)[,case_tbs]
    }
    for (col in scen_cols){
      scen_name <- scen_attribute_value[scen_attribute_value$Attribute == "name",col]
      #Change the arguments values to be used
      for (scen_attr in scen_attr_l) {
        attribute_value[attribute_value$Attribute == scen_attr, "Value"] <- scen_attribute_value[scen_attribute_value$Attribute == scen_attr,col]
      }
      write.csv(attribute_value, "architecture/attribute_value.csv", row.names = FALSE)
      #Call function
      source(paste0("functions/",function_tbc,".R"),local=TRUE)
      #Clear the environment that contains functions results
      if (exists(x="fun.res.env",envir = .GlobalEnv)){
        remove(list="fun.res.env",envir = .GlobalEnv)
      }
      #Get function results
      results <- append(do.call(get(function_tbc), list()), setNames(c(scen_name,gsub("attribute_value_","",tmp_attr_name)),c("Scenario","Case")))
      #Store results in a new list element
      results_l[[length(results_l) + 1]] <- results
      #Rename new element with par and par_value
      names(results_l)[length(results_l)]<-paste0(scen_name,":",gsub("attribute_value_","",tmp_attr_name))
    }
  }
  if ("Complete"%in%sens_tbc){
    sens_tbc_name <- sens_tbc
  } else {
    sens_tbc_name <- paste0("sens",paste(gsub("sens","",sens_tbc),collapse = "-"))
  }
  #Write results
  save(list="results_l",file=paste0("outputs/out_sens_a/",function_tbc,"_min_max_",sens_tbc_name,"_",scen_tbc,".RData"))
  #Update attribute values to use by default values
  attribute_value$Value <- attribute_value$Default
  write.csv(attribute_value, "architecture/attribute_value.csv", row.names = FALSE)
  #Clear the environment that contains functions results
  if (exists(x="fun.res.env",envir = .GlobalEnv)){
    remove(list="fun.res.env",envir = .GlobalEnv)
  }
}
