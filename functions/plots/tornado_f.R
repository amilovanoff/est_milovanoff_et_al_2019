#Function: Returns a data frame of the sensitivty analysis results for function_tbc, the best case scenario arguments values and worst case scenario arguments values.
##x_axis = the variable representing the x axis (usually Year)
##y_axis = the variable representing the values (usually value)
##x_val = the value of x to consider
##var_tbc = the variable to consider (in delta calculations)
##case_ref = the value of reference for var_tbc to compare with
##var_to_sum = Variables to sum (aggregate)
##cumulative = Use cumulative values or not?
tornado_f<-function(function_tbc="fleet_ghg_f",scen_tbc="def",sens_tbc="complete",dts_names="fleet_ghg_fuel",y_axis="value",x_axis="Year",x_val=2030,var_tbc = "Scenario",case_ref="Constant",var_to_sum=c("Fuel","Stage","Process"),cumulative="n"){
  #Source
  source("functions/plots/cum_long_dtf_f.R")
  source("functions/plots/delta_long_dtf_f.R")
  source("outputs/functions/read_f.R",local=TRUE)
  #Inputs
  attribute_value<-read.csv("architecture/attribute_value.csv",stringsAsFactors = FALSE, check.names = FALSE)
  #For each sensitivity parameter, we consider the minimum and maximum value
  attribute_value_min <- attribute_value
  attribute_value_max <- attribute_value
  #Output: tornado_dtf is the data table to plot tornado graph
  tornado_dtf <- NULL
  for (sens_a in sens_tbc){
    temp_dtf <- do.call(read_sens_analysis_f,list(function_tbc=function_tbc,sens_tbc=sens_a,scen_tbc=scen_tbc,dts_names=dts_names))
    #Aggregate variables
    if ("n"%in%var_to_sum){
      agg_dtf <- temp_dtf
    } else {
      agg.formula <- reformulate(termlabels = setdiff(colnames(temp_dtf),var_to_sum),response = y_axis)
      agg_dtf <- aggregate(data = temp_dtf,agg.formula,FUN=sum)
    }
    #Calculative cumulative values if specified
    if (cumulative=="y"){
      #Calculate cumulative results
      dtf <- cum_long_dtf_f(dtf=agg_dtf,cum_variable = y_axis,cum_param=x_axis)
    } else{
      dtf <- agg_dtf
    }
    #Reduce dtf to only considered x_val
    dtf <- dtf[dtf[,x_axis]==x_val,]
    #if case_ref is "n", means that no delta is considered, only the absolute value
    if (case_ref=="n"){
      delta_dtf <- dtf
      #scen_list: List of scenarios to consider in calculations. All scenarios if no reference case
      scen_list <- unique(delta_dtf[,var_tbc])
    } else {
      #Calculate difference between case_ref and case_tbc for each sensitivity analysis
      delta_dtf <- delta_long_dtf_f(dtf=dtf, x_axis=x_axis, y_axis=y_axis, ref_var=var_tbc, ref_case=case_ref)
      #Replace old value columns by delta value columns
      delta_dtf <- delta_dtf[,colnames(delta_dtf)!=y_axis]
      colnames(delta_dtf)[colnames(delta_dtf)==paste0("delta_",y_axis)] <- y_axis
      #scen_list: List of scenarios to consider in calculations. All scenarios except reference case
      scen_list <- setdiff(unique(delta_dtf[,var_tbc]),case_ref)
    }
    #sens_par_l: List of attributes in the sensivitiy analyses
    sens_par_l <- setdiff(unique(delta_dtf$Sens_attribute),"all")
    #temp_dt_col is used to create the temporary dtf in loop
    temp_dt_col <- setdiff(colnames(delta_dtf),c(y_axis,"Attribute_value"))
    for (sens_par in sens_par_l){
      #def_val
      def_attr <- subset(attribute_value,Attribute==sens_par)$Default
      #par_name is the name of the attribute. If NA in the table, consider sens_par
      par_name<-ifelse(subset(attribute_value,Attribute==sens_par)$Description%in%c(NA,""),sens_par,subset(attribute_value,Attribute==sens_par)$Description)
      for (scen in scen_list){
        subset_dt <- subset(delta_dtf,Sens_attribute==sens_par & get(var_tbc)==scen)
        temp_dt <- subset_dt[1,temp_dt_col]
        def_attr_val <- as.numeric(subset(delta_dtf,Sens_attribute=="all" & get(var_tbc)==scen,get(y_axis)))
        #min_row is the row of min y_axis value (either minimum delta compared to case_ref or minimum value)
        min_row<-which.min(subset_dt[,y_axis])
        min_val <- subset_dt[min_row,y_axis]
        max_row<-which.max(subset_dt[,y_axis])
        max_val <- subset_dt[max_row,y_axis]
        if (min_val <= def_attr_val & max_val >= def_attr_val){
          attribute_value_min[attribute_value_min$Attribute==sens_par,scen] <- subset_dt[min_row,"Attribute_value"]
          temp_dt[1,"y_start"] <- min_val
          attribute_value_max[attribute_value_max$Attribute==sens_par,scen] <- subset_dt[max_row,"Attribute_value"]
          temp_dt[1,"y_end"] <- max_val
        } else if (min_val > def_attr_val){
          attribute_value_min[attribute_value_min$Attribute==sens_par,scen] <- def_attr
          temp_dt[1,"y_start"] <- def_attr_val
          attribute_value_max[attribute_value_max$Attribute==sens_par,scen] <- subset_dt[max_row,"Attribute_value"]
          temp_dt[1,"y_end"] <- max_val
        } else if (max_val < def_attr_val){
          attribute_value_min[attribute_value_min$Attribute==sens_par,scen] <- subset_dt[min_row,"Attribute_value"]
          temp_dt[1,"y_start"] <- min_val
          attribute_value_max[attribute_value_max$Attribute==sens_par,scen] <- def_attr
          temp_dt[1,"y_end"] <- def_attr_val
        }
        temp_dt[1,"def"] <- def_attr_val
        temp_dt[1,"Attribute_name"] <- par_name
        temp_dt[1,"Sens_analysis"] <- sens_a
        tornado_dtf <- rbind(tornado_dtf,temp_dt)
      }
    }
  }
  #Calculate delta in tornado_dtf
  tornado_dtf[,"delta"] <- tornado_dtf[,"y_end"]-tornado_dtf[,"y_start"]
  return(list(tornado_dtf=tornado_dtf,
              attribute_value_min=attribute_value_min,
              attribute_value_max=attribute_value_max))
  
}
