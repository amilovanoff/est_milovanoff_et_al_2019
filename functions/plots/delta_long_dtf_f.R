#Create a data frame with the delta values for long tables
#dtf is the dataframe to use
#x_axis is the variable representing the x axis (usually Year)
#y_axis is the variable representing the values (usually value)
#ref_var is the variable containing the scenarios to compare with a reference case
#ref_case if the reference case in the variable ref_var
#out_var is a variable that we don't want to include (useful for sensitivity analysis)
delta_long_dtf_f<-function(dtf,x_axis,y_axis,ref_var,ref_case,out_var=NULL){
  delta_dtf<-dtf
  #param_col contains the column numbers of the variables to consider
  param_col<-which(!colnames(dtf) %in% c(x_axis,y_axis,out_var))
  #set_param contains the unique set of parameters to be considered
  set_param<-data.frame(dtf[!duplicated(dtf[,param_col]),param_col],stringsAsFactors = FALSE,check.names = FALSE)
  #Force colnames
  colnames(set_param)<-colnames(dtf)[param_col]
  #Create the data frame with the reference case associated with each set of parameters
  set_param_ref<-set_param
  set_param_ref[,ref_var]<-ref_case
  for (r in 1:nrow(set_param)){
    #rows contains the rows in the data frame to calculate the cumulative function
    rows<-which(sapply(1:nrow(delta_dtf),function(x)paste(delta_dtf[x,param_col],collapse=",")) %in% paste(set_param[r,],collapse=","))
    ref_rows<-which(sapply(1:nrow(delta_dtf),function(x)paste(delta_dtf[x,param_col],collapse=",")) %in% paste(set_param_ref[r,],collapse=","))
    x_axis_l<-unique(delta_dtf[rows,x_axis])
    for (x_val in x_axis_l){
      ref_value<-delta_dtf[intersect(ref_rows,which(delta_dtf[,x_axis]==x_val)),y_axis]
      delta_dtf[intersect(rows,which(delta_dtf[,x_axis]==x_val)),paste0("delta_",y_axis)]<-delta_dtf[intersect(rows,which(delta_dtf[,x_axis]==x_val)),y_axis]-ref_value
    }
  }
  return(delta_dtf)
}
