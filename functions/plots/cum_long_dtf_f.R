#Create a data frame with the cumulative values for long tables
#dtf is the dataframe to use
#cum_variable is the variable (column) we want to cumulate
#cum_param is the variable (column) leading the accumulation (generally time, such as years)
cum_long_dtf_f<-function(dtf,cum_variable,cum_param="Year"){
  cum_dtf<-dtf
  param_col<-which(!colnames(dtf) %in% c(cum_variable,cum_param))
  set_param<-data.frame(dtf[!duplicated(dtf[,param_col]),param_col],stringsAsFactors = FALSE)
  for (r in 1:nrow(set_param)){
    #rows contains the rows in the data frame to calculate the cumulative function
    rows<-which(sapply(1:nrow(dtf),function(x)paste(dtf[x,param_col],collapse=",")) %in% paste(set_param[r,],collapse=","))
    #Order rows from the smaller to higher values of cum_param
    rows<-rows[order(dtf[rows,cum_param])]
    #Calculate cumulative values for cum_variable and substitute the values in the dataframe
    cum_dtf[rows,cum_variable]<-cumsum(dtf[rows,cum_variable])
  }
  return(cum_dtf)
}