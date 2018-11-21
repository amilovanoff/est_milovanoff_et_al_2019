#Compute the pay_back time
#dtf is the dataframe to use
#x_axis is the variable representing the x axis (usually Year)
#y_axis is the variable representing the values (usually value)
#f_x_val is the final value of the x_axis to consider
#ref_var is the variable containing the scenarios to compare with a reference case
#ref_case if the reference case in the variable ref_var
pay_back_time_f<-function(dtf,y_axis,x_axis,i_x_val=2016,f_x_val="last",ref_var,ref_case){
  #param_col contains the column numbers of the variables to consider
  param_col<-which(!colnames(dtf) %in% c(y_axis,x_axis))
  #set_param contains the unique set of parameters to be considered
  set_param<-data.frame(dtf[!duplicated(dtf[,param_col]),param_col],stringsAsFactors = FALSE,check.names = FALSE)
  #Force colnames
  colnames(set_param)<-colnames(dtf)[param_col]
  #Delete the rows that contain the reference case
  set_param<-data.frame(set_param[set_param[,ref_var]!=ref_case,],stringsAsFactors = FALSE,check.names = FALSE)
  #Force colnames
  colnames(set_param)<-colnames(dtf)[param_col]
  #Create the data frame with the reference case associated with each set of parameters
  set_param_ref<-set_param
  set_param_ref[,ref_var]<-ref_case
  #Output file
  pay_back_dt<-set_param
  for (r in 1:nrow(set_param)){
    if (i_x_val=="lw_first_yr"){
      first_yr<-as.numeric(set_param[r,i_x_val])
    } else {
      first_yr<-as.numeric(i_x_val)
    }
    #rows contains the rows in the data frame to calculate the cumulative function
    rows<-intersect(which(sapply(1:nrow(dtf),function(x)paste(dtf[x,param_col],collapse=",")) %in% paste(set_param[r,],collapse=",")),
                    which(dtf[,x_axis]>=(first_yr-1)))
    ref_rows<-intersect(which(sapply(1:nrow(dtf),function(x)paste(dtf[x,param_col],collapse=",")) %in% paste(set_param_ref[r,],collapse=",")),
                        which(dtf[,x_axis]>=(first_yr-1)))
    #Order rows from the smaller to higher values of cum_param
    rows<-rows[order(dtf[rows,x_axis])]
    ref_rows<-ref_rows[order(dtf[ref_rows,x_axis])]
    #Create temp_dt: Difference between case and reference case
    temp_dt<-data.frame(dtf[rows,y_axis]-dtf[ref_rows,y_axis])
    rownames(temp_dt)<-dtf[rows,x_axis]
    if (any(temp_dt<0)){
      #row_min is row which the difference is 0 or negative
      row_min<-min(which(temp_dt<0))
      #Estimate pay_back time
      if (row_min==1){
        pay_back=0
      } else {
        pay_back<-as.numeric(rownames(temp_dt)[row_min-1])-temp_dt[row_min-1,]/(temp_dt[row_min,]-temp_dt[row_min-1,])-first_yr+1
      }
    } else {
      pay_back<-NA
      }
    pay_back_dt[r,"Payback"]<-pay_back
    if (f_x_val=="last"){
      pay_back_dt[r,"Final changes"]<-temp_dt[nrow(temp_dt),1]
    }
    
    }
    return(pay_back_dt)
}
