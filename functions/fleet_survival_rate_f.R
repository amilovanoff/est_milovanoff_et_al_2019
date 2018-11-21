fleet_survival_rate_f<-function(survival_rate_mdl=NA,
                                cumulative_rate="n",
                                scrappage_rate="n"){
  source("architecture/attribute_f.R",local=TRUE)
  attribute_f(fun_name="fleet_survival_rate_f")
  #Functions
  source("functions/survival_rate_f.R",local=TRUE)
  #Create output file
  dt_col<-c("Data","Size","Age","value")
  fleet_survival_rate_dt<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  size_tbc<-c("Car","Light truck")
  age_tbc<-0:30
  for (size in size_tbc){
    for (age in age_tbc){
      sur_rate<-do.call(survival_rate_f,list(age=age, size=size, survival_rate_mdl=survival_rate_mdl,cumulative_rate=cumulative_rate,scrappage_rate=scrappage_rate))
      fleet_survival_rate_dt[nrow(fleet_survival_rate_dt)+1,c("Size","Age","value")]<-c(size,age,sur_rate)
    }
  }
  data_name<-"rate"
  if (scrappage_rate=="n"){
    data_name<-paste("Survival",data_name)
  } else {
    data_name<-paste("Scrappage",data_name)
  }
  if (cumulative_rate=="y"){data_name<-paste("Cumulative",data_name)}
  fleet_survival_rate_dt[,"Data"]<-data_name
  #Format
  fleet_survival_rate_dt[,"Age"]<-as.numeric(fleet_survival_rate_dt[,"Age"])
  fleet_survival_rate_dt[,"value"]<-as.numeric(fleet_survival_rate_dt[,"value"])
  return(fleet_survival_rate_dt)
}
