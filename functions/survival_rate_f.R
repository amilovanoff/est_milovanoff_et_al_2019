###>Function: Gives the probability of a vehicle of age "age" to survive knowing that it survives until age "age -1".
#scrap_rate_mdl: 1=empirical data from VISION ; 2=empirical data from TEDB ; 3=Logistic curve from Bandivadekar ; 4=Weibull distribution from Garcia et al.
survival_rate_f<-function(age,
                       size,
                       survival_rate_mdl=NA,
                       cumulative_rate="n",
                       scrappage_rate="n"){
  source("architecture/attribute_f.R",local=TRUE)
  attribute_f(fun_name="survival_rate_f")
  if (survival_rate_mdl%in%c(1,2)){
    #Define survival_rates_dt
    #Inputs
    survival_rates<-read.csv("inputs/survival_rates.csv",stringsAsFactors = FALSE)
    scrap_rate_src<-ifelse(survival_rate_mdl==1,"VISION","TEDB")
    #Extract survival rates data associated with source
    survival_rates_dt<-subset(survival_rates,subset=Source==scrap_rate_src)
    if(cumulative_rate=="y"){
      age_l<-0:age
    } else if (cumulative_rate=="n"){
      age_l<-age
    }
    survival_rate<-1
    for (a in age_l){
      #Check if age in dataset. Otherwise assume constant annual survival rate after the maximum age
      if (a>max(unique(survival_rates_dt$Age))){
        age_tbc<-max(unique(survival_rates_dt$Age))
      } else {
        age_tbc<-a
      }
      #Calculate survival_rate
      survival_rate<-survival_rate*as.numeric(subset(survival_rates_dt,Data=="Annual Survival rate"&Size%in%c(size,"all")&Age==age_tbc,select=value))
    }
    }else if(survival_rate_mdl==3){
      #Parameters definition for Bandivadekar model
      scrap_par=data.frame("lifetime" = c(16.9,15.5), "beta"= c(0.28,0.22))
      rownames(scrap_par)=c("Car", "Light truck")
      if(cumulative_rate=="y"){
        survival_rate<-1-1/(1+exp(-scrap_par$beta[which(rownames(scrap_par)==size)]*(age-scrap_par$lifetime[which(rownames(scrap_par)==size)])))
      } else {
        survival_rate<-(1-1/(1+exp(-scrap_par$beta[which(rownames(scrap_par)==size)]*(age-scrap_par$lifetime[which(rownames(scrap_par)==size)]))))/(1-1/(1+exp(-scrap_par$beta[which(rownames(scrap_par)==size)]*(age-1-scrap_par$lifetime[which(rownames(scrap_par)==size)]))))
      }
      }else if(survival_rate_mdl==4){
        #Weibull distribution from Garcia et al.
        scrap_par=data.frame("lambda" = c(11,11), "mu"= c(35,35))
        rownames(scrap_par)=c("Car", "Light truck")
        if(cumulative_rate=="y"){
          survival_rate<-exp(-((age+scrap_par$lambda[which(rownames(scrap_par)==size)])/scrap_par$mu[which(rownames(scrap_par)==size)])^scrap_par$lambda[which(rownames(scrap_par)==size)])
        } else {
          survival_rate<-exp(-((age+scrap_par$lambda[which(rownames(scrap_par)==size)])/scrap_par$mu[which(rownames(scrap_par)==size)])^scrap_par$lambda[which(rownames(scrap_par)==size)])/(exp(-((age-1+scrap_par$lambda[which(rownames(scrap_par)==size)])/scrap_par$mu[which(rownames(scrap_par)==size)])^scrap_par$lambda[which(rownames(scrap_par)==size)]))
        }
      }
  #Scrappage of survival
  if(scrappage_rate=="y"){
    rate<-1-survival_rate
  } else {
    rate<-survival_rate
  }
  return(rate)
}


