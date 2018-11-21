#Create the recovery rates per material 
rec_rate_f<-function(rec_scen=NA){
  #Assign the default values of arguments
  source("architecture/attribute_f.R",local = TRUE)
  #Assign default value
  attribute_f("rec_rate_f")
  #Library
  library(reshape2)
  #Input files
  rec_rate_hist <-read.csv("inputs/recovery_rate_hist.csv",stringsAsFactors = FALSE,check.names = FALSE)
  rec_rate_scen_dt<-read.csv("inputs/recovery_rate_scen.csv",stringsAsFactors = FALSE,check.names = FALSE)
  #Other parameters
  #Consider only data for rec_scen
  rec_rate_scen<-rec_rate_scen_dt[rec_rate_scen_dt$Scenario%in%c("All",rec_scen),]
  #Create output file
  rec_rate_proj<-rec_rate_hist
  #Loop for scrapped material]
  for (scrap_mat in unique(rec_rate_scen$`Scrapped material`)){
    #scen_yr is the objective year of the scenario
    scen_yr<-rec_rate_scen$Year[rec_rate_scen$`Scrapped material`==scrap_mat
                                &rec_rate_scen$Data=="Total recovery"]
    #Create the objective year row
    rec_rate_proj[nrow(rec_rate_proj)+1,c("Scenario","Scrapped material","Year")]<-c(rec_scen,scrap_mat,scen_yr)
    #Obtain the list of recovered material
    rec_mat_l<-unique(rec_rate_scen$`Recovered material`[rec_rate_scen$`Scrapped material`==scrap_mat 
                                                         &rec_rate_scen$Data=="Material contribution"])
    #Loop for recovered material
    for (rec_mat in rec_mat_l){
      #Fill the objective row with the amount of recovered material
    rec_rate_proj[rec_rate_proj$Scenario==rec_scen
                  &rec_rate_proj$`Scrapped material`==scrap_mat
                  &rec_rate_proj$Year==scen_yr,rec_mat]<-
      rec_rate_scen$Value[rec_rate_scen$`Scrapped material`==scrap_mat
                          &rec_rate_scen$`Recovered material`==rec_mat
                          &rec_rate_scen$Data=="Material contribution"]*
      rec_rate_scen$Value[rec_rate_scen$`Scrapped material`==scrap_mat
                          &rec_rate_scen$Data=="Total recovery"]
    }
    #fill the non-recovered materials with 0
    rec_rate_proj[rec_rate_proj$Scenario==rec_scen
                  &rec_rate_proj$`Scrapped material`==scrap_mat
                  &rec_rate_proj$Year==scen_yr,setdiff(colnames(rec_rate_proj),c(rec_mat_l,"Scenario","Scrapped material","Year","Loss"))]<-0
    #fill loss as sum of the recovery rate minus 1
    rec_rate_proj[rec_rate_proj$Scenario==rec_scen
                  &rec_rate_proj$`Scrapped material`==scrap_mat
                  &rec_rate_proj$Year==scen_yr,"Loss"]<-1-sum(rec_rate_proj[rec_rate_proj$Scenario==rec_scen
                                                                          &rec_rate_proj$`Scrapped material`==scrap_mat
                                                                          &rec_rate_proj$Year==scen_yr,
                                                                          setdiff(colnames(rec_rate_proj),c("Scenario","Scrapped material","Year","Loss"))])
    #Linear regression from 2016 to final year
    for (y in 2016:(scen_yr-1)){
      rec_rate_proj[nrow(rec_rate_proj)+1,c("Scenario","Scrapped material","Year")]<-c(rec_scen,scrap_mat,y)
      rec_rate_proj[rec_rate_proj$Scenario==rec_scen
                    &rec_rate_proj$`Scrapped material`==scrap_mat
                    &rec_rate_proj$Year==y,setdiff(colnames(rec_rate_proj),c("Scenario","Scrapped material","Year"))]<-
        rec_rate_proj[rec_rate_proj$Scenario%in%c(rec_scen,"All")
                      &rec_rate_proj$`Scrapped material`==scrap_mat
                      &rec_rate_proj$Year==(y-1),setdiff(colnames(rec_rate_proj),c("Scenario","Scrapped material","Year"))]+
        (rec_rate_proj[rec_rate_proj$Scenario%in%c(rec_scen,"All")
                      &rec_rate_proj$`Scrapped material`==scrap_mat
                      &rec_rate_proj$Year==(scen_yr),setdiff(colnames(rec_rate_proj),c("Scenario","Scrapped material","Year"))]-
        rec_rate_proj[rec_rate_proj$Scenario%in%c(rec_scen,"All")
                      &rec_rate_proj$`Scrapped material`==scrap_mat
                      &rec_rate_proj$Year==(2015),setdiff(colnames(rec_rate_proj),c("Scenario","Scrapped material","Year"))])/
        (scen_yr-2015)
    }
  }
  #Format output file
  rec_rate <-
    melt(rec_rate_proj,
         id = c("Scenario","Scrapped material","Year"),
         variable.name = "Recovered material")
  rec_rate <- rec_rate[-which(rec_rate$value == 0),]
  rec_rate$`Recovered material` <-as.character(rec_rate$`Recovered material`)
  return(rec_rate)
}
