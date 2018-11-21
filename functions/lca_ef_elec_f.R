###>Function: Returns the annual life cycle GHG emission factors of electricity production for BEV and PHEV in the U.S. [kg CO2 eq. / kWh]
lca_ef_elec_f<-function(aeo_scen=NA,
                        lcia_method=NA,
                        lcia_cat=NA,
                        ef_elec_impro=NA,
                        last_yr=NA){
  source("architecture/attribute_f.R",local=TRUE)
  attribute_f("lca_ef_elec_f")
  #Input
  EF_us_elec_prod <- read.csv("inputs/EF_us_elec_prod.csv", stringsAsFactors = FALSE, check.names = FALSE)
  us_elec_mix <- read.csv("inputs/us_elec_mix.csv", stringsAsFactors = FALSE, check.names = FALSE)
  elec_techno_match <- read.csv("inputs/elec_techno_match.csv", stringsAsFactors = FALSE, check.names = FALSE)
  #Other parameters
  if (grepl("_",aeo_scen)){
    aeo_scen_tbc<-substring(aeo_scen,1,as.numeric(regexpr(pattern="_",aeo_scen))-1)
  } else {
    aeo_scen_tbc<-aeo_scen
  }
  #Create output file
  dt_col<-c("Year","Unit","value")
  lca_ef_elec<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  years_l<-2015:last_yr
  for (year in years_l){
    if (ef_elec_impro=="n" | year==2015){
      year_tbc<-2016
    } else if (ef_elec_impro=="y"){
      year_tbc<-year
    }
    ef_elec_dt <- subset(us_elec_mix,subset=Aeo_case==aeo_scen_tbc & Year==year_tbc & Source!="Total")
    for (elec_techno in unique(ef_elec_dt$Source)){
      ecoi_techno<-elec_techno_match[elec_techno_match$AEO==elec_techno,"ecoInvent"]
      if (length(ecoi_techno)!=0){
        ef_elec_dt[ef_elec_dt$Source==elec_techno,"EF"]<-mean(EF_us_elec_prod[EF_us_elec_prod$name %in% ecoi_techno,'lca score'])
      } else {
        ef_elec_dt[ef_elec_dt$Source==elec_techno,"EF"]<-0
      }
    }
    #Estimate the emission factor for the mix
    ef_elec<-sum(ef_elec_dt[,"rel_Value"]*ef_elec_dt[,"EF"])
    #Fill the final output
    lca_ef_elec[nrow(lca_ef_elec)+1,c("Year","value")]<-c(year,ef_elec)
  }
  lca_ef_elec[,"Unit"]<-"kg CO2 eq / kWh"
  return(lca_ef_elec)
}
  
