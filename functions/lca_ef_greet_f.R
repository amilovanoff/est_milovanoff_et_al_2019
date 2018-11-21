###>Function: Returns the static emission factors taken from the GREET model
lca_ef_greet_f<-function (aeo_scen = NA,
                         lcia_method = NA,
                         lcia_cat = NA,
                         fast_mode="n"){
  source("architecture/attribute_f.R",local=TRUE)
  attribute_f("lca_ef_greet_f")
  #Input files
  LCA_process <- read.csv("inputs/LCA_process.csv", stringsAsFactors = FALSE, check.names = FALSE)
  conv<-read.csv("inputs/conversion_units.csv", stringsAsFactors = FALSE, check.names = FALSE,row.names = 1)
  fuel_specs<-read.csv("inputs/fuel_specs.csv", stringsAsFactors = FALSE, check.names = FALSE,row.names = 1)
  EF_assembly_GREET <- read.csv("inputs/EF_assembly_GREET.csv", stringsAsFactors = FALSE, check.names = FALSE)
  EF_fuel_GREET <- read.csv("inputs/EF_fuel_GREET.csv", stringsAsFactors = FALSE, check.names = FALSE)
  EF_mat_trans_GREET <- read.csv("inputs/EF_mat_trans_GREET.csv", stringsAsFactors = FALSE, check.names = FALSE)
  EF_mat_prod_GREET <- read.csv("inputs/EF_mat_prod_GREET.csv", stringsAsFactors = FALSE, check.names = FALSE)
  #Other parameters
  greet_cat<-"GHGs"
  #Output file
  LCA_process<-LCA_process[LCA_process$Source=="GREET",]
  ef_greet<-LCA_process[LCA_process$Source=="GREET",c("Unit","Phase","Process","Source")]
  ef_greet[,lcia_cat]<-0
  #Fill the emission factors from GREET
  for (cat in greet_cat){
    #Fill env mat for Primary Material production
    for (mat in which(ef_greet$Phase=="Primary Material Production"&ef_greet$Source=="GREET")){
    ef_greet[mat,lcia_cat]<-EF_mat_prod_GREET[which(EF_mat_prod_GREET$`LCI`==cat),LCA_process$GREET[mat]]/10^3*conv["lb","1 kg"]
    }
    #Fill env mat for Secondary Material production
    for (mat in which(ef_greet$Phase=="Secondary Material Production"&ef_greet$Source=="GREET")){
      ef_greet[mat,lcia_cat]<-EF_mat_prod_GREET[which(EF_mat_prod_GREET$`LCI`==cat),LCA_process$GREET[mat]]/10^3*conv["lb","1 kg"]
    }
    #Fill env mat for Vehicle assembly
    ef_greet[which(ef_greet$Process=="Vehicle Assembly"),lcia_cat]<-
      sum(as.numeric(EF_assembly_GREET[which(EF_assembly_GREET$LCI==cat),
                            strsplit(LCA_process$GREET[which(LCA_process$Process=="Vehicle Assembly")],",")[[1]]]))/10^3
    #Fill env for manufacturing
    for (mat in which(ef_greet$Phase=="Manufacturing"&!grepl("Assembly",ef_greet$Process))){
      ef_greet[mat,lcia_cat]<-
        mean(as.numeric(EF_mat_trans_GREET[which(EF_mat_trans_GREET$LCI==cat),
                                           strsplit(LCA_process$GREET[mat],",")[[1]]]))/
        10^3*conv["lb","1 kg"]
    }
  #Fill env for Fuel production
  for (fuel in which(ef_greet$Phase=="Fuel Production"&ef_greet$Source=="GREET")){
      if (ef_greet$Process[fuel]!="Electricity"){
      ef_greet[fuel,lcia_cat]<-EF_fuel_GREET[which(EF_fuel_GREET$Data=="WTP"&EF_fuel_GREET$LCI==cat),LCA_process$GREET[fuel]]/
        10^3*conv["mmBTU","1 BTU"]*as.numeric(fuel_specs[ef_greet$Process[fuel],"LHV"])*conv["gal","1 L"]
    } else if (ef_greet$Process[fuel]=="Electricity") {
      ef_greet[fuel,lcia_cat]<-EF_fuel_GREET[which(EF_fuel_GREET$Data=="WTP"&EF_fuel_GREET$LCI==cat),LCA_process$GREET[fuel]]/
        10^3*conv["mmBTU","1 kWh"]
    }
  }
  #Fill env for Fuel use
  for (fuel in which(ef_greet$Phase=="Fuel Use"&ef_greet$Source=="GREET")){
      if (!is.na(EF_fuel_GREET[which(EF_fuel_GREET$Data=="PTW"&EF_fuel_GREET$LCI==cat),LCA_process$GREET[fuel]])){
        ef_greet[fuel,lcia_cat]<-EF_fuel_GREET[which(EF_fuel_GREET$Data=="PTW"&EF_fuel_GREET$LCI==cat),LCA_process$GREET[fuel]]/
          10^3*EF_fuel_GREET[which(EF_fuel_GREET$Data=="Fuel Economy"),LCA_process$GREET[fuel]]*conv["gal","1 L"]
      }else {
        ef_greet[fuel,lcia_cat]<-0
    }
  }
  #Fill env for End of life
  ef_greet[which(ef_greet$Process=="Vehicle Disposal"),lcia_cat]<-
      sum(as.numeric(EF_assembly_GREET[which(EF_assembly_GREET$LCI==cat),
                            strsplit(LCA_process$GREET[which(LCA_process$Process=="Vehicle Disposal")],",")[[1]]]))/10^3
  }
  return(ef_greet)
}
