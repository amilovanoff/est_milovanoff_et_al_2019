###>Function: Calculate the annual inputs for primary aluminum GHG emission factor calcualtion with historical data of energy efficiency from IAI
lca_prim_alu_inputs_f<-function(last_yr=NA,
                                alu_eff_impro=NA){
  #Source
  source("architecture/attribute_f.R",local=TRUE)
  attribute_f("lca_prim_alu_inputs_f")
  library(readxl)
  #Input files
  source<-"aa"
  list_inputs<-list.files("inputs/primary_alu/inputs", pattern=paste0("inputs_",source), full.names = TRUE)
  input_names<-list.files("inputs/primary_alu/inputs", pattern=paste0("inputs_",source), full.names = FALSE)
  IAI_data<-read.csv("inputs/primary_alu/electricity/original_data/primary-aluminium-smelting-energy-intensity.csv", header = TRUE,stringsAsFactors=FALSE, dec=".",check.names = FALSE)
  list_reg_IAI<-read.csv("inputs/primary_alu/list_region_IAI.csv", check.names = FALSE,stringsAsFactors=FALSE)
  #Loop for input processes
  for (l in 1:length(list_inputs)){
    inputs_alu<-data.frame()
    name<-input_names[l]
    stat_input<-read.csv(list_inputs[l], header = TRUE,na.strings = "",stringsAsFactors=FALSE, check.names = FALSE)
    for (y in 2000:last_yr){
    input_alu<-stat_input
    input_alu[,"Year"]<-y
    #Update the energy efficiency of aluminum liquid production
    if (grepl("alu_liquid", name)){
      #Loop for IAI regions
      for (j in 1:nrow(input_alu)){
        region_IAI<-list_reg_IAI$`IAI region`[which(list_reg_IAI$Region==input_alu$Region[j])]
        #If issue in region, consider world data.
        if (region_IAI == "Estimated Unreported" | "ND" %in% IAI_data[which(IAI_data$Year==y & IAI_data$Category=="AC"),region_IAI]){
          region_IAI="World"
        }
        #Estimate efficiency for projected values
        if (y <= 2015){
          input_alu[j, "Electricity, medium voltage"]<-(-as.numeric(IAI_data[which(IAI_data$Year==y & IAI_data$Category=="AC"),which(colnames(IAI_data)==region_IAI)])/1000)
        } else {
          #rowi is the first historic energy consumption value available for region_IAI. rowf is the last.
          rowi<-min(which(IAI_data[,region_IAI]!="ND" & IAI_data$Category=="AC"))
          rowf<-max(which(IAI_data[,region_IAI]!="ND" & IAI_data$Category=="AC"))
          #Estimate the linear annual rate from historical data.
          if (alu_eff_impro=="y"){
            annual_rate<-(as.numeric(IAI_data[rowi,region_IAI])-as.numeric(IAI_data[rowf,region_IAI]))/
              as.numeric(IAI_data[rowi,region_IAI])/
              (IAI_data$Year[rowi]-IAI_data$Year[rowf])
            if (annual_rate<(-0.005)){annual_rate=(-0.005)}
          } else {
            annual_rate=0
          }
          #Update the energy efficiency from annual rate. Assumption: linear improvements
          input_alu[j, "Electricity, medium voltage"]<-(-as.numeric(IAI_data[rowf,region_IAI])*(1+annual_rate)^(y-IAI_data$Year[rowf])/1000)
        }
        }
      }
    inputs_alu<-rbind(inputs_alu, input_alu)
    }
    assign(gsub(".csv","",name),inputs_alu)
  }
  results<-list()
  for (name in input_names){
    results<-append(results,list(get(gsub(".csv","",name))))
  }
  results<-setNames(results,gsub(".csv","",input_names))
  return(results)
  }

