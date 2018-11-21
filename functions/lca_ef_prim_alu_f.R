#This script create the data of primary aluminum used in the different regions using the MFA of apparent consumption of primary aluminum
lca_ef_prim_alu_f<-function(fast_mode="n"){
  #Argument's reader
  source("architecture/attribute_f.R",local=TRUE)
  attribute_f("lca_ef_prim_alu_f")
  #Source
  source("functions/lca_prod_prim_alu_f.R",local=TRUE)
  source("functions/mfa_prim_alu_f.R",local=TRUE)
  #Other parameters
  #ef_prod_alu contains the emission factors of primary aluminum production per region
  lca_prod_prim_alu_f_res<-do.call(lca_prod_prim_alu_f,list())
  ef_prod_alu<-lca_prod_prim_alu_f_res[["ef_prod_alu"]]
  #alu_cons_reg contains the apparent consumption of primary aluminum in the considered country per producing region
  mfa_prim_alu_f_res<-do.call(mfa_prim_alu_f,list(fast_mode=fast_mode))
  alu_cons_reg<-mfa_prim_alu_f_res[["alu_cons_reg"]]
  years_tbc<-unique(ef_prod_alu$Year)
  #Output files
  dt_col<-c("Year","Unit","value")
  lca_ef_prim_alu<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  #Loop for years
  for (y in years_tbc){
    #rows contains the rows to consider in ef_prod_alu
    rows<-which(ef_prod_alu$Year==y)
    lca_ef_prim_alu[nrow(lca_ef_prim_alu)+1,]<-c(as.numeric(y),"kg CO2 eq / kg prim alu cons",sum(ef_prod_alu$value[rows]*alu_cons_reg[ef_prod_alu$Region[rows],as.character(y)]))
  }
  #convert values in numeric
  lca_ef_prim_alu$value<-as.numeric(lca_ef_prim_alu$value)
  lca_ef_prim_alu$Year<-as.numeric(lca_ef_prim_alu$Year)
  return(list(lca_ef_prim_alu=lca_ef_prim_alu))
}
