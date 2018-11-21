###>Function: Returns the component material composition of a vehicle after secondary weight changes from initial component material composition and component material composition after primary weight changes
alonso_sms_f <- function(mat_cont,
                         mat_cont_LW,
                         alonso_pwt_r=NA,
                         sms_coef_rand=NA,
                         alonso_sms_ratio=NA){
  #Assign the default values of arguments
  source("architecture/attribute_f.R",local = TRUE)
  #Assign default value
  attribute_f(fun_name="alonso_sms_f")
  #input files
  sms_coef <-read.csv("inputs/Alonso_sms_coef.csv",stringsAsFactors = FALSE,check.names = FALSE)
  comp_dt <-read.csv("inputs/component_equivalency.csv",stringsAsFactors = FALSE,check.names = FALSE)
  prim_sav <- mat_cont["Total", "Total"] - mat_cont_LW["Total", "Total"]
  #Output files
  mat_cont_LW_sms <- mat_cont_LW
  #subcomp_tbc is the list of the subcomponents to consider
  if (alonso_pwt_r=="n"){
  subcomp_tbc<-setdiff(sms_coef$Subcomponent[which(sms_coef$Subcomponent %in% colnames(mat_cont_LW_sms))],
                      comp_dt$`Own subcomponent`[comp_dt$`Own component`=="Powertrain"])
  } else if (alonso_pwt_r=="y"){
    subcomp_tbc<-sms_coef$Subcomponent[which(sms_coef$Subcomponent %in% colnames(mat_cont_LW_sms))]
  }
  #Random generation of sms_coef
  if (sms_coef_rand=="y"){
    sms_coef[sms_coef$Subcomponent %in% subcomp_tbc,"Own"]<-
      rnorm(n=length(which(sms_coef$Subcomponent %in% subcomp_tbc)),
            mean=sms_coef$`Mass influence coef - mean`[sms_coef$Subcomponent %in% subcomp_tbc],
            sd=sms_coef$`Mass influence coef - std error`[sms_coef$Subcomponent %in% subcomp_tbc])*
      alonso_sms_ratio
  } else if (sms_coef_rand=="n") {
    sms_coef[sms_coef$Subcomponent %in% subcomp_tbc,"Own"]<-
      sms_coef$`Mass influence coef - mean`[sms_coef$Subcomponent %in% subcomp_tbc]*
      alonso_sms_ratio
  }
  
  #mass_decomp is the mass decompounding coefficient associated with the considered subcomponents
  mass_decomp <-
    sum(sms_coef$Own[sms_coef$Subcomponent %in% subcomp_tbc]) /
    (1 - sum(sms_coef$Own[sms_coef$Subcomponent %in% subcomp_tbc]))
  
  for (subcomp in subcomp_tbc) {
    mass_dec_comp <-
      sum(sms_coef$Own[which(sms_coef$Subcomponent == subcomp)]) /
      (1 - sum(sms_coef$Own[sms_coef$Subcomponent %in% subcomp_tbc]))
    
    mat_cont_LW_sms[, subcomp] <- mat_cont_LW[, subcomp] *
      ifelse(mat_cont_LW["Total", subcomp] == 0,0,(1 - prim_sav * mass_dec_comp / mat_cont_LW["Total", subcomp]))
  }
  #Format output file
  mat_cont_LW_sms["Total", colnames(mat_cont_LW_sms)!="Total"] <-
    colSums(mat_cont_LW_sms[rownames(mat_cont_LW_sms)!="Total", colnames(mat_cont_LW_sms)!="Total"])
  mat_cont_LW_sms[,"Total"] <-
    rowSums(mat_cont_LW_sms[, colnames(mat_cont_LW_sms)!="Total"])
  #Check secondry savings
  #(mat_cont_LW["Total","Total"]-mat_cont_LW_sms["Total","Total"])/prim_sav-mass_decomp
  return(mat_cont_LW_sms)
}
