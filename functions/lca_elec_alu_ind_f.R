###>Function: Returns the annual GHG emission factors of electricity production for aluminum industries per region
lca_elec_alu_ind_f<-function(last_yr=NA){
  #Source
  source("architecture/attribute_f.R",local=TRUE)
  attribute_f("lca_elec_alu_ind_f")
  library(readxl)
  #Input files
  env_mat_elec<-read.csv("inputs/primary_alu/electricity/env_mat_elec_alu_ind.csv",stringsAsFactors = FALSE,check.names = FALSE)
  list_reg<-read.csv("inputs/primary_alu/list_region.csv",stringsAsFactors = FALSE,check.names = FALSE)
  list_process<-read.csv("inputs/primary_alu/electricity/elec_alu_ind_lc_process.csv",stringsAsFactors = FALSE,check.names = FALSE)
  iai_elec_mix<-read.csv("inputs/primary_alu/electricity/elec_mix_IAI_2000-2015.csv",stringsAsFactors = FALSE,check.names = FALSE)
  elec_losses<-read.csv("inputs/primary_alu/electricity/elec_losses.csv", stringsAsFactors=FALSE, check.names = FALSE)
  #Ohter parameters
  years_tbc<-2000:last_yr
  #Output files
  dt_col<-c("Year","Region","value")
  ef_elec_alu_ind<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  #Creation of technology matrix
  A<-matrix(0,ncol=nrow(list_process), nrow=nrow(list_process))
  rownames(A)<-list_process$Process
  colnames(A)<-list_process$Process
  diag(A)<-1
  #Loop for years
  for (y in years_tbc){
    #Loop for regions
    for (region in unique(list_reg$Region)){
      #Filing technology matrix for the specific country - For area "RoW", no emission factor
      if (y %in% iai_elec_mix$Year){
          #Fill losses in the technology matrix
          loss_process<-list_process$Process[list_process$Data=="loss"]
          A[loss_process,loss_process]<-elec_losses[elec_losses$Region==region,loss_process]
          #Fill electricity mixes
          for (elec_techno in list_process$Process[list_process$Data=="electricity production"]){
            A[rownames(A)==elec_techno,1]<-(-as.numeric(subset(iai_elec_mix,subset = Year == y & Region == region, select = elec_techno)))
          }
          #Check sum of mixes is 1. Otherwise no calculations
          if (round(sum(A[which(rownames(A)!=loss_process),loss_process])-(-1),digits=2)==0){
            #Solving the data
            inv_A<-solve(A)
            X<-matrix(0,nrow=ncol(inv_A))
            rownames(X)<-colnames(inv_A)
            X[rownames(X)=="Electricity, medium voltage, aluminium industry"]<-1
            Y<-inv_A%*%X
            #Creation of environmental matrix (except electricity process that)
            B<-matrix(0,nrow=1, ncol=nrow(A))
            colnames(B)<-rownames(A)
            for (i in 1:(ncol(B))){
              if (length(grep(colnames(B)[i],colnames(env_mat_elec)))==1){
                B[1,i]=as.numeric(env_mat_elec[1,as.numeric(grep(colnames(B)[i],colnames(env_mat_elec)))])
              } else { 
                proc_mat<-env_mat_elec[1,as.numeric(grep(colnames(B)[i],colnames(env_mat_elec)))]
                if (length(grep(region,colnames(proc_mat)))==1){
                  B[1,i]=as.numeric(proc_mat[1,grep(region,colnames(proc_mat))])
                } else {
                  B[1,i]=as.numeric(proc_mat[1,grep("UN-EUROPE",colnames(proc_mat))])
                }
              }
            }
            #Calculate the emission factor
            ef_elec_ind<-as.numeric(B%*%Y)
            #Fill output
            ef_elec_alu_ind[nrow(ef_elec_alu_ind)+1,]<-c(y,region,ef_elec_ind)
          } else {
            ef_elec_alu_ind[nrow(ef_elec_alu_ind)+1,]<-c(y,region,0)
          }
      } else {
        ef_elec_alu_ind[nrow(ef_elec_alu_ind)+1,]<-c(y,region,as.numeric(subset(ef_elec_alu_ind,subset = Year == 2015 & Region == region,select=value)))
        }
      }
    }
  ef_elec_alu_ind[,"Unit"]<-"kg CO2 eq / kWh"
  return(list(ef_elec_alu_ind=ef_elec_alu_ind))
}
