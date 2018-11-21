#This script creates the data of primary aluminum based on the processes defined, the inputs and the LCIA of the processes (IPCC) dynamically.
lca_prod_prim_alu_f<-function(last_yr=NA){
  #Source
  source("architecture/attribute_f.R",local=TRUE)
  attribute_f("lca_prod_prim_alu_f")
  library(readxl)
  #Source
  source("functions/lca_elec_alu_ind_f.R",local=TRUE)
  source("functions/lca_prim_alu_inputs_f.R",local=TRUE)
  #Inputs
  env_mat<-read.csv("inputs/primary_alu/env_mat_prim_alu.csv",stringsAsFactors = FALSE,check.names = FALSE)
  list_reg<-read.csv("inputs/primary_alu/list_region.csv", stringsAsFactors = FALSE)[,"Region"]
  list_process<-read.csv("inputs/primary_alu/prim_alu_lc_process.csv",stringsAsFactors = FALSE,check.names = FALSE)[,"Process"]
  #Other parameters
  input_name<-"ecoi"
  lca_elec_alu_ind_f_res<-do.call(lca_elec_alu_ind_f,list())
  ef_elec_alu_ind<-lca_elec_alu_ind_f_res[["ef_elec_alu_ind"]]
  years_tbc<-unique(ef_elec_alu_ind$Year)
  lca_prim_alu_inputs_res<-do.call(lca_prim_alu_inputs_f,list())
  list_inputs<-names(lca_prim_alu_inputs_res)
  #Output files
  dt_col<-c("Year","Region","value")
  ef_prod_alu<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  for (y in years_tbc){
    #Creation of technology matrix
    A<-matrix(0,ncol=length(list_process), nrow=length(list_process))
    rownames(A)<-list_process
    colnames(A)<-list_process
    diag(A)<-1
    for (region in list_reg){
      #Filing technology matrix for the specific country - For area "RoW", no emission factor
      if (region=="RoW" | as.numeric(subset(ef_elec_alu_ind,subset = Year == y & Region == region,select=value))==0){
        ef_prod_alu[nrow(ef_prod_alu)+1,]<-c(y,region,0)
        }else {
          #Fill the technology matrix with input files
          for (k in 1:length(list_inputs)){
            #Extracts the inputs for the current year from the general data
            input<-subset(lca_prim_alu_inputs_res[[k]],subset = Year == as.character(y))
            process_name<-as.character(colnames(input)[2])
            for (l in 2:ncol(input)){
              A[which(rownames(A)==colnames(input)[l]),which(colnames(A)==process_name)]<-as.numeric(as.character(input[which(input$Region==region),l]))
              }
            }
          #Solving the data
          inv_A<-solve(A)
          X<-matrix(0,nrow=ncol(inv_A))
          rownames(X)<-colnames(inv_A)
          X[rownames(X)=="Aluminium, primary, ingot"]<-1
          Y<-inv_A%*%X
          #Creation of environmental matrix (except electricity process that)
          B<-matrix(0,nrow=1, ncol=nrow(A))
          colnames(B)<-rownames(A)
          for (i in 1:(ncol(B)-1)){
            if (length(grep(colnames(B)[i],colnames(env_mat)))==1){
              B[1,i]=as.numeric(env_mat[1,as.numeric(grep(colnames(B)[i],colnames(env_mat)))])
            } else { 
              proc_mat<-env_mat[1,as.numeric(grep(colnames(B)[i],colnames(env_mat)))]
              if (length(grep(region,colnames(proc_mat)))==1){
                B[1,i]=as.numeric(proc_mat[1,grep(region,colnames(proc_mat))])
              } else {
                B[1,i]=as.numeric(proc_mat[1,grep("GLO",colnames(proc_mat))])
              }
            }
  }
  #Filling the environmental matrix (electricity process) with specific country
  B[1,colnames(B)=="Electricity, medium voltage"]<-as.numeric(subset(ef_elec_alu_ind,subset = Year == y & Region == region,select=value))
  #Calculate emission factor
  ef_prod_alu[nrow(ef_prod_alu)+1,]<-c(y,region,as.numeric(B%*%Y))
        }
      }
  }
  ef_prod_alu[,"Unit"]<-"kg CO2eq / kg prim alu"
  ef_prod_alu$value<-as.numeric(ef_prod_alu$value)
  ef_prod_alu$Year<-as.numeric(ef_prod_alu$Year)
  return(list(ef_prod_alu=ef_prod_alu))
  }

