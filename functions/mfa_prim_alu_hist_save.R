###>Function: Creates the list of producing regions per country per year.
#THIS FUNCTION IS NOT MEANT TO BE RAN. It creates a list of dataframe to be stored in interm_results
mfa_prim_alu_hist_f<-function(mfa_alu_cont=NA,
                              kastner_mdl=NA){
  source("architecture/attribute_f.R",local=TRUE)
  attribute_f("mfa_prim_alu_hist_f")
  library(readxl)
  #Input files
  files_td<-list.files("inputs/primary_alu/MFA/original_data", pattern="resourcetradeearth", full.names = TRUE)
  prod_ds <- read_excel("inputs/primary_alu/MFA/original_data/aluminum_production_USGS.xlsx")
  list_com<-read.csv("inputs/list_commodities.csv", stringsAsFactors = FALSE)
  list_country<-read.csv("inputs/primary_alu/list_country.csv", stringsAsFactors = FALSE)
  #Other parameters
  years_tbc<-2000:2016
  prod_ds[is.na(prod_ds)]<-0
  #Create output files
  mfa_prim_alu_hist<-list()
  for (y in years_tbc){
    #Download the data for the specific year
    trades_ds <- read_excel(files_td[grep(pattern=y,files_td)],sheet = "Trades")
    mat_trad<-matrix(0,ncol=nrow(list_country), nrow=nrow(list_country))
    rownames(mat_trad)=list_country$Country
    colnames(mat_trad)=list_country$Country
    mat_prod<-mat_trad
    #Create the list of commodities to consider in the calculations
    commodities<-list_com[which(list_com$Class=="Unwrought"),2]
    #Create the dataset from the the databased associated with the specific year and the considered commodities
    dataset<-trades_ds[which(trades_ds$Year==y & (trades_ds$Resource %in% commodities)),]
    #Force every NA to be 0 in the weight
    dataset$`Weight (1000kg)`[which(is.na(dataset$`Weight (1000kg)`))]<-0
    #Create the matrix of bilateral trade data (Z in the math model). The rows are the importer. The columns are the exporter. Cheked.
    for (i in 1:nrow(dataset)){
      #No factor of 0.7 because unwrought aluminum.
      mat_trad[which(rownames(mat_trad)==dataset$Importer[i]),which(colnames(mat_trad)==dataset$Exporter[i])]<-mat_trad[which(rownames(mat_trad)==dataset$Importer[i]),which(colnames(mat_trad)==dataset$Exporter[i])] + dataset$`Weight (1000kg)`[i]
    }
    if (mfa_alu_cont=="high"){
    #Create the list of commodities to consider in the calculations
    commodities<-list_com[which(list_com$Class=="Crude and semicrude"),2]
    #Create the dataset from the the databased associated with the specific year and the considered commodities
    dataset<-trades_ds[which(trades_ds$Year==y & (trades_ds$Resource %in% commodities)),]
    #Force every NA to be 0 in the weight
    dataset$`Weight (1000kg)`[which(is.na(dataset$`Weight (1000kg)`))]<-0
    #Create the matrix of bilateral trade data (Z in the math model). The rows are the importer. The columns are the exporter. Cheked.
    for (i in 1:nrow(dataset)){
      #Include a factor of 0.7. Assumption is 70% of the semi-crude are primary aluminum
      mat_trad[which(rownames(mat_trad)==dataset$Importer[i]),which(colnames(mat_trad)==dataset$Exporter[i])]<-mat_trad[which(rownames(mat_trad)==dataset$Importer[i]),which(colnames(mat_trad)==dataset$Exporter[i])] + 0.7*dataset$`Weight (1000kg)`[i]
    }
    }
    #Remove the re-imports and re-exports by forcing the diagonal to be 0
    diag(mat_trad)<-0
    #Check the total global production
    #sum(diag(mat_trad))==sum(prod_ds[, which(colnames(prod_ds)==y)])
    #Create production matrix (p_hat in the math model). Checked  
    for (j in 1:nrow(prod_ds)){
      mat_prod[which(rownames(mat_prod)==prod_ds$Country[j]),which(colnames(mat_prod)==prod_ds$Country[j])]<-as.numeric(prod_ds[j, which(colnames(prod_ds)==y)])
    }
    if (kastner_mdl=="y"){
      #Create summation matrix (i in the math model)
      ones<-matrix(1,nrow=nrow(list_country), ncol=1)
      #Create the vector of domestic production plus imports (DMI) (x in the math model). Checked.
      vec_dmi<- mat_prod %*% ones + mat_trad %*% ones
      #Consistency check: DMI should always by higher than the exports in a given region. If not, create an inventory change vector to add to the DMI vector.
      vec_inv<-matrix(0, nrow=nrow(list_country), ncol=1)
      rownames(vec_inv)=rownames(vec_dmi)
      vec_inv[which(vec_dmi<colSums(mat_trad))]<-colSums(mat_trad)[which(vec_dmi<colSums(mat_trad))] - vec_dmi[which(vec_dmi<colSums(mat_trad))]
      #sum(vec_inv)/(sum(mat_trad)+sum(mat_prod))
      vec_dmi<-vec_dmi+vec_inv
      #length(which(vec_dmi<colSums(mat_trad)))
      #Create the reciprocal of vector of DMI (x_hat-1 in the math model)
      inv_mat_dmi<-matrix(0,nrow=nrow(vec_dmi), ncol=nrow(vec_dmi))
      rownames(inv_mat_dmi)=rownames(vec_dmi)
      colnames(inv_mat_dmi)=rownames(vec_dmi)
      #Remove the rows and columns with 0 DMI and inventory change. Invert the resulting matrix.
      vec_dmi2<-as.matrix(vec_dmi[which(vec_dmi!=0)])
      rownames(vec_dmi2)<-rownames(vec_dmi)[which(vec_dmi!=0)]
      mat_dmi2<-matrix(0,nrow=nrow(vec_dmi2), ncol=nrow(vec_dmi2))
      rownames(mat_dmi2)=rownames(vec_dmi2)
      colnames(mat_dmi2)=rownames(vec_dmi2)
      diag(mat_dmi2)<-vec_dmi2
      inv_mat_dmi2<-solve(mat_dmi2)
      #Fill the diagonal of x_hat-1 with the matrix previously inverted
      for (l in 1:nrow(inv_mat_dmi)){
        if (length(which(rownames(inv_mat_dmi2)==rownames(inv_mat_dmi)[l])==1)) {
          diag(inv_mat_dmi)[l]<-diag(inv_mat_dmi2)[which(rownames(inv_mat_dmi2)==rownames(inv_mat_dmi)[l])]
        }
      }
      #sum(diag(inv_mat_dmi2))==sum(inv_mat_dmi)
      #Create the export share matrix (A in the math model).  
      mat_ex <-mat_trad %*% inv_mat_dmi
      #Check the consistency (the sums of the columns should be lower than 1): which(colSums(mat_ex)>1)
      #Create the matrix of DMI according to country of origin  
      R<- solve(diag(nrow(mat_ex)) - mat_ex) %*% mat_prod
      #Create the vector of apparent consumption country i
      vec_cons<-(vec_dmi-colSums(mat_trad))/vec_dmi
      vec_cons[is.nan(vec_cons)]<-0
      mat_cons<-matrix(0,nrow=nrow(vec_cons), ncol=nrow(vec_cons))
      rownames(mat_cons)=rownames(vec_cons)
      colnames(mat_cons)=rownames(vec_cons)
      diag(mat_cons)<-vec_cons
      #Create the matrix of apparent national level consumptionm according to country of origin (R_hat in the math model). rij is the part of the apparent consumption of country i originating from country j.
      R_hat<-mat_cons %*% R
      #Create the relative matrix of apparent national level consumption according to country of origin.
      #rel_R_hat<-R_hat / rowSums(R_hat)
      #rel_R_hat[is.nan(rel_R_hat)]<-0
      #which(rowSums(rel_R_hat)>1.000001)
    } else {
      R_hat = mat_trad + mat_prod
    }
    #Aggregate the exporters by producing regions
    temp_reg<-matrix(0,nrow=ncol(R_hat),ncol=length(unique(list_country$Region)))
    colnames(temp_reg)<-unique(list_country$Region)
    rownames(temp_reg)<-colnames(R_hat)
    #Loop on consuming countries
    for (cons_country in rownames(temp_reg)){
      #Loop on producing regions
      for (prod_region in colnames(temp_reg)){
        temp_reg[cons_country,prod_region]<-sum(R_hat[cons_country,colnames(R_hat) %in% list_country$Country[list_country$Region==prod_region]])
      }
    }
    #Calculate the relative contribution of each producing region
    rel_temp_reg<-temp_reg
    for (cons_country in rownames(temp_reg)){
      if (sum(temp_reg[cons_country,])!=0){
        rel_temp_reg[cons_country,]<-temp_reg[cons_country,]/sum(temp_reg[cons_country,])
      }
    }
    mfa_prim_alu_hist<-append(mfa_prim_alu_hist,setNames(list(rel_temp_reg),y))
  }
  save(list="mfa_prim_alu_hist",file="outputs/out_def/mfa_prim_alu_hist_f_def.RData")
  return()
}
mfa_prim_alu_hist_f()
