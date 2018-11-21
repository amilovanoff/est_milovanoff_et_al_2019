#This script creates an environmental matrix with the LCIA of the studied processes. It seperates electricity processes with other
library(readxl)
lf<-list.files("data/primary_alu/original_data",pattern="ipcc", full.names=TRUE)
lf_woelec<-lf[!grepl("Electricity", lf)]
env_mat<-data.frame()
for (l in 1:length(lf_woelec)){
  mat1 <- read_excel(lf_woelec[l], col_names=FALSE)
  if (as.character(mat1[which(mat1[,1]=="Impact category"),3])=="Total"){
  name_product<-as.character(mat1[which(mat1[,1]=="Impact category"),4])}
  else {name_product<-as.character(mat1[which(mat1[,1]=="Impact category"),3])}
  env_mat[1,l]<-name_product
  env_mat[2,l]<-as.numeric(mat1[which(mat1[,1]=="IPCC GWP 100a"),3])
}
colnames(env_mat)<-env_mat[1,]
env_mat<-tail(env_mat,-1)
rownames(env_mat)="IPCC kg CO2 eq"
#write.csv(env_mat, "data/primary_alu/env_mat_prim_alu.csv")

