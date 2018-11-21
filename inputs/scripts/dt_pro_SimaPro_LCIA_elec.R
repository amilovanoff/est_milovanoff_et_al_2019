#This script creates an environmental matrix with the LCIA of the studied processes. It seperates electricity processes with other
library(readxl)
#Input files
lf<-list.files("data/primary_alu/electricity/original_data",pattern="ipcc", full.names=TRUE)
#Output file
env_mat_elec<-data.frame()
for (l in 1:length(lf)){
  mat1 <- read_excel(lf[l], col_names=FALSE)
  if (as.character(mat1[which(mat1[,1]=="Impact category"),3])=="Total"){
  name_product<-as.character(mat1[which(mat1[,1]=="Impact category"),4])}
  else {name_product<-as.character(mat1[which(mat1[,1]=="Impact category"),3])}
  env_mat_elec[1,l]<-name_product
  env_mat_elec[2,l]<-as.numeric(mat1[which(mat1[,1]=="IPCC GWP 100a"),3])
}
colnames(env_mat_elec)<-env_mat_elec[1,]
env_mat_elec<-tail(env_mat_elec,-1)
rownames(env_mat_elec)="kg CO2 eq"

#write.csv(env_mat_elec, "data/primary_alu/electricity/env_mat_elec_alu_ind.csv")

