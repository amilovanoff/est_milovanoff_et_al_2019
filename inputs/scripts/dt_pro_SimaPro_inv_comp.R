#This script creates a file with the the inventory and emissions associated with the processes.
library(readxl)
#"Aluminium, primary, ingot"
#"aluminium production, primary, liquid, Soderberg"
#"aluminium production, primary, liquid, prebake"
#"Anode, paste"
#"Anode, prebake"
#"Aluminium oxide"
#"Aluminium hydroxide"
#"Bauxite"
pattern="high voltage"
list_files<-list.files("C:/Users/Alexandre/Dropbox/UofT/Ford/SimaPro/Lib ecoInvent 3.3, allo def, electricity", pattern=pattern, full.names=TRUE)
matinput<-data.frame()
matinput[1,1]<-"Materials/fuels"
eninput<-data.frame()
eninput[1,1]<-"Electricity/heat"
emi_air<-data.frame()
emi_air[1,1]<-"Emissions to air"
emi_wt<-data.frame()
emi_wt[1,1]<-"Emissions to water"
for (l in 1:length(list_files)){
mat1 <- read_excel(list_files[l], col_names=FALSE, skip = 9)
name_process<-as.character(mat1[which(mat1[,1]=="Process name"),2])
matinput[1,ncol(matinput)+1]<-name_process
eninput[1,ncol(eninput)+1]<-name_process
emi_wt[1,ncol(emi_wt)+1]<-name_process
emi_air[1,ncol(emi_air)+1]<-name_process

for (i in (which(mat1[,1]=="Materials/fuels")+1):(which(mat1[,1]=="Electricity/heat")-1)){
  if (!is.na(mat1$X__1[i])) {
    if (length(which(matinput[,1]==as.character(mat1[i,1])))==0) {
      matinput[nrow(matinput)+1,1]<-mat1[i,1]
      matinput[nrow(matinput),ncol(matinput)]<-mat1[i,2]
      }
    else {matinput[which(matinput[,1]==as.character(mat1[i,1])),ncol(matinput)]<-mat1[i,2]}
  }}

for (j in (which(mat1[,1]=="Electricity/heat")+1):(which(mat1[,1]=="Emissions to air")-1)){
  if (!is.na(mat1$X__1[j])) {
    if (length(which(eninput[,1]==as.character(mat1[j,1])))==0) {
      eninput[nrow(eninput)+1,1]<-mat1[j,1]
      eninput[nrow(eninput),ncol(eninput)]<-mat1[j,2]
    }
    else {eninput[which(eninput[,1]==as.character(mat1[j,1])),ncol(eninput)]<-mat1[j,2]}
  }}

for (k in (which(mat1[,1]=="Emissions to air")+1):(which(mat1[,1]=="Emissions to water")-1)){
  if (!is.na(mat1$X__1[k])) {
    if (length(which(emi_air[,1]==as.character(mat1[k,1])))==0) {
      emi_air[nrow(emi_air)+1,1]<-mat1[k,1]
      emi_air[nrow(emi_air),ncol(emi_air)]<-mat1[k,3]
    }
    else {emi_air[which(emi_air[,1]==as.character(mat1[k,1])),ncol(emi_air)]<-mat1[k,3]}
  }}

for (m in (which(mat1[,1]=="Emissions to water")+1):(which(mat1[,1]=="Emissions to soil")-1)){
  if (!is.na(mat1$X__1[m])) {
    if (length(which(emi_wt[,1]==as.character(mat1[m,1])))==0) {
      emi_wt[nrow(emi_wt)+1,1]<-mat1[m,1]
      emi_wt[nrow(emi_wt),ncol(emi_wt)]<-mat1[m,3]
    }
    else {emi_wt[which(emi_wt[,1]==as.character(mat1[m,1])),ncol(emi_wt)]<-mat1[m,3]}
  }}

}
#setwd("C:/Users/Alexandre/Dropbox/UofT/Ford/SimaPro/Comparison_proces")
#write.csv(emi_air, file=paste0(pattern, "_emi_air.csv"))
#write.csv(emi_wt, file=paste0(pattern, "_emi_wt.csv"))
#write.csv(matinput, file=paste0(pattern, "_mat_input.csv"))
#write.csv(eninput, file=paste0(pattern, "_en_input.csv"))
#write.csv(rbind(matinput,eninput,emi_air,emi_wt), file=paste0(pattern, ".csv"))

