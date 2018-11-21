# This script creates the input file for the LCA of electricity aluminum industry from IAI data from 2000 to 2015
#Input files
library(readxl)
list_reg_IAI<-read.csv("data/primary_alu/list_region_IAI.csv", check.names = FALSE,stringsAsFactors=FALSE)
IAI_data<-read.csv("data/primary_alu/electricity/original_data/primary-aluminium-smelting-power-consumption.csv", header = FALSE,stringsAsFactors=FALSE, dec=".")
prod_ds <- read_excel("data/primary_alu/MFA/original_data/aluminum_production_USGS.xlsx")
elec_techno<-read.csv("data/primary_alu/electricity/elec_techno.csv", stringsAsFactors=FALSE, check.names = FALSE)
#Output files
iai_elec_prod<-data.frame()
can<-data.frame()
for (y in 2000:2015){
  #Create temporary data.frame
  dt_col<-c("Year","Region",elec_techno$Technology)
  stat_elec_prod<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = length(unique(list_reg_IAI$Region))),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  stat_elec_prod$Year<-y 
  stat_elec_prod$Region<-unique(list_reg_IAI$Region)
  #Calculate the contribution of the Canada in the North American prod and the total energy consumed in NA.
  can_cont<-as.numeric(prod_ds[which(prod_ds$Country=="Canada"),as.character(y)]/sum(prod_ds[which(prod_ds$Country=="United States" | prod_ds$Country=="Canada"),as.character(y)]))
  NA_elec<-as.numeric(gsub(",","",IAI_data[which(IAI_data$V1==y)+9,3]))
  # "i"  represents the rows (from hydro to natural gas) for a specific year
  for (i in (which(IAI_data$V1==y)+4):(which(IAI_data$V1==y)+8)){
    # "j" represents the columns of IAI data (so the regions)
    for (j in 2:10){
      region<-list_reg_IAI$Region[which(list_reg_IAI$`IAI region`==(IAI_data[which(IAI_data$V1==y),j]))]
      # IF ND, we assume 0
      if (IAI_data[i,j]=="ND"){
        stat_elec_prod[which(stat_elec_prod$Region==region),grep(IAI_data[i,1],colnames(stat_elec_prod),ignore.case=TRUE)]<-as.numeric(0)
      } else {
        #If NA, we assume that CA-QC will only consumer hydro (from canada contribution and total electricity consumed in NA) and the rest of the electrictiy will be for US.
      if (IAI_data[which(IAI_data$V1==y),j]=="North America"){
        if (IAI_data[i,1]=="Hydro"){
          stat_elec_prod[which(stat_elec_prod$Region==region[1]),grep(IAI_data[i,1],colnames(stat_elec_prod),ignore.case=TRUE)]<-can_cont*NA_elec
          stat_elec_prod[which(stat_elec_prod$Region==region[2]),grep(IAI_data[i,1],colnames(stat_elec_prod),ignore.case=TRUE)]<-as.numeric(gsub(",","",IAI_data[i,j]))-can_cont*NA_elec
        } else {
          stat_elec_prod[which(stat_elec_prod$Region==region[1]),grep(IAI_data[i,1],colnames(stat_elec_prod),ignore.case=TRUE)]<-as.numeric(0)
          stat_elec_prod[which(stat_elec_prod$Region==region[2]),grep(IAI_data[i,1],colnames(stat_elec_prod),ignore.case=TRUE)]<-as.numeric(gsub(",","",IAI_data[i,j]))
        }
      } else {
        stat_elec_prod[which(stat_elec_prod$Region==region),grep(IAI_data[i,1],colnames(stat_elec_prod),ignore.case=TRUE)]<-as.numeric(gsub(",","",IAI_data[i,j]))
      }
      }
    }
  }
 
 #  #This processes the data in order to get the relative contribution of the technologies. 
 # for (i in 1:10){
 #    sum<-sum(as.numeric(stat_elec_prod[i,4:8]))
 #   for (j in 4:8){
 #      if (sum!=0){
 #      stat_elec_prod[i,j]<-(-as.numeric(stat_elec_prod[i,j]))/sum
 #      }
 #    }
 #  }
  #Compiles the results in the final file
  iai_elec_prod<-rbind(iai_elec_prod, stat_elec_prod)
  can[nrow(can)+1,1]<-can_cont
  rownames(can)[nrow(can)]<-y
}


#Consider the electricity mixes from the generation
iai_elec_mix<-iai_elec_prod
#rows contains the rows with the positiv production
rows<-which(rowSums(iai_elec_prod[,elec_techno$Technology])!=0)
#Estimate the relative production
iai_elec_mix[rows,elec_techno$Technology]<-iai_elec_prod[rows,elec_techno$Technology]/rowSums(iai_elec_prod[rows,elec_techno$Technology])
#Write files
# write.csv(iai_elec_prod, "data/primary_alu/electricity/elec_prod_IAI_2000-2015.csv", row.names = FALSE)
# write.csv(iai_elec_mix, "data/primary_alu/electricity/elec_mix_IAI_2000-2015.csv", row.names = FALSE)
