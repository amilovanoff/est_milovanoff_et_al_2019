#Compiles a new adjusted file for the material content per component in vehicles. Takes into account assumptions for steel content (DUCKER)
library(reshape)
mat_comp <- read.csv("data/GREET_mat_wt_veh_comp_ORIGINAL.csv", stringsAsFactors = FALSE, check.names = FALSE)
comp_dt<-read.csv("data/component_equivalency.csv", stringsAsFactors = FALSE, check.names = FALSE)
vh_techno <- read.csv("data/vehicle_technology.csv", stringsAsFactors = FALSE, check.names = FALSE)
component<-unique(comp_dt$GREET)
comp<-"Body"
for (i in 1:400){
  for (j in 1:ncol(mat_comp)){
    if (is.na(mat_comp[i,j])){
      mat_comp[i,j]<-0
    }
  }
  if(mat_comp[i,"Material"] %in% component){
    comp<-mat_comp[i,"Material"]
    mat_comp[i,"Component"]<-0
  }else {
    mat_comp[i,"Component"]<-as.character(comp)
  }
  #Dissociates steel in "Mild steel" and "HSS/AHSS"
  if (mat_comp[i,"Material"]=="Steel" & mat_comp[i,"Component"]=="Body"){
    nw_rw<-mat_comp[i,]
    nw_rw[1,"Material"]<-"HSS/AHSS"
    #Assumpions based on Ducker studies (Metallic: In body and closures - 47% of the flat rolled steel is Mild steel and other. 53% is HSS/AHSS
    for (j in 4:(ncol(nw_rw)-1)){
      nw_rw[1,j]<-0.53*mat_comp[i,j]
      mat_comp[i,j]<-0.47*mat_comp[i,j]
    }
    mat_comp<-rbind(mat_comp[1:i,],nw_rw,mat_comp[(i+1):nrow(mat_comp),])
    i=i+1
  }
  }
mat_comp<-mat_comp[-which(mat_comp$Component==0 | mat_comp$Material==0),]
#Check
aggregate(x= mat_comp[,4:(ncol(mat_comp)-1)],
          by = mat_comp[,c("Size", "Scenario","Component")],
          FUN=sum)
#Renames Stainless Steel in Stainless Stl and Cast iron for matching purpose
mat_comp[which(mat_comp$Material=="Stainless Steel"),"Material"]<-"Stainless Stl"
mat_comp[which(mat_comp$Material=="Cast iron"),"Material"]<-"Cast Iron"

#Create a long table
mat_comp<-melt(mat_comp, id=c("Size", "Scenario","Material", "Component"))
colnames(mat_comp)[which(colnames(mat_comp)=="variable")]<-"Technology"
mat_comp$Technology<-as.character(mat_comp$Technology)

#write.csv(mat_comp,"data/GREET&DUCK_mat_wt_veh_comp.csv", row.names =FALSE)
