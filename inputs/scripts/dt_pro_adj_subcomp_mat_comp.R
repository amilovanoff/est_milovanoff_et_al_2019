#Adjuste the material composition of the subcomponent based on global assumptions
#Called function
source("functions/Fleet_mat_cont_f.R")
#Input files
mat_comp_15 <-
  read.csv(
    "data/glo_mat_comp_15_DUCKER.csv",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
mat_comp <-
  read.csv(
    "data/component_material_comp_ACSM.csv",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
wgt_dt<-
  read.csv(
    "data/Fleet_wgt_75to15.csv",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
#Other parameters
techno="ICEV-G"
techno_adj="Glo"
size="Car"
#Creation Output file
mat_cont <- mat_cont_f(techno, size, adjusted="n", relative = "y")
mat_cont_adj <- mat_cont

alu_content <-
  sum(mat_comp_15[which(mat_comp_15$Material%in%c("Wrought Aluminum","Cast Aluminum")), "2015"])
#Assumption: 12% of aluminum is in Body (Ducker) - It reduces Mild steel
mat_cont_adj["Wrought Aluminum", "Body"] <-
  mat_comp_15[which(mat_comp_15$Material == "Wrought Aluminum"), "2015"] - mat_cont["Wrought Aluminum", "Total"]
if (0.12 * alu_content - mat_cont_adj["Wrought Aluminum", "Body"] > 0) {
  mat_cont_adj["Mild steel and other steels", "Body"] <-
    mat_cont["Mild steel and other steels", "Body"] - (0.12 * alu_content)
  mat_cont_adj["Cast Aluminum", "Body"] <-
    0.12 * alu_content - mat_cont_adj["Wrought Aluminum", "Body"]
} else {
  mat_cont_adj["Mild steel and other steels", "Body"] <-
    mat_cont_adj["Mild steel and other steels", "Body"] - mat_cont_adj["Wrought Aluminum", "Body"]
}
#Assumption: 19% of aluminum (cast) is in Transmission  (Ducker) - It reduces Mild steel
mat_cont_adj["Mild steel and other steels", "Transmission"] <-
  mat_cont["Mild steel and other steels", "Transmission"] - (0.19 * alu_content -
                                                                   mat_cont_adj["Wrought Aluminum", "Transmission"])
mat_cont_adj["Cast Aluminum", "Transmission"] <-
  0.19 * alu_content - mat_cont_adj["Wrought Aluminum", "Transmission"]
#Assumption: 10% of aluminum is in interior (Ducker) - It reduces Mild steel
mat_cont_adj["Mild steel and other steels", "Interior"] <-
  mat_cont["Mild steel and other steels", "Interior"] - (0.1 * alu_content -
                                                               mat_cont_adj["Wrought Aluminum", "Interior"])
mat_cont_adj["Cast Aluminum", "Interior"] <-
  0.1 * alu_content - mat_cont_adj["Wrought Aluminum", "Interior"]
#Assumption: 8% of aluminum is in chassis (Ducker) - It reduces Cast Iron
mat_cont_adj["Cast Iron", "Chassis"] <-
  mat_cont["Cast Iron", "Chassis"] - (0.08 * alu_content - mat_cont_adj["Wrought Aluminum", "Chassis"])
mat_cont_adj["Cast Aluminum", "Chassis"] <-
  0.08 * alu_content - mat_cont_adj["Wrought Aluminum", "Chassis"]
#Assumption:All the mild steel in Wheels is cast aluminum
mat_cont_adj["Cast Aluminum", "Wheels"]<-
  mat_cont_adj["Mild steel and other steels", "Wheels"]
mat_cont_adj["Mild steel and other steels", "Wheels"]<-0
#Assumption: we adjust cast aluminum content - It reduces Mild steel. All subcomponents except if mild steel is zero
columns_tba<-which(mat_cont_adj["Mild steel and other steels",]!=0&colnames(mat_cont_adj)!="Total")
mat_cont_adj["Mild steel and other steels",columns_tba] <-
  mat_cont_adj["Mild steel and other steels", columns_tba] -
  mat_cont_adj["Cast Aluminum", columns_tba] * 
  ((mat_comp_15[mat_comp_15$Material == "Cast Aluminum", "2015"] - 
     sum(mat_cont_adj["Cast Aluminum",mat_cont_adj["Mild steel and other steels",]==0])) / 
     sum(mat_cont_adj["Cast Aluminum", columns_tba]) -
    1)
mat_cont_adj["Cast Aluminum", columns_tba] <-
  mat_cont_adj["Cast Aluminum", columns_tba] * 
  (mat_comp_15[mat_comp_15$Material == "Cast Aluminum", "2015"] - 
      sum(mat_cont_adj["Cast Aluminum",mat_cont_adj["Mild steel and other steels",]==0])) / 
     sum(mat_cont_adj["Cast Aluminum", columns_tba])

#Assumption: we adjust "Cast Iron" content - It increased Mild Steel
mat_cont_adj["Mild steel and other steels", 1:(ncol(mat_cont_adj) - 1)] <-
  mat_cont_adj["Mild steel and other steels", 1:(ncol(mat_cont_adj) - 1)] -
  mat_cont_adj["Cast Iron", 1:(ncol(mat_cont_adj) - 1)] * (mat_comp_15[which(
   mat_comp_15$Material == "Cast Iron"
  ), "2015"] / sum(mat_cont_adj["Cast Iron", 1:(ncol(mat_cont_adj) - 1)]) -
    1)
mat_cont_adj["Cast Iron", 1:(ncol(mat_cont_adj) - 1)] <-
  mat_cont_adj["Cast Iron", 1:(ncol(mat_cont_adj) - 1)] * mat_comp_15[which(
    mat_comp_15$Material == "Cast Iron"
  ), "2015"] / sum(mat_cont_adj["Cast Iron", 1:(ncol(mat_cont_adj) - 1)])
#Assumption: 50% of steel in Body and exterior is HSS/AHSS (Ducker)
mat_cont_adj["HSS/AHSS", c("Body", "Exterior")] <-
  mat_cont_adj["Mild steel and other steels", c("Body", "Exterior")] * 0.5
mat_cont_adj["Mild steel and other steels", c("Body", "Exterior")] <-
  mat_cont_adj["Mild steel and other steels", c("Body", "Exterior")] * 0.5
#Assumption: Rest of HSS/HSS is in chassis (Modaresi)
mat_cont_adj["HSS/AHSS", "Chassis"] <-
  mat_comp_15[which(
    mat_comp_15$Material == "HSS/AHSS"
  ), "2015"] - sum(mat_cont_adj["HSS/AHSS", c("Body", "Exterior")])
mat_cont_adj["Mild steel and other steels", "Chassis"] <-
  mat_cont_adj["Mild steel and other steels", "Chassis"] - mat_cont_adj["HSS/AHSS", "Chassis"]
#We adjust Mild steel
mat_cont_adj["Other", 1:(ncol(mat_cont_adj) - 1)] <-
  mat_cont_adj["Other", 1:(ncol(mat_cont_adj) - 1)] -
  mat_cont_adj["Mild steel and other steels", 1:(ncol(mat_cont_adj) -1)]*
  (mat_comp_15[which(mat_comp_15$Material == "Mild steel and other steels"), "2015"]/
     sum(mat_cont_adj["Mild steel and other steels", 1:(ncol(mat_cont_adj)-1)]) - 1)
mat_cont_adj["Mild steel and other steels", 1:(ncol(mat_cont_adj) - 1)] <-
  mat_cont_adj["Mild steel and other steels", 1:(ncol(mat_cont_adj) - 1)] *
  mat_comp_15[which(mat_comp_15$Material == "Mild steel and other steels"
  ), "2015"] / sum(mat_cont_adj["Mild steel and other steels", 1:(ncol(mat_cont_adj) -
                                                                    1)])
#Update totals
mat_cont_adj[1:(nrow(mat_cont_adj) - 1), "Total"] <-
  rowSums(mat_cont_adj[1:(nrow(mat_cont_adj) - 1), 1:(ncol(mat_cont_adj) -
                                                        1)])
mat_cont_adj["Total", ] <-
  colSums(mat_cont_adj[1:(nrow(mat_cont_adj) - 1), ])
#Check
mat_cont_adj["Total", ] - mat_cont["Total", ]
mat_cont_adj[-which(rownames(mat_cont_adj)=="Total"), "Total"] - mat_comp_15[, "2015"]
#Create the adjusted material composition per component file
adj_mat_comp<-mat_comp[,1:5]
for (s in 1:(ncol(mat_cont_adj) - 1)) {
  for (m in 1:(nrow(mat_cont_adj) - 1)){
adj_mat_comp[which(adj_mat_comp$Technology%in%c("Glo","ICEV-G,ICEV-D,FFV,CNG")
                   &adj_mat_comp$Subcomponent==colnames(mat_cont_adj)[s]),rownames(mat_cont_adj)[m]]<-
  mat_cont_adj[m,s]/mat_cont_adj["Total",s]
  }
}
#Fill we the unaltered components
rows<-which(is.na(adj_mat_comp$`HSS/AHSS`))
for (r in rows){
  adj_mat_comp[r,6:ncol(adj_mat_comp)]<-mat_comp[r,6:ncol(mat_comp)]
}
#write.csv(adj_mat_comp,"data/component_material_comp_adj.csv", row.names = FALSE)
