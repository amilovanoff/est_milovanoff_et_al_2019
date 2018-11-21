###>Script: Creates the csv file with the vehicle material composition for average vehicles from 1980 to 2014 based on TEDB, EPA data and Ducker assumptions.
#Input files
mat_content <- read.csv("inputs/data/glo_mat_comp_75to14.csv",stringsAsFactors = FALSE,check.names = FALSE)
material <- read.csv("inputs/material_equivalency.csv",stringsAsFactors = FALSE,check.names = FALSE)
vh_techno <- read.csv("inputs/vehicle_technology.csv",stringsAsFactors = FALSE,check.names = FALSE)
#Variables
techno <- unique(mat_content$Technology)
#Creation output file
first_yr<-1980
last_yr<-2014
dt_col <- c("Size","Technology","Material","Source",first_yr:last_yr)
fleet_mt_comp <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = length(unique(material$Own))),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
fleet_mt_comp[, "Size"] <- "LDV"
fleet_mt_comp[, "Technology"] <- techno
fleet_mt_comp[, "Source"] <- "TEDB&DUCK"
fleet_mt_comp[, "Material"] <-unique(material$Own)
#Loop for years before 1995
for (y in 1980:1995) {
  #Adjust wrought aluminum amount with DUCKER data (relative) and share of wrought aluminum in aluminum content
  fleet_mt_comp[fleet_mt_comp$`Material` == "Wrought Aluminum" & fleet_mt_comp$Technology == techno, as.character(y)] <-
    subset(mat_content,Category=="material content (lbs)" & Source=="Ducker" & Data=="Aluminum")[,as.character(y)]/
    subset(mat_content,Category=="curb weight (lbs)" & Source=="EPA" & Size=="LDV")[,as.character(y)]*
    subset(mat_content,Category=="material content (%)" & Source=="Modaresi" & Data=="Wrought Aluminum")[,as.character(y)]
  #Adjust cast aluminum amount with DUCKER data (relative) and share of cast aluminum in aluminum content
  fleet_mt_comp[fleet_mt_comp$`Material`=="Cast Aluminum" & fleet_mt_comp$Technology==techno, as.character(y)] <-
    subset(mat_content,Category=="material content (lbs)" & Source=="Ducker" & Data=="Aluminum")[,as.character(y)]/
    subset(mat_content,Category=="curb weight (lbs)" & Source=="EPA" & Size=="LDV")[,as.character(y)]*
    subset(mat_content,Category=="material content (%)" & Source=="Modaresi" & Data=="Cast Aluminum")[,as.character(y)]
  #Rest of the materials (same material content than in 1995)
  for (mat in setdiff(unique(material$Own),c("Cast Aluminum","Wrought Aluminum"))) {
    fleet_mt_comp[fleet_mt_comp$Material==mat, as.character(y)] <-
      #Material composition of 1995 adjusted with the aluminum content of year y.
      sum(mat_content[which(sapply(mat_content$Data[which(mat_content$Category == "material content (%)"
                                                          &mat_content$Source == "TEDB")],
                                   function(x)grepl(x, material$TEDB[which(material$Own == mat)]))),
                      as.character(1995)]) *
      (1 + mat_content[which(mat_content$Data == "Aluminum" &
                               mat_content$Category == "material content (%)"),
                       as.character(1995)] - sum(fleet_mt_comp[grep("Aluminum", fleet_mt_comp$Material), as.character(y)]))
  }
  #Other materials
  fleet_mt_comp[fleet_mt_comp$Material=="Other", as.character(y)] <- 1 - sum(fleet_mt_comp[fleet_mt_comp$Material!="Other", as.character(y)])
}
#Loop for rest of the years
for (y in 1996:2014) {
  #Relative material composition of all materials
  for (mat in unique(material$Own)) {
    #If aluminum, needs to be splitted in Wrought and cast
    if (mat == "Wrought Aluminum") {
      fleet_mt_comp[fleet_mt_comp$Material==mat, as.character(y)] <- 
        subset(mat_content,Data=="Aluminum" & Category=="material content (%)")[,as.character(y)]*
        subset(mat_content,Category=="material content (%)" & Source=="Modaresi" & Data=="Wrought Aluminum")[,as.character(y)]
    } else if (mat == "Cast Aluminum") {
      fleet_mt_comp[fleet_mt_comp$Material==mat, as.character(y)] <-
        subset(mat_content,Data=="Aluminum" & Category=="material content (%)")[,as.character(y)]*
        subset(mat_content,Category=="material content (%)" & Source=="Modaresi" & Data=="Cast Aluminum")[,as.character(y)]
    } else {
      fleet_mt_comp[fleet_mt_comp$Material==mat, as.character(y)] <-
        sum(mat_content[which(sapply(mat_content$Data[which(mat_content$Category == "material content (%)" &mat_content$Source == "TEDB")],
        function(x)
          grepl(x, material$TEDB[which(material$Own == mat)]))),
        as.character(y)])
    }
  }
}
write.csv(fleet_mt_comp,"inputs/data/glo_rel_mat_comp_80to14.csv", row.names = FALSE)