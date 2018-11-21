###>Script: Creates the csv file with the vehicle material composition per size and technology from 1980 to 2015 based on TEDB and EPA data
#Sources
source("functions/fleet_i_mat_cont_f.R",local=TRUE)
source("functions/fleet_i_comp_wgt_f.R",local=TRUE)
#Input files
mat_content <- read.csv("inputs/data/glo_rel_mat_comp_80to14.csv",stringsAsFactors = FALSE,check.names = FALSE)
material <-read.csv("inputs/material_equivalency.csv",stringsAsFactors = FALSE,check.names = FALSE)
wgt_dt<-read.csv("inputs/fleet_wgt_75to15.csv",stringsAsFactors = FALSE,check.names = FALSE)
vh_techno <- read.csv("inputs/vehicle_technology.csv",stringsAsFactors = FALSE,check.names = FALSE)
conv<-read.csv("inputs/conversion_units.csv", stringsAsFactors = FALSE, check.names = FALSE,row.names = 1)
#Other inputs
fleet_i_comp_wgt_f_res <- do.call(fleet_i_comp_wgt_f,list())
fleet_compo_wgt <- fleet_i_comp_wgt_f_res[["fleet_compo_wgt"]]
fleet_i_mat_cont_f_res <- do.call(fleet_i_mat_cont_f,list())
fleet_i_mc <- fleet_i_mat_cont_f_res[["fleet_i_mc"]]
#Creation output file
first_yr<-1980
last_yr<-2015
dt_col <- c("Size","Technology","Material","Source",first_yr:last_yr)
fleet_mt_comp <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
for (size in c("Car","Light truck")){
  for (techno in unique(vh_techno$own)){
    if (grepl("BEV",techno) | grepl("PHEV",techno)){tmp_techno <- substring(techno,0,as.numeric(regexpr(pattern="[[:digit:]]{1}",techno))-1)
    } else {
      tmp_techno <- techno
    }
    first_rw <- nrow(fleet_mt_comp) + 1
    last_rw <-nrow(fleet_mt_comp) + length(unique(material$Own))+1
    fleet_mt_comp[first_rw:last_rw, "Size"] <- size
    fleet_mt_comp[first_rw:last_rw, "Technology"] <- techno
    fleet_mt_comp[first_rw:(last_rw-1), "Source"] <- "TEDB&DUCK"
    fleet_mt_comp[first_rw:last_rw, "Material"] <-c(unique(material$Own), "Total")
    for (y in 1980:2015) {
      #IF EPA data not available, consider GREET weight through component function (output already in kg)
      if (nrow(subset(wgt_dt,Technology==tmp_techno & Size==size & Source=="EPA"))==0 & nrow(subset(wgt_dt,Technology==tmp_techno & Size==size & Source=="GREET"))!=0){
        CW <- subset(fleet_compo_wgt,Technology==techno & Size=="Car" & Component=="Total")[,"Weight"]
        fleet_mt_comp[last_rw, "Source"] <- "GREET"
      #If technology-specific EPA and GREET not available, consider Glo data from EPA
      } else if (nrow(subset(wgt_dt,Technology==tmp_techno & Size==size & Source=="EPA"))==0 & nrow(subset(wgt_dt,Technology==tmp_techno & Size==size & Source=="GREET"))==0){
        CW <- subset(wgt_dt,Technology=="Glo" & Size==size &Source=="EPA")[,as.character(y)]*conv["kg","1 lb"]
        fleet_mt_comp[last_rw, "Source"] <- "EPA:Glo"
      #Use technology-specific EPA  
      } else {
        CW <- subset(wgt_dt,Technology==tmp_techno & Size==size &Source=="EPA")[,as.character(y)]*conv["kg","1 lb"]
        fleet_mt_comp[last_rw, "Source"] <- "EPA"
      }
      #For following technology, use own material composition function with adjusted component
      if (!tmp_techno%in%c("HEV", "PHEV","PHEV", "BEV", "FCV") & y!=2015){
        for (mat in unique(material$Own)){
          fleet_mt_comp[fleet_mt_comp$Size==size & fleet_mt_comp$Technology==techno & fleet_mt_comp$Material==mat,as.character(y)] <- 
            subset(mat_content,Technology=="Glo" & Material==mat)[,as.character(y)]*CW
        }
        fleet_mt_comp[last_rw,as.character(y)] <- CW
        
      } else {
        for (mat in unique(material$Own)){
          fleet_mt_comp[fleet_mt_comp$Size==size & fleet_mt_comp$Technology==techno & fleet_mt_comp$Material==mat,as.character(y)] <- 
            subset(fleet_i_mc,Size==size & Technology==techno & Material==mat)[,"Weight"]
        }
        fleet_mt_comp[last_rw,as.character(y)] <- CW 
      }
    }
    
  }
}
write.csv(fleet_mt_comp,"inputs/fleet_mat_comp_80to15.csv", row.names = FALSE)
