comp_data<-read.csv("data/comp_mat_comp_US.csv", stringsAsFactors = FALSE, check.names = FALSE)
TEDB_data<-read.csv("data/Fleet_mat_comp_80to15DATA.csv", stringsAsFactors = FALSE, check.names = FALSE)
source("functions/Fleet_mat_cont_f.R")
size_l=c("Car", "Light truck")
year=2014
vh_techno <- read.csv("data/vehicle_technology.csv", stringsAsFactors = FALSE, check.names = FALSE)
techno=unique(vh_techno[,"Own"])[1]
for (s in 1:2) {
  #Compiles TEDB data
  size=size_l[s]
  first_rw<-(nrow(comp_data)+1)
  last_rw<-(nrow(comp_data)+length(TEDB_data$`Material (lbs)`[which(TEDB_data$Size==size)]))
  comp_data[first_rw:last_rw,"Material"]<-TEDB_data$`Material (lbs)`[which(TEDB_data$Size==size)]
  comp_data[first_rw:last_rw,"Size"]<-size
  comp_data[first_rw:last_rw,"Source"]<-"TEDB"
  comp_data[first_rw:last_rw,"Year"]<-year
  comp_data[first_rw:last_rw,"Technology"]<-"All"
  comp_data[first_rw:last_rw,"Content (lbs)"]<-TEDB_data[which(TEDB_data$Size==size), as.character(year)]
  comp_data[first_rw:last_rw,"Relative content"]<-comp_data[first_rw:last_rw,"Content (lbs)"]/
    TEDB_data[which(TEDB_data$Size==size&TEDB_data$`Material (lbs)`=="Total (lbs)"), as.character(year)]
  
  #Compiles GREET data
  for (t in 1:nrow(vh_techno)){
    techno=vh_techno[t,size]
    mat_cont<-mat_cont_f(techno,size, "GREET", scenario,batt_type_ev )
    first_rw<-(nrow(comp_data)+1)
    last_rw<-(nrow(comp_data)+length(TEDB_data$`Material (lbs)`[which(TEDB_data$Size==size)]))
    comp_data[first_rw:last_rw,"Material"]<-rownames(mat_cont)
    comp_data[first_rw:last_rw,"Size"]<-size
    comp_data[first_rw:last_rw,"Source"]<-paste0("GREET", scenario)
    comp_data[first_rw:last_rw,"Year"]<-"ND"
    comp_data[first_rw:last_rw,"Technology"]<-techno
    comp_data[first_rw:last_rw,"Content (lbs)"]<-mat_cont$Total
    comp_data[first_rw:last_rw,"Relative content"]<-comp_data[first_rw:last_rw,"Content (lbs)"]/
      mat_cont["Total", "Total"]
  }
}

for (s in 1:2) {  
  #Compiles GREET data for scenario 2
  scenario=2
  size=size_l[s]
  techno=vh_techno[1,size]
  mat_cont<-mat_cont_f(techno,size, "GREET", scenario,batt_type_ev )
  first_rw<-(nrow(comp_data)+1)
  last_rw<-(nrow(comp_data)+length(TEDB_data$`Material (lbs)`[which(TEDB_data$Size==size)]))
  comp_data[first_rw:last_rw,"Material"]<-rownames(mat_cont)
  comp_data[first_rw:last_rw,"Size"]<-size
  comp_data[first_rw:last_rw,"Source"]<-paste0("GREET", scenario)
  comp_data[first_rw:last_rw,"Year"]<-"ND"
  comp_data[first_rw:last_rw,"Technology"]<-techno
  comp_data[first_rw:last_rw,"Content (lbs)"]<-mat_cont$Total
  comp_data[first_rw:last_rw,"Relative content"]<-comp_data[first_rw:last_rw,"Content (lbs)"]/
    mat_cont["Total", "Total"]
}


#write.csv(comp_data, "data/comp_mat_comp_US.csv", row.names = FALSE)
