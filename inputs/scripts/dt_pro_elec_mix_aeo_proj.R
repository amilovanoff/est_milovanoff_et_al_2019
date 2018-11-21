###>Creates prospective electricity mixes from AEO projections.
source("inputs/scripts/api_eia_data.R")
#Inputs
#key is the personnal key to access EIA API
key<-"fd6b6196a799f52d34de4ebdb4e99d4e"
aeo_case_list <- c("REF","HOP","LOP")
aeo_year=2018
us_elec_mix <- NULL
for (aeo_case in aeo_case_list){
  elec_gen <- getAEOElecGen(aeo_year=aeo_year,aeo_case=aeo_case,key=key)
  #Copy extracted data in file
  write.csv(elec_gen,paste0("inputs/data/electricity_generation_aeo",aeo_year,"_",aeo_case,".csv"), row.names = FALSE)
  #Calculate national mixes
  agg.formula<-reformulate(termlabels = setdiff(colnames(elec_gen),c("Region","Value")),response = "Value")
  elec_gen_us<-aggregate(data = elec_gen,agg.formula,FUN=sum)
  for (year in unique(elec_gen_us$Year)){
    elec_gen_us[elec_gen_us$Year==year,"rel_Value"] <- subset(elec_gen_us,Year==year)[,"Value"]/subset(elec_gen_us,Year==year & Source=="Total")[,"Value"]
  }
  elec_gen_us <- elec_gen_us[with(elec_gen_us,order(Year,Source)),]
  us_elec_mix <- rbind(us_elec_mix,elec_gen_us)
}
write.csv(us_elec_mix,"inputs/us_elec_mix.csv",row.names = FALSE)

