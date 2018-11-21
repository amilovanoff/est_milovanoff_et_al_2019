#Compile CSV files to ease comparison
library(readr)
library(data.table)
pattern="Rel_US_crude"
list_files<- list.files("C:/Users/Alexandre/Dropbox/UofT/Ford/Work/CSV files/Results_R/Aluminum", pattern=pattern, full.names=TRUE)
files_names<-paste0("mat_",substr(list_files,nchar(list_files)-7,nchar(list_files)-4))
for (i in 1:length(list_files)){
assign(files_names[i],read.csv(list_files[i]))
year=substring(files_names[i],5,8)
assign(files_names[i],setNames(get(files_names[i]),c("Producing_countries",year)))
}
assign(pattern,get(files_names[1]))
for (i in 2:length(files_names)){
  assign(pattern, merge.data.frame(get(pattern),get(files_names[i]),"Producing_countries"))
}
CSV_name=paste0(pattern,".csv")
#write.csv(get(pattern),file=CSV_name)
