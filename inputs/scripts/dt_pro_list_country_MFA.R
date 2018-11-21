#Check the country list with the exporting and importing countries given by ResourceTrades. It updates list_country
list_country<-read.csv("data/list_country.csv", stringsAsFactors = FALSE)
files_td<-list.files("MFA/Original_data", pattern="resourcetradeearth", full.names = TRUE)
for (y in 2000:2015){
  #Download the data for the specific year
  trades_ds <- read_excel(files_td[grep(pattern=y,files_td)],sheet = "Trades")
  list_exporter<-sort(unique(trades_ds$Exporter))
  list_importer<-sort(unique(trades_ds$Importer))
  #Verification of countries names
  for (c in 1:length(list_importer)){
    if (length(which(list_country$Country==list_importer[c]))!=1) {
      print(list_importer[c])
      #list_country[nrow(list_country)+1, 1]<-list_importer[c]
    }
  }
  for (c in 1:length(list_exporter)){
    if (length(which(list_country$Country==list_exporter[c]))!=1) {
      print(list_exporter[c])
      #list_country[nrow(list_country)+1, 1]<-list_exporter[c]
    }
  }
  }
#write.csv(list_country, "list_country.csv")
