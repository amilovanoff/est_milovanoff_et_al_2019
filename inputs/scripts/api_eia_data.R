###>Creates functions to obtain AEO data from EIA API.

library(XML)
library(reshape2)

getAEOVehicleFC <- function(aeo_year,aeo_case,key){
  aeo_case_name <- switch(aeo_case,
                          "REF"="Reference",
                          "HOP"="High price",
                          "LOP"="Low price")
  
  gen_cat_list=getCatEIA(key=key)
  aeo_cat=as.numeric(as.character(gen_cat_list$Sub_Categories$category_id[gen_cat_list$Sub_Categories$name=="Annual Energy Outlook"]))
  aeo_cat_list=getCatEIA(key=key,cat=aeo_cat)
  aeo_yr_cat=as.numeric(as.character(aeo_cat_list$Sub_Categories$category_id[grep(aeo_year,aeo_cat_list$Sub_Categories$name)]))
  aeo_yr_list=getCatEIA(key=key,cat=aeo_yr_cat)
  aeo_case_id=as.numeric(as.character(aeo_yr_list$Sub_Categories$category_id[aeo_yr_list$Sub_Categories$name==aeo_case_name]))
  aeo_case_list=getCatEIA(key=key,cat=aeo_case_id)
  aeo_trans_id=as.numeric(as.character(aeo_case_list$Sub_Categories$category_id[aeo_case_list$Sub_Categories$name=="Transportation Sector"]))
  aeo_trans_list=getCatEIA(key=key,cat=aeo_trans_id)
  aeo_data_type_id=as.numeric(as.character(aeo_trans_list$Sub_Categories$category_id[aeo_trans_list$Sub_Categories$name=="New Light-Duty Vehicle Fuel Economy (miles per gallon)"]))
  aeo_data_series=getCatEIA(key=key,cat=aeo_data_type_id)
  #Keep all rowas
  aeo_series_row <- 1:length(aeo_data_series$Series_IDs$name)
  #Output
  dt_col<-c("tmp","Year","Value")
  aeo_dt<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  for (i in aeo_series_row){
    serie_name<-as.character(aeo_data_series$Series_IDs[i,"name"])
    serie_id<-as.character(aeo_data_series$Series_IDs[i,"series_id"])
    tmp_dt<-getEIA(ID=serie_id,key=key)
    tmp_dt[,"tmp"]<-serie_name
    #Combine
    aeo_dt<-rbind(aeo_dt,tmp_dt)
  }
  #Format table
  aeo_dt <- cbind(aeo_dt,colsplit(aeo_dt$tmp," : ",names=c("Data","Technology","Size")))
  aeo_dt[aeo_dt$Size=="",c("Size","Technology")] <- colsplit(subset(aeo_dt,Size=="")$Technology,": ",names=c("Technology","Size"))[,c("Size","Technology")]
  aeo_dt[,"Class"]<-colsplit(aeo_dt$Size,", ",names=c("Size","Aeo_case","Aeo_year"))$Size
  aeo_dt[,"Size"] <- NULL
  aeo_dt[,"tmp"] <- NULL
  #Enter unit
  aeo_dt[,"Unit"] <- "MPG"
  aeo_dt[,"Aeo_year"] <- aeo_year
  aeo_dt[,"Aeo_case"] <- aeo_case
  return(aeo_dt)
}

getAEOElecGen <- function(aeo_year,aeo_case,key){
  aeo_case_name <- switch(aeo_case,
                          "REF"="Reference",
                          "HOP"="High price",
                          "LOP"="Low price")
  
  gen_cat_list=getCatEIA(key=key)
  aeo_cat=as.numeric(as.character(gen_cat_list$Sub_Categories$category_id[gen_cat_list$Sub_Categories$name=="Annual Energy Outlook"]))
  aeo_cat_list=getCatEIA(key=key,cat=aeo_cat)
  aeo_yr_cat=as.numeric(as.character(aeo_cat_list$Sub_Categories$category_id[grep(aeo_year,aeo_cat_list$Sub_Categories$name)]))
  aeo_yr_list=getCatEIA(key=key,cat=aeo_yr_cat)
  aeo_case_id=as.numeric(as.character(aeo_yr_list$Sub_Categories$category_id[aeo_yr_list$Sub_Categories$name==aeo_case_name]))
  aeo_case_list=getCatEIA(key=key,cat=aeo_case_id)
  aeo_trans_id=as.numeric(as.character(aeo_case_list$Sub_Categories$category_id[aeo_case_list$Sub_Categories$name=="Electric Power Sector"]))
  aeo_trans_list=getCatEIA(key=key,cat=aeo_trans_id)
  aeo_data_type_id=as.numeric(as.character(aeo_trans_list$Sub_Categories$category_id[aeo_trans_list$Sub_Categories$name=="Electricity Generation by Electricity Market Module Region and Source (billion kilowatthours)"]))
  aeo_data_series=getCatEIA(key=key,cat=aeo_data_type_id)
  #Keep all rowas
  aeo_series_row <- 1:length(aeo_data_series$Series_IDs$name)
  #Output
  dt_col<-c("tmp","Year","Value")
  aeo_dt<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  for (i in aeo_series_row){
    serie_name<-as.character(aeo_data_series$Series_IDs[i,"name"])
    serie_id<-as.character(aeo_data_series$Series_IDs[i,"series_id"])
    tmp_dt<-getEIA(ID=serie_id,key=key)
    tmp_dt[,"tmp"]<-serie_name
    #Combine
    aeo_dt<-rbind(aeo_dt,tmp_dt)
  }
  #Format table
  aeo_dt<-subset(cbind(aeo_dt,colsplit(aeo_dt$tmp," : ",names=c("Data_type","Region","Source"))),select=-tmp)
  aeo_dt$Source<-colsplit(aeo_dt$Source,", ",names=c("Source","Aeo_case","Aeo_year"))$Source
  #Enter unit
  aeo_dt[,"Unit"] <- "billion kilowatthours"
  aeo_dt[,"Aeo_year"] <- aeo_year
  aeo_dt[,"Aeo_case"] <- aeo_case
  return(aeo_dt)
}


getAEOTransportation <- function(aeo_year,aeo_case,aeo_data,key){
  aeo_case_name <- switch(aeo_case,
                          "REF"="Reference",
                          "HOP"="High price",
                          "LOP"="Low price")
  
  aeo_data_name <- switch (aeo_data,
                           "sales"="Light-Duty Vehicle Sales by Technology Type (thousands), United States",
                           "stock"="Light-Duty Vehicle Stock by Technology Type (millions)",
                           print("ERROR: aeo_data not recognized")
  )
  aeo_data_unit <- switch (aeo_data,
                           "sales"=10^3,
                           "stock"=10^6,
                           print("ERROR: aeo_data not recognized")
  )
  
  gen_cat_list=getCatEIA(key=key)
  aeo_cat=as.numeric(as.character(gen_cat_list$Sub_Categories$category_id[gen_cat_list$Sub_Categories$name=="Annual Energy Outlook"]))
  aeo_cat_list=getCatEIA(key=key,cat=aeo_cat)
  aeo_yr_cat=as.numeric(as.character(aeo_cat_list$Sub_Categories$category_id[grep(aeo_year,aeo_cat_list$Sub_Categories$name)]))
  aeo_yr_list=getCatEIA(key=key,cat=aeo_yr_cat)
  aeo_case_id=as.numeric(as.character(aeo_yr_list$Sub_Categories$category_id[aeo_yr_list$Sub_Categories$name==aeo_case_name]))
  aeo_case_list=getCatEIA(key=key,cat=aeo_case_id)
  aeo_trans_id=as.numeric(as.character(aeo_case_list$Sub_Categories$category_id[aeo_case_list$Sub_Categories$name=="Transportation Sector"]))
  aeo_trans_list=getCatEIA(key=key,cat=aeo_trans_id)
  aeo_data_type_id=as.numeric(as.character(aeo_trans_list$Sub_Categories$category_id[aeo_trans_list$Sub_Categories$name==aeo_data_name]))
  aeo_data_series=getCatEIA(key=key,cat=aeo_data_type_id)
  #Eliminate series with "percent" or "total"
  aeo_series_row1<-intersect(grep("Percent",as.character(aeo_data_series$Series_IDs$name),fixed = TRUE,invert=TRUE),grep("Total",as.character(aeo_data_series$Series_IDs$name),invert=TRUE,fixed=TRUE))
  #Keep only series containing "car" and "light truck"
  aeo_series_row2<-union(grep("car",as.character(aeo_data_series$Series_IDs$name),ignore.case = TRUE,invert=FALSE),grep("light truck",as.character(aeo_data_series$Series_IDs$name),ignore.case = TRUE,invert=FALSE))
  aeo_series_row<-intersect(aeo_series_row1,aeo_series_row2)
  #Output
  dt_col<-c("tmp","Year","Value")
  aeo_dt<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  for (i in aeo_series_row){
    serie_name<-as.character(aeo_data_series$Series_IDs[i,"name"])
    serie_id<-as.character(aeo_data_series$Series_IDs[i,"series_id"])
    tmp_dt<-getEIA(ID=serie_id,key=key)
    tmp_dt$Value <- tmp_dt$Value*aeo_data_unit
    tmp_dt[,"tmp"]<-serie_name
    #Combine
    aeo_dt<-rbind(aeo_dt,tmp_dt)
  }
  #Format table
  aeo_dt<-subset(cbind(aeo_dt,colsplit(aeo_dt$tmp," : ",names=c("Data_type","Size","Technology"))),select=-tmp)
  aeo_dt$Technology<-colsplit(aeo_dt$Technology,", ",names=c("Technology","Country","Aeo_case","Aeo_year"))$Technology
  aeo_dt$Size[grep("car",aeo_dt$Size,ignore.case = TRUE)]<-"Car"
  aeo_dt$Size[grep("light truck",aeo_dt$Size,ignore.case = TRUE)]<-"Light truck"
  #Enter unit
  aeo_dt[,"Unit"] <- "vehicle"
  aeo_dt[,"Aeo_year"] <- aeo_year
  aeo_dt[,"Aeo_case"] <- aeo_case
  aeo_dt[,"Data_type"] <- aeo_data
  return(aeo_dt)
}

getAEOMarketShare <- function(aeo_year,aeo_case,key){
  sales_dt <- getAEOTransportation(aeo_year=aeo_year,aeo_case=aeo_case,aeo_data="sales",key=key)
  #Outputs
  dt_col<-c("Year","Size","Value")
  size_market_share_dt <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  dt_col<-c("Year","Size","Technology","Value")
  Technology_market_share_dt <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  for (yr in unique(sales_dt$Year)){
    #total_sales is total number of sales
    total_sales <- sum(subset(sales_dt,Year==yr,select=Value))
    for (sz in unique(sales_dt$Size)){
      #size_sales is total number of sales per size
      size_sales <- sum(subset(sales_dt,Year==yr&Size==sz,select=Value))
      #Update size_market_share
      size_market_share_dt[nrow(size_market_share_dt)+1,]<-c(yr,sz,size_sales/total_sales)
      for (techno in unique(subset(sales_dt,Year==yr&Size==sz)$Technology)){
        techno_sales <- sum(subset(sales_dt,Year==yr&Size==sz&Technology==techno,select=Value))
        Technology_market_share_dt[nrow(Technology_market_share_dt)+1,]<-c(yr,sz,techno,techno_sales/size_sales)
      }
    }
  }
  
}

getEIA <- function(ID, key){
  
  ID <- unlist(strsplit(ID, ";"))
  key <- unlist(strsplit(key, ";"))
  
  url <- paste("http://api.eia.gov/series?series_id=", ID, "&api_key=", key, "&out=xml", sep="" )
  
  doc <- xmlParse(file=url, isURL=TRUE)
  
  df <- data.frame(
    Year = sapply(doc["//data/row/date"], XML::xmlValue),
    Value = sapply(doc["//data/row/value"], XML::xmlValue)
  )
  
  ### Sort from oldest to newest ----
  df <- df[ with(df, order(Year)), ]
  ### Convert Factors in numerics
  df$Year <- as.numeric(as.character(df$Year))
  df$Value <- as.numeric(as.character(df$Value))
  
  return(df)
}

getCatEIA <- function(cat=999999999, key){
  
  key <- unlist(strsplit(key, ";"))
  
  ifelse(cat==999999999,
         url <- paste("http://api.eia.gov/category?api_key=", key, "&out=xml", sep="" ),
         
         url <- paste("http://api.eia.gov/category?api_key=", key, 
                      "&category_id=", cat, "&out=xml", sep="" )
  )
  for( i in 1:3 ) {
    doc <- tryCatch(readLines(url, warn = FALSE), error = function(w) FALSE)
    if (class(doc) != "logical"){
      doc <- xmlParse(doc)
      break
    }
    else
      if(i == 3)
        stop(paste0("Attempted to retrieve data for category #", cat, 
                    " and failed ", i, " times. \n This is likely due to a communication error ", 
                    "with the EIA website."))
  }
  
  Parent_Category <- tryCatch(xmlToDataFrame(
    nodes = XML::getNodeSet(doc, "//category/parent_category_id")), 
    warning=function(w) FALSE, error=function(w) FALSE)
  
  Sub_Categories <- xmlToDataFrame(nodes = XML::getNodeSet(doc, "//childcategories/row"))
  
  Series_IDs <- xmlToDataFrame(nodes = XML::getNodeSet(doc, "///childseries/row"))
  
  Categories <- list(Parent_Category, Sub_Categories, Series_IDs)
  names(Categories) <- c("Parent_Category", "Sub_Categories", "Series_IDs")
  
  return(Categories)
}
