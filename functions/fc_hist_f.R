###>Function: Create the fuel consumption by vehicle type until 2015
#Attributes:size, techno, fuel_type
fc_hist_f <- function(size,
                      techno,
                      fuel_type){
  #Assign arguments default values
  source("architecture/attribute_f.R",local = TRUE)
  attribute_f("fc_hist_f")
  #Other functions
  source("functions/fc_ev_hist_f.R",local = TRUE)
  #Input files
  stock_dt <-read.csv("inputs/fleet_vint_stock_REF.csv",stringsAsFactors = FALSE)
  fe_vision<-read.csv("inputs/fe_vision_dt_2017.csv", stringsAsFactors = FALSE, check.names = FALSE)
  degra_fac<-read.csv("inputs/fc_degra_factor_vision.csv", stringsAsFactors = FALSE, check.names = FALSE)
  FE_EPA<-read.csv("inputs/EPA_FE.csv", stringsAsFactors = FALSE, check.names = FALSE)
  conv<-read.csv("inputs/conversion_units.csv", stringsAsFactors = FALSE, check.names = FALSE,row.names = 1)
  fuel_conv<-read.csv("inputs/fuel_conversion.csv", stringsAsFactors = FALSE, check.names = FALSE)
  vh_techno <- read.csv("inputs/vehicle_technology.csv", stringsAsFactors = FALSE, check.names = FALSE)
  #Other parameters
  first_yr=1980
  last_yr=2015
  #Define the source to use
  if (techno%in%c("ICEV-G","ICEV-D")){
    fc_src="EPA"
  } else if (techno%in%c("BEV100","BEV300","PHEV20","PHEV40")){
    fc_src="own"
    } else {
    fc_src="VISION"
    }
  #Obtain the first model year to consider for FC
  vint_stock <- subset(stock_dt,Technology==techno & Size==size)
  min_y <- min(subset(vint_stock, Value!=0)[,"Year"])
  #Creation output files
  dt_col <- c("Technology","Size","Fuel type","Unit","Year","Value")
  FC_dt <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  unit_fuel <- unique(vh_techno$`Fuel unit`[vh_techno$`Fuel type`==fuel_type])
  unit_fc <- paste0(unit_fuel,"/100km")
  #fuel_conv_fact is a conversion factor to convert from L equivalent gasoline to L of fuel (or kWh)
  fuel_conv_fact<-fuel_conv[fuel_conv$Fuel=="Gasoline"&fuel_conv$Data=="Conversion factor","value"]/fuel_conv[fuel_conv$Fuel==fuel_type&fuel_conv$Data=="Conversion factor","value"]
  if (fc_src=="VISION"){
    #vision_techno contains the list of equivalent technologies in vision data
    vision_techno<-unlist(strsplit(vh_techno$vision[vh_techno$own==techno],";"))
    #VISION data are unadjusted and combined. We need to consider the degradation factor provided by VISION.
    deg_fac <- subset(degra_fac,Technology==techno & Size==size & `Fuel type`==fuel_type)[,"Degradation factor"]
    #Loop for years
    for (y in first_yr:last_yr){
      #Check if there a stock of vehicle for year y. temp_techno are the technologies of AEO associated with techno.
      if (y < min_y){
        next
      #Check if a FC value is available for any equivalent technologies
      } else if(all(is.na(subset(fe_vision,Technology%in%vision_techno & Size==size & `Fuel type`==fuel_type & Data=="Unadjusted Fuel Economy")[,as.character(y)]))){
        fc_value <- NA
        #If some FE data are NA but not all, then consider only no Na data        
        } else {
          #Do not consider technologies with NA values
          fe_value <- subset(fe_vision,Technology%in%vision_techno & Size==size & `Fuel type`==fuel_type & Data=="Unadjusted Fuel Economy" & !is.na(get(as.character(y))))[,as.character(y)]
          #Convert fuel economy (MPGe) in fuel consumpton (L of kWh/100km)
          fc_value<-1/(fe_value*deg_fac)*conv["L","1 gal"]*conv["mile","1 km"]*100*fuel_conv_fact
        }
      FC_dt[nrow(FC_dt)+1,c("Technology","Size","Fuel type","Year","Value")] <- c(techno,size,fuel_type,y,fc_value)
    }
  } else if (fc_src=="EPA"){
    #Loop for years
    for (y in first_yr:last_yr){
      if (y < min_y){
        next
        #Check if a FC value is available for any equivalent technologies
      } else if (!as.character(y) %in% colnames(FE_EPA)){
        fc_value <- NA
        #Check if sales-weighted data are available for year y
        } else if (nrow(subset(FE_EPA,Size==size & Technology==techno & `Fuel type`==fuel_type & Data=="Sales-weighted FC"))!=0){
          #Extract combined data. Multiply by fuel conversion factor
          fc_value <- subset(FE_EPA,Size==size & Technology==techno & `Fuel type`==fuel_type & Data=="Sales-weighted FC" & `Drive Cycle`=="Combined")[,as.character(y)]*fuel_conv_fact
          #If no sales-weighted data. Consider the fc_qt of the fuel consumption distribution for the model year y
        }
      FC_dt[nrow(FC_dt)+1,c("Technology","Size","Fuel type","Year","Value")] <- c(techno,size,fuel_type,y,fc_value)
      }
  } else if (fc_src=="own"){
    #Loop for years
    for (y in first_yr:last_yr){
      if (y < min_y){
        next
      } else {
        fc_value <- do.call(fc_ev_hist_f,list(size=size,techno=techno,fuel_type=fuel_type))
      }
      FC_dt[nrow(FC_dt)+1,c("Technology","Size","Fuel type","Year","Value")] <- c(techno,size,fuel_type,y,fc_value)
    }
  }
  #Solve NA discrepancy: Discrepancy between first sales and model years. Assumption: FC from the first available model year.
  #Some NA could stay in the middle of data. Not considered here.
  if (any(is.na(FC_dt[,"Value"]))){
    #f_y_na is the list of years with NA values
    f_y_na <- subset(FC_dt,is.na(Value))[,"Year"]
    #f_y_val is the list of years with numerical values
    f_y_val <- subset(FC_dt,!is.na(Value) & Value!="NS")[,"Year"]
    #for each NA value, consider the closest numerical value (but above in years)
    for (y in f_y_na){
      temp_y <- min(f_y_val[f_y_val>y])
      FC_dt[FC_dt$Year==y,"Value"] <- FC_dt[FC_dt$Year==temp_y,"Value"]
    }
  }
  #Format
  FC_dt[,"Unit"] <- unit_fc
  FC_dt[,c("Year","Value")] <- sapply(c("Year","Value"),function(x)as.numeric(FC_dt[,x])) 
  
  return(FC_dt)
}
#fc_dt<-do.call(fc_hist_f,list(size="Car",techno="BEV",fuel_type="Electricity"))
