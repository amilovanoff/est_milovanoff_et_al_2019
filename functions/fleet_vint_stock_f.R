###>Function: Creates the vintaging stock model of the light-duty fleet per technology type, age and year
fleet_vint_stock_f <-function(aeo_scen=NA,
                              survival_rate_mdl=NA,
                              first_yr=1970,
                              last_yr = NA,
                              adj_stock = NA) {
  #Function setup
  source("architecture/attribute_f.R",local = TRUE)
  attribute_f("fleet_vint_stock_f")
  library(reshape2)
  #Functions
  source("functions/survival_rate_f.R",local = TRUE)
  #Input files
  hist_dts <-read.csv("inputs/fleet_st&sl_hist.csv",header = TRUE,stringsAsFactors = FALSE)
  proj_dts <- read.csv(paste0("inputs/fleet_st&sl_proj_",aeo_scen,".csv"),header = TRUE,stringsAsFactors = FALSE)
  dts <- rbind(hist_dts, subset(proj_dts,Year>2016,select=-c(Aeo_case,Aeo_year)))
  remove(hist_dts, proj_dts)
  vh_techno <-read.csv("inputs/vehicle_technology.csv",stringsAsFactors = FALSE,check.names = FALSE)
  #Create output files
  dt_col<-c("Size","Technology","Age","Year","Value")
  fleet_vint_stock<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  for (sz in c("Car","Light truck")){
    for (techno in unique(vh_techno$own)){
      #Create dataframe stock
      dt_col<-as.character(0:30)
      dt_row<-as.character(first_yr:last_yr)
      stock <-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = length(dt_row)),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
      rownames(stock)<-dt_row
      #Fill the first year with sales for the technology and size
      stock[as.character(first_yr),as.character(0)] <- sum(subset(dts,Technology==techno & Size==sz & Year==first_yr & Data_type=="sales")$Value)
      #Estimate the rest of the vintaging stock based on the total stock for the year, size and technology, minus the sales and the survival rate functions normalized
      #Checks if stock is higher than sales
      if ((sum(subset(dts,Technology==techno & Size==sz & Year==first_yr & Data_type=="stock")$Value)- stock[as.character(first_yr),as.character(0)])>0) {
        #survived pop represent the theoritical normalized survived population distribution based on the cumulative survivale rates
        survived_pop<-sapply(1:30, function (x) do.call(survival_rate_f,list(age=x, size=sz, survival_rate_mdl=survival_rate_mdl,cumulative_rate="y",scrappage_rate="n"))) /
          sum(sapply(1:30, function (x) do.call(survival_rate_f,list(age=x, size=sz, survival_rate_mdl=survival_rate_mdl,cumulative_rate="y",scrappage_rate="n"))))
        #The first year stock is based on the survive pop distirbution and the initial stock (minus new vehicles)
        stock[as.character(first_yr), colnames(stock) %in% as.character(1:30)] <-survived_pop*
          (sum(subset(dts,Technology==techno & Size==sz & Year==first_yr & Data_type=="stock")$Value)- stock[as.character(first_yr),as.character(0)])
      }
      #For following years
      for (y in (first_yr + 1):last_yr) {
        #The new vehicles are compiled in a new row
        new_sales <- sum(subset(dts,Technology==techno & Size==sz & Year==y & Data_type=="sales")$Value)
        stock[as.character(y), as.character(0)] <- new_sales
        #The rest of the vintaging is estimated
        for (age in 1:30) {
          #scrap_rate is the scrappage rate at age age
          sur_rate<-do.call(survival_rate_f,list(age=age, size=sz, survival_rate_mdl=survival_rate_mdl,cumulative_rate="n",scrappage_rate="n"))
          #Fill stock from previous year stock and scrap rate
          stock[as.character(y), as.character(age)] <-as.numeric(stock[as.character(y-1), as.character(age-1)])*sur_rate
        }
        #We can build the stock with previous approach. But this will provide different stocks than the historical ones.
        #Assumption: To adjust the stock, 
        ##1) We compare total and estimated with new sales, if total stock is lower than sales, then discrepancy in data. We do nothing
        ##2) If total stock is lower than estimated, then estimated stoch underestimated the scrappage. We evenly decrease the old cars (no new sales)
        if (y <= 2017 | adj_stock == "y"){
          #The estimated stock is adjusted to the given data
          tot_stock <- sum(subset(dts,Technology==techno & Size==sz & Year==y & Data_type=="stock")$Value)
          est_stock <- sum(stock[as.character(y), as.character(0:30)])
          #If total stock is lower than new sales. Discrepancy in the data. We do not adjust.
          if ((tot_stock - new_sales) <= 0 & (est_stock - new_sales) != 0) {
          #If total stock is lower than estimated stock. We adjust by evenly decreasing the old cars in the estimated stock.
          } else if ((tot_stock - new_sales) / (est_stock - new_sales) <= 1 & (tot_stock - new_sales) > 0 & (est_stock - new_sales) != 0) {
            #Adjustement: Ratio between total stock and estimated stock. Assumption: Age distribution is respected.
            stock[as.character(y), as.character(1:30)] <-stock[as.character(y), as.character(1:30)]*(tot_stock - new_sales)/(est_stock - new_sales)
          #If total stock is higher than estimated stock. We adjust the estimated by limiting the scraped vehicles.
          } else if ((tot_stock - new_sales) / (est_stock - new_sales) > 1 & (est_stock - new_sales) != 0) {
            #Adjust the cars per age by limiting to the previous year stock
            for (age in 1:30) {
              #non_adj_stock is the sum of stock of that cannot be adjusted (initially new sales, then the already adjusted stocks)
              non_adj_stock <-sum(stock[as.character(y), as.character(0:(age - 1))])
              #If adjustement higher than previous year, we assume that the same previous age stock is conserved
              if (stock[as.character(y), as.character(age)]*(tot_stock - non_adj_stock) / (est_stock - non_adj_stock) > stock[as.character(y-1), as.character(age - 1)] 
                  & (est_stock - non_adj_stock) != 0) {
                stock[as.character(y), as.character(age)] <-stock[as.character(y-1), as.character(age - 1)]
                #If not, we adjuste with same ratio than before
              } else if (stock[as.character(y), as.character(age)] * (tot_stock - non_adj_stock) / (est_stock - non_adj_stock) <= stock[as.character(y-1), as.character(age - 1)] 
                         & (est_stock - non_adj_stock) != 0) {
                stock[as.character(y), as.character(age)] <-stock[as.character(y), as.character(age)]*(tot_stock - non_adj_stock)/(est_stock - non_adj_stock)
              }
              #Update the estimated stock
              est_stock <-sum(stock[as.character(y), as.character(0:30)])
            }
          }
        }
      }
      #Format
      stock[,"Year"] <- rownames(stock)
      #Melt
      ids_names<-colnames(stock)[!grepl(pattern="[[:digit:]]{1}",colnames(stock))]
      long_stock_dt <- melt(stock, id.vars = ids_names,variable.name = "Age")
      #Format
      colnames(long_stock_dt)[colnames(long_stock_dt)=="value"] <- "Value"
      long_stock_dt[,"Technology"]<-techno
      long_stock_dt[,"Size"] <- sz
      #Combine
      fleet_vint_stock <- rbind(fleet_vint_stock,long_stock_dt)
    }
  }
  #Format
  fleet_vint_stock$Value <- trunc(fleet_vint_stock$Value)
  #Check
  # veh_stock<-data.frame("Historical stock" = sapply((first_yr:last_yr),function(x) sum(subset(dts,Year==x&Data_type=="stock",Value))),check.names = FALSE)
  # rownames(veh_stock)<-first_yr:last_yr
  # veh_stock[,"Estimated stock"]<-sapply((first_yr:last_yr),function(x) sum(subset(fleet_vint_stock,Year==x,Value)))
  # veh_stock[,"Adjustement rates"]<-veh_stock[,"Historical stock"]/veh_stock[,"Estimated stock"]
  # sum(veh_stock$`Historical stock`)==sum(veh_stock$`Estimated stock`)
  return(list(fleet_vint_stock=fleet_vint_stock))
  }
