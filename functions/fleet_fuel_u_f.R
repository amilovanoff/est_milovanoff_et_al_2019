###>Function: Calculates the on-road fuel use by scenarios and fuel type
fleet_fuel_u_f<-function (aeo_scen = NA,
                          adj_stock = NA,
                          survival_rate_mdl = NA,
                          last_yr=NA,
                          fast_mode="n"){
  #Source
  source("architecture/attribute_f.R",local=TRUE)
  attribute_f("fleet_fuel_u_f")
  #Inputs files
  vh_techno <- read.csv("inputs/vehicle_technology.csv", stringsAsFactors = FALSE, check.names = FALSE)
  #Functions' Outputs
  fleet_fc_proj_f_res<-do.call(fun_res_f,list(fun_name="fleet_fc_proj_f",fast_mode=fast_mode))
  fleet_FC_dt<-fleet_fc_proj_f_res[["fleet_FC_dt"]]
  #fleet_vkmt_share contains the VKMT share per fuel type, technology, size and scenario.
  fleet_vkmt_share_f_res<-do.call(fun_res_f,list(fun_name="fleet_vkmt_share_f",fast_mode=fast_mode))
  fleet_vkmt_share<-fleet_vkmt_share_f_res[["fleet_vkmt_share"]]
  #fleet_vkmt
  fleet_vkmt_f_res<-do.call(fun_res_f,list(fun_name="fleet_vkmt_f",fast_mode=fast_mode))
  fleet_vint_vkmt <- fleet_vkmt_f_res[["fleet_vint_vkmt"]]
  LWscen_l<-unique(fleet_FC_dt$Scenario)
  fuels<-unique(unlist(strsplit(vh_techno$`Fuel type`, ";")))
  first_yr=2015
  #Creation output files
  #fuel_use is the data.frame with the fuel use per year, scenario and fuel
  dt_col<-c("Scenario","Fuel","Unit",first_yr:last_yr)
  fuel_use<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  #fuel_use_techno contains the fuel use per year, scenario, fuel and technology
  dt_col<-c("Scenario","Size","Technology","Fuel","Unit",first_yr:last_yr)
  fuel_use_techno<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  #nw_vh_fuel_us is the data frame with the fuel use per new vehicles per year, scenario and fuel
  nw_vh_fuel_use<-fuel_use
  #Loop for LWscen
  for (LWscen in LWscen_l){
    rows<-nrow(fuel_use)+1:length(fuels)
    fuel_use[rows,"Fuel"]<-fuels
    fuel_use[rows,"Unit"]<-sapply(fuels,function(x) unique(vh_techno$`Fuel unit`[vh_techno$`Fuel type`==x]))[fuels]
    fuel_use[rows,"Scenario"]<-LWscen
    nw_vh_fuel_use[rows,"Fuel"]<-fuels
    nw_vh_fuel_use[rows,"Unit"]<-sapply(fuels,function(x) unique(vh_techno$`Fuel unit`[vh_techno$`Fuel type`==x]))[fuels]
    nw_vh_fuel_use[rows,"Scenario"]<-LWscen
    #Loop for size
    for (size in c("Car", "Light truck")) {
      #Loop for technology
      for (techno in unique(vh_techno[, "own"])) {
        #Vintaged VKMT
        stock_VKMT<-subset(fleet_vint_vkmt,subset = Adj_stock==adj_stock & Survival_rate_mdl==survival_rate_mdl & Size==size & Technology == techno)
        #Fuel types per technology
        fuel_l <-unlist(strsplit(vh_techno$`Fuel type`[which(vh_techno$own == techno)][1], ";"))
        for (fuel_type in fuel_l) {
          #Create the temporary dataset
          fuel_use_techno_temp<-setNames(data.frame(matrix(0,ncol = ncol(fuel_use_techno), nrow = 1),stringsAsFactors = FALSE,check.names = FALSE),colnames(fuel_use_techno))
          #Fuel Economy for technology
          FC_dt<-subset(fleet_FC_dt,subset=Size==size & Technology==techno & `Fuel type`==fuel_type & Scenario==LWscen)
          if (!is.null(FC_dt)){
          for (y in first_yr:last_yr) {
            if (sum(subset(stock_VKMT,Year==y,Value))!=0){
              #Loop on ages with values different to 0
              for (age in subset(stock_VKMT,Year==y & Value!=0)[,"Age"]) {
                #vkmt_share is the share of distance traveled on fuel_type. If car older than 2015 models, consider VKMT share of 2015 models.
                vkmt_share<-as.numeric(subset(fleet_vkmt_share,
                                              subset= Scenario == LWscen & Size == size & Technology == techno & `Fuel type` == fuel_type,
                                              select = as.character(ifelse(y-age<2015,2015,y-age))))
                #Fill fuel_use_techno_temp
                fuel_use_techno_temp[1,as.character(y)] <- fuel_use_techno_temp[1,as.character(y)]+
                  as.numeric(subset(stock_VKMT,Year==y & Age==age,Value))*
                  vkmt_share*
                  subset(FC_dt,Year==(y-age))[,"Value"]/100
                #Fill fuel_use
                fuel_use[intersect(rows,which(fuel_use$Fuel==fuel_type)),as.character(y)]<-
                  ifelse(is.na(fuel_use[intersect(rows,which(fuel_use$Fuel==fuel_type)),as.character(y)]),0,
                         fuel_use[intersect(rows,which(fuel_use$Fuel==fuel_type)),as.character(y)])+
                  as.numeric(subset(stock_VKMT,Year==y & Age==age,Value))*
                  vkmt_share*
                  subset(FC_dt,Year==(y-age))[,"Value"]/100
                }
              #Fill nw_vh_fuel_use
              nw_vh_fuel_use[intersect(rows,which(nw_vh_fuel_use$Fuel==fuel_type)),as.character(y)]<-
                ifelse(is.na(nw_vh_fuel_use[intersect(rows,which(nw_vh_fuel_use$Fuel==fuel_type)),as.character(y)]),0,
                       nw_vh_fuel_use[intersect(rows,which(nw_vh_fuel_use$Fuel==fuel_type)),as.character(y)])+
                as.numeric(subset(stock_VKMT,Year==y & Age==0,Value))*
                vkmt_share*
                subset(FC_dt,Year==(y))[,"Value"]/100
            } else {
              fuel_use_techno_temp[1,as.character(y)]<-0
            }
          }
          }
          #Fill rest of data for fuel_use_techno_temp
          fuel_use_techno_temp[1,c("Scenario","Size","Technology","Fuel","Unit")]<-c(LWscen,size,techno,fuel_type,unique(vh_techno$`Fuel unit`[vh_techno$`Fuel type`==fuel_type]))
          #Merge fuel_use_techno
          fuel_use_techno<-rbind(fuel_use_techno,fuel_use_techno_temp)
          }
        
      }
    }
  }
  results<-list(fuel_use=fuel_use,
                fuel_use_techno=fuel_use_techno,
                nw_vh_fuel_use=nw_vh_fuel_use)
  return(results)
}
