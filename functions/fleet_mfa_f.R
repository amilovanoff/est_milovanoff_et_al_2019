###>Function: Creates the material flow associated with the light duty vehicles
fleet_mfa_f <- function(rec_scen=NA,
                        aeo_scen = NA,
                        adj_stock = NA,
                        survival_rate_mdl = NA,
                        last_yr = NA,
                        fast_mode="n") {
  #Assign the default values of arguments
  source("architecture/attribute_f.R",local = TRUE)
  #Assign default value
  attribute_f("fleet_mfa_f")
  #Source
  source("functions/rec_rate_f.R",local = TRUE)
  #Input files
  vh_techno <-read.csv("inputs/vehicle_technology.csv",stringsAsFactors = FALSE,check.names = FALSE)
  mat_comp_80to15 <-read.csv("inputs/fleet_mat_comp_80to15.csv",stringsAsFactors = FALSE,check.names = FALSE)
  material_dt <-read.csv("inputs/material_equivalency.csv",stringsAsFactors = FALSE,check.names = FALSE)
  stock_dt <-read.csv(paste0("inputs/fleet_vint_stock_",aeo_scen,".csv"),stringsAsFactors = FALSE,check.names = FALSE)
  nw_scrap_rt <-read.csv("inputs/new_scrap_rate.csv",stringsAsFactors = FALSE,check.names = FALSE)#New scrap rate
  prim_sec_share <-read.csv("inputs/prim_sec_share.csv",stringsAsFactors = FALSE,check.names = FALSE)#Share of primary and external secondary materials
  recovery_rt <-do.call(rec_rate_f,list(rec_scen=rec_scen))
  #Functions' Outputs
  fleet_mc_proj_f_res<-do.call(fun_res_f,list(fun_name="fleet_mc_proj_f",fast_mode=fast_mode))
  mat_comp_15to50<-fleet_mc_proj_f_res[["fleet_mt_comp_proj"]]
  #Other parameters
  size_l <- c("Car", "Light truck")
  LWscen_l<-unique(mat_comp_15to50$Scenario)
  #mat_l is the list of all matrices
  mat_l<-c("mat_emb","mat_dem","mat_scrap","mat_int_sec","mat_prim","mat_sec","mat_prim_share","mat_sec_share")
  #Scrap input per secondary material: Represent the losses between the recovered materials and the secondary material supply
  scrap_input = 1
  for (LWscen in LWscen_l){
    #Output files
    #mat_emb is the matrix of material embodied in the new vehicles
    mat_emb <-matrix(0,ncol = length(2015:last_yr),nrow = length(material_dt$Own))
    colnames(mat_emb) <- 2015:last_yr
    rownames(mat_emb) <- material_dt$Own
    #mat_dem is the matrix of material demanded in the manufacturing phase of the new vehicles (include new scrap loss)
    mat_dem <- mat_emb[-which(rownames(mat_emb)=="Other"),]
    #mat_scrap is the matrix of scrapped materials
    mat_scrap <-matrix(0,ncol = length(2015:last_yr),nrow = length(material_dt$Own))
    colnames(mat_scrap) <- 2015:last_yr
    rownames(mat_scrap) <- material_dt$Own
    #mat_rec is the matrix of recovered material (e.g. recovered from scrapped materials)
    rec_mat_l<-union(material_dt$Own,unique(recovery_rt$`Recovered material`))
    mat_rec <-matrix(0, ncol = length(2015:last_yr), nrow = length(rec_mat_l))
    colnames(mat_rec) <- 2015:last_yr
    rownames(mat_rec) <- rec_mat_l
    #mat_int_sec is the matrix of internal secondary material acquired from the recovered materials vehicles
    mat_int_sec <- mat_rec[which(rownames(mat_rec)%in%material_dt$Own),]
    #mat_prim is the matrix of primary material production
    mat_prim <- mat_dem
    #mat_sec is the matrix of secondary material supply (internal and external)
    mat_sec <- mat_dem
    #Loop for size
    for (size in size_l) {
      #Loop for technology
      for (techno in unique(vh_techno$own)) {
        #Agregate the technology-specific vehicle's material composition. Warning: rownames
        hist_mat_comp <- subset(mat_comp_80to15,Size == size & Technology == techno)[,c("Material",as.character(1980:2014))]
        rownames(hist_mat_comp) <- hist_mat_comp[,"Material"]
        hist_mat_comp[,"Material"] <- NULL
        proj_mat_comp <- subset(mat_comp_15to50,Size == size & Technology == techno & Scenario==LWscen)[,c("Material",as.character(2015:last_yr))]
        rownames(proj_mat_comp) <- proj_mat_comp[,"Material"]
        proj_mat_comp[,"Material"] <- NULL
        row_names_mat_compt <- rownames(proj_mat_comp)
        mat_comp <-cbind(hist_mat_comp[row_names_mat_compt,],proj_mat_comp[row_names_mat_compt,])
        #Extract the technology-specific vitanging stock
        stock <- subset(stock_dt,subset = Adj_stock==adj_stock & Survival_rate_mdl==survival_rate_mdl & Size==size & Technology == techno)
        #Loop for year
        for (y in 2015:last_yr) {
          #scrap_veh_dt contains the number of scrapped vehicles in year "y" by age for technology techno and size size
          dt_col<-c("Age","Value")
          scrap_veh_dt<-setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
          #For all ages, calculate number of scrapped vehicles by age at scrapping. No vehicle of age 0 is being scrapped.
          for (age in 1:30){
            scrap_veh_dt[nrow(scrap_veh_dt)+1,c("Age","Value")] <- c(age,as.numeric(subset(stock,Year==(y-1) & Age==(age-1),Value))-as.numeric(subset(stock,Year==y & Age==age,Value)))
          }
          scrap_veh_dt[nrow(scrap_veh_dt)+1,c("Age","Value")] <- c(31,as.numeric(subset(stock,Year==y-1 & Age==30,Value)))
          #Loop for material
          for (m in 1:nrow(mat_emb)) {
            mat_emb[m, as.character(y)] <- mat_emb[m, as.character(y)] +
              as.numeric(subset(stock,Year==y & Age==0,Value)) *
              mat_comp[which(rownames(mat_comp) == rownames(mat_emb)[m]), as.character(y)]
            #Loop for age
            for (age in 1:31) {
              mat_scrap[m, as.character(y)] <- mat_scrap[m, as.character(y)] +
                as.numeric(subset(scrap_veh_dt,Age==age,Value)) *
                mat_comp[which(rownames(mat_comp) == rownames(mat_scrap)[m]), as.character(y - age)]
            }
          }
        }
      }
    }
    #Loop for embodied materials in vehicles
    for (m in 1:nrow(mat_dem)) {
      #Material demand is higher than the material embodied because of new scrap losses
      mat_dem[m,] <-
        mat_emb[rownames(mat_dem)[m],] / (1 - nw_scrap_rt$Rate[which(nw_scrap_rt$Material == rownames(mat_dem)[m])])
      #Assumption: 100% of new scrap are recovered
      mat_rec[rownames(mat_dem)[m],] <-
        mat_rec[rownames(mat_dem)[m],]+
        nw_scrap_rt$Rate[which(nw_scrap_rt$Material == rownames(mat_dem)[m])] *
        mat_dem[m,]
      #Check
      #print(max(mat_dem[rownames(mat_dem)[m],]-mat_rec[rownames(mat_dem)[m],]-mat_emb[rownames(mat_dem)[m],]))
    }
    #Loop for year
    for (y in 2015:last_yr) {
      #Loop for scrapped material
      for (m in 1:(nrow(mat_scrap) - 1)) {
        #reco_mat_l is the list of recovered materials for the specific scrapped material
        reco_mat_l <-
          unique(recovery_rt$`Recovered material`[which(
            recovery_rt$Year == y
            &recovery_rt$`Scrapped material` == rownames(mat_scrap)[m]
          )])
        #Loop for recovered material
        for (l in 1:length(reco_mat_l)) {
          mat_rec[reco_mat_l[l], as.character(y)] <-
            mat_rec[reco_mat_l[l], as.character(y)] +
            mat_scrap[m, as.character(y)] *
            recovery_rt$value[which( recovery_rt$Year == y
                                     & recovery_rt$`Scrapped material` == rownames(mat_scrap)[m]
                                     & recovery_rt$`Recovered material` == reco_mat_l[l]
            )]
        }
      }
      #Internal secondary materials come from recovered materials
      mat_int_sec[, as.character(y)] <-
        mat_rec[rownames(mat_int_sec), as.character(y)] / scrap_input
      #Loop for material demand
      for (material in sort(rownames(mat_prim),decreasing=TRUE)) {
        #If material different than Cast Aluminum
        if (material!="Cast Aluminum"){
          #If internal secondary is higher than demand. No primary production.
          if (mat_rec[material, as.character(y)] / scrap_input >= mat_dem[material, as.character(y)]) {
            mat_prim[material, as.character(y)] = 0
            mat_int_sec[material, as.character(y)] = mat_dem[material, as.character(y)]
            mat_sec[material, as.character(y)] = mat_dem[material, as.character(y)]
            #Else, primary production and external secondary according to share rates
          } else {
            #Share of primary production
            mat_prim[material, as.character(y)] = prim_sec_share$Primary[which(prim_sec_share$Material == material)] *
              (mat_dem[material, as.character(y)] - mat_rec[material, as.character(y)] / scrap_input)
            mat_int_sec[material, as.character(y)] = mat_rec[material, as.character(y)] / scrap_input
            #Share of secondary (internal and external)
            mat_sec[material, as.character(y)] = mat_int_sec[material, as.character(y)] +
              prim_sec_share$`External secondary`[which(prim_sec_share$Material == material)] *
              (mat_dem[material, as.character(y)] - mat_int_sec[material, as.character(y)])
          }
        } else {
          #If internal secondary is higher than demand. No primary production.
          if ((mat_rec[material, as.character(y)] / scrap_input >= mat_dem[material, as.character(y)])|
              ((mat_rec[material, as.character(y)]+mat_rec["Wrought Aluminum", as.character(y)])/scrap_input-mat_int_sec["Wrought Aluminum", as.character(y)])
              >=mat_dem[material, as.character(y)]) {
            mat_prim[material, as.character(y)] = 0
            mat_int_sec[material, as.character(y)] = mat_dem[material, as.character(y)]
            mat_sec[material, as.character(y)] = mat_dem[material, as.character(y)]
            #Else, primary production and external secondary according to share rates
          } else {
            #Share of primary production
            mat_int_sec[material, as.character(y)] = (mat_rec[material, as.character(y)]+mat_rec["Wrought Aluminum", as.character(y)])/scrap_input-mat_int_sec["Wrought Aluminum", as.character(y)]
            mat_prim[material, as.character(y)] = prim_sec_share$Primary[which(prim_sec_share$Material == material)] *
              (mat_dem[material, as.character(y)] - mat_int_sec[material, as.character(y)])
            #Share of secondary (internal and external)
            mat_sec[material, as.character(y)] = mat_int_sec[material, as.character(y)] +
              prim_sec_share$`External secondary`[which(prim_sec_share$Material == material)] *
              (mat_dem[material, as.character(y)] - mat_int_sec[material, as.character(y)])
          }
        }
      }
    }
    #Create matrix with primary and secondary shares
    mat_prim_share <- mat_prim / mat_dem[rownames(mat_prim), ]
    mat_sec_share <- mat_sec / mat_dem[rownames(mat_sec), ]
   
    for (mat in mat_l){
      tmp_mat<-as.data.frame(get(mat),sting.as.factors=FALSE,check.names=FALSE)
      tmp_mat[,"Material"]<-rownames(tmp_mat)
      rownames(tmp_mat)<-NULL
      tmp_mat[,"Scenario"]<-LWscen
      if (exists(paste0(mat,"_dt"))){
        tmp_dt<-get(paste0(mat,"_dt"))
        assign(paste0(mat,"_dt"),rbind(tmp_dt,tmp_mat))
      } else{
        assign(paste0(mat,"_dt"),tmp_mat)
      }
    }
  }
  
  results<-list(mat_emb_dt=mat_emb_dt,
                mat_dem_dt=mat_dem_dt,
                mat_scrap_dt=mat_scrap_dt,
                mat_int_sec_dt=mat_int_sec_dt,
                mat_prim_dt=mat_prim_dt,
                mat_sec_dt=mat_sec_dt,
                mat_prim_share_dt=mat_prim_share_dt,
                mat_sec_share_dt=mat_sec_share_dt)
  return(results)
}
