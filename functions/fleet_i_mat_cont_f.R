###>Function: Builds the material composition  of the vehicles by size and technology type for all sucomponents
fleet_i_mat_cont_f <- function (mat_cont_adj=NA,
                                rel_mat_cont = "n",
                                fast_mode="n") {
  #Assign default values
  source("architecture/attribute_f.R",local = TRUE)
  attribute_f(fun_name="fleet_i_mat_cont_f")
  #Input files
  ori_mat_comp <-read.csv("inputs/component_material_comp_ACSM.csv",stringsAsFactors = FALSE,check.names = FALSE)
  adj_mat_comp <-read.csv("inputs/component_material_comp_adj.csv",stringsAsFactors = FALSE,check.names = FALSE)
  material <-read.csv("inputs/material_equivalency.csv",stringsAsFactors = FALSE,check.names = FALSE)
  vh_techno <-read.csv("inputs/vehicle_technology.csv",stringsAsFactors = FALSE,check.names = FALSE)
  comp_dt <-read.csv("inputs/component_equivalency.csv",stringsAsFactors = FALSE,check.names = FALSE)
  #Function inputs
  fleet_i_comp_wgt_f_res <- do.call(fun_res_f,list(fun_name="fleet_i_comp_wgt_f",fast_mode=fast_mode))
  fleet_subcompo_wgt <- fleet_i_comp_wgt_f_res[["fleet_subcompo_wgt"]]
  #Other parameters
  #Extract the relative weight of the considered materials in the component
  if (mat_cont_adj=="y"){
    mat_comp<-adj_mat_comp
  } else {
    mat_comp<-ori_mat_comp
  }
  #Creation output file
  dt_col <- c("Size","Technology","Component","Subcomponent","Material","Weight")
  fleet_mat_cont <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  for (size in c("Car","Light truck")){
    for (techno in unique(vh_techno$own)){
      #tmp_techno is used for mat_comp (for BEV and PHEV)
      if (grepl("BEV",techno) | grepl("PHEV",techno)){
        range <- subset(vh_techno,own==techno)$Range
        tmp_techno <- substring(techno,0,as.numeric(regexpr(pattern="[[:digit:]]{1}",techno))-1)
      } else {
        range<-NA
        tmp_techno <- techno
      }
      #subcomponent is the list of subcomponents in veh
      subcomponent <- setdiff(unique(subset(fleet_subcompo_wgt,Size==size & Technology==techno)[,"Subcomponent"]),"Total")
      for (subcomp in subcomponent){
        component <- subset(comp_dt,`Own subcomponent`==subcomp)[,"Own component"]
        row_tbc <- which((sapply(1:nrow(mat_comp), function(x) tmp_techno %in% unlist(strsplit(mat_comp$Technology[x],","))) | mat_comp$Technology=="Glo") & mat_comp$Subcomponent==subcomp)
        for (mat in unique(material$Own)) {
          #mat_compo is the material composition of mat in subcomp (in weight if relative is no, in % if relative is year)
          mat_compo <- subset(fleet_subcompo_wgt,Size==size & Technology==techno & Subcomponent==subcomp)[,ifelse(rel_mat_cont=="y","Relative","Weight")]*mat_comp[row_tbc,mat]
          #Fill output file
          fleet_mat_cont[nrow(fleet_mat_cont)+1,] <- c(size,techno,component,subcomp,mat,mat_compo)
        }
      }
    }
  }
  #Format
  fleet_mat_cont$Weight <- as.numeric(fleet_mat_cont$Weight)
  agg.formula <- reformulate(termlabels = setdiff(colnames(fleet_mat_cont),c("Component","Subcomponent","Weight")),response = "Weight")
  fleet_i_mc <- aggregate(data = fleet_mat_cont,agg.formula,FUN=sum)
  return(list(fleet_mat_cont=fleet_mat_cont,fleet_i_mc=fleet_i_mc))
}

