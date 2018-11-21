#This script applies the Kastner's model for primary aluminum products
mfa_prim_alu_f<-function(mfa_alu_proj=NA,
                         last_yr=NA,
                         country=NA,
                         fast_mode="n"){
  #Assign arguments default values
  source("architecture/attribute_f.R",local=TRUE)
  attribute_f("mfa_prim_alu_f")
  #Inputs
  load("outputs/out_def/mfa_prim_alu_hist_f_def.RData")
  list_country<-read.csv("inputs/primary_alu/list_country.csv", stringsAsFactors = FALSE)
  #Other parameters
  cons_vec<-matrix(0,ncol=nrow(list_country), nrow=1)
  colnames(cons_vec)<-unique(list_country$Country)
  cons_vec[,country]<-1
  hist_years<-as.numeric(names(mfa_prim_alu_hist))
  proj_years<-(max(as.numeric(names(mfa_prim_alu_hist)))+1):last_yr
  #alu_cons_reg_hist contains the mix of producing region for consumption in country for historical data
  alu_cons_reg_hist<-data.frame(matrix(0,ncol = 0, nrow = length(unique(list_country$Region))),stringsAsFactors = FALSE,check.names = FALSE)
  rownames(alu_cons_reg_hist)<-unique(list_country$Region)
  for (year in hist_years){
    prod_mat<-as.matrix(mfa_prim_alu_hist[[as.character(year)]])
    alu_cons_reg_hist<-cbind(alu_cons_reg_hist,setNames(as.data.frame(t(cons_vec %*% prod_mat)),as.character(year)))
  }
  #Output file
  alu_cons_reg<-alu_cons_reg_hist
  if (mfa_alu_proj=="constant"){
    for (y in proj_years){
      alu_cons_reg[,as.character(y)]<-alu_cons_reg[,as.character(y-1)]
    }
  } else if(mfa_alu_proj%in%c("trends","local")){
    if (mfa_alu_proj=="local"){
      trend_reg<-list_country$Region[list_country$Country==country]
      #Assumption: In local scenario, we assume a 1% annual increase in local contribution of the region
      trends_dt<-matrix(0,ncol=1,nrow=nrow(alu_cons_reg_hist))
      rownames(trends_dt)<-rownames(alu_cons_reg_hist)
      trends_dt[trend_reg,1]<-0.01
    } else if (mfa_alu_proj=="trends"){
      trends_dt<-as.matrix((alu_cons_reg_hist[,as.character(hist_years[length(hist_years)])]-alu_cons_reg_hist[,as.character(hist_years[1])])/(hist_years[length(hist_years)]-hist_years[1]))
      rownames(trends_dt)<-rownames(alu_cons_reg_hist)
      #Assumption: The three higher linear trends are considered
      trend_reg<-rownames(trends_dt)[order(abs(trends_dt),decreasing=TRUE)[1:3]]
      trends_dt[!rownames(trends_dt)%in%trend_reg,]<-0
    }
    for (y in proj_years){
      ##Assume annual linear trends for trend_reg as long as values are between 0 and 1. Otherwise, keep constant
      if (all(alu_cons_reg[,as.character(y-1)]+trends_dt>=0)&all(alu_cons_reg[,as.character(y-1)]+trends_dt<=1)){
        alu_cons_reg[,as.character(y)]<-alu_cons_reg[,as.character(y-1)]+trends_dt
      } else {
        alu_cons_reg[,as.character(y)]<-alu_cons_reg[,as.character(y-1)]
      }
    #Adjust other regions: Assumption: other regions keep the same distribution.
    alu_cons_reg[!rownames(alu_cons_reg)%in%trend_reg,as.character(y)]<-alu_cons_reg[!rownames(alu_cons_reg)%in%trend_reg,as.character(y-1)]/(1-sum(alu_cons_reg[trend_reg,as.character(y-1)]))*(1-sum(alu_cons_reg[trend_reg,as.character(y)]))
    }
  }
  return(list(alu_cons_reg=alu_cons_reg))
}

