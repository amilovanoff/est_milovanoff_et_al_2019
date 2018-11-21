#Sources
source("functions/fleet_frv_f.R",local=TRUE)
if (exists("frv_dt")){rm(list="frv_dt")}
for (frv_pwt_r in c(0,0.5,1)){
  fleet_frv_f_res<-do.call(fleet_frv_f,list(frv_impro="Med",frv_pwt_r=frv_pwt_r,fast_mode="y"))
  fleet_frv_f_res[,"Powertrain resizing"]<-frv_pwt_r
  if (exists("frv_dt")){
    frv_dt<-rbind(frv_dt,fleet_frv_f_res)
  } else {
    frv_dt<-fleet_frv_f_res
  }
}
write.csv(frv_dt,"data/fleet_frv_stat.csv",row.names = FALSE)
