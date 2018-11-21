###>Function: Update the fuel consumptions of a vehicle type (i.e., size and technology) from year-1 and year by including technological changes and weight changes
fc_upgrade_f<-function(size,
                       techno,
                       fuel_type, 
                       year,
                       i_FC,
                       wgt_svg,
                       i_wgt,
                       fc_impro=NA,
                       frv_impro=NA,
                       frv_pwt_r=NA,
                       frv_src=NA){
  #Source
  source("architecture/attribute_f.R",local=TRUE)
  attribute_f("fc_upgrade_f")
  #Source
  source("functions/frv_kim_f.R",local=TRUE)
  source("functions/fc_impro_f.R",local=TRUE)
  #Call frv_kim_f: Obtain FRV
  frv_kim_f_args<-list(year=year,
                       size=size,
                       techno=techno,
                       fuel_type=fuel_type,
                       i_FC=i_FC,
                       i_wgt=i_wgt,
                       frv_impro=frv_impro,
                       frv_pwt_r=frv_pwt_r)
  FRV <- do.call(frv_kim_f,frv_kim_f_args)
  #Obtain the FC changes
  delta_fc <- do.call(fc_impro_f,list(size = size,techno = techno,fuel_type = fuel_type,year=year,fc_impro=fc_impro))
  #Calculate new fuel consumption: Add delta FC and substract weight related fuel consumption savings
  new_fc <- i_FC+delta_fc+wgt_svg*FRV/100
  return(list(f_FC=new_fc,
              FRV=FRV,
              fc_annual_changes=delta_fc))
  }
