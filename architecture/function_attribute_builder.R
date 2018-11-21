#Create a data frame with all the functions (scripts finishing with _f.R), their arguments and default values
#List all the scripts including functions
f_list<-list.files(path="functions",pattern = "_f.R",full.names = TRUE)
#Run the functions
for (i in 1:length(f_list)){
  #Run the script
  source(f_list[[i]])
}
#Extract the function names
f_names<- as.vector(lsf.str())
#Create output files
dt_col<-c("Function","Attribute","Value","Type")
f_dt<-setNames(data.frame(matrix(ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
#imp_args contains the list of all attributes that are implicit (i.e., values not updated by user)
imp_args<-c("size",
            "techno",
            "fuel_type", 
            "wgt_svg_dt", 
            "i_FC",
            "i_wgt",
            "wgt_svg", 
            "stock", 
            "rpg_mat",
            "rpd_mat", 
            "component", 
            "fun_name",
            "relative",
            "LWscen",
            "year",
            "FC_dt",
            "first_yr",
            "wgt_dt",
            "cumulative_rate",
            "scrappage_rate",
            "age",
            "atr_imp",
            "mat_cont",
            "mat_cont_LW",
            "rel_mat_cont")
#scen_args contains the list of attributes that are scenario attribute (i.e., updated by the user but correspond to scenarios. Not used in sensitivity analyses)
scen_args <- c("LWscen_tbc",
               "lw_first_yr",
               "lw_last_yr",
               "E85_scen",
               "last_yr",
               "ethanol_level_scen")
#run_args contains the list of attributes that are computational attribute (i.e., only used for computational purposes. Not used in sensitivity analyses)
run_args <- c("fast_mode")
#Loop on functions
for (f in f_names){
  #Extract attributes of function "f"
  list<-as.vector(formals(f))
  if (length(list)!=0){
    #Rows of data frame
    rows<-nrow(f_dt)+1:length(list)
    #Fill function names, and argument names
    f_dt[rows,"Function"]<-f
    f_dt[rows,"Attribute"]<-names(list)
    #For each argument, exctract the default value ("" if no value)
    if (length(names(list))>0){
      for (l in 1:length(names(list))){
        f_dt[rows[l],"Value"]<-ifelse(is.symbol(list[[l]]),"",as.character(list[[l]]))
        if(names(list)[l]%in%imp_args){
          f_dt[rows[l],"Type"]<-"Implicit"
        } else if (names(list)[l]%in%scen_args){
          f_dt[rows[l],"Type"]<-"Scenario"
        } else if (names(list)[l]%in%run_args){
          f_dt[rows[l],"Type"]<-"Run"
        } else{
          f_dt[rows[l],"Type"]<-"Explicit"
        }
      }
    }
  }
}
ex_f_dt <- data.frame(Function=unique(f_dt$Function),Attributes=sapply(unique(f_dt$Function),function(x)paste0(subset(f_dt,Function==x & Type=="Explicit")$Attribute,collapse=";")))

write.csv(f_dt,"architecture/function_attributes.csv",row.names = FALSE,na="")
write.csv(ex_f_dt,"architecture/function_explicit_attributes.csv",row.names = FALSE,na="")

#Read attribute_value
attribute_value<-read.csv("architecture/attribute_value.csv",stringsAsFactors = FALSE, check.names = FALSE)
#current_args is the list of attributes already in attribute_value
current_args <- attribute_value$Attribute
#ex_args_l is the list of attributes used in the functions
ex_args_l<-sort(unique(f_dt[f_dt$Type%in%c("Explicit","Scenario"),"Attribute"]))
#attr_list is the union of current_args and ex_args_l
attr_list <- union(current_args,ex_args_l)
#Update attribute_value with default values
for (attr in attr_list){
  #If attribute is in attribute_value, update used value with default values
  if (attr %in% current_args){
    attribute_value[attribute_value$Attribute==attr,"Value"] <- attribute_value[attribute_value$Attribute==attr,"Default"]
  } else if (!attr %in% current_args){
    attribute_value[nrow(attribute_value)+1,"Attribute"] <- attr
  }
}
#Order attribute_value by attributes name
attribute_value <- attribute_value[order(attribute_value$Attribute),]
write.csv(attribute_value,"architecture/attribute_value.csv",row.names = FALSE)

remove(list=ls())
