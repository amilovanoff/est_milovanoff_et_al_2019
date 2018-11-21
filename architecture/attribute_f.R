attribute_f<-function(fun_name){
  #Define parent environment as environment
  e=parent.frame()
  #Input files
  function_attributes<-read.csv("architecture/function_attributes.csv",stringsAsFactors = FALSE,check.names = FALSE,na.strings = "")
  attribute_value<-read.csv("architecture/attribute_value.csv",stringsAsFactors = FALSE,check.names = FALSE)
  #attr_to_update is the list of attributes of function fun_name to update
  attr_to_update <- unique(subset(function_attributes,Function==fun_name & Type%in%c("Explicit","Scenario"))$Attribute)
  #Check if arguments, Otherwise nothing
  if (length(attr_to_update)!=0){
    #Loop on the arguments
    for (args in attr_to_update){
      #Check if the value is already assigned in the parent environment. If yes, no assignment.
      if (is.na(get0(args,envir = e,ifnotfound = NA))){
        #If argument is numeric. Convert in numeric
        if (attribute_value$Type[attribute_value$Attribute==args] == "num"){
          arg_val<-as.numeric(attribute_value$`Value`[attribute_value$Attribute==args])
        } else {
          arg_val<-attribute_value$`Value`[attribute_value$Attribute==args]
        }
      assign(args,arg_val, envir = e)
      }
    }
  }
}

fun_res_f<-function(fun_name=NA,fast_mode="n"){
  #Environmental setup
  ##If the environment which contains the functions' results does not exist, it creates it.
  if (!exists("fun.res.env",where = .GlobalEnv)){
    assign("fun.res.env",new.env(),envir = .GlobalEnv)
  }
  #Check if the results of the function are not alread in fun.res.env
  if (exists(paste0(fun_name,"_res"),where = fun.res.env)){
    #Get the results of the function from fun.res.env
    fun_res <- get(paste0(fun_name,"_res"), envir = fun.res.env)
  } else {
    if (fast_mode=="y"){
      #Load default outputs. It is not in good format, extract the format and assign to good names in fun.res.env
      load(paste0("outputs/out_def/",fun_name,"_def.RData"),envir = fun.res.env)
      temp_res <- get("results_l",envir = fun.res.env)[["Scenario:Default"]]
      remove("results_l",envir = fun.res.env)
      assign(paste0(fun_name,"_res"),temp_res[which(names(temp_res)!="Scenario")],envir = fun.res.env)
      fun_res <- get(paste0(fun_name,"_res"), envir = fun.res.env)
    } else {
      #Read the function, generate the results and assign it in fun.res.env
      source(paste0("functions/",fun_name,".R"),local=TRUE)
      fun_res <- do.call(get(fun_name),list())
      assign(paste0(fun_name,"_res"),fun_res,envir = fun.res.env)
    }
  }
  return(fun_res)
}
