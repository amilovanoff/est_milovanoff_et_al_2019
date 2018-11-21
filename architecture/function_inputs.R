#script_inputs_f read a list of scripts and find .csv, .xlsx or list-files inputs in the scripts
script_inputs_f <- function(script_list){
  #Ouputs
  dt_col<-c("Function","Input","Type")
  input_dt<-setNames(data.frame(matrix(ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  input_lists<-NULL
  #search_cha_dt is the dataframw with characters to find in different lines
  search_cha_dt <- data.frame(cha_to_find=c("read.csv","read_excel","list.files","source","fun_res_f"),
                              start_cha=c("read.csv","read_excel","list.files","function","fun_name"),
                              end_cha=c('.csv"','.xlsx"','full.names','.R"','_f"'),
                              type=c('.csv','.xlsx','List of files','.R',".R"),
                              stringsAsFactors = FALSE)
  #f_name_list is the list of script names
  f_name_list <- substring(script_list,as.numeric(regexpr(pattern="/",script_list))+1,as.numeric(regexpr(pattern=".R",script_list))-1)
  for (i in 1:length(script_list)){
    #Function names
    f_name <- f_name_list[i]
    #Read .R file as file
    file<-file(script_list[i],open="r")
    #Convert into lines
    text_file<-readLines(file)
    close(file)
    #input_check veryfies if inputs. If not, create empty line
    input_check=FALSE
    for (j in 1:nrow(search_cha_dt)){
      cha_ts1 <- search_cha_dt[j,"cha_to_find"]
      cha_ts2 <- search_cha_dt[j,"start_cha"]
      cha_ts3 <- search_cha_dt[j,"end_cha"]
      type <- search_cha_dt[j,"type"]
      tmp_lines<-text_file[grepl(cha_ts1,text_file)&grepl(cha_ts2,text_file)&grepl(cha_ts3,text_file)]
      if (length(tmp_lines)!=0){
        #Update input table with previous list
        for (k in 1:length(tmp_lines)){
          if (grepl("paste0",tmp_lines[k])){
            #Get input and update table
            tmp_input<-substring(tmp_lines[k], as.numeric(regexpr(pattern="paste0",tmp_lines[k]))+8,as.numeric(regexpr(pattern=cha_ts3,tmp_lines[k]))+nchar(cha_ts3)-2)
            input_dt[nrow(input_dt)+1,]<-c(f_name,tmp_input,type)
          }else{
            #Get input and update table
            tmp_input<-substring(tmp_lines[k], as.numeric(regexpr(pattern=cha_ts2,tmp_lines[k]))+nchar(cha_ts2)+2,as.numeric(regexpr(pattern=cha_ts3,tmp_lines[k]))+nchar(cha_ts3)-2)
            input_dt[nrow(input_dt)+1,]<-c(f_name,tmp_input,type)
          }
        }
        input_check=TRUE
      }
    }
    #If no input, empty line
    if (input_check==FALSE){
      input_dt[nrow(input_dt)+1,]<-c(f_name,NA,NA)
    }
  }
  #Format. Delete . after _f
  input_dt$Input <- gsub("_f.R","_f",input_dt$Input)
  input_lists<-sort(unique(input_dt$Input))
  #Create matrix of scripts and inputs
  #matrix_inputs represents the inputs in matrix format. Rows are the inputs and columns are the functions
  matrix_inputs <- matrix(0,nrow = length(input_lists),ncol = length(f_name_list),dimnames = list(input_lists,f_name_list))
  for (f_name in f_name_list){
    tmp_inpu_list <- subset(input_dt,Function==f_name)$Input
    if (!NA %in% tmp_inpu_list){
      for (input in tmp_inpu_list){
        matrix_inputs[input,f_name] <- 1
      }
    }
  }
  #matrix_sources represents the source inputs in matrix format. Rows are the source functions and columns are the functions
  matrix_sources <- matrix(0,nrow = length(f_name_list),ncol = length(f_name_list),dimnames = list(f_name_list,f_name_list))
  for (f_name in f_name_list){
    tmp_inpu_list <- subset(input_dt,Type==".R" & Function==f_name & !grepl("plots/",Input))$Input
    if (!NA %in% tmp_inpu_list){
      for (input in tmp_inpu_list){
        matrix_sources[input,f_name] <- 1
      }
    }
  }
  return(list(input_dt=input_dt,matrix_inputs=matrix_inputs,matrix_sources=matrix_sources))
}

#Get list of inputs of all functions
f_list <- list.files(path="functions",pattern = "_f.R",full.names = TRUE)
function_inputs_res <- script_inputs_f(script_list=f_list)
write.csv(function_inputs_res[["input_dt"]],"architecture/function_inputs_dt.csv",row.names = FALSE,na="")
write.csv(function_inputs_res[["matrix_inputs"]],"architecture/function_inputs_matrix.csv",row.names = TRUE,na="")
write.csv(function_inputs_res[["matrix_sources"]],"architecture/function_sources_matrix.csv",row.names = TRUE,na="")

#script_list <- list.files(path="inputs/scripts/",pattern = ".R",full.names = TRUE)
#function_inputs_res <- script_inputs_f(script_list=script_list)
#write.csv(function_inputs_res[["input_dt"]],"architecture/function_inputs_dt.csv",row.names = FALSE,na="")
#write.csv(function_inputs_res[["matrix_inputs"]],"architecture/function_inputs_list.csv",row.names = FALSE,na="")


