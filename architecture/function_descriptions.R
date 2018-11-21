###>Script: Obtain the list of functions and their description. Write a .csv file.
script_list <- list.files(path="functions",pattern = "_f.R",full.names = TRUE)
#Ouputs
dt_col<-c("Function","Description")
description_dt<-setNames(data.frame(matrix(ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
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
  tmp_lines<-text_file[grepl("###>",text_file)]
  if (length(tmp_lines)!=0){
    tmp_desc <- substring(tmp_lines, 5,nchar(tmp_lines))
    description_dt[nrow(description_dt)+1,]<-c(f_name,tmp_desc)
  } else {
    description_dt[nrow(description_dt)+1,]<-c(f_name,NA)
  }
}
write.csv(description_dt,"architecture/function_descriptions.csv",row.names = FALSE,na="")
