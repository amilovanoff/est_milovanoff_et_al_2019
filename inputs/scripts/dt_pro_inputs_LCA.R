list_region<-read.csv("data/list_region.csv")
list_inputs<-list.files("LCA", pattern="inputs_reg_primary", full.names = TRUE)
# For primary ingot process
inputs<-read.csv(list_inputs[1], header=FALSE, stringsAsFactors = FALSE)
rownames(inputs)<-inputs[,1]
inputs[,1]<-NULL
colnames(inputs)<-inputs[1,]
process_name<-as.character(inputs[1,1])
inputs<-inputs[-1,]
inputs[which(rownames(inputs)=="UN-EUROPE"),2]<-"-0.694722"
inputs[which(rownames(inputs)=="UN-EUROPE"),3]<-"-0.325278"

  if (list_country[i,2]=="UN-OCEANIA"){
    inputs[i+1,2]="-1.02"
    inputs[i+1,3]="0"
  }
  if (list_country[i,2]=="CN"){
    inputs[i+1,2]="-1.02"
    inputs[i+1,3]="0"
  }
  if (list_country[i,2]=="IAI Area 1"){
    inputs[i+1,2]="-1.02"
    inputs[i+1,3]="0"
  }
  if (list_country[i,2]=="IAI Area 2, without Quebec"){
    inputs[i+1,2]="-0.8823"
    inputs[i+1,3]="-0.1377"
  }
  if (list_country[i,2]=="IAI Area 3"){
    inputs[i+1,2]="-0.728382"
    inputs[i+1,3]="-0.291618"
  }
  if (list_country[i,2]=="IAI Area 4&5 without China"){
    inputs[i+1,2]="-1.003884"
    inputs[i+1,3]="-0.016116"
  }
  if (list_country[i,2]=="IAI Area 8"){
    inputs[i+1,2]="-1.02"
    inputs[i+1,3]="0"
  }
  if (list_country[i,2]=="CA-QC"){
    inputs[i+1,2]="-1.00769"
    inputs[i+1,3]="-0.07831017"
  }
  if (list_country[i,2]=="0"){
    inputs[i+1,2]="-1.00769"
    inputs[i+1,3]="-0.07831017"
  }
}
write.csv(inputs, "inputs_primary_alu_ingot.csv")
