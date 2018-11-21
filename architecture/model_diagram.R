###>Creates Data Flow Diagrams per module
library(diagram)
#matrix_function represents the inputs in matrix format. Rows are the function and columns are the inputs
matrix_function <- as.matrix(read.csv("architecture/function_architecture.csv",row.names = 1))
module_output <- list("Vehicle module"=c("fleet_mc_proj_f","fleet_fc_proj_f"),
                      "Fleet module"=c("fleet_fuel_u_f"),
                      "Automotive Material Flow module"=c("fleet_mfa_f"),
                      "LCA module"=c("fleet_lca_f","vehicle_lca_f"))
tmp_mat_f <- matrix_function
for (module in names(module_output)){
  #Outputs to consider
  output_list <- module_output[[module]]
  #module_index contains the indexes of the modules already in the matrix
  module_index <- grep("module",colnames(tmp_mat_f))
  #inv_mat_f show the depedencies of the matrix functions. Columns are the functions and rows are the dependent inputs
  inv_mat_f <- solve(diag(ncol(tmp_mat_f)-length(module_index))-t(tmp_mat_f[!grepl("module",rownames(tmp_mat_f)),!grepl("module",colnames(tmp_mat_f))]))
  if (length(output_list)>1){
    #tmp_mat_f_tp contains the matrix to plot
    tmp_mat_f_tp <- tmp_mat_f[c(which(rowSums(inv_mat_f[,output_list]) >= 1),module_index),c(which(rowSums(inv_mat_f[,output_list]) >= 1), module_index)]
    function_names <- names(which(rowSums(inv_mat_f[,output_list]) >= 1))
  } else {
    #tmp_mat_f_tp contains the matrix to plot
    tmp_mat_f_tp <- tmp_mat_f[c(which(inv_mat_f[,output_list] >= 1),module_index),c(which(inv_mat_f[,output_list] >= 1), module_index)]
    function_names <- names(which(inv_mat_f[,output_list]>= 1))
  }
  names_f_tp <- rownames(tmp_mat_f_tp)
  #inv_mat_f_tp show the depedencies of the matrix functions to plot. Columns are the functions and rows are the dependent inputs
  inv_mat_f_tp <- solve(diag(ncol(tmp_mat_f_tp)-length(module_index))-t(tmp_mat_f_tp[!grepl("module",rownames(tmp_mat_f_tp)),!grepl("module",colnames(tmp_mat_f_tp))]))
  #level_nb
  level_nb <- max(inv_mat_f_tp) + 1 
  #indirect_inputs_mat contains the indirect and idrect inputs (columnes) by function (rows)
  indirect_inputs_mat <- t(inv_mat_f_tp)-diag(nrow(inv_mat_f_tp))
  indirect_row_max <- sapply(1:nrow(indirect_inputs_mat),function(x)max(indirect_inputs_mat[x,]))
  function_order <- NULL
  #position_vector
  position_vector <- NULL
  for (level in 1:level_nb){
    function_level <- which(indirect_row_max==(level-1))
    position_vector <- c(position_vector,length(function_level))
    function_order <- c(function_order,function_level)
  }
  #Add modules
  col_order <- c(grep("module",colnames(tmp_mat_f_tp)),function_order)
  row_order <- c(grep("module",colnames(tmp_mat_f_tp)),function_order)
  position_vector[1] <- position_vector[1] + length(module_index)
  
  png(filename=paste("model_architecture/",module,"dfd.png"),width = 150*max(position_vector), height = 200*length(position_vector),units="px")
  plotmat(tmp_mat_f_tp[row_order,col_order],
          name = colnames(tmp_mat_f_tp[,col_order]), pos = position_vector, curve = 0,
          box.lwd = 2,box.cex = 0.9, box.size = 0,box.prop = 0.25, box.type = "ellipse",
          arr.width = 0.3,arr.length = 0.2,arr.pos=0.5,arr.type="triangle", endhead = FALSE,arr.lwd = 1,cex.txt=0,
          shadow.size = 0, my = 0, mx = 0,
          relsize = 1, main = module)
  dev.off()
  #Create function for following modules
  #1) Delete lines currently considered. They include
  add_col <- matrix(rowSums(tmp_mat_f[,function_names]))
  colnames(add_col) <- module
  tmp_mat_f <- cbind(tmp_mat_f,add_col)
  add_row <- matrix(0,ncol=ncol(tmp_mat_f))
  rownames(add_row) <- module
  tmp_mat_f <- rbind(tmp_mat_f,add_row)
  tmp_mat_f <- tmp_mat_f[!rownames(tmp_mat_f) %in% function_names,!rownames(tmp_mat_f) %in% function_names]
  
}
