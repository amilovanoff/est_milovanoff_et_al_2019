#Function that returns a dataframe to plot the ribbons and arrows between lines
##dtf is the initial dataframe
##y_axis is the variable on the Y axis
##x_axis is the variable on the x axis
##var_color is the variable the represent the different lines (color)
##ref_var let the user defines if the arrows/ribbons for each observation start with the highest observation ("reference") or following the decreasing order ("order")
arrows_ribbon_f<-function(dtf,
                          y_axis,
                          x_axis,
                          var_color,
                          ref_var="order",
                          order_var="decreasing"){
  #Parameters
  #x_axis_l contains the list of the x_axis value
  x_axis_l<-unique(dtf[,x_axis])
  #var_color_order is the order of the lines to consider. We assume the order to the last x_axis_l value order.
  if (order_var=="decreasing"){
    var_color_order<-dtf[dtf[,x_axis]==x_axis_l[length(x_axis_l)],var_color][order(dtf[dtf[,x_axis]==x_axis_l[length(x_axis_l)],y_axis],decreasing=TRUE)]
  } else if (order_var=="simulation"){
    var_color_order<-unique(dtf[,var_color])
  } else if (order_var=="increasing"){
    var_color_order<-dtf[dtf[,x_axis]==x_axis_l[length(x_axis_l)],var_color][order(dtf[dtf[,x_axis]==x_axis_l[length(x_axis_l)],y_axis],decreasing=FALSE)]
  }
  #var_color_ref is the var_color of reference. Lines and ribbons start from there
  var_color_ref<-var_color_order[1]
  for (c in 2:length(var_color_order)){
    if (ref_var=="order"){
    dtf[dtf[,var_color]==var_color_order[c],"y_start"]<-dtf[dtf[,var_color]==var_color_order[c-1],y_axis]
    } else if (ref_var=="reference"){
      dtf[dtf[,var_color]==var_color_order[c],"y_start"]<-dtf[dtf[,var_color]==var_color_ref,y_axis]
    }
    dtf[dtf[,var_color]==var_color_order[c],"y_end"]<-dtf[dtf[,var_color]==var_color_order[c],y_axis]
    dtf[dtf[,var_color]==var_color_order[c],"delta_y"]<-dtf[dtf[,var_color]==var_color_order[c],"y_end"]-dtf[dtf[,var_color]==var_color_order[c],"y_start"]
    dtf[dtf[,var_color]==var_color_order[c],"rel_delta_y"]<-dtf[dtf[,var_color]==var_color_order[c],"delta_y"]/dtf[dtf[,var_color]==var_color_order[c],"y_start"]
  }
  dtf[,var_color]<-factor(dtf[,var_color],levels=var_color_order)
  
  return(dtf)
}
