rc_revise_variable_name = function(Ctable, var_name = NULL, new_var_name = NULL){

  if(!(var_name %in% unlist(Ctable[,"variable"]))){
    message(str_c(var_name," ", "is not in the Ctable variable colun. No modification made to Ctable"))
  }else{
    Ctable[which(Ctable[,"variable"] == var_name),"variable"] = new_var_name
  }
  return(Ctable)
}
