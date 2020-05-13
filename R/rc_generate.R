generate_response_curves = function(ABC = NULL, spend_seq = NULL){
  abc_list = as.list(ABC[1:ncol(ABC)])
  abc_list = map(abc_list, ~.x %>% set_names(rownames(ABC)))
  resp_curves = map_df(abc_list, ~.x["Adjusted A"]/(1+.x["B"]*(spend_seq/1000)^.x["C"]))

  res =
    bind_cols(Spend = spend_seq, resp_curves) %>%
    gather("MediaChannel", "Response", -Spend)
  return(res)
}
