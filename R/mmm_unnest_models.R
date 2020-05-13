mmm_unnest_models = function(NMP, salesmodel, submodels){
  
  unnested_decomp = 
    salesmodel %>%
    unwind(model2 = submodels$PI, use_model2_panel = NMP) %>% 
    unwind(model2 = submodels$KBA, use_model2_panel = NMP) %>%
    unwind(model2 = submodels$PC, use_model2_panel = NMP) %>%
    unwind(model2 = submodels$OAO, use_model2_panel = NMP) %>%
    unwind(model2 = submodels$SRC, use_model2_panel = NMP) %>%
    unwind(model2 = submodels$MA, use_model2_panel = NMP)
  
  return(unnested_decomp)
}