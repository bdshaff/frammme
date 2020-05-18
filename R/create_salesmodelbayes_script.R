create_salesmodelbayes_script = function(NMP = NULL, FiscalYear = NULL, modeling_start_date = NULL, modeling_end_date = NULL){
  nmps = nmp_dispatch(NMP)
  nmp = nmps$nmp
  NAMEPLATE = nmps$NAMEPLATE

  file_path = str_c("modeling/SalesModels/",NMP,"-Bayes/",NAMEPLATE,"-",FiscalYear,"-Bayes.R")

  print(file_path)
  file.create(file_path)

  script_header =
    str_c('\n  NAMEPLATE = \"', NAMEPLATE ,'\"
         \n  NMP = \"', NMP, '\"
         \n  nmp = \"', nmp, '\"
         \n  rgn = c("CR","MAR","MTN","MWR","NER","NWR","SER","WR")
         \n  FiscalPeriod = \"', FiscalYear, '\"\n')

  script_body = '
input_file_ModelData = here::here("data","processed_data","ModelData.xlsx")
input_file_ModelFitCurves = here::here("data","fit_curves","Fit_Curves.xlsx")

input_file_ModelSetup = here::here("modeling","SalesModels",str_c(NMP,"-Bayes"),"input",str_c(nmp,"-ModelSetup.csv"))
input_file_ModelSpec = here::here("modeling","SalesModels",str_c(NMP,"-Bayes"),"input",str_c(nmp,"-ModelSpec.csv"))

out_dest_mod_obj = here::here("output","SalesModels",str_c(NMP,"-Bayes"),str_c(NAMEPLATE,"-",FiscalPeriod,"-mod_obj.Rds"))

##############################################################
########################### START ############################
##############################################################

# line by line
mod_obj = create_mod_obj()
mod_obj = add_group_selector(mod_obj, vehicles = nmp, regions = rgn)
mod_obj = activate_model_setup(mod_obj, input_file_ModelSetup)
mod_obj = activate_model_spec(mod_obj, input_file_ModelSpec)
mod_obj = Load_FitCurves(mod_obj, input_file_ModelFitCurves)
mod_obj = Load_Data(mod_obj, input_file_ModelData)
mod_obj = Transform(mod_obj, print = FALSE)
mod_obj = SetDateRange(mod_obj)
mod_obj = Run_Model(mod_obj)

# or tidy-style
mod_obj =
  create_mod_obj() %>%
  add_group_selector(vehicles = nmp, regions = rgn) %>%
  activate_model_setup(input_file_ModelSetup) %>%
  activate_model_spec(input_file_ModelSpec) %>%
  Load_FitCurves(input_file_ModelFitCurves) %>%
  Load_Data(input_file_ModelData) %>%
  Transform(print = FALSE) %>%
  SetDateRange() %>%
  Run_Model()


##############################################################
################## EXPORT AND SAVE RESULTS ###################
##############################################################

saveRDS(mod_obj, out_dest_mod_obj)

##############################################################
########################### END ##############################
##############################################################
  '
  script = str_c(script_header, script_body)
  write(script, file = file_path, append = TRUE)
  dir.create(str_c("modeling/SalesModels/",NMP,"-Bayes/input"))
}
