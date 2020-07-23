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
input_file_ModelFitCurves <- here::here("data", "fit_curves", "Fit_Curves.xlsx")
input_file_ModelData <- here::here("data", "processed_data", "ModelData.xlsx")
input_file_FMIData <- here::here("data", "processed_data", "FMI.csv")
input_file_SpendData <- here::here("data", "roi_app_data", "daily_spend_data.csv")
input_file_MSRPData <- here::here("data/roi_app_data/MSRP.csv")

input_files <-
  list(
    input_file_ModelData = input_file_ModelData,
    input_file_FMIData = input_file_FMIData,
    input_file_SpendData = input_file_SpendData,
    input_file_MSRPData = input_file_MSRPData
  )

input_file_ModelSetup <- here::here("modeling", "SalesModels", str_c(NMP, "-Bayes"), "input", str_c(nmp, "-ModelSetup.csv"))
input_file_ModelSpec <- here::here("modeling", "SalesModels", str_c(NMP, "-Bayes"), "input", str_c(nmp, "-ModelSpec.csv"))

out_dest_mod_obj <- here::here("output", "SalesModels", str_c(NMP, "-Bayes"), str_c(NAMEPLATE, "-", FiscalPeriod, "-mod_obj.Rds"))

##############################################################
# Define the Dummy Variable Function
##############################################################

Add_Dummy <- function(mod_obj) {
  cs <- mod_obj$cs
  ts <- mod_obj$Time

  n_cols_before <- ncol(mod_obj$data)

  mod_obj$data <-
    mod_obj$data %>%
    mutate(
      DUM_2017_11_01 = if_else(!!sym(ts) == "2017-11-01", 1, 0),
      DUM_2018_01_01 = if_else(!!sym(ts) == "2018-01-01", 1, 0),
      DUM_2017_05_01 = if_else(!!sym(ts) == "2017-05-01", 1, 0),
      DUM_2018_03_01 = if_else(!!sym(ts) == "2018-03-01", 1, 0),
      DUM_2018_09_01 = if_else(!!sym(ts) == "2018-09-01", 1, 0)
    )

  n_added_cols <- ncol(mod_obj$data) - n_cols_before
  n_cols_after <- ncol(mod_obj$data)
  added_dummy_cols <- names(mod_obj$data)[(n_cols_after - (n_added_cols - 1)):n_cols_after]

  dummy_spec <-
    data.frame(added_dummy_cols, added_dummy_cols, "Base", "Dummy", 1, "None", 0, 1) %>%
    set_colnames(names(mod_obj$spec)[c(1:6, 17:18)])

  mod_obj$spec <-
    bind_rows(mod_obj$spec, dummy_spec)

  return(mod_obj)
}

##############################################################
########################### START ############################
##############################################################

mod_obj <- create_mod_obj()
mod_obj <- add_group_selector(mod_obj, vehicles = nmp, regions = rgn)
mod_obj <- activate_model_setup(mod_obj, input_file_ModelSetup)
mod_obj <- activate_model_spec(mod_obj, input_file_ModelSpec)
mod_obj <- Load_FitCurves(mod_obj, input_file_ModelFitCurves)
mod_obj <- Load_Data(mod_obj, input_files)
mod_obj <- Transform(mod_obj, print = TRUE)
mod_obj <- Set_Date_Range(mod_obj)
mod_obj <- Add_Dummy(mod_obj)
mod_obj <- Run_Model_Panel(mod_obj)
mod_obj <- Decomp(mod_obj)

##############################################################
################ LOAD SUBMODELS AND UNNEST ###################
##############################################################

submodels <- mmm_load_submodels(mod_obj$nmp)
unnested_decomp <- mmm_unnest_models(NMP = mod_obj$NMP, salesmodel = mod_obj$decomp_list, submodels = submodels)
mod_obj$unnested_decomp_list <- unnested_decomp

##############################################################
########################### RC ###############################
##############################################################

Ctable <-
  mod_obj$unnested_decomp_list$varCont %>%
  filter(Categories != "Date") %>%
  pivot_longer(names_to = "Date", values_to = "Value", 5:ncol(.)) %>%
  mutate(
    Date = ymd(Date),
    FiscalYear = FY(Date, FYBegin = 4)
  ) %>%
  group_by(variable, Categories, Model, Group, FiscalYear) %>%
  summarise(Value = sum(Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = FiscalYear, values_from = Value) %>%
  ungroup() %>%
  filter(!(grepl("PANEL", variable) & !grepl(mod_obj$NMP, variable))) %>%
  mutate(Categories = case_when(
    Categories %in% c("TV T1", "TV T1 MBR") ~ "TV T1",
    Categories %in% c("TV T2 EVENT", "TV T2 PRODUCT") ~ "TV T2",
    Categories %in% c("DIGITAL TOTAL T1", "DIGITAL TOTAL T1 MBR", "DIGITAL T1") ~ "Digital Display T1",
    Categories %in% c("DIGITAL TOTAL T2 MBR", "DIGITAL TOTAL T2", "DIGITAL T2") ~ "Digital T2",
    Categories %in% c("STREAMING T1", "STREAMING T1 MBR") ~ "Streaming T1",
    Categories %in% c("SEARCH") ~ "Search T1",
    Categories %in% c("RADIO T2") ~ "Radio T2",
    Categories %in% c("ADDRESSABLE TV T1") ~ "Addressable TV T1",
    Categories %in% c("SOCIAL DISPLAY T1", "SOCIAL VIDEO T1") ~ "Social T1",
    Categories %in% c("SOCIAL DISPLAY T2", "SOCIAL DISPLAY T2 MBR") ~ "Social T2",
    Categories %in% c("MAGAZINE T1") ~ "Print",
    TRUE ~ Categories
  ))

ABClist <-
  ABCsReachGen(
    Contrib = Ctable,
    fmi_data = mod_obj$Ftable,
    spend = mod_obj$Stable,
    multiplier = pull(filter(mod_obj$Mtable, FiscalPeriod == "FY18"), MSRP),
    fit = fit,
    fit_displayt1 = fit_displayt1,
    fit_displayt2 = fit_displayt2,
    fit_st = fit_st,
    fit_addressable = fit_addressable,
    panel = mod_obj$nmp,
    year = "2019",
    From = as.character(mod_obj$EndDate - 721),
    To = as.character(mod_obj$EndDate)
  )

mod_obj$ABClist <- ABClist

##############################################################
################## Media Mix Optimization ####################
##############################################################

mod_obj <- Media_Mix(mod_obj, total_budget = 81213085, incremental = 100000, interactive = FALSE)

##############################################################
############### EXPORT AND SAVE MODEL OBJECT #################
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
