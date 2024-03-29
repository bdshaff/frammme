create_salesmodel_script = function(NMP = NULL, FiscalYear = NULL, modeling_start_date = NULL, modeling_end_date = NULL){

  nmps = nmp_dispatch(NMP)
  nmp = nmps$nmp
  NAMEPLATE = nmps$NAMEPLATE

  file_path = str_c("modeling/SalesModels/",NMP,"/",NAMEPLATE,"-",FiscalYear,".R")

  print(file_path)
  file.create(file_path)

  script_header =
    str_c('\n  NAMEPLATE = \"', NAMEPLATE ,'\"
         \n  NMP = \"', NMP, '\"
         \n  nmp = \"', nmp, '\"
         \n  rgn = c("CR","MAR","MTN","MWR","NER","NWR","SER","WR")
         \n  modeling_start_date = \"', modeling_start_date, '\"
         \n  modeling_end_date = \"', modeling_end_date, '\"
         \n  FiscalPeriod = \"', FiscalYear, '\"\n')

  script_body = '

  out_dest_decomp = here::here("modeling","SalesModels", NMP, str_c(NAMEPLATE,"-",FiscalPeriod,"-Decomp.csv"))
  out_dest_undecomp = here::here("output","SalesModels", NMP, str_c(NAMEPLATE,"-",FiscalPeriod,"-UnnestedDecomp.Rds"))

  in_data_fmi = here::here("data","processed_data", "FMI.csv")
  in_data_kpi = here::here("data","processed_data", "KPI.csv")
  in_var_cat = here::here("data","categorization","StandardizedVariableCategorization.xlsx")


  ##############################################################
  ########################### START ############################
  ##############################################################

  print("")
  message(str_c("1 - ", NAMEPLATE, " SALES MODEL SCRIPT RUN INITIATED"))
  options(scipen = 999, digits = 4)

  ##############################################################
  ######################## LOADING DATA ########################
  ##############################################################

  mmm_data = mmm_load_data(nmp, rgn, in_data_fmi, in_data_kpi)

  FMI = mmm_data$FMI
  KPI = mmm_data$KPI
  fmi_dataP = mmm_data$fmi_dataP
  nonfmi_dataP = mmm_data$nonfmi_dataP

  ##############################################################
  ################## DEFINE THE MODEL STRING ###################
  ##############################################################

  stringformatPanel_Div = readRDS(here::here("modeling","ModelStrings",str_c(nmp,"-model-string.Rds")))

  message(str_c("6 - ", NAMEPLATE, "  Model String Defined AS:"))
  print(stringformatPanel_Div)

  ##############################################################
  ########### DATA TRANSFORMATIONS  VIA StringGen ##############
  ##############################################################

  DF_Panel_Div =
    StringGen(model_string = stringformatPanel_Div,
              media_data = fmi_dataP,
              nonmedia_data = nonfmi_dataP,
              Panel = 8,
              start_date = modeling_start_date,
              end_date = modeling_end_date)

  message(str_c("7 - ", NAMEPLATE, "  - StringGen Ran"))

  ##############################################################
  ################## FIT MODEL VIA RunModel ####################
  ##############################################################

  model_Div =
    RunModel(Formula = stringformatPanel_Div,
             DF = DF_Panel_Div,
             Panel = 8)

  message(str_c("8 - ", NAMEPLATE, "  - model_Div Ran"))

  DF_Panel_Div = OutResidDMA(DF_Panel_Div, model_Div)

  stringformatPanel_Div = paste(stringformatPanel_Div,
                                "residualdum_dma",
                                sep = " + ")

  ##############################################################
  ############### EXPORT REGRESSION COEFFICIENTS ###############
  ##############################################################

  setwd(here::here("modeling","SalesModels", NMP))
  exportREGCoeff(Formula = stringformatPanel_Div,
                 DF = DF_Panel_Div,
                 Panel = 8,
                 model = model_Div,
                 nameDF = str_c(NAMEPLATE, "-", FiscalPeriod),
                 kpi_var = str_c(nmp,"_sales"))

  ##############################################################
  ################### COMPUTE CONTRIBUTIONS ####################
  ##############################################################

  Sales_div = contributionsADD(reg_name = str_c(NAMEPLATE, "-", FiscalPeriod),
                               cat_table = in_var_cat,
                               aggregation_period = "FiscalYear",
                               FY_Begins = 4,
                               min_ref = c(),
                               mean_ref = c(),
                               TIV = TRUE,
                               start_date = modeling_start_date)
  setwd(here::here())

  message(str_c("9 - ", NAMEPLATE, "  Decomp Computed"))

  Sales_div$varContPct = Sales_div$varContPct[,-ncol(Sales_div$varContPct)]

  ##############################################################
  ######################## SUB MODELS ##########################
  ##############################################################

  submodels = mmm_load_submodels(nmp)
  message(str_c("10 - ", NAMEPLATE, " - Sub Models Loaded"))

  ##############################################################
  ######################## UNNESTING ###########################
  ##############################################################

  unnested_decomp = mmm_unnest_models(NMP, salesmodel = Sales_div, submodels = submodels)
  unnested_decomp_contrib = unnested_decomp[[1]]
  message(str_c("11 - ", NAMEPLATE, "  Decomp Unnested"))

  ##############################################################
  ################## EXPORT AND SAVE RESULTS ###################
  ##############################################################

  write_csv(unnested_decomp_contrib, out_dest_decomp)
  message(str_c("12 - ", NAMEPLATE, "  - Unnested Decomp Exported To:"))
  print(out_dest_decomp)

  saveRDS(unnested_decomp, out_dest_undecomp)
  message(str_c("13 - ", NAMEPLATE, "  Unnested Decomp Saved To:"))
  print(out_dest_undecomp)

  ##############################################################
  ########################### END ##############################
  ##############################################################
  '
  script = str_c(script_header, script_body)
  write(script, file = file_path, append = TRUE)
}
