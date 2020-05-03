# 6) Defensive programming: supressing unnecessary messages/warings, adding useful messages/warnings and errors
# 7) testing and asserting


create_submodel_script = function(SBM = NULL, NMP = NULL, FiscalYear = NULL, modeling_start_date = NULL, modeling_end_date = NULL){
  if(NMP == "TTN"){
    NAMEPLATE = "Titan"
    nmp = "ttn"
  }else if(NMP == "VER"){
    NAMEPLATE = "Versa"
    nmp = "ver"
  }else if(NMP == "ALT"){
    NAMEPLATE = "Altima"
    nmp = "alt"
  }else if(NMP == "ARM"){
    NAMEPLATE = "Armada"
    nmp = "arm"
  }else if(NMP == "FRO"){
    NAMEPLATE = "Frontier"
    nmp = "fro"
  }else if(NMP == "LEF"){
    NAMEPLATE = "LEAF"
    nmp = "lef"
  }else if(NMP == "RGE"){
    NAMEPLATE = "ROGUE"
    nmp = "rge"
  }else if(NMP == "RGS"){
    NAMEPLATE = "RogueSport"
    nmp = "rgs"
  }else if(NMP == "MUR"){
    NAMEPLATE = "Murano"
    nmp = "mur"
  }else if(NMP == "MAX"){
    NAMEPLATE = "Maxima"
    nmp = "max"
  }else if(NMP == "PTH"){
    NAMEPLATE = "Pathfinder"
    nmp = "pth"
  }else if(NMP == "SEN"){
    NAMEPLATE = "Sentra"
    nmp = "sen"
  }else if(NMP == "NV"){
    NAMEPLATE = "NV"
    nmp = "nv"
  }else if(NMP == "ALL"){
    NAMEPLATE = "ALL"
    nmp = "all"
  }else{
    print(NMP)
    message("need to configure")
  }

  file_path = str_c("modeling/SubModels/", SBM,"/", FiscalYear,"-AllModels","/", SBM,"-",FiscalYear,".R")

  print(file_path)
  file.create(file_path)

  script_header =
    str_c('\n  NAMEPLATE = \"', NAMEPLATE ,'\"
         \n  NMP = \"', NMP, '\"
         \n  SBM = \"', SBM, '\"
         \n  nmp = c("alt", "arm", "fro", "lef", "max", "mur", "pth", "rge", "sen", "ttn", "ver")
         \n  rgn = \"100\"
         \n  modeling_start_date = \"', modeling_start_date, '\"
         \n  modeling_end_date = \"', modeling_end_date, '\"
         \n  FiscalPeriod = \"', FiscalYear, '\"\n')

  script_body = '

  out_dest_contrib_table = here::here("modeling","SubModels",SBM,str_c(FiscalPeriod,"-AllModels"),str_c(SBM,"-",FiscalPeriod,"-Decomp.csv"))
  out_dest_contrib = here::here("output","SubModels",SBM,"all-SRC.Rds")
  in_data_fmi = here::here("data","processed_data", "2020-02-27_FY18P6NissanModelingFMI.csv")
  in_data_kpi = here::here("data","processed_data", "2020-03-02_FY18P6NissanModelingKPI.csv")

  ##############################################################
  ########################### START ############################
  ##############################################################


  print("")
  message("1 - SEARCH SUB MODEL SCRIPT RUN INITIATED")

  options(scipen = 999, digits = 4)

  ##############################################################
  ######################## LOADING DATA ########################
  ##############################################################

  modeled = c("alt", "arm", "fro",
              "lef", "max", "mur",
              "pth", "rge", "sen",
              "ttn", "ver")


  mmm_data = mmm_load_data(nmp, rgn, in_data_fmi, in_data_kpi)
  FMI = mmm_data$FMI
  KPI = mmm_data$KPI
  fmi_dataP = mmm_data$fmi_dataP
  nonfmi_dataP = mmm_data$nonfmi_dataP

  ##############################################################
  ################## DEFINE THE MODEL STRING ###################
  ##############################################################


  stringformatPanel_Div = str_squish("{search<D>search_MEANSBY}_Log ~
                                         aa_Log_Lag1 +
                                         ma_Log_Lag4 +
                                         total_digital_direct_t1_E1R1P56D10 +
                                         social_display_direct_t1_E1R1P56D10 +
                                         tv_direct_t1_E1R0P56D10 +
                                         digital_totalmbr_direct_t2_E6R6P56D10 +
                                         addressabletv_direct_t1_E4R4P07D10 +
                                         DUM_2017_03_01 +
                                         DUM_2019_02_01 +
                                         mbr_tv_direct_t1_E4R4P42D10 +
                                         mbr_streaming_direct_t1_E4R4P56D10 +
                                         social_display_direct_t2_E8R4P07D10")


  message("7 - Model String Defined AS:")
  print(stringformatPanel_Div)


  ##############################################################
  ############ DATA TRANDSORMATIONS VIA StringGen ##############
  ##############################################################

  DF_Panel_Div =
  StringGen(model_string = stringformatPanel_Div,
            media_data = fmi_dataP,
            nonmedia_data = nonfmi_dataP,
            Panel = 11,
            start_date = modeling_start_date,
            end_date = modeling_end_date)

  message("8 - StringGen Ran")

  ##############################################################
  ################## FIT MODEL VIA RunModel ####################
  ##############################################################

  model_Div = RunModel(Formula = stringformatPanel_Div,
                       DF = DF_Panel_Div,
                       Panel = 11)

  message("9 - model_Div Ran")

  ##############################################################
  ############### EXPORT REGESSION COEFFICIENTS ################
  ##############################################################

  setwd(here::here("modeling","SubModels",SBM, str_c(FiscalPeriod,"-AllModels")))
  exportREG(Formula = stringformatPanel_Div,
              DF = DF_Panel_Div,
              Panel = 11,
              model = model_Div,
              nameDF = str_c(SBM,"-",FiscalPeriod))


  ##############################################################
  ################### COMPUTE CONTRIBUTIONS ####################
  ##############################################################

  contrib = contributions(reg_name =  str_c(SBM,"-",FiscalPeriod),
                          cat_table = here::here("data","categorization","StandardizedVariableCategorization.xlsx"),
                          aggregation_period = "FiscalYear",
                          FY_Begins = 4,
                          min_ref = c(11,12),
                          mean_ref = c())
   setwd(here::here())

   contrib_table = contrib[[1]]

   message("10 - Decomp Computed")


  ##############################################################
  ################## EXPORT AND SAVE RESULTS ###################
  ##############################################################

  write_csv(contrib_table, out_dest_contrib_table)
  message("11 - Decomp Exported To:")
  print(out_dest_contrib_table)

  #ONLY IF YOU HAVE AN ADDITIONAL NA COLUMN IN VARPCNT
  contrib$varContPct = contrib$varContPct[,-ncol(contrib$varContPct)]
  saveRDS(contrib, out_dest_contrib)
  message("12 - Submodel Contributions Saved To:")
  print(out_dest_contrib)
  '

  script = str_c(script_header, script_body)
  write(script, file = file_path, append = TRUE)

}

create_salesmodel_script = function(NMP = NULL, FiscalYear = NULL, modeling_start_date = NULL, modeling_end_date = NULL){
  if(NMP == "TTN"){
    NAMEPLATE = "Titan"
    nmp = "ttn"
  }else if(NMP == "VER"){
    NAMEPLATE = "Versa"
    nmp = "ver"
  }else if(NMP == "ALT"){
    NAMEPLATE = "Altima"
    nmp = "alt"
  }else if(NMP == "ARM"){
    NAMEPLATE = "Armada"
    nmp = "arm"
  }else if(NMP == "FRO"){
    NAMEPLATE = "Frontier"
    nmp = "fro"
  }else if(NMP == "LEF"){
    NAMEPLATE = "LEAF"
    nmp = "lef"
  }else if(NMP == "RGE"){
    NAMEPLATE = "ROGUE"
    nmp = "rge"
  }else if(NMP == "RGS"){
    NAMEPLATE = "RogueSport"
    nmp = "rgs"
  }else if(NMP == "MUR"){
    NAMEPLATE = "Murano"
    nmp = "mur"
  }else if(NMP == "MAX"){
    NAMEPLATE = "Maxima"
    nmp = "max"
  }else if(NMP == "PTH"){
    NAMEPLATE = "Pathfinder"
    nmp = "pth"
  }else if(NMP == "SEN"){
    NAMEPLATE = "Sentra"
    nmp = "sen"
  }else if(NMP == "NV"){
    NAMEPLATE = "NV"
    nmp = "nv"
  }else{
    print(NMP)
    message("need to configure")
  }

  file_path = str_c("modeling/SalesModels/",NMP,"/",NAMEPLATE,"-",FiscalYear,".R")

  print(file_path)
  file.create(file_path)

  script_header =
  str_c('\n  NAMEPLATE = \"', NAMEPLATE ,'\",
         \n  NMP = \"', NMP, '\",
         \n  nmp = \"', nmp, '\",
         \n  rgn = c("CR","MAR","MTN","MWR","NER","NWR","SER","WR"),
         \n  modeling_start_date = \"', modeling_start_date, '\",
         \n  modeling_end_date = \"', modeling_end_date, '\",
         \n  FiscalPeriod = \"', FiscalYear, '\"\n')

  script_body = '

  out_dest_decomp = here::here("modeling","SalesModels", NMP, str_c(NAMEPLATE,"-",FiscalPeriod,"-Decomp.csv"))
  out_dest_undecomp = here::here("output","SalesModels", NMP, str_c(NAMEPLATE,"-",FiscalPeriod,"-UnnestedDecomp.Rds"))

  in_data_fmi = here::here("data","processed_data", "FMI.csv")
  in_data_kpi = here::here("data","processed_data", "KPI.csv")


  ##############################################################
  ########################### START ############################
  ##############################################################

  print("")
  message(str_c("1 - ", NAMEPLATE, " SALES MODEL SCRIPT RUN INITIATED"))
  options(scipen = 999, digits = 4)

  ##############################################################
  ######################## LOADING DATA ########################
  ##############################################################

  FMI =
  mmm_data = mmm_load_data(nmp, rgn, in_data_fmi, in_data_kpi)

  FMI = mmm_data$FMI
  KPI = mmm_data$KPI
  fmi_dataP = mmm_data$fmi_dataP
  nonfmi_dataP = mmm_data$nonfmi_dataP

  ##############################################################
  ################## DEFINE THE MODEL STRING ###################
  ##############################################################

  stringformatPanel_Div = str_squish("")

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
  # exportREGCoeff(Formula = stringformatPanel_Div,
  #                DF = DF_Panel_Div,
  #                Panel = 8,
  #                model = model_Div,
  #                nameDF = "n_Titan-FY18P6",
  #                kpi_var = "ttn_sales")

  ##############################################################
  ################### COMPUTE CONTRIBUTIONS ####################
  ##############################################################

  Sales_div = contributionsADD(reg_name = str_c(NAMEPLATE, "-", FiscalPeriod),
                               cat_table = str_c(NAMEPLATE, "-Variable-Categorization.xlsx"),
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

create_responsecurve_script = function(NMP = NULL, FiscalYear = NULL, modeling_start_date = NULL, modeling_end_date = NULL){
  if(NMP == "TTN"){
    NAMEPLATE = "Titan"
    nmp = "ttn"
  }else if(NMP == "VER"){
    NAMEPLATE = "Versa"
    nmp = "ver"
  }else if(NMP == "ALT"){
    NAMEPLATE = "Altima"
    nmp = "alt"
  }else if(NMP == "ARM"){
    NAMEPLATE = "Armada"
    nmp = "arm"
  }else if(NMP == "FRO"){
    NAMEPLATE = "Frontier"
    nmp = "fro"
  }else if(NMP == "LEF"){
    NAMEPLATE = "LEAF"
    nmp = "lef"
  }else if(NMP == "RGE"){
    NAMEPLATE = "ROGUE"
    nmp = "rge"
  }else if(NMP == "RGS"){
    NAMEPLATE = "RogueSport"
    nmp = "rgs"
  }else if(NMP == "MUR"){
    NAMEPLATE = "Murano"
    nmp = "mur"
  }else if(NMP == "MAX"){
    NAMEPLATE = "Maxima"
    nmp = "max"
  }else if(NMP == "PTH"){
    NAMEPLATE = "Pathfinder"
    nmp = "pth"
  }else if(NMP == "SEN"){
    NAMEPLATE = "Sentra"
    nmp = "sen"
  }else if(NMP == "NV"){
    NAMEPLATE = "NV"
    nmp = "nv"
  }else{
    print(NMP)
    message("need to configure")
  }
  print(NMP)
  print("hey")
  file_path = str_c("modeling/ResponseCurves/",NMP,"/",NAMEPLATE,"-",FiscalYear,".R")

  print(file_path)
  file.create(file_path)

  script_header =
    str_c('\n  NAMEPLATE = \"', NAMEPLATE ,'\",
         \n  NMP = \"', NMP, '\",
         \n  nmp = \"', nmp, '\",
         \n  FiscalPeriod = \"', FiscalYear, '\"\n')

  script_body = '
    in_decomp = here::here("output","SalesModels", NMP, str_c(NAMEPLATE,"-",FiscalPeriod,"-UnnestedDecomp.Rds"))
  out_dest_abc = here::here("modeling","ResponseCurves", NMP, str_c(NAMEPLATE,"-",FiscalPeriod,"-ABC.csv"))
  out_dest_abcs = here::here("output","ResponseCurves", NMP, str_c(NAMEPLATE,"-",FiscalPeriod,"-RespCurve.Rds"))

  ##############################################################
  ########################### START ############################
  ##############################################################

  print("")
  message(str_c("1 - ",NAMEPLATE, " - RESPONSE CURVES SCRIPT RUN INITIATED"))

  ##############################################################
  ######################## LOADING DATA ########################
  ##############################################################

  modeled = c("alt", "arm", "fro",
               "lef", "max", "mur",
               "nv", "pth", "rge",
               "sen", "ttn", "ver")

  FMI =
    read.csv(here::here("data","processed_data", "2020-02-27_FY18P6NissanModelingFMI.csv"),
             stringsAsFactors = FALSE) %>%
    collect(n = Inf) %>%
    filter(region == "100") %>%
    filter(vehicle %in% modeled) %>%
    mutate(week = ymd(week))

  message(str_c("2 - ",NAMEPLATE, " FMI data loaded"))

  # Pivot
  fmi_dataP =
    data.table::dcast(FMI, vehicle + week ~ var + type + tier,
                      value.var = "value",
                      fun.aggregate = sum) %>% as.data.frame(.)


  fmi_dataP[is.na(fmi_dataP)]  = 0

  message(str_c("3 - ",NAMEPLATE, " FMI Data Pivited"))

  ##############################################################
  ###################### LOADING DECOMP ########################
  ##############################################################

  in_decomp = readRDS(in_decomp)
  message(str_c("4 - ",NAMEPLATE, " Unnested Decomp Loaded"))

  ##############################################################
  ###################### Create Ctable #########################
  ##############################################################

  var_tblPG =
    in_decomp$varCont %>%
    filter(Categories != "Date") %>%
    tidyr::gather("Date", "Value", 5:ncol(.)) %>%
    mutate(Date = ymd(Date),
           FiscalYear = FY(Date, FYBegin = 4))

  common_dates =
    var_tblPG %>%
    filter(Date > "2018-09-30" & Date < "2019-04-01") %>%
    mutate(FiscalYear = 2019)

  Ctable =
    bind_rows(var_tblPG, common_dates) %>%
    group_by(variable, Categories, Model, Group, FiscalYear) %>%
    summarise(Value = sum(Value, na.rm = TRUE)) %>%
    spread(FiscalYear, Value) %>%
    ungroup() %>%
    mutate(Categories = case_when(Categories %in% c("TV T1","TV T1 MBR") ~ "TV T1",
                                  Categories %in% c("TV T2 EVENT", "TV T2 PRODUCT") ~ "TV T2",
                                  Categories %in% c("DIGITAL TOTAL T1", "DIGITAL TOTAL T1 MBR","DIGITAL T1") ~ "Digital Display T1",
                                  Categories %in% c("DIGITAL TOTAL T2 MBR", "DIGITAL TOTAL T2","DIGITAL T2") ~ "Digital T2",
                                  Categories %in% c("STREAMING T1","STREAMING T1 MBR") ~ "Streaming T1",
                                  Categories %in% c("SEARCH") ~ "Search T1",
                                  Categories %in% c("RADIO T2") ~ "Radio T2",
                                  Categories %in% c("ADDRESSABLE TV T1") ~ "Addressable TV T1",
                                  Categories %in% c("SOCIAL DISPLAY T1", "SOCIAL VIDEO T1") ~ "Social T1",
                                  Categories %in% c("SOCIAL DISPLAY T2", "SOCIAL DISPLAY T2 MBR") ~ "Social T2",
                                  Categories %in% c("MAGAZINE T1") ~ "Print",
                                  TRUE ~ Categories)) %>%
    filter(!(grepl("PANEL",variable) & !grepl(NMP, variable)))

  message(str_c("5 - ",NAMEPLATE, " Ctable Created"))

  ##############################################################
  ################### Create Spend Table #######################
  ##############################################################

  spendFY18P6 =
    read_csv(here::here("data","roi_app_data","daily_spend_data.csv")) %>%
    mutate(Date = case_when(`Media Channel` == "Social T2" ~ "9/1/19",
                            TRUE ~ Date)) %>%
    mutate(Date = mdy(Date)) %>%
    filter(Model == NAMEPLATE,
           Date >= "2018-10-01",
           Date <= "2019-09-01") %>%
    mutate(FY = "FY18+6",
           media_agg = `Media Channel`,
           model_agg = nmp) %>%
    group_by(FY, model_agg, media_agg) %>%
    summarise(Spend = sum(Spend, na.rm = TRUE))

  message(str_c("6 - ",NAMEPLATE, " - Spend Table Created"))

  ##############################################################
  ##################### Combine Tables #########################
  ##############################################################

  aa =
    Ctable %>%
    select(-c(variable, Model, Group)) %>%
    group_by(Categories) %>%
    summarise_all(funs(sum)) %>%
    filter(Categories == "AA") %>% pull("2019")

  spendpcnt =
    spendFY18P6 %>%
    mutate(pcnt = Spend/sum(Spend))%>%
    select(media_agg, pcnt) %>%
    mutate(fy18_6 = aa*pcnt)


  spendpcnt =
    left_join(spendpcnt,
              as.data.frame(table(Ctable$Categories)),
              by = c("media_agg" = "Var1")) %>%
    mutate(split = fy18_6/Freq)

  spendpcnt[is.na(spendpcnt)] <- 0


  modCtable =
    left_join(Ctable, spendpcnt, by = c("Categories" = "media_agg"))

  modCtable[is.na(modCtable)] = 0

  Ctable_with_aasplit =
    modCtable %>%
    filter(Categories != "AA") %>%
    mutate(`2019` = `2019` + split) %>%
    select(-c(pcnt,fy18_6, Freq, split, model_agg, FY))

  message(str_c("7 - ",NAMEPLATE, " - AA Redistributed"))

  ##############################################################
  ###################### Adjust ERPD ###########################
  ##############################################################

  var_name = ""
  new_var_name = ""
  Ctable_with_aasplit_revised = revise_variable_name(Ctable_with_aasplit, var_name, new_var_name)


  ##############################################################
  ###################### Compute ABC ###########################
  ##############################################################

  message(str_c("8 - ",NAMEPLATE, " - Innitiating ABC Reach Computation"))

  ABClist =
    ABCsReachGen(Contrib = Ctable_with_aasplit_revised,
               fit = fit,
               fit_displayt1 = fit_displayt1,
               fit_displayt2 = fit_displayt2,
               fit_st = fit_st,
               fit_addressable = fit_addressable,
               fmi_data = fmi_dataP,
               panel = nmp,
               year = "2019",
               From = "2017-10-09",
               To = "2019-09-30",
               spend = spendFY18P6,
               multiplier = 49262)

  message(str_c("9 - ",NAMEPLATE, " ABC Reach Computation Complete"))

  ##############################################################
  ################## EXPORT AND SAVE RESULTS ###################
  ##############################################################

  write_csv(ABClist$ABCs, out_dest_abc)
  message(str_c("10 - ",NAMEPLATE, " ABC output file exported to:"))
  print(out_dest_abc)

  saveRDS(ABClist, out_dest_abcs)
  message(str_c("11 - ",NAMEPLATE, " full ABC output saved to:"))
  print(out_dest_abcs)

  ##############################################################
  ########################### END ##############################
  ##############################################################
  '
  script = str_c(script_header, script_body)
  write(script, file = file_path, append = TRUE)
}

add_drake_workflow = function(FiscalYear = NULL){
  if(!dir.exists("drake_workflow")){
    dir.create("drake_workflow")
    dir.create("drake_workflow/wR")
    
    file.create("drake_workflow/make.R")
    script_body_make = '
    library(drake)
    library(here)
    
    source(here::here("drake_workflow","wR","packages.R"))
    source(here::here("drake_workflow","wR","functions.R"))
    source(here::here("drake_workflow","wR","load_fit_curves.R"))
    source(here::here("drake_workflow","wR","plan.R"))
    
    vis_drake_graph(plan, targets_only = TRUE)
    
    make(plan, verbose = 2)
    
    vis_drake_graph(plan, targets_only = TRUE)
    '
    write(script_body_make, file ="drake_workflow/make.R", append = TRUE)

    file.create("drake_workflow/wR/functions.R")
    script_body_functions = '
    FiscalYear = "FY19"

    run_submodel_pi = code_to_function(here::here("modeling","SubModels","PI",
                                                  stringr::str_c(FiscalYear,"-AllModels"),
                                                  stringr::str_c("PI-",FiscalYear,".R")))
    
    run_submodel_pc = code_to_function(here::here("modeling","SubModels","PC",
                                                  stringr::str_c(FiscalYear,"-AllModels"),
                                                  stringr::str_c("PC-",FiscalYear,".R")))
    
    run_submodel_ma = code_to_function(here::here("modeling","SubModels","MA",
                                                  stringr::str_c(FiscalYear,"-AllModels"),
                                                  stringr::str_c("MA-",FiscalYear,".R")))
    
    run_submodel_oao = code_to_function(here::here("modeling","SubModels","OAO",
                                                   stringr::str_c(FiscalYear,"-AllModels"),
                                                   stringr::str_c("OAO-",FiscalYear,".R")))
    
    run_submodel_kba = code_to_function(here::here("modeling","SubModels","KBA",
                                                   stringr::str_c(FiscalYear,"-AllModels"),
                                                   stringr::str_c("KBA-",FiscalYear,".R")))
    
    run_submodel_src = code_to_function(here::here("modeling","SubModels","SRC",
                                                      stringr::str_c(FiscalYear,"-AllModels"),
                                                      stringr::str_c("SRC-",FiscalYear,".R")))
    
    
    run_salesmodel_altima = code_to_function(here::here("modeling","SalesModels","ALT",
                                                        stringr::str_c("Altima-",FiscalYear,".R")))
    
    run_salesmodel_armada = code_to_function(here::here("modeling","SalesModels","ARM",
                                                        stringr::str_c("Armada-",FiscalYear,".R")))
    
    run_salesmodel_frontier = code_to_function(here::here("modeling","SalesModels","FRO",
                                                          stringr::str_c("Frontier-",FiscalYear,".R")))
    
    run_salesmodel_leaf = code_to_function(here::here("modeling","SalesModels","LEF",
                                                      stringr::str_c("LEAF-",FiscalYear,".R")))
    
    run_salesmodel_maxima = code_to_function(here::here("modeling","SalesModels","MAX",
                                                        stringr::str_c("Maxima-",FiscalYear,".R")))
    
    run_salesmodel_murano = code_to_function(here::here("modeling","SalesModels","MUR",
                                                        stringr::str_c("Murano-",FiscalYear,".R")))
    
    run_salesmodel_nv = code_to_function(here::here("modeling","SalesModels","NV",
                                                    stringr::str_c("NV-",FiscalYear,".R")))
    
    run_salesmodel_pathfinder = code_to_function(here::here("modeling","SalesModels","PTH",
                                                            stringr::str_c("Pathfinder-",FiscalYear,".R")))
    
    run_salesmodel_rogue = code_to_function(here::here("modeling","SalesModels","RGE",
                                                       stringr::str_c("Rogue-",FiscalYear,".R")))
    
    run_salesmodel_roguesport = code_to_function(here::here("modeling","SalesModels","RGS",
                                                            stringr::str_c("RogueSport-",FiscalYear,".R")))
    
    run_salesmodel_sentra = code_to_function(here::here("modeling","SalesModels","SEN",
                                                        stringr::str_c("Sentra-",FiscalYear,".R")))
    
    run_salesmodel_titan = code_to_function(here::here("modeling","SalesModels","TTN",
                                                       stringr::str_c("Titan-",FiscalYear,".R")))
    
    run_salesmodel_versa = code_to_function(here::here("modeling","SalesModels","VER",
                                                       stringr::str_c("Versa-",FiscalYear,".R")))
    
    run_respcurve_altima = code_to_function(here::here("modeling","ResponseCurves","ALT",
                                                       stringr::str_c("Altima-",FiscalYear,"-RespCurves.R")))
    
    run_respcurve_armada = code_to_function(here::here("modeling","ResponseCurves","ARM",
                                                       stringr::str_c("Armada-",FiscalYear,"-RespCurves.R")))
    
    run_respcurve_frontier = code_to_function(here::here("modeling","ResponseCurves","FRO",
                                                         stringr::str_c("Frontier-",FiscalYear,"-RespCurves.R")))
    
    run_respcurve_leaf = code_to_function(here::here("modeling","ResponseCurves","LEF",
                                                     stringr::str_c("LEAF-",FiscalYear,"-RespCurves.R")))
    
    run_respcurve_maxima = code_to_function(here::here("modeling","ResponseCurves","MAX",
                                                       stringr::str_c("Maxima-",FiscalYear,"-RespCurves.R")))
    
    run_respcurve_murano = code_to_function(here::here("modeling","ResponseCurves","MUR",
                                                       stringr::str_c("Murano-",FiscalYear,"-RespCurves.R")))
    
    run_respcurve_nv = code_to_function(here::here("modeling","ResponseCurves","NV",
                                                   stringr::str_c("NV-",FiscalYear,"-RespCurves.R")))
    
    run_respcurve_pathfinder = code_to_function(here::here("modeling","ResponseCurves","PTH",
                                                           stringr::str_c("Pathfinder-",FiscalYear,"-RespCurves.R")))
    
    run_respcurve_rogue = code_to_function(here::here("modeling","ResponseCurves","RGE",
                                                      stringr::str_c("Rogue-",FiscalYear,"-RespCurves.R")))
    
    run_respcurve_roguesport = code_to_function(here::here("modeling","ResponseCurves","RGS",
                                                           stringr::str_c("RogueSport-",FiscalYear,"-RespCurves.R")))
    
    run_respcurve_sentra = code_to_function(here::here("modeling","ResponseCurves","SEN",
                                                       stringr::str_c("Sentra-",FiscalYear,"-RespCurves.R")))
    
    run_respcurve_titan = code_to_function(here::here("modeling","ResponseCurves","TTN",
                                                      stringr::str_c("Titan-",FiscalYear,"-RespCurves.R")))
    
    run_respcurve_versa = code_to_function(here::here("modeling","ResponseCurves","VER",
                                                      stringr::str_c("Versa-",FiscalYear,"-RespCurves.R")))
    '
    write(script_body_functions, file ="drake_workflow/wR/functions.R", append = TRUE)
    
    file.create("drake_workflow/wR/plan.R")
    script_body_plan = '
    plan = drake_plan(
    src_submodel_all = run_submodel_src(),
    kba_submodel_all = run_submodel_kba(),
    oao_submodel_all = run_submodel_oao(),
    pc_submodel_all = run_submodel_pc(),
    ma_submodel_all = run_submodel_ma(),
    pi_submodel_all = run_submodel_pi()
  
    altima_salesmodel = run_salesmodel_altima(
      src_submodel_all,
      kba_submodel_all,
      oao_submodel_all,
      pc_submodel_all,
      ma_submodel_all,
      pi_submodel_all,
    ),
    
    armada_salesmodel = run_salesmodel_armada(
      src_submodel_all,
      kba_submodel_all,
      oao_submodel_all,
      pc_submodel_all,
      ma_submodel_all,
      pi_submodel_all,
    ),
    
    frontier_salesmodel = run_salesmodel_frontier(
      src_submodel_all,
      kba_submodel_all,
      oao_submodel_all,
      pc_submodel_all,
      ma_submodel_all,
      pi_submodel_all,
    ),
    
    leaf_salesmodel = run_salesmodel_leaf(
      src_submodel_all,
      kba_submodel_all,
      oao_submodel_all,
      pc_submodel_all,
      ma_submodel_all,
      pi_submodel_all,
    ),
    
    maxima_salesmodel = run_salesmodel_maxima(
      src_submodel_all,
      kba_submodel_all,
      oao_submodel_all,
      pc_submodel_all,
      ma_submodel_all,
      pi_submodel_all,
    ),
    
    murano_salesmodel = run_salesmodel_murano(
      src_submodel_all,
      kba_submodel_all,
      oao_submodel_all,
      pc_submodel_all,
      ma_submodel_all,
      pi_submodel_all,
    ),
    
    nv_salesmodel = run_salesmodel_nv(
      src_submodel_all,
      kba_submodel_all,
      oao_submodel_all,
      pc_submodel_all,
      ma_submodel_all,
      pi_submodel_all,
    ),
    
    pathfinder_salesmodel = run_salesmodel_pathfinder(
      src_submodel_all,
      kba_submodel_all,
      oao_submodel_all,
      pc_submodel_all,
      ma_submodel_all,
      pi_submodel_all,
    ),
    
    rogue_salesmodel = run_salesmodel_rogue(
      src_submodel_all,
      kba_submodel_all,
      oao_submodel_all,
      pc_submodel_all,
      ma_submodel_all,
      pi_submodel_all,
    ),
    
    roguesport_salesmodel = run_salesmodel_roguesport(
      src_submodel_all,
      kba_submodel_all,
      oao_submodel_all,
      pc_submodel_all,
      ma_submodel_all,
      pi_submodel_all,
    ),
    
    sentra_salesmodel = run_salesmodel_sentra(
      src_submodel_all,
      kba_submodel_all,
      oao_submodel_all,
      pc_submodel_all,
      ma_submodel_all,
      pi_submodel_all,
    ),
    
    titan_salesmodel = run_salesmodel_titan(
      src_submodel_all,
      kba_submodel_all,
      oao_submodel_all,
      pc_submodel_all,
      ma_submodel_all,
      pi_submodel_all,
    ),
    
    versa_salesmodel = run_salesmodel_versa(
      src_submodel_all,
      kba_submodel_all,
      oao_submodel_all,
      pc_submodel_all,
      ma_submodel_all,
      pi_submodel_all,
    ),
  
    altima_respcurves = run_respcurve_altima(altima_salesmodel),
    armada_respcurves = run_respcurve_armada(armada_salesmodel),
    frontier_respcurves = run_respcurve_frontier(frontier_salesmodel),
    leaf_respcurves = run_respcurve_leaf(leaf_salesmodel),
    maxima_respcurves = run_respcurve_maxima(maxima_salesmodel),
    murano_respcurves = run_respcurve_murano(murano_salesmodel),
    nv_respcurves = run_respcurve_nv(nv_salesmodel),
    pathfinder_respcurves = run_respcurve_pathfinder(pathfinder_salesmodel),
    rogue_respcurves = run_respcurve_rogue(rogue_salesmodel),
    roguesport_respcurves = run_respcurve_roguesport(roguesport_salesmodel),
    sentra_respcurves = run_respcurve_sentra(sentra_salesmodel),
    titan_respcurves = run_respcurve_titan(titan_salesmodel),
    versa_respcurves = run_respcurve_versa(versa_salesmodel)
  )
    '
    write(script_body_plan, file ="drake_workflow/wR/plan.R", append = TRUE)
    
    file.create("drake_workflow/wR/packages.R")
    script_body_packages = '
    library(plyr)
    library(dplyr)
    library(RcppRoll)
    library(plm)
    library(tidyverse)
    library(compiler)
    library(data.table)
    library(readxl)
    library(stringr)
    library(DataCombine)
    library(lubridate)
    library(zoo)
    library(decompR)
    library(OneStepBrandScience)
    library(plotly)
    library(magrittr)
    
    source(here::here("R","Source_StringModelV9_BDS2.R"))
    source(here::here("R","Decomp_RCready_V5.r"))
    source(here::here("R","my_ABCsReachGen.R"))
    source(here::here("R","ResponseCurveFunctions.R"))
    
    source(here::here("R","mmm_load_data.R"))
    source(here::here("R","mmm_load_submodel.R"))
    source(here::here("R","mmm_load_submodels.R"))
    source(here::here("R","mmm_unnest_models.R"))
    
    conflicted::conflict_prefer("gather", "tidyr")
    conflicted::conflict_prefer("rename", "dplyr")
    conflicted::conflict_prefer("arrange", "dplyr")
    conflicted::conflict_prefer("filter", "dplyr")
    conflicted::conflict_prefer("mutate", "dplyr")
    conflicted::conflict_prefer("first", "dplyr")
    conflicted::conflict_prefer("dcast", "reshape2")
    conflicted::conflict_prefer("summarise", "dplyr")
    conflicted::conflict_prefer("melt", "reshape2")
    '
    write(script_body_packages, file ="drake_workflow/wR/packages.R", append = TRUE)
    
    file.create("drake_workflow/wR/load_fit_curves.R")
    script_body_curves = '
    pth1 = here::here("data","fit_curves","2016-07-20 Nissan MIXEDREACHPARAM Adresponse.xlsx")
    pth2 = here::here("data","fit_curves","2019-08-16 Nissan MIXEDREACHPARAM Adresponse by Channel.xlsx")
    
    fit = read_excel(pth1)
    vars = fit[,-1]
    
    fit_displayt1 = read_excel(pth2, sheet = "DisplayT1")
    vars_display_t1 = fit_displayt1[,-1]
    
    fit_displayt2 = read_excel(pth2, sheet = "DisplayT2")
    vars_display_t2 = fit_displayt2[,-1]
    
    fit_st = read_excel(pth2, sheet = "ST")
    vars_streaming = fit_st[,-1]
    
    fit_addressable = read_excel(pth2, sheet = "Addressable")
    vars_addressable = fit_addressable[,-1]
    '
    write(script_body_curves, file ="drake_workflow/wR/load_fit_curves.R", append = TRUE)
  }
}

add_shiny_app = function(){}

create_mmm_project = function(path = "~/Desktop/", FiscalYear = NULL, 
                              modeling_start_date = NULL, modeling_end_date = NULL, 
                              add_drake_workflow = TRUE,
                              open_proj = TRUE,
                              init_packrat = FALSE){
  require(stringr)
  require(usethis)
  require(here)
  
  if(!dir.exists(path)){
    stop(stringr::str_c(path," doesn't exist. Please specify a valid path to directory."))
  }
  #1 Check the format of the start and end date strings
  #2 Check that fiscal year is a valid string

  mmm_proj_name = stringr::str_c(FiscalYear,"-Modeling")
  dir_loc = stringr::str_c(path,"/",mmm_proj_name)
  print(dir_loc)
  
  if(dir.exists(dir_loc)){
    stop(str_c("Project at ",dir_loc," already exists."))
  }else{
    dir.create(dir_loc)
    setwd(dir_loc)
  }

  if(!dir.exists("modeling")){
    dir.create("modeling")

    dir.create("modeling/SubModels")
    sbms = c("KBA","MA","OAO","PC","PI","SRC")
    for(sbm in sbms){
      location = stringr::str_c("modeling/SubModels/",sbm)
      dir.create(location)
      dir.create(stringr::str_c(location,"/", FiscalYear,"-AllModels"))
      print(location)
      print(str_c(location,"/", FiscalYear,"-AllModels"))
      create_submodel_script(SBM = sbm, NMP = "ALL",
                             FiscalYear = FiscalYear,
                             modeling_start_date = modeling_start_date,
                             modeling_end_date = modeling_end_date)
    }

    dir.create("modeling/SalesModels")
    nmps = c("ALT","ARM","FRO","LEF","MAX","MUR","NV","PTH","RGE","RGS","SEN","TTN","VER")
    for(nmp in nmps){
      location = stringr::str_c("modeling/SalesModels/",nmp)
      print(location)
      dir.create(location)
      create_salesmodel_script(NMP = nmp, FiscalYear = FiscalYear,
                               modeling_start_date = modeling_start_date,
                               modeling_end_date = modeling_end_date)
    }

    dir.create("modeling/ResponseCurves")
    for(nmp in nmps){
      location = stringr::str_c("modeling/ResponseCurves/",nmp)
      print(location)
      dir.create(location)
      create_responsecurve_script(NMP = nmp, FiscalYear = FiscalYear,
                                  modeling_start_date = modeling_start_date,
                                  modeling_end_date = modeling_end_date)

    }

  }

  if(!dir.exists("output")){
    dir.create("output")

    dir.create("output/SubModels")
    sbms = c("KBA","MA","OAO","PC","PI","SRC")
    for(sbm in sbms){
      dir.create(stringr::str_c("output/SubModels/",sbm))
    }

    dir.create("output/SalesModels")
    dir.create("output/ResponseCurves")
    nmps = c("ALT","ARM","FRO","LEF","MAX","MUR","NV","PTH","RGE","RGS","SEN","TTN","VER")
    for(nmp in nmps){
      dir.create(stringr::str_c("output/SalesModels/",nmp))
      print(stringr::str_c("output/SalesModels/",nmp))
      dir.create(stringr::str_c("output/ResponseCurves/",nmp))
      print(stringr::str_c("output/ResponseCurves/",nmp))
    }

  }

  if(!dir.exists("data")){
    dir.create("data")
    dir.create("data/processed_data")
    dir.create("data/categorization")
    dir.create("data/fit_curves")
  }

  if(!dir.exists("R")){
    dir.create("R")
  }
  
  usethis::create_project(path = dir_loc, open = open_proj, rstudio = TRUE)
  if(init_packrat == TRUE){
    packrat::init()
  }
}

create_mmm_project(path = "~/Desktop/",
                   FiscalYear = "FY19",
                   modeling_start_date = "2015-04-01",
                   modeling_end_date = "2019-09-30",
                   add_drake_workflow = TRUE,
                   open_proj = TRUE,
                   init_packrat = TRUE)
