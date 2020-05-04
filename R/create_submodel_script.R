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