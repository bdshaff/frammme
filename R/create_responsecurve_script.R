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
    NAMEPLATE = "Rogue"
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
    str_c('\n  NAMEPLATE = \"', NAMEPLATE ,'\"
         \n  NMP = \"', NMP, '\"
         \n  nmp = \"', nmp, '\"
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
