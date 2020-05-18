#' Creat an MMM Project
#'
#' @param path - path to location where project folder should be created.
#' @param FiscalYear - string specifying the fiscal period for the project.
#' @param modeling_start_date - yyyy-mm-dd fromat string.
#' @param modeling_end_date - yyyy-mm-dd fromat string.
#' @param add_drake_workflow - logical. should a drake workflow subdirectory be created.
#' @param open_proj - logical. should the Rstudio project be opened when created.
#' @param init_packrat - logical. should a packrat folder be initiated automatically.
#'

create_mmm_project = function(path = "~/Desktop/", FiscalYear = NULL,
                              modeling_start_date = NULL, modeling_end_date = NULL,
                              add_drake_workflow = TRUE,
                              open_proj = TRUE,
                              init_packrat = FALSE){
  require(drake)
  require(tidyverse)
  require(usethis)
  require(here)
  require(OneStepBrandScience)
  require(decompR)
  require(frammme)
  require(mmmlegacy)
  require(mmmactive)

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

  usethis::create_project(path = dir_loc, open = open_proj, rstudio = TRUE)
  if(init_packrat == TRUE){
    packrat::init()
  }

  if(!dir.exists("modeling")){
    dir.create("modeling")

    dir.create("modeling/ModelStrings")

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

      location = stringr::str_c("modeling/SalesModels/",nmp,"-Bayes")
      print(location)
      dir.create(location)
      create_salesmodelbayes_script(NMP = nmp, FiscalYear = FiscalYear,
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
      dir.create(stringr::str_c("output/SalesModels/",nmp,"-Bayes"))
      print(stringr::str_c("output/SalesModels/",nmp,"-Bayes"))
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

  if(dir.exists("R")){
    print("R/model_strings.R")
    file.create("R/model_strings.R")
    script = '
require(stringr)
######################################################################
############################ SRC Submodel ############################
######################################################################

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

saveRDS(stringformatPanel_Div, "modeling/ModelStrings/src-model-string.Rds")

######################################################################
############################ PI Submodel #############################
######################################################################

stringformatPanel_Div = str_squish("{pi<D>pi_MEANSBY}_Log ~
                                       kba_agg_Log_Lag5 +
                                       pc_Log_Lag5 +
                                       social_display_direct_t1_E4R1P56D10 +
                                       aa_Log_Lag2 +
                                       total_digital_direct_t1_E2R2P98D10 +
                                       tv_e_reg_direct_t2_E3R5P63D04 +
                                       digital_total_direct_t2_E1R1P56D10 +
                                       radio_direct_t2_E6R4P56D10 +
                                       mbr_streaming_direct_t1_E1R1P07D10 +
                                       comp_spend_Lag3 +
                                       social_display_direct_t2_E4R1P56D10")

saveRDS(stringformatPanel_Div, "modeling/ModelStrings/pi-model-string.Rds")

######################################################################
############################ PC Submodel #############################
######################################################################


stringformatPanel_Div = str_squish("{pc<D>pc_MEANSBY}_Log ~
                                       oao_Log_Lag3 +
                                       digital_totalmbr_direct_t2_E4R1P56D10 +
                                       search_Log_Lag1 +
                                       Panel_alt<*>tv_direct_t1_E3R1P14D10 +
                                       Panel_lef<*>tv_direct_t1_E1R1P42D10 +
                                       Panel_rge<*>RANGE_2018_07_01_to_2018_09_01 +
                                       Panel_ttn<*>RANGE_2015_09_01_to_2015_11_01 +
                                       Panel_ttn<*>RANGE_2017_02_01_to_2017_03_01 +
                                       Panel_ttn<*>RANGE_2017_04_01_to_2017_06_01 +
                                       Panel_ttn<*>digital_total_direct_t2_E2R1P28D10 +
                                       RANGE_2015_06_01_to_2015_09_01 +
                                       RANGE_2015_04_01_to_2015_05_01 +
                                       Panel_ttn<*>tv_e_reg_direct_t2_E3R1P28D10 +
                                       addressabletv_direct_t1_E1R1P56D10 +
                                       Panel_rge<*>mbr_total_digital_direct_t1_E3R1P14D10 +
                                       Panel_alt<*>mbr_total_digital_direct_t1_E2R1P42D10 +
                                       Panel_rge<*>mbr_tv_direct_t1_E5R0P70D01 +
                                       Panel_rge<*>DUM_2015_04_01 +
                                       Panel_rge<*>RANGE_2017_01_01_to_2017_03_01 +
                                       Panel_rge<*>tv_e_ceq_direct_t2_E7R4P35D20 +
                                       ma_Log_Lag8 +
                                       social_display_direct_t1_E3R1P42D10 +
                                       social_display_direct_t2_E3R1P42D10")

saveRDS(stringformatPanel_Div, "modeling/ModelStrings/pc-model-string.Rds")

######################################################################
########################### OAO Submodel #############################
######################################################################

stringformatPanel_Div = str_squish("{oao<D>oao_MEANSBY}_Log ~
                                       factor1 +
                                       factor3 +
                                       search_Log_Lag2 +
                                       tv_halo_t1_E3R1P14D10 +
                                       total_digital_direct_t1_E8R1P28D10 +
                                       radio_direct_t2_E8R5P56D10 +
                                       Panel_alt<*>RANGE_2016_06_01_to_2016_08_01 +
                                       Panel_alt<*>RANGE_2017_06_01_to_2017_08_01 +
                                       sedan_src_Log_Lag1 +
                                       ma_Log_Lag8 +
                                       Panel_alt<*>tv_p_eeq_direct_t2_E5R1P42D10 +
                                       Panel_lef<*>tv_p_eeq_direct_t2_E4R0P56D10 +
                                       addressabletv_direct_t1_E5R1P35D15 +
                                       Panel_sen<*>tv_e_reg_direct_t2_E7R1P49D10 +
                                       total_digital_halo_t1_E8R1P56D10 +
                                       social_display_direct_t1_E3R0P14D05 +
                                       social_display_direct_t2_E3R0P14D05 +
                                       mbr_tv_direct_t1_E4R4P07D10 +
                                       Panel_max<*>RANGE_2015_06_01_to_2015_08_01 +
                                       Panel_max<*>RANGE_2016_09_01_to_2016_11_01 +
                                       Panel_sen<*>RANGE_2016_06_01_to_2016_09_01 +
                                       Panel_alt<*>digital_total_direct_t2_E1R0P42D20")

saveRDS(stringformatPanel_Div, "modeling/ModelStrings/oao-model-string.Rds")

######################################################################
########################### MA Submodel ##############################
######################################################################

stringformatPanel_Div  = str_squish("{ma<D>ma_MEANSBY}_Log ~
                                        aa_Log_Lag2 +
                                        tv_p_reg_direct_t2_E2R1P35D10 +
                                        sedan_src_Log_Lag8 +
                                        ev_src_Log_Lag2 +
                                        crossover_src_Log_Lag8 +
                                        truck_src_Log_Lag9 +
                                        suv_src_Log_Lag4 +
                                        tv_e_eeq_direct_t2_E8R1P56D10 +
                                        digital_total_direct_t2_E6R2P56D10 +
                                        digital_totalmbr_direct_t2_E8R0P77D05 +
                                        tv_halo_t1_E8R0P14D05 +
                                        tv_direct_t1_E8R0P07D03 +
                                        mbr_tve_direct_t1_E8R0P14D06 +
                                        mbr_streaming_direct_t1_E2R1P07D10 +
                                        mag_direct_t1_E2R0P07D05 +
                                        mbr_total_digital_direct_t1_E1R1P07D10 +
                                        Panel_rge<*>digital_totalmbr_direct_t2_E8R0P77D10 +
                                        Panel_alt<*>radio_direct_t2_E3R1P07D05 +
                                        Panel_alt<*>tv_halo_t1_E8R4P63D10 +
                                        Panel_rge<*>digital_total_direct_t2_E1R0P56D04 +
                                        Panel_alt<*>aa_Log + total_digital_direct_t1_E1R1P56D10 +
                                        social_display_direct_t1_E1R1P56D10 +
                                        social_display_direct_t2_E1R1P56D10")

saveRDS(stringformatPanel_Div, "modeling/ModelStrings/ma-model-string.Rds")

######################################################################
########################### KBA Submodel #############################
######################################################################


stringformatPanel_Div = str_squish("{kba_agg<D>kba_agg_MEANSBY}_Log ~
                                       pc_Log_Lag2 +
                                       tvc_direct_t1_E5R1P56D10 +
                                       tv_p_ceq_direct_t2_E4R1P56D10 +
                                       tv_e_ceq_direct_t2_E1R1P14D10 +
                                       search_Log_Lag2 +
                                       Panel_alt<*>DUM_2018_05_01 +
                                       Panel_alt<*>DUM_2018_10_01 +
                                       Panel_ttn<*>RANGE_2017_10_01_to_2017_11_01 +
                                       Panel_ttn<*>RANGE_2016_09_01_to_2016_10_01 +
                                       Panel_ttn<*>RANGE_2016_01_01_to_2016_02_01 +
                                       Panel_ttn<*>DUM_2018_09_01 +
                                       RANGE_2018_04_01_to_2018_05_01 +
                                       digital_total_direct_t2_E3R1P56D10 +
                                       Panel_lef<*>RANGE_2018_01_01_to_2018_10_01 +
                                       Panel_arm<*>DUM_2016_09_01 +
                                       Panel_rge<*>DUM_2018_05_01 +
                                       RANGE_2018_06_01_to_2018_11_01 +
                                       digital_totalmbr_direct_t2_E2R2P35D10 +
                                       radio_direct_t2_E1R1P49D10 +
                                       mag_direct_t1_E1R1P42D10 +
                                       addressabletv_direct_t1_E5R1P56D10 +
                                       streaming_direct_t1_E2R1P21D10 +
                                       social_video_direct_t1_E2R1P21D10 +
                                       total_digital_direct_t1_E1R1P21D10 +
                                       social_display_direct_t1_E1R1P21D10 +
                                       social_display_direct_t2_E1R1P21D10")

saveRDS(stringformatPanel_Div, "modeling/ModelStrings/kba-model-string.Rds")


######################################################################
######################### ALTIMA SalesModel ##########################
######################################################################


stringformatPanel_Div = str_squish("alt_sales_div_tiv<D>alt_sales_div_tiv_MEANSBY ~
                                   alt_pnur<D>alt_msrp +
                                   alt_pi_Log +
                                   alt_tv_p_reg_direct_t2_E9R9P14D10 +
                                   alt_digital_total_direct_t2_E8R8P07D20 +
                                   alt_tv_e_reg_direct_t2_E8R1P07D20 +
                                   alt_ma_Log_Lag7 +
                                   RANGE_2018_10_01_to_2018_12_01 +
                                   DUM_2018_06_01 +
                                   alt_tv_direct_t1_E2R0P70D10 +
                                   Panel_NER<*>DUM_2016_03_01 +
                                   Panel_NWR<*>DUM_2017_03_01 +
                                   Panel_NWR<*>DUM_2018_01_01 +
                                   Panel_NWR<*>RANGE_2016_04_01_to_2016_12_01 +
                                   Panel_NWR<*>RANGE_2017_05_01_to_2017_09_01 +
                                   alt_comp_spend_Log +
                                   RANGE_2015_06_01_to_2015_08_01 +
                                   alt_mbr_streaming_direct_t1_E5R5P70D10 +
                                   RANGE_2017_12_01_to_2018_02_01 +
                                   alt_digital_totalmbr_direct_t2_E8R5P35D10 +
                                   alt_search_Log_Lag9 +
                                   alt_total_digital_direct_t1_E2R1P28D10 +
                                   alt_social_display_direct_t1_E2R1P28D10 +
                                   alt_social_display_direct_t2_E2R1P28D10")

saveRDS(stringformatPanel_Div, "modeling/ModelStrings/alt-model-string.Rds")


######################################################################
######################### ARMADA SalesModel ##########################
######################################################################

stringformatPanel_Div = str_squish("arm_sales_div_tiv<D>arm_sales_div_tiv_MEANSBY ~
                                   arm_pi_Log_Lag1 +
                                   arm_aa_Log_Lag5 +
                                   arm_pnur<D>arm_msrp +
                                   arm_addressabletv_direct_t1_E6R5P49D10 +
                                   RANGE_2018_04_01_to_2019_03_01 +
                                   RANGE_2018_08_01_to_2018_10_01 +
                                   DUM_2019_01_01 +
                                   arm_mbr_tve_direct_t1_E8R7P21D10 +
                                   RANGE_2017_07_01_to_2017_09_01 +
                                   arm_tve_halo_t1_E7R5P56D05 +
                                   arm_comp_spend_Lag4 +
                                   arm_tv_e_reg_direct_t2_E7R7P07D20 +
                                   arm_digital_totalmbr_direct_t2_E8R0P28D08")

saveRDS(stringformatPanel_Div, "modeling/ModelStrings/arm-model-string.Rds")

######################################################################
######################### FRONTIER SalesModel ########################
######################################################################

stringformatPanel_Div = str_squish("fro_sales_div_tiv<D>fro_sales_div_tiv_MEANSBY ~
                                   fro_pnur<D>fro_msrp +
                                   fro_pi_Log_Lag3 +
                                   aging +
                                   RANGE_2015_09_01_to_2019_01_01 +
                                   fro_tv_p_reg_direct_t2_E8R0P70D10 +
                                   fro_digital_total_direct_t2_E3R0P07D05 +
                                   DUM_2017_09_01 +
                                   fro_tv_e_reg_direct_t2_E1R4P14D10<*>RANGE_2017_04_01_to_2019_03_01 +
                                   DUM_2017_03_01 +
                                   DUM_2017_06_01 +
                                   DUM_2018_03_01 +
                                   DUM_2015_08_01 +
                                   DUM_2017_12_01 +
                                   DUM_2016_05_01 +
                                   DUM_2016_03_01 +
                                   DUM_2016_10_01 +
                                   DUM_2018_07_01 +
                                   fro_truck_src_Log_Lag4")

saveRDS(stringformatPanel_Div, "modeling/ModelStrings/fro-model-string.Rds")

######################################################################
######################### LEAF SalesModel ############################
######################################################################

stringformatPanel_Div =
  str_squish("lef_sales_div_tiv<D>lef_sales_div_tiv_MEANSBY ~
             lef_pnur<D>lef_msrp +
             lef_pi_Log +
             RANGE_2017_06_01_to_2018_01_01 +
             lef_addressabletv_direct_t1_E4R4P35D30 +
             Panel_SER<*>RANGE_2015_04_01_to_2015_06_01 +
             lef_tv_direct_t1_E6R1P56D01<*>RANGE_2019_06_01_to_2019_09_01 +
             Panel_CR<*>DUM_2016_12_01 +
             Panel_MWR<*>DUM_2016_03_01 +
             Panel_MWR<*>DUM_2017_03_01 +
             Panel_NER<*>RANGE_2017_08_01_to_2017_09_01 +
             lef_mbr_total_digital_direct_t1_E8R5P42D20 +
             lef_digital_totalmbr_direct_t2_E1R1P56D10<A>lef_digital_total_direct_t2_E3R1P21D10 +
             lef_tv_e_reg_direct_t2_E8R1P70D40<*>RANGE_2019_02_01_to_2019_03_01 +
             lef_social_display_direct_t1_E1R1P07D10 +
             Panel_WR<*>RANGE_2016_09_01_to_2016_12_01 +
             Panel_NWR<*>DUM_2016_12_01 +
             Panel_NWR<*>DUM_2017_06_01 +
             Panel_NWR<*>RANGE_2018_03_01_to_2018_05_01 +
             Panel_NER<*>RANGE_2017_01_01_to_2017_04_01 +
             Panel_MTN<*>RANGE_2016_10_01_to_2016_12_01 +
             lef_social_video_direct_t1_E3R1P21D10 +
             DUM_2018_12_01 +
             Panel_MTN<*>DUM_2018_09_01 +
             Panel_MTN<*>DUM_2019_03_01 +
             Panel_MWR<*>DUM_2018_03_01 +
             Panel_MWR<*>DUM_2017_06_01 +
             Panel_NER<*>DUM_2018_03_01 +
             RANGE_2019_01_01_to_2019_03_01 +
             DUM_2019_07_01 +
             DUM_2019_08_01")

saveRDS(stringformatPanel_Div, "modeling/ModelStrings/lef-model-string.Rds")

######################################################################
######################### MAXIMA SalesModel ##########################
######################################################################

stringformatPanel_Div = str_squish("max_sales_div_tiv<D>max_sales_div_tiv_MEANSBY ~
                                   max_pi_Log_Lag7 +
                                   max_pnur<D>max_msrp_Log +
                                   max_kba_agg_Log_Lag5 +
                                   max_digital_totalmbr_direct_t2_E8R1P07D20 +
                                   max_oao_Log_Lag1")

saveRDS(stringformatPanel_Div, "modeling/ModelStrings/max-model-string.Rds")

######################################################################
######################### MURANO SalesModel ##########################
######################################################################

stringformatPanel_Div = str_squish("mur_sales_div_tiv<D>mur_sales_div_tiv_MEANSBY ~
                                   Insentives_Log +
                                   mur_pi_Log_Lag5 +
                                   mur_tv_e_reg_direct_t2_E6R0P28D05 +
                                   mur_mbr_tv_direct_t1_E5R1P07D10 +
                                   mur_streaming_direct_t1_E7R1P35D10 +
                                   DUM_2018_04_01 +
                                   DUM_2017_11_01 +
                                   RANGE_2018_08_01_to_2018_12_01 +
                                   Panel_NWR<*>RANGE_2018_04_01_to_2019_02_01 +
                                   aging +
                                   RANGE_2018_04_01_to_2019_03_01<*>mur_tv_p_reg_direct_t2_E9R0P70D05 +
                                   RANGE_2015_04_01_to_2018_03_01<*>mur_tv_p_reg_direct_t2_E9R9P70D05 +
                                   mur_digital_totalmbr_direct_t2_E3R2P28D10 +
                                   mur_addressabletv_direct_t1_E1R1P77D10 +
                                   mur_search_Log_Lag4 +
                                   mur_ma_Log_Lag6")

saveRDS(stringformatPanel_Div, "modeling/ModelStrings/mur-model-string.Rds")

######################################################################
########################### NV SalesModel ############################
######################################################################

stringformatPanel_Div = str_squish("nv_sales_div_tiv<D>nv_sales_div_tiv_MEANSBY ~
                                   nv_pnur<D>nv_msrp +
                                   nv_total_digital_direct_t1_E8R4P07D10 +
                                   nv_search_Log_Lag9 +
                                   DUM_2017_05_01 +
                                   DUM_2018_05_01 +
                                   DUM_2017_12_01 +
                                   DUM_2017_07_01 +
                                   DUM_2017_09_01 +
                                   DUM_2017_03_01 +
                                   DUM_2015_11_01 +
                                   DUM_2015_04_01 +
                                   DUM_2016_12_01 +
                                   DUM_2019_01_01 +
                                   DUM_2019_03_01 +
                                   DUM_2018_02_01 +
                                   DUM_2015_07_01 +
                                   DUM_2018_03_01 +
                                   DUM_2015_12_01 +
                                   DUM_2018_11_01 +
                                   DUM_2018_10_01 +
                                   DUM_2016_05_01")

saveRDS(stringformatPanel_Div, "modeling/ModelStrings/nv-model-string.Rds")


######################################################################
####################### PATHFINDER SalesModel ########################
######################################################################

stringformatPanel_Div = str_squish("pth_sales_div_tiv<D>pth_sales_div_tiv_MEANSBY ~
                                   pth_pi_Log_Lag2 +
                                   pth_digital_totalmbr_direct_t2_E1R1P28D10 +
                                   pth_total_digital_direct_t1_E2R2P07D10 +
                                   pth_tv_p_ceq_direct_t2_E6R6P35D25 +
                                   pth_tv_e_reg_direct_t2_E5R1P56D10 +
                                   pth_tv_direct_t1_E6R1P56D05<*>RANGE_2015_04_01_to_2018_03_01 +
                                   pth_comp_spend_Log_Lag2 +
                                   pth_pnur<D>pth_msrp +
                                   aging_Log_Lag7 +
                                   pth_digital_total_halo_t2_E1R1P07D10 +
                                   DUM_2016_09_01 +
                                   RANGE_2017_04_01_to_2017_06_01 +
                                   DUM_2017_01_01 +
                                   DUM_2017_08_01 +
                                   DUM_2017_11_01 +
                                   DUM_2018_02_01 +
                                   pth_suv_src_Log +
                                   DUM_2018_03_01 +
                                   DUM_2018_06_01")

saveRDS(stringformatPanel_Div, "modeling/ModelStrings/pth-model-string.Rds")

######################################################################
######################### ROGUE SalesModel ###########################
######################################################################

stringformatPanel_Div = str_squish("rge_sales_div_tiv<D>rge_sales_div_tiv_MEANSBY ~
                                   rge_pnur<D>rge_msrp +
                                   rge_pi_Log +
                                   rge_mbr_tv_direct_t1_E5R1P14D10 +
                                   aging +
                                   rge_digital_total_direct_t2_E3R1P28D20 +
                                   rge_tv_p_reg_direct_t2_E8R5P14D20 +
                                   rge_tv_direct_t1_E1R1P07D10 +
                                   DUM_2018_09_01 +
                                   DUM_2016_12_01 +
                                   DUM_2017_03_01 +
                                   DUM_2017_05_01 +
                                   rge_tv_e_eeq_direct_t2_E2R1P14D10 +
                                   rge_streaming_halo_t1_E3R1P56D10 +
                                   RANGE_2017_09_01_to_2017_10_01 +
                                   DUM_2017_12_01 +
                                   rge_aa_Log_Lag4 +
                                   rge_total_digital_direct_t1_E6R5P63D10 +
                                   rge_streaming_direct_t1_E7R1P70D10<*>RANGE_2015_04_01_to_2018_03_01")

saveRDS(stringformatPanel_Div, "modeling/ModelStrings/rge-model-string.Rds")

######################################################################
####################### ROGUE Sport SalesModel #######################
######################################################################

stringformatPanel_Div = str_squish("rgs_sales_div_tiv<D>rgs_sales_div_tiv_MEANSBY ~
                                   rgs_pnur<D>rgs_msrp +
                                   rgs_pi_Log_Lag1 +
                                   rgs_crossover_src_Log_Lag3 +
                                   rgs_tv_p_eeq_direct_t2_E4R4P56D10 +
                                   rgs_digital_total_direct_t2_E3R3P28D15 +
                                   rgs_digital_total_halo_t2_E1R1P70D10 +
                                   rgs_total_digital_halo_t1_E5R4P56D10 +
                                   rgs_tv_p_reg_halo_t2_E4R1P56D10")

saveRDS(stringformatPanel_Div, "modeling/ModelStrings/rgs-model-string.Rds")

######################################################################
######################### SENTRA SalesModel ##########################
######################################################################

stringformatPanel_Div = str_squish("sen_sales_div_tiv<D>sen_sales_div_tiv_MEANSBY ~
                                     sen_pi_Log_Lag8 +
                                     sen_pnur<D>sen_msrp_Log +
                                     sen_digital_total_direct_t2_E1R1P07D20 +
                                     sen_tv_e_reg_halo_t2_E2R1P07D10 +
                                     sen_tv_e_eeq_direct_t2_E6R4P07D10 +
                                     DUM_2018_05_01 +
                                     DUM_2017_10_01 +
                                     sen_tv_p_ceq_direct_t2_E8R4P28D10 +
                                     DUM_2018_03_01 +
                                     sen_ma_Log_Lag4")

saveRDS(stringformatPanel_Div, "modeling/ModelStrings/sen-model-string.Rds")

######################################################################
######################### TITAN SalesModel ###########################
######################################################################

stringformatPanel_Div = str_squish("ttn_sales_div_tiv<D>ttn_sales_div_tiv_MEANSBY ~
                                     ttn_pi_Log_Lag1 +
                                     ttn_pnur<D>ttn_msrp +
                                     ttn_tv_e_eeq_direct_t2_E1R1P07D10 +
                                     ttn_streaming_direct_t1_E1R1P07D10<*>RANGE_2015_04_01_to_2018_03_01 +
                                     ttn_digital_total_direct_t2_E4R1P14D10 +
                                     DUM_2017_11_01 +
                                     DUM_2018_01_01 +
                                     DUM_2017_05_01 +
                                     ttn_aa_Log_Lag4 +
                                     DUM_2018_03_01 +
                                     ttn_comp_spend_Log_Lag3 +
                                     RANGE_2016_09_01_to_2019_03_01 +
                                     ttn_total_digital_direct_t1_E6R1P70D13 +
                                     ttn_tv_direct_t1_E6R1P70D05 +
                                     DUM_2018_09_01 +
                                     ttn_truck_src_Log_Lag3")

saveRDS(stringformatPanel_Div, "modeling/ModelStrings/ttn-model-string.Rds")

######################################################################
######################### VERSA SalesModel ###########################
######################################################################

stringformatPanel_Div = str_squish("ver_sales_div_tiv<D>ver_sales_div_tiv_MEANSBY ~
                                     ver_pnur<D>ver_msrp +
                                     ver_pi_Log_Lag4 +
                                     ver_tv_p_reg_direct_t2_E8R0P70D03 +
                                     ver_tv_e_reg_direct_t2_E3R1P28D10 +
                                     Panel_NER<*>DUM_2017_10_01 +
                                     Panel_MTN<*>RANGE_2018_04_01_to_2019_03_01 +
                                     Panel_MWR<*>RANGE_2017_11_01_to_2018_12_01 +
                                     ver_tv_halo_t1_E6R3P07D20 +
                                     RANGE_2016_12_01_to_2018_10_01 +
                                     ver_oao +
                                     ver_digital_totalmbr_direct_t2_E5R3P35D10 +
                                     ver_search_Log_Lag3")

saveRDS(stringformatPanel_Div, "modeling/ModelStrings/ver-model-string.Rds")
    '
    write(script, file = "R/model_strings.R", append = TRUE)
    source("R/model_strings.R")
  }

  if(add_drake_workflow == TRUE){
    add_drake_workflow(FiscalYear = FiscalYear)
  }

}
