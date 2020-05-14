add_drake_workflow = function(FiscalYear = NULL){
  if(!dir.exists("drake_workflow")){
    dir.create("drake_workflow")
    dir.create("drake_workflow/wR")

    file.create("drake_workflow/make.R")
    script_body_make = '
library(drake)

source(here::here("drake_workflow","wR","packages.R"))
source(here::here("drake_workflow","wR","script_functions.R"))
source(here::here("drake_workflow","wR","load_fit_curves.R"))
source(here::here("drake_workflow","wR","plan.R"))

vis_drake_graph(plan, targets_only = TRUE)

make(plan, verbose = 2)

vis_drake_graph(plan, targets_only = TRUE)
    '
    write(script_body_make, file ="drake_workflow/make.R", append = TRUE)

    file.create("drake_workflow/wR/script_functions.R")

    script_header_functions = str_c('\n  FiscalYear = \"', FiscalYear, '\"\n')
    script_body_functions = '

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
                                                       stringr::str_c("Altima-",FiscalYear,".R")))

    run_respcurve_armada = code_to_function(here::here("modeling","ResponseCurves","ARM",
                                                       stringr::str_c("Armada-",FiscalYear,".R")))

    run_respcurve_frontier = code_to_function(here::here("modeling","ResponseCurves","FRO",
                                                         stringr::str_c("Frontier-",FiscalYear,".R")))

    run_respcurve_leaf = code_to_function(here::here("modeling","ResponseCurves","LEF",
                                                     stringr::str_c("LEAF-",FiscalYear,".R")))

    run_respcurve_maxima = code_to_function(here::here("modeling","ResponseCurves","MAX",
                                                       stringr::str_c("Maxima-",FiscalYear,".R")))

    run_respcurve_murano = code_to_function(here::here("modeling","ResponseCurves","MUR",
                                                       stringr::str_c("Murano-",FiscalYear,".R")))

    run_respcurve_nv = code_to_function(here::here("modeling","ResponseCurves","NV",
                                                   stringr::str_c("NV-",FiscalYear,".R")))

    run_respcurve_pathfinder = code_to_function(here::here("modeling","ResponseCurves","PTH",
                                                           stringr::str_c("Pathfinder-",FiscalYear,".R")))

    run_respcurve_rogue = code_to_function(here::here("modeling","ResponseCurves","RGE",
                                                      stringr::str_c("Rogue-",FiscalYear,".R")))

    run_respcurve_roguesport = code_to_function(here::here("modeling","ResponseCurves","RGS",
                                                           stringr::str_c("RogueSport-",FiscalYear,".R")))

    run_respcurve_sentra = code_to_function(here::here("modeling","ResponseCurves","SEN",
                                                       stringr::str_c("Sentra-",FiscalYear,".R")))

    run_respcurve_titan = code_to_function(here::here("modeling","ResponseCurves","TTN",
                                                      stringr::str_c("Titan-",FiscalYear,".R")))

    run_respcurve_versa = code_to_function(here::here("modeling","ResponseCurves","VER",
                                                      stringr::str_c("Versa-",FiscalYear,".R")))
    '
    script_functions = str_c(script_header_functions, script_body_functions)
    write(script_functions, file ="drake_workflow/wR/script_functions.R", append = TRUE)

    file.create("drake_workflow/wR/plan.R")
    script_body_plan = '
    plan = drake_plan(
    src_submodel_all = run_submodel_src(),
    kba_submodel_all = run_submodel_kba(),
    oao_submodel_all = run_submodel_oao(),
    pc_submodel_all = run_submodel_pc(),
    ma_submodel_all = run_submodel_ma(),
    pi_submodel_all = run_submodel_pi(),

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
library(car)
library(plotly)
library(magrittr)
library(janitor)

library(OneStepBrandScience)
library(decompR)
library(frammme)
library(mmmlegacy)
library(mmmactive)

conflicted::conflict_prefer("unwind", "mmmlegacy")
conflicted::conflict_prefer("contributions", "mmmlegacy")
conflicted::conflict_prefer("gather", "tidyr")
conflicted::conflict_prefer("rename", "dplyr")
conflicted::conflict_prefer("arrange", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("mutate", "dplyr")
conflicted::conflict_prefer("first", "dplyr")
conflicted::conflict_prefer("lag", "dplyr")
conflicted::conflict_prefer("summarise", "dplyr")
conflicted::conflict_prefer("melt", "reshape2")
conflicted::conflict_prefer("dcast", "reshape2")
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
