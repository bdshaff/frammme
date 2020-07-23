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
run_submodel_pi <- code_to_function(here::here(
  "modeling", "SubModels", "PI",
  stringr::str_c(FiscalYear, "-AllModels"),
  stringr::str_c("PI-", FiscalYear, ".R")
))

run_submodel_pc <- code_to_function(here::here(
  "modeling", "SubModels", "PC",
  stringr::str_c(FiscalYear, "-AllModels"),
  stringr::str_c("PC-", FiscalYear, ".R")
))

run_submodel_ma <- code_to_function(here::here(
  "modeling", "SubModels", "MA",
  stringr::str_c(FiscalYear, "-AllModels"),
  stringr::str_c("MA-", FiscalYear, ".R")
))

run_submodel_oao <- code_to_function(here::here(
  "modeling", "SubModels", "OAO",
  stringr::str_c(FiscalYear, "-AllModels"),
  stringr::str_c("OAO-", FiscalYear, ".R")
))

run_submodel_kba <- code_to_function(here::here(
  "modeling", "SubModels", "KBA",
  stringr::str_c(FiscalYear, "-AllModels"),
  stringr::str_c("KBA-", FiscalYear, ".R")
))

run_submodel_src <- code_to_function(here::here(
  "modeling", "SubModels", "SRC",
  stringr::str_c(FiscalYear, "-AllModels"),
  stringr::str_c("SRC-", FiscalYear, ".R")
))
    '
    script_functions = str_c(script_header_functions, script_body_functions)
    write(script_functions, file ="drake_workflow/wR/script_functions.R", append = TRUE)

    file.create("drake_workflow/wR/plan.R")
    script_body_plan = '
plan <- drake_plan(
  src_submodel_all = run_submodel_src(),
  kba_submodel_all = run_submodel_kba(),
  oao_submodel_all = run_submodel_oao(),
  pc_submodel_all = run_submodel_pc(),
  ma_submodel_all = run_submodel_ma(),
  pi_submodel_all = run_submodel_pi(),
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
pth1 <- here::here("data", "fit_curves", "2016-07-20 Nissan MIXEDREACHPARAM Adresponse.xlsx")
pth2 <- here::here("data", "fit_curves", "2019-08-16 Nissan MIXEDREACHPARAM Adresponse by Channel.xlsx")

fit <- read_excel(pth1)
vars <- fit[, -1]

fit_displayt1 <- read_excel(pth2, sheet = "DisplayT1")
vars_display_t1 <- fit_displayt1[, -1]

fit_displayt2 <- read_excel(pth2, sheet = "DisplayT2")
vars_display_t2 <- fit_displayt2[, -1]

fit_st <- read_excel(pth2, sheet = "ST")
vars_streaming <- fit_st[, -1]

fit_addressable <- read_excel(pth2, sheet = "Addressable")
vars_addressable <- fit_addressable[, -1]

    '
    write(script_body_curves, file ="drake_workflow/wR/load_fit_curves.R", append = TRUE)
  }
}
