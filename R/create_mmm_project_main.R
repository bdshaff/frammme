# create_mmm_project = function(path = "~/Desktop/", FiscalYear = NULL,
#                               modeling_start_date = NULL, modeling_end_date = NULL,
#                               add_drake_workflow = TRUE,
#                               open_proj = TRUE,
#                               init_packrat = FALSE){
#   require(stringr)
#   require(usethis)
#   require(here)
#
#   if(!dir.exists(path)){
#     stop(stringr::str_c(path," doesn't exist. Please specify a valid path to directory."))
#   }
#   #1 Check the format of the start and end date strings
#   #2 Check that fiscal year is a valid string
#
#   mmm_proj_name = stringr::str_c(FiscalYear,"-Modeling")
#   dir_loc = stringr::str_c(path,"/",mmm_proj_name)
#   print(dir_loc)
#
#   if(dir.exists(dir_loc)){
#     stop(str_c("Project at ",dir_loc," already exists."))
#   }else{
#     dir.create(dir_loc)
#     setwd(dir_loc)
#   }
#
#   if(!dir.exists("modeling")){
#     dir.create("modeling")
#
#     dir.create("modeling/SubModels")
#     sbms = c("KBA","MA","OAO","PC","PI","SRC")
#     for(sbm in sbms){
#       location = stringr::str_c("modeling/SubModels/",sbm)
#       dir.create(location)
#       dir.create(stringr::str_c(location,"/", FiscalYear,"-AllModels"))
#       print(location)
#       print(str_c(location,"/", FiscalYear,"-AllModels"))
#       create_submodel_script(SBM = sbm, NMP = "ALL",
#                              FiscalYear = FiscalYear,
#                              modeling_start_date = modeling_start_date,
#                              modeling_end_date = modeling_end_date)
#     }
#
#     dir.create("modeling/SalesModels")
#     nmps = c("ALT","ARM","FRO","LEF","MAX","MUR","NV","PTH","RGE","RGS","SEN","TTN","VER")
#     for(nmp in nmps){
#       location = stringr::str_c("modeling/SalesModels/",nmp)
#       print(location)
#       dir.create(location)
#       create_salesmodel_script(NMP = nmp, FiscalYear = FiscalYear,
#                                modeling_start_date = modeling_start_date,
#                                modeling_end_date = modeling_end_date)
#     }
#
#     dir.create("modeling/ResponseCurves")
#     for(nmp in nmps){
#       location = stringr::str_c("modeling/ResponseCurves/",nmp)
#       print(location)
#       dir.create(location)
#       create_responsecurve_script(NMP = nmp, FiscalYear = FiscalYear,
#                                   modeling_start_date = modeling_start_date,
#                                   modeling_end_date = modeling_end_date)
#
#     }
#
#   }
#
#   if(!dir.exists("output")){
#     dir.create("output")
#
#     dir.create("output/SubModels")
#     sbms = c("KBA","MA","OAO","PC","PI","SRC")
#     for(sbm in sbms){
#       dir.create(stringr::str_c("output/SubModels/",sbm))
#     }
#
#     dir.create("output/SalesModels")
#     dir.create("output/ResponseCurves")
#     nmps = c("ALT","ARM","FRO","LEF","MAX","MUR","NV","PTH","RGE","RGS","SEN","TTN","VER")
#     for(nmp in nmps){
#       dir.create(stringr::str_c("output/SalesModels/",nmp))
#       print(stringr::str_c("output/SalesModels/",nmp))
#       dir.create(stringr::str_c("output/ResponseCurves/",nmp))
#       print(stringr::str_c("output/ResponseCurves/",nmp))
#     }
#
#   }
#
#   if(!dir.exists("data")){
#     dir.create("data")
#     dir.create("data/processed_data")
#     dir.create("data/categorization")
#     dir.create("data/fit_curves")
#   }
#
#   if(!dir.exists("R")){
#     dir.create("R")
#   }
#
#   if(add_drake_workflow == TRUE){
#     add_drake_workflow()
#   }
#
#   usethis::create_project(path = dir_loc, open = open_proj, rstudio = TRUE)
#   if(init_packrat == TRUE){
#     packrat::init()
#   }
# }
