source("create_mmm_project_main.R")
source("create_submodel_script.R")
source("create_salesmodel_script.R")
source("create_responsecurve_script.R")
source("add_frake_workflow.R")
source("add_shiny_app.R")


create_mmm_project(path = "~/Desktop/",
                   FiscalYear = "FY19",
                   modeling_start_date = "2015-04-01",
                   modeling_end_date = "2019-09-30",
                   add_drake_workflow = TRUE,
                   open_proj = TRUE,
                   init_packrat = TRUE)