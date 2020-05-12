#' frammme: Framework for MMM projects
#'
#' Functions to Generate an MMM Project Framework.
#' The main function of this package is `create_mmm_project`
#' which takes parameters as arguments and creates a folder with an RStudio project following the MMM modeling framework.
#'
#' @section Functions:
#'
#' @usage
#' This example creates an MMM modeling project directory at the location pointed to by the path argument.
#' The add_drake_workflow adds a subdirectory with a drake workflow set up for execution.
#'
#' library(frammme)
#' create_mmm_project(path = "~/Desktop/",
#'                    FiscalYear = "FY19",
#'                    modeling_start_date = "2015-04-01",
#'                    modeling_end_date = "2019-09-30",
#'                    add_drake_workflow = TRUE,
#'                    open_proj = TRUE,
#'                    init_packrat = FALSE)
#'
#' @details The result of the call to create_mmm_project creates a directory with the following directory tree.
#' Model scripts are automatically added and ready for execution via the helper functions create_submodel_script,
#' create_salesmodel_script, and create_responsecurve_script.
#'
#' You can run fs::dir_tree() to see the structure of the created project.
#'
#'
#' @docType package
#' @name frammme
NULL
