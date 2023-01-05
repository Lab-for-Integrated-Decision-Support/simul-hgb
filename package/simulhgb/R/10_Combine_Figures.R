# Extracted functions from `10_Combine_Figures.Rmd`

#'
#' @title Make File Name
#'
#' @description Helper function to make the file name from site, file, run.date
#'
.makeFileName <- function (site, file, run.date) {

  return(
    file.path(
      Sys.getenv('PICU_LAB_DATA_PATH'),
      paste0(site, '_', file, '_', run.date, '.rData')
    )
  )
}


#'
#' @title List Vars
#'
#' @description Lists the variables within an rData file
#'
#' @param site The name of the site for extraction (e.g. `URMC` or `CHOP`)
#' @param file The rData file name (without the `<site>` or `<date>` parts)
#' @param run.date The run.date string
#' @param attach If TRUE, attaches to the file [Default]
#' @param detach If TRUE, detaches the file after extraction [Default]
#'
listVars <- function (site, file, run.date, attach = T, detach = T) {

  file.name <- .makeFileName(site,file,run.date)

  if (attach)
    attach(file.name)

  pos <- which(search() == paste0('file:', file.name))


  print(ls(pos = pos))

  if (detach)
    detach(pos = pos)
}


#'
#' @title Extract Var
#'
#' @description Extracts a variable from an rData file to the Global Env
#'
#' @param old.var The variable name in the rData file (not character string)
#' @param new.var.name The character string of the new variable name to be
#'     stored in the Global Env
#' @param site The name of the site for extraction (e.g. `URMC` or `CHOP`)
#' @param file The rData file name (without the `<site>` or `<date>` parts)
#' @param run.date The run.date string
#' @param attach If TRUE, attaches to the file [Default]
#' @param detach If TRUE, detaches the file after extraction [Default]
#'
extractVar <- function (old.var, new.var.name, site, file, run.date,
                        attach = T, detach = T) {

  file.name <- .makeFileName(site,file,run.date)

  if (attach)
    attach(file.name)

  pos <- which(search() == paste0('file:', file.name))

  assign(
    x = new.var.name,
    value = old.var,
    envir = .GlobalEnv
  )

  if (detach)
    detach(pos = pos)
}
