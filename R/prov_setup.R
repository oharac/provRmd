#' Set up git provenance tracking process
#'
#' This function sets up a number of global variables used to track provenance
#' information during the run of the script.  If used within an R Markdown
#' script, uses the knitted file name and directory.  Sets up NULL variables that
#' will contain prov_track and script_track info, timing info, and other info
#' used in the final provenance output dataframe.
#' @param run_tag A character string that briefly describes this particular run. Default = 'standard run'.
#' @export
#' @examples
#' prov_setup()
#' prov_setup(run_tag = 'testing with new data set')

prov_setup <- function (run_tag = 'standard run') {

  ### create environment for all the running variables
  message('creating .provEnv environment in the parent environment')
  assign('.provEnv', new.env(), envir = parent.frame())

  ### set up current directory and file for knitted script.
  ### If not being knitted (e.g. run chunk at a time) the knitr::: call returns
  ### character(0) so set to a valid temp string.

  script_dir <- file.path(getwd(), knitr:::.knitEnv$input.dir) %>%
    stringr::str_sub(1, -3) %>%                            ### ditch annoying '/.' at the end
    stringr::str_replace(path.expand('~'), '~')            ### ditch specific home for generic home

  if(length(script_dir) == 0) {
    ### not being knitted; so escape without acting
    message('prov_setup() only operates within the context of knitting an Rmd.')
    return(invisible())
    # script_dir  <- getwd()  ### default for non-knitted operations
  }

  assign('script_dir', script_dir, envir = .provEnv)

  parent_script_file <- file.path(script_dir, knitr:::knit_concord$get("infile"))
  if(length(parent_script_file) == 0) {
    parent_script_file  <- 'Rmd_not_knitted'
  }
  assign('parent_script_file', parent_script_file, envir = .provEnv)

  ### set the parent_id variable to the parent script; this will be
  ### temporarily modified during a 'source' call so files operated on
  ### by sourced script will get a new parent.
  assign('parent_id', parent_script_file, envir = .provEnv)

  ### set directory for provenance log .csv (for script_prov()):
  assign('log_dir', file.path(script_dir, 'prov'), envir = .provEnv)

  ### initialize the prov_track global variable
  assign('prov_track', NULL, envir = .provEnv)

  ### initialize the script_track global variable
  assign('script_track', NULL, envir = .provEnv)

  ### initialize run_tag global variable based on input argument
  assign('run_tag', run_tag, envir = .provEnv)

  ### initialize sequence global variable - starts at 1 and then
  ### the parent script will be set to 0
  assign('sequence', 1, envir = .provEnv)

  ### initialize process timing
  assign('start_time', proc.time(), envir = .provEnv)

  options(stringsAsFactors = FALSE) ### because factors are annoying
}

