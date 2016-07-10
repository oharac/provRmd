#' Wrap up provenance tracking
#'
#' This function complets the provenance tracking by capturing the provenance
#' info for the parent script, as well as the session and system info.  This
#' is saved to the provenance log file.  An optional footer is created (by
#' default) including a brief session/system summary, workflow chart, and
#' table of inputs/outputs.
#' @param footer Create a footer including session and system info.
#' @param workflow Create a flow chart of the workflow as determined by the provenance tracking, and append to the footer (footer must be TRUE)
#' @param table Create an output table in the footer; 'dt' creates a DT::datatable() interactive table while 'kable' creates a static table; 'none' creates no table
#' @param cleanup Deletes pre-built footers copied in prov_setup().
#' @param tag Argument passed to script_prov(); a run tag to be included in provenance log, e.g. 'standard run' (default)
#' @param commit_outputs  Argument passed to script_prov(); create a commit for any new files created during this run? (default TRUE)
#' @export
#' @examples
#' git_wrapup()
#' git_wrapup(footer = FALSE, tag = 'debugging data prep', commit_outputs = FALSE)

prov_wrapup <- function(footer   = TRUE,
                        workflow = TRUE,
                        table    = c('dt', 'kable', 'none')[1],
                        cleanup  = FALSE,
                        tag      = .prov_run_tag,
                        commit_outputs = TRUE) {

  .script_prov_out_df <- script_prov(.prov_parent_script_file,
                                     tag = tag,
                                     commit_outputs = commit_outputs)

  assign('.script_prov_out_df', .script_prov_out_df, envir = .GlobalEnv)

  message('script_prov complete...')

  if(footer) {
    message('footer = ', footer)
    # knitr::knit_child(system.file('footer/prov_ftr.Rmd', package = 'provRmd'))
    knitr::knit_child('prov/prov_ftr0.Rmd')
  }

  if(workflow & footer) {
    message('workflow = ', workflow)
    prov_plot_out <- plot_prov(.script_track)

    print(render_graph(prov_plot_out))
  }

  if(table != 'none' & footer) {
    message('table = ', table)
    prov_tbl <- .script_track %>% #[ , 1:10] %>%
      mutate(commit_url  = str_replace(commit_url, 'Previous commit: ', ''),
             commit_url  = ifelse(commit_url == 'no version control info found', NA, commit_url),
             commit_hash = ifelse(is.na(commit_url), NA,
                                  sprintf('[%s](%s)', str_sub(commit_url, -40, -34), commit_url)))
    # sprintf('<a href = %s>%s</a>', commit_url, str_sub(commit_url, -6, -1))))

    run_id   <- first(prov_tbl$run_id)
    run_hash <- first(prov_tbl$run_hash) %>% str_sub(1, 7)
    run_tag  <- first(prov_tbl$run_tag)
    run_date <- first(prov_tbl$run_date)

    prov_tbl1 <- prov_tbl %>%
      mutate(file_name = basename(file_loc),
             file_dir  = dirname(file_loc) %>% str_replace(path.expand('~'), '~')) %>%
      dplyr::select(sequence, file_name, parent_chunk,
                    file_dir, filetype,
                    uncomm_chgs = uncommitted_changes, commit_hash)

    if(table == 'dt') {
      print(
        DT::datatable(prov_tbl1 %>%
                      arrange(filetype, file_name),
                    caption = sprintf('Provenance summary for run %s (%s): %s (%s)',
                                      run_id, run_hash, run_tag, run_date),
                    rownames = FALSE,
                    class = 'stripe hover compact',
                    options  = list(dom = 'tp'))
      )
    } else if(table == 'kable') {
      print(
        knitr::kable(prov_tbl1 %>%
                       arrange(filetype, file_name),
                     caption = sprintf('Provenance summary for run %s (%s): %s (%s)',
                                       run_id, run_hash, run_tag, run_date),
                     row.names = FALSE)
      )
    } else {
      warning('Unrecognized parameter to "table" argument in prov_wrapup()')
    }
  }

  if(cleanup) {
    message('cleanup = ', cleanup)
    ### delete footers from local prov/ directory
    ftr_list <- list.files(system.file('footer', package = 'provRmd'))
    invisible(
      lapply(ftr_list, function(x) {
        unlink(file.path('prov', x))
      })
    )
  }
}

