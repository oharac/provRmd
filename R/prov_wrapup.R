#' Wrap up provenance tracking
#'
#' This function complets the provenance tracking by capturing the provenance
#' info for the parent script, as well as the session and system info.  This
#' is saved to the provenance log file.  An optional footer is created (by
#' default) including a brief session/system summary, workflow chart, and
#' table of inputs/outputs.  If a footer is desired, this function should be
#' called inside a code chunk with chunk option: \code{results = 'asis'}
#' @param include_summary Should a summary of session and system info be included in the rendered Rmd?
#' @param include_workflow Should a flow chart of the workflow be included in the rendered Rmd?
#' @param include_table Should an output table be included in the rendered Rmd?
#' @param commit_outputs  Argument passed to script_prov(); create a commit for any new files created during this run? (default TRUE)
#' @param plot_dir Direction of workflow plot: \code{'TD'} (default) creates a top-down plot
#' while \code{'LR'} creates a left-right plot.
#' @param include_header Should a "Provenance" header be included?
#' @param header_level At what Markdown header style should the "Provenance" header be
#' assigned? e.g. '#' = header 1, '##' header 2, etc. Default: '#' (header 1 style)
#' @param commit_outputs  Argument passed to script_prov(); create a commit for
#' any new files created during this run? (default TRUE)
#' @export
#' @examples
#' prov_wrapup()

prov_wrapup <- function(include_summary  = TRUE,
                        include_workflow = TRUE,
                        include_table    = TRUE,
                        plot_dir         = 'TD',
                        commit_outputs   = TRUE,
                        include_header   = TRUE,
                        header_level     = '#') {

  # suppressMessages(
    suppressWarnings({
      script_prov_out_df <- script_prov(.provEnv$parent_script_file, commit_outputs = commit_outputs)
    })
  # )

  if(any(include_summary, include_workflow, include_table) & include_header) {
    ### add a header
    cat(paste0(header_level, ' Provenance\n'))
  }

  if(include_summary) {
    # message('... in prov_wrapup.R, generating summary')

    cat(sprintf('- _Run ID: %s (%s); run tag: "%s"_\n',
                script_prov_out_df$run_id,
                script_prov_out_df$run_hash %>% stringr::str_sub(1, 7),
                script_prov_out_df$run_tag))
    cat(sprintf('- _Run elapsed time: %s seconds; run memory usage: %s MB_\n',
                script_prov_out_df$elapsed_time,
                script_prov_out_df$memory_use))
    cat(sprintf('- _System info: _\n    - _%s_\n', script_prov_out_df$msg_sys))
    cat(sprintf('    - _%s_\n', script_prov_out_df$msg_ses))
    cat(sprintf('    - _%s_\n', script_prov_out_df$msg_base_pkgs))
    cat(sprintf('    - _%s_\n', script_prov_out_df$msg_att_pkgs))
  }

  if(include_workflow) {
    # message('... in prov_wrapup.R, generating workflow')
    cat('\n')
    prov_dgr_out <- plot_prov(.provEnv$script_track, plot_dir = plot_dir)

    # message('... in prov_wrapup.R, exporting graph to svg')

    cruft <- capture.output({
      svg <- DiagrammeRsvg::export_svg(DiagrammeR::render_graph(prov_dgr_out))
    })

    # message('... in prov_wrapup.R, printing svg plot')

    print(htmltools::HTML(svg))

    if(require(htmlwidgets)) {
      if(!exists('log_dir', envir = .provEnv)) {
        warning('No provenance directory assigned - no plot will be saved.\n')
      } else {
        # message('... in prov_wrapup.R, saving plot as html widget')

        if(!dir.exists(.provEnv$log_dir)) dir.create(.provEnv$log_dir)
        .provEnv$graph_file <- file.path(.provEnv$log_dir, sprintf('%s_workflow.html', basename(.provEnv$parent_script_file)))
        message('Saving workflow provenance plot to ', .provEnv$graph_file)
        htmlwidgets::saveWidget(DiagrammeR::render_graph(prov_dgr_out),
                                file = .provEnv$graph_file,
                                selfcontained = TRUE)
      }
    }
  }

  if(include_table) {
    # message('... in prov_wrapup.R, generating table output')

    cat('\n')
    prov_tbl <- .provEnv$script_track %>%
      dplyr::mutate(commit_url  = stringr::str_replace(commit_url, 'Previous commit: ', ''),
                    commit_url  = ifelse(commit_url == 'no version control info found', NA, commit_url),
                    commit_hash = ifelse(is.na(commit_url), NA,
                                         sprintf('[%s](%s)', stringr::str_sub(commit_url, -40, -34), commit_url)),
                    commit_hash_short = ifelse(is.na(commit_url), NA,
                                               sprintf('%s', stringr::str_sub(commit_url, -40, -34))))

    run_id   <- first(prov_tbl$run_id)
    run_hash <- first(prov_tbl$run_hash) %>% stringr::str_sub(1, 7)
    run_tag  <- first(prov_tbl$run_tag)
    run_date <- first(prov_tbl$run_date)

    prov_tbl1 <- prov_tbl %>%
      dplyr::mutate(file_name = basename(file_loc),
                    file_dir  = dirname(file_loc) %>% stringr::str_replace(path.expand('~'), '~')) %>%
      dplyr::select(sequence, file_name, parent_chunk,
                    file_dir, filetype,
                    uncomm_chgs = uncommitted_changes, commit_hash_short)

    DT::datatable(prov_tbl1 %>%
                    dplyr::arrange(sequence, desc(filetype), file_name),
                  caption = sprintf('Provenance summary for run %s (%s): %s (%s)',
                                    run_id, run_hash, run_tag, run_date),
                  rownames = FALSE,
                  class = 'stripe hover compact',
                  options  = list(dom = 'tp'))
  }
}
