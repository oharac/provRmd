#' Wrap up provenance tracking
#'
#' This function complets the provenance tracking by capturing the provenance
#' info for the parent script, as well as the session and system info.  This
#' is saved to the provenance log file.  An optional footer is created (by
#' default) including a brief session/system summary, workflow chart, and
#' table of inputs/outputs.  If a footer is desired, this function should be
#' called inside a code chunk with chunk option: \code{results = 'asis'}
#' @param include_summary Include a summary including session and system info in the rendered Rmd.
#' @param include_workflow Create a flow chart of the workflow as determined by
#' the provenance tracking, and insert into rendered Rmd.
#' @param plot_dir Direction of workflow plot: \code{'TD'} (default) creates a top-down plot
#' while \code{'LR'} creates a left-right plot
#' @param include_table Create an output table in the rendered Rmd.
#' @param commit_outputs  Argument passed to script_prov(); create a commit for any new files created during this run? (default TRUE)
#' @export
#' @examples
#' prov_wrapup()

prov_wrapup <- function(include_summary  = TRUE,
                        include_workflow = TRUE,
                        plot_dir = 'TD',
                        include_table    = TRUE,
                        commit_outputs = TRUE) {

  suppressMessages(suppressWarnings({
    script_prov_out_df <- script_prov(.prov_parent_script_file, commit_outputs = commit_outputs)
  }))

  if(any(include_summary, include_workflow, include_table)) {
    ### add a header
    cat('# Provenance\n')
  }

  if(include_summary) {
    cat(sprintf('- _Run ID: %s (%s); run tag: "%s"_\n',
                script_prov_out_df$run_id,
                script_prov_out_df$run_hash %>% str_sub(1, 7),
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
    cat('\n')
    prov_dgr_out <- plot_prov(.script_track, plot_dir = plot_dir)

    cruft <- capture.output({
      svg <- DiagrammeRsvg::export_svg(DiagrammeR::render_graph(prov_dgr_out))
    })
    print(htmltools::HTML(svg))
  }

  if(include_table) {
    cat('\n')
    prov_tbl <- .script_track %>%
      dplyr::mutate(commit_url  = str_replace(commit_url, 'Previous commit: ', ''),
                    commit_url  = ifelse(commit_url == 'no version control info found', NA, commit_url),
                    commit_hash = ifelse(is.na(commit_url), NA,
                                         sprintf('[%s](%s)', str_sub(commit_url, -40, -34), commit_url)),
                    commit_hash_short = ifelse(is.na(commit_url), NA,
                                               sprintf('%s', str_sub(commit_url, -40, -34))))

    run_id   <- first(prov_tbl$run_id)
    run_hash <- first(prov_tbl$run_hash) %>% str_sub(1, 7)
    run_tag  <- first(prov_tbl$run_tag)
    run_date <- first(prov_tbl$run_date)

    prov_tbl1 <- prov_tbl %>%
      dplyr::mutate(file_name = basename(file_loc),
                    file_dir  = dirname(file_loc) %>% str_replace(path.expand('~'), '~')) %>%
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
