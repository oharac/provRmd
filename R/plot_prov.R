#' Access git provenance information
#'
#' This function takes a filename and reads its git log, strips to its
#' most recent commit, adds a line to prov_trackfor this file, and returns
#' a dataframe with git provenance information.
#' @param df A dataframe of a provenance log file (i.e. \code{.provEnv$script_track}
#' created by call to \code{script_prov()})
#' @param plot_dir Should the graph plot top-to-bottom ('TB') or
#' left-to-right ('LR')?  Defaults to 'LR'.
#' @return Returns a graph object created by the \code{{DiagrammeR}} package.  This
#' should be passed to a call to \code{DiagrammeR::render_graph()}
#' @export
#' @examples
#' prov_graph <- plot_prov(.provEnv$script_prov)
#' DiagrammeR::render_graph(prov_graph)

plot_prov <- function(df = .provEnv$script_track, plot_dir = c('TB', 'LR')[1]) {

  if(is.null(knitr:::.knitEnv$input.dir)) {
    message('plot_prov() only operates within the context of knitting an Rmd.')
    return(invisible()) ### if not being knitted, escape immediately
  }

  df <- df %>%
    dplyr::filter(run_id == max(run_id)) %>%
    dplyr::mutate(from_loc = rdf_subject,
                  to_loc   = rdf_object,
                  rel      = rdf_predicate)
  ### NOTE: from subject to object is active; this will ensure that the
  ### sequence goes down the page.  But predicates are usually
  ### passive voice, so e.g. subject WASGENERATEDBY.  So: edges
  ### should have direction set to reverse?
  # message('... in plot_prov.R, setting up shapes_df')

  shapes_df <- data.frame(
                 filetype  = c('input',         'output',        'parent_script', 'parent_chunk',   'sourced_script', 'plot'),
                 shape     = c('oval',          'oval',          'rectangle',     'rectangle',      'rectangle',      'diamond'),
                 color     = c( hsv(.6, .5, .7), hsv(.3, .5, .7), hsv(.1, .5, .7), hsv(.1, .5, .7), hsv(.1, .5, .7), hsv(.8, .5, .7)),
                 fillcolor = c( hsv(.6, .3, .9), hsv(.3, .4, .9), hsv(.1, .4, .9), hsv(.1, .4, .9), hsv(.15, .2, 1), hsv(.8, .3, .9)))
    # fontcolor, fontname


  # message('... in plot_prov.R, setting up nodes_id_df')

  nodes_id_df <- df %>%
    dplyr::arrange(sequence, file_loc) %>%
    dplyr::select(node_name = file_loc) %>%
    dplyr::distinct() %>%
    dplyr::mutate(node_id = 1:n())


  # message('... in plot_prov.R, setting up nodes_df')
  ### setting up nodes df -----
  nodes_df <- df %>%
    dplyr::select(file_loc, parent_chunk, filetype, commit_url, uncommitted_changes) %>%
    dplyr::mutate(node_name = file_loc,
                  label     = basename(file_loc),
                  tooltip   = commit_url,
                  style     = 'filled',
                  fontsize  = 6,
                  fontcolor = 'grey20',
                  fontname  = 'Helvetica',
                  penwidth  = 2) %>%
    dplyr::left_join(nodes_id_df %>%
                       rename(id = node_id), by = 'node_name') %>%
    dplyr::left_join(shapes_df, by = 'filetype') %>%
    dplyr::distinct()

  # print(nodes_df)

  ### special cases: no git tracking, or uncommitted changes
  nodes_df <- nodes_df %>%
    dplyr::mutate(color    = ifelse(uncommitted_changes == TRUE, 'yellow', color),
                  penwidth = ifelse(uncommitted_changes == TRUE,     3,    penwidth),
                  color    = ifelse(stringr::str_detect(commit_url, 'no version control'), 'red', color),
                  penwidth = ifelse(stringr::str_detect(commit_url, 'no version control'),   3,   penwidth))

  ### if a file is both input and output, select only output (for plot attributes)
  nodes_df <- nodes_df %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(in_out = any(filetype == 'output') & any(filetype == 'input')) %>%
    dplyr::filter(!(in_out & filetype == 'input')) %>%
    dplyr::select(-in_out) %>%
    dplyr::ungroup()


  ### setting up arrows_df and edges_df -----
  arrows_df <- data.frame(
                 rel   = c('prov:used', 'prov:wasGeneratedBy', 'prov:wasExecutedBy'),
                 color = c( hsv(.6, .5, .4),    hsv(.3, .5, .4),       hsv(.1, .5, .4)))

  # message('... in plot_prov.R, setting up edges_df')
  edges_df <- df %>%
    dplyr::select(from_loc, to_loc, rel) %>%
    dplyr::filter(from_loc != to_loc) %>%
    dplyr::left_join(nodes_id_df %>%
                       rename(from = node_id), by = c('from_loc' = 'node_name')) %>%
    dplyr::left_join(nodes_id_df %>%
                       rename(to = node_id), by = c('to_loc' = 'node_name')) %>%
    dplyr::mutate(label = stringr::str_replace(rel, 'prov:', ''),
                  #dir       = 'back',
                  tooltip   = rel,
                  fontsize  = 6,
                  fontcolor = 'grey20',
                  fontname  = 'Helvetica',
                  penwidth  = 1,
                  #arrowhead = 'diamond', # if dir is 'back', use arrowtail
                  #arrowtail = 'box',
                  arrowsize = .5) %>%
    dplyr::left_join(arrows_df, by = 'rel') %>%
    dplyr::distinct()

  ### generating graph -----
  # message('... in plot_prov.R, generating graph ', getwd())
  # write_csv(nodes_df, 'nodes.csv')
  # write_csv(edges_df, 'edges.csv')

  prov_gr <- DiagrammeR::create_graph(nodes_df = nodes_df,
                                      edges_df = edges_df
                                      # attr_theme = sprintf('rankdir = %s', plot_dir)
                                      # node_attrs   = NULL,
                                      # edge_attrs   = NULL,
                                      # directed     = TRUE,
                                      # graph_name   = NULL,
                                      # graph_time   = NULL,
                                      # graph_tz     = NULL,
                                      # generate_dot = TRUE
                                      ) %>%
    DiagrammeR::add_global_graph_attrs(
      attr      = c("layout", "rankdir"),
      value     = c("dot",    plot_dir),
      attr_type = c("graph",  "graph"))


  # message('... in plot_prov.R, returning plot')
  return(invisible(prov_gr))

}


### organize better
### color code re: committed/unchanged = green, committed/changed = yellow, uncommitted = red?
### link to file on github or something?
### proper RDF predicates, shapes, etc
### info on plot: run tag, date, name; sys.info, session.info
### use md5 tags for unique run IDs (vs human readable)
