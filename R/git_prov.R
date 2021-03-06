#' Access git provenance information
#'
#' This function takes a filename and reads its git log, strips to its
#' most recent commit, and appends it to the provenance tracking dataframe.
#' If a prov log exists (and \code{nolog} is FALSE), this information is
#' added to the log.
#' @param filename A valid file name (relative or absolute) or vector of file names
#' @param filetype The role of this file within this context: 'input',
#' 'output', 'parent_script', 'sourced_script', or 'plot'.  Defaults to 'input'.
#' @param nogit Should this file be skipped for git provenance purposes? Defaults to FALSE.
#' @param nolog Should this git provenance information be omitted from
#' the log file? Defaults to FALSE.
#' @param not_tracked Files used in a script but not tracked in Git can be identified
#' by a non-response from a \code{git log} command; however, for files known to be not
#' tracked in this repo (e.g. on a remote server or in a different repository) it may be
#' desirable to simply skip the \code{git log} and mark it right away as not tracked,
#' while still including it in the prov log and workflow, etc.
#' Defaults to FALSE.
#'
#' @export
#' @examples
#' git_prov('raw_data.csv', filetype = 'input')
#' git_prov('raw_data.csv', filetype = 'input', nogit = TRUE)
#'   ### will skip provenance tracking for this file
#' git_prov('raw_data.csv', filetype = 'input', nolog = TRUE)
#'   ### will gather git commit info for this file, but will not add to the
#'   ### git tracking data frame.  This can be useful if you just want to
#'   ### peek at the commit info.

git_prov <- function(filename,
                     filetype = c('input', 'output', 'parent_script', 'sourced_script', 'plot')[1],
                     nogit = FALSE,
                     nolog = FALSE,
                     not_tracked = FALSE) {

  ### Make sure we're operating in a knir environment
  if(is.null(knitr:::.knitEnv$input.dir)) {
    message('git_prov() only operates within the context of knitting an Rmd.')
    return(invisible()) ### if not being knitted, escape immediately
  }

  ### skip out if explicitly told to not track git for this file
  if(nogit == TRUE) {
    return(invisible()) ### skip out of git_prov if nogit == TRUE
  }

  for(git_file in filename) { ### git_file <- filename[1]

    ### attempt to read git_info for script or input
    if(!not_tracked) {
      message('Checking git provenance for ', git_file, '...')
      suppressWarnings({
        git_info <- system2('git', args = sprintf('log --follow %s', git_file), stderr = FALSE, stdout = TRUE)[1:3]
      })
    } else {
      git_info <- c(NA, NA, NA)  ### hey hey hey, goodbye
    }

    if(not_tracked) {

      message(sprintf('File `%s`: not_tracked == TRUE, skipping git log call', git_file))
      git_commit_url  <- 'no version control info: file not tracked in Git'
      git_uncommitted <- NA

    } else if(is.na(git_info[1])) {    ### if git_info[1] is NA, commit info not found.

      message(sprintf('File `%s`: git commit info unavailable.  Not version-controlled in Git?', git_file))
      git_commit_url  <- 'no version control info found'
      git_uncommitted <- NA

    } else {

      ### git_info[1] is not NA, so commit info is available.
      ### find whether uncommitted differences in this file.
      ### in stringr::str_detect, '$' makes sure git_file string is at end of line.

      suppressWarnings({
        git_diff <- system2('git', args = 'diff HEAD', stderr = TRUE, stdout = TRUE)
      })

      git_diff_check <- which(stringr::str_detect(git_diff, sprintf('%s$', basename(git_file))) &
                                stringr::str_detect(git_diff, 'diff --git'))
      git_uncommitted <- length(git_diff_check) > 0

      ### convert commit info to a hyperlinked commit info string.
      git_loc  <- system2('git', args = 'config --get remote.origin.url', stderr = TRUE, stdout = TRUE)

      if(filetype == 'output')
        git_commit_url <- sprintf('Previous commit: %s/commit/%s', sub('.git', '', git_loc, fixed = TRUE), gsub('commit ', '', git_info[1]))
      else
        git_commit_url <- sprintf('%s/commit/%s', sub('.git', '', git_loc, fixed = TRUE), gsub('commit ', '', git_info[1]))

      git_link <- sprintf('commit [%s](%s)', gsub('commit ', '', git_info[1]), git_commit_url)

      message(sprintf('File `%s`: most recent commit info: %s; uncommitted changes = %s', git_file,
                      paste(git_info[1], git_info[2], git_info[3], collapse = '; '), git_uncommitted))
    }

    git_file <- git_file %>%
      # stringr::str_replace(dir_M, 'Mazu:') # %>%
      stringr::str_replace(path.expand('~'), '~')

    chunk_name <- knitr::opts_current$get("label")
    if(length(chunk_name) == 0) chunk_name <- 'not knitted'
    chunk_name <- stringr::str_replace_all(chunk_name, ' ', '_')

    git_df <- data.frame('sequence'      = ifelse(filetype == 'parent_script', 0, .provEnv$sequence),
                         'parent_fn'     = .provEnv$parent_id,
                         'parent_chunk'  = chunk_name,
                         'file_loc'      = git_file,
                         'filetype'      = tolower(filetype),
                         'commit_url'    = git_commit_url,
                         'commit_author' = sub('Author: ', '', git_info[2]),
                         'commit_date'   = sub('Date: ', '', git_info[3]),
                         'uncommitted_changes' = as.logical(git_uncommitted))

    ### increment the provenance sequence index
    assign('sequence', .provEnv$sequence + 1, envir = .provEnv)

    ### Binds git_df to the global .provEnv$prov_track variable, and reassigns it to the higher environment.
    ### nolog argument to git_prov allows to check git info without logging it (for peek_csv() below)
    if(!nolog) {
      assign('prov_track', .provEnv$prov_track %>% rbind(git_df), envir = .provEnv)
    }
  }

  return(invisible())
}

