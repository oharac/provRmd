% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/script_prov.R
\name{script_prov}
\alias{script_prov}
\title{Assemble script provenance information}
\usage{
script_prov(script_file = .provEnv$parent_script_file,
  tag = .provEnv$run_tag, commit_outputs = FALSE)
}
\arguments{
\item{script_file}{This should be the name of the parent script, to enable
a call to \code{git_prov()}.  The default is \code{.provEnv$parent_script_file}
as set up in \code{prov_setup()}.}

\item{tag}{An optional run tag; defaults to the run tag set in prov_setup().}

\item{commit_outputs}{Should a git commit be created for the output files?
Defaults to FALSE; newly created outputs will show as not git-controlled,
while modified outputs will show as changed since last commit.}
}
\value{
Returns (invisibly) a named list of characters containing system
and session info for the R Markdown knitting session; list includes the
following items: run_id, run_hash, run_tag, elapsed_time, memory_use,
msg_sys, msg_ses, msg_git, msg_base_pkgs, msg_att_pkgs
}
\description{
This function gathers the information from all provenance tracking within
the script and adds system and session info, as well as RDF predicate info.
}
\examples{
script_prov()
}
