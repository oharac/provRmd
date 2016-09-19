### To do:
* how to do memory tracking/profiling
* for non-git-tracked files, at least get some metadata - 
    * file size; created/modified date; owner
* new color for parent script to differentiate from code chunks
* in git_prov(), allow a named list to pass both the filenames and file types
    * e.g. a non-named vector and a filetype arg (or vector of file types?)
    * a named vector in the form of c('input' = '<filename>')
* in commit_prov(), commit message is really long if it includes the full pathname of the running script.  Truncate it to just basename?  or include a couple levels of dirname (to get v2016 and prep folder names perhaps?)

### Instructions/documentation

* use document() then install() to set shit up.
* when pushed to github, should be able to do install_github():

```
  devtools::install_github('oharac/provRmd')
  library(provRmd)
  prov_setup()
```

### how to use build ignore?
### add @return to function docs
### make static footer for .doc or .pdf (test for type of knit env?)
### finish prov_wrapup so it actually works
