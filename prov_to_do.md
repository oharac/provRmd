### To do:
* how to do memory tracking/profiling
* can the footer be added as a command?  
    * direct call to knit child?  
    * seems to be issues with plotting graphs and tables... wtf

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
