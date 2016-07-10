### To do:

* add sequence tracking
    * how to deal with code chunks in sequence? 
    * maybe: just set sequence = 0 (or 1) manually in script_prov
* how to do memory tracking/profiling
* can the footer be added as a command?  
    * direct call to knit child

### Instructions/documentation

* use document() then install() to set shit up.
* when pushed to github, should be able to do install_github():


```
  devtools::install_github('oharac/provRmd')
  library(provRmd)
  prov_setup()
```

