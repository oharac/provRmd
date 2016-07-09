### To do:

* use .var notation to hide the variables?  it's annoying to fill up the environment with crap
* identify code chunks in parent Rmd and use those as identifiers for nodes (in conjunction with file name)
    * knitr::opts_current$get("label") is the beast; what to use as the separator? $? @?

* use document() then install() to set shit up.
* when pushed to github, should be able to do install_github():

```
  devtools::install_github('oharac/provRmd')
  library(provRmd)
  prov_setup()
```

