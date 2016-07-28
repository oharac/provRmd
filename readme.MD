### provRmd

Tracks provenance when knitting an Rmd document

###To use:

from R console: `devtools::install_github('oharac/provRmd', build_vignettes = TRUE)`

within first code chunk:

```
library(provRmd)
prov_setup()
```

at end of the Rmd (as a standalone code chunk) (also `prov_ftr.Rmd`, `prov_ftr1.Rmd`, 
`prov_ftr2.Rmd` at varying levels of complexity; or design your own):

````
    ``` {r child = 'prov/prov_ftr3.Rmd'}
    ```
````
