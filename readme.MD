### provRmd

Tracks provenance using Git version control when knitting an Rmd document

### Quick start:

* From R console: `devtools::install_github('oharac/provRmd', build_vignettes = TRUE)`
* Within first code chunk:

```
### if using any of these packages, load first; provRmd masks some of the
### read/write functions to automate git provenance collection
library(readr)
library(raster)
library(rgdal)

### here is the real meat:
library(provRmd)
prov_setup()
```

* Then use standard R markdown to code your script, as per usual.  Best results
if you clearly label your code chunks and write outputs from each, then read
as inputs into the next chunk (or register as inputs manually using `git_prov()`)
    * Note: calling namespaces (e.g. `raster::raster()`) will circumvent provenance
logging.  This may be a good thing or bad thing... 
    * If you want to ensure provenance logging, use `provRmd::raster()` (for example).
* At end of the Rmd, include this within a code chunk to process the git provenance
dataframe and capture session and system info, and then create a formatted
footer for the knitted R Markdown output.  The options for this
code chunk need to include `results = 'asis'` for the footer to render
properly in R Markdown.

````
prov_wrapup()
````

### Other functions

* `script_prov()` is a minimal version of `prov_wrapup()` that simply processes
the provenance and writes a provenance log.  It does not create a footer.
* `git_prov()` is the function that captures git commit info for any files
used as inputs, outputs, or sourced scripts.
* `plot_prov()` (called after `script_prov()`) converts a provenance log into
a workflow diagram.

See help files for each function for descriptions of arguments and returned values.

See the vignette (`vignette('provRmd')`) for more details and examples of use.
