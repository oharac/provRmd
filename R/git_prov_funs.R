#' Register functions to enable git provenance tracking
#'
#' Utility functions to assist working with git_prov.  Each of these
#' reassigns the original function to a new function in the provRmd
#' namespace.  The new function just adds an automatic call to git_prov()
#' when appropriate.
#'
#' To override these functions, use the double-colon notation  (e.g.
#' utils::write.csv() or readr::read_csv()) to call the function directly
#' from the original package; alternately, add an argument
#' of nogit = TRUE to suppress the git_prov() call.
#' @name git_prov_funs
#'

### functions from base:
#' @rdname git_prov_funs
#' @export
source <- function(source_fn, ..., nogit = FALSE) {
  ### .provEnv$parent_id will change within this script to point to the sourced file.
  ### didn't seem to work with local change - so setting it globally

  if(is.null(knitr:::.knitEnv$input.dir)) {
    tmp <- base::source(file = source_fn, ...)
  } else {
    ### save the current .provEnv$parent_id value temporarily.
    .provEnv$parent_id_temp <- .provEnv$parent_id

    ### reset the .provEnv$parent_id value to the sourced file
    assign('parent_id', source_fn, envir = .provEnv)

    tmp <- base::source(file = source_fn, ...)

    ### reset .provEnv$parent_id back to original value
    assign('parent_id', .provEnv$parent_id_temp, envir = .provEnv)
    if(!is.null(knitr:::.knitEnv$input.dir))
      git_prov(source_fn, filetype = 'sourced_script', nogit)
  }
  return(invisible(tmp))
}

#' @rdname git_prov_funs
#' @export
read.csv <- function(file, stringsAsFactors = FALSE, nogit = FALSE, ...) {
  x <- utils::read.csv(file, ..., stringsAsFactors = stringsAsFactors)
  if(!is.null(knitr:::.knitEnv$input.dir))
    git_prov(file, filetype = 'input', nogit)
  return(x)
}



#' @rdname git_prov_funs
#' @export
write.csv <- function(x, file, row.names = FALSE, nogit = FALSE, ...) {
  utils::write.csv(x, file = file, ..., row.names = row.names)
  if(!is.null(knitr:::.knitEnv$input.dir))
    git_prov(file, filetype = 'output', nogit)
  return(invisible(NULL))
}

### functions from readr:
#' @rdname git_prov_funs
#' @export
read_csv <- function(file, nogit = FALSE, ...) {
  x <- readr::read_csv(file, ...)
  if(!is.null(knitr:::.knitEnv$input.dir))
    git_prov(file, filetype = 'input', nogit)
  return(x)
}

#' @rdname git_prov_funs
#' @export
write_csv <- function(x, path, nogit = FALSE, ...) {
  readr::write_csv(x, path = path, ...)
  if(!is.null(knitr:::.knitEnv$input.dir))
    git_prov(path, filetype = 'output', nogit)
  return(invisible(x))
}

### functions to read/write shapefiles:
#' @rdname git_prov_funs
#' @export
readOGR <- function(dsn, layer, stringsAsFactors = FALSE, nogit = FALSE, ...) {
  x <- rgdal::readOGR(dsn = dsn, layer = layer, ..., stringsAsFactors = stringsAsFactors)
  if(!is.null(knitr:::.knitEnv$input.dir))
    git_prov(sprintf('%s/%s.shp', dsn, layer), filetype = 'input', nogit)
  return(x)
}

#' @rdname git_prov_funs
#' @export
writeOGR <- function(obj, dsn, layer, driver = 'ESRI Shapefile', nogit = FALSE, ...) {
  rgdal::writeOGR(obj, dsn = dsn, layer = layer, ..., driver = driver)
  if(!is.null(knitr:::.knitEnv$input.dir))
    git_prov(sprintf('%s/%s.shp', dsn, layer), filetype = 'output', nogit)
  return(invisible(obj))
}

#' @rdname git_prov_funs
#' @export
readShapePoly <- function(fn, nogit = FALSE, ...) {
  x <- maptools::readShapePoly(fn, ...)
  if(!is.null(knitr:::.knitEnv$input.dir))
    git_prov(paste(fn, '.shp', sep = ''), filetype = 'input', nogit)
  return(x)
}

#' @rdname git_prov_funs
#' @export
writePolyShape <- function(x, fn, nogit = FALSE, ...) {
  maptools::writePolyShape(x, fn, ...)
  if(!is.null(knitr:::.knitEnv$input.dir))
    git_prov(paste(fn, '.shp', sep = ''), filetype = 'output', nogit)
  return(invisible(x))
}

### functions to read/write rasters:
#' @rdname git_prov_funs
#' @export
raster <- function(x, nogit = FALSE, ...) {
  if(is.character(x) & !is.null(knitr:::.knitEnv$input.dir)) {
    y <- raster::raster(x, ...)
    git_prov(x, filetype = 'input', nogit)
    return(y)
  } else {
    return(raster::raster(x, ...))
  }
}

#' @rdname git_prov_funs
#' @export
brick <- function(x, nogit = FALSE, ...) {
  if(is.character(x) & !is.null(knitr:::.knitEnv$input.dir)) {
    y <- raster::brick(x, ...)
    git_prov(x, filetype = 'input', nogit)
    return(y)
  } else {
    return(raster::brick(x, ...))
  }
}

#' @rdname git_prov_funs
#' @export
stack <- function(x, nogit = FALSE, ...) {
  if(is.character(x[[1]]) & !is.null(knitr:::.knitEnv$input.dir)) {
    y <- raster::stack(x, ...)
    git_prov(x, filetype = 'input', nogit)
    return(y)
  } else {
    return(raster::stack(x, ...))
  }
}

#' @rdname git_prov_funs
#' @export
writeRaster <- function(x, filename, bylayer = FALSE, nogit = FALSE, ...) {
  raster::writeRaster(x, filename, ..., bylayer = bylayer)
  if(raster::nlayers(x) & bylayer == TRUE & length(x) == 1 & !is.null(knitr:::.knitEnv$input.dir)) {
    message('Using writeRaster with bylayer = TRUE works best if using a vector of filenames')
  }

  if(!is.null(knitr:::.knitEnv$input.dir)) {
      git_prov(filename, filetype = 'output', nogit)
  }
  return(invisible(x))
}

#' @rdname git_prov_funs
#' @export
gdal_rasterize <- function(src_datasource, dst_filename, ..., nogit = FALSE) {
  gdalUtils::gdal_rasterize(...)
    if(!is.null(knitr:::.knitEnv$input.dir)) {
      git_prov(src_datasource, filetype = 'input', nogit)
      git_prov(dst_filename, filetype = 'output', nogit)
    }
  return(invisible(NULL))
}

#' @rdname git_prov_funs
#' @export
rasterize <- function(x, y, filename = '', nogit = FALSE, ...) {
  z <- raster::rasterize(x, y, ..., filename = filename)
  if(filename != '' & !is.null(knitr:::.knitEnv$input.dir)) {
    git_prov(filename, filetype = 'output', nogit)
  }
  return(z)
}


