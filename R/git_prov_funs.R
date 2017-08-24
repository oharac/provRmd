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
source <- function(source_fn, ..., nogit = FALSE, not_tracked = FALSE) {
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
      git_prov(source_fn, filetype = 'sourced_script', nogit, not_tracked)
  }
  return(invisible(tmp))
}

#' @rdname git_prov_funs
#' @export
read.csv <- function(file, stringsAsFactors = FALSE, nogit = FALSE, not_tracked = FALSE, ...) {
  x <- utils::read.csv(file, ..., stringsAsFactors = stringsAsFactors)
  if(!is.null(knitr:::.knitEnv$input.dir))
    git_prov(file, filetype = 'input', nogit, not_tracked)
  return(x)
}



#' @rdname git_prov_funs
#' @export
write.csv <- function(x, file, row.names = FALSE, nogit = FALSE, not_tracked = FALSE, ...) {
  utils::write.csv(x, file = file, ..., row.names = row.names)
  if(!is.null(knitr:::.knitEnv$input.dir))
    git_prov(file, filetype = 'output', nogit, not_tracked)
  return(invisible(NULL))
}

### functions from readr:
#' @rdname git_prov_funs
#' @export
read_csv <- function(file, nogit = FALSE, not_tracked = FALSE, ...) {
  x <- readr::read_csv(file, ...)
  if(!is.null(knitr:::.knitEnv$input.dir))
    git_prov(file, filetype = 'input', nogit, not_tracked)
  return(x)
}

#' @rdname git_prov_funs
#' @export
write_csv <- function(x, path, nogit = FALSE, not_tracked = FALSE, ...) {
  readr::write_csv(x, path = path, ...)
  if(!is.null(knitr:::.knitEnv$input.dir))
    git_prov(path, filetype = 'output', nogit, not_tracked)
  return(invisible(x))
}

### functions to read/write shapefiles:
#' @rdname git_prov_funs
#' @export
readOGR <- function(dsn, layer, stringsAsFactors = FALSE, nogit = FALSE, not_tracked = FALSE, ...) {
  x <- rgdal::readOGR(dsn = dsn, layer = layer, ..., stringsAsFactors = stringsAsFactors)
  if(!is.null(knitr:::.knitEnv$input.dir))
    git_prov(sprintf('%s/%s.shp', dsn, layer), filetype = 'input', nogit, not_tracked)
  return(x)
}

#' @rdname git_prov_funs
#' @export
writeOGR <- function(obj, dsn, layer, driver = 'ESRI Shapefile', nogit = FALSE, not_tracked = FALSE, ...) {
  rgdal::writeOGR(obj, dsn = dsn, layer = layer, ..., driver = driver)
  if(!is.null(knitr:::.knitEnv$input.dir))
    git_prov(sprintf('%s/%s.shp', dsn, layer), filetype = 'output', nogit, not_tracked)
  return(invisible(obj))
}

#' @rdname git_prov_funs
#' @export
readShapePoly <- function(fn, nogit = FALSE, not_tracked = FALSE, ...) {
  x <- maptools::readShapePoly(fn, ...)
  if(!is.null(knitr:::.knitEnv$input.dir))
    git_prov(paste(fn, '.shp', sep = ''), filetype = 'input', nogit, not_tracked)
  return(x)
}

#' @rdname git_prov_funs
#' @export
writePolyShape <- function(x, fn, nogit = FALSE, not_tracked = FALSE, ...) {
  maptools::writePolyShape(x, fn, ...)
  if(!is.null(knitr:::.knitEnv$input.dir))
    git_prov(paste(fn, '.shp', sep = ''), filetype = 'output', nogit, not_tracked)
  return(invisible(x))
}

### functions to read/write rasters:
#' @rdname git_prov_funs
#' @export
raster <- function(x, nogit = FALSE, not_tracked = FALSE, ...) {
  if(is.character(x) & !is.null(knitr:::.knitEnv$input.dir)) {
    y <- raster::raster(x, ...)
    git_prov(x, filetype = 'input', nogit, not_tracked)
    return(y)
  } else {
    return(raster::raster(x, ...))
  }
}

#' @rdname git_prov_funs
#' @export
brick <- function(x, nogit = FALSE, not_tracked = FALSE, ...) {
  if(is.character(x) & !is.null(knitr:::.knitEnv$input.dir)) {
    y <- raster::brick(x, ...)
    git_prov(x, filetype = 'input', nogit, not_tracked)
    return(y)
  } else {
    return(raster::brick(x, ...))
  }
}

#' @rdname git_prov_funs
#' @export
stack <- function(x, nogit = FALSE, not_tracked = FALSE, ...) {
  if(is.character(x[[1]]) & !is.null(knitr:::.knitEnv$input.dir)) {
    y <- raster::stack(x, ...)
    git_prov(x, filetype = 'input', nogit, not_tracked)
    return(y)
  } else {
    return(raster::stack(x, ...))
  }
}

#' @rdname git_prov_funs
#' @export
writeRaster <- function(x, filename, bylayer = FALSE, nogit = FALSE, not_tracked = FALSE, ...) {
  raster::writeRaster(x, filename, ..., bylayer = bylayer)
  if(raster::nlayers(x) & bylayer == TRUE & length(x) == 1 & !is.null(knitr:::.knitEnv$input.dir)) {
    message('Using writeRaster with bylayer = TRUE works best if using a vector of filenames')
  }

  if(!is.null(knitr:::.knitEnv$input.dir)) {
      git_prov(filename, filetype = 'output', nogit, not_tracked)
  }
  return(invisible(x))
}

#' @rdname git_prov_funs
#' @export
gdal_rasterize <- function(src_datasource, dst_filename, ..., nogit = FALSE, not_tracked = FALSE) {
  gdalUtils::gdal_rasterize(...)
    if(!is.null(knitr:::.knitEnv$input.dir)) {
      git_prov(src_datasource, filetype = 'input', nogit, not_tracked)
      git_prov(dst_filename, filetype = 'output', nogit, not_tracked)
    }
  return(invisible(NULL))
}

#' @rdname git_prov_funs
#' @export
rasterize <- function(x, y, filename = '', nogit = FALSE, not_tracked = FALSE, ...) {
  z <- raster::rasterize(x, y, ..., filename = filename)
  if(filename != '' & !is.null(knitr:::.knitEnv$input.dir)) {
    git_prov(filename, filetype = 'output', nogit, not_tracked)
  }
  return(z)
}

#' @rdname git_prov_funs
#' @export
ggsave <- function(filename, ..., nogit = FALSE, not_tracked = FALSE) {
  z <- ggplot2::ggsave(filename = filename, ...)
  if(!is.null(knitr:::.knitEnv$input.dir)) {
    git_prov(filename, filetype = 'plot', nogit, not_tracked)
  }
  return(invisible(z))
}


