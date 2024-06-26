#' Subset layers/attributes of `Spat*` objects
#'
#' @description
#'
#' Select (and optionally rename) attributes/layers in `Spat*` objects, using a
#' concise mini-language. See **Methods**.
#'
#' @export
#' @rdname select.Spat
#' @name select.Spat
#'
#' @importFrom dplyr select
#'
#' @param .data A `SpatRaster` created with [terra::rast()] or a `SpatVector`
#'   created with [terra::vect()].
#' @param ... <[`tidy-select`][dplyr::dplyr_tidy_select]> One or more unquoted
#'   expressions separated by commas. Layer/attribute names can be used as if
#'   they were positions in the `Spat*` object, so expressions like `x:y` can
#'   be used to select a range of layers/attributes.
#'
#' @return A `Spat*` object  of the same class than `.data`. See **Methods**.
#'
#' @seealso [dplyr::select()], [terra::subset()]
#'
#' @family single table verbs
#' @family dplyr.cols
#' @family dplyr.methods
#'
#' @section \CRANpkg{terra} equivalent:
#'
#' [terra::subset()]
#'
#' @section Methods:
#'
#' Implementation of the **generic** [dplyr::select()] function.
#'
#' ## `SpatRaster`
#'
#' Select (and rename) layers of a `SpatRaster`. The result is a `SpatRaster`
#' with the same extent, resolution and crs than `.data`. Only the number (and
#' possibly the name) of layers is modified.
#'
#' ## `SpatVector`
#'
#' The result is a `SpatVector` with the selected (and possibly renamed)
#' attributes on the function call.
#'
#'
#' @examples
#'
#' library(terra)
#'
#' # SpatRaster method
#'
#' spatrast <- rast(
#'   crs = "EPSG:3857",
#'   nrows = 10,
#'   ncols = 10,
#'   extent = c(100, 200, 100, 200),
#'   nlyr = 6,
#'   vals = seq_len(10 * 10 * 6)
#' )
#'
#' spatrast %>% select(1)
#'
#' # By name
#' spatrast %>% select(lyr.1:lyr.4)
#'
#' # Rename
#' spatrast %>% select(a = lyr.1, c = lyr.6)
#'
#' # SpatVector method
#'
#' f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
#'
#' v <- vect(f)
#'
#' v
#'
#' v %>% select(1, 3)
#'
#' v %>% select(iso2, name2 = cpro)
select.SpatRaster <- function(.data, ...) {
  # Create a template df for assessing results
  # Use only first cell for speed up
  df <- as.data.frame(.data[1], na.rm = FALSE)

  # Convert factors/chars to nums
  is_numeric <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  df[!is_numeric] <- lapply(df[!is_numeric], as.numeric)

  df[1, ] <- seq_len(ncol(df))


  result <- dplyr::select(df, ...)

  # Now translate from result to terra

  final_rast <- terra::subset(.data, as.integer(result))

  # Set new names if anything has changed
  names(final_rast) <- names(result)

  return(final_rast)
}

#' @export
#' @rdname select.Spat
select.SpatVector <- function(.data, ...) {
  # Use tibble method
  tbl <- as_tibble(.data)
  selected <- dplyr::select(tbl, ...)

  # Bind and groups
  vend <- cbind(.data[, 0], selected)

  vend <- group_prepare_spat(vend, selected)

  return(vend)
}

#' @export
dplyr::select
