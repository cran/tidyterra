#' Fill in missing values with previous or next value on a `SpatVector`
#'
#' @description
#'
#' Fills missing values in selected columns using the next or previous entry.
#' This is useful in the common output format where values are not repeated,
#' and are only recorded when they change.
#'
#'
#' @export
#' @importFrom tidyr fill
#'
#' @family tidyr.missing
#' @family tidyr.methods
#'
#' @rdname fill.SpatVector
#' @name fill.SpatVector
#'
#'
#' @param data A `SpatVector`.
#' @param ... <[`tidy-select`][tidyr::tidyr_tidy_select]> Columns to fill.
#' @inheritParams tidyr::fill
#'
#' @seealso [tidyr::fill()]
#'
#' @return A `SpatVector` object.
#'
#' @section Methods:
#'
#' Implementation of the **generic** [tidyr::fill()] function for `SpatVector`.
#'
#' @section Grouped `SpatVector`:
#'
#' With grouped `SpatVector` created by [group_by.SpatVector()], `fill()` will
#' be applied _within_ each group, meaning that it won't fill across group
#' boundaries.
#'
#' @examples
#' library(dplyr)
#'
#' lux <- terra::vect(system.file("ex/lux.shp", package = "terra"))
#'
#' # Leave some blanks for demo purporses
#'
#' lux_blnk <- lux %>%
#'   mutate(NAME_1 = if_else(NAME_1 != NAME_2, NA, NAME_2))
#'
#'
#' as_tibble(lux_blnk)
#'
#' # `fill()` defaults to replacing missing data from top to bottom
#' lux_blnk %>%
#'   fill(NAME_1) %>%
#'   as_tibble()
#'
#'
#' # direction = "up"
#' lux_blnk %>%
#'   fill(NAME_1, .direction = "up") %>%
#'   as_tibble()
#'
#' # Grouping and downup - will restore the initial state
#' lux_blnk %>%
#'   group_by(ID_1) %>%
#'   fill(NAME_1, .direction = "downup") %>%
#'   as_tibble()
#'
fill.SpatVector <- function(data, ...,
                            .direction = c("down", "up", "downup", "updown")) {
  # Use own method
  tbl <- as_tibble(data)
  filled <- tidyr::fill(tbl, ..., .direction = .direction)

  # Bind and prepare
  vend <- cbind(data[, 0], filled)
  vend <- group_prepare_spat(vend, filled)

  return(vend)
}

#' @export
tidyr::fill
