% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arrange-SpatVector.R
\name{arrange.SpatVector}
\alias{arrange.SpatVector}
\title{Order SpatVectors using column values}
\usage{
\method{arrange}{SpatVector}(.data, ..., .by_group = FALSE)
}
\arguments{
\item{.data}{A SpatVector created with \code{\link[terra:vect]{terra::vect()}}.}

\item{...}{<\code{\link[dplyr:arrange]{data-masking}}> Variables, or functions of
variables. Use \code{\link[dplyr:desc]{dplyr::desc()}} to sort a variable in descending order.}

\item{.by_group}{If \code{TRUE}, will sort first by grouping variable. Applies to
grouped SpatVectors only.}
}
\value{
A SpatVector object.
}
\description{
\code{arrange()} orders the geometries of a SpatVector by the values of
selected columns.
}
\section{terra equivalent}{


\code{\link[terra:sort]{terra::sort()}}
}

\section{Methods}{


Implementation of the \strong{generic} \code{\link[dplyr:arrange]{dplyr::arrange()}} function for
SpatVectors.
}

\examples{

library(terra)
library(dplyr)

v <- vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))


# Single variable

v \%>\%
  arrange(desc(iso2))


# Two variables
v \%>\%
  mutate(even = as.double(cpro) \%\% 2 == 0, ) \%>\%
  arrange(desc(even), desc(iso2))


# With new variables
v \%>\%
  mutate(area_geom = terra::expanse(v)) \%>\%
  arrange(area_geom)
}
\seealso{
\code{\link[dplyr:arrange]{dplyr::arrange()}}

Other single table verbs: 
\code{\link{filter.Spat}},
\code{\link{mutate.Spat}},
\code{\link{rename.Spat}},
\code{\link{select.Spat}},
\code{\link{slice.Spat}},
\code{\link{summarise.SpatVector}()}

Other dplyr verbs that operate on rows:
\code{\link{distinct.SpatVector}()},
\code{\link{filter.Spat}},
\code{\link{slice.Spat}}

Other dplyr methods:
\code{\link{bind_cols.SpatVector}},
\code{\link{bind_rows.SpatVector}},
\code{\link{count.SpatVector}()},
\code{\link{distinct.SpatVector}()},
\code{\link{filter-joins.SpatVector}},
\code{\link{filter.Spat}},
\code{\link{glimpse.Spat}},
\code{\link{group-by.SpatVector}},
\code{\link{mutate-joins.SpatVector}},
\code{\link{mutate.Spat}},
\code{\link{pull.Spat}},
\code{\link{relocate.Spat}},
\code{\link{rename.Spat}},
\code{\link{rowwise.SpatVector}()},
\code{\link{select.Spat}},
\code{\link{slice.Spat}},
\code{\link{summarise.SpatVector}()}
}
\concept{dplyr.methods}
\concept{dplyr.rows}
\concept{single table verbs}
