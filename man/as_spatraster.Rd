% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_spatraster.R
\name{as_spatraster}
\alias{as_spatraster}
\title{Coerce a data frame to SpatRaster}
\usage{
as_spatraster(x, ..., xycols = 1:2, crs = "", digits = 6)
}
\arguments{
\item{x}{A tibble or data frame.}

\item{...}{additional arguments passed on to \code{\link[terra:rast]{terra::rast()}}.}

\item{xycols}{A vector of integers of length 2 determining the position of
the columns that hold the x and y coordinates.}

\item{crs}{A crs on several formats (PROJ.4, WKT, EPSG code, ..) or
and spatial object from sf or terra that includes the target coordinate
reference system. See \code{\link[=pull_crs]{pull_crs()}}. See \strong{Details}.}

\item{digits}{integer to set the precision for detecting whether points are
on a regular grid (a low number of digits is a low precision).}
}
\value{
A SpatRaster.
}
\description{
\code{as_spatraster()} turns an existing  data frame or tibble, into a SpatRaster.
This is a wrapper of \code{\link[terra:rast]{terra::rast()}} S4 method for \code{data.frame}.
}
\details{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#questioning}{\figure{lifecycle-questioning.svg}{options: alt='[Questioning]'}}}{\strong{[Questioning]}} If no \code{crs} is provided and the tibble
has been created with the method \code{\link[=as_tibble.SpatRaster]{as_tibble.SpatRaster()}}, the \code{crs} is
inferred from \code{attr(x, "crs")}.
}
\section{terra equivalent}{


\code{\link[terra:rast]{terra::rast()}}
}

\examples{
library(terra)

r <- rast(matrix(1:90, ncol = 3), crs = "EPSG:3857")

r

# Create tibble
as_tbl <- as_tibble(r, xy = TRUE)

as_tbl

# From tibble
newrast <- as_spatraster(as_tbl, crs = "EPSG:3857")
newrast

}
\seealso{
\code{\link[=pull_crs]{pull_crs()}}

Coercing objects:
\code{\link{as_coordinates}()},
\code{\link{as_sf}()},
\code{\link{as_spatvector}()},
\code{\link{as_tibble.Spat}},
\code{\link{fortify.Spat}}
}
\concept{coerce}
