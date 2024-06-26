% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rowwise-SpatVector.R
\name{rowwise.SpatVector}
\alias{rowwise.SpatVector}
\title{Group \code{SpatVector} objects by rows}
\usage{
\method{rowwise}{SpatVector}(data, ...)
}
\arguments{
\item{data}{A \code{SpatVector} object. See \strong{Methods}.}

\item{...}{<\code{\link[dplyr:dplyr_tidy_select]{tidy-select}}> Variables to be
preserved when calling \code{\link[=summarise.SpatVector]{summarise.SpatVector()}}. This is typically a set
of variables whose combination uniquely identify each row. See
\code{\link[dplyr:rowwise]{dplyr::rowwise()}}.}
}
\value{
The same \code{SpatVector} object with an additional attribute.
}
\description{
\code{rowwise()} allows you to compute on a \code{SpatVector} a row-at-a-time.
This is most useful when a vectorised function doesn't exist.

Most \CRANpkg{dplyr} verbs implementation in \CRANpkg{tidyterra} preserve
row-wise grouping, with the exception of \code{\link[=summarise.SpatVector]{summarise.SpatVector()}}. You can
explicitly ungroup with \code{\link[=ungroup.SpatVector]{ungroup.SpatVector()}} or \code{\link[=as_tibble]{as_tibble()}}, or convert
to a grouped \code{SpatVector} with \code{\link[=group_by.SpatVector]{group_by.SpatVector()}}.
}
\details{
See \strong{Details} on \code{\link[dplyr:rowwise]{dplyr::rowwise()}}.
}
\section{Methods}{


Implementation of the \strong{generic} \code{\link[dplyr:rowwise]{dplyr::rowwise()}} function for
\code{SpatVector} objects.

\strong{When mixing} \CRANpkg{terra} \strong{and} \CRANpkg{dplyr} \strong{syntax} on a
row-wise \code{SpatVector} (i.e, subsetting a \code{SpatVector} like \code{v[1:3,1:2]}) the
\code{groups} attribute can be corrupted. \CRANpkg{tidyterra} would try to
re-generate the \code{SpatVector}. This would be triggered the next time you use
a \CRANpkg{dplyr} verb on your \code{SpatVector}.

Note also that some operations (as \code{terra::spatSample()}) would create a new
\code{SpatVector}. In these cases, the result won't preserve the \code{groups}
attribute. Use \code{\link[=rowwise.SpatVector]{rowwise.SpatVector()}} to re-group.
}

\examples{
library(terra)
library(dplyr)

v <- terra::vect(system.file("shape/nc.shp", package = "sf"))

# Select new births
nb <- v \%>\%
  select(starts_with("NWBIR")) \%>\%
  glimpse()

# Compute the mean of NWBIR on each geometry
nb \%>\%
  rowwise() \%>\%
  mutate(nb_mean = mean(c(NWBIR74, NWBIR79)))

# Additional examples
\donttest{
# use c_across() to more easily select many variables
nb \%>\%
  rowwise() \%>\%
  mutate(m = mean(c_across(NWBIR74:NWBIR79)))

# Compute the minimum of x and y in each row

nb \%>\%
  rowwise() \%>\%
  mutate(min = min(c_across(NWBIR74:NWBIR79)))

# Summarising
v \%>\%
  rowwise() \%>\%
  summarise(mean_bir = mean(BIR74, BIR79)) \%>\%
  glimpse() \%>\%
  autoplot(aes(fill = mean_bir))

# Supply a variable to be kept
v \%>\%
  mutate(id2 = as.integer(CNTY_ID / 100)) \%>\%
  rowwise(id2) \%>\%
  summarise(mean_bir = mean(BIR74, BIR79)) \%>\%
  glimpse() \%>\%
  autoplot(aes(fill = as.factor(id2)))
}
}
\seealso{
\code{\link[dplyr:rowwise]{dplyr::rowwise()}}

Other \CRANpkg{dplyr} verbs that operate on group of rows:
\code{\link{count.SpatVector}()},
\code{\link{group-by.SpatVector}},
\code{\link{summarise.SpatVector}()}

Other \CRANpkg{dplyr} methods:
\code{\link{arrange.SpatVector}()},
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
\code{\link{select.Spat}},
\code{\link{slice.Spat}},
\code{\link{summarise.SpatVector}()}
}
\concept{dplyr.groups}
\concept{dplyr.methods}
