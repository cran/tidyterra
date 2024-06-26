% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/group-data-SpatVector.R
\name{group_data.SpatVector}
\alias{group_data.SpatVector}
\alias{group_keys.SpatVector}
\alias{group_indices.SpatVector}
\alias{group_vars.SpatVector}
\alias{groups.SpatVector}
\alias{group_size.SpatVector}
\alias{n_groups.SpatVector}
\title{Grouping metadata for \code{SpatVector} objects}
\usage{
\method{group_data}{SpatVector}(.data)

\method{group_keys}{SpatVector}(.tbl, ...)

\method{group_indices}{SpatVector}(.data, ...)

\method{group_vars}{SpatVector}(x)

\method{groups}{SpatVector}(x)

\method{group_size}{SpatVector}(x)

\method{n_groups}{SpatVector}(x)
}
\arguments{
\item{.data, .tbl, x}{A \code{SpatVector}.}

\item{...}{Use of \code{...} is now deprecated; please use \code{group_by()} first
instead.}
}
\value{
See the description of the function. The results are usually tibbles,
lists or vectors. These functions does not return \code{SpatVector} objects.
}
\description{
This collection of functions accesses data about grouped \code{SpatVector} objects
in various ways:
\itemize{
\item \code{\link[=group_data]{group_data()}} returns a tibble that defines the grouping structure.
The columns give the values of the grouping variables. The last column,
always called \code{.rows}, is a list of integer vectors that gives the
location of the rows in each group.
\item \code{\link[=group_keys]{group_keys()}} returns a tibble describing the groups.
\item \code{\link[=group_rows]{group_rows()}} returns a list of integer vectors giving the rows that
each group contains.
\item \code{\link[=group_indices]{group_indices()}} returns an integer vector the same length as \code{.data}
that gives the group that each row belongs to.
\item \code{\link[=group_vars]{group_vars()}} gives names of grouping variables as character vector.
\item \code{\link[=groups]{groups()}} gives the names of the grouping variables as a list of symbols.
\item \code{\link[=group_size]{group_size()}} gives the size of each group.
\item \code{\link[=n_groups]{n_groups()}} gives the total number of groups.
}

See \code{\link[dplyr:group_data]{dplyr::group_data()}}.
}
\examples{
library(terra)

v <- vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
v$gr_1 <- rep_len(c("A", "A", "B"), length.out = nrow(v))
v$gr_2 <- rep_len(c("C", "D"), length.out = nrow(v))

# Ungrouped

n_groups(v)

group_vars(v)

group_keys(v)

group_size(v)

groups(v)

group_rows(v)

group_data(v)

group_indices(v)

# Grouped by one var
gv <- group_by(v, gr_1)

n_groups(gv)

group_vars(gv)

group_keys(gv)

group_size(gv)

groups(gv)

group_rows(gv)

group_data(gv)

group_indices(gv)

# Grouped by several vars

gv2 <- group_by(v, gr_1, gr_2)

n_groups(gv2)

group_vars(gv2)

group_keys(gv2)

group_size(gv2)

groups(gv2)

group_rows(gv2)

group_data(gv2)

group_indices(gv2)
}
\keyword{internal}
