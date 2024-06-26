% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/join-SpatVector.R
\name{mutate-joins.SpatVector}
\alias{mutate-joins.SpatVector}
\alias{inner_join.SpatVector}
\alias{left_join.SpatVector}
\alias{right_join.SpatVector}
\alias{full_join.SpatVector}
\title{Mutating joins for \code{SpatVector} objects}
\usage{
\method{inner_join}{SpatVector}(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL
)

\method{left_join}{SpatVector}(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL
)

\method{right_join}{SpatVector}(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL
)

\method{full_join}{SpatVector}(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL
)
}
\arguments{
\item{x}{A \code{SpatVector} created with \code{\link[terra:vect]{terra::vect()}}.}

\item{y}{A data frame or other object coercible to a data frame. \strong{If a
\code{SpatVector} of \code{sf} object} is provided it would return an error (see
\code{\link[terra:intersect]{terra::intersect()}} for performing spatial joins).}

\item{by}{A join specification created with \code{\link[dplyr:join_by]{join_by()}}, or a character
vector of variables to join by.

If \code{NULL}, the default, \verb{*_join()} will perform a natural join, using all
variables in common across \code{x} and \code{y}. A message lists the variables so
that you can check they're correct; suppress the message by supplying \code{by}
explicitly.

To join on different variables between \code{x} and \code{y}, use a \code{\link[dplyr:join_by]{join_by()}}
specification. For example, \code{join_by(a == b)} will match \code{x$a} to \code{y$b}.

To join by multiple variables, use a \code{\link[dplyr:join_by]{join_by()}} specification with
multiple expressions. For example, \code{join_by(a == b, c == d)} will match
\code{x$a} to \code{y$b} and \code{x$c} to \code{y$d}. If the column names are the same between
\code{x} and \code{y}, you can shorten this by listing only the variable names, like
\code{join_by(a, c)}.

\code{\link[dplyr:join_by]{join_by()}} can also be used to perform inequality, rolling, and overlap
joins. See the documentation at \link[dplyr:join_by]{?join_by} for details on
these types of joins.

For simple equality joins, you can alternatively specify a character vector
of variable names to join by. For example, \code{by = c("a", "b")} joins \code{x$a}
to \code{y$a} and \code{x$b} to \code{y$b}. If variable names differ between \code{x} and \code{y},
use a named character vector like \code{by = c("x_a" = "y_a", "x_b" = "y_b")}.

To perform a cross-join, generating all combinations of \code{x} and \code{y}, see
\code{\link[dplyr:cross_join]{cross_join()}}.}

\item{copy}{If \code{x} and \code{y} are not from the same data source,
and \code{copy} is \code{TRUE}, then \code{y} will be copied into the
same src as \code{x}.  This allows you to join tables across srcs, but
it is a potentially expensive operation so you must opt into it.}

\item{suffix}{If there are non-joined duplicate variables in \code{x} and
\code{y}, these suffixes will be added to the output to disambiguate them.
Should be a character vector of length 2.}

\item{...}{Other parameters passed onto methods.}

\item{keep}{Should the join keys from both \code{x} and \code{y} be preserved in the
output?
\itemize{
\item If \code{NULL}, the default, joins on equality retain only the keys from \code{x},
while joins on inequality retain the keys from both inputs.
\item If \code{TRUE}, all keys from both inputs are retained.
\item If \code{FALSE}, only keys from \code{x} are retained. For right and full joins,
the data in key columns corresponding to rows that only exist in \code{y} are
merged into the key columns from \code{x}. Can't be used when joining on
inequality conditions.
}}
}
\value{
A \code{SpatVector} object.
}
\description{
Mutating joins add columns from \code{y} to \code{x}, matching observations based on
the keys. There are four mutating joins: the inner join, and the three outer
joins.

See \code{\link[dplyr:mutate-joins]{dplyr::inner_join()}} for details.
}
\section{\CRANpkg{terra} equivalent}{


\code{\link[terra:merge]{terra::merge()}}
}

\section{Methods}{


Implementation of the \strong{generic} \code{\link[dplyr:mutate-joins]{dplyr::inner_join()}} family
\subsection{\code{SpatVector}}{

The geometry column has a sticky behavior. This means that the result would
have always the geometry of \code{x} for the records that matches the join
conditions.

Note that for \code{\link[=right_join]{right_join()}} and \code{\link[=full_join]{full_join()}} it is possible to return
empty geometries (since \code{y} is expected to be a data frame with no
geometries). Although this kind of joining operations may not be common on
spatial manipulation, it is possible that the function crashes, since
handling of \code{EMPTY} geometries differs on \CRANpkg{terra} and \CRANpkg{sf}.
}
}

\examples{
library(terra)
library(ggplot2)
# Vector
v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

# A data frame
df <- data.frame(
  cpro = sprintf("\%02d", 1:10),
  x = runif(10),
  y = runif(10),
  letter = rep_len(LETTERS[1:3], length.out = 10)
)

# Inner join
inner <- v \%>\% inner_join(df)

nrow(inner)
autoplot(inner, aes(fill = letter)) + ggtitle("Inner Join")


# Left join

left <- v \%>\% left_join(df)
nrow(left)

autoplot(left, aes(fill = letter)) + ggtitle("Left Join")

\donttest{
# Right join
right <- v \%>\% right_join(df)
nrow(right)

autoplot(right, aes(fill = letter)) + ggtitle("Right Join")

# There are empty geometries, check with data from df
ggplot(right, aes(x, y)) +
  geom_point(aes(color = letter))


# Full join
full <- v \%>\% full_join(df)
nrow(full)

autoplot(full, aes(fill = letter)) + ggtitle("Full Join")

# Check with data from df
ggplot(full, aes(x, y)) +
  geom_point(aes(color = letter))
}
}
\seealso{
\code{\link[dplyr:mutate-joins]{dplyr::inner_join()}}, \code{\link[dplyr:mutate-joins]{dplyr::left_join()}}, \code{\link[dplyr:mutate-joins]{dplyr::right_join()}},
\code{\link[dplyr:mutate-joins]{dplyr::full_join()}}, \code{\link[terra:merge]{terra::merge()}}

Other \CRANpkg{dplyr} verbs that operate on pairs \code{Spat*}/data.frame:
\code{\link{bind_cols.SpatVector}},
\code{\link{bind_rows.SpatVector}},
\code{\link{filter-joins.SpatVector}}

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
\concept{dplyr.pairs}
