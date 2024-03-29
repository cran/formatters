% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R, R/tostring.R
\name{toString}
\alias{toString}
\alias{toString,MatrixPrintForm-method}
\title{\code{toString}}
\usage{
toString(x, ...)

\S4method{toString}{MatrixPrintForm}(
  x,
  widths = NULL,
  tf_wrap = FALSE,
  max_width = NULL,
  col_gap = mf_colgap(x),
  hsep = NULL
)
}
\arguments{
\item{x}{ANY. Object to be prepared for rendering.}

\item{...}{Passed to individual methods.}

\item{widths}{numeric (or  \code{NULL}). (proposed) widths for the columns
of \code{x}. The expected length  of this numeric vector can be
retrieved with  \code{ncol() + 1}  as the  column of row  names must
also be considered.}

\item{tf_wrap}{logical(1). Should  the texts for  title, subtitle,
and footnotes be wrapped?}

\item{max_width}{integer(1), character(1) or \code{NULL}. Width that title
and   footer   (including   footnotes)  materials   should   be
word-wrapped to. If \code{NULL}, it is  set to the current print width
of the  session (\code{getOption("width")}). If set to \code{"auto"},
the width of the table (plus any table inset) is used. Ignored
completely if \code{tf_wrap} is \code{FALSE}.}

\item{col_gap}{numeric(1). Space (in characters) between columns}

\item{hsep}{character(1). Characters to repeat to create
header/body separator line. If \code{NULL}, the object value will be
used. If \code{" "}, an empty separator will be printed. Check \code{\link[=default_hsep]{default_hsep()}}
for more information.}
}
\value{
A character string containing the ASCII rendering
of the table-like object represented by \code{x}
}
\description{
Transform a complex object into a string representation ready
to be printed or written to a plain-text file

All objects that are printed to console pass by \code{toString}. This function allows
fundamental formatting specifications for the final output, like column widths and
relative wrapping (\code{width}), title and footer wrapping (\code{tf_wrap = TRUE} and
\code{max_width}), or horizontal separator character (e.g. \code{hsep = "+"}).
}
\details{
Manual insertion of newlines is not supported when \code{tf_wrap} is on
and will result in a warning and undefined wrapping behavior. Passing
vectors of already split strings remains supported, however in this
case each string is word-wrapped separately with the behavior
described above.
}
\examples{
mform <- basic_matrix_form(mtcars)
cat(toString(mform))

}
\seealso{
\code{\link[=wrap_string]{wrap_string()}}
}
