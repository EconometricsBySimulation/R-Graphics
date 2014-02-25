\name{addHead}
\alias{addHead}
\title{add head}
\usage{
addHead(x = 0, y = 0, s = 1/100, face = "default", hat = "none",
  lwd = 1, linecol = 1, hatcol = 2, head = c(50, 75, 10, 15))
}
\arguments{
  \item{x:}{left bottom alignment of figure}

  \item{y:}{left bottom alignment of figure}

  \item{s:}{scale (default 1/100)}

  \item{face:}{"default" ("neutral"), "happy", "sad",
  "annoyed", "surprised" face can also be a numeric matrix
  with 4 rows and four columns.  The rows give coordinates
  of: \enumerate{ \item left eye x, y, x diameter, y
  diameter \item right eye x, y, x diameter, y diameter
  \item mouth x, y, x diameter, y diameter.  If the third
  value of the fourth column is Inf, x, y, x, y of a
  straight line.  \item mouth start and stop positions. If
  the third value is Inf, plot a line for the mouth (plus
  one unused values) }}

  \item{hat:}{single logical plot hat or single character
  "none" or "default" (default "none")}

  \item{lwd:}{line weight}

  \item{linecol:}{color of lines - any color}

  \item{hatcol:}{color of hat - any color}

  \item{head:}{length 4 specifying head x-y location and
  x-y diameters}
}
\value{
matrix of head coordinates
}
\description{
add a head
}
\examples{
plot(0:1, 0:1, type = "n")
       addHead(hat = "default")
       addHead(y = -0.5, face = matrix(c(46, 77, 2.5, 2,
                       54, 77, 2.5, 2,
                       50, 70, 6, 2,
                       -160, -20, 0, 0), nrow = 4, ncol = 4, byrow = TRUE))
}

