\name{plotStick}
\alias{plotStick}
\title{Plot Stick Men and Women at coordiates}
\usage{
plotStick(x, y, xlim, ylim, xlab = "x", ylab = "y", lwd = 1,
  linecol = 1, hatcol = NULL, shcol = NULL, clcol = NULL,
  gender = c("male", "female"), arms = "default", face = "default",
  legs = "default", hat = "default", tower = 10, ...)
}
\arguments{
  \item{x}{}

  \item{y}{}

  \item{xlim}{x-axis limits to use, otherwise range of x
  expanded}

  \item{ylim}{}

  \item{xlab}{}

  \item{ylab}{}

  \item{lwd}{line weight}

  \item{linecol}{color of lines}

  \item{hatcol}{color of hat, or NULL to supress hat
  (default \code{NULL})}

  \item{shcol}{color of shirt, or NULL to supress shirt
  (default \code{NULL})}

  \item{clcol}{color of clothes, or NULL to supress clothes
  (default \code{NULL})}

  \item{arms}{character vector "down", "neutral", "up",
  "hip", "wave"}

  \item{gender}{character vector "male", "female"}

  \item{face}{list or character vector "default"
  ("neutral"), "happy", "sad", "annoyed", "surprised" face
  can also be a numeric matrix with 4 rows and four
  columns.  See \code{\link{drawStick}} for more details.}

  \item{legs}{single character "default" or "apart"}

  \item{hat}{single logical plot hat or automatic if NA
  (default NA)}
}
\description{
A Stick Man or Woman plot.
}
\examples{
plotStick(x = 1:10, y = c(1:5, 5:1))
    points(1:10, c(1:5, 5:1), col = 2)
    points(1:10, c(1:5, 5:1), col = 2, pch = 3)
    plotStick(x = 1:10, y = sin(1:10), hatcol = 2, shcol = rainbow(10),
        clcol = 1:10, gender = c("male", "female"),
        arms = c("down", "neutral", "up", "hip", "wave"))
}
\author{
Francis Smart, Mango Solutions
}

