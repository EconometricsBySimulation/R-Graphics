\name{addArms}
\alias{addArms}
\title{add arms}
\usage{
addArms(x = 0, y = 0, s = 1/100, arms = "default", lwd = 1,
  linecol = 1, shcol = NULL, torso = c(50, 60, 50, 35), w = 5)
}
\arguments{
  \item{x:}{left bottom alignment of figure}

  \item{y:}{left bottom alignment of figure}

  \item{s:}{scale (default 1/100)}

  \item{arms:}{single character "default" ("down"),
  "neutral", "up", "hip", "wave", or numeric matrix with
  two rows for left and right coordinates}

  \item{lwd:}{line weight}

  \item{linecol:}{color of lines}

  \item{shcol:}{color of shirt, or NULL to supress shirt
  (default \code{NULL})}

  \item{torso:}{length 4 vector of x-y coordinates of top
  and bottom of torso}

  \item{w:}{width parameter (default 5)}
}
\value{
matrix of arms coordinates
}
\description{
add arms
}
\examples{
plot(0:1, 0:1, type = "n")
   addArms(arms = "neutral", linecol = 2)
   addArms(arms = "hip", linecol = 4)
   addArms(arms = "wave", linecol = 5)
   addArms(shcol = 2)
   addArms(arms = "up", linecol = 3, shcol = 3)
   addArms(arms = "neutral", shcol = 4)
}

