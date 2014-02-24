\name{addLegs}
\alias{addLegs}
\title{add legs}
\usage{
addLegs(x = 0, y = 0, s = 1/100, gender = c("male", "female"),
  lwd = 1, linecol = 1, clcol = NULL, legs = "default", tail = c(50,
  35), w = 5)
}
\arguments{
  \item{scale:}{size of figure}

  \item{x:}{left bottom alignment of figure}

  \item{y:}{left bottom alignment of figure}

  \item{s:}{scale (default 1/100)}

  \item{gender:}{"male", "female"}

  \item{lwd:}{line weight}

  \item{linecol:}{color of lines}

  \item{clcol:}{color of clothes, or NULL to supress
  clothes (default \code{NULL})}

  \item{legs:}{single character (default \code{'default'})}

  \item{tail:}{x-y coordiates of leg join with torso
  (default \code{c(50, 35)})}

  \item{w:}{width parameter (default 5)}
}
\value{
list with locations of head, arms and legs plot(0:1, 0:1,
type = "n") addLegs()
}
\description{
add legs
}
\author{
Francis Smart, Mango Solutions
}

