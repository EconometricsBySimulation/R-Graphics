\name{drawStick}
\alias{drawStick}
\title{Stick Man/Woman}
\usage{
drawStick(x = 0, y = 0, scale = 1, gender = c("male", "female"),
  lwd = 3, linecol = 1, hatcol = 2, shcol = NULL, clcol = NULL,
  arms = "default", face = "default", hat = "default", shirt = FALSE,
  ...)
}
\arguments{
  \item{scale:}{size of figure}

  \item{x:}{left bottom alignment of figure}

  \item{y:}{left bottom alignment of figure}

  \item{lwd:}{line weight}

  \item{linecol:}{color of lines}

  \item{hatcol:}{color of hat}

  \item{shcol:}{color of shirt, or NULL to supress shirt
  (default \code{NULL})}

  \item{clcol:}{color of clothes, or NULL to supress
  clothes (default \code{NULL})}

  \item{arms:}{single character "down", "neutral", "up",
  "hip", "wave"}

  \item{gender:}{"male", "female"}

  \item{face:}{"happy", "sad", "annoyed", "surprised"}

  \item{hat:}{single logical plot hat or automatic if NA
  (default NA)}
}
\value{
list with locations of head, arms and legs
}
\description{
Create a Stick Man or Woman See
http://www.econometricsbysimulation.com/2014/01/stick-figure-function-r.html
for more information
}
\examples{
# Map of stick figures
   par(mar = rep(0, 4))

   plot(c(.25,1.25), c(0,3), type="n", xaxt='n', yaxt='n', ann=FALSE)

   drawStick(0,0, arms="hip")
   drawStick(.5,0, gender="female", arms="up")

   drawStick(0,1, arms="neutral", lwd=2, linecol=gray(.5),
            clcol="red", face="sad")

   drawStick(.5,1, gender="female", arms="down", clcol="purple",
            lwd=2, linecol=gray(.5), face="sad",hat=T)

   drawStick(0,2, arms="wave", linecol=gray(.7),
            clcol="blue", face="surprised",hat=F)

   drawStick(.5,2, gender="female", arms="hip", clcol="light blue",
            linecol=gray(.7),face="annoyed")

   # Solitary annoyed figure
   plot(c(.25,.75), c(0,1), type="n")
   drawStick(0, 0, face="surprised", gender="male",
       arms="wave", hat=FALSE)
}
\author{
Francis Smart, Mango Solutions
}

