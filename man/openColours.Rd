\name{openColours}
\alias{openColours}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{openair colours}
\description{
 Pre-defined openair colours and definition of user-defined colours
}
\usage{
openColours(scheme = "default", n = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{scheme}{The pre-defined schemes are "increment", "default", 
  "brewer1", "heat", "jet", "hue", "greyscale".}
  \item{n}{The number of colours to be returned.}
}
\details{
  This in primarily an internal openair function to make it easy for
  users to select particular colour schemes, or define their own range
  of colours of a user-defined length.

  Each of the pre-defined schemes have merits and their use will depend
  on a particular situation. For showing incrementing concentrations
  e.g. high concentrations emphasised, then "default", "heat", "jet" and
  "increment" are very useful.

  To colour-code categorical-type problems e.g. colours for different
  pollutants, "hue" and "brewer1" are useful.

  When publishing in black and white, "greyscale" is often convenient. 
  With most openair functions, as well as generating a greyscale colour 
  gradient, it also resets strip background and other coloured text and lines 
  to greyscale values. 

  Failing that, the user can define their own schemes based on R colour
  names. To see the full list of names, type \code{colors()} into R.
}
\value{
  Returns colour values - see examples below.
}
\references{ ~put references to the literature/web site here ~ }
\author{David Carslaw}

%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
# to return 5 colours from the "jet" scheme:
cols <- openColours("jet", 5)
cols

# to interpolate between named colours e.g. 10 colours from yellow to
#  green to red:
cols <- openColours(c("yellow", "green", "red"), 10)
cols

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
