\name{SegmentationData}
\alias{SegmentationData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\code{SegmentationData} Object Initialization
}
\description{
A generic function for initializing a \code{\linkS4class{SegmentationData}} object.  Depending on the type of data from which the object is initialized, various correctness checks are performed to ensure validity.
}
\usage{
SegmentationData(segmentationData, wordlist, forceTierNames = TRUE, ...)
\S4method{SegmentationData}{TextGrid,data.frame,logical}(segmentationData, wordlist, forceTierNames)
\S4method{SegmentationData}{character,missing,missing}(segmentationData, wordlist, forceTierNames)
}
\arguments{
  \item{segmentationData}{
    A \code{\linkS4class{TextGrid}} object or a \code{character} string that points to either
    a TextGrid file or a tab-separated table on the local filesystem.
}
  \item{wordlist}{
    A \code{data.frame} created from a tab-separated table of the wordlist used in one of the tasks
    in the Learning To Talk project.
}
  \item{forceTierNames}{
    A \code{logical} value.  If \code{TRUE} and the \code{segmentationData} argument is a
    \code{TextGrid} object, then the tier names of the \code{segmentationData} argument are
    forced to \code{'Word'}, \code{'Repetition'}, \code{'Context'}, and \code{'SegmNotes'}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A \code{linkS4class{SegmentationData}} object.
}
\references{
\url{learningtotalk.org}
}
\author{
Patrick Reidy
}
\note{
%%  ~~further notes~~
}


\seealso{
\code{\link{SegmentationData-methods}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }

