\name{SegmentationData-class}
\Rdversion{1.1}
\docType{class}
\alias{SegmentationData-class}
\alias{coerce<-,SegmentationData,data.frame-method}
\alias{coerce,SegmentationData,data.frame-method}
\alias{endTime,SegmentationData-method}
\alias{SaveData,SegmentationData,character-method}
\alias{SaveData,SegmentationData,stringIsTable-method}
\alias{segmenter,SegmentationData-method}
\alias{startTime,SegmentationData-method}
\alias{subjectID,SegmentationData-method}
\alias{task,SegmentationData-method}
\alias{textGridXMax,SegmentationData-method}
\alias{textGridXMin,SegmentationData-method}
\alias{timeUnit,SegmentationData-method}

\title{Class \code{SegmentationData}}
\description{
The \code{SegmentationData} class is used to parse and analyze
segmented TextGrids that are created as a part of the Learning To
Talk project.
}
\section{Objects from the Class}{
Objects can be created by calls to the generic constructor function
\code{link{SegmentationData}}.
}
\section{Slots}{
  \describe{
    \item{\code{segmenter}:}{A \code{character} string; the segmenter's initials.}
    \item{\code{subjectID}:}{A \code{character} string; the subject's alphanumeric identification code.}
    \item{\code{task}:}{A \code{character} string; the name of the experimental task.}
    \item{\code{textGridXMin}:}{A \code{numeric} value; the start time of the recording session.}
    \item{\code{textGridXMax}:}{A \code{numeric} value; the end time of the recording session.}
    \item{\code{timeUnit}:}{A \code{character} string; the unit in which the time measurements are expressed.}
  }
}
\section{Extends}{
Class \code{\linkS4class{data.frame}}, directly.
Class \code{\linkS4class{list}}, by class "data.frame", distance 2.
Class \code{\linkS4class{oldClass}}, by class "data.frame", distance 2.
Class \code{\linkS4class{vector}}, by class "data.frame", distance 3.
}
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "SegmentationData", to = "data.frame")}: ... }
    \item{endTime}{\code{signature(.Object = "SegmentationData")}: Returns the value of the \code{textGridXMax} slot.}
    \item{segmenter}{\code{signature(.Object = "SegmentationData")}: Returns the value of the \code{segmenter} slot.}
    \item{startTime}{\code{signature(.Object = "SegmentationData")}: Returns the value of the \code{textGridXMin} slot.}
    \item{subjectID}{\code{signature(.Object = "SegmentationData")}: Returns the value of the \code{subjectID} slot.}
    \item{task}{\code{signature(.Object = "SegmentationData")}: Returns the value of the \code{task} slot.}
    \item{textGridXMax}{\code{signature(.Object = "SegmentationData")}: Returns the value of the \code{textGridXMax} slot.}
    \item{textGridXMin}{\code{signature(.Object = "SegmentationData")}: Returns the value of the \code{textGridXMin} slot.}
    \item{timeUnit}{\code{signature(.Object = "SegmentationData")}: Returns the value of the \code{timeUnit} slot.}

    \item{SaveData}{\code{signature(x = "SegmentationData", format = "character")}: }
	 }
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("SegmentationData")
}
\keyword{classes}
