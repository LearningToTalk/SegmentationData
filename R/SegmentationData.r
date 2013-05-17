
# Author:       Patrick Reidy <reidy@ling.ohio-state.edu>
# Affiliations: The Ohio State University, Dept. of Linguistics <linguistics.osu.edu>
#               Learning To Talk <learningtotalk.org>
# Date:         2013 May 4

library('TextGrid', quietly = TRUE)




#######################################
#  SegmentationData class definition  #
#######################################

setClass(Class = 'SegmentationData',
         representation = representation(segmenter    = 'character',
                                         subjectID    = 'character',
                                         task         = 'character',
                                         textGridXMin = 'numeric',
                                         textGridXMax = 'numeric',
                                         timeUnit     = 'character'),
         contains       = c('data.frame')
)


###########################################
#  SegmentationData constructor function  #
###########################################

setGeneric(name = 'SegmentationData',
           def  = function(segmentationData, wordlist, forceTierNames = TRUE, ...) {standardGeneric('SegmentationData')}
)

setMethod(f = 'SegmentationData',
          signature  = c(segmentationData = 'TextGrid', wordlist = 'data.frame'),
          definition = function(segmentationData, wordlist, forceTierNames) {
            # Check the tier names of the TextGrid segmentationData.
            valid.tier.names <- c('Word', 'Repetition', 'Context', 'SegmNotes')
            if (forceTierNames) {
              names(segmentationData) <- valid.tier.names
            } else {
              if (! identical(names(segmentationData), valid.tier.names)) {
                message('A SegmentationData object was not constructed because')
                message(sprintf('the tier names of TextGrid %s are invalid.', textGridName(segmentationData)))
                message('Use the option `forceTierNames = TRUE` to fix this issue.')
                flush.console()
              }
            }
            # Check the interval labels on the Word tier of the TextGrid segmentationData.
            word.labels    <- as.character(segmentationData[['Word']]$Text)
            invalid.labels <- which(                              # An invalid Word label is:
              (! is.na(word.labels)) &                            #  1. not NA and
                (! word.labels %in% as.character(wordlist$Word))  #  2. not a target word in wordlist.
              )
            if (length(invalid.labels > 0)) {
              invalid.intervals          <- segmentationData[['Word']][invalid.labels]
              invalid.intervals          <- as(invalid.intervals, 'data.frame')
              invalid.intervals$Interval <- invalid.labels
              invalid.intervals          <- invalid.intervals[, c('Interval', 'Text', 'XMin', 'XMax')]
              message('A SegmentationData object was not constructed because')
              message('the following intervals on the Word tier have invalid labels:')
              print(invalid.intervals)
              flush.console()
              return(NULL)
            }
            # Check the interval labels on the Repetition tier of the TextGrid segmentationData.
            repetition.labels <- as.character(segmentationData[['Repetition']]$Text)
            valid.labels      <- c('1', '1m', '2')
            invalid.labels    <- which(                  # An invalid Repetition label is:
              (! is.na(repetition.labels)) &             #  1. not NA and
                (! repetition.labels %in% valid.labels)  #  2. not one of '1', '1m', or '2'.
              )
            if (length(invalid.labels > 0)) {
              invalid.intervals          <- segmentationData[['Repetition']][invalid.labels]
              invalid.intervals          <- as(invalid.intervals, 'data.frame')
              invalid.intervals$Interval <- invalid.labels
              invalid.intervals          <- invalid.intervals[, c('Interval', 'Text', 'XMin', 'XMax')]
              message('A SegmentationData object was not constructed because')
              message('the following intervals on the Repetition tier have invalid labels:')
              print(invalid.intervals)
              flush.console()
              return(NULL)
            }
            # Check the interval labels on the Context tier of the TextGrid segmentationData.
            context.labels <- as.character(segmentationData[['Context']]$Text)
            valid.labels   <- c('Malaprop', 'NonResponse', 'Other', 'Other_Repeat', 'Other_VoicePrompt',
                                'Perseveration', 'TargetRep', 'TargetRep_Check', 'TargetRep_Repeat',
                                'TargetRep_Repeat_Check', 'TargetRep_VoicePrompt', 'WrongWord')
            invalid.labels <- which(                  # An invalid Context label is:
              (! is.na(context.labels)) &             #  1. not NA and
                (! context.labels %in% valid.labels)  #  2. not one of the valid labels listed above.
              )
            if (length(invalid.labels) > 0) {
              invalid.intervals          <- segmentationData[['Context']][invalid.labels]
              invalid.intervals          <- as(invalid.intervals, 'data.frame')
              invalid.intervals$Interval <- invalid.labels
              invalid.intervals          <- invalid.intervals[, c('Interval', 'Text', 'XMin', 'XMax')]
              message('A SegmentationData object was not constructed because')
              message('the following intervals on the Context tier have invalid labels:')
              print(invalid.intervals)
              flush.console()
              return(NULL)
            }
            # Convert the data in the TextGrid segmentationData to a data.frame with the columns:
            #  Word, XMin, XMax, Repetition, Context, Notes
            segm.dframe            <- subset(segmentationData[['Word']], ! is.na(Text))
            segm.dframe            <- as(segm.dframe, 'data.frame')
            names(segm.dframe)     <- c('XMin', 'XMax', 'Word')
            segm.dframe$Repetition <- as.character(NA)
            segm.dframe$Context    <- as.character(NA)
            segm.dframe$Notes      <- as.character(NA)
            for (row in 1:nrow(segm.dframe)) {
              word.tgrid <- TimeSlice(segmentationData,
                                      sliceFrom = segm.dframe$XMin[row],
                                      sliceTo   = segm.dframe$XMax[row])
              repetition <- word.tgrid[['Repetition']]$Text
              repetition <- repetition[! is.na(repetition)]
              if (length(repetition) == 1) {
                segm.dframe$Repetition[row] <- repetition
              } else {
                word.error <- segm.dframe[row, c('Word', 'XMin', 'XMax')]
                message('A SegmentationData object was not constructed because')
                message('the following Word did not have a unique Repetition.')
                print(word.error)
                flush.console()
              }
              context    <- word.tgrid[['Context']]$Text
              context    <- context[! is.na(context)]
              if (length(context) == 1) {
                segm.dframe$Context[row] <- context
              } else {
                word.error <- segm.dframe[row, c('Word', 'XMin', 'XMax')]
                message('A SegmentationData object was not constructed because')
                message('the following Word did not have a unique Context.')
                print(word.error)
                flush.console()
              }
              segm.notes <- word.tgrid[['SegmNotes']]$Mark
              if (length(segm.notes) == 1) {
                segm.notes             <- paste(segm.notes, collapse = '; ')
                segm.dframe$Notes[row] <- segm.notes
              }
            }
            # Check that the Repetition numbers are validly assigned.
            repn.errors <- c()
            for (row in 1:nrow(segm.dframe)) {
              this.repn <- segm.dframe$Repetition[row]
              this.word <- segm.dframe$Word[row]
              if (row == 1) {
                prev.repn <- '1'
                prev.word <- ''
              } else {
                prev.repn <- segm.dframe$Repetition[row - 1]
                prev.word <- segm.dframe$Word[row - 1]
              }
              invalid.1 <- (this.repn %in% c('1', '1m')) & ! (prev.repn %in% c('1', '2'))
              invalid.2 <- (this.repn %in% c('2')) & 
                (! (prev.repn %in% c('1m')) | (this.word != prev.word))
              if (invalid.1 | invalid.2) {
                repn.errors <- c(repn.errors, row)
              }
            }
            if (length(repn.errors) > 0) {
              message("A SegmentationData object was not constructed because")
              message("the following Repetition labels aren't validly assigned.")
              flush.console()
              for (row in repn.errors) {
                repn.error <- segm.dframe[c(row-1, row), c('Word', 'Repetition', 'XMin', 'XMax')]
                print(repn.error)
                message('')
                flush.console()
              }
              return(NULL)
            }
            # Assign Trial numbers to each segmented production.
            segm.dframe$Trial <- as.numeric(NA)
            trial             <- 1
            for (row in 1:nrow(segm.dframe)) {
              segm.dframe$Trial[row] <- trial
              this.repn <- segm.dframe$Repetition[row]
              if (this.repn %in% c('1', '2')) {
                trial <- trial + 1
              }
            }
            # Check the assigned Trial numbers against the wordlist data.frame.
            trials      <- segm.dframe$Trial
            trial.nums  <- sort(unique(trials))
            rows        <- 1:nrow(segm.dframe)
            wlist.words <- as.character(wordlist$Word)
            trial.words <- c()
            for (num in trial.nums) {
              row <- min(rows[trials == num])
              trial.words <- c(trial.words, segm.dframe$Word[row])
            }
            if (! identical(trial.words, wlist.words)) {
              word.diffs <- length(trial.words) - length(wlist.words)
              trial.nums <- as.character(1:nrow(wordlist))
              if (word.diffs > 0) {
                # Then there are more trials in the segmentationData than in the wordlist.
                wlist.words <- c(wlist.words, rep('', word.diffs))
                trial.nums  <- c(trial.nums, rep('', word.diffs))
              } else {
                # Otherwise, there are more trials in the wordlist than the segmentationData.
                trial.words <- c(trial.words, rep('', abs(word.diffs)))
              }
              trial.errors <- data.frame(Trial        = trial.nums,
                                         Wordlist     = wlist.words,
                                         Segmentation = trial.words,
                                         stringsAsFactors = FALSE)
              trial.errors <- within(trial.errors, Check <- ifelse(Wordlist == Segmentation, '', '*'))
              message('A SegmentationData object was not created because')
              message("the trials in the TextGrid and the wordlist differ.")
              print(trial.errors)
              return(NULL)
            } else {
              # Add the rest of the data from the wordlist table to the segmentation data.
              reps.per.trial <- as.numeric(table(segm.dframe$Trial))
              segm.dframe$WordTranscription <- rep(as.character(wordlist$WorldBet), reps.per.trial)
              segm.dframe$ConsonantTarget   <- rep(as.character(wordlist$TargetC), reps.per.trial)
              segm.dframe$VowelTarget       <- rep(as.character(wordlist$TargetV), reps.per.trial)
              segm.dframe$Frame             <- rep(as.character(wordlist$Frame), reps.per.trial)
              column.order                  <- c('Trial', 'XMin', 'XMax', 'Word', 
                                                 'WordTranscription', 'ConsonantTarget', 
                                                 'VowelTarget', 'Frame', 'Repetition',
                                                 'Context', 'Notes')
              segmentation.data             <- segm.dframe[, column.order]
              # Parse the name of the TextGrid segmentationData to get the task, subjectID, and segmenter.
              tgrid.name.split <- strsplit(textGridName(segmentationData), split = '_')[[1]]
              task             <- tgrid.name.split[1]
              subject.id       <- tgrid.name.split[2]
              segmenter        <- sub(pattern = 'segm',
                                      replacement = '',
                                      x = tgrid.name.split[3])
              new(Class = 'SegmentationData',
                  segmentation.data,
                  segmenter    = segmenter,
                  subjectID    = subject.id,
                  task         = task,
                  textGridXMin = startTime(segmentationData),
                  textGridXMax = endTime(segmentationData),
                  timeUnit     = timeUnit(segmentationData))
            }
          }
)

setMethod(f = 'SegmentationData',
          signature  = c(segmentationData = 'character', wordlist = 'missing', forceTierNames = 'missing'),
          definition = function(segmentationData) {
            segm.data <- read.delim(segmentationData, stringsAsFactors = FALSE)
            # Columns for slot names: Segmenter, SubjectID, Task, TextGridXMin, TextGridXMax, TimeUnit
            segmenter  <- unique(segm.data$Segmenter)
            if (length(segmenter) != 1) {
              message('A SegmentationData object was not constructed because')
              message('there is not a unique Segmenter code.')
              message('Segmenter codes found in the data.frame:')
              print(segmenter)
              flush.console()
              return(NULL)
            }
            subject.id <- unique(segm.data$SubjectID)
            if (length(subject.id) != 1) {
              message('A SegmentationData object was not constructed because')
              message('there is not a unique SubjectID code.')
              message('SubjectID codes found in the data.frame:')
              print(subject.id)
              flush.console()
              return(NULL)
            }
            task       <- unique(segm.data$Task)
            if (length(task) != 1) {
              message('A SegmentationData object was not constructed because')
              message('there is not a unique Task code.')
              message('Task codes found in the data.frame:')
              print(task)
              flush.console()
              return(NULL)
            }
            tgrid.xmin <- unique(segm.data$TextGridXMin)
            if (length(tgrid.xmin) != 1) {
              message('A SegmentationData object was not constructed because')
              message('there is not a unique TextGridXMin value.')
              message('TextGridXMin values found in the data.frame:')
              print(tgrid.xmin)
              flush.console()
              return(NULL)
            }
            tgrid.xmax <- unique(segm.data$TextGridXMax)
            if (length(tgrid.xmax) != 1) {
              message('A SegmentationData object was not constructed because')
              message('there is not a unqiue TextGridXMax value.')
              message('TextGridXMax values found in the data.frame:')
              print(tgrid.xmax)
              flush.console()
              return(NULL)
            }
            time.unit  <- unique(segm.data$TimeUnit)
            if (length(time.unit) != 1) {
              message('A SegmentationData object was not constructed because')
              message('there is not a unique TimeUnit value.')
              message('TimeUnit values found in the data.frame:')
              print(time.unit)
              flush.console()
              return(NULL)
            }
            data.columns <- c('Trial', 'XMin', 'XMax', 'Word', 
                              'WordTranscription', 'ConsonantTarget', 
                              'VowelTarget', 'Frame', 'Repetition',
                              'Context', 'Notes')
            new(Class = 'SegmentationData',
                segm.data[, data.columns],
                segmenter    = segmenter,
                subjectID    = subject.id,
                task         = task,
                textGridXMin = tgrid.xmin,
                textGridXMax = tgrid.xmax,
                timeUnit     = time.unit)
          }
)



#######################################
#  SegmentationData coercion methods  #
#######################################

# Coercion to a data.frame
setAs(from = 'SegmentationData',
      to   = 'data.frame',
      def  = function(from) {
        to.dframe <- data.frame(from@.Data, stringsAsFactors = FALSE)
        names(to.dframe) <- from@names
        return(to.dframe)
      }
)




#######################################
#  SegmentationData slot-get methods  #
#######################################

if (! isGeneric('segmenter')) {
  setGeneric(name = 'segmenter',
             def  = function(.Object) {standardGeneric('segmenter')}
             )
}
setMethod(f = 'segmenter',
          signature  = c(.Object = 'SegmentationData'),
          definition = function(.Object) {.Object@segmenter}
          )

if (! isGeneric('subjectID')) {
  setGeneric(name = 'subjectID',
             def  = function(.Object) {standardGeneric('subjectID')}
             )
}
setMethod(f = 'subjectID',
          signature  = c(.Object = 'SegmentationData'),
          definition = function(.Object) {.Object@subjectID}
          )

if (! isGeneric('task')) {
  setGeneric(name = 'task',
             def  = function(.Object) {standardGeneric('task')}
             )
}
setMethod(f = 'task',
          signature  = c(.Object = 'SegmentationData'),
          definition = function(.Object) {.Object@task}
          )

if (! isGeneric('textGridXMin')) {
  setGeneric(name = 'textGridXMin',
             def  = function(.Object) {standardGeneric('textGridXMin')}
             )
}
setMethod(f = 'textGridXMin',
          signature  = c(.Object = 'SegmentationData'),
          definition = function(.Object) {.Object@textGridXMin}
          )

if (! isGeneric('startTime')) {
  setGeneric(name = 'startTime',
             def  = function(.Object) {standardGeneric('startTime')}
             )
}
setMethod(f = 'startTime',
          signature  = c(.Object = 'SegmentationData'),
          definition = function(.Object) {.Object@textGridXMin}
          )

if (! isGeneric('textGridXMax')) {
  setGeneric(name = 'textGridXMax',
             def  = function(.Object) {standardGeneric('textGridXMax')}
             )
}
setMethod(f = 'textGridXMax',
          signature  = c(.Object = 'SegmentationData'),
          definition = function(.Object) {.Object@textGridXMax}
          )

if (! isGeneric('endTime')) {
  setGeneric(name = 'endTime',
             def  = function(.Object) {standardGeneric('endTime')}
  )
}
setMethod(f = 'endTime',
          signature  = c(.Object = 'SegmentationData'),
          definition = function(.Object) {.Object@textGridXMax}
)

if (! isGeneric('timeUnit')) {
  setGeneric(name = 'timeUnit',
             def  = function(.Object) {standardGeneric('timeUnit')}
             )
}
setMethod(f = 'timeUnit',
          signature  = c(.Object = 'SegmentationData'),
          definition = function(.Object) {.Object@timeUnit}
          )



