
library('Filenames')


##################################################################
#  Class definitions that are used for SaveData method dispatch  #
##################################################################

setClass(Class    = 'stringIsTable',
         contains = c('character')
)

setClass(Class    = 'stringIsTextGrid',
         contains = c('character')
)



#########################################
#  SaveData generic function & methods  #
#########################################

if (! isGeneric('SaveData')) {
  setGeneric(name = 'SaveData',
             def  = function(x, format, destination = './', ...) {standardGeneric('SaveData')}
  )
}

setMethod(f = 'SaveData',
          signature  = c(x = 'SegmentationData', format = 'stringIsTable'),
          definition = function(x, format, destination, ...) {
            # Write out the x@.Data data.frame as a tab-delimited table.
            segm.data.basename     <- paste(task(x), subjectID(x), 'SegmentationData', sep = '_')
            segm.data.filename     <- paste(segm.data.basename, 'txt', sep = '.')
            segm.data.filepath     <- paste(destination, segm.data.filename, sep = '/')
            segm.data              <- as(x, 'data.frame')
            segm.data$Segmenter    <- segmenter(x)
            segm.data$SubjectID    <- subjectID(x)
            segm.data$Task         <- task(x)
            segm.data$TextGridXMin <- textGridXMin(x)
            segm.data$TextGridXMax <- textGridXMax(x)
            segm.data$TimeUnit     <- timeUnit(x)
            write.table(format(segm.data, nsmall = 14), file = segm.data.filepath, 
                        sep = '\t', quote = FALSE, row.names = FALSE, 
                        ...)
#            slot.data.basename <- paste(task(x), subjectID(x), 'SegmentationSlots', sep = '_')
#            slot.data.filename <- paste(slot.data.basename, 'txt', sep = '.')
#            slot.data.filepath <- paste(destination, slot.data.filename, sep = '/')
#            slot.data          <- data.frame(Segmenter    = segmenter(x),
#                                             SubjectID    = subjectID(x),
#                                             Task         = task(x),
#                                             TextGridXMin = textGridXMin(x),
#                                             TextGridXMax = textGridXMax(x),
#                                             TimeUnit     = timeUnit(x))
#            write.table(slot.data, file = slot.data.filepath, ...)
          }
          )

setMethod(f = 'SaveData',
          signature  = c(x = 'SegmentationData', format = 'character'),
          definition = function(x, format, destination, ...) {
            format       <- tolower(format)
            format.class <- switch(format,
                                   table    = new(Class = 'stringIsTable', format),
                                   textgrid = new(Class = 'stringIsTextGrid', 'TextGrid'))
            SaveData(x, format.class, destination, ...)
          }
          )
