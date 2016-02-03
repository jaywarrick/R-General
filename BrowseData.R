library(foreign)
library(shiny)
library(plotly)
fileTable <- read.arff('/Users/jaywarrick/Documents/JEX/Feature Extraction/temp/JEXData0000000055.arff')
data <- read.arff(fileTable$Value[1])

data <- data.table(data)
setorder(data, Id, Label, MaskChannel_ImageChannel, Measurement)

shinyData <- reorganize(data, idCols = c('Id'), measurementCols=c('Measurement','MaskChannel_ImageChannel'), valueCols=c('Value'))
runApp('/Users/jaywarrick/Public/DropBox/GitHub/R-General/DataBrowser')
