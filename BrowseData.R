rm(list=ls())
source('~/.Rprofile')
library(data.table)

library(foreign)
fileTable <- read.arff('/Users/jaywarrick/Documents/JEX/Feature Extraction/temp/JEXData0000000055.arff')

temp <- read.arff(fileTable$Value[1])
setorder(temp, Id, Label, MaskChannel_ImageChannel, Measurement)
shinyData <- data.table(reorganize(temp, idCols = c('Id'), measurementCols=c('Measurement','MaskChannel_ImageChannel'), valueCols=c('Value')))
sourceGitHubFile(user='jaywarrick', repo='R-General', branch='master', file='DataBrowser/ui.R')
sourceGitHubFile(user='jaywarrick', repo='R-General', branch='master', file='DataBrowser/server.R')
shinyApp(ui=myUI, server=myServer)
