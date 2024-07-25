#' Font installation instructions.
#'
#' Go to the jaywarrick/R-General github repository.
#' There are 5 open fonts in folders - Open Sans, Roboto, Quicksand, Muli, and Montserrat
#'
#' On windows - type 'fonts' in the windows search bar and hit enter to open system fonts folder
#' then drag the actual .ttf files from within the github folders to the font folder to install them
#'
#' On mac - open the system's 'Font Book.app' and you can drag each font folder to the
#' app to install them. Important note: The font must be installed for all users. To do so
#' you must drag the font to the "Computer" folder in Font Book. The first font installed
#' will install to the "User", which then allows the "Computer" folder to be visible.
#' Drag remaining into "Computer" and then redrag first one into "Computer" and delete
#' the first one from the "User" folder.
#'
#' THEN you can run the following script all the way through the last step where the 'extrafont'
#' package is installed, loaded, and run to load all system and user fonts, such as those
#' just installed.


install.packages('party')
install.packages('rpart')
install.packages('randomForest')
install.packages('e1071')
install.packages('rgl')
install.packages('caret')
install.packages('plotly')
install.packages('boot')
install.packages('peakPick')
install.packages('ff')
install.packages('geometry')
install.packages('gsl')
install.packages('hypergeo')
install.packages('tiff')

install.packages('Matrix')
install.packages('matrixStats')
install.packages('corrgram')
install.packages('pracma')
install.packages('smoother')
install.packages('bit64')
install.packages('signal')
install.packages('xtable')
install.packages('foreign')
install.packages('gtools')
install.packages('RCurl')
install.packages('curl')
install.packages('spatstat')
install.packages('shotGroups')
install.packages('plyr')
install.packages('psych')
install.packages('EMCluster')
install.packages('data.table')
install.packages("Rserve", , "http://rforge.net/", type = "source")
install.packages('TeachingDeoms')
install.packages('gridExtra')
install.packages('units')
install.packages('survminer')
install.packages('extrafont')
install.packages('fontcm')
install.packages('Cairo')
install.packages('varhandle')
install.packages('VGAM')
install.packages('NMOF')
install.packages('openssl')
install.packages('deSolve')


install.packages('qpcR')
install.packages('shinyjs')
install.packages('readxl')
install.packages('writexl')
install.packages('data.table')
install.packages('shinyalert')
install.packages('shinyAce')
install.packages('zip')
install.packages('R.utils')
install.packages('httr')
install.packages('DT')

# devtools::install_github('nstrayer/shinysense')

# Enabling bash on Windows for data.table line parsing capabilities...
# Install Cygwin https://cygwin.com/install.html
# Add C:\cygwin64\bin to the system 'Path' variable (right-click computer > advanced settings > environment variables)

library(extrafont)
# install.packages('extrafontdb') # To reset font table
font_import()
y
