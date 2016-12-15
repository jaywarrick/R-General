'Hi, Jay. Defining your default suite of favorite functions...'
'Change these in the file ~/.Rprofile'

loopingPalette <- function(k, cols=palette()[-1])
{
	n <- length(cols)
	return(cols[((k-1) %% (n)) + 1])
}

getColors <- function(x, cols, conds)
{
	return(cols[match(x, conds)])
}

getAlphas <- function(x, alphas, conds)
{
	return(alphas[match(x, conds)])
}

writePlots <- function(x, y, index, folder='/Users/jaywarrick/Downloads', filePrefix='SingleChannel_', ...)
{
	path <- file.path(folder, paste0(filePrefix,'.pdf'))
	pdf(path, width=7, height=5)
	data.table.plot(x, y, ...)
	legend('topright', legend=c('MM.1R', 'RPMI'), pch=20, bg='white', col=adjustcolor(c('red','blue'), alpha.f=0.5), title='Cell Type')
	dev.off()
	print(paste0('Plotted to file - ', path))
}

# This function is needed to plot within data.table because the graphics devices
# get confused while looping/grouping causing the wrong data to be plotted or co-plotted
# Copying the data eliminates this issue. HOWEVER WATCH OUT FOR SENDING data.table
# variables as arguments in '...' as this problem will again arise for that parameter
# (e.g., col=variable, the color will be wrong at times)
data.table.plot <- function(x, y, ...)
{
	if(length(which(is.finite(x))) > 0)
	{
		plot(x=copy(x), y=copy(y), ...)
		print('Made a plot')
	}
}

wilcox.test.combined <- function(data, replCols, condCol, valCol, two.tailed=TRUE)
{
     require(data.table)
     x1 <- data.table(data)

     getStats <- function(x, y, cond1, cond2, ...)
     {
          if(length(x) == 0 || length(y) == 0)
          {
               # This results in a missing row in the results details table for the experiment with either missing x or y data
               return(NULL)
          }
          temp <- wilcox.test(x=x, y=y, ...)
          W <- as.numeric(temp$statistic)

          counts <- table(c(x, y))

          n <- length(x)
          m <- length(y)
          N <- n + m

          # Taken from R source code for wilcox.test
          z <- W - n * m / 2
          z <- z - sign(z)*0.5
          SIGMA <- sqrt((n * m / 12) * ((n + m + 1) - sum(counts^3 - counts) / ((n + m) * (n + m - 1))))
          z <- z/SIGMA
          
          p1 <- 2*pnorm(-abs(z))

          p.approx <- 2*pnorm(-abs(z))
          return(list(W=W, p.value=temp$p.value, N=length(x) + length(y), E=n * m / 2, V=SIGMA^2, z.approx=z, p.approx=p.approx))
     }

     conds <- unique(x1[[condCol]])
     if(length(conds) != 2)
     {
     	stop("Must have 2 and only 2 conditions to compare.")
     }
     x2 <- x1[,getStats(x=.SD[get(condCol)==conds[1]][[valCol]], y=.SD[get(condCol)==conds[2]][[valCol]]), by=replCols]

     x2[,':='(Wi=W/(N+1), Ei=E/(N+1), Vi=V/((N+1)^2)), by=replCols]

     Wtot <- sum(x2$Wi)
     Etot <- sum(x2$Ei)
     Vtot <- sum(x2$Vi)
     
     ztot <- (Wtot-Etot)/(sqrt(Vtot))

     if(two.tailed)
     {
          p.overall <- 2*pnorm(-abs((Wtot-Etot)/(sqrt(Vtot))))
     }
     else
     {
          p.overall <- pnorm(-abs((Wtot-Etot)/(sqrt(Vtot))))
     }

     return(list(details=x2, p.overall=p.overall, alpha.prime=1-(1-p.overall)^(nrow(x2)), cond1=conds[1], cond2=conds[2], z.score=ztot))
}

error.bar <- function(x, y, upper, lower=upper, length=0.1, drawlower=TRUE, ...)
{
     # if(length(x) != length(y) | (length(y) != length(lower) | length(lower) != length(upper))
     #      stop("vectors must be same length")
     if(drawlower)
     {
          suppressWarnings(arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...))
     }
     else
     {
          suppressWarnings(arrows(x,y+upper, x, y, angle=90, code=1, length=length, ...))
     }
}

getData <- function(dbPath, ds, x, y, type, name)
{
	library(foreign)
     ret <- list()
     ret$ds <- ds
     ret$x <- x
     ret$y <- y
     ret$type <- type
     ret$name <- name
     ret$dbPath <- dbPath
     ret$tmpPath <- file.path(dbPath,'temp','RScriptTempFolder')
     ret$jxdDir <- file.path(dbPath, ds, paste0('Cell_x',x,'_y',y), paste0(type,'-',name))
     ret$jxdFilePath <- file.path(ret$jxdDir, paste0('x',x,'_y',y,'.jxd'))
     if(file.exists(ret$jxdFilePath))
     {
     	ret$jxdTable <- read.arff(ret$jxdFilePath)
     	if(type == 'File')
     	{
     		ret$fileList <- file.path(ret$db,read.arff(ret$jxdFilePath)$Value)
     	}
     	return(ret)
     }
     else
     {
     	warning(paste('Could not find the specified file:', ret$jxdFilePath))
     	return(NULL)
     }
}

st <- function(...)
{
     out <- '';
     for(txt in list(...))
     {
          out <- paste(out, as.character(txt), sep='')
     }
     return(out)
}

#' Reorganize a table from long form to wide form.
#'
#' This function can be applied to data.frame objects or data.table objects. It
#' returns the same type as given.
#'
#' @param data A data.table or data.frame
#' @param idCols A character vector of id column names (e.g., 'Id')
#' @param measurementCols A character vector of column names that describe measurements (e.g., a column
#' called 'Measurement' with values such as 'Min', 'Max', 'Mean', etc.
#' @param valueCols A character vector of column names (typically one) that contains the numeric data
#' to reorganize into wide format (e.g., 'Value')
#'
#' @export
reorganize <- function(data, idCols=NULL, measurementCols='Measurement', valueCols='Value', ...)
{
	library(data.table)
     isDataTable <- FALSE
     if(is.data.table(data))
     {
          isDataTable <- TRUE
     }
     else
     {
          data <- data.table(data)
     }
     
     # Parse commas to indicate that the string should be split into multiple items.
     measurementCols <- strsplit(measurementCols, ',', fixed=T)
     
     # Leading and trailing spaces are not good so remove them in case (makes it easier for JEX)
     measurementCols <- mapply(gsub, '^\\s+|\\s+$', '', measurementCols)

     # If idCols = NULL, then use all remaining cols except measurementCols and valueCols
     if(is.null(idCols))
     {
          idCols <- names(data)[!(names(data) %in% c(measurementCols, valueCols))]
     }

     formula <- as.formula(paste(paste(idCols, collapse='+'), " ~ ", paste(measurementCols, collapse='+')))
     print(formula)
     data <- dcast(data, as.formula(paste(paste(idCols, collapse='+'), " ~ ", paste(measurementCols, collapse='+'))), value.var = valueCols, ...)
     if(isDataTable)
     {
          return(data)
     }
     else
     {
          return(data.frame(data))
     }
}

#' Take an arff file and reorganize it into a more standard 'table' format. Specifically this is used to
#' import an arff file from JEX as JEX uses a column called 'Measurement' to define the type of measurment
#' or property being stored and 'Value', the value of that property.
#'
#' @param data An object that is the result of using foreign::read.arff(file) on an arff file
#' @param baseName An optional basename to add to whatever label is in the \code{nameCol} portion of each row entry
#' @param convertToNumeric An option to convert the columns of information within \code{data} leading up to
#' \code{nameCol} and \code{valueCol} to numeric or to leave as text. Default is to convert to numeric (i.e., TRUE)
#' @param nameCol The name of the column that describes the nature of the value in the \code{valueCol}
#' @param valueCol The name of the column with the values of the properties listed in the \code{nameCol}
reorganizeTable <- function (data, baseName = NA, convertToNumeric = TRUE, nameCol = "Measurement", valueCol = "Value")
{
     require(plyr)
     idCols <- names(data)
     idCols <- idCols[-which(idCols %in% c(nameCol, valueCol))]
     newData <- data.frame(stringsAsFactors = FALSE)
     measurements <- unique(data[, nameCol])
     for (m in measurements) {
          if (is.na(baseName)) {
               newColName <- m
               newColName <- gsub(" ", ".", newColName, fixed = TRUE)
          }
          else {
               newColName <- paste(baseName, ".", m, sep = "")
               newColName <- gsub(" ", ".", newColName, fixed = TRUE)
          }
          temp <- data[data[, nameCol] == m, ]
          temp2 <- temp[, c(idCols, valueCol)]
          if(length(idCols) == 0)
          {
               temp2 <- data.frame(ReallyRandomNameYo=temp2)
               names(temp2) <- newColName
          }
          else
          {
               names(temp2)[names(temp2) == valueCol] <- newColName
          }
          if (nrow(newData) == 0)
          {
               newData <- temp2
          }
          else
          {
               newData <- merge(newData, temp2, by = idCols)
          }
     }
     if (convertToNumeric) {
          for (n in idCols) {
               newData[, n] <- as.numeric(as.character(newData[, n]))
          }
     }
     return(newData)
}

reorganizeFeatureTable <- function (data, baseName = NA, specialNames = c("Channel"), convertToNumeric = TRUE, nameCol='Measurement', valueCol='Value')
{
     require(data.table)

     myfunc <- function(keys, values, allKeys, sd)
     {
          names(values) <- keys
          ret <- as.list(values[allKeys])
          names(ret) <- allKeys
          return(ret)
     }

     # First collapse specialName columns into the nameCol column
     for(specialName in specialNames)
     {
          if(specialName != '' && specialName %in% names(data))
          {
               data[,specialName] <- gsub(" ", "", data[,specialName])
               data[,nameCol] <- paste0(data[,nameCol], '.', data[,specialName])
          }
     }
     data <- data[, !(names(data) %in% specialNames)]


     # Grab the nameCol, idCols, and valueCol
     idCols <- names(data)
     idCols <- idCols[-which(idCols %in% c(nameCol, valueCol))]
     measurements <- unique(data[, nameCol])

     data <- data.table(data)
     data <- data[,myfunc(keys=.SD[[nameCol]],values=.SD[[valueCol]],allKeys=unique(data[[nameCol]]), sd=.SD),by=idCols]
     data <- data.frame(data)

     if (convertToNumeric) {
          for (n in idCols) {
               data[, n] <- as.numeric(as.character(data[, n]))
          }
     }
     return(data)
}

first <- function(x)
{
     return(x[1])
}

last <- function(x)
{
     require(pracma)
     return(x[numel(x)])
}

getWeightedR2 <- function(y, model)
{
     r <- residuals(model)
     f <- fitted(model)
     w <- weights(model)
     return(getWeightedR2_Vectors(y, r, f, w))
}

getWeightedR2_Vectors <- function(y, r, f, w)
{
     SSr <- sum(w*r^2);
     SSt <- sum(w*(y-mean(y))^2)
     return(1-(SSr/SSt))
}

siegel.tukey <- function(x, y, id.col = FALSE, adjust.median = F,
                         rnd = -1, alternative = "two.sided", mu = 0, paired = FALSE,
                         exact = FALSE, correct = TRUE, conf.int = FALSE, conf.level = 0.95) {
     ###### published on:
     #   http://www.r-statistics.com/2010/02/siegel-tukey-a-non-parametric-test-for-equality-in-variability-r-code/
     ## Main author of the function:  Daniel Malter

     # x: a vector of data

     # y: Group indicator (if id.col=TRUE); data of the second
     #   group (if
     # id.col=FALSE). If y is the group indicator it MUST take 0
     #   or 1 to indicate
     # the groups, and x must contain the data for both groups.

     # id.col: If TRUE (default), then x is the data column and y
     #   is the ID column,
     # indicating the groups. If FALSE, x and y are both data
     #   columns. id.col must
     # be FALSE only if both data columns are of the same length.

     # adjust.median: Should between-group differences in medians
     #   be leveled before
     # performing the test? In certain cases, the Siegel-Tukey
     #   test is susceptible
     # to median differences and may indicate significant
     #   differences in
     # variability that, in reality, stem from differences in
     #   medians.

     # rnd: Should the data be rounded and, if so, to which
     #   decimal? The default
     # (-1) uses the data as is. Otherwise, rnd must be a
     #   non-negative integer.
     # Typically, this option is not needed. However,
     #   occasionally, differences in
     # the precision with which certain functions return values
     #   cause the merging
     # of two data frames to fail within the siegel.tukey
     #   function. Only then
     # rounding is necessary. This operation should not be
     #   performed if it affects
     # the ranks of observations.

     # arguments passed on to the Wilcoxon test. See
     #   ?wilcox.test

     # Value: Among other output, the function returns the data,
     #   the Siegel-Tukey
     # ranks, the associated Wilcoxon's W and the p-value for a
     #   Wilcoxon test on
     # tie-adjusted Siegel-Tukey ranks (i.e., it performs and
     #   returns a
     # Siegel-Tukey test). If significant, the group with the
     #   smaller rank sum has
     # greater variability.

     # References: Sidney Siegel and John Wilder Tukey (1960) A
     #   nonparametric sum
     # of ranks procedure for relative spread in unpaired
     #   samples. Journal of the
     # American Statistical Association. See also, David J.
     #   Sheskin (2004)
     # Handbook of parametric and nonparametric statistical
     #   procedures. 3rd
     # edition. Chapman and Hall/CRC. Boca Raton, FL.

     # Notes: The Siegel-Tukey test has relatively low power and
     #   may, under certain
     # conditions, indicate significance due to differences in
     #   medians rather than
     # differences in variabilities (consider using the argument
     #   adjust.median).

     # Output (in this order)

     # 1. Group medians (after median adjustment if specified)
     # 2. Wilcoxon-test for between-group differences in medians
     #   (after the median
     # adjustment if specified)
     # 3. Data, group membership, and the Siegel-Tukey ranks
     # 4. Mean Siegel-Tukey rank by group (smaller values indicate
     #   greater
     # variability)
     # 5. Siegel-Tukey test (Wilcoxon test on tie-adjusted
     #   Siegel-Tukey ranks)

     is.formula <- function(x) class(x) == "formula"

     if (is.formula(x)) {
          y <- do.call(c, list(as.name(all.vars(x)[2])), envir = parent.frame(2))
          x <- do.call(c, list(as.name(all.vars(x)[1])), envir = parent.frame(2))  # I am using parent.frame(2) since if the name of the variable in the equation is 'x', then we will mistakenly get the function in here instead of the vector.
          id.col <- TRUE
          # print(x)
          # print(ls.str())
          # data=data.frame(c(x,y),rep(c(0,1),c(length(x),length(y))))
          # print(data)
     }

     if (id.col == FALSE) {
          data = data.frame(c(x, y), rep(c(0, 1), c(length(x), length(y))))
     } else {
          data = data.frame(x, y)
     }
     names(data) = c("x", "y")
     data = data[order(data$x), ]
     if (rnd > -1) {
          data$x = round(data$x, rnd)
     }

     if (adjust.median == T) {
          cat("\n", "Adjusting medians...", "\n", sep = "")
          data$x[data$y == 0] = data$x[data$y == 0] - (median(data$x[data$y ==
                                                                          0]))
          data$x[data$y == 1] = data$x[data$y == 1] - (median(data$x[data$y ==
                                                                          1]))
     }
     cat("\n", "Median of group 1 = ", median(data$x[data$y == 0]),
         "\n", sep = "")
     cat("Median of group 2 = ", median(data$x[data$y == 1]), "\n",
         "\n", sep = "")
     cat("Testing median differences...", "\n")
     print(wilcox.test(data$x[data$y == 0], data$x[data$y == 1]))

     # The following must be done for the case when id.col==F
     x <- data$x
     y <- data$y

     cat("Performing Siegel-Tukey rank transformation...", "\n",
         "\n")



     sort.x <- sort(data$x)
     sort.id <- data$y[order(data$x)]

     data.matrix <- data.frame(sort.x, sort.id)

     base1 <- c(1, 4)
     iterator1 <- matrix(seq(from = 1, to = length(x), by = 4)) -
          1
     rank1 <- apply(iterator1, 1, function(x) x + base1)

     iterator2 <- matrix(seq(from = 2, to = length(x), by = 4))
     base2 <- c(0, 1)
     rank2 <- apply(iterator2, 1, function(x) x + base2)

     #print(rank1)
     #print(rank2)

     if (length(rank1) == length(rank2)) {
          rank <- c(rank1[1:floor(length(x)/2)], rev(rank2[1:ceiling(length(x)/2)]))
     } else {
          rank <- c(rank1[1:ceiling(length(x)/2)], rev(rank2[1:floor(length(x)/2)]))
     }


     unique.ranks <- tapply(rank, sort.x, mean)
     unique.x <- as.numeric(as.character(names(unique.ranks)))

     rank.matrix <- data.frame(unique.x, unique.ranks)

     ST.matrix <- merge(data.matrix, rank.matrix, by.x = "sort.x",
                        by.y = "unique.x")

     print(ST.matrix)

     cat("\n", "Performing Siegel-Tukey test...", "\n", sep = "")

     ranks0 <- ST.matrix$unique.ranks[ST.matrix$sort.id == 0]
     ranks1 <- ST.matrix$unique.ranks[ST.matrix$sort.id == 1]

     cat("\n", "Mean rank of group 0: ", mean(ranks0), "\n", sep = "")
     cat("Mean rank of group 1: ", mean(ranks1), "\n", sep = "")

     print(wilcox.test(ranks0, ranks1, alternative = alternative,
                       mu = mu, paired = paired, exact = exact, correct = correct,
                       conf.int = conf.int, conf.level = conf.level))

     return(list(ranks0=ranks0, ranks1=ranks1))
}

paperParams <- function(rows, cols, labsize=1)
{
     par(mfrow = c(rows,cols))
     par(lwd = 1, col='black')
     par(mar = c(3.7,3.7,0.8,0.8)); # beyond plot frame [b,l,t,r]
     par(mgp = c(2.5,0.8,0)); # placement of axis labels [1], numbers [2] and symbols[3]
     par(oma = c(0.1,0.1,0.1,0.1)); # cutoff beyond other measures

     par(cex = 1.25)
     par(cex.lab = labsize) # axislabelsize"
     par(cex.axis = 0.75)
}

paperPlot <- function(xlimit=c(0.01,1), ylimit=c(0.01,1), xlabel='x', ylabel='y', xcol='darkgreen', ycol='darkred', plotaxes=TRUE, log='', xticks=NULL, yticks=NULL, letter='a)')
{


     #     xlimit=c(min(lx),max(lx))
     #     ylimit=c(min(ly),max(ly))
     #     xlabel=expression(paste('[Virus] ', alpha, ' (1/h)'))
     #     ylabel='[Virus] Max Intensity'
     #     xcol=xcol
     #     ycol=ycol
     #     log='y'
     #     plot(x=c(),y=c(), xlim=xlimit, ylim=ylimit, xlab=xlabel, ylab=ylabel, axes=TRUE, log=log)
     #
     plot(c(),c(), xlim=xlimit, ylim=ylimit, xlab='', ylab='', axes=FALSE, log=log)
     title(ylab=ylabel, col.lab=ycol)
     title(xlab=xlabel, col.lab=xcol)
     if(plotaxes)
     {

          if(is.null(yticks))
          {
               axis(2, las=1)
          }
          else if(is.character(yticks))
          {
               axis(2, las=1, at=as.numeric(yticks), labels=yticks)
          }
          else if(is.expression(yticks))
          {
               axis(2, las=1, at=unlist(lapply(eval(as.list(yticks)), 'eval')), labels=yticks)
          }

          if(is.null(xticks))
          {
               axis(1, las=1)
          }
          else if(is.character(xticks))
          {
               axis(1, las=1, at=as.numeric(xticks), labels=xticks)
          }
          else if(is.expression(xticks))
          {
               axis(1, las=1, at=unlist(lapply(eval(as.list(xticks)), 'eval')), labels=xticks)
          }
     }

     box(col='black',lwd=2)
     if(letter != '')
     {
          mtext(side=3,adj=0,text=paste('(',letter,')',sep=''),padj=1.8, outer=TRUE, cex=1.5)
     }
}

paperHist <- function(letter='a)',histogram, xlabel='bins', ylabel='Density', labsize=1.5, xlim, ylim, xcol='red', bar.col='grey')
{
     #     plot(c(), c(), frame.plot=FALSE, xlab=xlabel, ylab='Density', cex.lab=labsize, main=NULL, yaxs='i', ylim=c(0,max(histogram$density)*1.04), xlim=c(min(histogram$breaks), max(histogram$breaks)))
     #     lines(histogram)
     par(mar = c(3.7,4,0.8,0.8)); # beyond plot frame [b,l,t,r]
     myHist(histogram, main=NULL, axes=FALSE, xlab=xlabel, ylab=ylabel, xlim=xlim, ylim=ylim, freq=FALSE, col=bar.col, col.lab=xcol)
     axis(1,las=1)
     axis(2,las=1)
     box(col='black',lwd=2, bty='l')
     mtext(side=3,adj=0,text=paste('(',letter,')',sep=''),padj=1.8, outer=TRUE, cex=1.3)
}

myHist <- function (histogram, freq = equidist, density = NULL, angle = 45, col = NULL,
                    border = par("fg"), lty = NULL, main = paste("Histogram of",
                                                                 histogram$xname), xlim = range(histogram$breaks), ylim = NULL, xlab = histogram$xname,
                    ylab, axes = TRUE, labels = FALSE, add = FALSE, width=1.0, offset=(1.0-width)/2, ...)
{
     y <- histogram$counts
     if(!freq)
     {
          y <- histogram$density
     }
     nB <- length(histogram$breaks)
     if (is.null(ylim))
          ylim <- range(y, 0)
     if (missing(ylab))
          ylab <- if (!freq)
               "Density"
     else "Frequency"
     plot.new()
     plot.window(xlim, ylim, "", xaxs='i', yaxs='i')
     title(main = main, xlab = xlab, ylab = ylab, ...)
     if (axes) {
          axis(1, ...)
          axis(2, ...)
     }

     if (width != 1.0 || offset != 0) {
          # Calculates the width of each bar in the histogram
          delta.breaks <- histogram$breaks[-1] - histogram$breaks[-nB];
          x.offset <- offset * delta.breaks;
          x.width <- width * delta.breaks;
          x <- histogram$breaks[-nB]+x.offset;
          rect(x, 0, x+x.width, y, col=col, border=border, angle = angle, density = density, lty=lty);
     } else {
          rect(histogram$breaks[-nB], 0, histogram$breaks[-1], y, col = col, border = border,
               angle = angle, density = density, lty = lty)
     }

     if ((logl <- is.logical(labels) && labels) || is.character(labels))
          text(histogram$mids, y, labels = if (logl) {
               if (freq)
                    histogram$counts
               else round(histogram$density, 3)
          }
          else labels, adj = c(0.5, -0.5))
     invisible()
}

getDensityColors <- function(x, y)
{
     #     library(hexbin)
     xunique <- unique(x)
     yunique <- unique(y)
     xync <- data.frame(x=c(), y=c(), n=c(), c=c())
     for(xu in xunique)
     {
          for(yu in yunique)
          {
               i <- sum(x==xu & y==yu)
               xync <- rbind(xync, data.frame(x=xu, y=yu, n=i))
          }
     }
     xync <- subset(xync, n > 0)
     theColors <- rev(rainbow(max(xync$n), end=0.6))
     xync$c <- theColors[xync$n]
     return(list(colors=theColors, data=xync))
}

multi.mixedorder <- function(..., na.last = TRUE, decreasing = FALSE){
     # For example...
     # data <- data[with(data, multi.mixedorder(Day, Conc, Sample.ID, relTimeStamp)), ]
     do.call(order, c(
          lapply(list(...), function(l){
               if(is.character(l)){
                    factor(l, levels=mixedsort(unique(l)))
               } else {
                    l
               }
          }),
          list(na.last = na.last, decreasing = decreasing)
     ))
}

merge.lists <- function(list1, list2)
{
     for(name in names(list2))
     {
          list1[[name]] <- list2[[name]]
     }
     return(list1)
}

source_https <- function(url, ...)
{
     # load package
     require(RCurl)

     # parse and evaluate each .R script
     sapply(c(url, ...), function(u)
     {
          eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
     })
}

sourceGitHubFile <- function(user, repo, branch, file)
{
     require(curl)
     destfile <- tempfile()
     fileToGet <- paste0("https://raw.githubusercontent.com/", user, "/", repo, "/", branch, "/", file)
     curl_download(url=fileToGet, destfile)
     source(destfile)
}

lseq <- function(from, to, length.out)
{
     exp(seq(log(from), log(to), length.out = length.out))
}

jplot <- function(x, y, text=c())
{
     xlab <- list(title = deparse(substitute(x)))
     ylab <- list(title = deparse(substitute(y)))
     plot_ly(mode='markers', x=x, y=y, text=text) %>%
          layout(xaxis = xlab, yaxis = ylab)
}

# Be sure to have a trailing line or carriage return after last closing bracket.
