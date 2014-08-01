print('Hi, Jay. Defining your default suite of favorite functions...')
print('Change these in the file ~/.Rprofile')

st <- function(...)
{
    out <- '';
    for(txt in list(...))
    {
        out <- paste(out, as.character(txt), sep='')
    }
    return(out)
}

reorganizeTable <- function(data, baseName=NA, convertToNumeric=TRUE)
{
    library(plyr)
    idCols <- names(data)
    idCols <- idCols[-which(idCols %in% c('Measurement','Value'))]
    newData <- data.frame(stringsAsFactors=FALSE)
    measurements <- unique(data$Measurement)
    #     m <- measurements[1]
    #     browser()
    for(m in measurements)
    {
        if(is.na(baseName))
        {
            newColName <- m
            newColName <- gsub(' ','.',newColName, fixed=TRUE) # Get rid of extraneous spaces
        }else
        {
            newColName <- paste(baseName,'.',m, sep='')
            newColName <- gsub(' ','.',newColName, fixed=TRUE) # Get rid of extraneous spaces
        }
        
        temp <- subset(data, Measurement==m)
        temp2 <- temp[,c(idCols,'Value')]
        names(temp2)[names(temp2)=="Value"] <- newColName
        if(nrow(newData) == 0)
        {
            newData <- temp2
        }else
        {
            newData <- merge(newData, temp2, by=idCols)
        }
    }
    
    if(convertToNumeric)
    {
        for(n in idCols)
        {
            newData[,n] <- as.numeric(as.character(newData[,n]))
        }
    }
    
    return(newData)
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
    
    #  arguments passed on to the Wilcoxon test. See
    #   ?wilcox.test
    
    # Value: Among other output, the function returns the data,
    #   the Siegel-Tukey
    # ranks, the associated Wilcoxons W and the p-value for a
    #   Wilcoxon test on
    # tie-adjusted Siegel-Tukey ranks (i.e., it performs and
    #   returns a
    # Siegel-Tukey test). If significant, the group with the
    #   smaller rank sum has
    # greater variability.
    
    # References: Sidney Siegel and John Wilder Tukey (1960) A
    #   nonparametric sum
    # of ranks procedure for relative spread in unpaired
    #   samples. Journal of the
    # American Statistical Association. See also, David J.
    #   Sheskin (2004)
    # Handbook of parametric and nonparametric statistical
    #   procedures. 3rd
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

# Be sure to have a trailing line or carriage return after last closing bracket.