'Hi, Jay. Defining your default suite of favorite functions...'
'Change these in the file ~/.Rprofile'

# Store default par settings for recalling later if needed.
.pardefault <- par(no.readonly = T)

#' repmat
#'
#' A function that is meant to recapitulate the repmat function of matlab.
#' This function is used here to make replicated versions of two point sets
#' so that 2D matrix math can be used to calculate the difference between
#' all possible combinations of points in the two point sets.
#'
#' @param x matrix to repeat/replicate
#' @param m integer number of rows of replication
#' @param n integer number of cols of replication
#' @param loopRows boolean indicating whether the each row should represent a looping sequence of the rows of x (loopRows==T produces a matrix that starts with x[1,], then x[2,]... while loopRows==F produces a matrix that starts with x[1,], then x[1,] repeated m times before switchin to x[2,])
#' @param loopCols boolean indicating whether the each col should represent a looping sequence of the cols of x (loopCols==T produces a matrix that starts with x[,1], then x[,2]... while loopRows==F produces a matrix that starts with x[,1], then x[,1] repeated n times before switchin to x[,2])
#'
#' @return matrix made by 'copying' the x matrix m-by-n times
#' @export
repmat <- function(x, m, n, loopRows=T, loopCols=T)
{
	if(loopRows & loopCols)
	{
		return(x[rep(1:nrow(x), times=m), rep(1:ncol(x), times=n)])
	}
	else if(loopRows & !loopCols)
	{
		return(x[rep(1:nrow(x), times=m), rep(1:ncol(x), each=n)])
	}
	else if(!loopRows & loopCols)
	{
		return(x[rep(1:nrow(x), each=m), rep(1:ncol(x), times=n)])
	}
	else
	{
		return(x[rep(1:nrow(x), each=m), rep(1:ncol(x), each=n)])
	}
}

#' Apply a function to a subset of columns in a data.table
#'
#' This function applyes another function to a subset of the columns
#' in a data.table. The parameter cols takes precedent over the
#' parameter col.filter.
#'
#' Note 1: When both ret.unique and in.place are F, then not all the
#' columns of the table are guaranteed to be returned in the result.
#'
#' Note 2: If in.place=T & ret.unique=T, then only first row of the
#' result is returned.
#'
#' @param x the data.table to apply the function to
#' @param FUN the function to apply
#' @param by row grouping of the data.table for the operation. Use this to retain information from other columns such as Id's. Default = NULL
#' @param cols the columns to which FUN will be applied. Default = NULL
#' @param col.filter a function that returns T or F for each column whether FUN should be applied. Default = is.numeric
#' @param in.place logical whether to perform the calculation by reference, which affects speed and what happens to other columns in the data.table that aren't being calculated upon. When false, only by columns are returned. Default = T
#' @param ret.unique logical whether to return only rows that have unique combinations of the 'by' columns ONLY when in.place=T. Otherwise, by default calculations not done in place only return unique combinations of 'by' columns. Useful when aggregating in.place such as with mean. Default = F
#' @param ... arguments to be passed to FUN
#'
#' @return the resultant data table is EDITED BY REFERENCE only if in.place=T & ret.unique=F, otherwise a copy is returned ('unique' function to remove duplicate rows returns a copy forcing this behavior when ret.unique=T).
#' @export
lapply.data.table <- function(x, FUN, by=NULL, cols=NULL, col.filter=is.numeric, in.place=T, ret.unique=F, ...)
{
     if(is.null(cols) && is.function(col.filter))
     {
          cols <- names(x)[as.logical(as.vector(lapply(x, col.filter)))]
     }

     if(ret.unique)
     {
          if(in.place)
          {
               x <- copy(x)
               x[, c(cols):=lapply(.SD, FUN=FUN, ...), .SDcols=cols, by=by]
               if(is.null(by))
               {
                    x <- x[1]
               }
               else
               {
                    x <- unique(x, by=by)
               }
          }
          else
          {
               x <- x[, lapply(.SD, FUN=FUN, ...), .SDcols=cols, by=by]
               # x <- unique(x, by=by) # not needed as this is done be default with the above call
          }
     }
     else
     {
          if(in.place)
          {
               x[, c(cols):=lapply(.SD, FUN=FUN, ...), .SDcols=cols, by=by]
          }
          else
          {
               x <- x[, lapply(.SD, FUN=FUN, ...), .SDcols=cols, by=by]
               # Produces the same result as if ret.unique=T due to behavior of previous call.
          }
     }
     return(x)
}

#' Make a complex ID column based on other columns
#'
#' This function pastes the values of the specified columns together
#' with a user-defined separator and assigns it to a user-defined
#' column name.
#'
#' @param x the data.table
#' @param cols the cols that define the complex ID
#' @param idName string defining the name of the complex ID column, default is cId
#' @param sep the string that will be used to separate the complex ID components
#'
#' @return edits the table by reference so no return value
#' @export
makeComplexId <- function(x, cols, sep='.', idName='cId')
{
     require(data.table)
     if(!is.data.table(x))
     {
          stop('This function requires a data.table')
     }
     x[, c(idName):=paste(mget(cols), collapse=sep), by=cols]
}

#' Fill missing rows in a data.table
#'
#' After merging tables, sometimes there are combinations of column
#' values that exist in one table but not in another and it is necessary
#' to make rows for these values. This is often helpful for creating
#' a matrix from this initially incomplete data.table.
#'
#' @param x the data.table
#' @param by the columns that uniquely define the rows
#' @param fill the value to fill when data is missing
#'
#' @return a new filled data.table
#' @export
fillMissingRows <- function(x, cols, fill=NULL, tempIdColName='tempId')
{
     require(data.table)
     if(!is.data.table(x))
     {
          stop('This function requires a data.table')
     }
     setkeyv(x, cols=cols)
     ret <- x[J(do.call(CJ,lapply(cols,function(y){unique(get(y))})))]

     if(!is.null(fill))
     {
          # Figure out the types of each column
          blah <- sapply(x, FUN=is.logical)
          blah.log <- data.table(log=blah, name=names(blah))

          blah <- sapply(x, FUN=is.numeric)
          blah.num <- data.table(num=blah, name=names(blah))

          blah <- sapply(x, FUN=is.character)
          blah.char <- data.table(char=blah, name=names(blah))

          blah <- merge(blah.log, blah.num, by=c('name'))
          blah <- merge(blah, blah.char, by=c('name'))

          filler = function(DT, types)
          {
               for (i in names(DT))
               {
                    if(types[name==i][['num']])
                    {
                         DT[is.na(get(i)), (i):=as.numeric(fill)][]
                    }
                    else
                    {
                         if(types[name==i][['char']])
                         {
                              DT[is.na(get(i)), (i):=as.character(fill)][]
                         }
                         else
                         {
                              DT[is.na(get(i)), (i):=as.logical(fill)][]
                         }
                    }
               }
          }
          filler(DT=ret, types=blah)
     }
     return(ret)
}

#' \%=\%
#'
#' Internal interface for the %=% assignment. This will be used to enable
#' Matlab-like assignment of variables from functions that return lists.
#'
#' E.g.,
#'   Matlab: [a, b] = dim(A)
#'   R: l(a, b) \%=\% dim(A)
#'
#' @param l left hand side of the assignment
#' @param r right hand side of the assignment
#'
#' @rdname equals
#'
#' @export
'%=%' <- function(l, r)
{
	UseMethod('%=%')
}

#' \%=\%.lbunch
#'
#' Internal function will be used to enable
#' Matlab-like assignment of variables from functions that return lists.
#'
#' E.g.,
#'
#'   Matlab: [m, n] = dim(A)
#'   R: l(m, n) \%=\% dim(A)
#'
#' @param l left hand side of the assignment
#' @param r right hand side of the assignment
#'
#' @rdname lbunch
#'
#' @export
#'
#' @examples A <- matrix(1:4, ncol=2); l(m, n) \%=\% dim(A);
'%=%.lbunch' <- function(l, r)
{
	Names = lapply(l, as.character)
	Envir = as.environment(-1)

	for (II in 1:length(Names)) {
		Name = Names[[II]]
		assign(Name, r[[II]], pos=Envir)
	}
}

#' l
#'
#' Internal function used with %=% to perform Matlab-like assignment
#' of variables from functions that return a list.
#'
#' @param ... variable to be gathered ans assigned in the list
#'
#' @export
l <- function(...)
{
	List = as.list(substitute(list(...)))[-1L]
	class(List) = 'lbunch'
	List
}

#' Read table from the clipboard
#'
#' This is a cool way to import data using the clipboard. The clipboard table
#' is typically copied as a tab delimited text 'file' connection
#'
#' @param os - c('mac','win'), string value indicating the platform to use
#' @param header - TRUE or FALSE, whether the header is included in the copied table
#' @param sep - text, defining the separator character used between values (needs to be in double quotes)
#' @param use.data.table - TRUE or FALSE, whether to return a data.table (default)
#' @param ... - additional variables supplied are passed onto the underlying read.table function (e.g., stringsAsFactors, comment.char, col.names)
#'
#' @export
read.clipboard <- function(os=c('mac','win'), header=T, sep="\t", use.data.table=T, ...)
{
	if(os[1]=='mac')
	{
		ret <- read.table(pipe('pbpaste'), header=header, sep=sep, ...) # Mac
	}
	else
	{
		ret <- read.table('clipboard', header=header, sep=sep, ...) # Windows
	}
	if(use.data.table)
	{
		return(data.table(ret))
	}
	else
	{
		return(ret)
	}
}

# m1, m2: the sample means
# s1, s2: the sample standard deviations
# n1, n2: the same sizes
# m0: the null value for the difference in means to be tested for. Default is 0.
# equal.variance: whether or not to assume equal variance. Default is FALSE.
t.test2 <- function(m1,s1,n1,m2,s2,n2,m0=0,equal.variance=FALSE)
{
	if( equal.variance==FALSE )
	{
		se <- sqrt( (s1^2/n1) + (s2^2/n2) )
		# welch-satterthwaite df
		df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
	} else
	{
		# pooled standard deviation, scaled by the sample sizes
		se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) )
		df <- n1+n2-2
	}
	t <- (m1-m2-m0)/se
	dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))
	names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
	return(dat)
}

assignToClusters <- function(data, nClusters=2, rndSeed=1234)
{
	library(EMCluster)
	set.seed(rndSeed)
	yo <- data[!is.na(data)]
	x <- data.frame(x=yo)

	# Get basic cluster results (results are potentially out of order)
	emobj <- simple.init(x, nclass = nClusters)
	control <- .EMControl(alpha = 0.99, short.iter = 200, short.eps = 1e-2,
					  fixed.iter = 1, n.candidate = 3,
					  EM.iter = 100, EM.eps = 1e-3, exhaust.iter = 5)
	ret <- emcluster(x, emobj, assign.class = TRUE, EMC=control)

	# Create a data.frame to return
	temp <- data.frame(x=x$x, Cluster.Raw=as.numeric(as.character(ret$class)))
	tempMu <- data.frame(mu=as.vector(ret$Mu), Cluster.Raw=1:nrow(ret$Mu))

	# Order the mu table so we can go through sequentially and rename the clusters in ascending order
	tempMu <- tempMu[order(tempMu$mu),]

	# Create two copies so that you can use one as an original and another as an edited version
	# Originals will be without the '2' while news will be with the '2'
	temp2 <- temp
	tempMu2 <- tempMu
	for(i in 1:nrow(tempMu))
	{
		# Go through mu's in ascending order and assign the ascending order class
		temp2[temp$Cluster.Raw==tempMu$Cluster.Raw[i],'Cluster.Raw'] <- i
		# Also rename the clusters in the duplicate mu table
		tempMu2$Cluster.Raw[i] <- i
	}

	duh <- max(temp2$Cluster.Raw)[1]
	temp2$Cluster.Clean <- duh

	thresh <- list()
	# Go in reverse order from the max cluster number down to 1
	for(i in nrow(tempMu2):2)
	{
		# Find the value that discriminates between each pair of clusters
		tempThresh <- max(temp2[temp2$Cluster.Raw == i-1 & temp2$x < tempMu2$mu[i],'x'])
		if(!length(tempThresh)==0 && !is.infinite(tempThresh[1]))
		{
			# Then we found a threshold
			thresh[[i-1]] <- tempThresh[1]
			# Assign everything below that threshold to the next lowest cluster
			temp2$Cluster.Clean[temp2$x <= tempThresh[1]] <- i-1
		}
	}

	pi <- c()
	n <- nrow(temp2)
	for(i in 1:max(temp2$Cluster.Clean))
	{
		pi <- c(pi, sum(temp2$Cluster.Clean == i)/n)
	}

	return(list(data=temp2, mu=tempMu2$mu, thresh=thresh, emclusterObj=ret, pi=pi))
}

#' Grouped Bar Plots
#'
#' Allows you to plot grouped bar plots based upon a 'grouping' variable or column in the data
#' Requires the error.bar function
#'
#' @param dt - the table with the data
#' @param y.column - name of the columne with the y-values you would like to plot
#' @param color.column - name of the column that should be associated with different bar colors
#' @param group.column - name of the column that should be associated with different groups
#' @param error.upper.column - name of the column with the magnitudes of the upper error bars (use NULL to avoid plotting, default)
#' @param error.lower.column - name of the column with the magnitudes of the lower error bars (default is error.upper.Column, use NULL to avoid plotting)
#' @param main - title for the plot
#' @param ylab - y label
#' @param xlab - x label
#' @param color.names - vector of names to override the color names contained in the table (must be the same length as produced by the table)
#' @param group.names - vector of names to override the group names contained in the table (must be the same length as produced by the table)
#' @param color.color - vector of color values (e.g., c('black', rgb(...), gray(...))) to override the automatically produced colors
#' @param rotate.x.labels - TRUE or FALSE whether to rotate the x labels 90 degrees or not so they fit (default FALSE)
#' @param plot.border - TRUE or FALSE whether to plot a black line around the plot (default TRUE)
#' @param args.error.bar - list with arguments for the error.bar function (default list(length=0.1)) (See error.bar specified in this file)
#' @param legend - TRUE or FALSE, whether to include a legend or not in the graph (only when a group.column is specified and present)
#' @param legend.border - TRUE or FALSE whether to plot a border around the legend
#' @param args.legend - list of parameters to pass to the 'legend' function (overrides automatically determined parameters) (see ?legend)
#' @param mar - numeric vector indicating the margins our the plot border. Units are lines (default c(4.5,4.5,2,2) = c(lower, left, upper, right))
#' @param ... - additional arguments that are passed to the barplot function (see ?barplot)
#'
#' @export
bar <- function(dt, y.column, color.column, group.column=NULL, error.upper.column=NULL, error.lower.column=error.upper.column,
			 main=NULL, ylab=NULL, xlab=NULL, color.names=NULL, group.names=NULL, color.colors=NULL, rotate.x.labels=F, plot.border=T,
			 args.error.bar=list(length=0.1),
			 legend=TRUE, legend.border=F, args.legend=list(),
			 mar=c(4.5,4.5,2,2), ...)
{
     # Detect whether or not upper and lower error bars will be plotted
     has.upper <- FALSE
     if(!is.null(error.upper.column) && error.upper.column %in% names(dt))
     {
          has.upper <- TRUE
     }
     has.lower <- FALSE
     if(!is.null(error.lower.column) && error.lower.column %in% names(dt))
     {
          has.lower <- TRUE
     }

	# Store the display names
	color.names.display <- color.names
	group.names.display <- group.names

	# Convert the table to a data.table
	dt <- as.data.table(dt)

	# Get the y values to plot
	y <- dt[[y.column]]

	# Get check the specified color and group columns
	if(is.null(color.column) || !(color.column %in% names(dt)))
	{
		stop("At least a color.column must be specified and must be present in the provided table of data. Aborting.")
	}
	if(!is.null(group.column) && !(group.column %in% names(dt)))
	{
		stop("The specified group column is not present in the provided table of data")
	}

	# Get the matrix needed for barplot
	if(!is.null(group.column))
	{
	     if(!has.upper && !has.lower)
	     {
	          subDT <- dt[, mget(c(color.column, group.column, y.column))]
	     }
	     if(!has.lower)
	     {
	          subDT <- dt[, mget(c(color.column, group.column, y.column, error.upper.column))]
	     }
	     if(!has.upper)
	     {
	          subDT <- dt[, mget(c(color.column, group.column, y.column, error.lower.column))]
	     }
	     else
	     {
	          subDT <- dt[, mget(c(color.column, group.column, y.column, error.upper.column, error.lower.column))]
	     }

		tempCast <- dcast(subDT, as.formula(paste(color.column, '~', group.column)), value.var=y.column)
		color.names <- tempCast[[1]]
		group.names <- names(tempCast)[2:ncol(tempCast)]
		mat <- as.matrix(tempCast[, 2:ncol(tempCast)])# Get error bar magnitudes if possible
	}
	else
	{
	     if(!has.upper && !has.lower)
	     {
	          subDT <- dt[, mget(c(color.column, y.column))]
	     }
	     if(!has.lower)
	     {
	          subDT <- dt[, mget(c(color.column, y.column, error.upper.column))]
	     }
	     if(!has.upper)
	     {
	          subDT <- dt[, mget(c(color.column, y.column, error.lower.column))]
	     }
		mat <- y
		color.names <- dt[[color.column]]
	}

	# Copy over group and color names from the table if not specified or if specified incorrectly
	if(is.null(color.names.display))
	{
		color.names.display <- color.names
	}
	else if(length(color.names.display) != length(color.names))
	{
		warning("The number of provided color names does not match the number being plotted. Using the color names in the color.column.")
		color.names.display <- color.names
	}
	if(!is.null(group.column))
	{
		if(is.null(group.names.display))
		{
			group.names.display <- group.names
		}
		else if(length(group.names.display) != length(group.names))
		{
			warning("The number of provided group names does not match the number being plotted. Using the group names in the group.column.")
			group.names.display <- group.names
		}
	}

	if(legend && !is.null(group.column))
	{
		args.legend.temp <- list(x="topright", bty=if(!legend.border)"n" else "o", inset=c(0,0))

		if(is.list(args.legend))
		{
			args.legend <- modifyList(args.legend.temp, args.legend)
		}
		else
		{
			args.legend <- args.legend.temp
		}
	}
	else
	{
		args.legend <- NULL
		group.names.display <- NULL
	}

	# Determine the extents of the axes to plot
	if(has.upper)
	{
	     temp <- y + dt[[error.upper.column]]
	     temp <- temp[is.finite(temp)]
		ymax <- max(temp)*21/20
	}
	else
	{
	     temp <- y[is.finite(y)]
		ymax <- max(y[is.finite(y)])*21/20
	}
	if(has.lower)
	{
	     temp <- y - dt[[error.lower.column]]
	     temp <- temp[is.finite(temp)]
		ymin <- min(temp)*21/20
	}
	else
	{
		ymin <- min(y[is.finite(y)])*21/20
	}

	# Compile the arguments to give to barplot
	if(is.null(color.colors))
	{
		color.colors <- hcl(h=seq(0,270, 270/(length(color.names)))[-length(color.names)])
	}
	else if(length(color.colors) != length(color.names))
	{
		warning("The number of colors does not match the number of color.names for the table.")
	}

	if(!is.null(group.column))
	{
		args.barplot <- list(beside=TRUE, height=mat, ylim=c(min(0, ymin), max(0,ymax)), main=main, names.arg=group.names.display,
						 col=color.colors,
						 legend.text=color.names.display, args.legend=args.legend, xpd=TRUE,
						 xlab=if(is.null(xlab)) group.column else xlab,
						 ylab=if(is.null(ylab)) y.column else ylab)
	}
	else
	{
		args.barplot <- list(beside=TRUE, height=mat, ylim=c(min(0, ymin), max(0,ymax)), main=main, names.arg=color.names.display,
						 col=color.colors,
						 legend.text=NULL, args.legend=NULL, xpd=TRUE,
						 xlab=if(is.null(xlab)) group.column else xlab,
						 ylab=if(is.null(ylab)) y.column else ylab)
	}

	args.barplot <- modifyList(args.barplot, list(...))
	if(!is.null(args.barplot[['log']]))
	{
	     if(grepl('y', args.barplot$log, fixed=T) & min(args.barplot$ylim) <= 0)
	     {
	          args.barplot$ylim[1] <- 0.9*min(mat)
	     }
	}

	# Rotate x-axis labels if desired
	if(rotate.x.labels)
	{
		args.barplot <- modifyList(args.barplot, list(las=2))
	}

	# Set the plot margins
	par(mar=mar)

	# If we need to, plot error bars
	if(has.upper || has.lower)
	{
		# Then plot some errobars
		# Sort things appropriately if we have a grouped bar plot, otherwise, no need to
		if(!is.null(group.column))
		{
			# First turn the color and group columns into factors so we can order things 'manually'
			subDT[[color.column]] <- factor( as.character(subDT[[color.column]]), levels=color.names)
			subDT[[group.column]] <- factor( as.character(subDT[[group.column]]), levels=group.names)
			subDT <- subDT[order(subDT[[group.column]], subDT[[color.column]])]
		}

		# Get error bar magnitudes if possible
		upper <- NULL
		if(has.upper)
		{
			upper <- subDT[[error.upper.column]]
		}
		lower <- NULL
		if(has.lower)
		{
			lower <- subDT[[error.lower.column]]
		}

		# Get the xlocations of where to place the error bars
		errloc <- as.vector(do.call(barplot, args.barplot))

		# Compile the error bar arguments
		args.error.final <- list(x=errloc, y=subDT[[y.column]], upper=subDT[[error.upper.column]], lower=lower, draw.lower=has.lower)
		args.error.final <- modifyList(args.error.final, args.error.bar)

		# Draw the error bars
		do.call(error.bar, args.error.final)
	}
	else
	{
		# Just plot the bars
		do.call(barplot, args.barplot)
	}

	# If a plot border is desired, draw it
	if(plot.border) box()
}

plotPolygon <- function(x, y, ...)
{
	xx <- c(x, rev(x))
	yy <- c(rep(0, length(x)), rev(y))
	polygon(xx, yy, ...)
}

# Plot results of clustering. 'data' is the vector of data that was clustered. 'cluster' is the
# corresponding vector of cluster assignments.
plotClusters <- function(data, cluster, thresh=NULL, breaks=40, density=F, polygon=F, ...)
{
	add <- FALSE
	col <- list(red=0, blue=0, green=0, alpha=0.5)
	tempHist <- hist(data, breaks=breaks, plot=FALSE)
	histMax <- which.max(tempHist$counts)
	myBreaks <- tempHist$breaks
	myLim <- max(tempHist$counts)
	scale <- 1
	if(density)
	{
		tempLab <- pretty(c(0, tempHist$density))
		scale <- tempHist$counts[histMax]/tempHist$density[histMax]
		tempAt <- tempLab*scale
	}
	for(i in unique(cluster))
	{
		if(length(data[cluster==i])>0)
		{
			tempCol <- col
			tempCol[((i+2)%%3)+1] <- 1
			freshCol <- do.call(rgb, tempCol)
			if(!density)
			{
				hist(data[cluster==i], breaks=myBreaks, xlab='Bin Value', ylab='Count', col=freshCol, add=add, freq=TRUE, ylim=c(0,myLim), ...)
			}
			else
			{
				duh <- hist(data[cluster==i], breaks=myBreaks, xlab='Bin Value', ylab='Prob. Density', yaxt='n', col=freshCol, add=add, freq=TRUE, ylim=c(0,myLim), ...)
				if(!add)
				{
					axis(2, at=tempAt, labels=tempLab)
				}
			}
			add <- TRUE
		}
	}
	if(!is.null(thresh))
	{
		abline(v=thresh, lwd=2, col='blue')
	}
	return(list(hist=tempHist, scale=scale))
}

normalizeToQuartile <- function(x, quartile=1.0)
{
	return(x/quantile(x, quartile))
}

# Filter order is n, and critical frequency W (digital must be between 0 and 1), and type
filterVector <- function(x, n=3, W=0.5, type='low')
{
	library(signal)
	bf <- butter(n, W)
	return(filtfilt(bf, x))
}

makeNumeric <- function(x)
{
	if(is.numeric(x))
	{
		return(x)
	}
	if(is.factor(x))
	{
		return(as.numeric(as.character(x)))
	}
	if(is.character(x) || is.logical(x))
	{
		return(as.numeric(x))
	}
}

convertColsToNumeric <- function(x, specifically=c(), exceptions=c())
{
	for(col in names(x))
	{
		if(length(specifically) > 0)
		{
			if(col %in% specifically && !is.numeric(x))
			{
				x[, (col):=makeNumeric(get(col))]
			}
		}
		else
		{
			if(!(col %in% exceptions) && !is.numeric(x))
			{
				x[, (col):=makeNumeric(get(col))]
			}
		}
	}
}

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

# This function is needed to plot within data.table because the graphics devices
# get confused while looping/grouping causing the wrong data to be plotted or co-plotted
# Copying the data eliminates this issue. HOWEVER WATCH OUT FOR SENDING data.table
# variables as arguments in '...' as this problem will again arise for that parameter
# (e.g., col=variable, the color will be wrong at times)
data.table.plotClusters <- function(data, cluster, thresh=NULL, breaks, ...)
{
	if(length(which(is.finite(data))) > 0)
	{
		plotClusters(data=copy(data), cluster=cluster, thresh=thresh, breaks=breaks, ...)
		print('Made a plot')
	}
}

# This function is needed to plot within data.table because the graphics devices
# get confused while looping/grouping causing the wrong data to be plotted or co-plotted
# Copying the data eliminates this issue. HOWEVER WATCH OUT FOR SENDING data.table
# variables as arguments in '...' as this problem will again arise for that parameter
# (e.g., col=variable, the color will be wrong at times)
data.table.lines <- function(x, y, ...)
{
	if(length(which(is.finite(x))) > 0)
	{
		lines(x=copy(x), y=copy(y), ...)
		print('Added lines to a plot')
	}
}

# This function is needed to plot within data.table because the graphics devices
# get confused while looping/grouping causing the wrong data to be plotted or co-plotted
# Copying the data eliminates this issue. HOWEVER WATCH OUT FOR SENDING data.table
# variables as arguments in '...' as this problem will again arise for that parameter
# (e.g., col=variable, the color will be wrong at times)
## Same as above, but histogram specific
data.table.hist <- function(x, ...)
{
     if(length(which(is.finite(x))) > 0)
     {
          hist(x=copy(x), ...)
          print('Made a histogram')
     }
}


# This function is needed to plot within data.table because the graphics devices
# get confused while looping/grouping causing the wrong data to be plotted or co-plotted
# Copying the data eliminates this issue. HOWEVER WATCH OUT FOR SENDING data.table
# variables as arguments in '...' as this problem will again arise for that parameter
# (e.g., col=variable, the color will be wrong at times)
data.table.points <- function(x, y, ...)
{
	if(length(which(is.finite(x))) > 0)
	{
		points(x=copy(x), y=copy(y), ...)
		print('Added points to a plot')
	}
}

wilcox.test.combined <- function(data, replCols, condCol, valCol, exact=NULL, two.tailed=TRUE)
{
     require(data.table)
     x1 <- data.table(data)

     getStats <- function(x, y, cond1, cond2, ...)
     {
     	x <- x[is.finite(x)]
     	y <- y[is.finite(y)]
          if(length(x) == 0 || length(y) == 0)
          {
               # This results in a missing row in the results details table for the experiment with either missing x or y data
               return(NULL)
          }
          temp <- wilcox.test(x=x, y=y, ...)
          W <- as.numeric(temp$statistic)

          counts <- table(c(x, y))

          n.x <- length(x)
          n.y <- length(y)
          N <- n.x + n.y

          # Taken from R source code for wilcox.test
          z <- W - n.x * n.y / 2
          z <- z - sign(z)*0.5
          SIGMA <- sqrt((n.x * n.y / 12) * ((n.x + n.y + 1) - sum(counts^3 - counts) / ((n.x + n.y) * (n.x + n.y - 1))))
          z <- z/SIGMA

          p1 <- 2*pnorm(-abs(z))

          p.approx <- 2*pnorm(-abs(z))

          return(list(W=W, p.value=temp$p.value, N=N, median.x=median(x), median.y=median(y), n.x=n.x, n.y=n.y, E=n.x * n.y / 2, V=SIGMA^2, z.score=z, p.value.approx=p.approx))
     }

     conds <- unique(x1[[condCol]])
     if(length(conds) != 2)
     {
     	stop("Must have 2 and only 2 conditions to compare.")
     }
     if(conds[1] < conds[2])
     {
     	conds <- rev(conds)
     }
     x2 <- x1[,getStats(x=.SD[get(condCol)==conds[1]][[valCol]], y=.SD[get(condCol)==conds[2]][[valCol]], exact=exact), by=replCols]
     # x2 <- x1[,getStats(x=.SD[get(condCol)==conds[1]][[valCol]], y=.SD[get(condCol)==conds[2]][[valCol]])$p.value, by=replCols]

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
     cat('p =', p.overall)
     return(list(details=x2, p.overall=p.overall, alpha.prime=1-(1-p.overall)^(nrow(x2)), cond1=conds[1], cond2=conds[2], Wtot=Wtot, Etot=Etot, Vtot=Vtot, z.score=ztot))
}

writeLatexWilcoxCombinedTable <- function(x, captionAddition='', includeVals=F, file=NULL)
{
	library(xtable)
	prettyTest <- getPrettySummary(x$details, x$cond1, x$cond2, includeVals=includeVals)
	prettyX <- data.frame(prettyTest)
	print(prettyX)
	if(!is.null(file))
	{
		sink(file=file)
	}
	cat('%%%% OVERALL STATS %%%%\n')
	cat('% ', x$cond1, ' vs. ', x$cond2, '\n', sep='')
	cat('% Overall p.value =', x$p.overall, '\n')
	cat('% Overall W =', x$Wtot, '\n')
	cat('% Overall E =', x$Etot, '\n')
	cat('% Overall V =', x$Vtot, '\n')
	cat('% Overall z.score =', x$z.score, '\n')
	cat('% alpha.prime =', x$alpha.prime, '\n')
	if(x$p.overall < 1e-3)
	{
		formatted.p.value <- formatC(signif(x$p.overall,digits=3), digits=1, format="e", flag="#")
	}
	else
	{
		formatted.p.value <- formatC(signif(x$p.overall,digits=3), digits=2, format="fg", flag="#")
	}

	theCaption <- paste('{\\bf ', x$cond1, ' vs. ', x$cond2, '.} Overall p-value = ', formatted.p.value, '.', sep='')
	if(captionAddition != '')
	{
		theCaption <- paste0(theCaption, ' ', captionAddition)
	}
	print.xtable(xtable(prettyX, caption=theCaption, align=rep('c',length(names(prettyX))+1)), type='latex', include.rownames = F)
	if(!is.null(file))
	{
		sink()
	}
}

getPSymbol <- function(pval)
{
	ret <- rep('', length(pval))
	ret[pval <= 0.05] <- '*'
	ret[pval <= 0.01] <- '**'
	ret[pval <= 0.001] <- '***'
	return(ret)
}

getDeltaSymbol <- function(V1, V2)
{
	ret <- rep('=', length(V1))
	ret[V1 < V2] <- '<'
	ret[V1 > V2] <- '>'
	return(ret)
}

getSignSymbol <- function(V1)
{
	ret <- rep('=', length(V1))
	ret[V1 > 0] <- '+'
	ret[V1 < 0] <- '-'
	return(ret)
}

getFirstSplit <- function(x)
{
	temp <- strsplit(x, ' ')
	return(temp[[1]][1])
}

getPrettySummary <- function(deets, cond.x, cond.y, includeVals=F)
{
	idcols <- names(deets)[!(names(deets) %in% c('W','p.value','N','median.x','median.y','n.x','n.y','E','V','z.score','p.value.approx','Wi','Ei','Vi'))]
	ret <- copy(deets[,c(idcols, 'W','z.score','p.value','n.x','n.y','median.x','median.y'), with=F])
	ret$p.symbol <- getPSymbol(ret$p.value)
	ret$p.value <- sprintf('%1.3f', deets$p.value)
	ret$difference.symbol <- getDeltaSymbol(ret$median.x, ret$median.y)
	ret$log2.ratio <- log2(ret$median.x/ret$median.y)
	setcolorder(ret, c(idcols,'W','n.x','n.y','median.x','difference.symbol','median.y','log2.ratio','z.score','p.value','p.symbol'))
	if(includeVals)
	{
		setnames(ret, c(idcols,'W','n.x','n.y','median.x','median.y','difference.symbol'), c(idcols,'U',paste0('n.',cond.x),paste0('n.',cond.y),paste0('median.',cond.x),paste0('median.',cond.y),'.'))
	}
	else
	{
		ret$median.x <- NULL
		ret$median.y <- NULL
		ret$difference.symbol <- NULL
		setnames(ret, c(idcols,'W','n.x','n.y'), c(idcols,'U',paste0('n.',cond.x),paste0('n.',cond.y)))
	}
	ret$log2.ratio <- sprintf('%1.2f', ret$log2.ratio)
	return(ret)
}

#' Draw error bars on a graph
#'
#' @param x - x locations to draw center point of the error bars
#' @param y - y corresponding y locations to draw the center point of the error bars
#' @param upper - the upper distance to draw the error bars
#' @param lower - the lower distance to draw the error bars (by default, drawn the same distance as defined by "upper")
#' @param length - the width/length of the error bar tops and bottoms
#' @param draw.lower - true or false, whether to draw the lower bar or not
#'
#' @export
error.bar <- function(x, y, upper, lower=upper, length=0.1, draw.lower=TRUE, ...)
{
     # if(length(x) != length(y) | (length(y) != length(lower) | length(lower) != length(upper))
     #      stop("vectors must be same length")
     if(draw.lower)
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
     data <- as.data.table(dcast(data, as.formula(paste(paste(idCols, collapse='+'), " ~ ", paste(measurementCols, collapse='+'))), value.var = valueCols, ...))
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
	library(gtools)
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

##### Loren's brief additions #####
se <- function(x)
{
     sd(x)/sqrt(length(x))
}

# Be sure to have a trailing line or carriage return after last closing bracket.
