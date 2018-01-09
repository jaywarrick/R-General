'Hi, Jay. Defining your default suite of favorite functions...'
'Change these in the file ~/.Rprofile'

##### NOTES: #####

# This works to keep certain columns along with new calcs
# duh[MaskChannel < 3, append(mget(blah), list(v1=v1, v2=v1*2)), by='MaskChannel']

# Store default par settings for recalling later if needed.
# .pardefault <- par(no.readonly = T)

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

apply2D <- function(x, col.fun=NULL, by=NULL, row.fun=NULL, ..., mCols=NULL, mColsContaining=NULL, mColFilter=NULL)
{
     if(is.null(mCols))
     {
          if(!is.null(mColsContaining))
          {
               mCols <- getColNamesContaining(x, mColsContaining)
          }
          else if(!is.null(mColFilter))
          {
               mCols <- as.logical(lapply(x, mColFilter))
               mCols <- names(x)[mCols]
          }
          else
          {
               mCols <- names(x)
          }
     }

     if(!is.null(col.fun))
     {
          x <- x[, lapply(.SD, col.fun), by=by]
     }

     if(!is.null(row.fun))
     {
          ret <- x[, apply(.SD, 1, row.fun), .SDcols=mCols]
     }
     return(ret)
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
lapply.data.table <- function(x, FUN, by=NULL, cols=NULL, col.filter=is.numeric, in.place=F, ret.unique=F, ...)
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

		blah <- sapply(x, FUN=is.double)
		blah.double <- data.table(double=blah, name=names(blah))

		blah <- sapply(x, FUN=is.integer)
		blah.int <- data.table(int=blah, name=names(blah))

		blah <- sapply(x, FUN=is.character)
		blah.char <- data.table(char=blah, name=names(blah))

		blah <- merge(blah.log, blah.num, by=c('name'))
		blah <- merge(blah, blah.char, by=c('name'))
		blah <- merge(blah, blah.double, by=c('name'))
		blah <- merge(blah, blah.int, by=c('name'))

		filler = function(DT, types)
		{
			for (i in names(DT))
			{
				if(types[name==i][['num']])
				{
					if(types[name==i][['double']])
					{
						DT[is.na(get(i)), (i):=as.double(fill)][]
					}
					if(types[name==i][['int']])
					{
						DT[is.na(get(i)), (i):=as.integer(fill)][]
					}
					else
					{
						DT[is.na(get(i)), (i):=as.numeric(fill)][]
					}

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

assignToClustersXY <- function(xData, yData, xClusters=2, yClusters=xClusters, rndSeed=1234)
{
     clusterX <- assignToClusters(xData, nClusters=xClusters, rndSeed=rndSeed)
     clusterY <- assignToClusters(yData, nClusters=yClusters, rndSeed=rndSeed)
     newClusters <- paste(clusterX$data$Cluster.Clean, '.', clusterY$data$Cluster.Clean)
     uniqueNewClusters <- sort(unique(newClusters))
     newClusters <- match(newClusters,uniqueNewClusters)
     return(list(clusters=newClusters, threshX=clusterX$thresh[[1]], threshY=clusterY$thresh[[1]]))
}

setClustersXY <- function(x, xcol, ycol, threshX, threshY, clusterCol='cluster')
{
     x[, cluster:=1]
     x[get(ycol) > threshX & get(xcol) <= threshX, cluster:=2]
     x[get(ycol) <= threshX & get(xcol) > threshX, cluster:=3]
     x[get(ycol) > threshX & get(xcol) > threshX, cluster:=4]
}

assignToClustersN <- function(data, nClusters=2, rndSeed=1234, clusterCol='cluster')
{
     library(EMCluster)
     set.seed(rndSeed)

     if(ncol(data)==1)
     {
          return(assignToClusters(data, nClusters=nClusters, rndSeed=rndSeed))
     }

     # Overall approach is to cluster rows with complete data,
     # then get the thresholds between groups
     # Determine which rows have NA values
     yo <- data[numColsTrue(data, test=function(x){!is.finite(x)})==0]
     x <- as.matrix(yo)

     # Get basic cluster results (results are potentially out of order)
     emobj <- simple.init(x, nclass = nClusters)
     control <- .EMControl(alpha = 0.99, short.iter = 200, short.eps = 1e-2,
                           fixed.iter = 1, n.candidate = 3,
                           EM.iter = 100, EM.eps = 1e-3, exhaust.iter = 5)
     ret <- emcluster(x, emobj, assign.class = TRUE, EMC=control)

     data[, c(clusterCol):= assign.class(as.matrix(data), ret, return.all = FALSE)$class]

     return(ret)

     # # Create a data.frame to return
     # temp <- data.frame(x=x$x, Cluster.Raw=as.numeric(as.character(ret$class)))
     # tempMu <- data.frame(mu=as.vector(ret$Mu), Cluster.Raw=1:nrow(ret$Mu))
     #
     # # Order the mu table so we can go through sequentially and rename the clusters in ascending order
     # tempMu <- tempMu[order(tempMu$mu),]
     #
     # # Create two copies so that you can use one as an original and another as an edited version
     # # Originals will be without the '2' while news will be with the '2'
     # temp2 <- temp
     # tempMu2 <- tempMu
     # for(i in 1:nrow(tempMu))
     # {
     #      # Go through mu's in ascending order and assign the ascending order class
     #      temp2[temp$Cluster.Raw==tempMu$Cluster.Raw[i],'Cluster.Raw'] <- i
     #      # Also rename the clusters in the duplicate mu table
     #      tempMu2$Cluster.Raw[i] <- i
     # }
     #
     # duh <- max(temp2$Cluster.Raw)[1]
     # temp2$Cluster.Clean <- duh
     #
     # thresh <- list()
     # # Go in reverse order from the max cluster number down to 1
     # for(i in nrow(tempMu2):2)
     # {
     #      # Find the value that discriminates between each pair of clusters
     #      tempThresh <- max(temp2[temp2$Cluster.Raw == i-1 & temp2$x < tempMu2$mu[i],'x'])
     #      if(!length(tempThresh)==0 && !is.infinite(tempThresh[1]))
     #      {
     #           # Then we found a threshold
     #           thresh[[i-1]] <- tempThresh[1]
     #           # Assign everything below that threshold to the next lowest cluster
     #           temp2$Cluster.Clean[temp2$x <= tempThresh[1]] <- i-1
     #      }
     # }
     #
     # pi <- c()
     # n <- nrow(temp2)
     # for(i in 1:max(temp2$Cluster.Clean))
     # {
     #      pi <- c(pi, sum(temp2$Cluster.Clean == i)/n)
     # }
     #
     # return(list(data=temp2, mu=tempMu2$mu, thresh=thresh, emclusterObj=ret, pi=pi))
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
     # Save default margins
     default.mar <- par('mar')

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
	     else
	     {
	          subDT <- dt[, mget(c(color.column, y.column, error.upper.column, error.lower.column))]
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
	par(mar=default.mar)
}

plotPolygon <- function(x, y, ...)
{
	xx <- c(x, rev(x))
	yy <- c(rep(0, length(x)), rev(y))
	polygon(xx, yy, ...)
}

plotClustersN <- function(xdata, ydata, clusterNums, alpha=0.3, pch=20, ...)
{
     my.colors <- setColor(loopingPalette(clusterNums), alpha=alpha)
     plot(xdata, ydata, col=my.colors, pch=pch, ...)
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

adjustColor <- function(my.colors, alpha.factor)
{
	require(grDevices)
	x <- col2rgb(my.colors, alpha = TRUE)/255
	x[4L,] <- x[4L,]*alpha.factor
	ret <- rgb(x[1L,],x[2L,],x[3L,],x[4L,])
	return(ret)
}

removeAlpha <- function(my.colors)
{
	require(grDevices)
	x <- col2rgb(my.colors)/255
	ret <- rgb(x[1L,],x[2L,],x[3L,])
	return(ret)
}

setColor <- function(my.colors, alpha)
{
	require(grDevices)
	x <- col2rgb(my.colors, alpha = TRUE)/255
	x[4L,] <- alpha
	ret <- rgb(x[1L,],x[2L,],x[3L,],x[4L,])
	return(ret)
}

getColors <- function(x, cols, conds)
{
	return(cols[match(x, conds)])
}

getAlphas <- function(x, alphas, conds)
{
	return(alphas[match(x, conds)])
}

# This function is needed to plot within data.table because the graphics devices
# get confused while looping/grouping causing the wrong data to be plotted or co-plotted
# Copying the data eliminates this issue. HOWEVER WATCH OUT FOR SENDING data.table
# variables as arguments in '...' as this problem will again arise for that parameter
# (e.g., col=variable, the color will be wrong at times)
data.table.plot <- function(x, y, log='', logicle.params, xlim=NULL, ylim=NULL, h=NULL, h.col='red', h.lty=1, h.lwd=2, v=NULL, v.col='red', v.lty=1, v.lwd=2, ...)
{
	if(length(which(is.finite(x))) > 0)
	{
		plot.logicle(x=copy(x), y=copy(y), log=log, xlim=xlim, ylim=ylim, logicle.params=logicle.params, h=h, h.col=h.col, h.lty=h.lty, h.lwd=h.lwd, v=v, v.col=v.col, v.lty=v.lty, v.lwd=v.lwd, ...)
		print('Made a plot')
	}
}

#' This is a wrapper function to data.table.plot to allow plotting of
#' many things at once.
#' save.file should be everything but the '.pdf' extension to leave room for alterning the file name with the plot.by statement if necessary
#' by is used to determine which groups are plotted per plot while plot.by is used to split plotting into multiple plots
#'
#' @param sample.size -1 = sample all, 0 = equal sample sizes of the same size of the smallest grouping, any number > 0 defines the sampled size, e.g. 100
#' @param gates list of gate objects returned by gatePointsInPlot function
data.table.plot.all <- function(data, xcol, ycol=NULL, errcol=NULL, alphacol=NULL, alpha.rank=T, alpha=0.5, by=NULL, plot.by=NULL,
						  gates=list(),
						  log='', logicle.params=NULL, xlim=NULL, ylim=NULL, xlab=NULL, ylab=NULL, pch.alpha=1, type=c('p','l','d','h'),
						  density.args=NULL, breaks=100, percentile.limits=c(0,1),
						  h=NULL, h.col='red', h.lty=1, h.lwd=2, v=NULL, v.col='red', v.lty=1, v.lwd=2,
						  legend=T, legend.pos='topright', legend.cex=0.5,
						  save.file=NULL, save.width=5, save.height=4,
						  sample.size=-1, polygons=list(), ...)
{
	if(!(xcol %in% names(data)))
	{
		stop(paste0('The xcol provided (', xcol, ') does not exist in the data table. Aborting.'))
	}
	if(type[1] %in% c('l','p'))
	{
		if(!(ycol %in% names(data)))
		{
			stop(paste0('The ycol provided (', ycol, ') does not exist in the data table. Aborting.'))
		}
	}
	# Create a temporary gating column if necessary
	hasGatedCol <- !is.null(data[['gated']])
	if(!hasGatedCol)
	{
		data[, gated:=T]
	}

	# Determine point colors
	if(is.null(by))
	{
	     data[, my.temp.color:='black']
	}
	else
	{
     	# Store base colors
     	data[, my.temp.color:=loopingPalette(k=.GRP), by=by]
	}

	# Determine alphas
	if(!is.null(alphacol))
	{
		if(alpha.rank)
		{
			data[, alphas:=rank(get(alphacol))]
			data[, alphas:=alphas/max(alphas)]
		}
		else
		{
			xRange <- range(data[[alphacol]], na.rm=T)
			if(xRange == 0)
			{
				warning("The min and the max of the data were the same so plotting all data as the default alpha transparency.")
				data[, alphas:=alpha]
			}
			else
			{
				data[, alphas:=(get(alphacol)-xRange[1])/(xRange[2] - xRange[1])]
			}
		}
	}
	else
	{
		data[, alphas:=alpha]
	}

	# Do not apply alpha to legend.
	if(is.null(plot.by))
	{
	     legend.colors <- data[, list(grp=paste0(.BY, collapse='.'), my.color=my.temp.color[1]), by=by]
	}
	else
	{
	     legend.colors <- data[, append(mget(plot.by), list(grp=paste0(.BY, collapse='.'), my.color=my.temp.color[1])), by=by]
	}
	legend.colors[, grpI:=.GRP, by=plot.by]

	# # Now apply alpha
	data[, my.temp.color:=adjustColor(my.temp.color, alpha.factor=alphas)]

	if(is.null(xlab))
	{
		xlab <- xcol
	}
	if(is.null(ylab))
	{
		if(type[1] %in% c('l','p') & !is.null(ycol))
		{
			ylab <- ycol
		}
		else
		{
			# This should work if the 'freq' parameter in the call to hist is left null (i.e., not TRUE)
			ylab='Prob. Density'
		}
	}

	my.by <- by # Just in case the naming is wierd when using with data.table

	if(is.null(save.file))
	{
		if(!is.null(plot.by))
		{
			data[, plot.wrapper(.SD, xcol=xcol, ycol=ycol, main=paste0(paste(plot.by, collapse='.'), ' = ', paste(.BY, collapse='.')), by=my.by, errcol=errcol, log=log, logicle.params=logicle.params, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, type=type, density.args=density.args, breaks=breaks, percentile.limits=percentile.limits, legend=legend, legend.pos=legend.pos, legend.cex=legend.cex, legend.colors=legend.colors[grpI==.GRP], save.file=NULL, sample.size=sample.size, alpha.backgated=alpha, polygons=polygons, ...), by=plot.by]
		}
		else
		{
			data[, plot.wrapper(.SD, xcol=xcol, ycol=ycol, main='', by=my.by, errcol=errcol, log=log, logicle.params=logicle.params, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, type=type, density.args=density.args, breaks=breaks, percentile.limits=percentile.limits, legend=legend, legend.pos=legend.pos, legend.cex=legend.cex, legend.colors=legend.colors, save.file=NULL, sample.size=sample.size, alpha.backgated=alpha, polygons=polygons, ...)]
		}
	}
	else
	{
		if(is.null(plot.by))
		{
			data[, plot.wrapper(.SD, xcol=xcol, ycol=ycol, main='', by=my.by, errcol=errcol, log=log, logicle.params=logicle.params, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, type=type, density.args=density.args, breaks=breaks, percentile.limits=percentile.limits, legend=legend, legend.pos=legend.pos, legend.cex=legend.cex, legend.colors=legend.colors[grpI==.GRP], save.file=paste0(save.file, '.pdf'), save.width=save.width, save.height=save.height, sample.size=sample.size, alpha.backgated=alpha, polygons=polygons, ...), by=plot.by]
		}
		else
		{
			data[, plot.wrapper(.SD, xcol=xcol, ycol=ycol, main=paste0(paste(plot.by, collapse='.'), ' = ', paste(.BY, collapse='.')), by=my.by, errcol=errcol, log=log, logicle.params=logicle.params, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, type=type, density.args=density.args, breaks=breaks, percentile.limits=percentile.limits, legend=legend, legend.pos=legend.pos, legend.cex=legend.cex, legend.colors=legend.colors, save.file=paste0(save.file, paste0(.BY, collapse='.'), '.pdf'), save.width=save.width, save.height=save.height, sample.size=sample.size, alpha.backgated=alpha, polygons=polygons, ...)]
		}
	}

	data[, my.temp.color:=NULL]

	# If necessary, remove gating column
	if(!hasGatedCol)
	{
		data[, gated:=NULL]
	}
}

plot.wrapper <- function(data, xcol, ycol, errcol=NULL, by, plot.by=NULL, pch.outline=rgb(0,0,0,0), alpha.backgated=1, log='', logicle.params=NULL, type=c('l','p','h','d'), density.args=NULL, breaks=100, percentile.limits=c(0,1), h=NULL, h.col='red', h.lty=1, h.lwd=2, v=NULL, v.col='red', v.lty=1, v.lwd=2, legend=T, legend.pos='topright', legend.cex=0.5, legend.colors=NULL, save.file=NULL, save.width=5, save.height=4, sample.size=-1, polygons=polygons, xlim=NULL, ylim=NULL, ...)
{
     logicle.params <- fillDefaultLogicleParams(x=xcol, y=ycol, logicle.params=logicle.params)

	# We have to have ylim as an arg so that we can override the NULL default from data.table.plot.all instead of it being hidden in the elipses

	calcNumToSample <- function(counts, sample.size, grpNum)
	{
		return(floor(counts[grp==grpNum]$nbackgated*(sample.size/counts[grp==grpNum]$ngated)))
	}

	if(!is.null(save.file))
	{
		pdf(save.file, width=save.width, height=save.height)
	}

	if(type[1] %in% c('p','l'))
	{
		start.logicle(x=data[[xcol]], y=data[[ycol]], log=log, logicle.params=logicle.params, xlim=xlim, ylim=ylim, ...)
	}

	if(type[1] == 'l')
	{
	     if(sample.size <= 0)
	     {
	          sample.size <- .Machine$integer.max
	     }
	     nGrps <- uniqueN(data, by=by)
	     grps <- sample(1:nGrps, min(sample.size, nGrps))
		data[, if(.GRP %in% grps){data.table.lines(x=get(xcol), y=get(ycol), log=log, logicle.params=logicle.params, col=adjustColor(loopingPalette(which(grps==.GRP)), alphas[1]), ...)}, by=by]
		if(!is.null(errcol))
		{
			# (x, y, upper=NULL, lower=upper, length=0.1, draw.lower=TRUE, log='', transX=1, transY=1, tickSepX=10, tickSepY=10)
		     data[, .SD[sample(.N, min(sample.size,.N))], by=by][, data.table.error.bar(x=get(xcol), y=get(ycol), upper=get(errcol), length=0.05, draw.lower=TRUE, log=log, logicle.params=logicle.params), by=by]
		}
		finish.logicle(log=log, logicle.params=logicle.params, h=h, h.col=h.col, h.lty=h.lty, h.lwd=h.lwd, v=v, v.col=v.col, v.lty=v.lty, v.lwd=v.lwd)
		if(legend & !is.null(by))
		{
		     temp <- list(...)
		     lwd=1
		     lty=1
		     if(!is.null(temp$lwd))
		     {
		          lwd=temp$lwd
		     }
		     if(!is.null(temp$tly))
		     {
		          lty=temp$lty
		     }
		     legend.colors[, GRP:=.GRP, by=by]
		     legend.colors[, GRPI:=match(GRP, grps)]
		     legend.colors <- unique(legend.colors, by=c('grp',by))
			legend(legend.pos, legend=legend.colors[GRP %in% grps]$grp, col=loopingPalette(legend.colors[GRP %in% grps]$GRPI), lty=lty, cex=legend.cex, lwd=lwd)
		}
	}
	else if(type[1] == 'p')
	{
		# Collect points to actually plot (both gated and backgated)
		if(sample.size >= 0)
		{
			if(!is.null(by))
			{
				counts <- data[, list(n=.N, ngated=sum(gated), nbackgated=sum(!gated), grp=.GRP), by=by]
				minN <- min(couts$ngated)
				if(sample.size == 0)
				{
					sampling.gated <- data[, list(i=sample(.I[gated], minN)), by=by]$i
					sampling.backgated <- data[, list(i=sample(.I[!gated], calcNumToSample(counts=counts, sample.size=minN, grpNum=.GRP))), by=by]$i
					sampling.gated <- sample(sampling.gated) # Randomize the order of the sampling so not all groups are plotted one after another.
					sampling.backgated <- sample(sampling.backgated) # Randomize the order of the sampling so not all groups are plotted one after another.
				}
				else
				{
					if(sample.size > minN)
					{
						warning(paste0('The specified sample.size was larger than the size of the smallest population being samples. The size of the smallest population was ', minN, '. Setting the sample.size to ', minN, ' and continuing.'))
						sample.size <- minN
					}
					sampling.gated <- data[, list(i=sample(.I[gated], sample.size)), by=by]$i
					sampling.backgated <- data[, list(i=sample(.I[!gated], calcNumToSample(counts=counts, sample.size=sample.size, grpNum=.GRP))), by=by]$i
					sampling.gated <- sample(sampling.gated) # Randomize the order of the sampling so not all groups are plotted one after another.
					sampling.backgated <- sample(sampling.backgated) # Randomize the order of the sampling so not all groups are plotted one after another.}
				}
			}
			else
			{
				if(sample.size == 0)
				{
					# Sample everything because there is only one group.
					sampling.gated <- sample(which(data[['gated']]))
					sampling.backgated <- sample(which(!data[['gated']]))
				}
				else
				{
					n <- sum(data[['gated']])
					if(sample.size > n)
					{
						warning(paste0('The specified sample.size was larger than the number of gated events which is ', n, '. Continuing by using the reduced sample size.'))
						sample.size <- n
					}
					# Sample the specified amount
					sampling.gated <- sample(which(data[['gated']]), size=sample.size)
					sampling.backgated <- sample(which(!data[['gated']]), size=sample.size)
				}
			}
		}
		else
		{
			# Sample everything.
			# This randomizes the order of the points so that
			# on group isn't plotted completely over the top of
			# another group.
			sampling.gated <- sample(which(data[['gated']]))
			sampling.backgated <- sample(which(!data[['gated']]))
		}

		# Plot the backgated data if desired
		plot.logicle(x=data[[xcol]][sampling.backgated], y=data[[ycol]][sampling.backgated], type='p', log=log, logicle.params=logicle.params, add=T, col=pch.outline, bg=rgb(0,0,0,alpha.backgated), pch=21, ...)

		# Plot the gated data
		plot.logicle(x=data[[xcol]][sampling.gated], y=data[[ycol]][sampling.gated], type='p', log=log, logicle.params=logicle.params, add=T, col=pch.outline, bg=data[['my.temp.color']][sampling.gated], pch=21, ...)

		# # data[, data.table.points(x=get(xcol), y=get(ycol), log=log, xlim=xlim, xlab=xlab, ylab=ylab, transX=transX, transY=transY, tickSepX=tickSepX, tickSepY=tickSepY, col=pch.outline, bg=my.temp.color, pch=21, ...), by=by]
		# if(!is.null(errcol))
		# {
		# 	# (x, y, upper=NULL, lower=upper, length=0.1, draw.lower=TRUE, log='', transX=1, transY=1, tickSepX=10, tickSepY=10))
		# 	data[, data.table.error.bar(x=get(xcol), y=get(ycol), upper=get(errcol), length=0.05, draw.lower=TRUE, log=log, transX=transX, transY=transY, tickSepX=tickSepX, tickSepY=tickSepY), by=by]
		# }
		finish.logicle(log=log, logicle.params=logicle.params, h=h, h.col=h.col, h.lty=h.lty, h.lwd=h.lwd, v=v, v.col=v.col, v.lty=v.lty, v.lwd=v.lwd)
		if(!is.null(by))
		{
			legend(legend.pos, legend=legend.colors$grp, col=pch.outline, pt.bg=legend.colors$my.color, pch=21, cex=legend.cex)
		}
	}
	else if(type == 'h' | type == 'd')
	{
		if(is.character(log))
		{
			if(log=='')
			{
				log <- F
			}
			else
			{
				log <- T
			}
		}

		# Set the xlimits if necessary
		if(is.null(xlim))
		{
			xlim <- getPercentileLimits(data[[xcol]], percentile.limits[1], percentile.limits[2])
		}

		# Set the histogram binning (may or may not be used)
		breaks <- c(-Inf, seq(xlim[1], xlim[2], length.out=breaks+1), Inf)

		# ylim will either be null or defined here. Override if null setting to max of all plots.
		if(is.null(ylim))
		{
			suppressWarnings(
				if(type[1] == 'h')
				{
					ylims <- data[gated==T & is.finite(get(xcol)), list(grp=.GRP, maxY=max(data.table.hist(x=get(xcol), type=type[1], log=log, logicle.params=logicle.params, density.args=density.args, breaks=breaks, border=removeAlpha(my.temp.color[1]), col=my.temp.color[1], xaxt='n', add=(.GRP!=1), silent=T, ...)$y[2:(length(breaks)-2)])), by=by]
				}
				else
				{
					ylims <- data[gated==T & is.finite(get(xcol)), list(grp=.GRP, maxY=max(data.table.hist(x=get(xcol), type=type[1], log=log, logicle.params=logicle.params, density.args=density.args, breaks=breaks, border=removeAlpha(my.temp.color[1]), col=my.temp.color[1], xaxt='n', add=(.GRP!=1), silent=T, ...)$y)), by=by]
				}
			)
			# Function needs to return a single value so we arbitraritly use 'max' of the 'y'
			data[gated==T, max(data.table.hist(x=get(xcol)[is.finite(get(xcol))], type=type[1], log=log, logicle.params=logicle.params, density.args=density.args, breaks=breaks, border=removeAlpha(my.temp.color[1]), col=my.temp.color[1], xlim=xlim, ylim=c(0,max(ylims[['maxY']])), xaxt='n', add=(.GRP!=1), silent=F, ...)$y), by=by]
		}
		else
		{
			data[gated==T, data.table.hist(x=get(xcol)[is.finite(get(xcol))], type=type[1], log=log, xlim=xlim, ylim=ylim, logicle.params=logicle.params, density.args=density.args, breaks=breaks, border=removeAlpha(my.temp.color[1]), col=my.temp.color[1], xaxt='n', add=(.GRP!=1), silent=F, ...), by=by]
		}

		if(log==T)
		{
			if(is.null(logicle.params))
			{
				drawLogicleAxis(axisNum=1)
			}
			else
			{
				drawLogicleAxis(axisNum=1, transition=logicle.params$transX, tickSep=logicle.params$tickSepX, base=logicle.params$base)
			}
		}
		else
		{
			axis(1)
		}
		if(!is.null(by))
		{
			legend(legend.pos, legend=legend.colors$grp, col=pch.outline, pt.bg=legend.colors$my.color, pch=21, cex=legend.cex)
		}
	}

	if(length(polygons) > 0)
	{
		for(polygon in polygons)
		{
			if(!is.null(logicle.params))
			{
				if(grepl('x',log,fixed=T))
				{
					polygon$x <- logicle(polygon$x, transition=logicle.params$transX, tickSep=logicle.params$tickSepX, base=logicle.params$base, neg.rmF)
				}
				if(grepl('y',log,fixed=T))
				{
					polygon$y <- logicle(polygon$y, transition=logicle.params$transY, tickSep=logicle.params$tickSepY, base=logicle.params$base, neg.rmF)
				}
			}
			plot(polygon, lwd=2, border='red', add=T)
		}
	}

	if(!is.null(save.file))
	{
		dev.off()
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
data.table.lines <- function(x, y, log='', logicle.params, h=NULL, h.col='red', h.lty=1, h.lwd=2, v=NULL, v.col='red', v.lty=1, v.lwd=2, ...)
{
	if(length(which(is.finite(x))) > 0)
	{
		l(x1, y1) %=% get.logicle(x=copy(x), y=copy(y), log=log, logicle.params=logicle.params)
		lines(x=x1, y=y1, ...)
		print('Made a plot')
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
		ret <- plot.hist(x=copy(x), ...)
		print('Made a histogram')
		return(ret)
	}
}


# This function is needed to plot within data.table because the graphics devices
# get confused while looping/grouping causing the wrong data to be plotted or co-plotted
# Copying the data eliminates this issue. HOWEVER WATCH OUT FOR SENDING data.table
# variables as arguments in '...' as this problem will again arise for that parameter
# (e.g., col=variable, the color will be wrong at times)
data.table.points <- function(x, y, log='', plot.logicle=F, logicle.params, h=NULL, h.col='red', h.lty=1, h.lwd=2, v=NULL, v.col='red', v.lty=1, v.lwd=2, ...)
{
	if(length(which(is.finite(x))) > 0)
	{
		if(plot.logicle)
		{
			plot.logicle(x=copy(x), y=copy(y), log=log, logicle.params=logicle.params, add=T, h=NULL, h.col='red', h.lty=1, h.lwd=2, v=NULL, v.col='red', v.lty=1, v.lwd=2, ...)
		}
		else
		{
			points(x=copy(x), y=copy(y))
		}
		print('Added points to a plot')
	}
}

# This function is needed to plot within data.table because the graphics devices
# get confused while looping/grouping causing the wrong data to be plotted or co-plotted
# Copying the data eliminates this issue. HOWEVER WATCH OUT FOR SENDING data.table
# variables as arguments in '...' as this problem will again arise for that parameter
# (e.g., col=variable, the color will be wrong at times)
data.table.error.bar <- function(x, y, upper=NULL, lower=upper, length=0.1, draw.lower=TRUE, log='', plot.logicle=F, logicle.params, ...)
{
	if(length(which(is.finite(x))) > 0)
	{
		l(x1, y1) %=% get.logicle(x=copy(x), y=copy(y), log=log, logicle.params=logicle.params)
		if(!is.null(upper))
		{
			l(xUpper, yUpper) %=% get.logicle(x=copy(x), y=copy(y+upper), log=log, logicle.params=logicle.params)
			upper1 <- yUpper-y1
		}
		else
		{
			upper1 <- NULL
		}
		if(!is.null(lower))
		{
			l(xLower, yLower) %=% get.logicle(x=copy(x), y=copy(y-lower), log=log, logicle.params=logicle.params)
			lower1 <- y1-yLower
		}
		else
		{
			lower1 <- NULL
		}

		error.bar(x=x1, y=y1, upper=upper1, lower=lower1, length=length, draw.lower=draw.lower, ...)
		print('Added error bars to a plot')
	}
}

# The optional args '...' are passed to corr.test in the psych package
pairwise.cor.test.internal <- function(x, id.cols=c(), ...)
{
	require('psych')
	measurement.cols <- getAllColNamesExcept(x, id.cols)
	temp <- x[, ..measurement.cols]
	return(corr.test(temp))
}

getSortedCorrelations <- function(cor.result)
{
	cor.result[upper.tri(cor.result)] <- NA
	ret <- cbind(which(!is.na(cor.result),arr.ind = TRUE),na.omit(as.vector(cor.result)))
	ret <- data.table(ret)
	ret <- ret[row != col]
	ret[, ':='(row=row.names(cor.result)[row], col=colnames(cor.result)[col])]
	setnames(ret, names(ret), c('M1','M2','cor'))
	ret[, toSort := abs(cor)]
	setorder(ret, -toSort)
	ret[, toSort:=NULL][]
	return(ret)
}

pairwise.cor.test <- function(x, by, id.cols=NULL, measurement.cols=NULL, ...)
{
	require('psych')
	if(!is.null(id.cols))
	{
		measurement.cols <- getAllColNamesExcept(x, c(id.cols, by))
	}
	if(is.null(measurement.cols))
	{
		measurement.cols <- getAllColNamesExcept(x, c(by))
	}
	ret <- x[, getSortedCorrelations(corr.test(.SD[, ..measurement.cols], ...)$r), by=by]
	return(ret)
}

#' This function is borrowed from http://www.dr-spiess.de/scripts/bigcor.R (by A.N. Spiess)
#'
#' Use convert to convert the output from a hard disk matrix to a RAM matrix
bigcor <- function(x, y=NULL, fun=c("cor","cov"), size=2000, verbose=TRUE, convert=T, ...)
{
	require('ff')
	fun <- match.arg(fun)
	if (fun == "cor") FUN <- cor else FUN <- cov
	if (fun == "cor") STR <- "Correlation" else STR <- "Covariance"
	if (!is.null(y) & NROW(x) != NROW(y)) stop("'x' and 'y' must have compatible dimensions!")

	NCOL <- ncol(x)
	if (!is.null(y)) YCOL <- NCOL(y)

	## calculate remainder, largest 'size'-divisible integer and block size
	size <- min(size, NCOL)
	REST <- NCOL %% size
	LARGE <- NCOL - REST
	NBLOCKS <- NCOL %/% size

	## preallocate square matrix of dimension
	## ncol(x) in 'ff' single format
	if (is.null(y)) resMAT <- ff(vmode = "double", dim = c(NCOL, NCOL))
	else resMAT <- ff(vmode = "double", dim = c(NCOL, YCOL))

	## split column numbers into 'nblocks' groups + remaining block
	GROUP <- rep(1:NBLOCKS, each = size)
	if (REST > 0) GROUP <- c(GROUP, rep(NBLOCKS + 1, REST))
	SPLIT <- split(1:NCOL, GROUP)

	## create all unique combinations of blocks
	COMBS <- expand.grid(1:length(SPLIT), 1:length(SPLIT))
	COMBS <- t(apply(COMBS, 1, sort))
	COMBS <- unique(COMBS)
	if (!is.null(y)) COMBS <- cbind(1:length(SPLIT), rep(1, length(SPLIT)))

	## initiate time counter
	timeINIT <- proc.time()

	## iterate through each block combination, calculate correlation matrix
	## between blocks and store them in the preallocated matrix on both
	## symmetric sides of the diagonal
	for (i in 1:nrow(COMBS)) {
		COMB <- COMBS[i, ]
		G1 <- SPLIT[[COMB[1]]]
		G2 <- SPLIT[[COMB[2]]]

		## if y = NULL
		if (is.null(y)) {
			if (verbose) cat(sprintf("#%d: %s of Block %s and Block %s (%s x %s) ... ", i, STR,  COMB[1],
								COMB[2], length(G1),  length(G2)))
			RES <- FUN(x[, G1], x[, G2], ...)
			resMAT[G1, G2] <- RES
			resMAT[G2, G1] <- t(RES)
		} else ## if y = smaller matrix or vector
		{
			if (verbose) cat(sprintf("#%d: %s of Block %s and 'y' (%s x %s) ... ", i, STR,  COMB[1],
								length(G1),  YCOL))
			RES <- FUN(x[, G1], y, ...)
			resMAT[G1, ] <- RES
		}

		if (verbose) {
			timeNOW <- proc.time() - timeINIT
			cat(timeNOW[3], "s\n")
		}

		gc()
	}

	if(NCOL > 1 & !is.null(colnames(x)))
	{
		colnames(resMAT) <- colnames(x)
		rownames(resMAT) <- colnames(x)
	}

	if(convert)
	{
		return(as.matrix(resMAT[,]))
	}
	else
	{
		return(resMAT)
	}
}

# This is for calculating the p-value by combining multiple experiments
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
#' NOTE: If you want supply an alternative aggregation function, due to intracacies of the dcast
#' implementation, you need to define a function and call it by the name 'agg'. This
#' function tests to see if it is supplied and calls it as fun.aggregate argument.
#' Otherwise, it just calls the function which uses the default fun.aggregate=length.
#' If you use agg, you can use rm(agg) to reset it to the default if desired.
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
	if(exists('agg'))
	{
	     data <- as.data.table(dcast(data, formula, value.var = valueCols, fun=agg, ...))
	}
	else
	{
	     args <- list(...)
	     if(!is.null(args$fun))
	     {
	          args$fun <- NULL
	          warning("An aggregation function can't be supplied to this function in the normal way
                         Instead, you must define a function called 'agg' in the parent environment
	                    then call this function. The function will find that 'agg' exists and supply
	                    it as the fun.aggregate argument. This is a workaround for an intracacy of
	                    the dcast function. Using default fun.aggregate=length instead.")
	          args <- c(list(data=data, formula=formula, value.var=valueCols), args)
	          data <- as.data.table(do.call(dcast, args))
	     }
	     else
	     {
	          data <- as.data.table(dcast(data, formula, value.var = valueCols, ...))
	     }
	}

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

##### Logicle Plotting #####
calcTransition <- function(x, minN=20)
{
     negNums <- c(x[x<0],-1*x[x<=0])
     if(length(negNums[is.finite(negNums)]) < minN)
     {
          # Then insufficient n to guess the 'zero' distribution
          # This will suggest using normal log scaling instead of logicle
          return(NULL)
     }
     else
     {
          return(3*mad(negNums, na.rm=T))
     }
}

get.logicle <- function(x, y, log, logicle.params, neg.rm=T)
{
     logicle.params <- fillDefaultLogicleParams(x=x, y=y, logicle.params=logicle.params)
	logX <- grepl('x',x=log,fixed=T)
	logY <- grepl('y',x=log,fixed=T)

	if(!is.null(logicle.params))
	{
		if(logX)
		{
			x1 <- logicle(x, transition=logicle.params$transX, tickSep=logicle.params$tickSepX, base=logicle.params$base, neg.rm=neg.rm)
		}
		else
		{
			x1 <- x
		}

		if(logY)
		{
			y1 <- logicle(y, transition=logicle.params$transY, tickSep=logicle.params$tickSepY, base=logicle.params$base, neg.rm=neg.rm)
		}
		else
		{
			y1 <- y
		}
	}
	else
	{
		# Then, if necessary, remove negative x and y values and warn the user
		if(logX & neg.rm)
		{
			posX <- x > 0
		}
		else
		{
			# Treat all values as 'positive' (i.e., plottable)
			posX <- rep(T, length(x))
		}

		if(logY & neg.rm)
		{
			posY <- y > 0
		}
		else
		{
			# Treat all values as 'positive' (i.e., plottable)
			posY <- rep(T, length(y))
		}

		plottable <- posX & posY
		x1 <- x[plottable]
		y1 <- y[plottable]
		if(sum(posX) < length(x))
		{
			warning('Negative values were removed from the X values (along with corresponding Y-values) due to log scaling.')
		}
		if(sum(posY) < length(y))
		{
			warning('Negative values were removed from the Y values (along with corresponding X-values) due to log scaling.')
		}
		
		if(logX)
		{
		     x1 <- log10(x1)
		}
		if(logY)
		{
		     y1 <- log10(y1)
		}
	}

	return(list(x=x1, y=y1))
}

start.logicle <- function(x, y, log='xy', logicle.params, ...)
{
     logicle.params <- fillDefaultLogicleParams(x=x, y=y, logicle.params=logicle.params)

	logX <- grepl('x',x=log,fixed=T)
	logY <- grepl('y',x=log,fixed=T)

	# This function works to scale things to logicle scale or for normal log scaling (removing negative values if necessary but leaving things unscaled)
	l(x1, y1) %=% get.logicle(x=x, y=y, log=log, logicle.params=logicle.params)

	# Determine xlim
	xlim <- list(...)$xlim
	if(is.null(xlim))
	{
		# Then xlim is not provided and we should calculate based on the data from 'get.logicle'
		xlim <- range(x1[is.finite(x1)], na.rm=T)
	}
	else
	{
		# Then it is provided in '...', but if if logicle.params is provided and the axis is to be scaled, we might need to scale the limits to the logicle scaling
		if(!is.null(logicle.params) & logX)
		{
			if(logX)
			{
				# Then scale the limits
				xlim <- range(logicle(x=xlim, transition=logicle.params$transX, tickSep=logicle.params$tickSepX, base=logicle.params$base, neg.rmF))
			}
			# Otherwise, don't do anything
		}

		# However, if it regular log scaling, the user might have provided a lower limit <=0 which could cause issues.
		# Check and change if necessary, printing a warning.
		if(is.null(logicle.params) & logX & min(xlim) <= 0)
		{
			xlim[1] <- min(x1) # Which is guaranteed to no not be zero based on 'get.logicle'
			warning('A 0 or negative limit was provided to the log-scaled x axis. Setting to min of the positive x values.')
		}
	}


	# Determine ylim
	ylim <- list(...)$ylim
	if(is.null(ylim))
	{
		# Then ylim is not provided and we should calculate based on the data from 'get.logicle'
		ylim <- range(y1[is.finite(y1)], na.rm=T)
	}
	else
	{
		# Then ylim is provided in '...', but if logicle.params is provided and the axis is to be scaled, we might need to scale the limits to the logicle scaling
		if(!is.null(logicle.params) & logY)
		{
			if(logY)
			{
				# Then scale the limits
				ylim <- range(logicle(x=ylim, transition=logicle.params$transY, tickSep=logicle.params$tickSepY, base=logicle.params$base, neg.rmF))
			}
			# Otherwise, don't do anything
		}

		# However, if it regular log scaling, the user might have provided a lower limit <=0 which could cause issues.
		# Check and change if necessary, printing a warning.
		if(is.null(logicle.params) & logY & min(ylim) <= 0)
		{
			ylim[1] <- min(y1) # Which is guaranteed to no not be zero based on 'get.logicle'
			warning('A 0 or negative limit was provided to the log-scaled y axis. Setting to min of the positive y values.')
		}
	}

	pars.plot <- list(...)
	pars.plot <- merge.lists(pars.plot, list(xlim=xlim, ylim=ylim, x=numeric(0), y=numeric(0), axes=F))
	do.call(plot, pars.plot)
	box(col='black',lwd=2)

	return(list(x=x1, y=y1))
}

finish.logicle <- function(log, logicle.params, h, h.col, h.lty, h.lwd, v, v.col, v.lty, v.lwd)
{
     logicle.params <- fillDefaultLogicleParams(x=x, y=y, logicle.params=logicle.params)
	# Determine which axes to transform
	logX <- grepl('x',x=log,fixed=T)
	logY <- grepl('y',x=log,fixed=T)

	# Draw axes if logicle.params was provided and the particular axis is logicle-scaled
	if(logX == 1)
	{
		drawLogicleAxis(axisNum=1, transition=logicle.params$transX, tickSep=logicle.params$tickSepX, base=logicle.params$base)
	}
	else
	{
		axis(1)
	}
	if(logY == 1)
	{
		drawLogicleAxis(axisNum=2, transition=logicle.params$transY, tickSep=logicle.params$tickSepY, base=logicle.params$base)
	}
	else
	{
		axis(2)
	}

	# This is now done within drawLogiclAxis
	# # Plot transition lines if necessary (only applies to logicle-scaled plots)
	# if(!is.null(logicle.params))
	# {
	# 	abline(v=logicle.params$transX, lty=2, col='gray', lwd=1)
	# 	abline(h=logicle.params$transX, lty=2, col='gray', lwd=1)
	# }

	# Plot h and v lines
	if(!is.null(h))
	{
		if(logY & !is.null(logicle.params))
		{
			abline(h=logicle(h, transition=logicle.params$transY, tickSep=logicle.params$tickSepY, base=logicle.params$base, neg.rmF), col=h.col, lty=h.lty, lwd=h.lwd)
		}
		else
		{
			abline(h=h, col=h.col, lty=h.lty, lwd=h.lwd)
		}
	}
	if(!is.null(v))
	{
		if(logX & !is.null(logicle.params))
		{
			abline(v=logicle(v, transition=logicle.params$transX, tickSep=logicle.params$tickSepX, base=logicle.params$base, neg.rmF), col=v.col, lty=v.lty, lwd=v.lwd)
		}
		else
		{
			abline(v=v, col=v.col, lty=v.lty, lwd=v.lwd)
		}
	}
}

getPrettyNum <- function(x, sigFigs=3)
{
     return(formatC(signif(x,digits=sigFigs), digits=sigFigs,format="fg", flag="#", drop0trailing = T))
}

fillDefaultLogicleParams <- function(x, y, logicle.params)
{
     if(is.null(logicle.params))
     {
          return(NULL)
     }
     if(is.null(y))
     {
          # Then just get params for x
          transition <- logicle.params$transition
          if(is.null(transition))
          {
               logicle.params$transition <- calcTransition(x)
          }
     }
     else
     {
          # Then get params for x
          transition <- logicle.params$transX
          if(is.null(transition))
          {
               logicle.params$transX <- calcTransition(x)
          }
          # Then get params for x
          transition <- logicle.params$transY
          if(is.null(transition))
          {
               logicle.params$transY <- calcTransition(y)
          }
     }
     return(logicle.params)
}

plot.logicle <- function(x, y, type='p', log='', logicle.params=NULL, h=NULL, h.col='red', h.lty=1, h.lwd=2, v=NULL, v.col='red', v.lty=1, v.lwd=2, add=F, randomize=T, ...)
{
     logicle.params <- fillDefaultLogicleParams(x=x, y=y, logicle.params=logicle.params)
	if(!add)
	{
		l(x1, y1) %=% start.logicle(x=x, y=y, log=log, logicle.params=logicle.params, ...)
	}
	else
	{
		if(grepl('x',log,fixed=T) & !is.null(logicle.params))
		{
			x1 <- logicle(x=x, transition=logicle.params$transX, tickSep=logicle.params$tickSepX, base=logicle.params$base, neg.rmF)
		}
		else
		{
			x1 <- x
		}
		if(grepl('y',log,fixed=T) & !is.null(logicle.params))
		{
			y1 <- logicle(x=y, transition=logicle.params$transY, tickSep=logicle.params$tickSepY, base=logicle.params$base, neg.rmF)
		}
		else
		{
			y1 <- y
		}
	}
	if(type == 'p')
	{
		points(x1, y1, ...)
	}
	else
	{
		lines(x1, y1, ...)
	}
	if(!add)
	{
		finish.logicle(log=log, logicle.params=logicle.params, h=h, h.col=h.col, h.lty=h.lty, h.lwd=h.lwd, v=v, v.col=v.col, v.lty=v.lty, v.lwd=v.lwd)
	}
	return(list(x=x1, y=y1))
}

unlogicle <- function(x, transition=NULL, tickSep=NULL, base=NULL)
{
	if(is.null(base))
	{
		base <- 10
	}
	valsToAdjust <- (x > transition) & !is.na(x)
	if(is.null(transition) & is.null(tickSep))
	{
		return(base^x)
	}
	else
	{
		if(transition <= 0)
		{
			warning('Transition must be greater than 0. Setting to 1.')
			transition <- 1
		}
		ordersDifferenceOnDisplay = (x[valsToAdjust] - transition) / tickSep
		x[valsToAdjust] <- transition*base^(ordersDifferenceOnDisplay)
		return(x)
	}
}

unlogicle <- function(x, transition=NULL, base=NULL, tickSep=NULL)
{
     if(is.null(base))
     {
          base <- 10
     }
     if(is.null(tickSep))
     {
          tickSep <- transition*log(base)
     }
     valsAbove <- (x > transition/tickSep) & !is.na(x)
     valsBelow <- (x <= transition/tickSep) & !is.na(x)
     if(is.null(transition))
     {
          return(base^x)
     }
     else
     {
          if(transition <= 0)
          {
               warning('Transition must be greater than 0. Setting to 1.')
               transition <- 1
          }
          x[valsAbove] <- base^{x[valsAbove] - (transition/tickSep - log(transition, base=base))}
          x[valsBelow] <- x[valsBelow]*tickSep
          return(x)
     }
}

# logicle <- function(x, transition=NULL, tickSep=NULL, base=NULL)
# {
# 	if(is.null(base))
# 	{
# 		base <- 10
# 	}
# 	if(is.null(transition) & is.null(tickSep))
# 	{
# 		ret <- log(x[x > 0], base=base)
# 		if(length(ret) < length(x))
# 		{
# 			warning('Values below zero were removed for performing log transformation.')
# 		}
# 		return(ret)
# 	}
# 	if(transition <= 0)
# 	{
# 		warning('Transition must be greater than 0. Setting to 1.')
# 		transition <- 1
# 	}
# 	# Just do it for the right indicies
# 	valsToAdjust <- (x > transition) & !is.na(x)
# 	x[valsToAdjust] <- transition + log(x[valsToAdjust]/transition, base=base)*tickSep
# 	return(x)
# }

logicle <- function(x, transition=NULL, base=NULL, tickSep=NULL, logicle.params=NULL, neg.rm=T)
{
     logicle.params <- fillDefaultLogicleParams(x=x, y=NULL, logicle.params=logicle.params)
     if(!is.null(logicle.params))
     {
          if(!is.null(logicle.params$transition))
          {
               transition <- logicle.params$transition
          }
          if(!is.null(logicle.params$base))
          {
               base <- logicle.paramsbase
          }
          if(!is.null(logicle.params$tickSep))
          {
               tickSep <- logicle.params$tickSep
          }
     }
     if(is.null(base))
     {
          base <- 10
     }
     if(is.null(tickSep))
     {
          tickSep <- transition*log(base)
     }
     if(is.null(transition))
     {
          if(neg.rm)
          {
               ret <- log(x[x > 0], base=base) # Note that here, NA's will evaluate to true and be passed along (this would not be case if testing for x < 0)
          }
          else
          {
               ret <- rep(NA, length(x))
               ret[!is.na(x) & x > 0] <- log(x[!is.na(x) & x > 0], base=base)
          }
          if(length(ret) < length(x))
          {
               warning('Values below zero were removed for performing log transformation.')
          }
          return(ret)
     }
     if(transition <= 0)
     {
          warning('Transition must be greater than 0. Setting to 1.')
          transition <- 1
     }
     # Just do it for the right indicies
     valsAbove <- (x > transition) & !is.na(x)
     valsBelow <- (x <= transition) & !is.na(x)
     x[valsAbove] <- log(x[valsAbove], base=base) + (transition/tickSep - log(transition, base=base))
     x[valsBelow] <- x[valsBelow]/tickSep
     return(x)
}

drawLogicleAxis <- function(axisNum=1, transition=NULL, tickSep=NULL, base=NULL, n.minor.ticks=9, las=0)
{
     if(is.null(base))
     {
          base <- 10
     }
     if(is.null(tickSep))
     {
          tickSep <- transition*log(base)
     }
     if(!is.null(transition))
     {
          if(transition <= 0)
          {
               warning('Transition must be greater than 0. Setting to 1.')
               transition <- 1
          }
     }

     # Get the numbers below the transition
     if(axisNum==1)
     {
          axis.limits <- par('usr')[1:2]
     }
     else
     {
          axis.limits <- par('usr')[3:4]
     }
     linLimits <- unlogicle(c(axis.limits[1],transition/tickSep), transition=transition, tickSep=tickSep, base=base)
     linSidePrettyNums <- pretty(linLimits)
     linSidePrettyNums <- linSidePrettyNums[linSidePrettyNums > min(linLimits) & linSidePrettyNums <= max(linLimits) ]

     # These are the true values of the log scale ticks proposed above
     # Thus they exist for x > 0
     logSideNums <- base^c(-100:100)
     tick.distances <- (logSideNums[2:length(logSideNums)] - logSideNums[1:(length(logSideNums)-1)])/(n.minor.ticks + 1)
     minor.ticks <- c()
     for(i in 1:(length(logSideNums)-1))
     {
          temp <- (1:n.minor.ticks)*tick.distances[i] + logSideNums[i]
          minor.ticks <- c(minor.ticks, temp)
     }

     # # logSideNumTicks <- base^c((-100:100)/)
     # n <- n.minor.ticks+2
     # minors <- log(pretty(10^logSideNums[1:2],n), base=base)-logSideNums[1]
     # minors <- minors[-c(1,n)]
     # minor.ticks = c(outer(minors,logSideNums,`+`))
     # minor.ticks <- minor.ticks[minor.ticks > min(axis.limits) & minor.ticks < max(axis.limits)]

     if(is.null(transition))
     {
          # Then treat as a regular log-scale where there are no negative values and
          # everything should have log-scale ticks.
          if(base == exp(1))
          {
               logSidePrettyLabels <- parse(text=paste("e^", log(logSideNums, base=base), sep=""))
          }
          else
          {
               logSidePrettyLabels <- parse(text=paste(base, "^", log(logSideNums, base=base), sep=""))
          }
          prettyLabels <- logSidePrettyLabels
          prettyNums <- logSideNums
     }
     else
     {
          # Some tick labels should be on linear scale and others on log-scale.
          if(base == exp(1))
          {
               logSidePrettyLabels <- parse(text=paste("e^", log(logSideNums, base=base), sep=""))
          }
          else
          {
               logSidePrettyLabels <- parse(text=paste(base, "^", log(logSideNums, base=base), sep=""))
          }

          # Truncate list of log side nums
          logSidePrettyLables <- logSidePrettyLabels[logSideNums > transition]
          logSideNums <- logSideNums[logSideNums > transition]
          minor.ticks <- minor.ticks[minor.ticks > transition]

          # Truncate and fix list of lin side nums
          linSidePrettyNums <- c(linSidePrettyNums, transition) # Add the transition point to the list
          linSidePrettyNums <- linSidePrettyNums[order(linSidePrettyNums)]
          linSidePrettyNums <- linSidePrettyNums[linSidePrettyNums <= transition]
          linSidePrettyLabels <- c(as.character(linSidePrettyNums))
          linSidePrettyLabels[length(linSidePrettyLabels)] <- as.character(getPrettyNum(linSidePrettyNums[length(linSidePrettyNums)]))

          # Aggregate list of nums
          prettyNums <- c(linSidePrettyNums, logSideNums)
          prettyLabels <- c(linSidePrettyLabels, logSidePrettyLables)

          # Add the transition point so it is clear where this is
          if(axisNum == 1)
          {
               abline(v=transition/tickSep, lty=2, col=rgb(0,0,0,0.6))
          }
          if(axisNum == 2)
          {
               abline(h=transition/tickSep, lty=2, col=rgb(0,0,0,0.6))
          }
     }

     ticks <- logicle(x=prettyNums, transition=transition, tickSep=tickSep, base=base, neg.rm=F)
     minor.ticks <- logicle(x=minor.ticks, transition=transition, tickSep=tickSep, base=base, neg.rm=F)

     if(axisNum == 2)
     {
          axis(axisNum, at=ticks, labels=prettyLabels, las=2)
          axis(axisNum, at=minor.ticks, tcl=par("tcl")*0.5, labels=FALSE)
     }
     else
     {
          axis(axisNum, at=ticks, labels=prettyLabels, las=las)
          axis(axisNum, at=minor.ticks, tcl=par("tcl")*0.5, labels=FALSE, las=las)
     }
}

# drawLogicleAxis <- function(axisNum=1, transition=NULL, tickSep=NULL, base=NULL, n.minor.ticks=9)
# {
# 	if(axisNum==1)
# 	{
# 		axis.limits <- pretty(par('usr')[1:2])
# 	}
# 	else
# 	{
# 		axis.limits <- pretty(par('usr')[3:4])
# 	}
# 	linSidePrettyNums <- unlogicle(axis.limits, transition=transition, tickSep=tickSep, base=base, neg.rm=F)
# 	linSidePrettyNums <- linSidePrettyNums[linSidePrettyNums >= min(axis.limits) & linSidePrettyNums <= max(axis.limits) ]
#
# 	if(is.null(base))
# 	{
# 		base <- 10
# 	}
#
# 	# These are the true values of the log scale ticks proposed above
# 	# Thus they exist for x > 0
# 	logSideNums <- base^c(-100:100)
# 	tick.distances <- (logSideNums[2:length(logSideNums)] - logSideNums[1:(length(logSideNums)-1)])/(n.minor.ticks + 1)
# 	minor.ticks <- c()
# 	for(i in 1:(length(logSideNums)-1))
# 	{
# 		temp <- (1:n.minor.ticks)*tick.distances[i] + logSideNums[i]
# 		minor.ticks <- c(minor.ticks, temp)
# 	}
#
# 	# # logSideNumTicks <- base^c((-100:100)/)
# 	# n <- n.minor.ticks+2
# 	# minors <- log(pretty(10^logSideNums[1:2],n), base=base)-logSideNums[1]
# 	# minors <- minors[-c(1,n)]
# 	# minor.ticks = c(outer(minors,logSideNums,`+`))
# 	# minor.ticks <- minor.ticks[minor.ticks > min(axis.limits) & minor.ticks < max(axis.limits)]
#
# 	if(is.null(transition))
# 	{
# 		# Then treat as a regular log-scale where there are no negative values and
# 		# everything should have log-scale ticks.
# 		if(base == exp(1))
# 		{
# 			logSidePrettyLabels <- parse(text=paste("e^", log(logSideNums, base=base), sep=""))
# 		}
# 		else
# 		{
# 			logSidePrettyLabels <- parse(text=paste(base, "^", log(logSideNums, base=base), sep=""))
# 		}
# 		prettyLabels <- logSidePrettyLabels
# 		prettyNums <- logSideNums
# 	}
# 	else
# 	{
# 		if(transition <= 0)
# 		{
# 			warning('Transition must be greater than 0. Setting to 1.')
# 			transition <- 1
# 		}
#
# 		# Some tick labels should be on linear scale and others on log-scale.
# 		if(base == exp(1))
# 		{
# 			logSidePrettyLabels <- parse(text=paste("e^", log(logSideNums, base=base), sep=""))
# 		}
# 		else
# 		{
# 			logSidePrettyLabels <- parse(text=paste(base, "^", log(logSideNums, base=base), sep=""))
# 		}
#
# 		# Truncate list of log side nums
# 		logSidePrettyLables <- logSidePrettyLabels[logSideNums > transition]
# 		logSideNums <- logSideNums[logSideNums > transition]
# 		minor.ticks <- minor.ticks[minor.ticks > transition]
#
# 		# Truncate and fix list of lin side nums
# 		linSidePrettyNums <- c(linSidePrettyNums, transition) # Add the transition point to the list
# 		linSidePrettyNums <- linSidePrettyNums[order(linSidePrettyNums)]
# 		linSidePrettyNums <- linSidePrettyNums[linSidePrettyNums <= transition]
# 		linSidePrettyLabels <- as.character(linSidePrettyNums)
#
# 		# Aggregate list of nums
# 		prettyNums <- c(linSidePrettyNums, logSideNums)
# 		prettyLabels <- c(linSidePrettyLabels, logSidePrettyLables)
#
# 		# Add the transition point so it is clear where this is
# 		if(axisNum == 1)
# 		{
# 			abline(v=transition, lty=2, col=rgb(0,0,0,0.6))
# 		}
# 		if(axisNum == 2)
# 		{
# 			abline(h=transition, lty=2, col=rgb(0,0,0,0.6))
# 		}
# 	}
#
# 	ticks <- logicle(x=prettyNums, transition=transition, tickSep=tickSep, base=base, neg.rm=F)
# 	minor.ticks <- logicle(x=minor.ticks, transition=transition, tickSep=tickSep, base=base, neg.rm=F)
#
# 	if(axisNum == 2)
# 	{
# 		axis(axisNum, at=ticks, labels=prettyLabels, las=2)
# 		axis(axisNum, at=minor.ticks, tcl=par("tcl")*0.5, labels=FALSE)
# 	}
# 	else
# 	{
# 		axis(axisNum, at=ticks, labels=prettyLabels)
# 		axis(axisNum, at=minor.ticks, tcl=par("tcl")*0.5, labels=FALSE)
# 	}
# }

getLogParam <- function(logX, logY)
{
	if(logX == 1 & logY == 1)
	{
		'xy'
	}
	else if(logX == 1)
	{
		'x'
	}
	else if(logY == 1)
	{
		'y'
	}
	else
	{
		''
	}
}

##### Histogram Plotting #####

#' For plotting histograms and density plots
#'
#' Note that you can add params such as mgp (default c(3,1,0)) to move axis labels out (increase 3)
#' Note that you can rotate labels 90
#' Note, you can plot just the center 'x' percentile of data (e.g., the middle 90 percent setting the limits to the top and bottom 5 percent)
plot.hist <- function(x, type=c('d','h'), log=F, neg.rm=T, logicle.params=NULL, density.args=NULL, breaks=100, add=F, border='black', col='gray', mar=c(5.1,5.1,4.1,2.1), mgp=c(4,1,0), las=c(0,2), silent=F, ...)
{
     logicle.params <- fillDefaultLogicleParams(x=x, y=NULL, logicle.params=logicle.params)
	default.mar <- par('mar')
	default.mgp <- par('mgp')
	default.las <- par('las')
	par(mar=mar, mgp=mgp, las=las[1])
	plot.params <- list(...)
	# Adjust the data to log/logicle scale if needed FIRST
	if(log)
	{
	     x <- logicle(x, logicle.params=logicle.params, neg.rm=neg.rm)
		if(!is.null(logicle.params) && !is.null(plot.params$xlim))
		{
		     # Then we should also control the limits of the plot since we'll be drawing a logicle axis
		     plot.params$xlim <- logicle(plot.params$xlim, logicle.params=logicle.params, neg.rmF)
		}
	}

	# Then plot the histogram or densitygram
	if(type[1]=='h')
	{
		if(is.null(plot.params))
		{
			ret <- hist(x, add=add, xaxt='n', col=col, plot=!silent, breaks=breaks, ...)
		}
		else
		{
			plot.params <- merge.lists(plot.params, list(x=x, add=add, xaxt='n', col=col, plot=!silent, breaks=breaks))
			ret <- do.call(hist, plot.params)
		}
		ret$x <- ret$mids
		ret$y <- ret$density
		print(ret$y)
	}
	else
	{
		if(is.null(density.args))
		{
			density.args <- list()
		}
		density.args$x <- x
		ret <- do.call(density, density.args)
		if(!silent)
		{
			if(add)
			{
				# Add zero levels before and after sequence of numbers
				polygon(ret, col=col, border=border)
			}
			else
			{
				if(is.null(plot.params))
				{
					plot(ret$x, ret$y, col=rgb(0,0,0,0), xaxt='n', ...)
					# Add zero levels before and after sequence of numbers
					polygon(ret, col=col, border=border)
				}
				else
				{
					plot.params <- merge.lists(plot.params, list(x=ret$x, y=ret$y, col=rgb(0,0,0,0), xaxt='n'))
					do.call(plot, plot.params)
					# Add zero levels before and after sequence of numbers
					polygon(ret, col=col, border=border)
				}
			}
		}
	}

	if(!add & !silent)
	{
		if(log)
		{
			if(is.null(logicle.params))
			{
				drawLogicleAxis(axisNum=1, las=las[1])
			}
			else
			{
				drawLogicleAxis(axisNum=1, transition=logicle.params$transition, tickSep=logicle.params$tickSep, base=logicle.params$base, las=las[1])
			}
		}
		else
		{
			axis(1)
		}
	}
	par(mar=default.mar, mgp=default.mgp, las=default.las)
	return(ret)
}

getPercentileLimits <- function(x, lower, upper)
{
	temp <- as.numeric(quantile(x, c(lower, upper), na.rm=T))
	return(c(temp[1], temp[2]))
}

##### Gating #####

autoGate <- function(x, y, border='red', lwd=2, log='', logicle.params=NULL, method=c('mean','median'), n.sig=2, ...)
{
	l(x1, y1) %=% plot.logicle(x, y, log=log, logicle.params=logicle.params, ...)
	if(method[1]=='mean')
	{
		x.mu <- mean(x1, na.rm=T)
		x.sd <- sd(x1, na.rm=T)
		y.mu <- mean(y1, na.rm=T)
		y.sd <- sd(y1, na.rm=T)
	}
	else
	{
		x.mu <- median(x1, na.rm=T)
		x.sd <- mad(x1, na.rm=T)
		y.mu <- median(y1, na.rm=T)
		y.sd <- mad(y1, na.rm=T)
	}

	a <- n.sig*x.sd
	b <- n.sig*y.sd

	poly <- plot.ellipse(x.mu, y.mu, a, b, border=border, lwd=lwd)

	isin <- inside.owin(x=x1, y=y1, w=poly)

	return(list(poly=poly, isin=isin, gated=which(isin), backgated=which(!isin)))
}

gatePointsInPlot <- function(x, y, border='red', lwd=2, pch=21, plot.logicle=T, log='', logicle.params=NULL, ...)
{
	require(spatstat)

	if(plot.logicle)
	{
		plot.logicle(x, y, type='p', pch=pch, log=log, logicle.params=logicle.params, ...)
		pts <- locator()
		poly <- owin(poly=pts)
		plot(poly, add=T, border=border, lwd=lwd)
		ret <- get.logicle(x=x, y=y, log=log, logicle.params=logicle.params)
		isin <- inside.owin(x=ret$x, y=ret$y, w=poly)
	}
	else
	{
		if(grepl('x', log, fixed=T))
		{
			y <- y[x > 0]
			x <- x[x > 0]
		}
		if(grepl('y', log, fixed=T))
		{
			x <- x[y > 0]
			y <- y[y > 0]
		}
		plot(x, y, type='p', pch=pch, log=log, ...)
		pts <- locator()
		poly <- owin(poly=pts)
		plot(poly, add=T, border=border, lwd=lwd)
	}
	return(list(poly=poly, isin=isin, gated=which(isin), backgated=which(!isin)))
}

combineGates <- function(op=c('intersect','union'), ...)
{
	gated <- NULL
	for(gate in list(...))
	{
		if(is.null(gated))
		{
			isin <- gate$isin
			gated <- gate$gated
			backgated <- gate$backgated
		}
		else
		{
			if(op == 'intersect')
			{
				isin <- isin & gate$isin
				gated <- intersect(filter, gate$gated)
				backgated <- union(backgated, gate$backgated)
			}
			else if(op=='union')
			{
				isin <- isin | gate$isin
				gated <- union(filter, gate$gated)
				backgated <- intersect(backgated, gate$backgated)
			}
			else
			{
				stop('The specified op is undefined')
			}
		}
	}
	return(list(isin=isin, gated=gated, backgated=backgated))
}

setGating <- function(x, ..., op='intersect')
{
	# Reset gating
	x[, gated:=F]

	# Create new gating
	ret <- combineGates(op=op, ...)

	# Apply new gating
	x[which(ret$isin), gated:=T]
}

##### Plotting General #####

getIntensityColors <- function(x, hue=T, hueStart=0.15, hueEnd=0, sat=T, satStart=0.1, satEnd=1, val=F, valStart=0, valEnd=1, alpha=1)
{
     minMax <- range(x[is.finite(x)])
     x <- x-minMax[1]
     scale <- minMax[2]-minMax[1]
     if(scale==0)
     {
          warning("The min and max are the same so the data couldn't be scaled. All data will show as one color.")
     }
     else
     {
          x <- x / scale;
     }

     my.hue <- 1
     if(hue)
     {
          my.hue <- hueStart + (hueEnd-hueStart)*x
     }
     my.sat <- 1
     if(sat)
     {
          my.sat <- satStart + (satEnd-satStart)*x
     }
     my.val <- 1
     if(val)
     {
          my.val <- valStart + (valEnd-valStart)*x
     }
     return(hsv(my.hue, my.sat, my.val, alpha))
}

##### Geometry #####

ellipse <- function (x, y, a, b, n)
{
	theta <- seq(0, 2*pi, length.out=n)
	r <- (a*b)/sqrt(b^2 * cos(theta)^2 + a^2 * sin(theta)^2)
	ret.x <- r * cos(theta) + x
	ret.y <- r * sin(theta) + y
	return(list(x=ret.x, y=ret.y))
}

plot.ellipse <- function(x, y, a, b, n=100, border='red', lwd=2, log='', logicle.params=NULL, add=T)
{
	require(spatstat)
	ret <- ellipse(x, y, a, b, n)
	if(!is.null(logicle.params))
	{
		if(grepl('x',log,fixed=T))
		{
			ret$x <- logicle(ret$x, transition=logicle.params$transX, tickSep=logicle.params$tickSepX, base=logicle.params$base, neg.rmF)
		}
		if(grepl('y',log,fixed=T))
		{
			ret$y <- logicle(ret$y, transition=logicle.params$transY, tickSep=logicle.params$tickSepY, base=logicle.params$base, neg.rmF)
		}
	}
	poly <- owin(poly=ret)
	plot(poly, add=add, border=border, lwd=lwd)
	return(poly)
}

##### Loren's brief additions #####
se <- function(x)
{
	sd(x)/sqrt(length(x))
}

##### JEX IO #####

getTableAsSVString <- function(x)
{
	paste(x[, do.call(paste, c(.SD, sep = ',')), .SDcols = names(duh)], collapse=';')
}

##### Similarity #####
sim.transform <- function(x)
{
	return( log( (1 + x) / (1 - x) ) )
}

sim.transform.range <- function(x, epsilon=0.01)
{
     mm <- range(x)
     return( log( (-1*(mm[1]-abs(mm[1]*epsilon)) + x) / ((mm[2]+abs(mm[2]*epsilon)) - x) ) )
}

sim.transform.percentile <- function(x, percentile=0.01)
{
     mm <- getPercentileLimits(x, lower=percentile, upper=1-percentile)
     return( log( (-mm[1] + x) / (mm[2] - x) ) )
}

sim.untransform <- function(x)
{
	return( (exp(x) - 1) / (exp(x) + 1) )
}

logitTransform <- function(p) { log(p/(1-p)) }

##### Stats #####

uniqueo <- function(x, rev=F)
{
     ret <- unique(x)
     if(rev)
     {
          ret <- ret[order(-ret)]
     }
     ret <- ret[order(ret)]
     return(ret)
}

Mode <- function(x) {
     ux <- unique(x)
     ux[which.max(tabulate(match(x, ux)))]
}

# Be sure to have a trailing line or carriage return after last closing bracket.
#
# unlogicle <- function(x, transition=1, tickSep=100)
# {
# 	valsToAdjust <- (x > transition) & !is.na(x)
# 	ordersDifferenceOnDisplay = (x[valsToAdjust] - transition) / tickSep
# 	x[valsToAdjust] <- transition*10^(ordersDifferenceOnDisplay)
# 	return(x)
# }
#
# logicle <- function(x, transition=1, tickSep=100)
# {
# 	if(transition <= 0)
# 	{
# 		warning('Transition must be greater than 0. Setting to 1.')
# 		transition <- 1
# 	}
# 	# Just do it for the right indicies
# 	valsToAdjust <- (x > transition) & !is.na(x)
# 	x[valsToAdjust] <- transition + log10(x[valsToAdjust]/transition)*tickSep
# 	return(x)
# }
