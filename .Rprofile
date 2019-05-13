'Hi, Jay. Defining your default suite of favorite functions...'
'Change these in the file ~/.Rprofile'

.define.fonts <- function()
{
	library(extrafont)
	if(getOS() == 'osx')
	{
		loadfonts(quiet=T)
	}
	else
	{
		loadfonts(device = "win", quiet=T)
	}
	# if(getOS()=='osx')
	# {
	# 	quartzFonts(Helvetica2 = c('Helvetica Neue Light', 'Helvetica Neue Bold', 'Helvetica Neue Light Oblique', 'Helvetica Neue Bold Oblique'))
	# }
	# if('Helvetica Neue2' %in% fonttable()$FamilyName)
	# {
	# 	.setFamily(list(Helvetica2 = 'Helvetica Neue2'))
	# }
	
	if(.tableHasFont('Roboto Light'))
	{
		.setFamily(list(Roboto = 'Roboto Light'))
	}
	if(.tableHasFont('Quicksand'))
	{
		.setFamily(list(Quicksand = 'Quicksand'))
	}
	if(.tableHasFont('Quicksand Regular'))
	{
		# Put this one right after the previous to allow it to take precedent on Mac
		.setFamily(list(Quicksand = 'Quicksand Regular'))
	}
	if(.tableHasFont('Open Sans Light'))
	{
		.setFamily(list(Open = 'Open Sans Light'))
	}
	if(.tableHasFont('Open Sans Regular'))
	{
		.setFamily(list(OpenRegular = 'Open Sans Regular'))
	}
	if(.tableHasFont('Muli Light'))
	{
		.setFamily(list(Muli = 'Muli Light'))
	}
	if(.tableHasFont('Montserrat Light'))
	{
		.setFamily(list(Montserrat = 'Montserrat Light'))
	}
}

.setFamily <- function(fontList)
{
	if(getOS() == 'osx')
	{
		fontList[[names(fontList)[1]]] <- rep(fontList[[1]],4)
		name <- fontList[[1]][1]
		if(.tableHasFont(paste0(name, ' Bold')))
		{
			fontList[[1]][c(2,4)] <- paste0(name, ' Bold')
		}
		else if(.tableHasFont(paste0(name, ' SemiBold')))
		{
			fontList[[1]][c(2,4)] <- paste0(name, ' SemiBold')
		}
		else if(.tableHasFont(paste0(name, ' Medium')))
		{
			fontList[[1]][c(2,4)] <- paste0(name, ' Medium')
		}
		else if(grepl('Light', name, fixed=T) && .tableHasFont(gsub('Light', 'Bold', name, fixed=T)))
		{
			fontList[[1]][c(2,4)] <- gsub('Light', 'Bold', name, fixed=T)
		}
		else if(grepl('Light', name, fixed=T) && .tableHasFont(gsub('Light', 'SemiBold', name, fixed=T)))
		{
			fontList[[1]][c(2,4)] <- gsub('Light', 'SemiBold', name, fixed=T)
		}
		else if(grepl('Light', name, fixed=T) && .tableHasFont(gsub('Light', 'Medium', name, fixed=T)))
		{
			fontList[[1]][c(2,4)] <- gsub('Light', 'Medium', name, fixed=T)
		}
		else if(grepl('Regular', name, fixed=T) && .tableHasFont(gsub('Regular', 'Bold', name, fixed=T)))
		{
			fontList[[1]][c(2,4)] <- gsub('Regular', 'Bold', name, fixed=T)
		}
		else if(grepl('Regular', name, fixed=T) && .tableHasFont(gsub('Regular', 'SemiBold', name, fixed=T)))
		{
			fontList[[1]][c(2,4)] <- gsub('Regular', 'SemiBold', name, fixed=T)
		}
		else if(grepl('Regular', name, fixed=T) && .tableHasFont(gsub('Regular', 'Medium', name, fixed=T)))
		{
			fontList[[1]][c(2,4)] <- gsub('Regular', 'Medium', name, fixed=T)
		}
		do.call(quartzFonts, fontList)
	}
	else
	{
		do.call(windowsFonts, fontList)
	}
}

.tableHasFont <- function(font)
{
	if(getOS() == 'osx')
	{
		return(font %in% fonttable()$FullName)
	}
	else
	{
		return(font %in% fonttable()$FamilyName)
	}
}

.pdfHasFont <- function(font)
{
	return(font %in% names(pdfFonts()))
}

.hasFont <- function(font)
{
	if(getOS()=='osx')
	{
		return(font %in% names(quartzFonts()))
	}
	else
	{
		return(font %in% windowsFonts())
	}
}

.use.lightFont <- function(font=NULL)
{
	.define.fonts()
	
	if(!is.null(font) && any(.hasFont(font)))
	{
		font <- font[which(.hasFont(font))[1]]
		print(paste0("Setting font to ", font))
		par(family=font)
		return()
	}
	else if(!is.null(font))
	{
		warning("Couldn't find the desired font. Substituting another light font instead.")
	}
	
	if(getOS()=='osx')
	{
		font <- c('Open','Roboto','Quicksand','Muli','Montserrat')
	}
	else
	{
		font <- c('Open Sans Light','Roboto Light','Quicksand Light','Muli Light','Montserrat Light')
	}
	
	if(any(.hasFont(font)))
	{
		font <- font[which(.hasFont(font))[1]]
		print(paste0("Setting font to ", font))
		par(family=font)
		return()
	}
	else if(!is.null(font))
	{
		warning("Couldn't find our favorite light fonts. Giving up and using default.")
	}
	
	# Windows needs help finding ghostscript for embedding fonts
	if(getOS() == 'windows')
	{
		gs <- Sys.glob(file.path('C:/','Program Files','gs', 'gs*', 'bin','*c.exe'))
		if(length(gs) > 0 && file.exists(gs))
		{
			print('Found Ghostscript executable. Setting environmental variable accordingly.')
			Sys.setenv(R_GSCMD = gs)
		}
	}
}

dev.off2 <- function(file)
{
	library(extrafont)
	dev.off()
	embed_fonts(file)
}

View2 <- function(x, show.n=1000)
{
	if('data.table' %in% class(x))
	{
		View(x[1:(min(show.n, nrow(x)))])
		print(paste('Showing', min(nrow(x), show.n), 'of', nrow(x), 'rows.'))
	}
	else if('data.frame' %in% class(x))
	{
		View(x[1:(min(show.n, nrow(x))),])
		print(paste('Showing', min(nrow(x), show.n), 'of', nrow(x), 'rows.'))
	}
	else
	{
		View(x)
	}
}

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

make3DPlotMatrices <- function(x, y, z, ...)
{
	x.0 <- uniqueo(x)
	y.0 <- uniqueo(y)
	X <- repmat(matrix(x.0, nrow=1), m=length(x.0), n=1)
	Y <- repmat(matrix(y.0, nrow=length(y.0)), m=1, n=length(y.0))
	ret <- data.table(x=x, y=y, z=z)
	ret <- reorganize(ret, measurementCols = 'y', idCols='x', valueCols='z', ...)
	Z <- data.matrix(ret[, 2:ncol(ret), with=F])
	return(list(X=X, Y=Y, Z=Z))
}

#' FUN MUST take a matrix version of return a vector (or named vector).
#' Allows passing multiple columns (i.e., .SD) using by.column=F
#'
rollapply.SD <- function(SD, width, FUN, ..., by = 1, by.column = F, fill = if (na.pad) NA, na.pad = T, partial = F, align = c("center", "left", "right"), coredata = T)
{
	# FUN MUST return a vector (or named vector which will then result in named columns)
	FUN3 <- function(SD2, FUN2, ...)
	{
		SD2 <- data.table(SD2)
		return(FUN2(SD2, ...))
	}
	
	ret <- data.table(as.matrix(rollapply(SD, width=width, FUN=FUN3, FUN2=FUN, ..., by=by, by.column=by.column, fill=fill, partial=partial, align=align, coredata=coredata)))
	#print(ret)
	return(ret)
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
	
	if(length(cols) == 0)
	{
		print('No changes necessary. No columns fit the filter.')
		return(x)
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
			x[, c(cols):=lapply(.SD, FUN=FUN, ...), .SDcols=cols, by=by][]
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
	paste.cols(x=x, cols=cols, name=idName, sep=sep)
}

getUniqueCombos <- function(x, idCols, ordered=T)
{
	ret <- unique(x, by=idCols)[, idCols, with=F]
	if(ordered)
	{
		setorderv(x, cols=idCols)
	}
	return(ret)
}

getUniqueCombosN <- function(x, idCols, ordered=T)
{
	ret <- x[, list(N=.N), by=idCols]
	if(ordered)
	{
		setorderv(x, cols=idCols)
	}
	return(ret)
}

getUniqueCombosAsStringVector <- function(x, idCols, ordered=T)
{
	dt <- getUniqueCombos(x=x, idCols=idCols, ordered=ordered)
	makeComplexId(x=dt, cols=idCols)
	return(dt$cId)
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
	library(data.table)
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
		if(II > length(r))
		{
			stop('The number of names exceed the number of return items.')
		}
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

assignToClustersXYTogether <- function(xData, yData, xClusters=2, yClusters=xClusters, rndSeed=1234)
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
	
	if(is.null(data))
	{
		stop('Data is NULL. Aborting assignToClusters.')
	}
	
	if(length(data[is.finite(data)]) < nClusters)
	{
		stop('The number of data points must be >= the number of desired clusters')
	}
	
	yo <- data[is.finite(data)]
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
		itemsBetweenMeans <- temp2$Cluster.Raw == i-1 & temp2$x < tempMu2$mu[i] & temp2$x > tempMu2$mu[i-1]
		if(!any(itemsBetweenMeans))
		{
			warning(paste("Couldn't find a threshold between", tempMu2$mu[i-1], "and", tempMu2$mu[i], ". Setting threshold halfway between means."))
			tempThresh <- mean(c(tempMu2$mu[i], tempMu2$mu[i-1]))
		}
		else
		{
			tempThresh <- max(temp2[itemsBetweenMeans,'x'])
		}
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

assignToClustersXY <- function(data, cols, nClusters=2, rndSeed=1234)
{
	library(EMCluster)
	set.seed(rndSeed)
	
	dt <- data[, cols, with=F]
	#
	# if(is.null(data))
	# {
	# 	stop('Data is NULL. Aborting assignToClusters.')
	# }
	#
	# if(length(data[is.finite(data)]) < nClusters)
	# {
	# 	stop('The number of data points must be >= the number of desired clusters')
	# }
	#
	yo <- rep(T, nrow(dt))
	for(col in cols)
	{
		yo <- yo & is.finite(dt[[col]])
	}
	dt <- dt[yo]
	mat <- as.data.frame.matrix(dt)
	
	# Get basic cluster results (results are potentially out of order)
	emobj <- simple.init(mat, nclass = nClusters)
	control <- .EMControl(alpha = 0.99, short.iter = 200, short.eps = 1e-2,
					  fixed.iter = 1, n.candidate = 3,
					  EM.iter = 100, EM.eps = 1e-3, exhaust.iter = 5)
	ret <- emcluster(mat, emobj, assign.class = TRUE, EMC=control)
	
	temp <- copy(data)
	temp[yo, Cluster.Raw:=as.numeric(as.character(ret$class))]
	temp <- as.data.frame(temp)
	
	# Create a data.frame to return
	# temp <- data.frame(mat=mat$x, Cluster.Raw=as.numeric(as.character(ret$class)))
	tempMu <- data.frame(mu=as.vector(ret$Mu), Cluster.Raw=1:nrow(ret$Mu))
	tempMu <- as.data.table(tempMu)[, list(mu=sum(mu^2)), by='Cluster.Raw']
	
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
	
	# thresh <- list()
	# # Go in reverse order from the max cluster number down to 1
	# for(i in nrow(tempMu2):2)
	# {
	# 	# Find the value that discriminates between each pair of clusters
	# 	itemsBetweenMeans <- temp2$Cluster.Raw == i-1 & temp2$x < tempMu2$mu[i] & temp2$x > tempMu2$mu[i-1]
	# 	if(!any(itemsBetweenMeans))
	# 	{
	# 		warning(paste("Couldn't find a threshold between", tempMu2$mu[i-1], "and", tempMu2$mu[i], ". Setting threshold halfway between means."))
	# 		tempThresh <- mean(c(tempMu2$mu[i], tempMu2$mu[i-1]))
	# 	}
	# 	else
	# 	{
	# 		tempThresh <- max(temp2[itemsBetweenMeans,'x'])
	# 	}
	# 	if(!length(tempThresh)==0 && !is.infinite(tempThresh[1]))
	# 	{
	# 		# Then we found a threshold
	# 		thresh[[i-1]] <- tempThresh[1]
	# 		# Assign everything below that threshold to the next lowest cluster
	# 		temp2$Cluster.Clean[temp2$x <= tempThresh[1]] <- i-1
	# 	}
	# }
	#
	# pi <- c()
	# n <- nrow(temp2)
	# for(i in 1:max(temp2$Cluster.Clean))
	# {
	# 	pi <- c(pi, sum(temp2$Cluster.Clean == i)/n)
	# }
	#
	# return(list(data=temp2, mu=tempMu2$mu, thresh=thresh, emclusterObj=ret, pi=pi))
	return(list(data=temp2, emclusterObje=ret))
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
#' @param error.bar.args - list with arguments for the error.bar function (default list(length=0.1)) (See error.bar specified in this file)
#' @param legend.plot - TRUE or FALSE, whether to include a legend or not in the graph (only when a group.column is specified and present)
#' @param args.legend - list of parameters to pass to the 'legend' function (overrides automatically determined parameters) (see ?legend)
#' @param mar - numeric vector indicating the margins our the plot border. Units are lines (default c(4.5,4.5,2,2) = c(lower, left, upper, right))
#' @param ... - additional arguments that are passed to the barplot function (see ?barplot)
#'
#' @export
bar <- function(dt, y.column, color.column, group.column=NULL, error.upper.column=NULL, error.lower.column=error.upper.column,
			 main=NULL, ylab=NULL, xlab=NULL, color.names=NULL, alpha=0.4, group.names=NULL, color.colors=NULL, rotate.x.labels=F, rotate.y.labels=F, plot.border=T,
			 error.bar.args=list(length=0.1),
			 legend.plot=TRUE, legend.args=list(),
			 par.args=list(mar=c(4.5,4.5,2,2)), use.pastels=T, ...)
{
	# Save default margins
	# default.mar <- par('mar')
	
	if(!is.null(legend.args) && !is.list(legend.args))
	{
		stop('legend.args must be a list. Aborting.')
	}
	
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
	
	if(legend.plot && !is.null(group.column))
	{
		legend.args <- merge.lists(list(x="topright", bty=if(!legend.border)"n" else "o", inset=c(0,0)), legend.args)
	}
	else
	{
		legend.args <- NULL
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
		if(use.pastels)
		{
			color.colors <- loopingPastels(seq_along(color.names), a=alpha) #hcl(h=seq(0,270, 270/(length(color.names)))[-length(color.names)])
		}
		else
		{
			color.colors <- hcl(h=seq(0,270, 270/(length(color.names)))[-length(color.names)])
		}
	}
	else if(length(color.colors) != length(color.names))
	{
		warning("The number of colors does not match the number of color.names for the table.")
	}
	
	if(!is.null(group.column))
	{
		if(legend)
		{
			args.barplot <- list(beside=TRUE, height=mat, ylim=c(min(0, ymin), max(0,ymax)), main=main, names.arg=group.names.display,
							 col=color.colors,
							 legend.text=color.names.display, args.legend=args.legend, xpd=TRUE,
							 xlab=if(is.null(xlab)) group.column else xlab,
							 ylab=if(is.null(ylab)) y.column else ylab)
		}
		else
		{
			args.barplot <- list(beside=TRUE, height=mat, ylim=c(min(0, ymin), max(0,ymax)), main=main, names.arg=group.names.display,
							 col=color.colors,
							 legend.text=F, args.legend=NULL, xpd=TRUE,
							 xlab=if(is.null(xlab)) group.column else xlab,
							 ylab=if(is.null(ylab)) y.column else ylab)
		}
		
	}
	else
	{
		args.barplot <- list(beside=TRUE, height=mat, ylim=c(min(0, ymin), max(0,ymax)), main=main, names.arg=color.names.display,
						 col=color.colors,
						 legend.text=F, args.legend=NULL, xpd=TRUE,
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
	if(rotate.x.labels | rotate.y.labels)
	{
		if(rotate.x.labels & !rotate.y.labels)
		{
			args.barplot <- modifyList(args.barplot, list(las=2))
		}
		if(rotate.x.labels & rotate.y.labels)
		{
			args.barplot <- modifyList(args.barplot, list(las=3))
		}
		if(!rotate.x.labels & rotate.y.labels)
		{
			args.barplot <- modifyList(args.barplot, list(las=1))
		}
	}
	
	# Set the plot margins
	do.call(par, par.args)
	
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
		args.error.final <- modifyList(args.error.final, error.bar.args)
		
		# Draw the error bars
		do.call(error.bar, args.error.final)
	}
	else
	{
		# Just plot the bars
		errloc <- do.call(barplot, args.barplot)
	}
	
	# If a plot border is desired, draw it
	if(plot.border) box()
	# par(mar=default.mar)
	
	return(list(x=errloc, y=args.barplot$height, args=args.barplot))
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
			freshCol <- loopingPastels(unique(cluster))[which(unique(cluster)==i)] #do.call(rgb, tempCol)
			if(!density)
			{
				if(!is.null(list(...)$xlab))
				{
					hist(data[cluster==i], breaks=myBreaks, ylab='Count', col=freshCol, add=add, freq=TRUE, ylim=c(0,myLim), ...)
				}
				else
				{
					hist(data[cluster==i], breaks=myBreaks, xlab='Bin Value', ylab='Count', col=freshCol, add=add, freq=TRUE, ylim=c(0,myLim), ...)
				}
			}
			else
			{
				if(!is.null(list(...)$xlab))
				{
					duh <- hist(data[cluster==i], breaks=myBreaks, ylab='Prob. Density', yaxt='n', col=freshCol, add=add, freq=TRUE, ylim=c(0,myLim), ...)
				}
				else
				{
					duh <- hist(data[cluster==i], breaks=myBreaks, xlab='Bin Value', ylab='Prob. Density', yaxt='n', col=freshCol, add=add, freq=TRUE, ylim=c(0,myLim), ...)
				}
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

adjustIntensity <- function(x, oldMin, oldMax, newMin, newMax)
{
	ratio = (newMax - newMin) / (oldMax - oldMin);
	offset = newMin - ratio*oldMin;
	x = x * ratio + offset;
	return(x)
}

colorize <- function(x, min.h=0.666, max.h=0, s=0.7, l=0.5, a=1)
{
	is.mat <- is.matrix(x)
	if(is.mat)
	{
		nc <- ncol(x)
		nr <- nrow(x)
		x <- as.vector(x)
	}
	lims <- range(x, na.rm=T)
	x <- adjustIntensity(x, lims[1], lims[2], min.h, max.h)
	ret <- hsl(h=x, s=s, l=l, a=a)
	if(is.mat)
	{
		ret <- matrix(ret, nrow=nr)
	}
	return(ret)
}

colorizeBlueToRed <- function(x, s=0.7, l=0.5, a=1)
{
	is.mat <- is.matrix(x)
	if(is.mat)
	{
		nc <- ncol(x)
		nr <- nrow(x)
		x <- as.vector(x)
	}
	lims <- range(x, na.rm=T)
	x <- adjustIntensity(x, lims[1], lims[2], 0.666, 0)
	ret <- hsl(h=x, s=s, l=l, a=a)
	if(is.mat)
	{
		ret <- matrix(ret, nrow=nr)
	}
	return(ret)
}

#' The value of the change should be between [-1,1].
#' Negative values darken while positive values lighten
#' Using -1 and 1 make the color black or white, respectively
changeLightness <- function(col, change=0)
{
	# The value of the change should be between [-1,1].
	# Negative values darken while positive values lighten
	# Using -1 and 1 make the color black or white, respectively
	rgbs <- data.table(t(col2rgb(col, alpha=TRUE)))
	rgbs[, c('h','s','v'):=data.table(t(rgb2hsv(r=red, g=green, b=blue)))]
	rgbs[, ':='(H=h, L=0.5*v*(2-s))]
	rgbs[, S:=v*s/(1-abs(2*L-1))]
	if(length(col) == length(change))
	{
		for(i  in 1:length(col))
		{
			if(change[i] >= 0)
			{
				rgbs[i, L2 := L + (1-L)*change[i]]
			}
			else
			{
				rgbs[i, L2 := L+L*change[i]]
			}
		}
	}
	else
	{
		if(change >= 0)
		{
			rgbs[, L2 := L + (1-L)*change]
		}
		else
		{
			rgbs[, L2 := L+L*change]
		}
	}
	
	# Handle rounding errors
	rgbs[L2 > 1, L2:=1]
	rgbs[L2 < 0, L2:=0]
	
	rgbs[, ret:=hsl(h=H, s=S, l=L2)]
	rgbs[, ret2:=setColor(ret, alpha=alpha/255)]
	return(rgbs$ret2)
}

hsl <- function(h, s=1, l=0.5, a=1) {
	x <- data.table(h=h, s=s, l=l, a=a)
	# x[, h:=h / 360]
	#ret <- data.table(r=rep(0,nrow(x)), g=as.double(0.0), b=as.double(0.0))
	x[, ':='(r=0, g=0, b=0)]
	x[s==0, ':='(r=l, g=l, b=l)]
	x[s!=0, q:=ifelse(l < 0.5, l * (1.0 + s), l + s - (l*s))]
	x[s!=0, p:= 2.0 * l - q]
	x[s!=0, r:= hue_to_rgb(p, q, h + 1/3)]
	x[s!=0, g:= hue_to_rgb(p, q, h)]
	x[s!=0, b:= hue_to_rgb(p, q, h - 1/3)]
	# print(x)
	return(rgb(x$r,x$g,x$b,alpha=x$a))
}

hue_to_rgb <- function(p, q, t)
{
	y <- data.table(p=p, q=q, t=t, ret=p)
	#print(y)
	y[, t:= (t %% 1.0)]
	y[t < (1/6), ret:=(p + (q - p) * 6.0 * t)]
	y[t >= 1/6 & t < 1/2, ret:= q]
	y[t >= 1/2 & t < 2/3, ret:= (p + ((q - p) * ((2/3) - t) * 6.0))]
	#print(y)
	return(y$ret)
}

#' Wavelength to RGB
#'
#' This function converts a given wavelength of light to an
#' approximate RGB color value.
#'
#' @param wavelength A wavelength value, in nanometers, in the human visual range from 380 nm through 750 nm.
#'        These correspond to frequencies in the range from 789 THz through 400 THz.
#' @param gamma The \eqn{\gamma} correction for a given display device. The linear RGB values will require
#'        gamma correction if the display device has nonlinear response.
#' @return a color string, corresponding to the result of \code{\link[grDevices]{rgb}} on the
#'        calculated R, G, B components.
#' @source Original code taken from  http://www.noah.org/wiki/Wavelength_to_RGB_in_Python
#' @references http://stackoverflow.com/questions/1472514/convert-light-frequency-to-rgb
#' @references http://www.fourmilab.ch/documents/specrend/


wave2rgb <- function(wavelength, gamma=0.8){
	
	#
	#    Based on code by Dan Bruton
	#    http://www.physics.sfasu.edu/astro/color/spectra.html
	#    '''
	
	if (wavelength >= 380 & wavelength <= 440) {
		attenuation = 0.3 + 0.7 * (wavelength - 380) / (440 - 380)
		R = ((-(wavelength - 440) / (440 - 380)) * attenuation) ^ gamma
		G = 0.0
		B = (1.0 * attenuation) ^ gamma
	}
	else if (wavelength >= 440 & wavelength <= 490) {
		R = 0.0
		G = ((wavelength - 440) / (490 - 440)) ^ gamma
		B = 1.0
	}
	else if (wavelength >= 490 & wavelength <= 510) {
		R = 0.0
		G = 1.0
		B = (-(wavelength - 510) / (510 - 490)) ^ gamma
	}
	else if (wavelength >= 510 & wavelength <= 580) {
		R = ((wavelength - 510) / (580 - 510)) ^ gamma
		G = 1.0
		B = 0.0
	}
	else if (wavelength >= 580 & wavelength <= 645) {
		R = 1.0
		G = (-(wavelength - 645) / (645 - 580)) ^ gamma
		B = 0.0
	}
	else if (wavelength >= 645 & wavelength <= 750) {
		attenuation = 0.3 + 0.7 * (750 - wavelength) / (750 - 645)
		R = (1.0 * attenuation) ^ gamma
		G = 0.0
		B = 0.0
	}
	else {
		R = 0.0
		G = 0.0
		B = 0.0
	}
	R = R * 255
	G = G * 255
	B = B * 255
	return (rgb(floor(R), floor(G), floor(B), max=255))
}

loopingPastels <- function(k, min.h=0.666, max.h=min.h + 1, max.k=min(max(k),10), s=0.7, l=0.5, a=0.4)
{
	cols=hsl(h=seq(min.h,max.h, length.out=max.k+1)[-(max.k+1)], s=s, l=l, a=a)
	n <- length(cols)
	return(cols[((k-1) %% (n)) + 1])
}

loopingPalette <- function(k, cols=palette()[-1])
{
	n <- length(cols)
	return(cols[((k-1) %% (n)) + 1])
}

adjustColor <- function(my.colors, alpha.factor)
{
	library(grDevices)
	x <- col2rgb(my.colors, alpha = TRUE)/255
	x <- data.table(t(x))
	x[, alpha:=alpha*alpha.factor]
	ret <- rgb(x$red,x$green,x$blue,x$alpha)
	return(ret)
}

removeAlpha <- function(my.colors)
{
	library(grDevices)
	x <- col2rgb(my.colors)/255
	ret <- rgb(x[1L,],x[2L,],x[3L,])
	return(ret)
}

setColor <- function(my.colors, alpha)
{
	library(grDevices)
	x <- col2rgb(my.colors)/255
	x <- data.table(t(x), alpha=alpha)
	ret <- rgb(x$red,x$green,x$blue,x$alpha)
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

# library(units)
# library(gridExtra)
# library(grid)
draw.table <- function(x, y, dt, font.size=10, newpage=F, padding=unit(c(1,1), units='mm'))
{
	x <- round(x, 1)
	y <- round(y, 1)
	x <- min(x, 1)
	x <- max(x, 0)
	y <- min(y, 1)
	y <- max(y, 0)
	n <- (11*(y*10))+x*10 + 1
	args <- rep(list(grob()), 11*11)
	theme <- ttheme_minimal(base_size = font.size, base_family = par('family'))
	print(theme$core$padding)
	theme$core$padding <- padding
	print(theme$core$padding)
	g <- tableGrob(fit.table, theme=theme, rows=NULL)
	args[[n]] <- g
	args$nrow=11
	args$ncol=11
	args$newpage=newpage
	do.call(grid.arrange, args)
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
data.table.plot.all <- function(data, xcol, ycol=NULL, errcol.upper=NULL, errcol.lower=errcol.upper, alphacol=NULL, colorcol=NULL, main.show=T, mar=NULL, alpha.rank=T, alpha=0.8, by=NULL, plot.by=NULL, save.by.index=F, line.color.by=by, randomize=F,
						  gates=list(),
						  min.h=0.666, max.h=min.h+0.8, contour.levels=4, contour.ngrid=20, contour.quantiles=T, contour.adj=c(1,1),
						  env.err=T, env.args=list(),
						  log='', logicle.params=NULL, trans.logit=c(F,F), main='', xlim=NULL, ylim=NULL, xlab=NULL, ylab=NULL, pch.alpha=1, type=c('p','c','l','d','h'),
						  density.args=NULL, cumulative=F, breaks=100, percentile.limits=c(0,1,0,1),
						  h=NULL, h.col='black', h.lty=2, h.lwd=1, v=NULL, v.col='black', v.lty=2, v.lwd=1,
						  legend.plot=T, legend.args=list(x='topright', cex=0.5, bg='white', bty='o', title=NULL, inset=0, ncol=1),
						  save.plot=T, save.file=NULL, save.type='png', save.width=5, save.height=4, family=c('Open Sans Light', 'Roboto Light', 'Quicksand', 'Muli Light', 'Montserrat Light'), res=300,
						  sample.size=-1, sample.seed=sample(1:1000, 1), polygons=list(),
						  spline.smooth=F, spline.spar=0.2, spline.n=.Machine$integer.max,
						  cross.fun=median, cross.cex=3, cross.pch=10, cross.lwd=2.5, cross.args=list(na.rm=T), cross.plot=F, 
						  mtext.args=NULL, time.stamp.plot=F, time.stamp.conversion=function(x){as.numeric(x)}, time.stamp.prefix='Time = ', time.stamp.suffix=' [h]',
						  ...)
{
	# REMEMBER: las works now to rotate the number labels (0-3)
	# REMEMBER: mgp is also an option, default is c(3,1,0). Change the 3 to move labels closer or farther from axis.
	
	if('type' %in% names(data))
	{
		stop("The name 'type' is used internally and can't be a column name. Change the name of that column. Sorry.")
	}
	
	if(is.null(data) || nrow(data)==0)
	{
		warning('Data is empty. Nothing to plot.')
		return()
	}
	legend.args <- merge.lists(list(x='topright', cex=0.5, bg='white', bty='o', title=NULL, lwd=2, lty=1, inset=0, ncol=1), legend.args)
	
	env.args <- merge.lists(list(env.alpha=0.5, env.smooth=F, env.spar=0.2), env.args)
	
	setkeyv(data, cols=c(plot.by, by))
	
	# If we are saving the file, then load fonts if possible
	# if(is.null(save.file))
	# {
	# 	if(all(family==c('Open Sans Light', 'Roboto Light', 'Quicksand', 'Muli Light', 'Montserrat Light')))
	# 	{
	# 		family <- c('Open', 'Roboto', 'Quicksand', 'Muli', 'Montserrat')
	# 	}
	# 	.use.lightFont(family)
	# }
	
	# Abort if 'xcol' is not in the data table
	if(!(xcol %in% names(data)))
	{
		stop(paste0('The xcol provided (', xcol, ') does not exist in the data table. Aborting.'))
	}
	
	# Abort if we don't have a ycol and we are doing an l or p plot.
	if(type[1] %in% c('l','p'))
	{
		if(!(ycol %in% names(data)))
		{
			stop(paste0('The ycol provided (', ycol, ') does not exist in the data table. Aborting.'))
		}
	}
	
	# # Create a temporary gating column if necessary
	# hasGatedCol <- !is.null(data[['gated']])
	# if(!hasGatedCol)
	# {
	# 	data[, gated:=T]
	# }
	
	# Create the legend table
	legend.colors <- getUniqueCombosN(data, idCols=c(plot.by, by))
	legend.colors[, plot.by.index:=.GRP, by=plot.by]
	legend.colors[, by.index:=.GRP, by=by]
	legend.colors[, line.color.by.index:=.GRP, by=line.color.by]
	paste.cols(legend.colors, cols=line.color.by, name='names', sep=':')
	
	# Set legend colors
	if(is.null(line.color.by))
	{
		legend.colors[, my.color:='black']
	}
	else if(is.null(legend.args$col))
	{
		legend.colors[, my.color:=loopingPastels(k=line.color.by.index, min.h=min.h, max.h=max.h, max.k=max(line.color.by.index), l=0.45, a=1)]
	}
	else
	{
		legend.colors[, my.color:=legend.args$col[line.color.by.index]]
	}
	
	# Set legend lty
	if(is.null(colorcol) && length(legend.args$lty) != 1 && length(legend.args$lty) != length(unique(legend.colors$line.color.by.index)))
	{
		stop("The number of lty is either not 1 or does not match the number of color groups in the data.")
	}
	if(length(legend.args$lty) == 1)
	{
		legend.colors[, lty:=legend.args$lty]
	}
	else
	{
		legend.colors[, lty:=legend.args$lty[line.color.by.index]]
	}
	
	# Set legend lwd
	if(is.null(colorcol) && length(legend.args$lwd) != 1 && length(legend.args$lwd) != length(unique(legend.colors$line.color.by.index)))
	{
		stop("The number of lty is either not 1 or does not match the number of color groups in the data.")
	}
	if(length(legend.args$lwd) == 1)
	{
		legend.colors[, lwd:=legend.args$lwd]
	}
	else
	{
		legend.colors[, lwd:=legend.args$lwd[line.color.by.index]]
	}
	
	if(!is.null(legend.args$legend))
	{
		if(is.null(colorcol) && length(legend.args$legend) != length(unique(legend.colors$line.color.by.index)))
		{
			stop("The number of legend names provided does not match the number of color groups in the data.")
		}
		legend.colors[, names:=legend.args$legend[line.color.by.index]]
	}
	setkeyv(legend.colors, c(plot.by, by))
	# Done creating legend table
	
	if(is.null(alphacol))
	{
		# Apply alpha
		legend.colors[, my.color:=adjustColor(my.color, alpha.factor=alpha)]
	}
	
	data <- data[legend.colors, nomatch=0]
	setnames(data, 'my.color', 'my.temp.color')
	if(!is.null(colorcol))
	{
		data[, my.temp.color:=get(colorcol)]
	}
	setkeyv(data, c(plot.by, by))
	
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
	
	# set xlab and ylab if necessary
	xlab <- getDefault(xlab, xcol)
	if(is.null(ylab))
	{
		if(type[1] %in% c('l','p','c') & !is.null(ycol))
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
	
	if(time.stamp.plot)
	{
		mtext.args <- merge.lists(list(adj=0, side=1, line=par('mgp')[1], cex=par('cex.lab')), getDefault(mtext.args, list()))
	}
	else
	{
		time.stamp.prefix <- ''
		time.stamp.suffix <- ''
		time.stamp.conversion <- function(x){return('')}
	}
	
	if(save.plot && !is.null(save.file))
	{
		if(is.null(plot.by))
		{
			data[, plot.wrapper(data=.SD, xcol=xcol, ycol=ycol, mar=mar, main=main, by=my.by, line.color.by=line.color.by, errcol.upper=errcol.upper, errcol.lower=errcol.lower, env.err=env.err, env.args=env.args, log=log, logicle.params=logicle.params, trans.logit=trans.logit, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, type=type, density.args=density.args, cumulative=cumulative, breaks=breaks, percentile.limits=percentile.limits, h=h, h.col=h.col, h.lty=h.lty, h.lwd=h.lwd, v=v, v.col=v.col, v.lty=v.lty, v.lwd=v.lwd, legend.plot=legend.plot, legend.args=legend.args, legend.colors=legend.colors[plot.by.index==.GRP], save.file=paste0(save.file,'.', save.type), save.width=save.width, save.height=save.height, sample.size=sample.size, sample.seed=sample.seed, family=family, res=res, alpha.backgated=alpha, polygons=polygons, cross.fun=cross.fun, cross.cex=cross.cex, cross.pch=cross.pch, cross.lwd=cross.lwd, cross.plot=cross.plot, contour.levels=contour.levels, contour.ngrid=contour.ngrid, contour.quantiles=contour.quantiles, contour.adj=contour.adj, randomize=randomize, spline.smooth=spline.smooth, spline.spar=spline.spar, spline.n=spline.n, plot.by.index=plot.by.index, mtext.args=mtext.args, ...)]
		}
		else
		{
			if(save.by.index)
			{
				data[, plot.wrapper(data=.SD, xcol=xcol, ycol=ycol, mar=mar, main=if (!main.show) '' else paste0(paste(as.character(plot.by), collapse='.'), ' = ', paste(lapply(.BY, as.character), collapse='.')), by=my.by, line.color.by=line.color.by, errcol.upper=errcol.upper, errcol.lower=errcol.lower, env.args=env.args, log=log, logicle.params=logicle.params, trans.logit=trans.logit, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, type=type, density.args=density.args, cumulative=cumulative, breaks=breaks, percentile.limits=percentile.limits, h=h, h.col=h.col, h.lty=h.lty, h.lwd=h.lwd, v=v, v.col=v.col, v.lty=v.lty, v.lwd=v.lwd, legend.plot=legend.plot, legend.args=legend.args, legend.colors=legend.colors, save.file=paste0(save.file, .GRP, '.', save.type), save.width=save.width, save.height=save.height, family=family, res=res, sample.size=sample.size, sample.seed=sample.seed, alpha.backgated=alpha, polygons=polygons, cross.fun=cross.fun, cross.cex=cross.cex, cross.pch=cross.pch, cross.lwd=cross.lwd, cross.args=cross.args, cross.plot=cross.plot, contour.levels=contour.levels, contour.ngrid=contour.ngrid, contour.quantiles=contour.quantiles, contour.adj=contour.adj, randomize=randomize, spline.smooth=spline.smooth, spline.spar=spline.spar, spline.n=spline.n, mtext.args=mtext.args, mtext.text=getDefault(mtext.args$text, paste0(time.stamp.prefix, time.stamp.conversion(.BY[[1]]), time.stamp.suffix)), ...), by=plot.by]
			}
			else
			{
				data[, plot.wrapper(data=.SD, xcol=xcol, ycol=ycol, mar=mar, main=if (!main.show) '' else paste0(paste(as.character(plot.by), collapse='.'), ' = ', paste(lapply(.BY, as.character), collapse='.')), by=my.by, line.color.by=line.color.by, errcol.upper=errcol.upper, errcol.lower=errcol.lower, env.args=env.args, log=log, logicle.params=logicle.params, trans.logit=trans.logit, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, type=type, density.args=density.args, cumulative=cumulative, breaks=breaks, percentile.limits=percentile.limits, h=h, h.col=h.col, h.lty=h.lty, h.lwd=h.lwd, v=v, v.col=v.col, v.lty=v.lty, v.lwd=v.lwd, legend.plot=legend.plot, legend.args=legend.args, legend.colors=legend.colors, save.file=paste0(save.file, paste0(.BY, collapse='.'), '.', save.type), save.width=save.width, save.height=save.height, family=family, res=res, sample.size=sample.size, sample.seed=sample.seed, alpha.backgated=alpha, polygons=polygons, cross.fun=cross.fun, cross.cex=cross.cex, cross.pch=cross.pch, cross.lwd=cross.lwd, cross.args=cross.args, cross.plot=cross.plot, contour.levels=contour.levels, contour.ngrid=contour.ngrid, contour.quantiles=contour.quantiles, contour.adj=contour.adj, randomize=randomize, spline.smooth=spline.smooth, spline.spar=spline.spar, spline.n=spline.n,  mtext.args=mtext.args, mtext.text=getDefault(mtext.args$text, paste0(time.stamp.prefix, time.stamp.conversion(.BY[[1]]), time.stamp.suffix)), ...), by=plot.by]
			}
		}
	}
	else
	{
		if(!is.null(plot.by))
		{
			data[, plot.wrapper(data=.SD, xcol=xcol, ycol=ycol, mar=mar, main=if (!main.show) '' else paste0(paste(as.character(plot.by), collapse='.'), ' = ', paste(lapply(.BY, as.character), collapse='.')), by=my.by, line.color.by=line.color.by, errcol.upper=errcol.upper, errcol.lower=errcol.lower, env.err=env.err, env.args=env.args, log=log, logicle.params=logicle.params, trans.logit=trans.logit, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, type=type, density.args=density.args, cumulative=cumulative, breaks=breaks, percentile.limits=percentile.limits, h=h, h.col=h.col, h.lty=h.lty, h.lwd=h.lwd, v=v, v.col=v.col, v.lty=v.lty, v.lwd=v.lwd, legend.plot=legend.plot, legend.args=legend.args, legend.colors=legend.colors[plot.by.index==.GRP], save.file=NULL, family=family, res=res, sample.size=sample.size, sample.seed=sample.seed, alpha.backgated=alpha, polygons=polygons, cross.fun=cross.fun, cross.cex=cross.cex, cross.pch=cross.pch, cross.lwd=cross.lwd, cross.args=cross.args, cross.plot=cross.plot, contour.levels=contour.levels, contour.ngrid=contour.ngrid, contour.quantiles=contour.quantiles, contour.adj=contour.adj, randomize=randomize, spline.smooth=spline.smooth, spline.spar=spline.spar, spline.n=spline.n,  mtext.args=mtext.args, mtext.text=getDefault(mtext.args$text, paste0(time.stamp.prefix, time.stamp.conversion(.BY[[1]]), time.stamp.suffix)), ...), by=plot.by]
		}
		else
		{
			data[, plot.wrapper(data=.SD, xcol=xcol, ycol=ycol, mar=mar, main=main, by=my.by, line.color.by=line.color.by, errcol.upper=errcol.upper, errcol.lower=errcol.lower, env.err=env.err, env.args=env.args, log=log, logicle.params=logicle.params, trans.logit=trans.logit, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, type=type, density.args=density.args, cumulative=cumulative, breaks=breaks, percentile.limits=percentile.limits, h=h, h.col=h.col, h.lty=h.lty, h.lwd=h.lwd, v=v, v.col=v.col, v.lty=v.lty, v.lwd=v.lwd, legend.plot=legend.plot, legend.args=legend.args, legend.colors=legend.colors, save.file=NULL, family=family, res=res, sample.size=sample.size, sample.seed=sample.seed, alpha.backgated=alpha, polygons=polygons, cross.fun=cross.fun, cross.cex=cross.cex, cross.pch=cross.pch, cross.lwd=cross.lwd, cross.args=cross.args, cross.plot=cross.plot, contour.levels=contour.levels, contour.ngrid=contour.ngrid, contour.quantiles=contour.quantiles, contour.adj=contour.adj, randomize=randomize, spline.smooth=spline.smooth, spline.spar=spline.spar, spline.n=spline.n,  mtext.args=mtext.args, ...)]
		}
	}
	
	
	print('Finished plotting.')
	
	# data[, my.temp.color:=NULL]
	# data[, alphas:=NULL]
	
	# # If necessary, remove gating column
	# if(!hasGatedCol)
	# {
	# 	data[, gated:=NULL]
	# }
}

start.plot.to.file <- function(save.file, save.width, save.height, family='Open Sans Light', res=300)
{
	embedTheFont <- F
	if(getOS()=='osx')
	{
		if(endsWith(save.file, 'png'))
		{
			png(save.file, width=save.width, height=save.height, units='in', res=res)
			if(!is.null(family))
			{
				.use.lightFont(font=family[1])
			}
		}
		else if(endsWith(save.file, 'pdf'))
		{
			library(Cairo)
			if(!is.null(family))
			{
				cairo_pdf(save.file, width=save.width, height=save.height, family=family[1])
			}
			else
			{
				cairo_pdf(save.file, width=save.width, height=save.height)
			}
			embedTheFont <- T
		}
		else
		{
			stop(paste0('The save type ', save.type, ' is not supported. Aborting'))
		}
		
		# if(any(.pdfHasFont(daFamily)))
		# {
		# 	font <- family[which(.pdfHasFont(family))[1]]
		# 	print(paste0("Setting the font to ", font))
		# 	pdf(save.file, width=save.width, height=save.height, family=font)
		# 	embedTheFont <- T
		# }
		# else
		# {
		# 	print("Couldn't find the specified font family. Using default.")
		# 	pdf(save.file, width=save.width, height=save.height)
		# }
	}
	else if(getOS()!='osx')
	{
		if(any(.hasFont(family)))
		{
			font <- family[which(.hasFont(family))[1]]
			print(paste0("Setting the font to ", font))
			save.file <- gsub('.pdf', '.png', save.file, fixed=T) # Make sure it is a png
			png(save.file, width=save.width, height=save.height, res=res, units='in', family=font)
			.use.lightFont(font=font)
		}
		else
		{
			print("Couldn't find the specified font family. Using default.")
			save.file <- gsub('.pdf', '.png', save.file, fixed=T) # Make sure it is a png
			png(save.file, width=save.width, height=save.height, res=res, units='in')
			.use.lightFont(font=font)
		}
		
	}
	
	return(embedTheFont)
}

tempFunc <- function(x, y, upper, lower, log, logicle.params, line.color.by, theColor, lty, env.alpha, spline.smooth, spline.spar, spline.n, env.err=env.err, env.args=env.args, ...)
{
	# Elipses is taken from calling environment (i.e., plot.wrapper)
	data.table.lines(x=x, y=y, log=log, logicle.params=logicle.params, lty=lty, spline.smooth=spline.smooth, spline.spar=spline.spar, spline.n=spline.n, ...)
	if(!is.null(upper))
	{
		data.table.error.bar(x=x, y=y, upper=upper, lower=lower, env.err=env.err, env.color=adjustColor(list(...)$col, env.args$env.alpha), env.args=env.args, length=0.05, draw.lower=TRUE, log=log, logicle.params=logicle.params, spline.smooth=spline.smooth, spline.spar=spline.spar, spline.n=spline.n)
	}
}

plot.wrapper <- function(data, xcol, ycol, errcol.upper=NULL, errcol.lower=errcol.upper, by, plot.by=NULL, mar=par('mar'), line.color.by=NULL, pch.outline=rgb(0,0,0,0), alpha.backgated=1, env.err=T, env.args=list(env.alpha=0.5, env.smooth=F, env.spar=0.2), log='', logicle.params=NULL, trans.logit=c(F,F), type=c('l','p','c','h','d'), density.args=NULL, cumulative=F, breaks=100, percentile.limits=c(0,1,0,1), h=NULL, h.col='red', h.lty=1, h.lwd=2, v=NULL, v.col='red', v.lty=1, v.lwd=2, legend.plot=T, legend.args=NULL, legend.colors=NULL, save.file=NULL, save.width=5, save.height=4, family, res=300, sample.size=-1, sample.seed=sample(1:1000), polygons=polygons, xlim=NULL, ylim=NULL, add=F, cross.fun=median, cross.cex=3, cross.pch=10, cross.lwd=2.5, cross.args=list(), cross.plot=F, contour.levels=5, contour.ngrid=20, contour.quantiles=T, contour.adj=c(1,1), contour.alphas=NULL, randomize=F, spline.smooth=F, spline.spar=0.1, spline.n=100, mtext.args=NULL, mtext.text='', ...)
{
	if(is.null(data) | nrow(data)==0)
	{
		warning('Data is empty. Nothing to plot.')
		return()
	}
	if(is.null(xcol))
	{
		stop("xcol must exist within the data.table and can't be NULL")
	}
	# if(is.null(ycol))
	# {
	# 	temp.logicle.params <- fillDefaultLogicleParams(x=data[[xcol]], y=NULL, logicle.params=logicle.params)
	# }
	# else
	# {
	# 	temp.logicle.params <- fillDefaultLogicleParams(x=data[[xcol]], y=data[[ycol]], logicle.params=logicle.params)
	# }
	
	embedTheFont <- F
	if(!is.null(save.file))
	{
		embedTheFont <- start.plot.to.file(save.file=save.file, save.width=save.width, save.height=save.height, family=family, res=res)
	}
	
	if(type[1] %in% c('p','l','c') && (is.null(list(...)$add) || !list(...)$add))
	{
		if(!is.numeric(data[[xcol]]))
		{
			stop('The xcol should be numeric')
		}
		if(!is.numeric(data[[ycol]]))
		{
			stop('The ycol should be numeric')
		}
		l(x1, y1, xlim, ylim, logicle.params.plc) %=% start.logicle(x=data[[xcol]], y=data[[ycol]], log=log, logicle.params=logicle.params, percentile.limits=percentile.limits, xlim=xlim, ylim=ylim, add=add, mar=mar, trans.logit=trans.logit, ...)
	}
	
	# Set some internal functions and sampling parameters
	my.sample <- function(x, size, seed)
	{
		set.seed(seed)
		return(sample(x=x, size=size))
	}
	if(sample.size <= 0)
	{
		sample.size <- .Machine$integer.max
	}
	
	# tempFunc <- function(x, y, upper, log, logicle.params, line.color.by, alphas, env.alpha)
	# {
	# 	data.table.lines(x=x, y=y, log=log, logicle.params=logicle.params, col=legend.colors[names==paste.mget(line.color.by,sep=':')]$my.color[1], ...)
	# 	if(!is.null(upper))
	# 	{
	# 		data.table.error.bar(x=x, y=y, upper=upper, env.err=env.err, env.color=adjustColor(legend.colors[names==paste.mget(line.color.by,sep=':')]$my.color[1], alpha.factor=env.alpha), length=0.05, draw.lower=TRUE, log=log, logicle.params=logicle.params)
	# 	}
	# }
	
	# Plot the data
	las <- getDefault(list(...)$las, c(1,1))
	if(type[1] == 'l')
	{
		temp <- copy(data)
		temp[, grp:=.GRP, by=by]
		temp <- temp[grp %in% my.sample(1:max(grp), min(sample.size, max(grp)), seed=sample.seed)]
		# Get sample
		if(randomize)
		{
			temp[, grp:=as.numeric(factor(grp, levels=my.sample(1:max(grp), min(sample.size, max(grp)), seed=sample.seed)))]
			setorderv(temp, c('grp', by))
		}
		# Print this out to make sure the legeng matches when doing sampling.
		#print(unique(temp$cId))
		
		# Plot
		if(is.null(errcol.upper) && is.null(errcol.lower))
		{
			# Call data.table.lines, only plotting the line if the .GRP is one of the randomly sampled numbers
			# Index colors according to their index in the randomly sampled list, that way you actually loop through the pallet as normal (i.e., "red", "green3", "blue", ...)
			temp[, tempFunc(x=get(xcol), y=get(ycol), upper=NULL, log=log, logicle.params=logicle.params.plc, col=my.temp.color[1], lwd=lwd[1], lty=lty[1], env.alpha=env.alpha, spline.smooth=spline.smooth, spline.spar=spline.spar, spline.n=spline.n, env.err=env.err, env.args=env.args, ...), by='grp']
		}
		else
		{
			# Call data.table.lines, only plotting the line if the .GRP is one of the randomly sampled numbers
			# Index colors according to their index in the randomly sampled list, that way you actually loop through the pallet as normal (i.e., "red", "green3", "blue", ...)
			temp[, tempFunc(x=get(xcol), y=get(ycol), upper=get(errcol.upper), lower=get(errcol.lower), log=log, logicle.params=logicle.params.plc, col=my.temp.color[1], lwd=lwd[1], lty=lty[1], env.alpha=env.alpha, spline.smooth=spline.smooth, spline.spar=spline.spar, spline.n=spline.n, env.err=env.err, env.args=env.args, ...), by='grp']
		}
		
		finish.logicle(log=log, logicle.params=logicle.params.plc, h=h, h.col=h.col, h.lty=h.lty, h.lwd=h.lwd, v=v, v.col=v.col, v.lty=v.lty, v.lwd=v.lwd, add=add, ...)
	}
	else if(type[1] == 'p' || type[1] == 'c')
	{
		temp <- data[, .SD[my.sample(1:.N, min(.N,sample.size), seed=sample.seed)], by=line.color.by]
		if(randomize)
		{
			temp <- temp[sample(.N,.N)]
		}
		
		plot.logicle(x=temp[[xcol]], y=temp[[ycol]], type=type[1], log=log, logicle.params=logicle.params, percentile.limits=percentile.limits, xlim=xlim, ylim=ylim, add=T, col=pch.outline, bg=temp[['my.temp.color']], pch=getDefault(legend.args$pch, 21), contour.levels=contour.levels, contour.ngrid=contour.ngrid, contour.quantiles=contour.quantiles, contour.adj=contour.adj, contour.alphas=contour.alphas,  ...)
		
		# # data[, data.table.points(x=get(xcol), y=get(ycol), log=log, xlim=xlim, xlab=xlab, ylab=ylab, transX=transX, transY=transY, tickSepX=tickSepX, tickSepY=tickSepY, col=pch.outline, bg=my.temp.color, pch=21, ...), by=by]
		# if(!is.null(errcol))
		# {
		# 	# (x, y, upper=NULL, lower=upper, length=0.1, draw.lower=TRUE, log='', transX=1, transY=1, tickSepX=10, tickSepY=10))
		# 	data[, data.table.error.bar(x=get(xcol), y=get(ycol), upper=get(errcol), length=0.05, draw.lower=TRUE, log=log, transX=transX, transY=transY, tickSepX=tickSepX, tickSepY=tickSepY), by=by]
		# }
		
		# Plot populations cross hairs if desired
		if(cross.plot)
		{
			# Plot the gated data
			cross.data <- temp[, list(x=do.call(cross.fun, c(list(x=get(xcol)), cross.args)), y=do.call(cross.fun, c(list(x=get(ycol)), cross.args))), by=c('my.temp.color',by)]
			setorderv(cross.data, cols=by, order=-1L)
			
			for(i in 1:nrow(cross.data))
			{
				plot.logicle(x=cross.data$x[i], y=cross.data$y[i], type='p', log=log, logicle.params=logicle.params, percentile.limits=percentile.limits, col='black', pch=cross.pch, cex=cross.cex, lwd=cross.lwd*1.3, add=T, ...)
				plot.logicle(x=cross.data$x[i], y=cross.data$y[i], type='p', log=log, logicle.params=logicle.params, percentile.limits=percentile.limits, col=setColor(cross.data$my.temp.color[i], 1), pch=cross.pch, cex=cross.cex, lwd=cross.lwd*0.65, add=T, ...)
			}
		}
		
		finish.logicle(log=log, logicle.params=logicle.params, h=h, h.col=h.col, h.lty=h.lty, h.lwd=h.lwd, v=v, v.col=v.col, v.lty=v.lty, v.lwd=v.lwd, add=add, trans.logit=trans.logit, ...)
	}
	else if(type == 'h' | type == 'd')
	{
		if(is.character(log) && type == 'h')
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
			xlim <- getPercentileValues(data[[xcol]], levels=c(percentile.limits[1:2]))
		}
		
		# Set the histogram binning (may or may not be used)
		if(length(breaks)>1)
		{
			breaks <- breaks
		}
		else
		{
			breaks <- c(-Inf, seq(xlim[1], xlim[2], length.out=breaks+1), Inf)
		}
		
		
		if(!is.null(xlim))
		{
			logicle.params2 <- fillDefaultLogicleParams(x=xlim, y=NULL, logicle.params=logicle.params)
		}
		else
		{
			logicle.params2 <- fillDefaultLogicleParams(x=data[[xcol]], y=NULL, logicle.params=logicle.params)
		}
		
		
		
		# ylim will either be null or defined here. Override if null setting to max of all plots.
		if(is.null(ylim))
		{
			suppressWarnings(
				if(type[1] == 'h')
				{
					ylims <- data[is.finite(get(xcol)), list(grp=.GRP, minY=min(data.table.hist(x=get(xcol), type=type[1], log=gsub('y','',log), xlim=xlim, logicle.params=logicle.params2, mar=mar, trans.logit=trans.logit, density.args=density.args, cumulative=cumulative, breaks=breaks, border=removeAlpha(my.temp.color[1]), col=my.temp.color[1], lwd=lwd[1], lty=lty[1], xaxt='n', yaxt='n', add=(add || (.GRP!=1)), silent=T, ...)$y[2:(length(breaks)-2)]), maxY=max(data.table.hist(x=get(xcol), type=type[1], log=log, logicle.params=logicle.params, mar=mar, trans.logit=trans.logit, density.args=density.args, cumulative=cumulative, breaks=breaks, border=removeAlpha(my.temp.color[1]), col=my.temp.color[1], xaxt='n', add=(.GRP!=1), silent=T, ...)$y[2:(length(breaks)-2)])), by=by]
				}
				else
				{
					# it is a density plot
					ylims <- data[is.finite(get(xcol)), list(grp=.GRP, minY=min(data.table.hist(x=get(xcol), type=type[1], log=gsub('y','',log), xlim=xlim, logicle.params=logicle.params2, mar=mar, trans.logit=trans.logit, density.args=density.args, cumulative=cumulative, breaks=breaks, border=removeAlpha(my.temp.color[1]), col=my.temp.color[1], lwd=lwd[1], lty=lty[1], xaxt='n', yaxt='n', add=(add || (.GRP!=1)), silent=T, ...)$y), maxY=max(data.table.hist(x=get(xcol), type=type[1], log=log, logicle.params=logicle.params, mar=mar, trans.logit=trans.logit, density.args=density.args, cumulative=cumulative, breaks=breaks, border=removeAlpha(my.temp.color[1]), col=my.temp.color[1], xaxt='n', add=(.GRP!=1), silent=T, ...)$y)), by=by]
				}
			)
			# Function needs to return a single value so we arbitraritly use 'max' of the 'y'
			ylim=c(min(ylims[['minY']]),max(ylims[['maxY']]))
		}
		if(!is.null(xlim))
		{
			logicle.params3 <- fillDefaultLogicleParams(x=xlim, y=ylim, logicle.params=logicle.params)
		}
		else
		{
			logicle.params3 <- fillDefaultLogicleParams(x=data[[xcol]], y=ylim, logicle.params=logicle.params)
		}
		data[, max(data.table.hist(x=get(xcol)[is.finite(get(xcol))], type=type[1], log=log, xlim=xlim, ylim=ylim, logicle.params=logicle.params3, mar=mar, trans.logit=trans.logit, density.args=density.args, cumulative=cumulative, breaks=breaks, border=removeAlpha(my.temp.color[1]), col=my.temp.color[1], lwd=lwd[1], lty=lty[1], xaxt='n', yaxt='n', add=(add || (.GRP!=1)), silent=F, ...)$y), by=by]
		
		finishABLine(h=h, h.col=h.col, h.lty=h.lty, h.lwd=h.lwd, v=v, v.col=v.col, v.lty=v.lty, v.lwd=v.lwd, log=log, logicle.params=logicle.params, trans.logit=trans.logit)
		
		if(!(!is.null(list(...)$axes) && list(...)$axes==F))
		{
			# Handle x axis
			if(!(!is.null(list(...)$xaxt) && list(...)$xaxt=='n'))
			{
				if(log==T || grepl('x',log,fixed=T))
				{
					if(is.null(logicle.params))
					{
						# This way we can provide defaults but override some things like 'lwd'
						axes.args <- list(axisNum=1, las=las[1], cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1), lwd=1)
						axes.args <- merge.lists(list(...), axes.args)
						do.call(drawLogicleAxis, axes.args)
						# drawLogicleAxis(axisNum=1, las=las[1], cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1), ...)
					}
					else
					{
						# This way we can provide defaults but override some things like 'lwd'
						axes.args <- list(axisNum=1, transition=logicle.params2$transX, tickSep=logicle.params2$tickSepX, base=logicle.params2$base, las=las[1], cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1), lwd=1)
						axes.args <- merge.lists(list(...), axes.args)
						do.call(drawLogicleAxis, axes.args)
						# drawLogicleAxis(axisNum=1, transition=logicle.params$transX, tickSep=logicle.params$tickSepX, base=logicle.params$base, las=las[1], cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1), ...)
					}
				}
				else
				{
					if(trans.logit[1])
					{
						# This way we can provide defaults but override some things like 'lwd'
						axes.args <- list(axisNum=1, las=las[1], cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1), lwd=1)
						axes.args <- merge.lists(list(...), axes.args)
						do.call(drawLogitAxis, axes.args)
						# drawLogitAxis(axisNum=1, las=las[1], cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1), ...)
					}
					else
					{
						# This way we can provide defaults but override some things like 'lwd'
						axes.args <- list(side=1, las=las[1], cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1), lwd=1)
						axes.args <- merge.lists(list(...), axes.args)
						do.call(axis, axes.args)
						# axis(side=1, las=las[1], cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1), ...)
					}
				}
			}
			
			# Handle y-axis
			# if(!(!is.null(list(...)$xaxt) && list(...)$yaxt=='n'))
			# {
			# 	# This way we can provide defaults but override some things like 'lwd'
			# 	axes.args <- list(side=2, las=las[2], cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1), lwd=1)
			# 	axes.args <- merge.lists(list(...), axes.args)
			# 	do.call(axis, axes.args)
			# 	# axis(side=2, las=las[2], cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1), ...)
			# }
			if(!(!is.null(list(...)$yaxt) && list(...)$yaxt=='n'))
			{
				if(grepl('y',log,fixed=T))
				{
					if(is.null(logicle.params))
					{
						# This way we can provide defaults but override some things like 'lwd'
						axes.args <- list(axisNum=2, las=las[1], cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1), lwd=1)
						axes.args <- merge.lists(list(...), axes.args)
						do.call(drawLogicleAxis, axes.args)
						# drawLogicleAxis(axisNum=1, las=las[1], cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1), ...)
					}
					else
					{
						# This way we can provide defaults but override some things like 'lwd'
						axes.args <- list(axisNum=2, transition=logicle.params3$transY, tickSep=logicle.params3$tickSepY, base=logicle.params3$base, las=las[1], cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1), lwd=1)
						axes.args <- merge.lists(list(...), axes.args)
						do.call(drawLogicleAxis, axes.args)
						# drawLogicleAxis(axisNum=1, transition=logicle.params$transX, tickSep=logicle.params$tickSepX, base=logicle.params$base, las=las[1], cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1), ...)
					}
				}
				else
				{
					if(trans.logit[1])
					{
						# This way we can provide defaults but override some things like 'lwd'
						axes.args <- list(axisNum=2, las=las[1], cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1), lwd=1)
						axes.args <- merge.lists(list(...), axes.args)
						do.call(drawLogitAxis, axes.args)
						# drawLogitAxis(axisNum=1, las=las[1], cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1), ...)
					}
					else
					{
						# This way we can provide defaults but override some things like 'lwd'
						axes.args <- list(side=2, las=las[1], cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1), lwd=1)
						axes.args <- merge.lists(list(...), axes.args)
						do.call(axis, axes.args)
						# axis(side=1, las=las[1], cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1), ...)
					}
				}
			}
		}
	}
	
	# Make the legend
	if(legend.plot & !is.null(by))
	{
		if(sample.size > 0)
		{
			legend.colors <- legend.colors[line.color.by.index %in% my.sample(1:max(line.color.by.index), min(sample.size, max(line.color.by.index)), seed=sample.seed)]
		}
		final.legend.colors <- getUniqueCombos(legend.colors, idCols=c(line.color.by, 'my.color', 'names', 'lty', 'lwd'))
		if(type[1] == 'p' || type[1] == 'c')
		{
			legend.args$legend <- final.legend.colors$names
			legend.args$col <- getDefault(pch.outline, rgb(0,0,0,0))
			legend.args$pt.bg <- final.legend.colors$my.color
			legend.args$pch <- getDefault(legend.args$pch, 21)
			do.call(legend, legend.args)
		}
		else
		{
			legend.args$legend <- final.legend.colors$names
			if(type[1] == 'd' || type == 'h')
			{
				legend.args$col <- removeAlpha(final.legend.colors$my.color)
			}
			else
			{
				legend.args$col <- final.legend.colors$my.color
			}
			legend.args$lwd <- final.legend.colors$lwd
			legend.args$lty <- final.legend.colors$lty
			do.call(legend, legend.args)
		}
	}
	
	if(length(polygons) > 0)
	{
		for(polygon in polygons)
		{
			if(!is.null(logicle.params))
			{
				logicle.params2 <- fillDefaultLogicleParams(x=data[[xcol]], y=data[[ycol]], logicle.params=logicle.params)
				
				if(grepl('x',log,fixed=T))
				{
					polygon$x <- logicle(polygon$x, transition=logicle.params2$transX, tickSep=logicle.params2$tickSepX, base=logicle.params2$base, neg.rm=F)
				}
				if(grepl('y',log,fixed=T))
				{
					polygon$y <- logicle(polygon$y, transition=logicle.params2$transY, tickSep=logicle.params2$tickSepY, base=logicle.params2$base, neg.rm=F)
				}
			}
			plot(polygon, lwd=2, border='red', add=T)
		}
	}
	
	if(!is.null(mtext.args))
	{
		if(!is.null(mtext.text))
		{
			mtext.args$text <- mtext.text
		}
		do.call(mtext, mtext.args)
	}
	
	if(!is.null(save.file))
	{
		if(embedTheFont)
		{
			dev.off2(save.file)
		}
		else
		{
			dev.off()
		}
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
data.table.lines <- function(x, y, log='', logicle.params=NULL, h=NULL, h.col='red', h.lty=1, h.lwd=2, v=NULL, v.col='red', v.lty=1, v.lwd=2, spline.smooth=F, spline.spar=0.2, spline.n=.Machine$integer.max, ...)
{
	if(length(which(is.finite(x))) > 0 && length(which(is.finite(y))) > 0)
	{
		l(x1, y1) %=% get.logicle(x=copy(x), y=copy(y), log=log, logicle.params=logicle.params)
		x1 <- x1[is.finite(x1) & is.finite(y1)]
		y1 <- y1[is.finite(x1) & is.finite(y1)]
		if(spline.smooth)
		{
			sp <- smooth.spline(x=x1, y=y1, spar=spline.spar)
			temp <- predict(sp, seq(min(x1, na.rm=T), max(x1, na.rm=T), length.out=min(c(length(x1), spline.n))))
			lines(temp$x, temp$y, ...)
		}
		else
		{
			lines(x=x1, y=y1, ...)
		}
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
data.table.error.bar <- function(x, y, upper=NULL, lower=upper, env.err=F, env.color=rgb(0,0,0,0.2), env.args=list(env.alpha=0.5, env.smooth=F, env.spar=0.2), length=0.1, draw.lower=TRUE, log='', plot.logicle=F, logicle.params, spline.smooth=F, spline.spar=0.2, spline.n=length(x), ...)
{
	if(length(which(is.finite(x))) > 0)
	{
		l(x1, y1) %=% get.logicle(x=copy(x), y=copy(y), log=log, logicle.params=logicle.params)
		x1 <- x1[is.finite(x1) & is.finite(y1)]
		y1 <- y1[is.finite(x1) & is.finite(y1)]
		if(spline.smooth)
		{
			sp <- smooth.spline(x=x1, y=y1, spar=spline.spar)
			x1 <- seq(min(x1, na.rm=T), max(x1, na.rm=T), length.out=min(c(length(x1), spline.n)))
			y1 <- predict(sp, x1)$y
		}
		if(!is.null(upper))
		{
			l(xUpper, yUpper) %=% get.logicle(x=copy(x), y=copy(y+upper), log=log, logicle.params=logicle.params)
			# This should occur in call to error.bar
			# if(env.args$env.smooth)
			# {
			# 	sp.upper <- smooth.spline(x=xUpper, y=yUpper, spar=env.args$env.spar)
			# 	yUpper <- predict(sp, seq(min(xUpper, na.rm=T), max(xUpper, na.rm=T), length.out=spline.n))$y
			# }
			upper1 <- yUpper-y1
		}
		else
		{
			upper1 <- NULL
		}
		if(!is.null(lower))
		{
			l(xLower, yLower) %=% get.logicle(x=copy(x), y=copy(y-lower), log=log, logicle.params=logicle.params)
			# This should occur in call to error.bar
			# if(env.args$env.smooth)
			# {
			# 	sp.lower <- smooth.spline(x=xLower, y=yLower, spar=spline.spar)
			# 	yLower <- predict(sp, seq(min(xLower, na.rm=T), max(xLower, na.rm=T), length.out=spline.n))$y
			# }
			lower1 <- y1-yLower
		}
		else
		{
			lower1 <- NULL
		}
		
		error.bar(x=x1, y=y1, upper=upper1, lower=lower1, env.err=env.err, env.color=env.color, length=length, draw.lower=draw.lower, env.args=env.args, ...)
		print('Added error bars to a plot')
	}
}

# The optional args '...' are passed to corr.test in the psych package
pairwise.cor.test.internal <- function(x, id.cols=c(), ...)
{
	library('psych')
	measurement.cols <- getAllColNamesExcept(x, id.cols)
	temp <- x[, ..measurement.cols]
	return(corr.test(temp))
}

getAllColNamesExcept <- function(x, names)
{
  return(names(x)[!(names(x) %in% names)])
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
	library('psych')
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

data.table.test <- function(x, val.col, compare.by, pair.by=NULL, for.each=NULL, test=c('wilcox','t.test'), p.adjust.method='BH', ...)
{
	my.test.1 <- function(x, y, test=c('wilcox','t.test'), ...)
	{
		ret1 <- NA
		if(test[1]=='wilcox')
		{
			tryCatch(ret1 <- wilcox.test(x, y, ...)$p.value, error=function(e){ret1 <- NA})
		}
		else
		{
			tryCatch(ret1 <- t.test(x, y, ...)$p.value, error=function(e){ret1 <- NA})
		}
		return(ret1)
	}
	
	my.test.2 <- function(dt1, dt2, val.col, pair.by=NULL, test, ...)
	{
		if(is.null(pair.by))
		{
			p.value <- my.test.1(dt1[[val.col]], dt2[[val.col]], test=test, paired=F, ...)
			
			if(is.na(p.value) || is.nan(p.value))
			{
				warning('NaN p-value found')
			}
			n1 <- nrow(dt1)
			n2 <- nrow(dt2)
		}
		else
		{
			# pair.by represents the replicates that should be matched
			setkeyv(dt1, pair.by)
			setkeyv(dt2, pair.by)
			subDt1 <- merge(dt1, dt2[, ..pair.by])
			subDt2 <- merge(dt2, dt1[, ..pair.by])
			# print(subDt1[])
			# print(subDt2[])
			p.value <- my.test.1(dt1[[val.col]], dt2[[val.col]], test=test, paired=T, ...)
			
			n1 <- nrow(subDt1)
			n2 <- NA
		}
		# print(list(p.value=p.value, n1=n1, n2=n2))
		return(list(p.value=p.value, n1=n1, n2=n2))
	}
	
	my.test.3 <- function(dt, val.col, compare.by, pair.by=NULL, test, ...)
	{
		if(!(val.col %in% names(dt)))
		{
			stop('The value column does not exist in the data.table provided. Aborting.')
		}
		dt <- copy(dt)
		dt.n <- dt[, list(n=.N), by=compare.by]
		dt.n <- dt.n[n > 1]
		setkeyv(dt, compare.by)
		setkeyv(dt.n, compare.by)
		dt <- dt[dt.n, nomatch=0]
		paste.cols(dt, cols=compare.by, name='splitNames', sep='.')
		if(nrow(dt)==0)
		{
			# None of the groups had enough samples
			return(NULL)
		}
		if(length(unique(dt$splitNames)) <= 1)
		{
			# There weren't enough groups to compare
			return(data.table(V1=unique(dt$splitNames), V2='', p.value=NA, n1=nrow(dt), n2=as.integer(0), p.value.adj=NA))
		}
		splits <- as.data.table(t(combn(uniqueo(dt$splitNames),2)))
		splits[, c('p.value','n1','n2'):=my.test.2(dt1=dt[splitNames==.BY[[1]] & is.finite(get(val.col))], dt2=dt[splitNames==.BY[[2]] & is.finite(get(val.col))], val.col=val.col, test=test, pair.by=pair.by, ...), by=.(V1,V2)]
		splits[p.value >= 0, p.value.adj:=p.adjust(p.value, method=p.adjust.method)]
		##print(splits[])
		return(splits)
	}
	
	x <- copy(x)
	setorderv(x, c(for.each, compare.by))
	ret <- x[, my.test.3(dt=.SD, val.col=val.col, compare.by=compare.by, pair.by=pair.by, test=test, ...), by=for.each]
	ret[, ':='(V1=factor(V1, levels=uniqueo(x[[compare.by]])), V2=factor(V2, levels=uniqueo(x[[compare.by]])))]
	setorderv(ret, c(for.each, 'V1', 'V2'))
	setnames(ret, c('V1','V2'), paste(compare.by, 1:2, sep='.'))
	return(ret[])
}

file.open <- function(path)
{
	shell(path)
}

#' This function is borrowed from http://www.dr-spiess.de/scripts/bigcor.R (by A.N. Spiess)
#'
#' Use convert to convert the output from a hard disk matrix to a RAM matrix
bigcor <- function(x, y=NULL, fun=c("cor","cov"), size=2000, verbose=TRUE, convert=T, ...)
{
	library('ff')
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

my.test <- function(x, y, test=c('wilcox','t.test'), adjust.p=F, adjust.method='BH', adjust.n=1, ...)
{
	if(all(is.null(x)) | all(is.na(x)) | length(x) == 0)
	{
		return(data.table(p=NA))
	}
	if(all(is.null(y)) | all(is.na(y)) | length(y) == 0)
	{
		return()
	}
	if(test[1]=='wilcox')
	{
		ret1 <- wilcox.test(x, y, ...)$p.value
	}
	else
	{
		ret1 <- t.test(x, y, ...)$p.value
	}
	if(adjust.p)
	{
		ret2 <- p.adjust(ret, method=adjust.method, n=adjust.n)
		return(list(p=ret1, p.adj=ret2))
	}
	return(list(p=ret1))
}

getWilcoxStatForEachGroup <- function(dt, valCol, by, ...)
{
	dt2 <- copy(dt)
	ret <- dt2[, getWilcoxStats(get(valCol),dt[[valCol]], ...), by=by]
	return(ret)
}

getWilcoxStats <- function(x, y, ...)
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
	sigma <- sqrt((n.x * n.y / 12) * (n.x + n.y + 1))
	effect.size <- z/sigma
	z <- z/SIGMA
	
	
	p1 <- 2*pnorm(-abs(z))
	
	p.approx <- 2*pnorm(-abs(z))
	
	return(list(W=W, p.value=temp$p.value, N=N, median.x=median(x), median.y=median(y), n.x=n.x, n.y=n.y, E=n.x * n.y / 2, V=SIGMA^2, z.score=z, effect.size=effect.size, p.value.approx=p.approx))
}

getCombnEffectSize_ <- function(dt, valCol, combn.by, rank.biserial)
{
	dt2 <- copy(dt)
	dt2[, splitNames:=paste(mget(combn.by), collapse='.'), by=combn.by]
	splits <- as.data.table(t(combn(uniqueo(dt2$splitNames),2)))
	splits[, ':='(effect.size=getEffectSize(x1=dt2[splitNames==V1][[valCol]], x2=dt2[splitNames==V2][[valCol]], rank.biserial=rank.biserial), N1=nrow(dt2[splitNames==V1]), N2=nrow(dt2[splitNames==V2])), by=.(V1,V2)]
	return(splits)
}

getCombnEffectSize <- function(dt, valCol, group.by, combn.by, rank.biserial=F)
{
	return(dt[, getCombnEffectSize_(.SD, valCol=valCol, combn.by=combn.by, rank.biserial=rank.biserial), by=group.by][])
}

#' Returns either the Hedge's g (like Cohen's d for unequal variance and sample size)
#' or the rank biseral coefficien (-1 to +1)
#'
getEffectSize <- function(x1, x2, rank.biserial=F)
{
	if(rank.biserial)
	{
		# http://core.ecu.edu/psyc/wuenschk/docs30/Nonparametric-EffectSize.pdf
		# Kirby (2014)
		# 1-2*U/(n1*n2) Rank Biserial Correlation
		# Result is 0-1 being the chance that a random sample from one sample will exceed that of a sample from the other population
		U <- wilcox.test(x1, x2)$statistic # W and U are same for R
		return(c(rb=as.numeric(1-2*U/(length(x1)*length(x2)))))
	}
	else
	{
		# https://en.wikipedia.org/wiki/Effect_size#Hedges'_g
		# Hedge's g
		s.star.num <- ((length(x1)-1)*sd(x1)^2)+((length(x2)-1)*sd(x2)^2)
		s.star.den <- length(x1) + length(x2) - 2
		s.star <- s.star.num/s.star.den
		g <- (mean(x2)-mean(x1))/s.star
		return(c(g=g))
	}
}

# This is for calculating the p-value by combining multiple experiments
wilcox.test.combined <- function(data, replCols, condCol, valCol, exact=NULL, two.tailed=TRUE)
{
	library(data.table)
	x1 <- data.table(data)
	
	
	
	conds <- unique(x1[[condCol]])
	if(length(conds) != 2)
	{
		stop("Must have 2 and only 2 conditions to compare.")
	}
	if(conds[1] < conds[2])
	{
		conds <- rev(conds)
	}
	x2 <- x1[,getWilcoxStats(x=.SD[get(condCol)==conds[1]][[valCol]], y=.SD[get(condCol)==conds[2]][[valCol]], exact=exact), by=replCols]
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

getPSymbol <- function(pval, not.sig='')
{
	if(is.list(pval))
	{
		collapseSymbols <- function(symbols)
		{
			if(length(symbols > 1))
			{
				symbols[symbols==''] <- '—'
			}
			return(paste(symbols, collapse=','))
		}
		ret <- as.character(lapply(lapply(pval, FUN=getPSymbol), collapseSymbols))
		return(ret)
	}
	ret <- rep(not.sig, length(pval))
	ret[pval <= 0.05] <- '*'
	ret[pval <= 0.01] <- '**'
	ret[pval <= 0.001] <- '***'
	ret[pval <= 0.0001] <- '****'
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
	return(getNSplit(x=x, n=1))
}

getNSplit <- function(x, n=1)
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

getEnvelope <- function(x, y, upper, lower=upper, logicle.params=NULL)
{
	# if(length(x) != length(y) | (length(y) != length(lower) | length(lower) != length(upper))
	#      stop("vectors must be same length")
	logicle.params <- fillDefaultLogicleParams(x, y, logicle.params)
	if(!is.null(logicle.params))
	{
		tempX <- logicle(x, transition=logicle.params$transX)
		tempY <- logicle(y, transition=logicle.params$transY)
		tempYUpper <- logicle(y+upper, transition=logicle.params$transY)
		tempYLower <- logicle(y-lower, transition=logicle.params$transY)
	}
	else
	{
		tempX <- x
		tempY <- y
		tempYUpper <- y + upper
		tempYLower <- y - lower
	}
	return(list(x=tempX, y=tempY, y.upper=tempYUpper, y.lower=tempYLower))
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
error.bar <- function(x, y, upper=NULL, lower=upper, length=0.1, draw.upper=TRUE, draw.lower=TRUE, logicle.params=NULL, env.err=F, env.color=rgb(0,0,0,0.2), env.args=list(env.alpha=0.5, env.smooth=F, env.spar=0.2), ...)
{
	if(is.null(upper))
	{
		upper <- lower
	}
	l(tempX, tempY, tempYUpper, tempYLower) %=% getEnvelope(x=x, y=y, upper=upper, lower=lower, logicle.params=logicle.params)
	
	if(env.err)
	{
		if(env.args$env.smooth)
		{
			l(sx, sy) %=% spline.envelope(tempX, tempYUpper, tempYLower, env.args=env.args, ...)
			polygon(sx, sy, col = env.color, border = NA)
		}
		else
		{
			polygon(c(rev(tempX), tempX), c(rev(tempYLower), tempYUpper), col = env.color, border = NA)
		}
	}
	else
	{
		if(draw.lower)
		{
			suppressWarnings(arrows(tempX, tempYLower, tempX, tempY, angle=90, code=1, length=length, ...))
		}
		if(draw.upper)
		{
			suppressWarnings(arrows(tempX, tempYUpper, tempX, tempY, angle=90, code=1, length=length, ...))
		}
	}
}

#
# Splining a polygon.
#
#   The rows of 'xy' give coordinates of the boundary vertices, in order.
#   'vertices' is the number of spline vertices to create.
#              (Not all are used: some are clipped from the ends.)
#   'k' is the number of points to wrap around the ends to obtain
#       a smooth periodic spline.
#
#   Returns an list containing the x and y points.
#
spline.poly <- function(x, y, vertices=10*length(x), k=3, ...) {
	
	if(length(x) != length(y))
	{
		stop("x and y must be the same length. Aborting spline.poly.")
	}
	if(length(x) < k)
	{
		stop("length of vectors must be >= k for wrapping the ends of the spline around the polygon.")
	}
	# Assert: xy is an n by 2 matrix with n >= k.
	xy <- matrix(c(x, y), ncol=2)
	
	# Wrap k vertices around each end.
	n <- dim(xy)[1]
	if (k >= 1) {
		data <- rbind(xy[(n-k+1):n,], xy, xy[1:k, ])
	} else {
		data <- xy
	}
	
	# Spline the x and y coordinates.
	data.spline <- spline(1:(n+2*k), data[,1], n=vertices, ...)
	x <- data.spline$x
	x1 <- data.spline$y
	x2 <- spline(1:(n+2*k), data[,2], n=vertices, ...)$y
	
	# Retain only the middle part.
	return(list(x=x1[k < x & x <= n+k], y=x2[k < x & x <= n+k]))
}

#
# Splining an envelope.
#
#   'x' and 'yupper' and 'ylower' give coordinates of the boundary vertices, in order.
#   'vertices' is the number of spline vertices to create.
#
#   Returns an list containing the x and y points.
#
spline.envelope <- function(x, yupper, ylower, env.args=list(env.smooth=F, env.spar=0.2), ...) {
	
	if((length(x) != length(yupper)) | (length(x) != length(ylower)))
	{
		stop("x and yupper and ylower must be the same length. Aborting spline.poly.")
	}
	
	# Spline the x and y coordinates.
	if(env.args$env.smooth)
	{
		data.spline.upper <- smooth.spline(x[is.finite(x) & is.finite(yupper)], yupper[is.finite(x) & is.finite(yupper)], spar=env.args$env.spar, ...)
		data.spline.lower <- smooth.spline(x[is.finite(x) & is.finite(ylower)], ylower[is.finite(x) & is.finite(ylower)], spar=env.args$env.spar, ...)
	}
	else
	{
		data.spline.upper <- spline(x[is.finite(x) & is.finite(yupper)], yupper[is.finite(x) & is.finite(yupper)], ...)
		data.spline.lower <- spline(x[is.finite(x) & is.finite(ylower)], ylower[is.finite(x) & is.finite(ylower)], ...)
	}
	
	# Retain only the middle part.
	return(list(x=c(rev(data.spline.upper$x),data.spline.lower$x), y=c(rev(data.spline.upper$y), data.spline.lower$y)))
}

unfactorizeNumerics <- function(x)
{
	library(varhandle)
	lapply.data.table(x, unfactor, col.filter = function(x){ if(is.factor(x)){ return(all(check.numeric(x))) }else{ return(F) } }, in.place = T)
}

#' Get a JEXData object from a well in a dataset in a database
#' Use a list() for labels to assign label values to all items
#' in the object.
#'
readJEXData <- function(dbPath, ds=NULL, e.x=NULL, e.y=NULL, type=NULL, name=NULL, labels=list())
{
	library(foreign)
	library(data.table)
	
	ret <- list()
	labelNames <- names(labels)
	labelValues <- as.vector(vapply(labels, as.character, ''))
	if(!is.null(labels) & !is.null(labelNames) & !is.null(labelValues) & length(labels) > 0)
	{
		for(i in 1:length(labels))
		{
			key <- labelNames[i]
			val <- labelValues[i]
			ret[[key]] <- val
		}
	}
	
	if(!is.null(ds))
	{
		ret$ds <- ds
		ret$e.x <- e.x
		ret$e.y <- e.y
		ret$type <- type
		ret$name <- name
		ret$dbPath <- dbPath
		ret$tmpPath <- file.path(dbPath,'temp','RScriptTempFolder')
		ret$jxdDir <- file.path(dbPath, ds, paste0('Cell_x',e.x,'_y',e.y), paste0(type,'-',name))
		ret$jxdFilePath <- file.path(ret$jxdDir, paste0('x',e.x,'_y',e.y,'.jxd'))
		temp <- list()
		if(file.exists(ret$jxdFilePath))
		{
			if(type=='Roi')
			{
				ret$roi <- list(readJEXMaxima(ret$jxdFilePath))
				ret <- as.data.table(ret)
				return(ret)
			}
			else
			{
				temp <- as.list(read.arff(ret$jxdFilePath))
				if(type == 'File' | type == 'Movie' | type == 'Image' | type == 'Roi' | type == 'Workflow')
				{
					temp$fileList <- file.path(ret$db,read.arff(ret$jxdFilePath)$Value)
				}
				temp$fileList <- gsub('\\\\','/',temp$fileList,fixed=T)
				ret <- as.data.table(c(ret, temp))
				unfactorizeNumerics(ret)
				return(ret)
			}
		}
		else
		{
			warning(paste('Could not find the specified file:', ret$jxdFilePath))
			ret <- as.data.table(ret)
		}
		return(ret)
	}
	else
	{
		ret$fileList <- dbPath
		return(ret)
	}
}

filterTableWithIdsFromAnotherTable <- function(x, filterTable, idCols)
{
	setkeyv(x, idCols)
	setkeyv(filterTable, idCols)
	return(x[unique(filterTable, by=idCols)[, idCols, with=F], nomatch=0])
}

#' sample.size is how many will try to be samples PER FILE.

readJEXDataTables <- function(jData, sample.size=-1, sampling.order.fun=NULL, samples.to.match.and.append=NULL, time.col=NULL, times=NULL, time.completeness=0, cellIdString=c('Id'), lines.without=NULL, lines.with=NULL, header=T, order.all.cols=T, ...)
{
	xList <- list()
	count <- 1;
	time.col.orig <- time.col
	if(is.null(time.col))
	{
		time.col <- c('T','Time','t','time','Frame','frame')[c('T','Time','t','time','Frame','frame') %in% names(jData)][1]
		if(is.na(time.col))
		{
			time.col <- NULL
		}
	}
	
	makeComplexId(jData, c('ds','e.x','e.y'))
	# For each entry
	imageDimsRet <- character(0)
	labelDimsRet <- character(0)
	for(tempId in uniqueo(jData$cId))
	{
		if(!is.null(jData[cId==tempId]$Valid) && jData[cId==tempId]$Valid[1]=='true')
		{
			dtList <- list()
			k <- 1
			for(daFile in jData[cId==tempId]$fileList)
			{
				if(!is.null(times) && !(jData[cId==tempId & fileList==daFile][[time.col]][1] %in% times))
				{
					print(paste('Skipping', daFile))
					next
				}
				print(paste('Reading', daFile))
				
				# Read in data
				if(endsWith(daFile,'.arff'))
				{
					dtList[[k]] <- data.table(read.arff(daFile))
				}
				else
				{
					words <- paste(lines.with, collapse="|")
					words <- gsub('$', "\\$", words, fixed=T)
					if(!is.null(lines.with))
					{
						dtList[[k]] <- fread(cmd=paste("grep -E \"", words, "\" \'", daFile, "\'", sep=""), header=T)
					}
					else if(!is.null(lines.without))
					{
						dtList[[k]] <- fread(cmd=paste("grep -v -E \"", words, "\" \'", daFile, "\'", sep=""), header=T)
					}
					else
					{
						dtList[[k]] <- fread(daFile, header=header)
					}
				}
				k <- k + 1
			}
			temp <- rbindlist(dtList, use.names = T, fill=T)
			if(nrow(temp)==0)
			{
				# Then no files fit the filter criteria
				next
			}
			
			if(is.null(time.col))
			{
				# Then the time column might be contained within the object (or not)
				if(!is.null(time.col.orig))
				{
					# Try to get from within the object
					time.col <- time.col.orig[time.col.orig %in% names(temp)][1]
				}
			}
			imageDims <- getAllColNamesExcept(temp, c(time.col, cellIdString,'Label','MaskChannel', 'ImageChannel', 'Measurement', 'Value'))
			imageDimsRet <- merge.vectors(imageDimsRet, imageDims)
			labelDims <- names(jData)[!(names(jData) %in% names(temp)) & !(names(jData) %in% c('ds','e.x','e.y','cId','type','name','dbPath','tmpPath','jxdDir','jxdFilePath','Metadata','Value','fileList'))]
			labelDimsRet <- merge.vectors(labelDimsRet, labelDims)
			
			temp[, c(labelDims, 'ds', 'e.x', 'e.y'):=jData[fileList==daFile, c(labelDims, 'ds', 'e.x', 'e.y'), with=F]]
			colsToOrder <- c('ds','e.x','e.y', labelDims, imageDims, time.col, cellIdString, 'Label', 'cId', 'MaskChannel', 'ImageChannel', 'Measurement', 'Value')
			setcolorder(temp, colsToOrder[colsToOrder %in% names(temp)])
			
			# If we have an existing table that we are supposed to find matching data for
			# then use that to determine sampling and append the new data.
			if(!is.null(samples.to.match.and.append))
			{
				# Get the uniqueIds of samples.to.match.and.append to match against, setting the keys for matching
				uniques.to.match <- c('ds','e.x','e.y',names(samples.to.match.and.append)[(names(samples.to.match.and.append) %in% c(cellIdString,imageDims))])
				setkeyv(samples.to.match.and.append, uniques.to.match)
				
				uniques <- c('ds','e.x','e.y',names(temp)[(names(temp) %in% c(cellIdString,imageDims))])
				if(!all(uniques %in% uniques.to.match) || !all(uniques.to.match %in% uniques))
				{
					print(paste0('Uniques to match: ', uniques.to.match))
					print(paste0('Uniques read: ', uniques))
					stop("The unique id cols in new data and the data to match up with need to match")
				}
				
				temp <- filterTableWithIdsFromAnotherTable(temp, samples.to.match.and.append, uniques.to.match)
				xList[[count]] <- temp
			}
			else
			{
				# We should sample according to other arguments.
				uniques <- names(temp)[(names(temp) %in% c(cellIdString,imageDims))]
				if(length(uniques) > 0)
				{
					setkeyv(temp, uniques)
				}
				if(!is.null(time.col))
				{
					if(time.col %in% names(temp))
					{
						nMin <- time.completeness*length(unique(temp[[time.col]]))
						if(time.completeness < 0 | time.completeness > 1)
						{
							stop("time.completeness parameter must be > 0 and <= 1 as it represents the minimum fraction of the timelapse for which as cell must have data in order to be kept.")
						}
						nTimes <- temp[, list(N=length(unique(get(time.col)))), by=uniques]
						nTimes <- nTimes[N >= nMin]
						nTimes[, N:=NULL]
						temp <- temp[nTimes]
						if(nrow(temp)==0)
						{
							warning("The data table did not contain data that had a sufficient number of timepoints. Warning.")
						}
					}
					else
					{
						stop("Couldn't find the specified time column in the data table being read. Aborting.")
					}
				}
				if(sample.size > 0)
				{
					uniqueIds <- unique(temp, by=uniques)[, uniques, with=F]
					actual.sample.size <- min(nrow(uniqueIds),sample.size)
					if(actual.sample.size < sample.size)
					{
						if(actual.sample.size == 0)
						{
							temp <- temp[F]
							warning("There weren't any samples to sample from. Returning an empty table. Warning.")
						}
						else
						{
							sampledIds <- uniqueIds[sample(nrow(uniqueIds), actual.sample.size)]
							temp <- temp[sampledIds]
							warning("The number of samples was less than the specified sample size. Returning all samples. Warning.")
						}
					}
					else
					{
						# Then sample.size == actual.sample.size
						if(!is.null(sampling.order.fun))
						{
							# Order the list and sample from the top of the list
							orderedUniqueIds <- sampling.order.fun(temp, idCols=c(cellIdString,imageDims), sample.size=actual.sample.size, ...)
							sampledIds <- orderedUniqueIds
						}
						else
						{
							# Take a random sampling of the list
							sampledIds <- uniqueIds[sample(nrow(uniqueIds), actual.sample.size)]
						}
						temp <- temp[sampledIds]
					}
					xList[[count]] <- temp
				}
				else
				{
					xList[[count]] <- temp
				}
			}
		}
		else
		{
			print(paste('Skipping', tempId, "because it was marked as not 'valid'"))
		}
		count = count + 1
	}
	
	# rbind the read in data
	ret <- rbindlist(xList, use.names=T, fill=T)
	
	if(!is.null(samples.to.match.and.append))
	{
		# Then append the read data to the sample data.
		ret <- rbindlist(list(a=ret, b=samples.to.match.and.append), use.names=T)
	}
	
	if(any(!(c(cellIdString, imageDimsRet) %in% names(ret))))
	{
		warning(paste0("The ID columns provided or used by default do not match column names. Particularly '", paste(idCols[!(c(cellIdString,imageDims) %in% names(ret))], collapse=','), "'. The complex ID could not be made."))
		return(ret)
	}
	idColsRet <- merge.vectors(c('ds','e.x','e.y'),c(cellIdString, imageDimsRet))
	makeComplexId(ret, cols=idColsRet)
	
	colsToOrder <- c('ds','e.x','e.y', labelDimsRet, imageDimsRet, time.col, cellIdString, 'Label', 'cId', 'MaskChannel', 'ImageChannel', 'Measurement', 'Value')
	setcolorder(ret, colsToOrder[colsToOrder %in% names(ret)])
	
	return(list(x=ret, time.col=time.col, idCols=idColsRet, imageDims=imageDimsRet, labelDims=labelDimsRet))
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

paste.mget <- function(mgetList, sep='', collapse=NULL)
{
	args <- c(mgetList, sep=sep, collapse=collapse)
	return(do.call(paste, args))
}

#' Paste together information from columns
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
paste.cols <- function(x, cols, name, sep='')
{
	library(data.table)
	if(!is.data.table(x))
	{
		stop('This function requires a data.table')
	}
	x[, c(name):=do.call(paste, c(.SD, sep=sep)), .SDcols=cols]
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
	
	if(!is.null(measurementCols))
	{
		# Parse commas to indicate that the string should be split into multiple items.
		measurementCols <- strsplit(measurementCols, ',', fixed=T)
		
		# Leading and trailing spaces are not good so remove them in case (makes it easier for JEX)
		measurementCols <- mapply(gsub, '^\\s+|\\s+$', '', measurementCols)
	}
	else
	{
		measurementCols <- names(data)[!(names(data) %in% c(idCols, valueCols))]
	}
	
	# If idCols = NULL, then use all remaining cols except measurementCols and valueCols
	if(is.null(idCols))
	{
		idCols <- names(data)[!(names(data) %in% c(measurementCols, valueCols))]
	}
	
	if(is.null(valueCols))
	{
		valueCols <- names(data)[!(names(data) %in% c(idCols, measurementCols))]
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
	library(plyr)
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
	library(data.table)
	
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
	library(pracma)
	return(x[numel(x)])
}

getDensityPeaks <- function(x, neighlim, deriv.lim=1, n=NULL, min.h=0.1, density.args=list(), make.plot=F, in.data=T, plot.args=list(), ...)
{
	# min.frac.peak.h is the minimum height of a peak in terms of the fractional range of the data.
	# Use n=-1 to get last peak
	# Use n=c(1,-1) to get first and last peak
	# Use n=NULL to get all peaks
	# Use n=NA to get the location of the mode of the density histogram
	# If in.data==T, then the closest x-location to the peak is returned
	# instead of the x location in the density distribution
	library(peakPick)
	density.args <- merge.lists(list(x=x[is.finite(x)]), density.args)
	blah <- do.call(density, density.args)
	peaks <- peakpick(matrix(blah$y, ncol=1), deriv.lim=deriv.lim, neighlim=neighlim)
	peaks[85]
	p.max <- max(blah$y)
	peaks <- peaks & (blah$y >= min.h*p.max)
	peaksi <- which(peaks)
	ret <- data.table(i=1:length(peaksi), peak.i=peaksi, peak.x=blah$x[peaksi], peak.y=blah$y[peaksi])
	
	# Plot all the peaks
	if(make.plot)
	{
		if(is.null(plot.args$type))
		{
			plot.args$type <- 'l'
		}
		if(is.null(plot.args$xlab))
		{
			plot.args$xlab <- 'x'
		}
		if(is.null(plot.args$ylab))
		{
			plot.args$ylab <- 'Prob. Density'
		}
		plot.args <- merge.lists(plot.args, list(x=blah$x, y=blah$y))
		do.call(plot, plot.args)
		plot.args$x <- ret$peak.x
		plot.args$y <- blah$y[ret$peak.i]
		plot.args <- merge.lists(plot.args, list(type='p'))
		do.call(points, plot.args)
	}
	if(all(is.null(n)))
	{
		# Do nothing, return all
	}
	else if(all(is.na(n)))
	{
		# Just return the max peak
		ret <- ret[peak.y == max(peak.y)]
	}
	else
	{
		# Return the specified maxima
		m <- n
		m[m < 1] <- nrow(ret)
		invalid <- m > nrow(ret)
		if(any(invalid))
		{
			warning(paste('Some peaks were not found...', m[invalid]))
		}
		peaksi <- peaksi[m[!invalid]]
		ret <- data.table(i=1:length(peaksi), peak.n=n[!invalid], peak.m=m[!invalid], peak.i=peaksi, peak.x=blah$x[peaksi], peak.y=blah$y[peaksi])
	}
	
	# Plot chosen peaks
	if(make.plot)
	{
		plot.args$x <- ret$peak.x
		plot.args$y <- blah$y[ret$peak.i]
		plot.args$type <- 'p'
		plot.args$col <- 'red'
		plot.args$pch <- 4
		do.call(points, plot.args)
	}

	if(in.data)
	{
		ret[, peak.i:=which.min(abs(x-peak.x)), by=c('i')]
		ret[, peak.x:=x[peak.i], by=c('i')]
	}
	
	return(ret[])
}

getDensityValleys <- function(x, neighlim, n=c(1,-1), nearestTo=NULL, min.h=0.1, density.args=list(), make.plot=F, in.data=T, plot.args=list(), ...)
{
	# min.frac.peak.h is the minimum height of a peak in terms of the fractional range of the data.
	# Use n=-1 to get last peak
	# If in.data==T, then the closest x-location to the peak is returned
	# instead of the x location in the density distribution
	library(peakPick)
	density.args <- merge.lists(list(x=x), density.args)
	blah <- do.call(density, density.args)
	peaks <- peakpick(matrix(1-blah$y, ncol=1), neighlim=neighlim)
	p.max <- max(blah$y)
	peaks <- peaks & (blah$y >= min.h*p.max)
	peaksi <- which(peaks)
	m <- n
	m[length(m)] <- length(peaksi)
	invalid <- m > length(peaksi)
	if(any(invalid))
	{
		warning(paste('Some peaks were not found...', m[invalid]))
	}
	peaksi <- peaksi[m[!invalid]]
	ret <- data.table(i=1:length(peaksi), peak.n=n[!invalid], peak.m=m[!invalid], peak.i=peaksi, peak.x=blah$x[peaksi])
	if(make.plot)
	{
		if(is.null(plot.args$type))
		{
			plot.args$type <- 'l'
		}
		if(is.null(plot.args$xlab))
		{
			plot.args$xlab <- 'x'
		}
		if(is.null(plot.args$ylab))
		{
			plot.args$ylab <- 'Prob. Density'
		}
		plot.args <- merge.lists(plot.args, list(x=blah$x, y=blah$y))
		do.call(plot, plot.args)
		plot.args$x <- ret$peak.x
		plot.args$y <- blah$y[ret$peak.i]
		plot.args <- merge.lists(plot.args, list(type='p'))
		do.call(points, plot.args)
	}
	if(in.data)
	{
		ret[, peak.i:=which.min(abs(x-peak.x)), by=c('i')]
		ret[, peak.x:=x[peak.i], by=c('i')]
	}
	if(!is.null(nearestTo))
	{
		ret <- ret[which.min(abs(peak.x-nearestTo))]
	}
	
	return(ret[])
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

setorderv.alphanum <- function(x, ...)
{
	data <- data[with(data, multi.mixedorder(Day, Conc, Sample.ID, relTimeStamp)), ]
}

multi.mixedorder <- function(..., na.last = TRUE, decreasing = FALSE){
	# For example...
	# data <- data[with(data, multi.mixedorder(Day, Conc, Sample.ID, relTimeStamp)), ]
	library(gtools)
	do.call(order, c(
		lapply(rev(list(...)), function(l){
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

merge.vectors <- function(vector1, vector2)
{
	ret <- c(vector1, vector2)
	ret <- unique(ret)
	return(ret)
}

source_https <- function(url, ...)
{
	# load package
	library(RCurl)
	
	# parse and evaluate each .R script
	sapply(c(url, ...), function(u)
	{
		eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
	})
}

sourceGitHubFile <- function(user, repo, branch, file)
{
	library(curl)
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
calcTransition <- function(x, base=10, frac=0.15)
{
	if(!any(is.finite(x)) || !any(x > 0))
	{
		return(base)
	}
	else
	{
		temp <- range(log(x[x > 0], base=base))
		return(base^(frac*(temp[2]-temp[1])+temp[1]))
	}
	# negNums <- c(x[x<0],-1*x[x<=0])
	# if(length(negNums[is.finite(negNums)]) < minN)
	# {
	# 	# Then insufficient n to guess the 'zero' distribution
	# 	# This will suggest using normal log scaling instead of logicle
	# 	return(NULL)
	# }
	# else
	# {
	# 	return(3*mad(negNums, na.rm=T))
	# }
}

get.logicle <- function(x, y, log, logicle.params, neg.rm=T, na.rm=T, trans.logit=c(F,F))
{
	logicle.params <- fillDefaultLogicleParams(x=x, y=y, logicle.params=logicle.params)
	logX <- grepl('x',x=log,fixed=T)
	logY <- grepl('y',x=log,fixed=T)
	
	logicle.params
	
	if(!is.null(logicle.params))
	{
		if(logX & !trans.logit[1])
		{
			x1 <- logicle(x, transition=logicle.params$transX, tickSep=logicle.params$tickSepX, base=logicle.params$base, neg.rm=neg.rm)
		}
		else
		{
			if(trans.logit[1])
			{
				x1 <- logit.transform(x)
			}
			else
			{
				x1 <- x
			}
		}
		
		if(logY & !trans.logit[2])
		{
			y1 <- logicle(y, transition=logicle.params$transY, tickSep=logicle.params$tickSepY, base=logicle.params$base, neg.rm=neg.rm)
		}
		else
		{
			if(trans.logit[2])
			{
				y1 <- logit.transform(y)
			}
			else
			{
				y1 <- y
			}
		}
	}
	else
	{
		# Then, if necessary, remove negative x and y values and warn the user
		if(logX & neg.rm & !trans.logit[1])
		{
			posX <- x > 0
		}
		else
		{
			# Treat all values as 'positive' (i.e., plottable)
			posX <- rep(T, length(x))
		}
		
		if(logY & neg.rm & !trans.logit[2])
		{
			posY <- y > 0
		}
		else
		{
			# Treat all values as 'positive' (i.e., plottable)
			posY <- rep(T, length(y))
		}
		
		# Then, if necessary, remove negative x and y values and warn the user
		if(any(is.na(x[posX])) & na.rm)
		{
			posX <- posX & !is.na(x)
		}
		
		if(any(is.na(y[posY])) & na.rm)
		{
			posY <- posY & !is.na(y)
		}
		
		plottable <- posX & posY
		x1 <- x
		x1[!plottable] <- NA
		y1 <- y
		y1[!plottable] <- NA
		if(sum(posX) < length(x))
		{
			warning('Negative values were removed from the X values (along with corresponding Y-values) due to log scaling.')
		}
		if(sum(posY) < length(y))
		{
			warning('Negative values were removed from the Y values (along with corresponding X-values) due to log scaling.')
		}
		
		if(logX & !trans.logit[1])
		{
			x1 <- log10(x1)
		}
		else
		{
			if(trans.logit[1])
			{
				x1 <- logit.transform(x)
			}
		}
		if(logY & !trans.logit[2])
		{
			y1 <- log10(y1)
		}
		else
		{
			if(trans.logit[2])
			{
				y1 <- logit.transform(y)
			}
		}
	}
	
	return(list(x=x1, y=y1))
}

start.logicle <- function(x, y, log='xy', trans.logit=c(F,F), logicle.params, add=F, mar=par('mar'), percentile.limits=c(0,1,0,1), ...)
{
	# Use the xlim and ylim to determine the logicle params if necessary
	if(!is.null(list(...)$xlim))
	{
		tempX <- list(...)$xlim
	}
	else
	{
		tempX <- x
	}
	if(!is.null(list(...)$ylim))
	{
		tempY <- list(...)$ylim
	}
	else
	{
		tempY <- y
	}
	logicle.params <- fillDefaultLogicleParams(x=tempX, y=tempY, logicle.params=logicle.params)
	
	logX <- grepl('x',x=log,fixed=T)
	logY <- grepl('y',x=log,fixed=T)
	
	# This function works to scale things to logicle scale or for normal log scaling (removing negative values if necessary but leaving things unscaled)
	l(x1, y1) %=% get.logicle(x=x, y=y, log=log, logicle.params=logicle.params, trans.logit=trans.logit, neg.rm=F, na.rm=F)
	
	# Establish limits
	lims <- c(getPercentileValues(x1, levels=percentile.limits[1:2]), getPercentileValues(y1, levels=percentile.limits[3:4]))
	if(!is.null(list(...)$xlim))
	{
		lims[1:2] <- list(...)$xlim
		if(logX | trans.logit[1])
		{
			lims[1:2] <- logicle(x=lims[1:2], transition=logicle.params$transX, tickSep=logicle.params$tickSepX, base=logicle.params$base, neg.rm=F, trans.logit=trans.logit)
		}
	}
	if(!is.null(list(...)$ylim))
	{
		lims[3:4] <- list(...)$ylim
		if(logY | trans.logit[2])
		{
			lims[3:4] <- logicle(x=lims[3:4], transition=logicle.params$transY, tickSep=logicle.params$tickSepY, base=logicle.params$base, neg.rm=F, trans.logit=trans.logit)
		}
	}
	
	# Determine xlim
	# If it regular log scaling, the user might have provided a lower limit <=0 which could cause issues.
	# Check and change if necessary, printing a warning.
	if(is.na(lims[1]))
	{
		lims[1] <- min(x1, na.rm=T) # Which is guaranteed to no not be zero if we remove NA and NaNs
		warning('A 0 or negative limit was provided to the log-scaled x axis. Setting to min of the positive x values.')
	}
	xlim <- lims[1:2]
	
	# Determine ylim
	# If it regular log scaling, the user might have provided a lower limit <=0 which could cause issues.
	# Check and change if necessary, printing a warning.
	if(is.na(lims[3]))
	{
		lims[3] <- min(y1, na.rm=T) # Which is guaranteed to no not be zero based on 'get.logicle'
		warning('A 0 or negative limit was provided to the log-scaled x axis. Setting to min of the positive x values.')
	}
	ylim <- lims[3:4]
	
	if(add)
	{
		return(list(x=x1, y=y1, xlim=lims[1:2], ylim=lims[3:4], logicle.params=logicle.params))
	}
	
	pars.plot <- list(...)
	pars.plot <- merge.lists(pars.plot, list(xlim=xlim, ylim=ylim, x=numeric(0), y=numeric(0), axes=F))
	if(!is.null(mar))
	{
		par(mar=mar)
	}
	do.call(plot, pars.plot)
	box(col='black',lwd=1)
	
	return(list(x=x1, y=y1, xlim=xlim, ylim=ylim, logicle.params=logicle.params))
}

finish.logicle <- function(log, logicle.params, h, h.col, h.lty, h.lwd, v, v.col, v.lty, v.lwd, add=F, trans.logit=c(F,F), las=c(0,0), ...)
{
	#logicle.params <- fillDefaultLogicleParams(x=x, y=y, logicle.params=logicle.params)
	# Determine which axes to transform
	logX <- grepl('x',x=log,fixed=T)
	logY <- grepl('y',x=log,fixed=T)
	
	# las <- 0
	# if(!is.null(list(...)$las))
	# {
	# 	las <- list(...)$las
	# }
	
	cex.axis <- 1
	if(!is.null(list(...)$cex.axis))
	{
		cex.axis <- list(...)$cex.axis
	}
	
	cex.lab <- 1
	if(!is.null(list(...)$cex.lab))
	{
		cex.lab <- list(...)$cex.lab
	}
	
	if(!add && (is.null(list(...)$axes) || list(...)$axes))
	{
		# Draw axes if logicle.params was provided and the particular axis is logicle-scaled
		if(logX == 1)
		{
			drawLogicleAxis(axisNum=1, transition=logicle.params$transX, tickSep=logicle.params$tickSepX, base=logicle.params$base, las=las[1], cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1), ...)
		}
		else
		{
			if(trans.logit[1])
			{
				drawLogitAxis(1, las=las[1], cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1), ...)
			}
			else
			{
				otherParams <- merge.lists(list(side=1, las=las[1], cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1)), list(...))
				otherParams <- merge.lists(otherParams, list(lwd=1))
				do.call(axis, otherParams)
			}
		}
		if(logY == 1)
		{
			drawLogicleAxis(axisNum=2, transition=logicle.params$transY, tickSep=logicle.params$tickSepY, base=logicle.params$base, las=las[1], cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1), ...)
		}
		else
		{
			if(trans.logit[2])
			{
				drawLogitAxis(2, las=las[1], cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1), ...)
			}
			else
			{
				otherParams <- merge.lists(list(side=2, las=las[1], cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1)), list(...))
				otherParams <- merge.lists(otherParams, list(lwd=1))
				do.call(axis, otherParams)
			}
		}
	}
	
	# This is now done within drawLogiclAxis
	# # Plot transition lines if necessary (only applies to logicle-scaled plots)
	# if(!is.null(logicle.params))
	# {
	# 	abline(v=logicle.params$transX, lty=2, col='gray', lwd=1)
	# 	abline(h=logicle.params$transX, lty=2, col='gray', lwd=1)
	# }
	
	# Plot h and v lines
	finishABLine(h=h, h.col=h.col, h.lty=h.lty, h.lwd=h.lwd, v=v, v.col=v.col, v.lty=v.lty, v.lwd=v.lwd, log=log, logicle.params=logicle.params)
}

finishABLine <- function(h=NULL, h.col='black', h.lty=1, h.lwd=2, v=NULL, v.col='black', v.lty=1, v.lwd=2, log='', logicle.params=NULL, trans.logit=c(F,F))
{
	
	#logicle.params <- fillDefaultLogicleParams(x=x, y=y, logicle.params=logicle.params)
	# Determine which axes to transform
	logX <- grepl('x',x=log,fixed=T)
	logY <- grepl('y',x=log,fixed=T)
	
	# Plot h and v lines
	if(!is.null(h))
	{
		if(logY & !is.null(logicle.params))
		{
			abline(h=logicle(h, transition=logicle.params$transY, tickSep=logicle.params$tickSepY, base=logicle.params$base, neg.rm=F, trans.logit=trans.logit[2]), col=h.col, lty=h.lty, lwd=h.lwd)
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
			abline(v=logicle(v, transition=logicle.params$transX, tickSep=logicle.params$tickSepX, base=logicle.params$base, neg.rm=F, trans.logit=trans.logit[1]), col=v.col, lty=v.lty, lwd=v.lwd)
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
	
	if(is.null(x))
	{
		stop('Although y can be NULL, x is not supposed to be NULL here. Aborting')
	}
	
	logicle.params$base <- getDefault(logicle.params$base, 10)
	
	# First set frac if needed
	logicle.params$frac <- getDefault(logicle.params[['frac']], 0.15)
	
	# Set fracX to frac if needed
	logicle.params$fracX <- getDefault(logicle.params$fracX, logicle.params$frac)
	
	# Set fracX to frac if needed
	logicle.params$fracY <- getDefault(logicle.params$fracY, logicle.params$frac)
	
	# Now fracX defaults to supplied fracX or supplied frac if needed
	logicle.params$frac <- logicle.params$fracX
	
	# Now fracY defaults to supplied fracX or supplied frac if needed
	logicle.params$fracY <- logicle.params$fracY
	
	logicle.params$transition <- getDefault(logicle.params$transition, calcTransition(x, base=logicle.params$base, frac=logicle.params$fracX))
	
	logicle.params$transX <- getDefault(logicle.params$transX, logicle.params$transition)
	
	logicle.params$transition <- logicle.params$transX
	
	
	if(!is.null(y))
	{
		logicle.params$transY <- getDefault(logicle.params$transY, calcTransition(y, base=logicle.params$base, frac=logicle.params$fracY))
	}
	
	# if(is.null(y))
	# {
	# 	suppressWarnings(transition <- max(logicle.params$transition, logicle.params$transX))
	# 	# max will return -Inf when provided all NULL args, so catch and set to NULL to use log scaling instead of logicle.
	# 	if(!is.finite(transition))
	# 	{
	# 		transition <- NULL
	# 	}
	# 	logicle.params$transition <- transition
	# 	logicle.params$transX <- transition
	# }
	# else
	# {
	# 	# Then get params for x
	# 	transition <- logicle.params$transX
	# 	if(is.null(transition))
	# 	{
	# 		logicle.params$transX <- calcTransition(x)
	# 	}
	# 	# Then get params for x
	# 	transition <- logicle.params$transY
	# 	if(is.null(transition))
	# 	{
	# 		logicle.params$transY <- calcTransition(y)
	# 	}
	# }
	
	logicle.params$tickSepX <- getDefault(logicle.params$tickSepX, calcTickSep(x=x, transition=logicle.params$transX, base=logicle.params$base, frac=logicle.params$fracX))
	
	logicle.params$tickSep <- logicle.params$tickSepX
	
	if(!is.null(y))
	{
		logicle.params$tickSepY <- getDefault(logicle.params$tickSepY, calcTickSep(x=y, transition=logicle.params$transY, base=logicle.params$base, frac=logicle.params$fracY))
	}
	
	return(logicle.params)
}

calcTickSep <- function(x, transition, base, frac)
{
	if(!is.finite(transition))
	{
		return(NA)
	}
	# Just do it for the right indicies
	valsAbove <- (x > transition) & is.finite(x)
	valsBelow <- (x <= transition) & is.finite(x)
	print(length(valsAbove))
	print(sum(valsAbove))
	if(sum(valsAbove) == 0)
	{
		upperRange <- c(1,base)
	}
	else
	{
		upperRange <- range(x[valsAbove])
	}
	
	if(sum(valsBelow) == 0)
	{
		lowerRange <- transition
	}
	else
	{
		lowerRange <- range(x[valsBelow])
		lowerRange <- lowerRange[2]-lowerRange[1]
		if(lowerRange == 0)
		{
			lowerRange <- transition
		}
	}
	logRangeAbove <- log(upperRange, base=base)
	ticksAbove <- logRangeAbove[2]-logRangeAbove[1]
	if(ticksAbove == 0)
	{
		ticksAbove <- 1
	}
	tickSep <- lowerRange*10^logit.transform(1-frac)/ticksAbove
}

plot.logicle <- function(x, y, type='p', mar=par('mar'), log='', logicle.params=NULL, trans.logit=c(F,F), percentile.limits=c(0,1,0,1), h=NULL, h.col='red', h.lty=1, h.lwd=2, v=NULL, v.col='red', v.lty=1, v.lwd=2, add=F, randomize=T, contour.levels=5, contour.ngrid=20, contour.quantiles=T, contour.adj=c(1,1), contour.alphas=NULL, ...)
{
	logicle.params <- fillDefaultLogicleParams(x=x, y=y, logicle.params=logicle.params)
	
	# Logicle the data (add=T)
	l(x1, y1, xlim, ylim) %=% start.logicle(x=x, y=y, log=log, logicle.params=logicle.params, trans.logit=trans.logit, mar=mar, add=add, percentile.limits=percentile.limits, ...)
	
	# Plot
	# if(!add)
	# {
	# 	l(x1, y1) %=% start.logicle(x=x, y=y, log=log, logicle.params=logicle.params, mar=mar, ...)
	# }
	# else
	# {
	# 	if(grepl('x',log,fixed=T) & !is.null(logicle.params))
	# 	{
	# 		x1 <- logicle(x=x, transition=logicle.params$transX, tickSep=logicle.params$tickSepX, base=logicle.params$base, neg.rm=F)
	# 	}
	# 	else
	# 	{
	# 		x1 <- x
	# 	}
	# 	if(grepl('y',log,fixed=T) & !is.null(logicle.params))
	# 	{
	# 		y1 <- logicle(x=y, transition=logicle.params$transY, tickSep=logicle.params$tickSepY, base=logicle.params$base, neg.rm=F)
	# 	}
	# 	else
	# 	{
	# 		y1 <- y
	# 	}
	# }
	if(type == 'p')
	{
		points(x1, y1, ...)
	}
	else if(type == 'c' && sum(is.finite(x1) & is.finite(y1)) > 0)
	{
		library('MASS')
		lims <- c(getPercentileValues(x1, levels=percentile.limits[1:2]), getPercentileValues(y1, levels=percentile.limits[3:4]))
		if(!is.null(list(...)$xlim))
		{
			lims[1:2] <- logicle(x=list(...)$xlim, transition=logicle.params$transX, tickSep=logicle.params$tickSepX, base=logicle.params$base, neg.rm=F)
		}
		if(!is.null(list(...)$ylim))
		{
			lims[3:4] <- logicle(x=list(...)$ylim, transition=logicle.params$transX, tickSep=logicle.params$tickSepX, base=logicle.params$base, neg.rm=F)
		}
		tempdt <- data.table(x=x1[is.finite(x1) & is.finite(y1)], y=y1[is.finite(x1) & is.finite(y1)], col=list(...)$bg[is.finite(x1) & is.finite(y1)])
		# tempdt[, col2:=factor(col, levels=unique(col))]
		# setorder(tempdt, -col2)
		tempdt2 <- tempdt[, list(list(z=kde2d(x, y, h=c(contour.adj[1]*bandwidth.nrd(x), contour.adj[2]*bandwidth.nrd(y)), lims=par('usr'), n=contour.ngrid))), by='col']
		ranges <- tempdt2[, list(zmax=max(V1[[1]]$z, na.rm=T), zmin=min(V1[[1]]$z, na.rm=T)), by='col']
		abs.ranges <- range(c(ranges$zmax, ranges$zmin), finite=T)
		if(length(contour.levels) == 1)
		{
			if(contour.quantiles)
			{
				abs.levels <- seq(0,1, length.out=contour.levels+2)[2:(contour.levels+2)]
			}
			else
			{
				delta <- (abs.ranges[2]-abs.ranges[1])/(contour.levels+(1-0)) # +0.25 for 5 levels 0 for 4
				abs.levels <- rev(abs.ranges[2] - (0:contour.levels)*delta)
			}
		}
		else
		{
			abs.levels <- contour.levels
		}
		# abs.levels <- seq(abs.ranges[1], abs.ranges[2], length.out=contour.levels+2)[2:(contour.levels+2)]
		contour.alphas <- getDefault(contour.alphas, seq(0.1,0.8, length.out=max(c(2,length(contour.levels))))[2:max(c(2,length(contour.levels)))])
		tempdt2[, filled.contour3(V1[[1]], col=setColor(.BY[[1]], contour.alphas), add=T, axes=F, levels=abs.levels, quantiles=contour.quantiles), by='col']
	}
	else
	{
		lines(x1, y1, ...)
	}
	if(!add)
	{
		finish.logicle(log=log, logicle.params=logicle.params, trans.logit=trans.logit, h=h, h.col=h.col, h.lty=h.lty, h.lwd=h.lwd, v=v, v.col=v.col, v.lty=v.lty, v.lwd=v.lwd, ...)
	}
	return(list(x=x1, y=y1))
}

scatterHist <- function(data, xcol, ycol, colorcol=NULL, by, log='', logicle.params=list(), xlab="", ylab="", lwd=1, add=F, ...)
{
	zones=matrix(c(2,0,1,3), ncol=2, byrow=TRUE)
	layout(zones, widths=c(4/5,1/5), heights=c(1/5,4/5))
	
	if(is.null(colorcol) || is.null(data$colorcol))
	{
		n <- uniqueN(data, by)
		data[, my.temp.color:=loopingPastels(.GRP, max.k=n), by=by]
		colorcol <- 'my.temp.color'
	}
	
	color.data <- data[, list(N=.N), by=c(colorcol,by)]
	setkeyv(data, by)
	setkeyv(color.data, by)
	setorderv(color.data, cols=by, order=-1L)
	for(i in 1:nrow(color.data))
	{
		plot.logicle(x=data[color.data[i, ..by]][[xcol]], y=data[color.data[i, ..by]][[ycol]], type='p', log=log, logicle.params=logicle.params, bg=color.data[i][[colorcol]], pch=21, add=i!=1, ...)
	}
	
	# Plot X margin
	par(mar=c(0,3,1,1))
	for(i in 1:nrow(color.data))
	{
		x <- data[color.data[i, ..by]][[xcol]]
		dahist = density(x)
		xy <- get.logicle(dahist$x, dahist$y, log=log, logicle.params=logicle.params)
		top = max(c(xy$y, xy$y))
		plot(x=xy$x, y=xy$y, type='l', log=log, logicle.params=logicle.params, col=color.data[i][[colorcol]], lwd=lwd, axes=F)
	}
	
	# Plot Y margin
	par(mar=c(3,0,1,1))
	for(i in 1:nrow(color.data))
	{
		x <- data[color.data[i, ..by]][[ycol]]
		dahist = density(x)
		xy <- get.logicle(dahist$x, dahist$y, log=log, logicle.params=logicle.params)
		top = max(c(xy$y, xy$y))
		plot(x=xy$y, y=xy$x, type='l', log=log, logicle.params=logicle.params, col=color.data[i][[colorcol]], lwd=lwd, axes=F)
	}
	
	
	
	# plot(xhist$x, xhist$y, type='l', axes=FALSE, ylim=c(0, top), col=col, lwd=lwd)
	#
	# plot(yhist$y, yhist$x, type='l', axes=FALSE, xlim=c(0, top), col=col, lwd=lwd)
	# par(oma=c(3,3,0,0))
	# mtext(xlab, side=1, line=1, outer=TRUE, adj=0,
	# 	 at=.8 * (mean(x) - min(x))/(max(x)-min(x)))
	# mtext(ylab, side=2, line=1, outer=TRUE, adj=0,
	# 	 at=(.8 * (mean(y) - min(y))/(max(y) - min(y))))
}

# unlogicle <- function(x, transition=NULL, tickSep=NULL, base=NULL)
# {
# 	if(is.null(base))
# 	{
# 		base <- 10
# 	}
# 	valsToAdjust <- (x > transition) & !is.na(x)
# 	if(is.null(transition) & is.null(tickSep))
# 	{
# 		return(base^x)
# 	}
# 	else
# 	{
# 		if(transition <= 0)
# 		{
# 			warning('Transition must be greater than 0. Setting to 1.')
# 			transition <- 1
# 		}
# 		ordersDifferenceOnDisplay = (x[valsToAdjust] - transition) / tickSep
# 		x[valsToAdjust] <- transition*base^(ordersDifferenceOnDisplay)
# 		return(x)
# 	}
# }

unlogicle <- function(x, transition=NULL, base=NULL, tickSep=NULL, trans.logit=F)
{
	if(trans.logit[1])
	{
		return(logit.untransform(x))
	}
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

logicle <- function(x, transition=NULL, base=NULL, tickSep=NULL, logicle.params=NULL, neg.rm=T, trans.logit=F)
{
	if(!is.vector(x) || !is.numeric(x))
	{
		stop('The logicle function requires a vector of numbers')
	}
	if(trans.logit[1])
	{
		return(logit.transform(x))
	}
	logicle.params <- fillDefaultLogicleParams(x=x, y=NULL, logicle.params=logicle.params)
	if(!is.null(logicle.params))
	{
		if(!is.null(logicle.params$transition))
		{
			transition <- logicle.params$transition
		}
		if(!is.null(logicle.params$base))
		{
			base <- logicle.params$base
		}
		if(!is.null(logicle.params$tickSep))
		{
			tickSep <- logicle.params$tickSep
		}
		if(!is.null(logicle.params$frac))
		{
			frac <- logicle.params$frac
		}
	}
	else
	{
		logicle.params <- list()
		logicle.params <- fillDefaultLogicleParams(x=x, y=NULL, logicle.params=logicle.params)
	}
	if(is.null(base))
	{
		base <- logicle.params$base
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
		warning(paste0('Transition that was provided (', transition, '), must be greater than 0. Setting to 1.'))
		transition <- 1
	}
	
	# Just do it for the right indicies
	valsAbove <- (x > transition) & !is.na(x)
	valsBelow <- (x <= transition) & !is.na(x)
	
	if(is.null(tickSep))
	{
		tickSep <- logicle.params$tickSep
	}
	
	x[valsAbove] <- log(x[valsAbove], base=base) + (transition/tickSep - log(transition, base=base))
	x[valsBelow] <- x[valsBelow]/tickSep
	return(x)
}

drawLogitAxis <- function(axisNum=1, base=10, n.minor.ticks=8, las=0, ...)
{
	###
	library(boot)
	library(zoo)
	
	## Generate logit ticks
	duh <- 0.9*10^(0:-10)
	duh <- cumsum(duh)
	
	# These are the true values of the log scale ticks proposed above
	# Thus they exist for x > 0
	logSideNums <- c(rev(1-duh), duh)
	tick.distances <- (logSideNums[2:length(logSideNums)] - logSideNums[1:(length(logSideNums)-1)])/(n.minor.ticks + 1)
	minor.ticks <- c()
	for(i in 1:(length(logSideNums)-1))
	{
		temp <- (1:n.minor.ticks)*tick.distances[i] + logSideNums[i]
		minor.ticks <- c(minor.ticks, temp)
	}
	minor.ticks <- minor.ticks[minor.ticks >= 0.9 | minor.ticks <= 0.1]
	
	# ###
	# # Then treat as a regular log-scale where there are no negative values and
	# # everything should have log-scale ticks.
	# if(base == exp(1))
	# {
	# 	logSidePrettyLabels <- parse(text=paste("e^", log(logSideNums, base=base), sep=""))
	# }
	# else
	# {
	# 	logSidePrettyLabels <- parse(text=paste(base, "^", log(logSideNums, base=base), sep=""))
	# }
	# prettyLabels <- logSidePrettyLabels
	prettyNums <- logSideNums
	
	
	###
	ticks <- logit.transform(x=prettyNums, base=base)
	minor.ticks <- logit.transform(x=minor.ticks, base=base)
	
	axis(axisNum, at=ticks, labels=formatC(logit.untransform(ticks, base=base), digits=5, format='f', drop0trailing = T), las=las, ...)
	axis(axisNum, at=minor.ticks, tcl=par("tcl")*0.5, labels=FALSE)
	axis(axisNum, at=logit.transform(0.5, base=base), labels='0.5', las=las, ...)
	axis(axisNum, at=logit.transform(seq(0.2,0.8,0.1), base=base), tcl=par("tcl")*0.5, labels=FALSE)
}

drawLogicleAxis <- function(axisNum=1, transition=NULL, tickSep=NULL, base=NULL, n.minor.ticks= if (is.null(base)) 9 else (round(base-1, digits=0)-1), las=0, rgl=F, rgl.side='+', lwd=1, overwrite.log.base=NULL, ...)
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
	if(rgl)
	{
		if(axisNum==1)
		{
			axis.limits <- par3d('bbox')[1:2]
		}
		else if(axisNum==2)
		{
			axis.limits <- par3d('bbox')[3:4]
		}
		else
		{
			axis.limits <- par3d('bbox')[5:6]
		}
	}
	else
	{
		if(axisNum==1)
		{
			axis.limits <- par('usr')[1:2]
		}
		else
		{
			axis.limits <- par('usr')[3:4]
		}
	}
	
	### Calculate log-scale information ###
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
		
		linLimits <- unlogicle(c(axis.limits[1],transition/tickSep), transition=transition, tickSep=tickSep, base=base)
		linSidePrettyNums <- pretty(linLimits, n=max(c(ceiling(5*(transition/tickSep-axis.limits[1]))/(axis.limits[2]-axis.limits[1]), 1)), min.n=1)
		linSidePrettyNums <- linSidePrettyNums[linSidePrettyNums > min(linLimits) & linSidePrettyNums <= max(linLimits) ]
		
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
	
	# Override lwd which is intended for curves, not axes.
	otherParams <- merge.lists(list(...), list(lwd=1))
	
	if(rgl)
	{
		temp <- ticks[ticks >= axis.limits[1] & ticks <= axis.limits[2]]
		if(axisNum == 1)
		{
			otherParams2 <- merge.lists(otherParams, list(edge=paste0('x',rgl.side), at=temp, labels=unlogicle(temp, transition=transition, base=base, tickSep=tickSep), las=2))
			do.call(axis3d, otherParams2)
			otherParams2 <- merge.lists(otherParams, list(edge=paste0('x',rgl.side), at=minor.ticks[minor.ticks >= axis.limits[1] & minor.ticks <= axis.limits[2]], tcl=par("tcl")*0.5, labels=FALSE))
			do.call(axis3d, otherParams2)
		}
		else if(axisNum == 2)
		{
			otherParams2 <- merge.lists(otherParams, list(edge=paste0('y',rgl.side), at=temp, labels=unlogicle(temp, transition=transition, base=base, tickSep=tickSep), las=2))
			do.call(axis3d, otherParams2)
			otherParams2 <- merge.lists(otherParams, list(edge=paste0('y',rgl.side), at=minor.ticks[minor.ticks >= axis.limits[1] & minor.ticks <= axis.limits[2]], tcl=par("tcl")*0.5, labels=FALSE))
			do.call(axis3d, otherParams2)
		}
		else
		{
			otherParams2 <- merge.lists(otherParams, list(edge=paste0('z',rgl.side), at=temp, labels=unlogicle(temp, transition=transition, base=base, tickSep=tickSep), las=2))
			do.call(axis3d, otherParams2)
			otherParams2 <- merge.lists(otherParams, list(edge=paste0('z',rgl.side), at=minor.ticks[minor.ticks >= axis.limits[1] & minor.ticks <= axis.limits[2]], tcl=par("tcl")*0.5, labels=FALSE, las=las))
			do.call(axis3d, otherParams2)
		}
	}
	else
	{
		if(is.null(overwrite.log.base))
		{
			otherParams2 <- merge.lists(otherParams, list(side=axisNum, at=ticks, labels=prettyLabels, las=las))
			do.call(axis, otherParams2)
			otherParams2 <- merge.lists(otherParams, list(side=axisNum, at=minor.ticks, tcl=par("tcl")*0.5, labels=FALSE))
			do.call(axis, otherParams2)
		}
		else
		{
			if(is.null(transition))
			{
				otherParams2 <- merge.lists(otherParams, list(side=axisNum, at=overwrite.log.base^ticks, labels=prettyLabels, las=las))
				do.call(axis, otherParams2)
				otherParams2 <- merge.lists(otherParams, list(side=axisNum, at=overwrite.log.base^minor.ticks, tcl=par("tcl")*0.5, labels=FALSE))
				do.call(axis, otherParams2)
			}
			else
			{
				otherParams2 <- merge.lists(otherParams, list(side=axisNum, at=ticks, labels=prettyLabels, las=las))
				do.call(axis, otherParams2)
				otherParams2 <- merge.lists(otherParams, list(side=axisNum, at=minor.ticks, tcl=par("tcl")*0.5, labels=FALSE))
				do.call(axis, otherParams2)
			}
			
		}
		
		# if(axisNum == 2)
		# {
		# 	axis(axisNum, at=ticks, labels=prettyLabels, las=las, ...)
		# 	axis(axisNum, at=minor.ticks, tcl=par("tcl")*0.5, labels=FALSE)
		# }
		# else
		# {
		# 	axis(axisNum, at=ticks, labels=prettyLabels, las=las, ...)
		# 	axis(axisNum, at=minor.ticks, tcl=par("tcl")*0.5, labels=FALSE, las=las)
		# }
	}
}

drawBbox3d <- function()
{
	box3d()
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
plot.hist <- function(x, type=c('d','h'), log='', trans.logit=F, neg.rm=T, logicle.params=NULL, density.args=NULL, breaks=100, add=F, border='black', col='gray', mar=NULL, mgp=NULL, las=NULL, silent=F, cumulative=F, ...)
{
	#### CHECK OUT WHY IT IS PLOTTING 0-0.2 instead of 0-2.96
	logicle.params.original <- copy(logicle.params)
	x.original <- copy(x)
	logicle.params <- fillDefaultLogicleParams(x=x, y=NULL, logicle.params=logicle.params)
	default.mar <- par('mar')
	default.mgp <- par('mgp')
	default.las <- par('las')
	if(is.null(mar))
	{
		mar <- default.mar
	}
	if(is.null(mgp))
	{
		mgp <- default.mgp
	}
	if(is.null(las))
	{
		las <- default.las
	}
	par(mar=mar, mgp=mgp, las=las[1])
	plot.params <- list(...)
	# Adjust the data to log/logicle scale if needed FIRST
	if((log==T || grepl('x',log,fixed=T)) & !trans.logit[1])
	{
		x <- logicle(x, logicle.params=logicle.params, neg.rm=neg.rm)
		if(!is.null(logicle.params) && !is.null(plot.params$xlim))
		{
			# Then we should also control the limits of the plot since we'll be drawing a logicle axis
			# print(logicle(plot.params$xlim, logicle.params=logicle.params, neg.rm=F))
			plot.params$xlim <- logicle(plot.params$xlim, logicle.params=logicle.params, neg.rm=F)
		}
	}
	if(trans.logit[1])
	{
		x <- logit.transform(x)
		if(!is.null(plot.params$xlim))
		{
			plot.params$xlim <- logit.transform(plot.params$xlim)
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
		# print(ret$y)
	}
	else
	{
		if(is.null(density.args))
		{
			density.args <- list()
		}
		density.args$x <- x
		if(length(x) > 1)
		{
			ret <- do.call(density, density.args[names(density.args) != 'draw.area'])
			if(cumulative)
			{
				ret$y <- cumsum(ret$y)
				ret$y <- ret$y/(max(ret$y))
				if(max(ret$x) < max(plot.params$xlim))
				{
					ret$x <- c(ret$x, max(plot.params$xlim))
					ret$y <- c(ret$y, 1)
				}
			}
			if(grepl('y', log, fixed=T))
			{
				logicle.params.y <- fillDefaultLogicleParams(x=x.original, y=ret$y, logicle.params=logicle.params.original)
				ret$y <- logicle(ret$y, transition=logicle.params.y$transY, tickSep=logicle.params.y$tickSepY, neg.rm=neg.rm)
				if(!is.null(plot.params$ylim))
				{
					# Then we should also control the limits of the plot since we'll be drawing a logicle axis
					# print(logicle(plot.params$xlim, logicle.params=logicle.params, neg.rm=F))
					plot.params$ylim <- logicle(plot.params$ylim, transition=logicle.params.y$transY, tickSep=logicle.params.y$tickSepY, neg.rm=F)
				}
			}
			if(!silent)
			{
				# Determine whether to draw the area or border or both
				draw.border <- T
				draw.area <- T
				if(!is.null(density.args$draw.border))
				{
					draw.border <- density.args$draw.border
				}
				if(!is.null(density.args$draw.area))
				{
					draw.area <- density.args$draw.area
				}
				
				# If adding to an existing plot...
				if(add)
				{
					if(!draw.area)
					{
						if(is.null(plot.params))
						{
							lines(ret$x, ret$y, col=border, ...)
							# plotPolygon(ret$x, ret$y, col=col, border=border)
						}
						else
						{
							# Draw lines and no polygon or border
							plot.params <- merge.lists(list(x=ret$x, y=ret$y, col=border, mar=mar, mgp=mgp), plot.params)
							# clip(x1=par('usr')[1], x2=par('usr')[2], y1=par('usr')[3], y2=par('usr')[4])
							do.call(lines, plot.params)
							# # Add zero levels before and after sequence of numbers
							# plotPolygon(ret$x, ret$y, col=col, border=border)
						}
						# # Draw lines and no polygon or border
						# plot.params <- merge.lists(list(x=ret$x, y=ret$y, col=border, mar=mar, mgp=mgp), plot.params)
						# # clip(x1=par('usr')[1], x2=par('usr')[2], y1=par('usr')[3], y2=par('usr')[4])
						# do.call(lines, plot.params)
					}
					else
					{
						if(!draw.border)
						{
							border <- rgb(0,0,0,0)
						}
						if(is.null(plot.params))
						{
							# Add zero levels before and after sequence of numbers
							plotPolygon(ret$x, ret$y, col=col, border=border)
						}
						else
						{
							# Add zero levels before and after sequence of numbers
							plotPolygon(ret$x, ret$y, col=col, border=border)
						}
					}
					# if(col==rgb(0,0,0,0))
					# {
					# 	# Draw lines and no polygon or border
					# 	plot.params <- merge.lists(list(x=ret$x, y=ret$y, col=border, mar=mar, mgp=mgp), plot.params)
					# 	# clip(x1=par('usr')[1], x2=par('usr')[2], y1=par('usr')[3], y2=par('usr')[4])
					# 	do.call(lines, plot.params)
					# }
					# else
					# {
					# 	# Draw polygon border
					# 	# Add zero levels before and after sequence of numbers
					# 	plotPolygon(ret$x, ret$y, col=col, border=border)
					# }
				}
				else
				{ # If creating a new plot
					
					if(!draw.area)
					{
						if(is.null(plot.params))
						{
							plot(ret$x, ret$y, col=rgb(0,0,0,0), xaxt='n', ...)
							# Add zero levels before and after sequence of numbers
							lines(ret$x, ret$y, col=border, ...)
							# plotPolygon(ret$x, ret$y, col=col, border=border)
						}
						else
						{
							plot.params <- merge.lists(plot.params, list(x=ret$x, y=ret$y, col=rgb(0,0,0,0), xaxt='n'))
							do.call(plot, plot.params)
							plot.params <- merge.lists(plot.params, list(col=border))
							do.call(lines, plot.params)
							# lines(ret$x, ret$y, col=border, ...)
							# # Add zero levels before and after sequence of numbers
							# plotPolygon(ret$x, ret$y, col=col, border=border)
						}
						# # Draw lines and no polygon or border
						# plot.params <- merge.lists(list(x=ret$x, y=ret$y, col=border, mar=mar, mgp=mgp), plot.params)
						# # clip(x1=par('usr')[1], x2=par('usr')[2], y1=par('usr')[3], y2=par('usr')[4])
						# do.call(lines, plot.params)
					}
					else
					{
						if(!draw.border)
						{
							border=rgb(0,0,0,0)
						}
						if(is.null(plot.params))
						{
							plot(ret$x, ret$y, col=rgb(0,0,0,0), xaxt='n', ...)
							# Add zero levels before and after sequence of numbers
							plotPolygon(ret$x, ret$y, col=col, border=border)
						}
						else
						{
							plot.params <- merge.lists(plot.params, list(x=ret$x, y=ret$y, col=rgb(0,0,0,0), xaxt='n'))
							do.call(plot, plot.params)
							# Add zero levels before and after sequence of numbers
							plotPolygon(ret$x, ret$y, col=col, border=border)
						}
					}
					
				}
			}
		}
		else
		{
			ret <- list(x=0, y=0)
		}
	}
	
	if(!add & !silent & !is.null(plot.params) & !is.null(plot.params$axes) )
	{
		# First check to see if plot.params has axes=F
		drawTheXAxis <- T
		drawTheYAxis <- T
		if(!is.null(plot.params))
		{
			if(!is.null(plot.params$axes))
			{
				if(!plot.params$axes)
				{
					drawTheXAxis <- F
					drawTheYAxis <- F
				}
			}
			if(!is.null(plot.params$xaxt))
			{
				if(plot.params$xaxt=='n')
				{
					drawTheXAxis <- F
				}
			}
			if(!is.null(plot.params$yaxt))
			{
				if(plot.params$yaxt=='n')
				{
					drawTheYAxis <- F
				}
			}
		}
		if(drawTheXAxis)
		{
			if(grepl('x', log, fixed=T))
			{
				if(is.null(logicle.params))
				{
					drawLogicleAxis(axisNum=1, las=las[1], cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1), ...)
				}
				else
				{
					drawLogicleAxis(axisNum=1, transition=logicle.params$transition, tickSep=logicle.params$tickSep, base=logicle.params$base, las=las[1], cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1), ...)
				}
			}
			else
			{
				if(trans.logit[1])
				{
					drawLogitAxis(axisNum=1, las=las[1], cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1), ...)
				}
				else
				{
					axis(1, cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1), ...)
				}
			}
		}
		if(drawTheYAxis)
		{
			if(grepl('y', log, fixed=T))
			{
				if(is.null(logicle.params))
				{
					drawLogicleAxis(axisNum=2, las=las[1], cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1), ...)
				}
				else
				{
					drawLogicleAxis(axisNum=2, transition=logicle.params.y$transition, tickSep=logicle.params.y$tickSep, base=logicle.params.y$base, las=las[1], cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1), ...)
				}
			}
			else
			{
				if(trans.logit[2])
				{
					drawLogitAxis(axisNum=2, las=las[1], cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1), ...)
				}
				else
				{
					axis(2, cex.lab=getDefault(list(...)$cex.lab, 1), cex.axis=getDefault(list(...)$cex.axis,1), ...)
				}
			}
		}
	}
	# par(mar=default.mar, mgp=default.mgp, las=default.las)
	return(ret)
}

getDefault <- function(x, default, test=is.null)
{
	if(test(x))
	{
		return(default)
	}
	else
	{
		return(x)
	}
}

getPercentileValues <- function(x, levels=c(0,1), finite=T, na.rm=T)
{
	blah <- levels
	blah[blah < 0] <- 0
	blah[blah > 1] <- 1
	if(finite)
	{
		temp <- as.numeric(quantile(x[is.finite(x)], blah, na.rm=na.rm))
	}
	else
	{
		temp <- as.numeric(quantile(x, blah, na.rm=na.rm))
	}
	temp[levels < 0] <- temp[levels < 0] - abs(temp[levels < 0]*levels[levels < 0])
	temp[levels > 1] <- temp[levels > 0] + abs(temp[levels > 0]*(levels[levels > 0]-1))
	return(temp)
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
	library(spatstat)
	
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
	library(spatstat)
	ret <- ellipse(x, y, a, b, n)
	if(!is.null(logicle.params))
	{
		if(grepl('x',log,fixed=T))
		{
			ret$x <- logicle(ret$x, transition=logicle.params$transX, tickSep=logicle.params$tickSepX, base=logicle.params$base, neg.rm=F)
		}
		if(grepl('y',log,fixed=T))
		{
			ret$y <- logicle(ret$y, transition=logicle.params$transY, tickSep=logicle.params$tickSepY, base=logicle.params$base, neg.rm=F)
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
	mm <- getPercentileValues(x, levels=c(percentile, 1-percentile))
	return( log( (-mm[1] + x) / (mm[2] - x) ) )
}

sim.transform.bounds <- function(x, x.min, x.max)
{
	return( log( (-x.min + x) / (x.max - x) ) )
}

sim.untransform <- function(x)
{
	return( (exp(x) - 1) / (exp(x) + 1) )
}

sim.untransform.bounds <- function(x, x.min, x.max)
{
	return((x.max*exp(x)+x.min)/(exp(x)+1))
}

logit.transform <- function(x, base=10)
{
	log(x/(1-x), base=base)
}

logit.untransform <- function(x, base=10)
{
	ret <- x
	ret[is.finite(x)] <- base^(x[is.finite(x)])/(1+base^(x[is.finite(x)]))
	ret[x==Inf] <- 1
	ret[x==-Inf] <- 0
	ret[is.na(x)] <- NA
	return(ret)
}

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

roll.mean <- function(x, win.width=2, na.rm=T, ...)
{
	library(zoo)
	# This will return a vector of the same size as original and will deal with NAs and optimize for mean.
	return(rollapply(x, width=win.width, FUN=mean, na.rm=na.rm, ..., partial=T, align='center'))
}

roll.median <- function(x, win.width=2, na.rm=T, ...)
{
	library(zoo)
	# This will return a vector of the same size as original and will deal with NAs and optimize for mean.
	return(rollapply(x, width=win.width, FUN=median, na.rm=na.rm, ..., partial=T, align='center'))
}

#''
#'One sd is equal to the win.width/2 and extends +/- 5 sigma
roll.gaussian <- function(x, win.width=2, ...)
{
	library(smoother)
	return(smth(x, method='gaussian', window=win.width, ...))
}

#' Get the adjustable running window average of the data
#' @param i The index within 'frames' at which to calculate an average over a window centered at this location
#' @param frames The frames in this track
#' @param widths A vector of window widths appropriate for each frame in the 'frames' of this track
#' @param data The vector of data for which we will calculate the windowed averages
getAverage <- function(i, frames, widths, data)
{
	# Subtract 1 to represent the number of intervals instead of number of points to average
	width <- widths[i] - 1
	
	# calculate the index on the left of the interval
	leftIndex <- i - floor(width/2)
	if(leftIndex < 1)
	{
		leftIndex <- 1
	}
	
	# calculate the width to reach index on the right of the interval
	if((leftIndex+width) > length(frames))
	{
		width <- length(frames)-leftIndex
	}
	
	# return the mean of the data over the interval
	mean(data[leftIndex:(leftIndex+width)])
}

#' Get the derivative of a vector
#' @param x A numeric vector on which to calculate the derivative
#' @param t A numeric vecotor of times with which to determine dt for derivative calculations
getDerivative <- function(x, t)
{
	v <- numeric(0)
	for(i in 1:length(x))
	{
		v <- c(v, localDerivative(x, t, i))
	}
	return(v)
}

#' Get the derivative of a vector
#' @param x A numeric vector on which to calculate the deltas (t+1) - (t)
getDeltas <- function(x)
{
	if(length(x) < 2)
	{
		return(numeric(0))
	}
	return(x[2:length(x)] - x[1:(length(x)-1)])
}

#' Get the derivative of a vector
#' @param x A numeric vector on which to calculate the deltas (t+1) - (t)
getPaddedDeltas <- function(x, pad=NA)
{
	return(c(pad, x[2:length(x)] - x[1:(length(x)-1)]))
}

#' Get the local derivative around a point in a vector accounding for boundary scenarios at the start and end of the vector
#' @param x A numeric vector of data
#' @param t A numeric vector of time for calculating dt of the derivative
#' @param i A numeric value indicating the index in the x and t for which to calculate the local derivative
localDerivative <- function(x, t, i)
{
	if(i == 1)
	{
		#return((x[i+1]-x[i])/(t[i+1]-t[i]))
		return(interpolateDerivative(x[i], x[i+1], x[i+2], t[i], t[i+1], t[i+2], t[i]))
	}
	else if(i == length(x))
	{
		#return((x[i]-x[i-1])/(t[i]-t[i-1]))
		return(interpolateDerivative(x[i-2], x[i-1], x[i], t[i-2], t[i-1], t[i], t[i]))
	}
	else
	{
		return(interpolateDerivative(x[i-1], x[i], x[i+1], t[i-1], t[i], t[i+1], t[i]))
	}
}

#' @title This is a three point interpolation of the derivative where the interpolated point
#' is the middle of the 3 points.
#'
#' @description This simplifies to the the three-point midpoint formula
#' when the time steps are equal but can handle when timesteps are unequal (i.e., the
#' time-step on either side of the 3 points is not equal)
#'
#' @param f0 numeric left function value
#' @param f1 numeric middle function value
#' @param f2 numeric right function value
#' @param x0 numeric left independent value
#' @param x1 numeric middle independent value
#' @param x2 numeric right independent value
#' @param xj numeric x value for which to evaluate the function
interpolateDerivative <- function(f0, f1, f2, x0, x1, x2, xj)
{
	term1 <- f0*((2*xj-x1-x2)/((x0-x1)*(x0-x2)))
	term2 <- f1*((2*xj-x0-x2)/((x1-x0)*(x1-x2)))
	term3 <- f2*((2*xj-x0-x1)/((x2-x0)*(x2-x1)))
	return(term1 + term2 + term3)
}

#' path is the file path to the jxd file holding the ROI information
readJEXMaxima <- function(path)
{
	library(data.table)
	parsePolygon <- function(polygon)
	{
		pairs <- strsplit(polygon,';')[[1]]
		x <- numeric(0)
		x0 <- numeric(0)
		y <- numeric(0)
		y0 <- numeric(0)
		index <- numeric(0)
		first <- TRUE
		for(pair in pairs)
		{
			nums <- strsplit(pair,',')[[1]]
			x <- append(x, as.numeric(nums[1]))
			y <- append(y, as.numeric(nums[2]))
			index <- append(index,as.numeric(nums[3]))
		}
		
		return(data.table(id=index, x=x, y=y))
	}
	y <- data.table(read.arff(path))
	idCols <- names(y)[!(names(y) %in% c('Metadata','Value'))]
	x <- y[Metadata=='polygonPts', parsePolygon(Value), by=idCols]
	return(x)
}

clear.warnings <- function()
{
	assign("last.warning", NULL, envir = baseenv())
}

sig.digits <- function(x, nSig=2, nTrail=0)
{
	# return(format(signif(x,digits=n), digits=n, justify='right'))
	ret <- signif(x,digits=nSig)
	ret.s <- as.character(ret)
	if(all(!grepl(".", ret.s, fixed=T)))
	{
		return(sprintf(paste0("%.",nTrail,"f"), ret))
	}
	ret.n <- tstrsplit(ret.s, ".", fixed=T, keep=2)[[1]]
	ret.n[is.na(ret.n)] <- ""
	ret.n <- nchar(ret.n)
	ret.n <- max(ret.n)
	return(sprintf(paste0("%.",max(ret.n,nTrail),"f"), ret))
}

getOS <- function()
{
	sysinf <- Sys.info()
	if (!is.null(sysinf))
	{
		os <- sysinf['sysname']
		if (os == 'Darwin')
			os <- "osx"
	}
	else
	{
		## mystery machine
		os <- .Platform$OS.type
		if (grepl("^darwin", R.version$os))
			os <- "osx"
		if (grepl("linux-gnu", R.version$os))
			os <- "linux"
	}
	tolower(os)
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

geom.mean = function(x, na.rm=TRUE, zero.propagate = FALSE){
	if(any(x < 0, na.rm = TRUE)){
		return(NaN)
	}
	if(zero.propagate){
		if(any(x == 0, na.rm = TRUE)){
			return(0)
		}
		exp(mean(log(x), na.rm = na.rm))
	} else {
		exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
	}
}

# x1,y1 is the central point (i.e., the point of interest and x0,y0 and x2,y2 are neighbors)
getColinearity <- function(x0, y0, x1, y1, x2, y2)
{
	# Test Code
	# maxI <- 100
	# n <- 1000
	# rndPts <- data.table(x=runif(n,-1,1), y=runif(n,-1,1))
	# rndPts <- rndPts[x^2+y^2 <= 1]
	# rndPts[, cId:=1:.N]
	# rndPts[, alpha:=getColinearity(sample(x), sample(y), 0, 0, sample(x), sample(y))]
	# hist(rndPts$alpha)
	
	v <- data.table(ax=x1-x0,ay=y1-y0,bx=x2-x1,by=y2-y1)
	v[, ':='(amag=sqrt(ax^2+ay^2), bmag=sqrt(bx^2+by^2), dot=ax*bx+ay*by)]
	v[, val:=dot/(amag*bmag)]
	v[, ret:=1-2*acos(abs(val))/pi]
	return(v$ret)
}

# x1,y1 is the central point (i.e., the point of interest and x0,y0 and x2,y2 are neighbors)
getPackingIndex <- function(x0, y0, x1, y1, x2, y2)
{
	# Test Code
	# maxI <- 100
	# n <- 1000
	# rndPts <- data.table(x=runif(n,-1,1), y=runif(n,-1,1))
	# rndPts <- rndPts[x^2+y^2 <= 1]
	# rndPts[, cId:=1:.N]
	# rndPts[, alpha:=getColinearity(sample(x), sample(y), 0, 0, sample(x), sample(y))]
	# hist(rndPts$alpha)
	
	v <- data.table(ax=x1-x0,ay=y1-y0,bx=x2-x1,by=y2-y1)
	v[, ':='(amag=sqrt(ax^2+ay^2), bmag=sqrt(bx^2+by^2), dot=ax*bx+ay*by)]
	v[, val:=dot/(amag*bmag)]
	v[, ret:=1-acos(val)/pi]
	# v[, ret:=180*acos(val)/pi]
	
	return(list(dmin=pmin(v$amag,v$bmag), dmax=pmax(v$amag,v$bmag), alpha=v$ret))
}

# The first point is the point of interest and the next two are the neighbors
getPackingIndex_Helper <- function(x, y)
{
	return(getPackingIndex(x[2], y[2], x[1], y[1], x[3], y[3]))
}

getCandidates <- function(dt, cId, xcol, ycol, xpos, ypos, searchRadius, N)
{
	dt[, r:=as.double((get(xcol)-xpos)^2 + (get(ycol)-ypos)^2)]
	dt <- dt[order(r)]
	dt <- dt[1:N]
	return(as.vector(dt[r < searchRadius^2, c(cId), with=F]))
}

getNeighborsInRadius <- function(dt, cIdCol, xcol, ycol, keep=c(), searchRadius, by=NULL)
{
	if(is.null(by))
	{
		dt2 <- copy(dt)
		dt[, neighbors:=list(list(getNeighborsInRadius_(dt2, cIdCol=cIdCol, theId=.BY[[1]], keep=keep, searchRadius=searchRadius))), by=cIdCol][]
	}
	else
	{
		thingsToDo <- getUniqueCombos(x, idCols=by, ordered=T)
		setkeyv(thingsToDo, by)
		setkeyv(dt, by)
		for(i in 1:nrow(thingsToDo))
		{
			print(paste0('Performing ', i, ' of ', nrow(thingsToDo), '.'))
			temp <- thingsToDo[i]
			setkeyv(temp, by)
			dt2 <- dt[temp]
			dt[temp, neighbors:=list(list(getNeighborsInRadius_(dt2, cIdCol=cIdCol, theId=.BY[[1]], keep=keep, searchRadius=searchRadius))), by=cIdCol][]
		}
	}
}

getNeighborsInRadius_ <- function(dt, cIdCol, theId, xcol, ycol, keep=c(), searchRadius)
{
	dt[, dx:=x-x[get(cIdCol)==theId]]
	dt[, dy:=y-y[get(cIdCol)==theId]]
	dt[, r:=sqrt(dx^2 + dy^2)]
	return(dt[r < searchRadius, c(cIdCol, keep, 'r'), with=F])
}

getNearestNeighbors <- function(x, searchRadius=0.1, cIdCol='cId', xcol='Geometric.COMX_None_Nuc', ycol='Geometric.COMY_None_Nuc', N=1)
{
	x$neighbors <- NULL
	x2 <- copy(x)
	x[, neighbors:=list()]
	for(ex in uniqueo(x$x))
	{
		for(ey in uniqueo(x$y))
		{
			x[x==ex & y==ey, neighbors:=list(list(getCandidates(x2[x==ex & y==ey], cId=cIdCol, xcol=xcol, ycol=ycol, xpos=get(xcol), ypos=get(ycol), searchRadius=searchRadius, N=N))), by=cIdCol]
		}
	}
	return(x)
}

# The first point is the point of interest and the next two are the neighbors
getColinearity_Helper <- function(x, y)
{
	return(getColinearity(x[2], y[2], x[1], y[1], x[3], y[3]))
}

getColinearityOfcIds <- function(x, cIdCol, cIds, xcol, ycol)
{
	temp <- x[match(cIds, get(cIdCol))]
	return(getColinearity_Helper(temp[[xcol]], temp[[ycol]]))
}

getPackingIndexOfcIds <- function(x, cIdCol, cIds, xcol, ycol)
{
	temp <- x[match(cIds, get(cIdCol))]
	return(getPackingIndex_Helper(temp[[xcol]], temp[[ycol]]))
}

getPaddedRange <- function(x, frac=0.01, lo=frac, hi=frac)
{
	lims <- range(x, finite=T)
	lims[1] <- lims[1] - (lo*abs(lims[1]))
	lims[2] <- lims[2] + (hi*abs(lims[2]))
	return(lims)
}

filled.contour3 <-
	function (x = seq(0, 1, length.out = nrow(z)),
			y = seq(0, 1, length.out = ncol(z)), z, zlim = range(z, finite = TRUE),
			levels = NULL, nlevels = 4, color.palette = loopingPastels, nnlevels = if (is.null(levels)) {nlevels} else {length(levels)},
			quantiles=F, col = color.palette(1:(nnlevels - 1)), plot.title, plot.axes,
			key.title, key.axes, xaxs = "i", yaxs = "i", las = 1,
			axes = TRUE, frame.plot = axes,mar, add=F, ...)
	{
		# modification by Ian Taylor of the filled.contour function
		# to remove the key and facilitate overplotting with contour()
		# further modified by Carey McGilliard and Bridget Ferris
		# to allow multiple plots on one page
		
		if (missing(z)) {
			if (!missing(x)) {
				if (is.list(x)) {
					z <- x$z
					y <- x$y
					x <- x$x
				}
				else {
					z <- x
					x <- seq.int(0, 1, length.out = nrow(z))
				}
			}
			else stop("no 'z' matrix specified")
		}
		else if (is.list(x)) {
			y <- x$y
			x <- x$x
		}
		if (any(diff(x) <= 0) || any(diff(y) <= 0))
			stop("increasing 'x' and 'y' values expected")
		# mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
		# on.exit(par(par.orig))
		# w <- (3 + mar.orig[2]) * par("csi") * 2.54
		# par(las = las)
		# mar <- mar.orig
		
		if(is.null(levels))
		{
			if(quantiles)
			{
				# Make quantile levels
				levels <- seq(0, 1, length.out=nlevels+2)[2:(nlevels+2)]
			}
			else
			{
				# Make z levels
				levels <- unique(seq(zlim[1], zlim[2], length.out=nlevels+2)[2:(nlevels+2)])
			}
		}
		else
		{
			if(quantiles)
			{
				# Convert quantile levels to z levels
				levels <- unique(getDensityQuantiles(xvec=x, yvec=y, zmat=z, levels=levels))
			}
		}
		
		if(!add)
		{
			plot.new()
		}
		# par(mar=mar)
		# plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
		if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1)
			stop("no proper 'z' matrix specified")
		if (!is.double(z))
			storage.mode(z) <- "double"
		
		.filled.contour(as.double(x), as.double(y), z, as.double(levels),
					 col = col)
		if (missing(plot.axes)) {
			if (axes) {
				title(main = "", xlab = "", ylab = "")
				Axis(x, side = 1)
				Axis(y, side = 2)
			}
		}
		else plot.axes
		if (frame.plot)
			box()
		if (missing(plot.title))
			title(...)
		else plot.title
		invisible()
	}

#' CRatio is the Nuclear:Cytoplasmic Concentration Ratio
#' RL is the Radial Localization
#' RLMax is the max RL for the cell / imaging setup
#' t is the fit paramter that is specific to the cell geometry and imaging setup
calcRL <- function(CRatio, RLMin=1, RLMax=1.1, s=1)
{
	RL <- RLMin*sqrt((CRatio*s/(CRatio*s+1))*(1-RLMax^2) + RLMax^2)
	return(RL)
}

#' Error function to be minimized
#' par is a named vector with parameter 's'
#' as its first and only parameter
#' s is the fit paramter that is specific to the cell geometry and imaging setup
errRL1 <- function(par, CRatio, RL)
{
	RL2 <- calcRL(CRatio=CRatio, RLMin=1, RLMax=max(RL), s=par[1])
	err <- sum((RL-RL2)^2)
	return(err)
}

#' Error function to be minimized
#' par is a named vector with parameters 's' and 'RLMax' (in that order)
#' s is the fit paramter that is specific to the cell geometry and imaging setup
#' RLMax is the fit paramter that represents the maximum value of RoGProtein / ROGHoechst
errRL2 <- function(par, CRatio, RL)
{
	RL2 <- calcRL(CRatio=CRatio, RLMin=1, RLMax=par[2], s=par[1])
	err <- sum((RL-RL2)^2)
	return(err)
}

#' Error function to be minimized
#' par is a named vector with parameters 's', 'RLMin', and 'RLMax' (in that order)
#' s is the fit paramter that is specific to the cell geometry and imaging setup
#' RLMax is the fit paramter that represents the maximum possible value of RoGProtein / ROGHoechst
#' RLMin is the fit paramter that represents the minimum possible value of RoGProtein / ROGHoechst
errRL3 <- function(par, CRatio, RL)
{
	RL2 <- calcRL(CRatio=CRatio, RLMin=par[3], RLMax=par[2], s=par[1])
	err <- sum((RL-RL2)^2)
	return(err)
}

#' CRatio is the Nuclear:Cytoplasmic Concentration Ratio
#' RL is the Radial Localization (RoGp/RoGh)^1
#' RLMax is the max RL for the cell / imaging setup
#' t is the fit paramter that is specific to the cell geometry and imaging setup
calcCRatio <- function(RL, RLMin, RLMax, s)
{
	ret <- ((RLMin^2*RLMax^2-RL^2)/((RL^2-RLMin^2)*s))
	ret[ret < 0 & ret > -1] <- 0
	ret[ret < 0 & ret <= -1] <- Inf
	return(ret)
}

#' CRatio is the Nuclear:Cytoplasmic Concentration Ratio
#' RL is the Radial Localization (RoGp/RoGh)^2
#' RLMax is the max RL for the cell / imaging setup
#' t is the fit paramter that is specific to the cell geometry and imaging setup
calcCRatio2 <- function(RL, RLMin, RLMax, s)
{
	ret <- ((RLMin*RLMax-RL)/((RL-RLMin)*s))
	ret[ret < 0 & ret > -1] <- 0
	ret[ret < 0 & ret <= -1] <- Inf
	return(ret)
}

#' ARatio is the Nuclear:Cytoplasmic Amount Ratio
#' RL is the Radial Localization
#' RLMax is the max RL for the cell / imaging setup
#' t is the fit paramter that is specific to the cell geometry and imaging setup
calcARatio <- function(RL, RLMin, RLMax, sSampled, sActual)
{
	CRatio <- calcCRatio(RL=RL, RLMin=RLMin, RLMax=RLMax, s=sSampled)
	return(CRatio*sActual/(CRatio*sActual + 1))
}

#' ARatio is the Nuclear:Cytoplasmic Amount Ratio
#' RL is the Radial Localization
#' RLMax is the max RL for the cell / imaging setup
#' t is the fit paramter that is specific to the cell geometry and imaging setup
calcARatio2 <- function(RL, RLMin, RLMax, sSampled, sActual)
{
	CRatio <- calcCRatio2(RL=RL, RLMin=RLMin, RLMax=RLMax, s=sSampled)
	return(CRatio*sActual/(CRatio*sActual + 1))
}

#' Calculate quantiles under a surface
getDensityQuantiles <- function(xvec, yvec, zmat, levels=c(0.25, 0.5, 0.75), res=100)
{
	if(any(levels > 1 | levels <= 0))
	{
		stop('Cannot compute for quantiles <= 0 or > 1')
	}
	tot <- getSurfaceVolume(xvec, yvec, zmat)
	# zVals <- rev(uniqueo(as.vector(zmat)))
	n <- length(levels)
	levelOrd <- order(levels)
	j <- 1
	ret <- rep(getPaddedRange(levels, frac=0.001)[2], length(levels)) # Set the highest level just above the max to be sure to encompass the value despite machine precision
	for(i in seq(min(zmat), max(zmat), length.out=res))
	{
		ztemp <- copy(zmat)
		ztemp[zmat > i] <- 0 # Truncate the mountain
		cur <- getSurfaceVolume(xvec, yvec, ztemp) # Get the volume of the truncated mountain
		if(cur >= levels[levelOrd[j]]*tot) # If greater than the specified fraction of the tot volume
		{
			ret[levelOrd[j]] <- i # Save the threshold zVal
			j <- j + 1 # index to the next threshold
			if(j > length(levels))
			{
				break
			}
		}
	}
	return(ret)
}

getSurfaceVolume <- function(xvec, yvec, zmat)
{
	df <- data.frame(x=rep(xvec, each=length(yvec)), y=yvec, z=as.vector(zmat))
	library(geometry)
	
	#find triangular tesselation of (x,y) grid
	res=delaunayn(as.matrix(df[,-3]),full=TRUE,options="Qz")
	
	#calulates sum of truncated prism volumes
	sum(mapply(function(triPoints,A) A/3*sum(df[triPoints,'z']),
			 split.data.frame(res$tri,seq_along(res$areas)),
			 res$areas))
}

hgf_3F2 <- function(a, b, c, d, e, z)
{
	library(hypergeo)
	return(genhypergeo(U=c(a, b, c), L=c(d, e), z=z))
}

hgf_2F1 <- function(a, b, c, z)
{
	library(gsl)
	if(z>=0 & z<1)
	{
		ret <- hyperg_2F1(a,b,c,z)
	}
	else if(z < 0)
	{
		ret <- hyperg_2F1(a,c-b,c,1-1/(1-z))/(1-z)^a
	}
	else
	{
		library(hypergeo)
		ret <- genhypergeo(U=c(a, b), L=c(c), z=z)
	}
	return(ret)
}

replaceAllNonAlphaNum <- function(strVec, newToken=' ')
{
	gsub("[^[:alnum:] ]", newToken, strVec)
}

# For use with box plot graphing function
drawRects <- function(at, width=3, col=setColor('lightblue', alpha=0.2))
{
	lim <- par('usr')
	for(x in at)
	{
		rect(x-width/2, lim[3], x+width/2, lim[4], border=NA, col=col)
	}
}

is.even <- function(x)
{
	return(x %% 2 == 0)
}

is.odd <- function(x)
{
	x %% 2 != 0
}

# For use with box plot graphing function
draw.p.symbols <- function(p, at.x, at.y=par('usr')[3], xadj=0.75, yadj=0.5, width=2, ...)
{
	if(is.numeric(p))
	{
		text(at.x, at.y + par('cxy')[2]*yadj, getPSymbol(p), adj=c(0, xadj), srt=90, pos=pos, ...)
	}
	else if(is.character(p))
	{
		text(at.x, at.y + par('cxy')[2]*yadj, p, adj=c(0, xadj), srt=90, ...)
	}
}

getBoxPlotAt <- function(n, w)
{
	temp <- data.table(g=rep(1:n, each=w), x=1:(n*w))
	at <- temp[, list(x=mean(x)), by='g']
	return(at)
}

data.table.box.plot <- function(x, ycol, xcol, by, percentile.limits=c(0,1,0,1), legend.plot=T, legend.args=list(), p=NULL, p.cex=0.8, p.adj.x=0.75, p.adj.y=0.5, save.it=F, save.type='png', save.dir=if(save.it){NULL}else{''}, save.file=if(save.it){NULL}else{''}, width=6, height=5, res=300, family='Open Sans Light', ylim=NULL, xlim=NULL, log='', logicle.params=NULL, col=NULL, mar=par('mar'), xlab=xcol, ylab=ycol, range=0, strip.chart=T, strip.method='jitter', strip.jitter=0.2, strip.col.change=-0.6, strip.cex=0.4, rect.col=setColor('black', alpha=0.1), abline.args=NULL, ...)
{
	daFile <- file.path(save.dir, save.file)
	
	# dtemp <- copy(d)
	# d <- copy(dtemp)
	d <- copy(x)
	
	setorderv(d, c(xcol, by))
	
	embedTheFont <- F
	if(save.it)
	{
		embedTheFont <- start.plot.to.file(save.file=file.path(save.dir, paste0(save.file, '.', save.type)), save.width=width, save.height=height, family=family, res=res)
		print(paste0('Saving to file to: ', file.path(save.dir, paste0(save.file, '.', save.type))))
		# png(daFile, width=width, height=height, res=300, units='in')
	}
	
	# Can only transform y axis
	if(grepl('y', log, fixed=T))
	{
		log='y'
	}
	else
	{
		log=''
	}
	
	# Get ylim
	daLegend <- getUniqueCombosAsStringVector(d, idCols=by)
	w <- length(daLegend)
	at <- getBoxPlotAt(n=length(unique(d[[xcol]])), w=w)
	at2 <- copy(at)
	xlim <- getDefault(xlim, c(min(at2$x-w), max(at$x+w)))
	at2[x==min(x), x:=xlim[1]]
	at2[x==max(x), x:=xlim[2]]
	
	# Start the plot
	trans.logit <- c(F,F)
	l(x1, y1, xlim, ylim) %=% start.logicle(x=at2[as.numeric(factor(d[[xcol]]))]$x, y=d[[ycol]], log=log, logicle.params=logicle.params, percentile.limits=percentile.limits, xlab=xlab, ylab=ylab, add=F, mar=mar, trans.logit=trans.logit, ylim=ylim, xlim=xlim, xaxs='i', ...)
	d[, c(ycol):=y1]
	
	las <- getDefault(list(...)$las, 1)
	
	####
	daFormula <- paste(ycol, '~')
	firstBy <- T
	for(daBy in rev(by))
	{
		if(firstBy)
		{
			daFormula <- paste(daFormula, daBy)
			firstBy <- F
		}
		else
		{
			daFormula <- paste(daFormula, '+', daBy)
		}
	}
	if(firstBy)
	{
		daFormula <- paste(daFormula, xcol)
	}
	else
	{
		daFormula <- paste(daFormula, '+', xcol)
	}
	daFormula <- as.formula(daFormula)
	drawRects(at=at[is.odd(at$g)]$x, width=w, col=rect.col)
	
	if(!is.null(abline.args))
	{
		do.call(abline, abline.args)
	}
	
	d <- refactor(d)
	col <- getDefault(col, loopingPastels(1:w, a=0.6))
	col2 <- changeLightness(col, change=strip.col.change)
	temp <- boxplot(daFormula, lex.order=F, xlab='', ylab='', add=T, data=d, outpch=21, outcex=0.5, col=col, axes=F, xlim=xlim, range=range, lty=1, ...)
	if(strip.chart)
	{
		stripchart(daFormula, data=d, pch=16, cex=strip.cex, col=col2, vertical=T, method=strip.method, jitter=strip.jitter, add=T)
	}
	axis(1, at=at$x, labels=unique(d[[xcol]]), las=2)#, ...)
	
	legend.args <- merge.lists(list(x='topright', y=par('usr')[4], pt.cex=1.5, bty='n', legend=daLegend, col='black', pt.bg=col, pch=22), legend.args)
	# legend.x <- getDefault(legend.args$x, 'topright')
	# legend.y <- getDefault(legend.args$y, par('usr')[4])
	# legend.pt.cex <- getDefault(legend.args$pt.cex, 1.5)
	# legend.bty <- getDefault(legend.args$bty, 'n')
	# legend.legend <- getDefault(legend.args$legend, daLegend)
	# legend.col <- getDefault(legend.args$col, 'black')
	# legend.pt.bg <- col
	# legend.pch <- getDefault(legend.args$pch, 22)
	if(legend.plot)
	{
		do.call(legend, legend.args)
	}
	# legend(x=legend.x, y=legend.y, pt.cex=legend.pt.cex, bty=legend.bty, legend=legend.legend, col=legend.col, pt.bg=legend.pt.bg, pch=legend.pch)
	
	if(log=='y')
	{
		drawLogicleAxis(axisNum=2, logicle.params=logicle.params, ...)
	}
	else
	{
		axis(2, ...)
	}
	
	if(!is.null(p))
	{
		if(length(unique(x[[xcol]])) != length(p))
		{
			warning('The length of the vector of p values was not the same as the number of items to plot on the x-axis')
		}
		draw.p.symbols(p=p[1:length(unique(x[[xcol]]))], at.x=at$x, xadj=p.adj.x, yadj=p.adj.y, width=w, cex=p.cex)
	}
	
	box()
	if(save.it)
	{
		if(embedTheFont)
		{
			dev.off2(file.path(save.dir, paste0(save.file, '.', save.type)))
		}
		else
		{
			dev.off()
		}
	}
}

makeGaussKernel <- function(r)
{
	require(tiff)
	r1 <- round(2.25*r,0)
	n <- 2*r1+1
	xx <- matrix(rep(seq(0,n-1,1), each=n), ncol=n)
	yy <- matrix(rep(seq(0,n-1,1), n), ncol=n)
	rr <- sqrt((xx-r1)^2 + (yy-r1)^2)
	# zz1 <- -dnorm(3*rr/r-3, sd=1)
	zz2 <- (dnorm(3*rr/r, sd=1))
	# s1 <- sum(zz1)
	s2 <- sum(zz2)
	# zz1 <- -zz1*s2/s1
	zz <- zz2/s2
	filled.contour(zz2)
	writeTIFF(what=zz, where='/Users/jwarrick/Downloads/kernel.tif', bits.per.sample=16L)
	return(zz)
}

gm <- function(x, na.rm=TRUE)
{
	exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

weighted.gm <- function(x, w, ...)
{
	exp(weighted.mean(log(x), w, ...))
}

markDuplicates <- function(x, by=key(x))
{
	x[, dups:=min(.I), by=by]
}

showDuplicates <- function(x, by=key(x), others=character(0))
{
	ret <- x[, merge.lists(list(n=.N, index=.I, subIndex=1:.N), mget(others)), by=by]
	ret <- ret[n > 1]
	setorderv(ret, by)
	ret[, index:=.I, by=by]
	View2(ret)
	return(ret)
}

makeMovie <- function(full.dir.path='Y:/Jay/R Projects/20181023 - Pt823', in.filename='PhaseDist_%d.png', out.filename='test.mp4', frame.rate=2, overwrite=T, type=c('sh','cmd'))
{
	if(overwrite)
	{
		if(file.exists(file.path(full.dir.path, out.filename)))
		{
			file.remove(file.path(full.dir.path, out.filename))
		}
	}
	da.path <- strsplit(full.dir.path, ':', fixed=T)[[1]]
	cmd <- paste0(paste0(da.path[1], ':'), ' & cd ', shQuote(full.dir.path, type=type[1]), ' & ls & ffmpeg -r 2 -f image2 -s 1920x1080 -i ', shQuote(in.filename, type=type[1]), " -vcodec libx264 -crf 15 -pix_fmt yuv420p ", shQuote(out.filename, type=type[1]))
	print(cmd)
	shell(cmd, intern=F)
}

#' How to make inset subplots...
#' fileToSave <- 'Y:/Jay/R Projects/Dom1/Nuc Amount vs Localization - Zoom - HiRes'
# # fileToSave <- '/Volumes/Miyamoto/Jay/R Projects/Dom1/Nuc Amount vs Localization - Zoom.pdf'
# scale <- 1.5
# nlevels <- 4
# ngrid <- 100
# dev.off()
# png(file=paste0(fileToSave, ' - ', nlevels, '.png'), width=6*scale, height=scale*4, res=600, units='in', family='TT Arial')
# # pdf(file=fileToSave, width=6*scale, height=scale*4, family='sans')
# # dev.off()
# par(mfcol=c(2,3))
# stains <- c('IkBa','RelA','cRel')
# stims <- c(T,F)
# ymins <- c(0.4, 0.4, 0.2, 0.4, 0.5, 0.5)
# ymaxs <- c(4.2, 1.9, 2.1, 1.9, 10.33, 1.9)
# xmins <- c(300, 300, 300, 300, 300, 300)
# xmaxs <- c(875, 495, 810, 495, 920, 495)
# # ylims <- data.table(lo=)
# n <- 1
# i <- 1
# for(stain in stains)
# {
# 	j <- 1
# 	for(stim in stims)
# 	{
# 		par(mfg=c(j,i)) # Choose the subfigure
# 		data.table.plot.all(z[ds %in% DS[1:3] & Stain==stain & Stim==stim], add=F, cex.lab=1.5, main.show=F, mar=c(3.6, 3.6, 1.5, 1.5), oma=c(0,0,0,0), ycol='NNucAmount', yaxs='i', xaxs='i', type='c', las=1, legend=F, h=1, h.lty=2, h.lwd=1, h.col='black', xlim=c(300,920), ylim=c(0.2, 10.3), contour.levels=nlevels, contour.ngrid=ngrid, mgp=c(2.4,1,0), xcol='Geometric.SizeIterable.None.WholeCell', colors=c('firebrick1','gray31'), alpha=0.2, xlab='Cell Area [pixels]', by=c('CellType'), plot.by=c('Stain','Stim'), cross.lwd=3, cross.fun=mean, cross.args=list(na.rm=T), cross.plot=F, ylab='Norm. Nuc. Expression', log='', logicle.params=list(transX=0.0000001, transY=0.01, base=2), legend.cex=1.0)
# 		axis(2, at=1, las=1)
# 		if(!(stain == 'cRel' && stim==T))
# 		{
# 			library(TeachingDemos)
# 			w <- grconvertX(par('usr')[1:2], from='user', to='in')
# 			w <- w[2]-w[1]
# 			h <- grconvertY(par('usr')[3:4], from='user', to='in')
# 			h <- h[2]-h[1]
#			# Have to convert from plot units to inches to specify size of inset
# 			subplot({
# 				par(mfg=c(j,i)) # rechoose the subfigure otherwise things get messed up.
# 				data.table.plot.all(z[ds %in% DS[1:3] & Stain==stain & Stim==stim], cex.lab=1.5, add=F, mar=NULL, main.show=F, ycol='NNucAmount', yaxs='i', xaxs='i', type='c', las=1, legend=F, h=1, h.lty=2, h.lwd=1, h.col='black', xlim=c(xmins[n], xmaxs[n]), ylim=c(ymins[n], ymaxs[n]), contour.levels=nlevels, contour.ngrid=ngrid, xcol='Geometric.SizeIterable.None.WholeCell', colors=c('firebrick1','gray31'), alpha=0.2, xlab='', by=c('CellType'), plot.by=c('Stain','Stim'), cross.lwd=3, cross.fun=mean, cross.args=list(na.rm=T), cross.plot=F, ylab='', log='', logicle.params=list(transX=0.0000001, transY=0.01, base=2), legend.cex=1.0)
# 			}, pars=list(mar=c(2,2,0,0), mgp=c(3.1,1,0), oma=c(0,0,0,0)), size=c(w*0.5, h*0.5), x='topright', hadj=1, vadj=1) # Put mar, mgp, and oma into pars or else things get screwed up.
# 		}
# 		j <- j + 1
# 		n <- n + 1
# 	}
# 	i <- i + 1
# }
# dev.off()

##### In Vivo Project Functions #####

hill <- function(x, top, bottom, b, xmid, s)
{
	return(bottom + (top-bottom)/((1+(x/xmid)^b)^s))
}

hill.par <- function(x, par, ...)
{
	if(length(par)==2)
	{
		return(as.vector(hill(x=x, top=1, bottom=0, b=par[1], xmid=par[2], s=1)))
	}
	if(length(par)==3)
	{
		return(as.vector(hill(x=x, top=1, bottom=par[1], b=par[2], xmid=par[3], s=1)))
	}
	if(length(par)==4)
	{
		return(as.vector(hill(x=x, top=par[1], bottom=par[2], b=par[3], xmid=par[4], s=1)))
	}
	else if(length(par)==5)
	{
		return(as.vector(hill(x=x, top=par[1], bottom=par[2], b=par[3], xmid=par[4], s=par[5])))
	}
	
}

getHillPar <- function(x, y)
{
	duh <- data.table(x=x, y=y)
	setorder(duh, y)
	ylim <- range(y)
	xmid <- x[which(x > mean(ylim))[1]]
	top <- ylim[2]
	bottom <- ylim[1]
	temp <- getPercentileValues(y, c(0.1, 0.9))
	b <- log10(81)/(log10(temp[1]/temp[2]))
	s <- 1
	return(c(top=top, bottom=bottom, b=b, xmid=xmid, s=s))
}

getHillError <- function(par, x, y, ...)
{
	ypred <- hill.par(x, par, ...)
	return(sum((ypred-y)^2))
}

fitHill5 <- function(x, y, ...)
{
	par0 <- getHillPar(x, y)
	daFit <- optim(par=par0, fn=getHillError, x=x, y=y, ...)
	return(daFit$par)
}

fitHill4 <- function(x, y, ...)
{
	par0 <- getHillPar(x, y)
	daFit <- optim(par=par0[1:4], fn=getHillError, x=x, y=y, ...)
	return(daFit$par)
}

fitHill3 <- function(x, y, ...)
{
	par0 <- getHillPar(x, y)
	daFit <- optim(par=par0[2:4], fn=getHillError, x=x, y=y, ...)
	return(daFit$par)
}

fitHill2 <- function(x, y, ...)
{
	par0 <- getHillPar(x, y)
	daFit <- optim(par=par0[3:4], fn=getHillError, x=x, y=y, ...)
	return(daFit$par)
}

data.table.expand.grid <- function(..., KEEP.OUT.ATTRS=T, stringsAsFactors = T)
{
	ret <- data.table(expand.grid(..., KEEP.OUT.ATTRS=KEEP.OUT.ATTRS, stringsAsFactors=stringsAsFactors))
	return(ret)
}

# Fitting the distribution
LL <- function(lambda, avg.size, x)
{
	R <- dPolyaAeppli(x=x, lambda=lambda, prob=1-1/(avg.size), log=T)
	return(-sum(R))
}

fitDist <- function(x)
{
	library(stats4)
	fit <- mle(LL, start = list(lambda = mean(x), avg.size=1.5), fixed = list(x=x), method = "L-BFGS-B", lower = c(0.01,1.01), upper = c(10,4))
	avg.size <- coef(fit)['avg.size']
	lambda <- coef(fit)['lambda']
	duh <- sqrt(diag(vcov(fit)))
	return(list(lambda=lambda, avg.size=avg.size, lambda.err=duh['lambda'], avg.size.err=duh['avg.size']))
}

myMAD <- function(x, na.rm=F)
{
	ret <- mad(x, na.rm=na.rm, constant=1)
	if(!any(is.finite(x)))
	{
		stop('No finite values to use for calculation. Aborting')
	}
	if(ret == 0)
	{
		ret <- sd(x)/1.4826
	}
	return(ret)
}

cutForMids <- function(x, n)
{
	breaks0 <- cut(x, breaks=n)
	breaks <- unlist(strsplit(unlist(strsplit(levels(breaks0), split='(', fixed=T)), split=']', fixed=T))
	breaks <- breaks[breaks != '']
	breaks <- uniqueo(as.numeric(unlist(strsplit(breaks, split=','))))
	mids <- (breaks[1:(length(breaks)-1)]+breaks[2:(length(breaks))])/2
	breaks0 <- as.numeric(as.character(factor(breaks0, levels=levels(breaks0), labels=mids)))
	return(list(x=breaks0, breaks=breaks, mids=mids))
}

Cox.SampleSize <- function(formula, data, alpha, beta, HR, frac0=0.5, test=c('all','2-tail','1-tail','equivalence'))
{
	res<-get.PE.PC(formula=formula, data=data, RR=HR)
	pE<-res$pE
	pC<-res$pC
	
	ret <- Cox.SampleSize.default(alpha=alpha, beta=beta, HR=HR, p0=pC, p1=pE, frac0=frac0, test=test)
	for(daName in names(ret))
	{
		ret[[daName]] <- merge.lists(ret[[daName]], list(p0=pC, p1=pE))
	}
	return(ret)
}

Cox.SampleSize.default <- function(alpha, beta, HR, p0, p1, frac0=0.5, test=c('all','2-tail','1-tail','equivalence')) 
{
	# alpha = 0.05
	# beta = 1-power
	# HR0 HR of control (i.e., what is being discriminated against)
	# HR1 HR of non-inferior group (typically slightly lower than HR0, assuming higher HR is worse)
	# p0 probability of observing event in control grp
	# p1 probability of observing event in non-inferior grp
	# frac0 the fraction of samples in the control grp (default is 0.5, i.e. 50%)
	
	d <- p0*frac0 + p1*(1-frac0)
	n2a <- (qnorm(alpha/2) + qnorm(beta))^2/((log(HR))^2 * frac0 * (1-frac0) * d)
	n2b <- (qnorm(alpha) + qnorm(beta))^2/((log(HR))^2 * frac0 * (1-frac0) * d)
	n2c <- (qnorm(alpha) + qnorm(beta/2))^2/((log(HR))^2 * frac0 * (1-frac0) * d)
	ret <- list(two.tailed=list(n=n2a, n0=frac0*n2a, n1=(1-frac0)*n2a), one.tailed=list(n=n2b, n0=frac0*n2b, n1=(1-frac0)*n2b), equivalence=list(n=n2c, n0=frac0*n2c, n1=(1-frac0)*n2c))
	if(test[1]=='two.tail')
	{
		return(ret$two.tailed)
	}
	else if(test[1]=='one.tailed')
	{
		return(ret$one.tailed)
	}
	else if(test[1]=='equivalence')
	{
		return(ret$equivalence)
	}
	else if(test[1]=='all')
	{
		return(ret)
	}
	else
	{
		stop('The name of the test is not recognized. Aborting.')
	}
}

# dat - a data frame with at least three columns: time, status, and x
# formula - e.g. Surv(time, status) ~ x, where x indicates which group a subject is in
# x can take only two possible values: "C" (control group) and "E" (Treatment group)
# and x should be a factor object of R
get.PE.PC<-function(formula, data, RR=1.5)
{
	fit<-survfit(formula=formula, data=data)
	
	# Pr (survive to time t_i | survived up to time t_{i-1})
	surv<-fit$surv
	strata<-fit$strata
	
	str<-names(strata)
	str1<-unlist(strsplit(str[1], split="="))[2]
	str2<-unlist(strsplit(str[2], split="="))[2]
	if(str1 != "C" || str2 != "E")
	{
		stop("group variable should take only two possible values: C and E")
	}
	
	if(str1 == "E")
	{ nE<-strata[1]
	nC<-strata[2]
	nTotal<-nE+nC
	set<-1:nTotal
	pos.E<-1:nE
	pos.C<-set[-pos.E]
	#surv.E<-surv[1:nE]
	#surv.C<-surv[-(1:nE)]
	} else if (str1 == "C") {
		nE<-strata[2]
		nC<-strata[1]
		nTotal<-nE+nC
		set<-1:nTotal
		pos.C<-1:nC
		pos.E<-set[-pos.C]
		
		#surv.E<-surv[1:nE]
		#surv.C<-surv[-(1:nE)]
	}
	
	surv.E<-surv[pos.E]
	surv.C<-surv[pos.C]
	
	# number of failed at time t
	#n.event.C<-fit$n.event[-c(1:nE)]
	n.event.C<-fit$n.event[pos.C]
	# number of at risk at time t 
	#  = number of failed  at time t  
	#    + number of alived at time t 
	#    + number of censored at time t
	#n.risk.C<-fit$n.risk[-c(1:nE)]
	n.risk.C<-fit$n.risk[pos.C]
	
	nTimes<-length(surv.C)
	nTimes1<-nTimes-1
	n.censored.C<-rep(0, nTimes)
	
	for(i in  1:nTimes1)
	{ 
		# number of censored + number of  alived
		tmp <-n.risk.C[i]-n.event.C[i] 
		n.censored.C[i] <-tmp-n.risk.C[i+1]
	}
	n.censored.C[nTimes]<-n.risk.C[nTimes]-n.event.C[nTimes]
	n.survive.C<-n.risk.C-n.event.C-n.censored.C
	
	lambda.C<-rep(0, nTimes)
	lambda.C[1]<- n.survive.C[1]/n.risk.C[1]
	for(i in 2:nTimes)
	{
		lambda.C[i]<-(n.survive.C[i-1]-n.event.C[i])/n.survive.C[i-1]
	}
	lambda.C<-1-lambda.C
	
	RRlambda.C<-RR*lambda.C
	
	#times<-fit$time[1:nC]
	times<-fit$time[pos.C]
	mat.event<-cbind(times, n.event.C, n.censored.C, n.survive.C, n.risk.C)
	mat.event<-rbind(c(0, 0, 0, 0, n.risk.C[1]), mat.event)
	colnames(mat.event)<-c("time", "nEvent.C", "nCensored.C", "nSurvive.C", "nRisk.C")
	
	
	delta.C<-n.censored.C/(n.censored.C+n.survive.C)
	
	
	lambda.C1<-lambda.C[1]
	A<-rep(0, nTimes)
	B<-rep(0, nTimes)
	C<-rep(0, nTimes)
	
	A[1]<-1
	B[1]<-1
	C[1]<-1
	for(i in 2:nTimes) 
	{
		i1<-i-1 
		A[i]<-prod(1-lambda.C[1:i1])
		B[i]<-prod(1-RRlambda.C[1:i1])
		C[i]<-prod(1-delta.C[1:i1])
	}
	
	D<-lambda.C*A*C
	E<-RRlambda.C*B*C
	
	pC<-sum(D)
	pE<-sum(E)
	
	
	mat<-cbind(times, lambda.C, RRlambda.C, delta.C, A, B, C, D, E)
	mat2<-rbind(c(0, 0, 0, 0, NA, NA, 1.0, NA, NA), mat)
	colnames(mat2)<-c("time", "lambda", "RRlambda", "delta", "A", "B", "C", "D", "E")
	
	res<-list(mat.lambda=mat2, mat.event=mat.event, pC=pC, pE=pE)
	
	return(res)
}