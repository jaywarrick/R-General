library(data.table)
# Requires source('/Users/jaywarrick/Documents/GitHub/R-General/.Rprofile')
# Requires source('/Users/jaywarrick/Documents/GitHub/R-Cytoprofiling/PreProcessingHelpers.R')

descaleTime <- function(x, t, n, force.t0=T)
{

	if(n < 2)
	{
		stop('n must be greater than 2 or more')
	}
	if(length(x) != length(t))
	{
		stop('x and t must be same length')
	}
	if(n >= length(x))
	{
		stop('n must be < length of x')
	}

	r <- length(x)
	ret <- data.table(x=x, t=t)
	ret <- ret[, ':='(x.diffs=cumsum(c(0, abs(x[2:r] - x[1:(r-1)]))), t.diffs=(t-min(t)))]

	ret[, x.diffs:=if(x.diffs[.N] == 0){0:(.N-1)}else{x.diffs}]
	ret[, t.i:=x.diffs %/% (x.diffs[.N]/(n))]
	t.i.max <- max(ret$t.i)
	ret[t.i == n, t.i:=n-1]

	if(force.t0)
	{
		ret <- ret[, list(x=mean(x), t.mean=mean(t.diffs), t.n=.N), by=.(t.i)]
	}
	else
	{
		ret <- ret[, list(x=mean(x), t.mean=mean(t), t.n=.N), by=.(t.i)]
	}
	return(ret)
}

interpolate <- function(x)
{
     if(sum(!is.na(x))==0)
     {
          # Then all data is NA and can't interpolate so just return x
          return(x)
     }
	x.na.i <- which(is.na(x))
	x.starts <- which(!is.na(x))[(which(!is.na(x)) %in% (x.na.i-1))]
	x.ends <- which(!is.na(x))[which(!is.na(x)) %in% (x.na.i+1)]
	if(length(x.starts) != length(x.ends))
	{
		stop("The number of indicies don't match")
	}
	x.n <- x.ends - x.starts
	x.deltas <- (x[x.ends]-x[x.starts])/x.n
	for(i in seq_along(x.starts))
	{
		x[x.starts[i]:x.ends[i]] <- x[x.starts[i]] + x.deltas[i] * 0:(x.ends[i]-x.starts[i])
	}
	return(x)
}

descaleTime.SD <- function(SD, t.col, n, by=NULL, cols=getAllColNamesExcept(SD, c(t.col, by)))
{
	cols <- cols[!cols %in% c(t.col, by)]
	ret <- NULL
	for(col in cols)
	{
		temp <- SD[, descaleTime(x=get(col), t=get(t.col), n=n), by=by]
		toRename <- getAllColNamesExcept(temp, c(t.col, by, 't.i'))
		setnames(temp, toRename, paste0(col, '.', toRename))
		if(is.null(ret))
		{
			ret <- temp
		}
		else
		{
			ret <- merge(ret, temp, by=c(by, 't.i'), use.names=T)
		}
	}
	return(ret)
}

descaleTime.data.table <- function(x, t.col, n, by=NULL, cols=getAllColNamesExcept(x, c(t.col, by)))
{
	# get rid of integers, converting them to doubles to avoid random conversion issues.
	lapply.data.table(x, FUN=as.double, in.place = T)

	# order by time
	setorderv(x, c(by, t.col))

	# unscale time from signals
	ret <- descaleTime.SD(SD=x, t.col=t.col, n=n, cols=cols, by=by)

	# fill missing rows in the output (i.e., missing time indicies)
	ret <- fillMissingRows(ret, cols=c(by,'t.i'), fill=NA)

	# set t.n to zero at interpolated points
	lapply.data.table(ret, FUN=function(x){x[is.na(x)] <- 0; return(x)}, cols=getColNamesContaining(ret, 't.n'), in.place = T)

	# interpolate NA's for other numeric data.
	lapply.data.table(ret, FUN=interpolate, by=by, in.place=T)
	
	# Any NAs left after interpolation can't be fixed and were just introduced most likely by 'fillMissingRows', so just remove.
	for(col in getAllColNamesExcept(ret, c(by, 't.i')))
	{
	     ret <- ret[!is.na(get(col))]
	}
	
	return(ret)
}

getTimeCorrelations.SD <- function(SD, tCol, idCols, valName=getAllColNamesExcept(SD, c(idCols, tCol)))
{
	if(length(valName) > 1)
	{
		stop("This function is intended to work on only one value column at a time. Choose only a single valName.")
	}
	ret <- reorganize(SD, measurementCols=idCols, valueCols=valName)
	ret <- as.matrix(ret[, getAllColNamesExcept(ret, tCol), with=F])
	ret <- getSortedCorrelations(bigcor(ret))
	ret$M0 <- valName
	setcolorder(ret, c('M0','M1','M2','cor'))
	return(ret)
}

getTimeCorrelations.data.table <- function(x, tCol, idCols, mCols=getAllColNamesExcept(x, c(tCol, idCols)))
{
	tList <- list()
	for(mCol in mCols)
	{
		tList[[mCol]] <- getTimeCorrelations.SD(x[, c(idCols, tCol, mCol), with=F], tCol=tCol, idCols=idCols)
	}
	return(rbindlist(tList, use.names=T))
}

#' Calculate distances from correlations of different metrics
#'
#' @param convert boolean whether to convert output to a matrix
getTimeCorrelationDist <- function(corTable, x=T, t.mean=T, t.n=F, convert=T)
{
	ret <- copy(corTable)
	ret[, cor:=sim.transform(cor)]
	replaceInf <- function(x)
	{
	     x[!is.finite(x)] <- max(x[is.finite(x)])
	     return(x)
	}
	ret[, cor:=replaceInf(cor), by='M0']
	ret[, cor:=abs(cor-max(cor)), by=.(M0)]

	x.m <- grepl('.x', ret$M0, fixed=T) & x
	x.t.mean <- grepl('.t.n', ret$M0, fixed=T) & t.mean
	x.t.n <- grepl('.t.mean', ret$M0, fixed=T) & t.n
	validRows <- which(x.m | x.t.mean | x.t.n)

	ret <- ret[validRows, list(cor=sum(cor^2)), by=.(M1,M2)]

	if(convert)
	{
		ids <- unique(c(ret$M1, ret$M2))
		ids <- sort(ids)
		ret <- rbindlist(list(ret, data.table(M1=ids, M2=ids, cor=0)))
		fillMissingRows(ret, cols=c('M1','M2'), fill=as.double(0))
		ret <- reorganize(ret, measurementCols='M2', valueCols = 'cor')
		ret <- as.matrix(ret[, ids, with=F])
		row.names(ret) <- ids
		ret <- makeSymmetric(ret)
	}

	return(ret)
}

makeSymmetric <- function(mat, fill.upper=T)
{
	NROW <- nrow(mat)
	NCOL <- ncol(mat)
	if(NROW != NCOL)
	{
		stop('Matrix must be square to use this function.')
	}

	if(fill.upper)
	{
		for(r in 1:NROW)
		{
			for(c in r:NCOL)
			{
				mat[r,c] <- mat[c,r]
			}
		}
	}
	else
	{
		for(c in 1:NCOL)
		{
			for(r in c:NROW)
			{
				mat[r,c] <- mat[c,r]
			}
		}
	}
	return(mat)
}
