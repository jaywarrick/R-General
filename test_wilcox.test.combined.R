
wilcox.test.combined <- function(data, replCols, condCol, valCol, two.tailed=TRUE)
{
     require(data.table)
     x1 <- data.table(data)

     getStats <- function(x, y, ...)
     {
          if(length(x) == 0 || length(y) == 0)
          {
               # This results in a missing row in the results details table for the experiment with either missing x or y data
               return(NULL)
          }
          temp <- wilcox.test(x=x, y=y, ...)
          W <- as.numeric(temp$statistic)

          counts <- table(c(x, y))

          n <- min(length(x), length(y))
          m <- max(length(x), length(y))
          N <- n + m

          # Taken from R source code for wilcox.test
          z <- W - n * m / 2
          z <- z - sign(z)*0.5
          SIGMA <- sqrt((n * m / 12) * ((n + m + 1) - sum(counts^3 - counts) / ((n + m) * (n + m - 1))))
          z <- z/SIGMA

          p.approx <- 2*pnorm(-abs(z))
          return(list(W=W, p.value=temp$p.value, N=length(x) + length(y), E=n * m / 2, V=SIGMA^2, p.approx=p.approx))
     }

     x2 <- x1[,getStats(x=.SD[get(condCol)==1][[valCol]], y=.SD[get(condCol)==2][[valCol]]), by=replCols]

     x2[,':='(Wi=W/(N+1), Ei=E/(N+1), Vi=V/((N+1)^2)), by=.(expt)]

     Wtot <- sum(x2$Wi)
     Etot <- sum(x2$Ei)
     Vtot <- sum(x2$Vi)

     if(two.tailed)
     {
          p.overall <- 2*pnorm(-abs((Wtot-Etot)/(sqrt(Vtot))))
     }
     else
     {
          p.overall <- pnorm(-abs((Wtot-Etot)/(sqrt(Vtot))))
     }

     return(list(details=x2, p.overall=p.overall, alpha.prime=1-(1-p.overall)^(nrow(x2))))
}

# duh <- data.frame(yo=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1), expt=c(1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2), cond=c(1,1,1,1,1,2,2,2,2,2,1,1,1,1,1,2,2,2,2,2), val=c(0.1,1,1.5,2,2.5,2,2.45,3,4,5,0.5,1,1.5,2,2.5,2,2.6,3,4,5))
#
# duh2 <- wilcox.test.combined(duh, replCols=c('yo','expt'), condCol='cond', valCol='val', two.tailed=FALSE)
# print(duh2)

eN <- 6
repN <- 3
vals <- c()
for(a in 1:eN)
{
     vals <- c(vals, rnorm(repN, mean=0, sd=1), rnorm(repN, mean=1, sd=1))
}
duh <- data.table(expt=rep(1:eN, each=repN*2), cond=rep(rep(c(1,2), each=repN), eN), val=vals)
duh <- duh[sample.int(n=nrow(duh), size=round(0.8*nrow(duh)))]
wilcox.test.combined(data=duh, replCols=c('expt'), condCol='cond', valCol='val', two.tailed=TRUE)

temp <- duh[expt==3 & cond==1]$val
plot(rep(0, length(temp)), temp, xlim=c(-1,1), ylim=c(-3,3), pch=20, col=rgb(0,1,0,0.3))
temp <- duh[expt==3 & cond==2]$val
points(rep(0, length(temp)), temp, pch=20, col=rgb(1,0,0,0.3))
