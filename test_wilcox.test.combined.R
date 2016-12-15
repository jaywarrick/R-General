
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

### Test if identical data in reverse order produces same statistical significance. It shouldn't

# This should be significant
dummy <- data.table(expt=c(1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2), cond=c(1,1,1,1,1,2,2,2,2,2,1,1,1,1,1,2,2,2,2,2), val=c(1,2,3,4,5,3,4,5,6,7,1,2,3,4,5,3,4,5,6,7))
wilcox.test.combined(data=dummy, replCols = 'expt', condCol = 'cond', valCol = 'val')

# This should not be significant
dummy <- data.table(expt=c(1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2), cond=c(1,1,1,1,1,2,2,2,2,2,1,1,1,1,1,2,2,2,2,2), val=c(1,2,3,4,5,3,4,5,6,7,3,4,5,6,7,1,2,3,4,5))
wilcox.test.combined(data=dummy, replCols = 'expt', condCol = 'cond', valCol = 'val')