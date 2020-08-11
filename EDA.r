data = read.csv('SP500.csv')
n = length(data[,1])
year_SP = 1981 + (1:n) * (1991.25 - 1981) / n 
plot(year_SP, data[,1], main = "S&P 500 daily returns",
    xlab = "year", type = "l", ylab = "log return")

plot(ecdf(data[,1]))

SPreturn = data[,1]

# histogram is a simple and well-known estimator
# of probability density functions
par(mfrow = c(2, 2))
hist(SPreturn, breaks=30, xlab="return",
    main="(a) 30 bins, full range")
hist(SPreturn, breaks=30, main=" (b) 30 bins, central range",
    xlim=c(-.04,.04), xlab="return") #,cex.main=1.5)
hist(SPreturn, breaks=20, main=" (c) 20 bins, central range",
    xlim=c(-.04,.04),xlab="return",)
hist(SPreturn, breaks=50, main=" (d) 50 bins, central range",
    xlim=c(-.04,.04), xlab="return")

# KDE using density() function are useful to
# estimate the true density of the distribution
# the parameter adjust can be used to fine-tune
# the bandwidth selected automatically
plot(density(data[,1], adjust=1), 
            xlim=c(-0.05,0.05), 
            lty='dashed',
            main="KDE - S&P500 returns")
lines(density(data[,1], adjust=1/3), lty='dotted')
lines(density(data[,1], adjust=3))
legend("topleft", legend=c("adjust=1", "adjust=1/3", "adjust=3"),
        lty=c('dashed','dotted','solid'))


par(mfrow=c(1,2))
plot(density(data[,1],kernel="gaussian",adjust=1),xlim=c(-.06,.06),
    main="(a) standard estimates",lwd=2,cex.lab=1.5,cex.axis=1.5,
    ylim=c(0,63),cex.main=1.5,lty=1)
z=seq(from=-.06,to=.06,by=.001)
lines(z,dnorm(z,mean=mean(data[,1]),sd=sd(data[,1])), 
        lty=2, lwd=3, cex.lab=1.5, cex.axis=1.5, col="red")
legend('topleft',c('estimated density','normal density'),
        lty=c(1,2),cex=1.5,box.lty=0,lwd=2,col=c("black","red"))
plot(density(data[,1],kernel="gaussian",adjust=1),xlim=c(-.06,.06),
    main="(b) robust estimates",lwd=2,cex.lab=1.5,cex.axis=1.5,
    ylim=c(0,63),cex.main=1.5,lty=1)
# using more robust estimators like median and MAD 
# give better fit using normal density
lines(z,dnorm(z,mean=median(data[,1]),sd=mad(data[,1])), 
        lty=2, lwd=3, cex.lab=1.5, cex.axis=1.5, col="red")
legend('topleft',c('estimated density','normal density'),
        lty=c(1,2),cex=1.5,box.lty=0,lwd=2,col=c("black","red"))


# we can also compare the empirical (sample) CDF
# with a theoretical CDF (e.g. gaussian)
set.seed("991155")
edf_norm=ecdf(rnorm(150))
par(mfrow=c(1,1))
plot(edf_norm, verticals=TRUE, do.p=FALSE,
    main="EDF and CDF")
tt=seq(from=-3, to=3, by=.01)
lines(tt,pnorm(tt),lty=2,lwd=2,col="red")

# normal probability plots
# qqnorm() function
# if normal plot non linear
# 1. plot is convex => LEFT SKEWNESS
# 2. plot is concave => RIGHT SKEWNESS
# 3. plot is convex-concave => HEAVY TAILS
# 4. plot is concave-convex => LIGHT TAILS
# use qqline() to plot a line
qqnorm(data[,1],datax=T)
qqline(data[,1],datax=T)

# boxplot useful for comparing several samples
# R function boxplot()
boxplot(data[,1], main="S&P500 returns")

data(EuStockMarkets)
mode(EuStockMarkets)
class(EuStockMarkets)
plot(EuStockMarkets)
# can use pdf("EuStocks.pdf",width=6,height=5)
# plot(EuStockMarkets)
# graphics.off()
# to print and save as pdf

logR = diff(log(EuStockMarkets))
plot(logR)

# plot data stored as a dataframe
plot(as.data.frame(logR))

# testing normality
# using qq plots
# and Shapiro-Wilk test
par(mfrow=c(2,2))
for(i in colnames(logR))
{ 
    qqnorm(logR[,i], datax=T, main=i)
    qqline(logR[,1], datax=T)
    print(shapiro.test(logR[,i]))
}

# create t-plots with different df
n=dim(logR)[1]
q_grid = (1:n) / (n + 1)
df_grid = c(1, 4, 6, 10, 20, 30)
index.names = dimnames(logR)[[2]]
for(i in 1:4)
{
    dev.new()
    par(mfrow=c(3,2))
    for(df in df_grid)
    {
        qqplot(logR[,i], qt(q_grid,df),
            main=paste(index.names[i], ", df =", df))
        abline(lm(qt(c(0.25, 0.75), df = df) ~
            quantile(logR[,i], c(0.25, 0.75))))
    }
}

library("fGarch")
x=seq(-.1, .1, by=.001)
par(mfrow=c(1,1))
df = 5
mad_t = mad(logR[,1],
    constant=sqrt(df / (df-2)) / qt(.75, df))
plot(density(logR[,1]), lwd=2, ylim=c(0,60))
lines(x, dstd(x,mean=mean(logR[,1]), sd=mad_t, nu=df),
    lty=5, lwd=2, col="red")
lines(x, dnorm(x,mean=mean(logR[,1]),sd=sd(logR[,1])),
    lty=3, lwd=4, col="blue")
legend("topleft",c("KDE", paste("t:df = ",df), "normal"),
    lwd=c(2,2,4), lty=c(1,5,3),
    col=c("black", "red", "blue"))

data = read.csv('MCD_PriceDaily.csv')
head(data)
adjPrice = data[,7]
plot(adjPrice, type="l",lwd=2)

logR = diff(log(adjPrice))
plot(logR, type="l",lwd=2)

hist(logR, 80, freq=FALSE)

qqnorm(logR,datax=T)
qqline(logR,datax=T)

# analyze Ford stock returns data
data = read.csv('Ford.csv')
head(data)
tail(data)
price=data[,3]
plot(price,type="l",lwd=2)
mean(price)
median(price)
sd(price)

qqnorm(price,datax=T)
qqline(price,datax=T)

print(shapiro.test(price))

