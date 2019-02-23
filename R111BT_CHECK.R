library(xts)
r = get(load('bt02.RData'))
pn=floor(1000*sign(r$fn-r$bsn)*(r$pn-r$bsn*r$spcurr)/(r$bs*r$spcurr))/1000
pp=floor(sign(r$f-r$bs)*(r$p-r$bs*r$spcurr)/(r$bs*r$spcurr))/1000

pn=floor(1000*sign(r$fn-r$bsn)*(r$pn-r$bsn*r$spcurr))/1000
pp=floor(1000*sign(r$f-r$bs)*(r$p-r$bs*r$spcurr))/1000

pn = pn[!is.na(pn)]
pp = pp[!is.na(pp)]
plot(cumsum(pn))
r[160:170,]

X = 1000
#pn=sign(r$bsn-r$fn)*(r$pn-r$fn*r$spcurr)   #/(X - 0.23*r$fn*r$spcurr)
pn=ifelse(r$bsn>r$fn,1,0)*(r$pn-r$fn*r$spcurr)   #/(X - 0.23*r$fn*r$spcurr)
res = X + cumsum(pn[floor(seq(1,length(pn),by=15))])
res = res/lag(res,1)
res=res[!is.na(res)]
plot(exp(cumsum(log(res))))

pn=ifelse(r$bsn>r$fn,1,-0.1)*(r$pn-r$fn*r$spcurr)   #/(X - 0.23*r$fn*r$spcurr)
plot(cumsum(pn[floor(seq(1,length(pn),by=10))]))

r = get(load('bt01.RData'))
plot(cumsum(r$p-r$bs*r$spcurr))

inverse = function (f, lower = -1, upper = 1) {
   function (y) uniroot((function (x) f(x) - y), lower = lower, upper = upper)[1]
}

d = distrib_fit; d = d[abs(d)<1]
e = ecdf(d)
ie = inverse(e)
ia = function(n){foreach(k=runif(n),.combine=c)%do%as.numeric(ie(k))}
hist(d,br=1000)
r1 = d; r2 = ia(100000)
h1 = hist(r1,plot=FALSE,br=seq(-1,1,len=1000))
h2 = hist(r2,plot=FALSE,br=seq(-1,1,len=1000))
plot(h1$mids,h1$density,xlim=c(-0.1,0.1))
lines(h2$mids,h2$density,xlim=c(-0.1,0.1),col='blue')

d1 = foreach(i=1:100000,.combine=c)%do%sum(d[i:(i+15)])
d2 = exp(d*sqrt(15))-1
sd(d1)
sd(d2)
mean(ifelse(d1< -0.01, -0.01 - d1, 0))
mean(ifelse(d2< -0.01, -0.01 - d2, 0))










