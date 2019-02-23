library(foreach)
p = 0.005*1:200

r = foreach(i = p)%do%tryCatch({list(p=i,ts=ecdf(get(load(paste('RData/res_CRYSTAL43-1-50000-100-',i,'-0-0-0.RData',sep='')))[[1]]$ts))}, error=function(ex){NA})
r = r[1<foreach(x=r,.combine=c)%do%length(x)]

x = -log(seq(exp(0.00001))

plot(




library(foreach)
fname = 'RData/res_CRYSTAL43-@n-50000-100-@a-0-0-0.RData'
ps =0.005*1:200

rs = foreach(p=ps)%do%{
	r = foreach(n=c(1:21),.combine=c)%do%
		tryCatch({get(load(gsub('@n',n,gsub('@a',p,fname))))[[1]]$ts}, error=function(e){c()})
	r = c(r[r<0],-r[r>0])
	print(p)
	r
}


fname1 = 'C:/R/r-prog/RData/res_CRYSTAL42-1e+05-30-@a-0-0-0.RData'
ps1 =0.02*1:50

rs1 = foreach(p=ps1)%do%{
	r = get(load(gsub('@a',p,fname1)))[[1]]$ts
	#r = c(r[r<0],-r[r>0])
	print(p)
	r
}


es = foreach(i=1:length(rs))%do%{
	#list(q1=quantile(r,0.001), q2=quantile(r,0.4999), rng=range(r[r>quantile(r,0.0001)]), e=ecdf(r), h=hist(r,br=1000,plot=FALSE))
	r = rs[[i]]
	#r1 = -rs[[i]]/quantile(rs[[i]],0.5)
	print(i)
	list(q11=quantile(r,0.0001), q12=quantile(r,0.9), e1=ecdf(r))#,
#		q21=quantile(r1,0.0001), q22=quantile(r1,0.9), e2=ecdf(r1))#, h=hist(r,br=1000,plot=FALSE))
}

es2 = foreach(i=1:length(rs))%do%{
	print(i)
	list(q1=quantile(rs[[i]],0.0001), q2=quantile(rs[[i]],0.3))
}

#xs=abs(c(es[[1]]$q11,es[[1]]$q12)); ec=es[[1]]$e; plot_func=plot
plot_e = function(xs, ec, plot_func){
	x = exp(seq(log(xs[1]),log(xs[2]),len=200))
	plot_func(log(x),log(ec(-x)),t='l',xlim=c(-8,11))
}

lm_e = function(xs, ec){
	x0 = exp(seq(log(xs[1]),log(xs[2]),len=200))
	x = log(x0)
	y = log(ec(-x0))
	lm(y~x)$coeff[2]
}

cv_e = function(xs, ec, xmax = 100){
	x0 = exp(seq(log(xs[1]),log(xs[2]),len=200))
	x0 = x0[x0<xmax]
	x = log(x0)
	y = log(ec(-x0))
	cor(x,y)
}


#xx = c(0.000001,0.003)
#xx = c(0.00002,1.9)
plot_e(abs(c(es2[[1]]$q1,es2[[1]]$q2)),es[[1]]$e,plot)
foreach(i=2:200)%do%plot_e(abs(c(es2[[i]]$q1,es2[[i]]$q2)),es[[i]]$e,lines)

m = as.numeric(foreach(i=1:185,.combine=c)%do%lm_e(abs(c(es2[[i]]$q1,es2[[i]]$q2)),es[[i]]$e))
mcov = as.numeric(foreach(i=1:200,.combine=c)%do%cv_e(abs(c(es2[[i]]$q1,es2[[i]]$q2)),es[[i]]$e,5))
plot(mcov)
which(mcov>-0.95)

#xx = c(0.005,0.012)
x = exp(seq(log(xx[1]),log(xx[2]),len=200))
#x = exp(seq(log(xx[1]),log(-e[[1]]$rng[1]),len=200))
plot(log(x),log(e[[1]]$e(-x)))

plot(es[[200]]$h)
r = rs[[7]]
hist(r[abs(r)<10],br=200)


plot(x,log(e[[1]]$e(-x)))
plot(x,log(e[[1]]$e(-x)))
plot(x,e[[1]]$e(-x))
plot(e[[1]]$h$mids,e[[1]]$h$density)
hist(get(load(gsub('@a',0.005,fname)))[[1]]$ts,br=1000)

r = e[[1]]$

