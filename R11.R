####library(quantmod)
####options(download.file.method="wget")
####options(download.file.method="wininet")
####library(quantmod)
####getSymbols(c('DGS10','SP500','USD3MTD156N'),src='FRED')
####getSymbols('^VIX')
####snp = diff(log(get('SP500'))); trs = get('DGS10'); lbr = get('USD3MTD156N'); vix0=get('VIX')$VIX.Close/(100*(12*30)^0.5)

my_get_symbol = function(name){
	system(paste('wget --no-check-certificate "https://research.stlouisfed.org/fred2/series/',name,'/downloaddata/',name,'.csv" -O "C:/R/',name,'.csv"',sep=''))
	dat = read.csv(paste(name,'.csv',sep=''),stringsAsFactors=FALSE)
	xts(as.numeric(dat[,2]),order.by=as.Date(dat[,1])) 
}
getSymbols('^VIX')
snp = diff(log(my_get_symbol('SP500'))); trs = my_get_symbol('DGS10'); lbr = my_get_symbol('USD3MTD156N'); vix0=get('VIX')$VIX.Close/(100*(12*30)^0.5)
 

source('R121.R')

library(doParallel)
cl <- makePSOCKcluster(2)
registerDoParallel(cores=30)
load('ids0xts.RData')
snp = ids0[,'SP500']; trs = ids0[,'DGS10']; lbr = ids0[,'USD3MTD156N']; vix0=ids0[,'VIX.Close']/(100*(12*30)^0.5)
#ids0=merge.xts(snp,trs,lbr,get('VIX')$VIX.Close); colnames(ids0)=c('SP500','DGS10','USD3MTD156N','VIX.Close'); save(ids0,file='ids0xts.RData')

ids = merge.xts(snp,trs,lbr); 
CUTOFF_DATE = '2007-05-13'
#CUTOFF_DATE = as.Date('2009-03-01')
ids = na.locf(ids)[index(ids)>CUTOFF_DATE,] # VIX is not included in Indices list, used just for sd!
vix0 = na.locf(vix0)[index(vix0)>CUTOFF_DATE,]
ids = ids[index(ids)%in%index(vix0),]
vix0 = vix0[index(vix0)%in%index(ids),]

idss = ids[,'USD3MTD156N']
# -- 20150722 -- #for(i in 1:dim(ids)[2]) idss[,i]=(ids[,i]-mean(ids[,i]))/sd(ids[,i])


Q_CNT <<- 21; 	

if(FALSE){
	#params1 = list(3*1:33,0,1+0.03*1:67,0.5,0.1); gparams = as.matrix(expand.grid(params1))
	#params1 = list(9*1:11,0,1+0.09*1:22,0.5,0.1); gparams = as.matrix(expand.grid(params1))
	params1 = list(30,0.02*1:50,0,0,0); gparams = as.matrix(expand.grid(params1))
	d = 0
	#dats = foreach(i=1:1)%dopar%{
	dats = foreach(i=1:nrow(gparams))%dopar%{
	#dats = for(i in 1:nrow(gparams)){
		p = as.character(gparams[i,])
			print(p); 
			#d = get(load(paste('RData\\res_CRYSTAL-2e+05-',paste(gparams[i,],collapse='-'),'.RData',sep=''))) 
			d = get(load(paste('RData\\res_CRYSTAL42-1e+05-',paste(gparams[i,],collapse='-'),'.RData',sep=''))) 
	
			###d = get(load(paste('RData\\res_CRYSTAL-2e+05-',paste(gparams[i,],collapse='-'),'.RData',sep=''))) 
			###list(data=d[[1]]$ts/sd(d[[1]]$ts), params=d[[1]]$params[c(1,3)])
			###list(data=d[[1]]$ts/sd(d[[1]]$ts), params=c(gparams[i,1],d[[1]]$params[2]))
	
			#list(data=d[[1]]$ts, params=c(gparams[i,1],d[[1]]$params[2]))
			#list(quant=as.numeric(quantile(d[[1]]$ts, seq(0,1,len=Q_CNT))), params=c(gparams[i,1],d[[1]]$params[2]))
			list(quant=as.numeric(quantile(d[[1]]$ts, seq(0,1,len=Q_CNT))), params=gparams[i,2])
	}
	save(dats, file='dats1_21.RData')

	params1 = 0.02*1:150; gparams = as.matrix(expand.grid(params1))
	d = 0
	#dats = foreach(i=1:1)%dopar%{
	dats = foreach(i=1:nrow(gparams))%dopar%{
	#dats = for(i in 1:nrow(gparams)){
		p = gparams[i,] #as.character(gparams[i,])
			print(p); 
			#d = get(load(paste('RData\\res_CRYSTAL-2e+05-',paste(gparams[i,],collapse='-'),'.RData',sep=''))) 
			#d = get(load(paste('RData\\res_CRYSTAL42-1e+05-',paste(gparams[i,],collapse='-'),'.RData',sep=''))) 
			d = list(list(ts=rnorm(200000,0,p)))

			###d = get(load(paste('RData\\res_CRYSTAL-2e+05-',paste(gparams[i,],collapse='-'),'.RData',sep=''))) 
			###list(data=d[[1]]$ts/sd(d[[1]]$ts), params=d[[1]]$params[c(1,3)])
			###list(data=d[[1]]$ts/sd(d[[1]]$ts), params=c(gparams[i,1],d[[1]]$params[2]))
	
			#list(data=d[[1]]$ts, params=c(gparams[i,1],d[[1]]$params[2]))
			#list(quant=as.numeric(quantile(d[[1]]$ts, seq(0,1,len=Q_CNT))), params=c(gparams[i,1],d[[1]]$params[2]))
			list(quant=as.numeric(quantile(d[[1]]$ts, seq(0,1,len=Q_CNT))), params=gparams[i,1])
	}
	save(dats, file='dats1_NORM.RData')

}
load('dats1_21.RData')
#load('dats1_NORM.RData')

MY_DISTRIBS <<- dats
MY_PARAMS <<- foreach(x=MY_DISTRIBS,.combine=rbind)%do%x$params

##P_CNT <<- 2; I_CNT <<- 3; 	
#P_CNT <<- 1; I_CNT <<- 3; 
P_CNT <<- 1; I_CNT <<- 1; 

##indic_to_param = function(ind,cf) { c(cf[1]+sum(ind*cf[2:4]),cf[5]+sum(ind*cf[6:8])) }
##indic_to_param = function(ind,cf) { c(cf[1]+sum(ind*cf[2:3]),cf[4]+sum(ind*cf[5:6])) }
##indic_to_param = function(ind,cf) { c(cf[1]+sum(ind*cf[2:4])) }
##indic_to_param = function(ind,cf) { c(cf[1]+sum(ind*cf[2])) }
##indic_to_param = function(ind,cf) { c(cf[1]+ind*cf[2]+ind*ind*cf[3]+ind*ind*ind*cf[4]) }
indic_to_param = function(ind,cf) { c(cf[1]+ind*cf[2]+ind*ind*cf[3]) }
MY_PARAMS_BND <<- foreach(i=1:P_CNT)%do%range(MY_PARAMS[,i])

INDICES <<- as.matrix(idss)[1:(dim(idss)[1]-1),]   # fundamental market indices
VALS_ACT <<- as.numeric(ids[,'SP500'])[2:dim(idss)[1]]
INDICES_BND <<- if(is.null(dim(INDICES))) list(range(INDICES)) else foreach(i=1:dim(INDICES)[2])%do%range(INDICES[,i])

TS_LEN <<- if(is.null(dim(INDICES))) length(INDICES) else dim(INDICES)[1]

vix <<- as.numeric(vix0)
#vix <<- array(0.03,length(vix0))



#cfs_grid = as.matrix(expand.grid(list(20*(-2:2),-2:2,-2:2,-2:2,-2:2,-2:2)))
#cfs_grid = list(c(-40,40),c(-2,2),c(-2,2),c(-2,2),c(-2,2),c(-2,2))
#cfs_grid = list(c(7,70),c(-4,4),c(-4,4),c(1,4),c(-0.2,0.2),c(-0.4,0.8))
#cfs_grid = list(c(10,30),c(-2,2),c(-2,2),c(1,3),c(-0.05,0.05),c(-0.2,0.2))

##cfs_grid = list(c(-20,20),c(-1,1),c(-1,1),c(-1,1))
##cfs_grid = list(c(0.3,1.2),c(-0.3,0.3))
cfs_grid = list(c(0.65,0.9),c(-0.2,0.3),c(-0.05,0.03))

#cfs_grid = list(c(-1,1),c(-0.5,0.5),c(-0.5,0.5),c(-0.5,0.5))
#cfs_grid = list(c(0.7,0.85),c(-0.04,0.04),c(-0.15,0.15),c(-0.25,0.05))
#cfs_grid = list(c(0.79,0.83),c(-0.04,0.02),c(-0.15,0.15),c(-0.15,0.1))
#cfs_grid = list(c(0.8,0.81),c(-0.01,0.01),c(-0.03,0.03),c(0,0.1))
#cfs_grid = list(c(0.802,0.809),c(-0.016,0.0065),c(0,0.09),c(-0.03,0.065))
#cfs_grid = list(c(0.6,1),c(-0.1,0.1),c(-0.5,0.5),c(-0.5,0.5))
#cfs_grid = list(c(0.79,0.82),c(-0.1,0.1),c(-0.5,0.5),c(-0.5,0.5))
#cfs_grid = list(c(0.79,0.82),c(-0.05,0.05),c(-0.2,0.2),c(-0.3,0.2))

foreach(j=1:100,.packages='foreach')%do%{
#foreach(j=1005:2000,.packages='foreach')%do%{
	ress = foreach(i=1:1000,.packages='foreach')%dopar%{
		p = foreach(c=cfs_grid,.combine=c)%do%runif(1,c[1],c[2])
		list(d=try_bayes2(p,TRUE)$value, params=p)
	}
	print(j)
	#save(ress, file=paste('RData_res\\ressNN_1-',j,'.RData',sep=''))
	save(ress, file=paste('RData_res\\ress_LBR_9-',j,'.RData',sep=''))
}




rl = array(0,0)
r_tmp = foreach(fn=Sys.glob('RData_res\\ress_LBR_9-*'),.packages='foreach')%dopar%{
	print(fn)
	r0 = get(load(fn))
	#r0 = r0[(!is.na(r0)) & !is.null(r0)]
	list(r0=r0,rl0=foreach(r1=r0,.combine=c)%do%r1$d)
} 
#r_tmp = r_tmp[!is.na(r_tmp)]
r = foreach(r0=r_tmp,.combine=c)%do%{
	rl = c(rl,r0$rl0)
	r0$r0
}

#rb = foreach(i=which(rl<10),.combine=rbind)%do%r[[i]]$params

irl = which(rl<1)
irl = irl[order(rl[irl])]
rb = foreach(i=irl,.combine=rbind)%do%r[[i]]$params
plot(rb[,2],rl[irl],cex=0.4)




plot(rl[irl[foreach(i=irl,.combine=c)%do%!is.null(r[[i]])]],rb[,1],cex=0.1)

rb_sample = rb#[1:8000,]
save(rb_sample, file='rb_sample.RData')

load('rb_sample.RData')
rnd = c(1:200,sample(1:2000,500))
p1 = foreach(i=1:length(rnd),.packages=c('igraph','foreach'))%dopar%{
	p = as.numeric(rb_sample[rnd[i],])
	r = optim(p,function(x){ y=try_bayes2(x)[[1]]; print(y); y })
	save(r, file=paste('RData_res\\optim_LBR_21_',paste(r$par,collapse='-'),'.RData',sep=''))
	r
}

save(p1, file='optim_21_p1.RData')

p_val = array(0,0)
p_par = foreach(fn=Sys.glob('RData_res\\optim_LBR_21_*'))%do%{
	r0 = get(load(fn))
	p_val = c(p_val,r0$value)
	r0$par
}
p_par = p_par[order(p_val)]
p_val = p_val[order(p_val)]

rb_optim = foreach(i=1:100,.combine=rbind)%do%c(p_val[i],p_par[[i]])
try_bayes2(t(rowSums(t(rb_optim[rb_optim[,5]<0,2:5])))/sum(rb_optim[,5]<0))


x=p_par[[1]]
t=try_bayes2(x)
plot(t$cells,ylim=c(0,0.5))
abline(h=1/20)


rb_my = foreach(d=dats,.combine=rbind)%do%{ d$quant*0.05 - as.numeric(quantile(VALS_ACT, seq(0,1,len=Q_CNT))) }

brr = seq(min(VALS_ACT),max(VALS_ACT),len=80)
h1 = hist(VALS_ACT,br=seq(min(VALS_ACT),max(VALS_ACT),len=80),plot=FALSE)
plot(h1$mids,h1$density,t='l',col='red')

params1 = list(30,0.02*1:50,0,0,0); gparams = as.matrix(expand.grid(params1))
i = 29
d = get(load(paste('RData\\res_CRYSTAL42-1e+05-',paste(gparams[i,],collapse='-'),'.RData',sep='')))[[1]]$ts
d = d*0.05
h2 = hist(d[d>brr[1] & d<brr[80]],br=brr,plot=FALSE)
lines(h2$mids,h2$density,t='l',col='blue')






rb[1,]
[1]  0.63603373 -0.02399343

cf_tmp=rb[1,]
a = try_bayes2(cf_tmp,TRUE)
#a = try_bayes2(c(0.75,0.05,-0.01),TRUE)

plot(a$cells,ylim=c(0,0.5)); abline(h=0.05)
cf_tmp = rb[1,]; do_verify = TRUE
h1=m$hits
h2=h1
for(i in 1:nrow(h2)){h2[i,]=array(0,ncol(h2)); h2[i,sample(20,1)]=1;}
wnd = 60
a1=aggregate(1:nrow(h1),by=list((1:nrow(h1))%/%wnd),FUN=function(x){sum(abs(rowSums(t(h1[x,]/wnd))[c(1,20)]-0.05)) })[,2]
a2=aggregate(1:nrow(h1),by=list((1:nrow(h1))%/%wnd),FUN=function(x){sum(abs(rowSums(t(h2[x,]/wnd))[c(1,20)]-0.05)) })[,2]
plot(a1,t='l',ylim=c(0,1)); lines(a2,lwd=3,col='blue'); abline(h=0)

rowSums(t(h1[x,]))






