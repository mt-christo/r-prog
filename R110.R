source('R121.R')
library(timeDate)
#update_ids0('/root/r-prog/')
#update_ids0('C:/Anton/')
load('ids0xts.RData')

#indic_to_param = function(ind,cf) { c(cf[1]+ind*cf[2]+ind*ind*cf[3]) }
indic_to_param = function(ind,cf) { c(cf[1]+ind[1]*cf[2]) }
params1 = list(100,0.005*1:200,0,0,0); gparams = as.matrix(expand.grid(params1))
date1 = '2007-05-13'; date2 = '2015-06-11'

snp = ids0[,'SP500']; trs = ids0[,'DGS10']; lbr = ids0[,'USD3MTD156N']; vix0=ids0[,'VIX.Close']/(100*252^0.5)
ids = merge.xts(snp,trs,lbr)[!is.na(snp),]; 
ids = na.locf(ids)[index(ids)>date1 & index(ids)<date2,] # VIX is not included in Indices list, used just for sd!
vix0 = na.locf(vix0)[index(vix0)>date1 & index(vix0)<date2,]
ids = ids[index(ids)%in%index(vix0),]
vix0 = vix0[index(vix0)%in%index(ids),]

idss = ids[,'USD3MTD156N'] 
#idss = ids[,c('USD3MTD156N','DGS10')] 
INDICES = idss;

Q_CNT <<- 21; 	

if(FALSE){
	d = 0
	dats = foreach(i=1:nrow(gparams))%do%{
		p = as.character(gparams[i,])
			print(p); 
			d = get(load(paste('RData\\50k\\res_CRYSTAL43-nk-',paste(gparams[i,],collapse='-'),'.RData',sep=''))) 
			list(quant=as.numeric(quantile(d, seq(0,1,len=Q_CNT))), params=gparams[i,2])
	}
	save(dats, file='dats1_21.RData')
}
load('dats2_21.RData')
#load('dats1_NORM.RData')

MY_DISTRIBS <<- dats
MY_PARAMS <<- foreach(x=MY_DISTRIBS,.combine=rbind)%do%x$params
P_CNT <<- 1; I_CNT <<- 1; 

MY_PARAMS_BND <<- foreach(i=1:P_CNT)%do%range(MY_PARAMS[,i])
INDICES <<- as.matrix(idss)[1:(dim(idss)[1]-1),]   # fundamental market indices
VALS_ACT <<- as.numeric(ids[,'SP500'])[2:dim(idss)[1]]
INDICES_BND <<- if(is.null(dim(INDICES))) list(range(INDICES)) else foreach(i=1:dim(INDICES)[2])%do%range(INDICES[,i])
TS_LEN <<- if(is.null(dim(INDICES))) length(INDICES) else dim(INDICES)[1]

vix <<- as.numeric(vix0)

cfs_grid = list(c(0.5,0.99),c(-0.07,0.07))
#cfs_grid = list(c(0.85,0.9),c(-0.05,0.1),c(-0.025,0.01))

foreach(j=1:2000,.packages='foreach')%do%{
	ress = foreach(i=1:100,.packages='foreach')%do%{
		p = foreach(c=cfs_grid,.combine=c)%do%runif(1,c[1],c[2])
		print(i)
		list(d=try_bayes2(p,TRUE)$value, params=p)
	}
	print(j)
	save(ress, file=paste('RData_res/ress_VIX252_8_',j,'.RData',sep=''))
}











