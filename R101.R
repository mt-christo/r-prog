source('R12.R')

N = 50000
params = list(1:3,1:3)
ptest = c(1.3,2.1)
res = foreach(i=params[[1]],.combine=c)%do%{ foreach(j=params[[2]])%do%list(params=c(i,j),data=rnorm(N,i,j)) }

# dats=res; p=ptest
paramed_pdf_interpol = function(dats,p){
	dp = foreach(i=1:length(p))%do%{ unique(foreach(d=dats,.combine=c)%do%d$params[i]) }
	bnd = foreach(i=1:length(dp))%do%{
		i1 = max(which(dp[[i]]<=p[i])); i2 = min(which(dp[[i]]>=p[i]))
		if(i1 != i2) dp[[i]][c(i1,i2)] else dp[[i]][i1]
	}
	if(!max(foreach(b=bnd,.combine=c)%do%is.na(b)))
		PPI(bnd,p,dats,length(dats[[1]]$data))
	else
		print('Point is outside of the grid')
}

# sN = length(res[[1]]$data)
PPI = function(bnd,p,dats,sN){
	twos = which(2 == foreach(b=bnd,.combine=c)%do%length(b))
	if(length(twos) > 0){
#print(1)
		t = twos[1]; p1 = bnd[[t]][1]; p2 = bnd[[t]][2]
		bnd1 = bnd; bnd1[[t]] = p1
		bnd2 = bnd; bnd2[[t]] = p2
		print('split')
		ts1 = PPI(bnd1,p,dats,sN); len1 = floor(2*sN*(p2-p[t])/(p2-p1))
		ts2 = PPI(bnd2,p,dats,sN); len2 = floor(2*sN*(p[t]-p1)/(p2-p1))
		c(sample(ts1,len1,replace=TRUE),sample(ts2,len2,replace=TRUE))
	} else {
		t = which(as.logical(foreach(d=dats,.combine=c)%do%min(d$params==unlist(bnd))))
		print(paste(paste(unlist(bnd),collapse='-'),': ',sd(dats[[t]]$data)))
#print(length(dats[[t]]$data))
		dats[[t]]$data
	}
}


b = paramed_pdf_interpol(a,c(1.5,2.5,3.5,1.1))
show_hist_comparison(lapply(a,function(x){x$data}),50)
h=hist(b,br=50,plot=FALSE); plot(h$mids,h$density,t='l',lwd=3,col='green') #lines(h$mids,h$density,t='l',lwd=3,col='green')
plot(h$mids,h$density,t='l',lwd=3,col='red')





