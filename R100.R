source('R4.R')
source('R121.R')
library(foreach)

#library(doParallel)
#cl <- makePSOCKcluster(5)
#registerDoParallel(cores=80)

params1 = list(30,0.02*1:50,0,0,0)
params2 = list(0,0)
N = 100000

calc_data_params_lists_SEQ_WITHFIX(c(N+1000,get_corr_snow_Memory_CRYSTAL,params1),params2,calc_exp42_Snow_Memory,'RData/res1_CRYSTAL42-') 
#calc_data_params_lists_SEQ_WITHFIX(c(N,get_corr_snow_Memory_CRYSTAL,params1),params2,calc_exp42_Snow_Memory,'RData/res2_CRYSTAL42-') 

#r = get(load(paste('RData/res1_CRYSTAL42-',N+1000,'-',paste(params1,collapse='-'),'.RData',sep='')))[[1]]$ts
#r = c(r,get(load(paste('RData/res2_CRYSTAL42-',N,'-',paste(params1,collapse='-'),'.RData',sep='')))[[1]]$ts)

#r1 = get(load('RData/res1_CRYSTAL42-51000-10-0.8-1-0.5-0.3.RData'))[[1]]$ts
#r2 = get(load('RData/res1_CRYSTAL42-51000-10-0.8-1-0.5-0.3.RData'))[[1]]$ts

r1 = array(0,0)
for(i in 1:50)
	r1 = c(r1,get(load(paste('RData/res',i,'_CRYSTAL42-1000-10-0.8-1-0.5-0.3.RData',sep='')))[[1]]$ts)
r2 = array(0,0)
for(i in 1:50)
	r2 = c(r2,get(load(paste('RData/res',i,'_CRYSTAL42-1000-30-0.8-1-0.5-0.3.RData',sep='')))[[1]]$ts)

h1 = hist(r1[abs(r1)<40][1:48000],plot=FALSE,br=seq(-40,40,len=300))
h2 = hist(r2[abs(r2)<40][1:48000],plot=FALSE,br=seq(-40,40,len=300))
plot(h2$mids,h2$counts/sum(h2$counts),t='l',lwd=3,col='red',xlim=c(-40,40))
lines(h1$mids,h1$counts/sum(h1$counts),t='l',lwd=3,col='blue')

N = 3000; p = 0.1
g = graph(edges=c(),n=N,directed=FALSE)
#for(j in 1:10)
for(i in 1:N) if(runif(1)<p) g = g + edge(i,sample(N,1))



