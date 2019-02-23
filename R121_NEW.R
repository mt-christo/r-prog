library(foreach)
library(igraph)
library(doMC) 
library(xts)
library(dplyr)
library(data.table)
registerDoMC(cores=7)

erdon = function(nv, prob){
    nv_prob = floor(nv*prob)
    g = graph(edges=c(), n=nv, directed=FALSE)
#    v1 = sample(1:nv, nv_prob)
#    v = array(0, 2*nv_prob*nv_prob)
#    v[2*(1:(nv_prob*nv_prob)) - 1] = rep(v1, nv_prob)
#    v[2*(1:(nv_prob*nv_prob))] = sample(1:nv, nv_prob*nv_prob, replace=TRUE)
    g = g + edge(sample(1:nv, 2*nv_prob*nv_prob, replace=TRUE))
    res = sqrt(sum(clusters(g)$csize^2))/nv
    res = res * (res - 1/(nv^2))/res
#    res = res/(1-res)
    return(res)
}

test_erdon = function(n, prob) foreach(i=1:7,.combine=c)%dopar%{ x=array(0,4000); for(i in 1:4000) x[i]=rnorm(1, 0, sd=erdon(n, prob)); x }
m = foreach(j=0.05*1:8,.combine=rbind)%do%{ foreach(i=10*1:6,.combine=c)%do%as.numeric(quantile(test_erdon(i, j))[2]) }


hist(x[abs(x)<500],br=90)
