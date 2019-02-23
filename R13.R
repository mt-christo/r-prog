library(hash)
library(igraph)
library(mnormt)
library(doMC) 
library(MBESS)
library(corpcor)
library(tseries)
library(xts)
library(quantmod)

# System parameters: 
# a - avalanche density (total amount of snowfall per one cell)
# b=2 - branching factor
# d<1 - decay factor (amount of snow dispensed at toppling, instead of toppled mass 1)

# Implementation parameters
# N - amount of cells
# da - snowflake mass (toppling happens at aggregated mass=1)

snow_graph_TOPPLE <- function(g_in,v_idxs,b_in,d_topple_in){
    g_out = g_in
    all_v_idxs = 1:vcount(g_out)
    for(i in v_idxs){
	V(g_out)[i]$m = V(g_out)[i]$m - 1
	for(j in sample(all_v_idxs,b_in)){
	    V(g_out)[j]$m = V(g_out)[j]$m + d_topple_in
	    g_out = g_out + edge(i,j)
	}
    }
    return(g_out)    
}

snow_graph <- function(g_in,v_idx,b_in,da_in,d_topple_in){
    g_out = g_in
    V(g_out)[v_idx]$m = V(g_out)[v_idx]$m + da_in
#    Excess=0; Num_Avalanche=0; Av_Array=c(length(which(a>1))); 
 #   step_i = 1
    av_idxs = c()
    while(max(V(g_out)$m)>1)
	g_out = snow_graph_TOPPLE(g_out,which(V(g_out)$m>1),b_in,d_topple_in)
    return(g_out)
}




get_corr_snow_graph_TOPPLE <- function(g_in,v_idxs,b_in,d_topple_in){
    g_out = g_in
    all_v_idxs = 1:vcount(g_out)
    for(i in v_idxs){
	V(g_out)[i]$m = V(g_out)[i]$m - 1
	for(j in sample(all_v_idxs,b_in)){
	    V(g_out)[j]$m = V(g_out)[j]$m + d_topple_in
	    g_out = g_out + edge(i,j)
	}
    }
    return(g_out)    
}

# g_in=g_in_Global; v_idx=sample(N,1); b_in=b; da_in=da; d_topple_in=d_topple
get_corr_snow_graph_Increment <- function(g_in,v_idx,b_in,da_in,d_topple_in){
    g_out = g_in
    V(g_out)[v_idx]$m = V(g_out)[v_idx]$m + da_in
    av_idxs = c()
    while(max(V(g_out)$m)>1)
	g_out = get_corr_snow_graph_TOPPLE(g_out,which(V(g_out)$m>1),b_in,d_topple_in)
    return(g_out)
}


get_corr_snow = function(N,a,b,d,da){
	#N = 25; a = 0.6; b = 2; d = 0.9; da = 0.2
	d_topple = d/b

	g = graph(edges=c(),n=N,directed=FALSE)
	v = V(g)
	for(i in 1:length(v)) V(g)[i]$m=0
	for(i in 1:ceiling(N*a/da)) g = get_corr_snow_graph_Increment(g,sample(N,1),b,da,d_topple)
	return(shortest.paths(g))
}


get_corr_snow_Memory = function(g_in_Global,N,a,b,d,da){
	g = get_corr_snow_graph_Increment(g_in_Global - edges(g_in_Global)[[1]],sample(N,1),b,da,d/b)
	return(list(graph = g, paths = shortest.paths(g)))
}




# project

N = 25; a = 0.3; b = 2; d = 0.9; da = 0.15
d_topple = d/b

g = graph(edges=c(),n=N,directed=FALSE)
v = V(g)
for(i in 1:length(v)) V(g)[i]$m=0

for(i in 1:ceiling(N*a/da)) g = get_corr_snow_graph_Increment(g - edges(g)[[1]],sample(N,1),b,da,d_topple)

tmp_res = list()
rp0 = rep(0, N)
rp1 = rep(1, N)
res =array(0,0)
res = foreach(i=1:1000, .combine=c)%do%{
	tmp_res = get_corr_snow_Memory(g,N,a,b,d,da)
	g = tmp_res$graph
	s0 = tmp_res$paths
	s = cor2cov(exp(log(0.9)*s0), rp1)
	s_r = tryCatch({ rmnorm(1, rp0, s)}, error=function(ex){ s1=make.positive.definite(s, tol=0.001); rmnorm(1, rp0, s1) })
	sum(s_r)/N
}


hist(res,br=100)




