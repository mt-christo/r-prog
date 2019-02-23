library(foreach)
library(hash)
library(igraph)
library(mnormt)
#library(doMC) 
library(MBESS)
library(corpcor)
library(tseries)
library(xts)
library(quantmod)
#library(RCurl)

#rm(list = ls(all = TRUE))

#prc_N = 8
#registerDoMC(prc_N)

#source('/mnt/G/RISK/ANTON/R/R4.R')
source('R4.R')

global_cols = c('red','darkgreen','blue','orange','pink','purple','cyan','brown')
for(i in 1:100) global_cols=c(global_cols,'black')

dopar_rank = 1000
SH_TRESH = 0.75




get_corr_1 = function(cells_num,corr,x2,x3){
	res = matrix(corr,cells_num,cells_num)
	diag(res) = 1
	return(res)
}


get_corr_2 = function(cells_num,corr_n,x2,x3){
	res = matrix(0,cells_num,cells_num)
	diag(res) = 1
	res[1:corr_n,1:corr_n] = 1
	return(res)
}

get_corr_3 = function(cells_num,b,b_prob){
	gen_rand_cells = function(probb){ ceiling(runif(b)*cells_num)[probb | runif(b) < (b_prob/b)]  }
	g = graph(edges=c(),n=cells_num,directed=FALSE)
	percs = gen_rand_cells(TRUE)
	while(length(percs) > 0){
		tmp_perc = percs[1]
		percs = percs[-1]
		tmp_len = V(g)$len[tmp_perc]
		new_percs = gen_rand_cells(FALSE)
		if(length(new_percs) > 0){
			for(i in new_percs)
				g = g + edge(tmp_perc,i)
			percs = c(new_percs,percs)		
		}
	}

	#print(g)
	#return(exp(log(corr_d)*shortest.paths(g)))
	return(shortest.paths(g))
}


get_corr_4 = function(cells_num,b,tree_size,corr_d){
	gen_rand_cells = function(){ ceiling(runif(b)*cells_num)  }
	g = graph(edges=c(),n=cells_num,directed=FALSE)
	init_cells = gen_rand_cells()
	for(i in init_cells){
		percs = c(i)
		cnt = 1
		while(cnt < tree_size){
			tmp_perc = percs[1]
			percs = percs[-1]
			new_percs = gen_rand_cells()
			for(j in new_percs)
				if(cnt < tree_size){
					g = g + edge(tmp_perc,j)
					cnt = cnt + 1
					percs = c(percs,j)
				} 
		}

	}

	#print(g)
	return(exp(log(corr_d)*shortest.paths(g)))
}

# cells_num=200; b=4; b_prob=0.24 -!- will be now using prob PER ONE b -!- ; corr_d=0.99
get_corr_5 = function(cells_num,b_float,b_prob,corr_d){
	gen_rand_cells = function(probb){ b=floor(b_float); b=if(runif(1)<b_float-b) b+1 else b; ceiling(runif(b)*cells_num)[probb | runif(b) < (b_prob/b)]  }
	g = graph(edges=c(),n=cells_num,directed=FALSE)
	percs = gen_rand_cells(TRUE)
	while(length(percs) > 0){
		tmp_perc = percs[1]
		percs = percs[-1]
		tmp_len = V(g)$len[tmp_perc]
		new_percs = gen_rand_cells(FALSE)
		if(length(new_percs) > 0){
			for(i in new_percs)
				g = g + edge(tmp_perc,i)
			percs = c(new_percs,percs)		
		}
	}

	#print(g)
	return(exp(log(corr_d)*shortest.paths(g)))
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

get_corr_snow_graph_TOPPLE_CRYSTAL <- function(g_in,v_idxs,b_in){
    g_out = g_in
    vcnt = vcount(g_out)
    all_v_idxs = 1:vcnt
    front = array(0,0)

    for(i in v_idxs){
	for(j in sample(all_v_idxs,b_in))
		if(V(g_out)[j]$m > 1) {
		    V(g_out)[j]$m = V(g_out)[j]$m - 1
		    V(g_out)$m = V(g_out)$m + 1/vcnt
		    g_out = g_out + edge(i,j)
		    front = c(front,j)
		}
    }

    return(list(graph=g_out, front=front))    
}



get_corr_snow_graph_TOPPLE_TO_NONCLUSTER <- function(g_in,v_idxs,b_in,d_topple_in){
    g_out = g_in
    all_v_idxs = 1:vcount(g_out)
    for(i in v_idxs){
	V(g_out)[i]$m = V(g_out)[i]$m - 1
	for(j in sample(all_v_idxs,b_in))
		if(shortest.paths(g_out)[i,j]%in%c(0,Inf)){
	    		V(g_out)[j]$m = V(g_out)[j]$m + d_topple_in
	    		g_out = g_out + edge(i,j)
		}
    }
    return(g_out)    
}

# g_in=g_in_Global; v_idx=sample(N,1); b_in=b; da_in=da; d_topple_in=d_topple
get_corr_snow_graph_Increment <- function(topple_FUNC, g_in, v_idx, b_in, da_in, d_topple_in){
    g_out = g_in
    V(g_out)[v_idx]$m = V(g_out)[v_idx]$m + da_in
    av_idxs = c()
    max_steps = length(V(g_out))
    i = 0
    while(max(V(g_out)$m)>1 && i<max_steps){
	g_out = get_corr_snow_graph_TOPPLE(g_out,which(V(g_out)$m>1),b_in,d_topple_in)
	i = i+1
    }

    return(g_out)
}

get_corr_snow_graph_Increment_CRYSTAL <- function(g_in,v_idx1,v_idx2,b_in,da_in){
    g_out = g_in
    V(g_out)[v_idx1]$m = V(g_out)[v_idx1]$m + da_in
    if(length(v_idx2)>0)
	V(g_out)[v_idx2]$m = V(g_out)[v_idx2]$m - da_in

    front = array(0,0)
    if(V(g_out)[v_idx1]$m > 1+da_in)
	front = c(front,v_idx1)

    while(length(front) > 0){
	topple = get_corr_snow_graph_TOPPLE_CRYSTAL(g_out,front,b_in)
	g_out = topple$graph
	front = topple$front
    }
    return(g_out)
}

get_corr_snow_graph_Increment_Fract <- function(g_in,v_idx,b_in_BASE,b_in_PROB,da_in,d_topple_in){
    g_out = g_in
    V(g_out)[v_idx]$m = V(g_out)[v_idx]$m + da_in
    av_idxs = c()
    max_steps = length(V(g_out))
    i = 0
    while(max(V(g_out)$m)>1 && i<max_steps){
	g_out = get_corr_snow_graph_TOPPLE(g_out,which(V(g_out)$m>1),b_in_BASE+rbinom(1,1,b_in_PROB),d_topple_in)
	i = i+1
    }

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


get_corr_snow_Memory = function(g_in_Global,N,a,b,d,da,d_topple){
	g = get_corr_snow_graph_Increment(get_corr_snow_graph_TOPPLE, g_in_Global - edges(g_in_Global)[[1]],sample(N,1),b,da,d_topple)
	return(list(graph = g, paths = shortest.paths(g)))
}


get_corr_snow_Memory_TO_NONCLUSTER = function(g_in_Global,N,a,b,d,da,d_topple){
	g = get_corr_snow_graph_Increment(get_corr_snow_graph_TOPPLE_TO_NONCLUSTER, g_in_Global - edges(g_in_Global)[[1]],sample(N,1),b,da,d_topple)
	return(list(graph = g, paths = shortest.paths(g)))
}


get_corr_snow_Memory_Fract = function(g_in_Global,N,a,b,d,da,d_topple){
	g = get_corr_snow_graph_Increment_Fract(g_in_Global - edges(g_in_Global)[[1]],sample(N,1),floor(b),b%%1,da,d/b)
	return(list(graph = g, paths = shortest.paths(g)))
}

## "d" is Concentration (when sinking starts)!!
get_corr_snow_Memory_CRYSTAL = function(g_in_Global,N,a,b,d,da,d_topple){
	g = get_corr_snow_graph_Increment_CRYSTAL(g_in_Global - edges(g_in_Global)[[1]], sample(N,1), if(sum(V(g_in_Global)$m)>d*N)sample(N,1) else c(), b, da)
	return(list(graph = g, paths = shortest.paths(g)))
}


#g0=get_corr_3(200,3,0.32); #plot(g0)
#n_steps=50000; corr_func=get_corr_3; cells_num=200; x1=4; x2=0.24; x3=0.99

calc_exp1 = function(n_steps,corr_func,cells_num,x1,x2,x3){
	res_arr = c()
	while(length(res_arr) < n_steps){
		res = unlist(foreach(i=1:(10*prc_N))%dopar%{foreach(i=1:5)%do%{ 
				s = make.positive.definite(cor2cov(corr_func(cells_num,x1,x2,x3), rep(1,cells_num)),tol=0.001)
				s = rmnorm(1, rep(0,cells_num), s) 
				#log(sum(s[s>0])) - log(sum(-s[s<0]))
				#buys=s[s>=0]; sells=s[s<0]; sl=length(sells); bl=length(buys)
				#log(sl) - log(bl)
				#log(length(which(s>0))) - log(length(which(s<0)))
				sum(s)/cells_num
			}})
		res_arr = c(res_arr, res)
		print(paste('params:',cells_num,x1,x2,x3,'-',length(res_arr)))
	}
	res_arr
}

calc_exp2 = function(n_steps,corr_func,cells_num,x1,x2,x3){
	res_arr = c()
	rp0 = rep(0, cells_num)
	rp1 = rep(1, cells_num)
	while(length(res_arr) < n_steps){
		res = unlist(foreach(i=1:(10*prc_N))%dopar%{foreach(j=1:5)%do%{ 
				s = corr_func(cells_num,x1,x2,x3)
				s = cor2cov(s, rp1)
				s_r = tryCatch({ rmnorm(1, rp0, s)}, error=function(ex){s1=make.positive.definite(s, tol=0.001); rmnorm(1, rp0, s1)})
#print(i)
				sum(s_r)/cells_num
			}})
		res_arr = c(res_arr, res)
		print(paste('params:',cells_num,x1,x2,x3,'-',length(res_arr)))
	}
	res_arr
}

# n_steps=100; corr_func=get_corr_3; cells_num=100; x1=3; x2=0.8; xs3=c(0.5,0.7,0.9); xs4=c(0.05,0.1,0.15);
calc_exp3 = function(n_steps,corr_func,cells_num,x1,x2,x_FIX){
	xs3 = x_FIX[[1]]
	xs4 = x_FIX[[2]]

	#dopar_rank = 100
	
	rp0 = rep(0, cells_num)
	rp1 = rep(1, cells_num)

	res = list()
	for(i in 1:length(xs3)){
		res[[i]] = list()
		length(res[[i]]) = length(xs4)
	}

	while(length(res[[1]][[1]]) < n_steps){
		# need to make adaptable for multiple-step Cholesky
		tmp_res = foreach(i=1:dopar_rank)%dopar%{
				s0 = corr_func(cells_num,x1,x2)
				foreach(x3=xs3)%do%{foreach(x4=xs4)%do%{
					s = cor2cov(exp(log(x3)*s0), x4*rp1)
					s_r = tryCatch({ rmnorm(1, rp0, s)}, error=function(ex){ s1=make.positive.definite(s, tol=0.001); rmnorm(1, rp0, s1) })
					sum(s_r)/cells_num
				}}}

		for(i in 1:length(tmp_res))
			for(j in 1:length(tmp_res[[i]]))
				for(k in 1:length(tmp_res[[i]][[j]]))
					res[[j]][[k]] = c(res[[j]][[k]], tmp_res[[i]][[j]][[k]])
			
		print(paste('params:',cells_num,x1,x2,'-',length(res[[1]][[1]])))
	}

	list(ts=res, xs3=xs3, xs4=xs4)
	#res
}

# n_steps=1000; corr_func=get_corr_snow_Memory; cells_num=50; x1=0.3; x2=2; x3=0.9; x4=0.1; x_FIX=list(0.1*(5:9),1)
calc_exp3_Snow_Memory = function(n_steps,corr_func,cells_num,x1,x2,x3,x4,x_FIX){
	rp0 = rep(0, cells_num)
	rp1 = rep(1, cells_num)
	g = graph(edges=c(),n=x1,directed=FALSE)

	foreach(x_fixs = foreach(i=x_FIX[[1]],.combine=c)%do%{ foreach(j=x_FIX[[2]])%do%c(i,j) })%dopar%{
		#N = 25; a = 0.3; b = 2; d = 0.9; da = 0.15
		N = cells_num; a = x1; b = x2; d = x3; da = x4
		xx1 = x_fixs[1]; xx2 = x_fixs[2]
		d_topple = d/b

		g = graph(edges=c(),n=N,directed=FALSE)
		v = V(g)
		for(i in 1:length(v)) V(g)[i]$m=0

		for(i in 1:ceiling(N*a/da)) g = get_corr_snow_graph_Increment(g - edges(g)[[1]],sample(N,1),b,da,d_topple)

		tmp_res = list()
		rp0 = rep(0, N)
		rp1 = rep(1, N)
		#res =array(0,0)
		res = foreach(i=1:n_steps, .combine=c)%do%{
			#tmp_res = get_corr_snow_Memory(g,N,a,b,d,da)
if(i%%100==0) print(i)
			tmp_res = corr_func(g,N,a,b,d,da)
			g = tmp_res$graph
			s0 = tmp_res$paths
			s = cor2cov(exp(log(xx1)*s0), xx2*rp1)
			s_r = tryCatch({ rmnorm(1, rp0, s)}, error=function(ex){ s1=make.positive.definite(s, tol=0.001); rmnorm(1, rp0, s1) })
			sum(s_r)/N
		}

		list(ts=res, params=c(x1,x2,x3,x4,xx1,xx2))
	}
}


#calc_exp3_Snow_Memory = function(n_steps,corr_func,cells_num,x1,x2,x3,x4,x_FIX){
calc_exp3_Snow_Memory_GC = function(n_steps,corr_func,cells_num,x1,x2,x3,x4){
	rp0 = rep(0, cells_num)
	rp1 = rep(1, cells_num)
	g = graph(edges=c(),n=x1,directed=FALSE)

	#foreach(x_fixs = foreach(i=x_FIX[[1]],.combine=c)%do%{ foreach(j=x_FIX[[2]])%do%c(i,j) })%dopar%{
		#N = 25; a = 0.3; b = 2; d = 0.9; da = 0.15
		N = cells_num; a = x1; b = x2; d = x3; da = x4
		#xx1 = x_fixs[1]; xx2 = x_fixs[2]
		d_topple = d/b

		g = graph(edges=c(),n=N,directed=FALSE)
		v = V(g)
		for(i in 1:length(v)) V(g)[i]$m=0

		for(i in 1:ceiling(N*a/da)) g = get_corr_snow_graph_Increment(g - edges(g)[[1]],sample(N,1),b,da,d_topple)

		tmp_res = list()
		rp0 = rep(0, N)
		rp1 = rep(1, N)
		#res =array(0,0)

		res = matrix(0,n_steps,2)
		colnames(res) = c('sz','av_p')

		foreach(i=1:n_steps, .combine=c)%do%{
			#tmp_res = get_corr_snow_Memory(g,N,a,b,d,da)
if(i%%100==0) print(i)
			tmp_res = corr_func(g,N,a,b,d,da)
			g = tmp_res$graph
			#s0 = tmp_res$paths
			#s = cor2cov(exp(log(xx1)*s0), xx2*rp1)
			#s_r = tryCatch({ rmnorm(1, rp0, s)}, error=function(ex){ s1=make.positive.definite(s, tol=0.001); rmnorm(1, rp0, s1) })
			#sum(s_r)/N

			c = clusters(g); 
			g_c = induced.subgraph(g,V(g)[c$membership==which.max(c$csize)]); 

			res[i,] = c(max(c$csize), average.path.length(g_c))
			#list(sd=sd(rowSums(s_r)/N), av_p=average.path.length(g_c), sz=max(c$csize))
		}

		list(mat=res, params=c(x1,x2,x3,x4))
	#}
}


calc_exp4_Snow_Memory = function(n_steps,corr_func,cells_num,x1,x2,x3,x4,x_FIX){
	rp0 = rep(0, cells_num)
	rp1 = rep(1, cells_num)
	g = graph(edges=c(),n=x1,directed=FALSE)

	foreach(x_fixs = foreach(i=x_FIX[[1]],.combine=c)%do%{ foreach(j=x_FIX[[2]])%do%c(i,j) })%dopar%{
		#N = 25; a = 0.3; b = 2; d = 0.9; da = 0.15
		N = cells_num; a = x1; b = x2; d = x3; da = x4
		xx1 = x_fixs[1]; xx2 = x_fixs[2]

		g = graph(edges=c(),n=N,directed=FALSE)
		v = V(g)
		for(i in 1:length(v)) V(g)[i]$m=0

		for(i in 1:ceiling(N*a/da)) g = get_corr_snow_graph_Increment(g - edges(g)[[1]],sample(N,1),b,da,0)

		tmp_res = list()
		rp0 = rep(0, N)
		rp1 = rep(1, N)
		#res =array(0,0)
		res = foreach(i=1:n_steps, .combine=c)%do%{
			tmp_res = corr_func(g,N,a,b,d,da)
if(i%%100==0) print(i)
			g = tmp_res$graph
			s0 = tmp_res$paths
			s = cor2cov(exp(log(0.99999)*s0), rnorm(N,mean=xx1,sd=xx2))
			s_r = tryCatch({ rmnorm(1, rp0, s)}, error=function(ex){ s1=make.positive.definite(s, tol=0.001); rmnorm(1, rp0, s1) })
			sum(s_r)/N
		}

		list(ts=res, params=c(x1,x2,x3,x4,xx1,xx2))
	}
}


#n_steps=1; corr_func=get_corr_snow_Memory_CRYSTAL; cells_num=25; x1=0; x2=3; x3=0.7; x4=0.15; x_FIX=list(1,0)
calc_exp41_Snow_Memory = function(n_steps,corr_func,cells_num,x1,x2,x3,x4,x_FIX){

	foreach(x_fixs = foreach(i=x_FIX[[1]],.combine=c)%do%{ foreach(j=x_FIX[[2]])%do%c(i,j) })%dopar%{

		#N = cells_num; a = x1; b = x2; d = x3; da = x4
		## x_fixs = c(x_FIX[[1]][1], x_FIX[[2]][1])
		#xx1 = x_fixs[1]; xx2 = x_fixs[2]

		N = cells_num; a = x1; b = x_fixs[1]; d = x3; da = x4
		xx1 = x2; xx2 = x_fixs[2]

		g = graph(edges=c(),n=N,directed=FALSE)
		v = V(g)
		for(i in 1:length(v)) V(g)[i]$m=0
		for(i in 1:ceiling(N*d/da)) { j = sample(N, 1); V(g)[j]$m = V(g)[j]$m + da }
		tmp_res = list()
		rp0 = rep(0, N)
		rp1 = rep(1, N)

		alf = 0

		res = foreach(i=1:n_steps, .combine=c)%do%{
			tmp_res = corr_func(g,N,a,b,d,da); #print(max(clusters(tmp_res$graph)$csize))
if(i%%100==0) print(i)
			g = tmp_res$graph
			cs = clusters(g)
			s0 = tmp_res$paths

			#alf = xx1 - 1
			#max_sz = xx2
			#sd_rnd = (1-alf*runif(N,min=0,max=(1-1/max_sz^alf)/alf))^(-1/alf)

			#sd_rnd = sd_rnd/sum(sd_rnd)
			#s = cor2cov(exp(log(0.99999)*s0),  sd_rnd)
			#s_r = tryCatch({ rmnorm(1, rp0, s)}, error=function(ex){ s1=make.positive.definite(s, tol=0.001); rmnorm(1, rp0, s1) })

			idx_corr = cs$membership==which.max(cs$csize)
			alf = xx1 - 1
			max_sz = xx2
			sd_rnd = (1-alf*runif(N,min=0,max=(1-1/max_sz^alf)/alf))^(-1/alf)
			sd_rnd = N*sd_rnd/sum(sd_rnd)
			
			s_r = array(0,N)
			s_r[idx_corr] = rnorm(1)*sd_rnd[idx_corr]
			s_r[!idx_corr] = rnorm(sum(!idx_corr),sd = sd_rnd[!idx_corr])

			if(max(cs$csize) < N)
				mean(s_r)/(1-sum(sd_rnd[which(cs$membership==which.max(cs$csize))]))
			else
				NA
		}

		list(ts=res[!is.na(res)], params=c(x1,x2,x3,x4,xx1,xx2))
	}
}

#n_steps=1; corr_func=get_corr_snow_Memory_CRYSTAL; cells_num=100; x1=0.8; x2=1.0001; x3=0.5; x4=0.1; x_FIX=list(2,1000000000); x_fixs=unlist(x_FIX)
calc_exp42_Snow_Memory = function(n_steps,corr_func,cells_num,x1,x2,x3,x4,x_FIX){
	N = cells_num; a = x1; b = x2; d = x3; da = x4
	rp0 = rep(0, cells_num)
	rp1 = rep(1, cells_num)
	by_sqrt_n = 1/sqrt(N)

#	foreach(x_fixs = foreach(i=x_FIX[[1]],.combine=c,.packages=c('igraph','foreach'))%do%{ foreach(j=x_FIX[[2]])%do%c(i,j) },.packages=c('igraph','foreach'))%dopar%{
	foreach(x_fixs = foreach(i=x_FIX[[1]],.combine=c)%do%{ foreach(j=x_FIX[[2]])%do%c(i,j) })%do%{

		xx1 = x_fixs[1]; xx2 = x_fixs[2]

		#res = foreach(i=1:n_steps, .combine=c, .packages=c('igraph','foreach'))%do%{
		res = foreach(i=1:n_steps, .combine=c, .packages=c('igraph','MBESS','corpcor'))%do%{
if(i%%100==0) print(i)

			rnd_As = runif(N)
			rnd_Ps = sample(N,N,replace=TRUE)

			g = graph(edges=c(),n=N,directed=FALSE)
			for(k in 1:N) 
				if(rnd_As[k] < a) 
					g = g + edge(k,rnd_Ps[k])

			s = cor2cov(exp(log(0.99999)*shortest.paths(g)),  rp1)
			s_r = tryCatch({ rmnorm(5000, rp0, s)}, error=function(ex){ s1=make.positive.definite(s, tol=0.001); rmnorm(5000, rp0, s1) })

			coeff = sd(rowSums(s_r)/N)
			x = mean(s_r[1,])
			x = if(coeff < 1) x/(1-coeff) else NA
			x = if(coeff > by_sqrt_n) x*(coeff-by_sqrt_n)/coeff else NA
			x
		}

		list(ts=res[!is.na(res)], params=c(x1,x2,x3,x4,xx1,xx2))
	}
}


#n_steps=1; corr_func=get_corr_snow_Memory_CRYSTAL; cells_num=100; x1=0.8; x2=1.0001; x3=0.5; x4=0.1; x_FIX=list(2,1000000000); x_fixs=unlist(x_FIX)
calc_exp43_Snow_Memory = function(n_steps,corr_func,cells_num,x1,x2,x3,x4,x_FIX){
	N = cells_num; a = x1; b = x2; d = x3; da = x4
	rp0 = rep(0, cells_num)
	rp1 = rep(1, cells_num)
	by_sqrt_n = 1/sqrt(N)

#	foreach(x_fixs = foreach(i=x_FIX[[1]],.combine=c,.packages=c('igraph','foreach'))%do%{ foreach(j=x_FIX[[2]])%do%c(i,j) },.packages=c('igraph','foreach'))%dopar%{
	foreach(x_fixs = foreach(i=x_FIX[[1]],.combine=c)%do%{ foreach(j=x_FIX[[2]])%do%c(i,j) })%do%{

		xx1 = x_fixs[1]; xx2 = x_fixs[2]

		#res = foreach(i=1:n_steps, .combine=c, .packages=c('igraph','foreach'))%do%{
		#res = foreach(i=1:n_steps, .combine=c, .packages=c('igraph','MBESS','corpcor'))%do%{
		res = array(0.0,0)
		for(i in 1:n_steps){
if(i%%100==0) aprint(i)

			rnd_As = runif(N)
			rnd_Ps = sample(N,N,replace=TRUE)

			g = graph(edges=c(),n=N,directed=FALSE)
			for(k in 1:N) 
				if(rnd_As[k] < a) 
					g = g + edge(k,rnd_Ps[k])

			s = cor2cov(exp(log(0.99999)*shortest.paths(g)),  rp1)
			s_r = tryCatch({ rmnorm(1, rp0, s)}, error=function(ex){ s1=make.positive.definite(s, tol=0.001); rmnorm(1, rp0, s1) })

			#coeff = sd(rowSums(s_r)/N)
			coeff = sqrt(sum(s))/N
			x = mean(s_r)
			x = if(coeff < 1) x/(1-coeff) else NA
			x = if(coeff > by_sqrt_n) x*(coeff-by_sqrt_n)/coeff else NA
			#x
			res[i] = x
		}

		list(ts=res[!is.na(res)], params=c(x1,x2,x3,x4,xx1,xx2))
	}
}



calc_exp5_Snow_Memory = function(n_steps,corr_func,cells_num,x1,x2,x3,x4,x_FIX){
	rp0 = rep(0, cells_num)
	rp1 = rep(1, cells_num)
	g = graph(edges=c(),n=x1,directed=FALSE)

	foreach(x_fixs = foreach(i=x_FIX[[1]],.combine=c)%do%{ foreach(j=x_FIX[[2]])%do%c(i,j) })%dopar%{
		#N = 25; a = 0.3; b = 2; d = 0.9; da = 0.15
		N = cells_num; a = x1; b = x2; d = x3; da = x4
		xx1 = x_fixs[1]; xx2 = x_fixs[2]
		d_topple = d/b

		g = graph(edges=c(),n=N,directed=FALSE)
		v = V(g)
		for(i in 1:length(v)) V(g)[i]$m=0

		for(i in 1:ceiling(N*a/da)) g = get_corr_snow_graph_Increment(g - edges(g)[[1]],sample(N,1),b,da,d_topple)

		tmp_res = list()
		rp0 = rep(0, N)
		rp1 = rep(1, N)
		#res =array(0,0)
		res = foreach(i=1:n_steps, .combine=c)%do%{
			#tmp_res = get_corr_snow_Memory(g,N,a,b,d,da)
if(i%%100==0) print(i)
			tmp_res = corr_func(g,N,a,b,d,da)
			g = tmp_res$graph
			c = clusters(g) 
			c_part = max(c$csize)
			if(c_part < N)
				(c_part^2 - c_part + N)^0.5/(N - c_part)
			else
				NA
			#s0 = tmp_res$paths
			#s = cor2cov(exp(log(0.99999)*s0), rnorm(N,mean=xx1,sd=xx2))
			#s_r = tryCatch({ rmnorm(1, rp0, s)}, error=function(ex){ s1=make.positive.definite(s, tol=0.001); rmnorm(1, rp0, s1) })
			#sum(s_r)/N
			
		}
		
		res_sd = res[!is.na(res)]
		res = rnorm(length(res_sd), mean=0, sd=res_sd)

		list(ts=res, sds=res_sd, params=c(x1,x2,x3,x4,xx1,xx2))
	}
}




calc_exp5_Snow_Memory = function(n_steps,corr_func,cells_num,x1,x2,x3,x4,x_FIX){
	rp0 = rep(0, cells_num)
	rp1 = rep(1, cells_num)
	g = graph(edges=c(),n=x1,directed=FALSE)

	foreach(x_fixs = foreach(i=x_FIX[[1]],.combine=c)%do%{ foreach(j=x_FIX[[2]])%do%c(i,j) })%dopar%{
		#N = 25; a = 0.3; b = 2; d = 0.9; da = 0.15
		N = cells_num; a = x1; b = x2; d = x3; da = x4
		xx1 = x_fixs[1]; xx2 = x_fixs[2]
		#d_topple = d/b
		d_topple = da

		g = graph(edges=c(),n=N,directed=FALSE)
		v = V(g)
		for(i in 1:length(v)) V(g)[i]$m=0

		for(i in 1:ceiling(N*a/da)) g = get_corr_snow_graph_Increment(g - edges(g)[[1]],sample(N,1),b,da,d_topple)

		tmp_res = list()
		rp0 = rep(0, N)
		rp1 = rep(1, N)
		#res =array(0,0)
		res = foreach(i=1:n_steps, .combine=c)%do%{
if(i%%100==0) print(i)
			tmp_res = corr_func(g,N,a,b,d,da,d_topple)
			g = tmp_res$graph
			c = clusters(g) 
			c_part = max(c$csize)
			if(c_part < N)
				(c_part^2 - c_part + N)^0.5/(N - c_part)
			else
				NA
			#s0 = tmp_res$paths
			#s = cor2cov(exp(log(0.99999)*s0), rnorm(N,mean=xx1,sd=xx2))
			#s_r = tryCatch({ rmnorm(1, rp0, s)}, error=function(ex){ s1=make.positive.definite(s, tol=0.001); rmnorm(1, rp0, s1) })
			#sum(s_r)/N
			
		}
		
		res_sd = res[!is.na(res)]
		res = rnorm(length(res_sd), mean=0, sd=res_sd)

		list(ts=res, sds=res_sd, params=c(x1,x2,x3,x4,xx1,xx2))
	}
}


calc_exp6_Snow_Memory = function(n_steps,corr_func,cells_num,x1,x2,x3,x4,x_FIX){
	rp0 = rep(0, cells_num)
	rp1 = rep(1, cells_num)
	g = graph(edges=c(),n=x1,directed=FALSE)
	c_part = 0 
	kapp = 0

	foreach(x_fixs = foreach(i=x_FIX[[1]],.combine=c)%do%{ foreach(j=x_FIX[[2]])%do%c(i,j) })%dopar%{
		#N = 25; a = 0.3; b = 2; d = 0.9; da = 0.15
		N = cells_num; a = x1; b = x2; d = x3; da = x4
		xx1 = x_fixs[1]; xx2 = x_fixs[2]
		#d_topple = d/b
		d_topple = da

		g = graph(edges=c(),n=N,directed=FALSE)
		v = V(g)
		for(i in 1:length(v)) V(g)[i]$m=0

		for(i in 1:ceiling(N*a/da)) g = get_corr_snow_graph_Increment(g - edges(g)[[1]],sample(N,1),b,da,d_topple)

		tmp_res = list()
		rp0 = rep(0, N)
		rp1 = rep(1, N)
		#res =array(0,0)
		res = foreach(i=1:n_steps, .combine=c)%do%{
if(i%%100==0) print(i)
			tmp_res = corr_func(g,N,a,b,d,da,d_topple)
			g = tmp_res$graph
			c = clusters(g) 
			c_part = max(c$csize)
			kapp = runif(1, c_part-1, c_part+1)
			if(kapp >= 0 && kapp < N)
				kapp/(N - kapp)
			else
				NA
			#s0 = tmp_res$paths
			#s = cor2cov(exp(log(0.99999)*s0), rnorm(N,mean=xx1,sd=xx2))
			#s_r = tryCatch({ rmnorm(1, rp0, s)}, error=function(ex){ s1=make.positive.definite(s, tol=0.001); rmnorm(1, rp0, s1) })
			#sum(s_r)/N
			
		}
		
		res_sd = res[!is.na(res)]
		res = rnorm(length(res_sd), mean=0, sd=res_sd)

		list(ts=res, sds=res_sd, params=c(x1,x2,x3,x4,xx1,xx2))
	}
}



# --------------------- #
# --------------------- #
# --------------------- #




proport_price_func = function(prc){
	BIG_NUM = 1000000000000 
	tick = 0.000001 
	r = prc 
	s = mean(r) 
	p = 0 
	while(abs(s)>tick && abs(p)<BIG_NUM){ 
		p = p + s 
		r = r*abs(s) 
		s = mean(r) 
	} 

	return(p)	
}


get_yhoo_shifted = function(yho,s,e){ 
	diff(log(as.numeric(yho[s:e]))) 
}


# p_len=100; p_func=get_corr_3; ps1=list(100,200,300); ps2=list(1,3,6); ps3=list(0.8,0.9,0.99); ps4=c(0.5,0.7,0.9); ps5=c(0.05,0.1,0.15); p_calc_func=calc_exp3; files_path='/mnt/G/RISK/ANTON/R/perc_corr/res-'
load_perc_corr_grid_files = function(p_len, p_func, ps1, ps2, ps3, p_calc_func, files_path){
	res = list()
	for(p1 in ps1) for(p2 in ps2) for(p3 in ps3){
		tmp_res = calc_data_params_vals_WITHFIX(list(p_len,p_func,p1,p2,p3),p_calc_func,files_path)
		for(i in 1:length(tmp_res[[2]])) for(j in 1:length(tmp_res[[3]]))
			res = c(res, list(list(params=c(p1,p2,p3,tmp_res[[2]][i],tmp_res[[3]][j]), data=tmp_res$ts[[i]][[j]])))
			
	}

	res	
}

load_perc_corr_grid_files_DOPAR = function(p_len, p_func, ps1, ps2, ps3, p_calc_func, files_path){
	res = foreach(p1=ps1, .combine=c)%dopar%{ foreach(p2=ps2, .combine=c)%dopar%{ foreach(p3=ps3, .combine=c)%do%{
		tmp_res = calc_data_params_vals(list(p_len,p_func,p1,p2,p3),p_calc_func,files_path)
		foreach(i=1:length(tmp_res[[2]]), .combine=c)%do%{ 
			foreach(j=1:length(tmp_res[[3]]), .combine=c)%do%{ 
				list(list(params=c(p1,p2,p3,tmp_res[[2]][i],tmp_res[[3]][j]), data=tmp_res$ts[[i]][[j]])) }}}}}

	res	
}

load_snow_files_DOPAR = function(p_len, p_func, ps1, ps2, ps3, ps4, ps5, p_calc_func, files_path){
	foreach(p1=ps1, .combine=c)%dopar%{ foreach(p2=ps2, .combine=c)%dopar%{ 
		foreach(p3=ps3, .combine=c)%do%{ foreach(p4=ps4, .combine=c)%do%{
			foreach(p5=ps5, .combine=c)%do%{
				tmp_res = calc_data_params_vals(list(p_len,p_func,p1,p2,p3,p4,p5),p_calc_func,files_path)
				foreach(r=tmp_res)%do%list(data=r$ts, params=r$params)
			}
		}}
	}}
}

shur_cdfg = function(x, cdf_type='ecdf',tresh=100000){
	#inter_diff = function(x){ res=c(); for(i in 1:(length(x)-1)) for(j in (i+1):length(x)) res=c(res,abs(x[i]-x[j])); res}
	inter_diff1 = function(x){ 
		d = floor(length(x)/10)
		unlist(foreach(k=0:(if(length(x)>d*10) 10 else 9))%do%{ 
			foreach(i=(1+k*d):min((k+1)*d,length(x)-1))%do%{ 
				foreach(j=(i+1):length(x))%do%{
					print(j)
					abs(x[i]-x[j])
				}}})    
	}

	inter_diff = function(x){ unlist(foreach(i=1:(length(x)-1))%do%{ res=c(); for(j in (i+1):length(x)) res=c(res,abs(x[i]-x[j])); res }) }

	x1=inter_diff(x) 
	#x1 = x1[x1<=quantile(x1,pcnt)]
	x1 = x1[x1<=tresh]
	if(cdf_type=='ecdf')
		ecdf(x1)
	else if(cdf_type=='my')
		list(x=sort(x1), y=(1:length(x1))/length(x1))
	else if(cdf_type=='plain')
		x1
}


show_hist_comparison = function(tss,br_num,den_cap=1000000){
	h_range = range(unlist(lapply(tss,function(ts){ range(ts); })))
	hists = lapply(tss,function(x) { hist(x,br=seq(h_range[1],h_range[2],l=br_num),plot=FALSE) })
	den_range = range(unlist(lapply(hists,function(x){ range(x$density) })))
	plot(hists[[1]]$mids[hists[[1]]$density<den_cap],hists[[1]]$density[hists[[1]]$density<den_cap],t='l',lwd=2,col=global_cols[1],xlim=h_range,ylim=den_range)
	if(length(hists)>1)
		for(i in 2:length(hists))
			lines(hists[[i]]$mids[hists[[i]]$density<den_cap],hists[[i]]$density[hists[[i]]$density<den_cap],t='l',lwd=2,col=global_cols[i])
}

show_tail_comparison = function(tss,tresh){
	tails = lapply(tss,function(x){ cx=sort(x); list(-log(-cx[cx<tresh]), log(((1:length(cx))/length(cx))[cx<tresh])) })
	rng_x = range(unlist(lapply(tails, function(x) { range(x[[1]]) })))
	rng_y = range(unlist(lapply(tails, function(x) { range(x[[2]]) })))
#plot(-log(-x),log(y),lwd=2,col='red');

	plot(tails[[1]][[1]],tails[[1]][[2]],t='p',lwd=2,col=global_cols[1],xlim=rng_x,ylim=rng_y)
	if(length(tails)>1)
		for(i in 2:length(tails))
			lines(tails[[i]][[1]],tails[[i]][[2]],t='p',lwd=2,col=global_cols[i],xlim=rng_x,ylim=rng_y)
}

#tss=list(ps_eq[ps_eq!=0]); x1=-100000; x2=-1
show_tail_comparison_RANGE = function(tss,x1,x2){
	tails = lapply(tss,function(x){ cx=sort(x); list(log(abs(cx[cx>x1 & cx<x2])), log(((1:length(cx))/length(cx))[cx>x1 & cx<x2])) })

	rng_x = range(unlist(lapply(tails, function(x) { range(x[[1]]) })))
	rng_y = range(unlist(lapply(tails, function(x) { range(x[[2]]) })))
#plot(-log(-x),log(y),lwd=2,col='red');

	plot(tails[[1]][[1]],tails[[1]][[2]],t='p',lwd=2,col=global_cols[1],xlim=rng_x,ylim=rng_y)
	if(length(tails)>1)
		for(i in 2:length(tails))
			lines(tails[[i]][[1]],tails[[i]][[2]],t='p',lwd=2,col=global_cols[i],xlim=rng_x,ylim=rng_y)
}

show_tail_comparison_NONLOG = function(tss,x1,x2){
	tails = lapply(tss,function(x){ cx=sort(x); list(cx[cx>x1 & cx<x2], ((1:length(cx))/length(cx))[cx>x1 & cx<x2]) })
	rng_x = range(unlist(lapply(tails, function(x) { range(x[[1]]) })))
	rng_y = range(unlist(lapply(tails, function(x) { range(x[[2]]) })))

	plot(tails[[1]][[1]],tails[[1]][[2]],t='p',lwd=2,col=global_cols[1],xlim=rng_x,ylim=rng_y)
	if(length(tails)>1)
		for(i in 2:length(tails))
			lines(tails[[i]][[1]],tails[[i]][[2]],t='p',lwd=2,col=global_cols[i],xlim=rng_x,ylim=rng_y)
}

list_arr_avg = function(lst){
	res = array(0,length(lst[[1]]))
	for(ar in lst)
		res = res + ar
	res/length(lst)
}



truncate_pc_params = function(x){
	c(max(floor(x[1]),2), max(floor(x[2]),1), min(max(x[3],0.00001),0.99), min(max(x[4],0.00001),0.999), max(x[5],0.000001))
}

#compare_mode = c('hist', 'cdf', 'scdf') 
compare_pc = function(pss,compare_mode,x_base,tresh,calc_lengths=c(50000,5000,1000)){
	get_pc_comp_data = function(ps){
		fn_prefix = '/mnt/G/RISK/ANTON/R/perc_corr/res-compare-'
		calc_data_params_lists_SEQ_WITHFIX(list(ps[[1]],ps[[2]],ps[[3]],ps[[4]],ps[[5]]),list(ps[[6]],ps[[7]]),ps[[8]],fn_prefix)
		load_perc_corr_grid_files_DOPAR(ps[[1]],ps[[2]],ps[[3]],ps[[4]],ps[[5]],ps[[8]],fn_prefix)[[1]]$data
	}		

	if(compare_mode == 'scdf'){
		tss = lapply(pss,function(x){ shur_cdfg(get_pc_comp_data(list(calc_lengths[3],get_corr_3,x[1],x[2],x[3],x[4],x[5],calc_exp3)),'ecdf',tresh) })
		lapply(tss,function(x){ list(x=x_base, y=x(x_base)) })
	} else if(compare_mode == 'hist'){
		tss = lapply(pss,function(x){ get_pc_comp_data(list(calc_lengths[1],get_corr_3,x[1],x[2],x[3],x[4],x[5],calc_exp3)) })
		rng = range(unlist(lapply(tss, range)))
		hss = lapply(1:length(tss),function(x){ hist(tss[[x]],br=if(length(x_base)>1) c(rng[1],x_base,rng[2]) else seq(rng[1],rng[2],l=x_base),plot=FALSE) })
		lapply(hss,function(x){ list(x=x$mids, y=x$density*range(diff(x$mids))[1]) })
	}
}

plot_my_series = function(srs,bold_i){
	xrng = range(unlist(lapply(srs,function(x){ range(x$x) })))
	yrng = range(unlist(lapply(srs,function(x){ range(x$y) })))
	plot(srs[[1]]$x,srs[[1]]$y,xlim=xrng,ylim=yrng,t='l',lwd=if(1==bold_i) 3 else 1,col=global_cols[1])
	if(length(srs)>1)
		for(i in 2:length(srs))
			lines(srs[[i]]$x,srs[[i]]$y,xlim=xrng,ylim=yrng,t='l',lwd=if(i==bold_i) 2 else 1,col=global_cols[i])

}


distrib_diff = function(x1,x2,diff_type,diff_len,diff_param){
	if(diff_type == 'CDF'){
		sx1 = x1
		sx2 = x2
	} else if(diff_type == 'Shur'){
		sx1 = shur_cdfg(x1,'plain'); sx2 = shur_cdfg(x2,'plain'); 
		q = mean(c(quantile(sx1,diff_param),quantile(sx2,diff_param)))
		sx1 = sx1[sx1<q]; sx2 = sx2[sx2<q]		
	} else if(diff_type == 'TwoShurs'){
		q = mean(c(quantile(x1,diff_param),quantile(x2,diff_param)))
		sx1 = x1[x1<q]; sx2 = x2[x2<q]		
	}

	rng = range(c(range(sx1),range(sx2))); rng_seq = seq(rng[1],rng[2],len=diff_len)
	sum((ecdf(sx1)(rng_seq)-ecdf(sx2)(rng_seq))^2)/length(rng_seq)
}


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

PPI_get_params_bounds = function(dats){
	dp = foreach(i=1:length(dats[[1]]$params))%do%{ unique(foreach(d=dats,.combine=c)%do%d$params[i]) }
	list(mins=foreach(p=dp,.combine=c)%do%min(p),maxs=foreach(p=dp,.combine=c)%do%max(p))
}

# sN = length(res[[1]]$data)
PPI = function(bnd,p,dats,sN){
	twos = which(2 == foreach(b=bnd,.combine=c)%do%length(b))
	if(length(twos) > 0){
#print(1)
		t = twos[1]; p1 = bnd[[t]][1]; p2 = bnd[[t]][2]
		bnd1 = bnd; bnd1[[t]] = p1
		bnd2 = bnd; bnd2[[t]] = p2
#print('split')
		ts1 = PPI(bnd1,p,dats,sN); len1 = floor(2*sN*(p2-p[t])/(p2-p1))
		ts2 = PPI(bnd2,p,dats,sN); len2 = floor(2*sN*(p[t]-p1)/(p2-p1))
		c(sample(ts1,len1,replace=TRUE),sample(ts2,len2,replace=TRUE))
	} else {
		t = which(as.logical(foreach(d=dats,.combine=c)%do%min(d$params==unlist(bnd))))
#print(paste(paste(unlist(bnd),collapse='-'),': ',sd(dats[[t]]$data)))
#print(length(dats[[t]]$data))
		dats[[t]]$data
	}
}




######### ---- BAYES ---- ##############


# q_borders - quantile borders (-Inf,...,Inf) dataframe; ts - [price] time series; 
# result: for every time moment retrieve quantile vector and record which quantile the ts element falls into.
# Returned value is "equality" measure, saying "how equal" are the empiric "variable quantile" counts.
# q_borders=qb; tss = as.numeric(VALS_ACT)
my_bayes = function(q_borders, tss, verify_type = 'CELLS'){
	hits = matrix(0,dim(q_borders)[1],dim(q_borders)[2]-1)
	for(i in 1:nrow(q_borders)) hits[i,] = diff(q_borders[i,]>tss[i])
	#sd((foreach(i=1:nrow(q_borders),.combine=sum)%do%)/length(tss))	

	if(verify_type == 'NONE') list(res=sd(rowSums(t(hits/length(tss))))) else 
		if(verify_type == 'CELLS') list(res=rowSums(t(hits/length(tss)))) else
		if(verify_type == 'HITS') list(hits=hits, res=rowSums(t(hits/length(tss)))) 
}

my_bayes_ThisCase = function(tss,cf){
	len = length(tss)
	params = matrix(0,len,4); qb = matrix(0,len,8)
	for(i in 1:len) params[i,] = indic_to_param(indics[i,],cf)
	tmp_qb = foreach(i=1:len)%dopar%{ 
		if(i%%50==0)print(i); 
		param_q_Simulate(params[i,],8) 
	}; 
	for(i in 1:len) qb[i,] = tmp_qb[[i]]

	my_bayes(qb,tss)	
}



try_bayes = function(dats_len,ts_len){
	options(stringsAsFactors = FALSE)
	indic_to_param = function(ind,cf) { c(ind[1]*cf[1],ind[2]*cf[2],ind[3]*cf[3],ind[4]*cf[4]) }
	indic_to_param2 = function(ind,cf) { c(sum(ind*cf[1:4]),sum(ind*cf[5:8]),sum(ind*cf[9:12]),sum(ind*cf[13:16])) }
	param_to_val_Simulate = function(p,prob_tmp) { if(prob_tmp>0.5) rnorm(1,p[1],p[2]) else rnorm(1,p[3],p[4]) }
	param_q_Simulate = function(p,q_count){ 
		len = 10000
		r1 = rnorm(len,p[1],p[2]); r2 = rnorm(len,p[3],p[4])
		c(-Inf,as.numeric(quantile(ifelse(runif(len)>0.5,r1,r2),seq(0,1,len=q_count)))[2:(q_count-1)],Inf)
	}
	param_sample_Simulate = function(p,len){ 
		r1 = rnorm(len,p[1],p[2]); r2 = rnorm(len,p[3],p[4])
		ifelse(runif(len)>0.5,r1,r2)
	}

	# dats_len = 10000
	dats = foreach(i1=1:4,.combine=c)%do%{ foreach(i2=1:4,.combine=c)%do%{ foreach(i3=1:4,.combine=c)%do%{ foreach(i4=1:4)%do%{ p=c(i1,i2,i3,i4); list(data=param_sample_Simulate(p,dats_len), params=p) }}}}
	#dats = foreach(i1=0.5*(1:8),.combine=c)%do%{ foreach(i2=0.5*(1:8),.combine=c)%do%{ foreach(i3=0.5*(1:8),.combine=c)%do%{ foreach(i4=0.5*(1:8))%do%{ p=c(i1,i2,i3,i4); list(data=param_sample_Simulate(p,dats_len), params=p) }}}}

	cf_ACT = 0.25*(array(1,16)+0.8*(runif(16)-0.5))
	N = ts_len; indics=matrix(0,N,4); params_ACT=matrix(0,N,4); vals_ACT=array(0,N); 
	for(i in 1:N) indics[i,] = sample(4,4)
	for(i in 1:N) params_ACT[i,] = indic_to_param2(indics[i,],cf_ACT)
	r = sample(0:1,N,replace=TRUE); for(i in 1:N) vals_ACT[i] = param_to_val_Simulate(params_ACT[i,],r[i])
	Q_CNT = 8; BNDS = PPI_get_params_bounds(dats)

	a=get(load('/mnt/G/RISK/ANTON/R/RData/a.RData'))
	cf_tmp=a$par


	len = N
	params = matrix(0,len,4); qb = matrix(0,len,Q_CNT); p=1:4
	for(i in 1:len) {
		p = indic_to_param2(indics[i,],cf_tmp)
		params[i,] = ifelse(p<BNDS[[1]],BNDS[[1]],ifelse(p>BNDS[[2]],BNDS[[2]],p))
	}
	tmp_qb = foreach(i=1:len)%dopar%{ 
		if(i%%5==0)print(paste('dats:',dats_len,'N:',ts_len,'len step:',i)); 
		#param_q_Simulate(params[i,],8) 
		s = paramed_pdf_interpol(dats,params[i,])
		c(-Inf,as.numeric(quantile(s,seq(0,1,len=Q_CNT)))[2:(Q_CNT-1)],Inf)
	}; 
	for(i in 1:len) qb[i,] = tmp_qb[[i]]

	my_bayes(qb,vals_ACT)
}


try_bayes1 = function(cf_tmp){
	#print(cf_tmp)

	pms_tmp = foreach(i=1:TS_LEN,.combine=rbind)%do%indic_to_param(INDICES[i,],cf_tmp) 
	#pms_tmp = t(t(INDICES)*cf_tmp)
	if(min(foreach(i=1:P_CNT,.combine=c)%do%{ r=range(pms_tmp[,i]); MY_PARAMS_BND[[i]][1]<=r[1] && MY_PARAMS_BND[[i]][2]>=r[2] }) == 1) {
		qb = matrix(0,TS_LEN,Q_CNT); 
		q_cnt_seq = seq(0,1,len=Q_CNT)
		q_cnt_cnt = 2:(Q_CNT-1)
		tmp_qb = #foreach(i=1:20,.combine=c)%do%{ 
			#foreach(j=0:((TS_LEN/20)-1))%do%
			foreach(i=1:TS_LEN)%do%{
				c(-Inf,
				as.numeric(quantile(MY_DISTRIBS[[which.min(as.numeric(rowSums(t(t(MY_PARAMS) - indic_to_param(INDICES[i,],cf_tmp))^2)))]]$data, q_cnt_seq))[q_cnt_cnt],
				#as.numeric(quantile(MY_DISTRIBS[[which.min(as.numeric(rowSums(t(t(MY_PARAMS) - indic_to_param(INDICES[j*20+i,],cf_tmp))^2)))]]$data, q_cnt_seq))[q_cnt_cnt],
				Inf) }
	
		#print('bayes')
		for(i in 1:TS_LEN) qb[i,] = tmp_qb[[i]]	
		m = my_bayes(qb,VALS_ACT)
		#print(m)
		m
	} else 1000000
}


# Every  histogram is VIX[t]-adjusted now
# assign<- vix variable before run
try_bayes2 = function(cf_tmp, do_verify = FALSE){
	print(cf_tmp)

	pms_tmp = foreach(i=1:TS_LEN,.combine=rbind)%do%indic_to_param(if(is.null(dim(INDICES))) INDICES[i] else INDICES[i,],cf_tmp) 
	#pms_tmp = t(t(INDICES)*cf_tmp)
	if(min(foreach(i=1:P_CNT,.combine=c)%do%{ r=range(pms_tmp[,i]); MY_PARAMS_BND[[i]][1]<=r[1] && MY_PARAMS_BND[[i]][2]>=r[2] }) == 1) {
		qb = matrix(0,TS_LEN,Q_CNT); 
		q_cnt_seq = seq(0,1,len=Q_CNT)
		q_cnt_cnt = 2:(Q_CNT-1)
		tmp_qb = #foreach(i=1:20,.combine=c)%do%{ 
			#foreach(j=0:((TS_LEN/20)-1))%do%
			foreach(i=1:TS_LEN)%do%{
				#print(i)
				c(-Inf,
				MY_DISTRIBS[[which.min(as.numeric(rowSums(t(t(MY_PARAMS) - indic_to_param(if(is.null(dim(INDICES))) INDICES[i] else INDICES[i,],cf_tmp))^2)))]]$quant[q_cnt_cnt]*vix[i],
				#as.numeric(quantile(MY_DISTRIBS[[which.min(as.numeric(rowSums(t(t(MY_PARAMS) - indic_to_param(if(is.null(dim(INDICES))) INDICES[i] else INDICES[i,],cf_tmp))^2)))]]$data*vix[i], q_cnt_seq))[q_cnt_cnt],
				##as.numeric(quantile(MY_DISTRIBS[[which.min(as.numeric(rowSums(t(t(MY_PARAMS) - indic_to_param(if(is.null(dim(INDICES))) INDICES[j*20+i] else INDICES[j*20+i,],cf_tmp))^2)))]]$data, q_cnt_seq))[q_cnt_cnt],
				Inf) 
			}
	
		#print('bayes')
		for(i in 1:TS_LEN) qb[i,] = tmp_qb[[i]]	
		m = my_bayes(qb,as.numeric(VALS_ACT),if(do_verify) 'HITS' else 'CELLS')
		#print(m)
		if(do_verify){
			#h0 = array(0,ncol(m$hits))
			#h = foreach(i=1:nrow(m$hits),.combine=c)%do%{ h0 = h0 + m$hits[i,]; sd(h0/i) }
			#list(value=sd(m$res), cells=m$res, hits=m$hits, qb=qb, vals=VALS_ACT)  else list(sd=sd(m$res),cells=m$res)
			#list(sd=sd(m$res), cells=m$res, hits=m$hits, qb=qb, vals=VALS_ACT)  else list(sd=sd(m$res),cells=m$res)
			#list(value=mean(c(sd(m$res*Q_CNT),cor(m$res[2:length(m$res)],m$res[1:(length(m$res)-1)]),sum(diff(h)>0)/length(h))), cells=m$res, hits=m$hits, qb=qb, vals=VALS_ACT)

			h = m$hits
			wnd = 60
			list(value=sqrt(mean(
#aggregate(1:nrow(h),by=list((1:nrow(h))%/%wnd),FUN=function(x){ sum(abs(rowSums(t(h[x,]/wnd))[c(1,Q_CNT-1)]-0.05)) })[,2]^2
#aggregate(1:nrow(h),by=list((1:nrow(h))%/%wnd),FUN=function(x){ sum(abs(rowSums(t(h[x,]/wnd))[c(1,2,3)]-0.05)) })[,2]^2
aggregate(1:nrow(h),by=list((1:nrow(h))%/%wnd),FUN=function(x){ sum(abs(rowSums(t(h[x,]/wnd))[2]-0.05)) })[,2]^2
)), cells=m$res, hits=m$hits, qb=qb, vals=VALS_ACT)
		} else list(value=sd(m$res),cells=m$res)
	} else list(value=1000000)
	#} else 1000000
}




######### ---- BAYES OPTIMIZATION ---- ##############



find_steps_steps0 = function(min_val, x, grad){
	steps0 = expand.grid(foreach(i=1:length(x))%do%c(-1,0,1))
	steps = t(x+grad*t(steps0))
	res = foreach(i=1:dim(steps)[1],.combine=c)%dopar%{ print(i); print(paste(min_val,x,grad,steps[i,],sep=', ')); try_bayes1(as.numeric(steps[i,])) }
	list(res_full=res, steps_full=steps, step=steps[which.min(res),], res=min(res))
}

find_steps2 = function(min_val, x, grad){
	steps = t(x+grad*t(expand.grid(foreach(i=1:length(x))%do%sample(c(-1,0,1),2))))
	res = foreach(i=1:dim(steps)[1],.combine=c)%dopar%{ print(i); try_bayes1(as.numeric(steps[i,])) }
	list(res_full=res, steps_full=steps, step=steps[which.min(res),], res=min(res))
}

find_steps_vector = function(min_val, x, grad, vec, n_steps){
	steps = t(x+t(t(matrix(vec,length(vec),n_steps))*seq(0,grad,len=n_steps)))
	res = foreach(i=1:dim(steps)[1],.combine=c)%dopar%{ print(i); try_bayes1(as.numeric(steps[i,])) }
	list(res_full=res, steps_full=steps, step=steps[which.min(res),], res=min(res))
}

find_my_opt = function(pt1_in, pt_in, grad){
	x = pt1_in; y = pt_in
	y = find_steps2(x$res, x$step, grad); print('Results:'); print(x$res); print(y$res); print(y$res<x$res)

	while(y$res < x$res){
		x = y
		y = find_steps2(x$res, x$step, grad); print('Results:'); print(x$res); print(y$res); print(y$res<x$res)
	}
	list(x=x,y=y)
}

plot_linear_valuation = function(ts){
	gg = function(x,y) { -exp(seq(log(x),log(y),len=50)) }
	ee = foreach(x=seq(0.0001,0.1,len=100),.combine=rbind)%do%{
		x1 = log(-gg(x,0.3)) 
		y1 = log(e(gg(x,0.3))) 
		c(x, summary(lm(y1 ~ x1))$coef[2,2])
	}
}

run_my_opt = function(pt1_in, pt_in){
	ptt = find_my_opt(pt1_in,pt_in,0.1)
	ptt = find_my_opt(ptt$x,ptt$y,0.05)
	ptt = find_my_opt(ptt$x,ptt$y,0.01)
	ptt = find_my_opt(ptt$x,ptt$y,0.005)
	ptt = find_my_opt(ptt$x,ptt$y,0.001)
	ptt = find_my_opt(ptt$x,ptt$y,0.0005)
	ptt
}

update_ids0 = function(path_root){
	my_get_symbol = function(name){
		system(paste('wget --no-check-certificate "https://research.stlouisfed.org/fred2/series/',name,'/downloaddata/',name,'.csv" -O "',path_root,name,'.csv"',sep=''))
		dat = read.csv(paste(path_root,name,'.csv',sep=''),stringsAsFactors=FALSE)
		xts(as.numeric(dat[,2]),order.by=as.Date(dat[,1])) 
	}

	#getSymbols('^VIX')
	snp = diff(log(my_get_symbol('SP500'))) #diff(log(get(getSymbols('^gspc'))$GSPC.Close)) 
	snpindex = my_get_symbol('SP500') #diff(log(get(getSymbols('^gspc'))$GSPC.Close)) 
	trs = my_get_symbol('DGS10') 
	lbr = my_get_symbol('USD3MTD156N')
	vix0 = my_get_symbol('VIXCLS') #get('VIX')$VIX.Close
	ids0 = merge.xts(snp,trs,lbr,vix0,snpindex) 
	colnames(ids0)=c('SP500','DGS10','USD3MTD156N','VIX.Close','SP500INDEX') 
	save(ids0,file='ids0xts.RData')
}




