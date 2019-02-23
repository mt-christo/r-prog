

calc1 = function(n_in,b_in,da_in,conc_in){
	N = n_in; b = b_in; da = da_in; l = conc_in
	g = graph(edges=c(),n=N,directed=FALSE)
	v = V(g)
	for(i in 1:length(v)) V(g)[i]$m=0

	cs = array(0,0)
	for(i in 1:3000) {
		g = get_corr_snow_graph_Increment_CRYSTAL(g - edges(g)[[1]],sample(N,1),if(sum(V(g)$m)>l)sample(N,1) else c(),b,da)
		if(i%%100==0) print(i)
	}
	for(i in 1:40000) {
		g = get_corr_snow_graph_Increment_CRYSTAL(g - edges(g)[[1]],sample(N,1),if(sum(V(g)$m)>l)sample(N,1) else c(),b,da)
		cs[length(cs)+1] = max(clusters(g)$csize)
		if(i%%100==0) print(i)
	}
	cs
}

ress = list(calc1(500,5,0.1,0.5),calc1(500,5,0.3,0.5),calc1(500,5,0.1,0.7),calc1(500,5,0.3,0.7))

plg = function(r,cols){
	a = aggregate(r[[1]],list(1*(1+r[[1]]%/%1)),FUN=length)
	x=a[-1,1]; y=a[-1,2]
	plot(log(x),log(y),t='l',col=cols[1],lwd=3,xlim=c(0,5),ylim=c(0,15))
	if(length(r)>1)
		for(i in 2:length(r)){
			a = aggregate(r[[i]],list(1*(1+r[[i]]%/%1)),FUN=length)
			x=a[-1,1]; y=a[-1,2]
			lines(log(x),log(y),t='l',col=cols[i],lwd=3,xlim=c(0,5),ylim=c(0,15))
		}
}

plg(ress,c('red','blue','green','orange'))
plg(ress[3],c('green'))

r1 = res[[1]]; r2 = res[[2]]
a1 = aggregate(r1,list(3*(1+r1%/%3)),FUN=length)
a2 = aggregate(r2,list(3*(1+r2%/%3)),FUN=length)

x1=a1[-1,1]; y1=a1[-1,2]
x2=a2[-1,1]; y2=a2[-1,2]
plot(log(x1),log(y1),t='l',col='blue',lwd=3,xlim=c(0,5),ylim=c(0,15))
lines(log(x2),log(y2),t='l',col='red',lwd=3,xlim=c(0,5),ylim=c(0,15))












get_corr_snow_graph_TOPPLE_CRYSTAL <- function(g_in,v_idxs,b_in){
    g_out = g_in
    all_v_idxs = 1:vcount(g_out)
    front = array(0,0)
    for(i in v_idxs){
	for(j in sample(all_v_idxs,b_in))
		if(V(g_out)[j]$m > 1) {
		    V(g_out)[j]$m = V(g_out)[j]$m - 1

		    recs = sample(all_v_idxs,b_in)
		    V(g_out)[recs]$m = V(g_out)[recs]$m + 1/b_in

		    g_out = g_out + edge(i,j)
		    front = c(front,j)
		}
    }
    return(list(graph=g_out, front=front))    
}

get_corr_snow_graph_Increment_CRYSTAL <- function(g_in,v_idx1,v_idx2,b_in,da_in){
    g_out = g_in

    V(g_out)[v_idx1]$m = V(g_out)[v_idx1]$m + da_in 
    if(length(v_idx2)>0)
	V(g_out)[v_idx2]$m = V(g_out)[v_idx2]$m - da_in 

    front = array(0,0)
    if(V(g_out)[v_idx1]$m > 1+da_in) front = c(front,v_idx1)

    while(length(front) > 0){
	topple = get_corr_snow_graph_TOPPLE_CRYSTAL(g_out,front,b_in)
	g_out = topple$graph
	front = topple$front
    }

    return(g_out)
}




source('R121.R')
params1 = list(25,0,3,0.7,0.3)
params2 = list(1.001,1000000000)
calc_data_params_lists_WITHFIX(c(10000,get_corr_snow_Memory_CRYSTAL,params1),params2,calc_exp41_Snow_Memory,'RData/res_CRYSTAL-')

r = get(load('RData\\res_CRYSTAL-10000-25-0-3-0.7-0.3.RData'))[[1]]$ts
r = get(load('RData\\res_CRYSTAL-30000-100-0-2-0.7-0.3.RData'))[[1]]$ts

r1 = -r[r<0]; a = aggregate(r1,list(0.3*(r1%/%0.3)),FUN=length); a=a[a[,2]>1,]; plot(log(a[,1]),log(a[,2]));

#n_steps=1; corr_func=get_corr_snow_Memory_CRYSTAL; cells_num=25; x1=0; x2=3; x3=0.7; x4=0.3; x_FIX=list(1.01,1000)
calc_exp41_Snow_Memory = function(n_steps,corr_func,cells_num,x1,x2,x3,x4,x_FIX){

	foreach(x_fixs = foreach(i=x_FIX[[1]],.combine=c)%do%{ foreach(j=x_FIX[[2]])%do%c(i,j) })%dopar%{
		N = cells_num; a = x1; b = x2; d = x3; da = x4
		# x_fixs = c(x_FIX[[1]][1], x_FIX[[2]][1])
		xx1 = x_fixs[1]; xx2 = x_fixs[2]

		g = graph(edges=c(),n=N,directed=FALSE)
		v = V(g)
		for(i in 1:length(v)) V(g)[i]$m=0
		for(i in 1:ceiling(N*d/da)) { j = sample(N, 1); V(g)[j]$m = V(g)[j]$m + da }
		tmp_res = list()
		rp0 = rep(0, N)
		rp1 = rep(1, N)

		tmp_res = corr_func(g,N,a,b,d,da); print(max(clusters(tmp_res$graph)$csize))
if(i%%100==0) print(i)
			g = tmp_res$graph
			cs = clusters(g)
			s0 = tmp_res$paths

			#sd_rnd = 1/runif(N); 
			#sd_rnd = (1-(xx1-1)*runif(N,min=0,max=1/(xx1-1)))^(-1/(xx1-1))
			#s = cor2cov(exp(log(0.99999)*s0),  sd_rnd)
			#s_r = tryCatch({ rmnorm(1, rp0, s)}, error=function(ex){ s1=make.positive.definite(s, tol=0.001); rmnorm(1, rp0, s1) })

			idx_corr = cs$membership==which.max(cs$csize)
			alf = xx1 - 1
			max_sz = xx2
			sd_rnd = (1-alf*runif(N,min=0,max=(1-1/max_sz^alf)/alf))^(-1/alf)
			sd_rnd = sd_rnd/sum(sd_rnd)
print(sum(idx_corr))
			
			s_r = array(0,N)
			s_r[idx_corr] = rnorm(1)*sd_rnd[idx_corr]
			s_r[!idx_corr] = rnorm(sum(!idx_corr),sd = sd_rnd[!idx_corr])

			if(max(cs$csize) < N)
				sum(s_r)/(1-sum(sd_rnd[idx_corr]))
			else
				NA
		}

		list(ts=res[!is.na(res)], params=c(x1,x2,x3,x4,xx1,xx2))
	}
}



xx1 = 1.01
xx2 = 1000000000
rr=array(0,0); sms=array(0,0); rs=array(0,0); ress=array(0,0)
r = foreach(i=1:5000,.combine=c)%do%{
			tmp_res = corr_func(g,N,a,b,d,da)
			if(i%%100==0) 
				print(i)
			g = tmp_res$graph
			rr[length(rr)+1] = max(clusters(g)$csize)

			cs = clusters(g)
			idx_corr = cs$membership==which.max(cs$csize)
			alf = xx1 - 1
			max_sz = xx2
			sd_rnd = (1-alf*runif(N,min=0,max=(1-1/max_sz^alf)/alf))^(-1/alf)
			sd_rnd = sd_rnd/sum(sd_rnd)
			
			s_r = array(0,N)
			s_r[idx_corr] = rnorm(1)*sd_rnd[idx_corr]
			s_r[!idx_corr] = rnorm(sum(!idx_corr),sd = sd_rnd[!idx_corr])

			rs[length(rs)+1] = if(max(cs$csize) < N)
							sum(s_r)
						else
							NA

			ress[length(ress)+1] = if(max(cs$csize) < N)
		  					sum(s_r)/(1-sum(sd_rnd[idx_corr]))
						else
							NA

			sms[length(sms)+1] = 1/(1-sum(sd_rnd[idx_corr]))
}

a = aggregate(rr,list(rr),FUN=length); x=log(a[,1]); y=log(a[,2])
plot(log(a[,1]),log(a[,2]),xlim=c(0,10),ylim=c(0,10))

max_sz = 1000000000
alf = 0.1
N=100
r = (1-alf*runif(N,min=0,max=(1-1/max_sz^alf)/alf))^(-1/alf)
r = r/sum(r)

a = aggregate(r,list(0.01*(1+r%/%0.01)),FUN=length); x=log(a[,1]); y=log(a[,2])














get_corr_snow_graph_Increment_CRYSTAL <- function(g_in,v_idx1,v_idx2,b_in,da_in){

    g_out = g_in


    V(g_out)[v_idx1]$m = V(g_out)[v_idx1]$m + da_in


    if(length(v_idx2)>0)

	V(g_out)[v_idx2]$m = V(g_out)[v_idx2]$m - da_in



    front = array(0,0)

    if(V(g_out)[v_idx1]$m > 1+da_in)
	front = c(front,v_idx1)



    while(length(front) > 0){

	topple = get_corr_snow_graph_TOPPLE_CRYSTAL_NONC(g_out,front,b_in)

	g_out = topple$graph

	front = topple$front

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









get_corr_snow_graph_TOPPLE_CRYSTAL_NONC <- function(g_in,v_idxs,b_in){

    g_out = g_in

    vcnt = vcount(g_out)

    all_v_idxs = 1:vcnt

    front = array(0,0)


    for(i in v_idxs){

	cs = clusters(g_out)	
	mc_id = which.max(cs$csize)
	nc_vertices = which(cs$membership!=mc_id)

	for(j in sample(nc_vertices,b_in,replace=TRUE))

		if(V(g_out)[j]$m > 1) {

		    V(g_out)[j]$m = V(g_out)[j]$m - 1

		    V(g_out)$m = V(g_out)$m + 1/vcnt

		    g_out = g_out + edge(i,j)

		    front = c(front,j)

		}

    }


    return(list(graph=g_out, front=front))    

}




## "d" is Concentration (when sinking starts)!!
get_corr_snow_Memory_CRYSTAL = function(g_in_Global,N,a,b,d,da,d_topple){
	g = get_corr_snow_graph_Increment_CRYSTAL(g_in_Global - edges(g_in_Global)[[1]], sample(N,1), if(sum(V(g_in_Global)$m)>d*N)sample(N,1) else c(), b, da)
	return(list(graph = g, paths = shortest.paths(g)))
}






#n_steps=1; corr_func=get_corr_snow_Memory_CRYSTAL; cells_num=25; x1=0; x2=3; x3=0.7; x4=0.15; x_FIX=list(1,0)
calc_exp41_Snow_Memory = function(n_steps,corr_func,cells_num,x1,x2,x3,x4,x_FIX){

	foreach(x_fixs = foreach(i=x_FIX[[1]],.combine=c)%do%{ foreach(j=x_FIX[[2]])%do%c(i,j) })%dopar%{
		N = cells_num; a = x1; b = x2; d = x3; da = x4
		# x_fixs = c(x_FIX[[1]][1], x_FIX[[2]][1])
		xx1 = x_fixs[1]; xx2 = x_fixs[2]

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
			sd_rnd = sd_rnd/sum(sd_rnd)
			
			s_r = array(0,N)
			s_r[idx_corr] = rnorm(1)*sd_rnd[idx_corr]
			s_r[!idx_corr] = rnorm(sum(!idx_corr),sd = sd_rnd[!idx_corr])

			if(max(cs$csize) < N)
				sum(s_r)/(1-sum(sd_rnd[which(cs$membership==which.max(cs$csize))]))
			else
				NA
		}

		list(ts=res[!is.na(res)], params=c(x1,x2,x3,x4,xx1,xx2))
	}
}














source('R4.R')
source('R121.R')
library(foreach)
params1 = list(25,0,c(2,3,4),0.5,c(0.05,0.1,0.2))
params2 = list(1.5,1000000000)

calc_data_params_lists_SEQ_WITHFIX(c(100000,get_corr_snow_Memory_CRYSTAL,params1),params2,calc_exp41_Snow_Memory,'RData/res_CRYSTAL-1.5-') 

getpl = function(x, is_plot, col){
	r = get(load(x))[[1]]$ts;
	if(is_plot)	
		plot(ecdf(r)(-seq(0.1,20,by=0.001)),log='xy',t='l',lwd=2,col=col)
	else
		lines(ecdf(r)(-seq(0.1,20,by=0.001)),log='xy',t='l',lwd=2,col=col)
}


getpl('RData\\res_CRYSTAL-1.5-1e+05-25-0-2-0.5-0.1.RData',TRUE,'red')
getpl('RData\\res_CRYSTAL-1.5-1e+05-25-0-3-0.5-0.1.RData',FALSE,'blue')

getpl('RData\\res_CRYSTAL-1.3-1e+05-25-0-2-0.5-0.1.RData',FALSE,'red')
getpl('RData\\res_CRYSTAL-1.3-1e+05-25-0-3-0.5-0.1.RData',FALSE,'blue')

getpl('RData\\res_CRYSTAL-1.1-1e+05-25-0-2-0.5-0.2.RData',FALSE,'red')
getpl('RData\\res_CRYSTAL-1.1-1e+05-25-0-3-0.5-0.1.RData',FALSE,'blue')
r1 = get(load('RData\\res_CRYSTAL-200000-25-0-2-0.5-0.1.RData'))[[1]]$ts;
r2 = get(load('RData\\res_CRYSTAL-1.1-5000-25-0-2-0.5-0.1.RData'))[[1]]$ts;
r3 = get(load('RData\\res_CRYSTAL-1.1-5000-25-0-2-0.5-0.1.RData'))[[1]]$ts;
#r4 = get(load('RData\\res_CRYSTAL-1.01-20000-25-0-3-0.5-0.4.RData'))[[1]]$ts;

plot(ecdf(r1)(-seq(0.1,20,by=0.001)),log='xy',t='l',lwd=2,col='red')
lines(ecdf(r2)(-seq(0.1,20,by=0.001)),log='xy',t='l',lwd=2,col='orange')
lines(ecdf(r3)(-seq(0.1,20,by=0.001)),log='xy',t='l',lwd=2,col='blue')

d = get(load('RData\\res_CRYSTAL-2e+05-25-0-2-0.5-0.1.RData'))
r = d[1.01==foreach(d0=d)%do%d0$params[5]]


r = d[1.01==foreach(d0=d)%do%d0$params[5]][[1]]$ts; plot(ecdf(r)(-seq(0.1,20,by=0.001)),log='xy',t='l',lwd=2,col='red')
r = d[1.03==foreach(d0=d)%do%d0$params[5]][[1]]$ts; lines(ecdf(r)(-seq(0.1,20,by=0.001)),log='xy',t='l',lwd=2,col='orange')
r = d[1.05==foreach(d0=d)%do%d0$params[5]][[1]]$ts; lines(ecdf(r)(-seq(0.1,20,by=0.001)),log='xy',t='l',lwd=2,col='blue')
r = d[1.07==foreach(d0=d)%do%d0$params[5]][[1]]$ts; lines(ecdf(r)(-seq(0.1,20,by=0.001)),log='xy',t='l',lwd=2,col='darkgreen')
r = d[1.09==foreach(d0=d)%do%d0$params[5]][[1]]$ts; lines(ecdf(r)(-seq(0.1,20,by=0.001)),log='xy',t='l',lwd=2,col='green')
r = d[1.11==foreach(d0=d)%do%d0$params[5]][[1]]$ts; lines(ecdf(r)(-seq(0.1,20,by=0.001)),log='xy',t='l',lwd=2,col='yellow')


#r_in=r
plotc = function(r_in,x0,x1,pl_func,col){
	x=exp(seq(log(x0),log(x1),len=1000)); y=ecdf(r_in)(-x); 
	pl_func(log(x),log(y),t='l',lwd=2,col=col)#,xlim=c(0,4),ylim=c(-9,-3))
	summary(lm(y~x))$coeff[2,1]
}


r = d[1.11==foreach(d0=d)%do%d0$params[5]][[1]]$ts; plotc(r,0.001,-min(r),plot,'brown')
r = d[1.09==foreach(d0=d)%do%d0$params[5]][[1]]$ts; plotc(r,0.001,-min(r),lines,'green')
r = d[1.07==foreach(d0=d)%do%d0$params[5]][[1]]$ts; plotc(r,0.001,-min(r),lines,'darkgreen')
r = d[1.05==foreach(d0=d)%do%d0$params[5]][[1]]$ts; plotc(r,0.001,-min(r),lines,'blue')
r = d[1.03==foreach(d0=d)%do%d0$params[5]][[1]]$ts; plotc(r,0.001,-min(r),lines,'orange')
r = d[1.01==foreach(d0=d)%do%d0$params[5]][[1]]$ts; plotc(r,0.001,-min(r),lines,'red')

plot(-(1+0.01*1:20),foreach(i=1+0.01*1:20,.combine=c)%do%plotc(d[i==foreach(d0=d)%do%d0$params[5]][[1]]$ts,lines,'red'),t='l')


plot(log(1:100),log(1/(1:100)))


a_step = 0.2
#x=r1; x=x[abs(x)<10]; sd(x); hist(x[x>-5 & x<5],xlim=c(-5,5),br=50)
a=list(); x=r1; x=-x[x<0]; ag = aggregate(x,list(a_step*(1+x%/%a_step)),FUN=length); ag = ag[ag[,2]>1,]; a=c(a,list(list(ag[,1],ag[,2])))
x=r2; x=-x[x<0]; ag = aggregate(x,list(a_step*(1+x%/%a_step)),FUN=length); ag = ag[ag[,2]>1,]; a=c(a,list(list(ag[,1],ag[,2])))
x=r3; x=-x[x<0]; ag = aggregate(x,list(a_step*(1+x%/%a_step)),FUN=length); ag = ag[ag[,2]>1,]; a=c(a,list(list(ag[,1],ag[,2])))
#x=r4; x=-x[x<0]; ag = aggregate(x,list(a_step*(1+x%/%a_step)),FUN=length); ag = ag[ag[,2]>1,]; a=c(a,list(list(ag[,1],ag[,2])))


x=a[[1]]; plot(log(x[[1]]),log(x[[2]]),t='l',lwd=2,col='red')
x=a[[2]]; lines(log(x[[1]]),log(x[[2]]),t='l',lwd=2,col='blue')
x=a[[3]]; lines(log(x[[1]]),log(x[[2]]),t='l',lwd=2,col='orange')
#x=a[[4]]; lines(log(x[[1]]),log(x[[2]]),t='l',lwd=2,col='darkgreen')


a1=a1[a1>-100000 & a1<1000000]

a2 = get(load('RData\\res_CRYSTAL-10000-25-0-3-0.5-0.4.RData'))[[1]]$ts; a2=a2[a2>-100000 & a2<1000000]
a3 = get(load('RData\\res_CRYSTAL-10000-25-0-3-0.7-0.15.RData'))[[1]]$ts; a3=a3[a3>-100000 & a3<1000000]
a4 = get(load('RData\\res_CRYSTAL-10000-25-0-3-0.7-0.4.RData'))[[1]]$ts; a4=a4[a4>-100000 & a4<1000000]










