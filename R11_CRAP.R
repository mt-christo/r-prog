#source('R12.R')
#source('/mnt/G/RISK/ANTON/R/R12.R')  ssh sr-risky cd /mnt/G/RISK/ANTON/R

if(1==0){
	params1 = list(50,0.3,2,0.9,0.4)
	params2 = list(0.1*(2:5),1)

	source('R12.R') # c(0.3,2,0.5,0.1,0.5,0.1)
	params1 = list(10,0.3,list(2,3),list(0.9,0.95,0.97),list(0.05,0.1,0.15))
	params2 = list(0.2*(3:5),0.05*(1:4))
	calc_data_params_lists_WITHFIX(c(15000,get_corr_snow_Memory,params1),params2,calc_exp3_Snow_Memory,'RData/res-')
}
params1 = list(10,0.3,list(2,3,4,5),list(0.5,0.6,0.7,0.8,0.9,0.93,0.95,0.96,0.97,0.98),list(0.1,0.2,0.3,0.4))
params2 = list(c(0.5,0.6,0.7,0.8,0.9,0.98),0.02*(1:5))
#params1 = list(50,0.3,list(2,3),list(0.7,0.8),list(0.1,0.2,0.3,0.4,0.5))
#params2 = list(0.1*(5:9),0.1*(1:4))
params1 = list(10,0.3,list(2,3),list(0.5,0.6,0.7,0.8,0.9,0.93,0.95,0.96,0.97,0.98),list(0.1,0.2,0.3,0.4))
params2 = list(c(0.5,0.6,0.7,0.8,0.9,0.98),0.02*(1:5))

#calc_data_params_lists_SEQ_WITHFIX(c(5000,get_corr_snow_Memory,params1),params2,calc_exp3_Snow_Memory,'/mnt/G/RISK/ANTON/R/RData/res-')
calc_data_params_lists_WITHFIX(c(30000,get_corr_snow_Memory,params1),params2,calc_exp3_Snow_Memory,'/mnt/G/RISK/ANTON/R/RData/res-')

calc_data_params_lists_WITHFIX(c(100000,get_corr_snow_Memory_Fract,list(20,0.4,
as.list(seq(2,5,by=0.5)),
as.list(seq(0.8,0.98,by=0.03))),
0.1),
list(0.9,seq(0.001,0.1,by=0.003)),calc_exp3_Snow_Memory,'/mnt/G/RISK/ANTON/R/RData/res-')

calc_data_params_lists(c(100000,get_corr_snow_Memory_Fract,list(20,0.4,
as.list(seq(2,5,by=0.1)),
as.list(c(0.8,0.83,0.86,0.89,0.91,0.93,0.95,0.96,0.97,0.98)),
as.list(seq(0.03,0.4,by=0.02)))),calc_exp3_Snow_Memory_GC,'/mnt/G/RISK/ANTON/R/RData/res_GC-')

calc_data_params_lists_WITHFIX(c(80000,get_corr_snow_Memory_Fract,list(10,0.3,list(2.1,2.2,2.3,2.4,2.5,2.6,2.7,2.8,2.9,3,3.1,3.2,3.3,3.4,3.5,3.6,3.7,3.8),0.98,0.1)),list(0.9,1),calc_exp3_Snow_Memory,'/mnt/G/RISK/ANTON/R/RData/res-')
calc_data_params_lists_WITHFIX(c(50000,get_corr_snow_Memory_Fract,list(10,0.3,list(2.1,2.5,2.9),0.98,0.1)),list(0.9,1),calc_exp3_Snow_Memory,'/mnt/G/RISK/ANTON/R/RData/res-')
ts = foreach(s=paste('RData/res-80000-10-0.3-',2+0.1*(1:18),'-0.98-0.1.RData',sep=''))%do%get(load(s))[[1]]$ts

b = get(load('RData/res-50000-10-0.3-2.5-0.98-0.1.RData'))[[1]]$ts
c = get(load('RData/res-50000-10-0.3-3-0.98-0.1.RData'))[[1]]$ts
show_hist_comparison(list(a$ts,b$ts,c$ts),100)

a = get(load('RData/res-1e+05-10-0.3-2.1-0.98-0.1.RData'))[[1]]$ts
b = get(load('RData/res-1e+05-10-0.3-2.5-0.98-0.1.RData'))[[1]]$ts
c = get(load('RData/res-1e+05-10-0.3-2.9-0.98-0.1.RData'))[[1]]$ts
show_hist_comparison(list(a,b,c),100)

rng = range(c(range(a),range(b),range(c))); brr = seq(rng[1],rng[2],len=100)
h1=hist(a,br=brr,plot=FALSE); h2=hist(b,br=brr,plot=FALSE); h3=hist(c,br=brr,plot=FALSE);

plot(ecdf(ts[[1]]),col='red',lwd=1,t='l',xlim=c(-1,0),ylim=c(0,0.6)); 
for(i in 2:length(ts)) lines(ecdf(ts[[i]]),col='darkgreen',lwd=1); 


calc_data_params_lists_WITHFIX(c(5000,get_corr_snow_Memory,params1),params2,calc_exp3_Snow_Memory,'RData/res-')

#a = load_snow_files_DOPAR(15000, get_corr_snow_Memory, params1[[1]], params1[[2]], params1[[3]], params1[[4]], params1[[5]], calc_exp3_Snow_Memory, '/mnt/G/RISK/ANTON/R/RData/3/res-')
#a = load_snow_files_DOPAR(10000, get_corr_snow_Memory, params1[[1]], params1[[2]], params1[[3]], params1[[4]], params1[[5]], calc_exp3_Snow_Memory, 'RData/res-')
#a = load_snow_files_DOPAR(30000, get_corr_snow_Memory, params1[[1]], params1[[2]], params1[[3]], params1[[4]], params1[[5]], calc_exp3_Snow_Memory, '/mnt/G/RISK/ANTON/R/RData/res-')

r = a[[match('0.3-2-0.98-0.1-0.9-0.1',foreach(x=a,.combine=c)%do%paste(x$params,collapse='-'))]]$data
hist(r,br=100)














# n_steps=30000; corr_func=get_corr_snow_Memory; cells_num=200; x1=0.4; x2=2; x3=0.86; x4=0.1; x_FIX=list(0.9,1)
# n_steps=30000; corr_func=get_corr_snow_Memory; cells_num=200; x1=0.4; x2=3; x3=0.96; x4=0.1; x_FIX=list(0.9,1)


# calc_exp3_Snow_Memory = function(n_steps,corr_func,cells_num,x1,x2,x3,x4,x_FIX){
	rp0 = rep(0, cells_num)
	rp1 = rep(1, cells_num)
	#g = graph(edges=c(),n=x1,directed=FALSE)

	#foreach(x_fixs = foreach(i=x_FIX[[1]],.combine=c)%do%{ foreach(j=x_FIX[[2]])%do%c(i,j) })%dopar%{
	

res = foreach(j=1:40,.combine=c)%dopar%{			
		#N = 25; a = 0.3; b = 2; d = 0.9; da = 0.15
		x_fixs = (foreach(i=x_FIX[[1]],.combine=c)%do%{ foreach(j=x_FIX[[2]])%do%c(i,j) })[[1]]
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
		av_ps = array(0,0)
		av_p = 0
		rnN = 20000
		foreach(i=1:n_steps)%do%{
			print(i/n_steps)

			tmp_res = corr_func(g,N,a,b,d,da)
			g = tmp_res$graph
			s0 = tmp_res$paths
			s = cor2cov(exp(log(xx1)*s0), xx2*rp1)
			s_r = tryCatch({ rmnorm(rnN, rp0, s)}, error=function(ex){ s1=make.positive.definite(s, tol=0.001); rmnorm(rnN, rp0, s1) })

			#plot(g);  
			c=clusters(g); 
			g_c = induced.subgraph(g,V(g)[c$membership==which.max(c$csize)]); 
			#vertex.connectivity(g_c)
	
			#v0; V(g)$m; 
			list(sd=sd(rowSums(s_r)/N), av_p=average.path.length(g_c), sz=max(c$csize))
		}
}
# save(res,file='sdd_1200000_20000_res.RData')    load('sdd_1200000_20000_res.RData')
# save(res2,file='sdd_1200000_20000_res2.RData')    load('sdd_1200000_20000_res2.RData')

# save(res,file='sdd_1200000_20000_resMore.RData')    load('sdd_1200000_20000_resMore.RData')
# save(res2,file='sdd_1200000_20000_res2More.RData')    load('sdd_1200000_20000_res2More.RData')

# save(res,file='sdd_800000_10000_res.RData')    load('sdd_800000_10000_res.RData')
# save(res2,file='sdd_800000_10000_res2.RData')    load('sdd_800000_10000_res2.RData')

# save(res2,file='sdd_200000_res2.RData')
# save(sdd,file='sdd_200000_sdd.RData')
# save(res2,file='sdd_800000_res2.RData')   load('sdd_800000_res2.RData')
# save(sdd,file='sdd_800000_sdd.RData')    load('sdd_800000_sdd.RData')

res2 = foreach(r=res,.combine=rbind)%do%data.frame(sd=r$sd,av_p=r$av_p,sz=r$sz) 

res22 = foreach(i=1:3)%dopar%{ foreach(j=1:length(res),.combine=c)%do%{ print(j); res[[j]][[i]] } }
res2 = data.frame(sd=res22[[1]],av_p=res22[[2]],sz=res22[[3]])

ro = res2[!is.nan(res2$av_p),]; ro = ro[order(ro$av_p,ro$sz),]

sdd = foreach(y=unique(ro$sz),.combine=rbind)%dopar%{
	tmp_x = unique(ro$av_p[ro$sz==y])
	data.frame(av_p=tmp_x,sz=array(y,length(tmp_x))
		,sd=foreach(x=tmp_x,.combine=c)%do%sd(ro$sd[ro$sz==y & ro$av_p==x])
		,mean=foreach(x=tmp_x,.combine=c)%do%mean(ro$sd[ro$sz==y & ro$av_p==x])
	)}

sdd = sdd[order(sdd$av_p, sdd$sz),]

hist(sdd$sd/sdd$mean,br=70)

hist(ro$av_p,br=200)
for(i in sort(unique(ro$sz))){
	s=ro$sz==i 
	plot(ro$av_p[s],ro$sd[s],xlim=c(1,10),ylim=0:1)
	Sys.sleep(0.1)
}

plot(ro$av_p,ro$sd)

library(LSD)
#via MASS and base R:

#filled.contour(with(df,MASS:::kde2d(ro$sz,ro$av_p)))
kde2dplot(ro$sz,ro$av_p,ncol=250,nlevels=900,grid=150)

s = sdd[sdd$sz==50,]
plot(s$av_p,s$mean)

s = ro[ro$sz==50,]
plot(s$av_p,



avp_div=0.0001; avpdivs = avp_div*ro$av_p%/%avp_div
asm = foreach(s=sort(unique(ro$sz)),.combine=cbind)%dopar%{ 
		print(s)
		#r_tmp = foreach(a=sort(unique(ro$av_p)),.combine=c)%do%mean(ro$sd[ro$sz==s & ro$av_p==a])
		r_tmp = foreach(a=sort(unique(avpdivs)),.combine=c)%do%mean(ro$sd[ro$sz==s & avpdivs==a])
		#r_tmp[is.nan(r_tmp)] = NA
		r_tmp[is.nan(r_tmp)] = 0
		data.frame(a=r_tmp)
	}
colnames(asm)=sort(unique(ro$sz))
library(plot3D)
persp3D(x=sort(unique(avpdivs)),y=sort(unique(ro$sz)),z=as.matrix(asm))
ribbon3D(x=sort(unique(avpdivs)),y=sort(unique(ro$sz)),z=as.matrix(asm),theta=1)

library(scatterplot3d)
#library(Rcmdr)

x=sdd$av_p; y=sdd$mean; z=sdd$sz; 
for(i in sort(unique(z))){
	z0 = i
	scatterplot3d(x=x[z==z0],y=y[z==z0],z=z[z==z0],xlim=range(x),ylim=range(y),zlim=range(z),grid=TRUE,type='h')
	Sys.sleep(0.4)
}


avp_div=0.0000001; avpdivs = avp_div*ro$av_p%/%avp_div
asm = foreach(s=sort(unique(ro$sz)),.combine=cbind)%dopar%{ 
		print(s)
		#r_tmp = foreach(a=sort(unique(ro$av_p)),.combine=c)%do%mean(ro$sd[ro$sz==s & ro$av_p==a])
		r_tmp = foreach(a=sort(unique(avpdivs)),.combine=c)%do%mean(ro$sd[ro$sz==s & avpdivs==a])
		#r_tmp[is.nan(r_tmp)] = NA
		r_tmp[is.nan(r_tmp)] = 0
		data.frame(a=r_tmp)
	}

asm2 = asm
for(i in 1:dim(asm2)[1]){
	if(i%%100==0) print(i/dim(asm2)[1])
	prev_val = asm2[i,1] 
	for(j in 1:dim(asm2)[2]) if(asm2[i,j]==0)asm2[i,j]=prev_val else prev_val=asm2[i,j] 
}
for(j in 1:dim(asm2)[2]){
	print(i/dim(asm2)[2])
	prev_val = asm2[1,j] 
	for(i in 1:dim(asm2)[1]) if(asm2[i,j]==0)asm2[i,j]=prev_val else prev_val=asm2[i,j] 
}
asm2[100,]

colnames(asm2)=sort(unique(ro$sz))
library(plot3D)
persp3D(x=sort(unique(avpdivs)),y=sort(unique(ro$sz)),z=as.matrix(asm2),theta=130,phi=-20)
ribbon3D(x=sort(unique(avpdivs)),y=sort(unique(ro$sz)),z=as.matrix(asm),theta=50,alpha=0.9).



sdiv = sdiv[order(sdiv$av_p, sdiv$sz),]


x=sdiv$av_p; y=sdiv$mean; z=sdiv$sz; 
scatterplot3d(x=x,y=y,z=z,xlim=range(x),ylim=range(y),zlim=range(z),grid=TRUE,type='p')

mesh(1:3,1:3)








X <- seq(0, pi, length.out = 50)
Y <- seq(0, 2*pi, length.out = 50)
M <- mesh(X, Y)
phi <- M$x
theta <- M$y
# x, y and z grids
r <- sin(4*phi)^3 + cos(2*phi)^3 + sin(6*theta)^2 + cos(6*theta)^4
x <- r * sin(phi) * cos(theta)
y <- r * cos(phi)
z <- r * sin(phi) * sin(theta)
# full colored image
surf3D(x, y, z, colvar = y, colkey = TRUE, shade = 0.1, box = FALSE, theta = 10)





#via ggplot (geom_density2d() calls kde2d())
library(ggplot2)
ggplot(ro[,2:3],aes(x=ro$sz,y=ro$av_p))+geom_density2d()+theme_bw()

		res = foreach(i=1:n_steps, .combine=c)%do%{
			#tmp_res = get_corr_snow_Memory(g,N,a,b,d,da)
			tmp_res = corr_func(g,N,a,b,d,da)
			g = tmp_res$graph
			s0 = tmp_res$paths
			s = cor2cov(exp(log(xx1)*s0), xx2*rp1)
			s_r = tryCatch({ rmnorm(100, rp0, s)}, error=function(ex){ s1=make.positive.definite(s, tol=0.001); rmnorm(100, rp0, s1) })

			#plot(g);  c=clusters(g); g_c = induced.subgraph(g,V(g)[c$membership==which.max(c$csize)]); vertex.connectivity(g_c)

			v0; V(g)$m; 
			v0=V(g)$m

			sum(s_r)/N
		}



		list(ts=res, params=c(x1,x2,x3,x4,xx1,xx2))
	}
}




















length(a)
mins = foreach(r=a,.combine=c)%do%min(r$data)
sds = foreach(r=a,.combine=c)%do%sd(r$data)
hist(a[mins<=-0.99][[1]]$data,br=200)

hist(a[sds<=0.015][[1]]$data,br=100)
min(sds)


foreach(a1=a)%do%{
	h=hist(a1$data,br=80,plot=FALSE)
	write.csv(t(data.frame(h$mids,h$density)),row.names=FALSE,file=paste('/mnt/G/RISK/ANTON/R/RData/',paste(a1$params,collapse='-'),'.csv',sep=''))
}


N = 10000; indics=matrix(0,N,4); params_ACT=matrix(0,N,4); vals_ACT=array(0,N); 
for(i in 1:N) indics[i,] = sample(10,4)
for(i in 1:N) params_ACT[i,] = indic_to_param(indics[i,],c(2,0.5,3,0.3))
r = sample(0:1,N,replace=TRUE); for(i in 1:N) vals_ACT[i] = param_to_val_Simulate(params_ACT[i,],r[i])

# qb is calculated for the SAME params, just to test
#test_coeff



#my_bayes_ThisCase(vals_ACT,c(2,0.5,3,0.3))
my_bayes_ThisCase_Shift = function(cf_shift) { my_bayes_ThisCase(vals_ACT,c(2,0.5,3,0.3)*cf_shift) }
res = c(
my_bayes_ThisCase_Shift(c(0.8,0.8,1,1)),
my_bayes_ThisCase_Shift(c(1,0.8,1,1)),
my_bayes_ThisCase_Shift(c(1.2,0.8,1,1)),
my_bayes_ThisCase_Shift(c(0.8,1,1,1)),
my_bayes_ThisCase_Shift(c(1,1,1,1)),
my_bayes_ThisCase_Shift(c(1.2,1,1,1)),
my_bayes_ThisCase_Shift(c(0.8,1.2,1,1)),
my_bayes_ThisCase_Shift(c(1,1.2,1,1)),
my_bayes_ThisCase_Shift(c(1.2,1.2,1,1))
)

t(matrix(res,3,3))









# cf_tmp=0.1*c(1.5,3.5,1.1,2.5,2.5,1.1,1.5,2.5,3.5,1.5,2.5,3.5,1.1,1.1,1.5,3.5)
err_func = function(cf_tmp){
	len = N
	params = matrix(0,len,4); qb = matrix(0,len,Q_CNT); p=1:4
	for(i in 1:len) {
		p = indic_to_param2(indics[i,],cf_tmp)
		params[i,] = ifelse(p<BNDS[[1]],BNDS[[1]],ifelse(p>BNDS[[2]],BNDS[[2]],p))
	}
	tmp_qb = foreach(i=1:len)%dopar%{ 
		if(i%%5==0)print(i); 
		#param_q_Simulate(params[i,],8) 
		s = paramed_pdf_interpol(dats,params[i,])
		c(-Inf,as.numeric(quantile(s,seq(0,1,len=Q_CNT)))[2:(Q_CNT-1)],Inf)
	}; 
	for(i in 1:len) qb[i,] = tmp_qb[[i]]

	my_bayes(qb,vals_ACT)#a0#a1#a00#a01
}

err_func(0.1*c(1.5,2.5,3.5,1.1,1.5,2.5,3.5,1.1,1.5,2.5,3.5,1.1,1.5,2.5,3.5,1.1))

a=optim(0.1*c(1.5,2.5,3.5,1.1,1.5,2.5,3.5,1.1,1.5,2.5,3.5,1.1,1.5,2.5,3.5,1.1), err_func)


save(a,file='/mnt/G/RISK/ANTON/R/RData/a.RData')


source('R12.R')
res1 = foreach(iii=1:15)%do%try_bayes(10000,500)
res2 = foreach(iii=1:15)%do%try_bayes(10000,1000)
res3 = foreach(iii=1:15)%do%try_bayes(50000,500)
res4 = foreach(iii=1:15)%do%try_bayes(50000,1000)
#res_100000 = foreach(iii=1:30)%do%try_bayes(100000)

b=list(r1=res_10000, r2=res_100000)
save(b,file='/mnt/G/RISK/ANTON/R/RData/a2.RData')

#b1=list(r1=unlist(res1), r2=unlist(res2), r3=unlist(res3), r4=unlist(res4))
#save(b1,file='/mnt/G/RISK/ANTON/R/RData/a1.RData')
b2p=list(r1=unlist(res1), r2=unlist(res2), r3=unlist(res3))
save(b2p,file='/mnt/G/RISK/ANTON/R/RData/a2p.RData')
b3p=list(r4=unlist(res4))
save(b3p,file='/mnt/G/RISK/ANTON/R/RData/a3p.RData')

save(b23,file='/mnt/G/RISK/ANTON/R/RData/a23.RData')
b23p=c(get(load('/mnt/G/RISK/ANTON/R/RData/a2p.RData')),get(load('/mnt/G/RISK/ANTON/R/RData/a3p.RData')))
save(b23p,file='/mnt/G/RISK/ANTON/R/RData/a23p.RData')

b=get(load('/mnt/G/RISK/ANTON/R/RData/a1.RData'))









try_bayes0 = function(dats_len,ts_len,cf_tmp){

	#dats_len = 10000; ts_len = 1000

	options(stringsAsFactors = FALSE)
	indic_to_param = function(ind,cf) { ind*cf }
	param_to_val_Simulate = function(p) { runif(1)*p }
	param_sample_Simulate = function(p,len){ runif(len)*p }

	dats = foreach(i1=1:8)%do%{ p = i1; list(data=param_sample_Simulate(p,dats_len), params=p) }
	#dats = foreach(i1=0.5*(1:16))%do%{ p = i1; list(data=param_sample_Simulate(p,dats_len), params=p) }

	cf_ACT = 1.31415926
	N = ts_len; indics=matrix(0,N,1); params_ACT=matrix(0,N,1); vals_ACT=array(0,N); 
	for(i in 1:N) indics[i,] = sample(4,1)
	for(i in 1:N) vals_ACT[i] = param_to_val_Simulate(indic_to_param(indics[i,],cf_ACT))
	Q_CNT = 8; BNDS = PPI_get_params_bounds(dats)


	#cf_tmp=1.6
	len = N
	params = matrix(0,len,1); qb = matrix(0,len,Q_CNT); p=1
	for(i in 1:len) {
		p = indic_to_param(indics[i,],cf_tmp)
		params[i,] = ifelse(p<BNDS[[1]],BNDS[[1]],ifelse(p>BNDS[[2]],BNDS[[2]],p))
	}
	tmp_qb = foreach(i=1:len)%dopar%{ 
		if(i%%5==0)print(paste('dats:',dats_len,'N:',ts_len,'len step:',i)); 
		s = paramed_pdf_interpol(dats,params[i,])
		c(-Inf,as.numeric(quantile(s,seq(0,1,len=Q_CNT)))[2:(Q_CNT-1)],Inf)
	}; 
	for(i in 1:len) qb[i,] = tmp_qb[[i]]

	my_bayes(qb,vals_ACT)
}


r1 = foreach(iii=1:15,.combine=c)%do%try_bayes0(10000,1000,1.7)
r2 = foreach(iii=1:15,.combine=c)%do%try_bayes0(10000,1000,1.55)
r3 = foreach(iii=1:15,.combine=c)%do%try_bayes0(10000,1000,1.4)
r4 = foreach(iii=1:15,.combine=c)%do%try_bayes0(10000,1000,1.31415926)
r5 = foreach(iii=1:15,.combine=c)%do%try_bayes0(20000,1000,1.31415926)
r6 = foreach(iii=1:15,.combine=c)%do%try_bayes0(10000,2000,1.31415926)
r7 = foreach(iii=1:15,.combine=c)%do%try_bayes0(20000,2000,1.31415926)
r8 = foreach(iii=1:15,.combine=c)%do%try_bayes0(20000,3000,1.31415926)
r9 = foreach(iii=1:15,.combine=c)%do%try_bayes0(20000,4000,1.31415926)
r10 = foreach(iii=1:15,.combine=c)%do%try_bayes0(20000,5000,1.31415926)
r11 = foreach(iii=1:15,.combine=c)%do%try_bayes0(20000,6000,1.31415926)
r12 = foreach(iii=1:15,.combine=c)%do%try_bayes0(20000,12000,1.31415926)
r13 = foreach(iii=1:15,.combine=c)%do%try_bayes0(20000,6000,1.4)

r_univar=foreach(r=list(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13),.combine=c)%do%paste('sd:',sd(r),'E',mean(r))




source('/mnt/G/RISK/ANTON/R/R12.R')
options(stringsAsFactors = FALSE)

#try_bayes1 = function(dats_len,ts_len,cf_tmp){
try_bayes1 = function(cf_tmp){

	#dats_len = 100000; ts_len = 2000
	print(cf_tmp)

	indic_to_param = function(ind,cf) { ind*cf }
	param_to_val_Simulate = function(p) { rnorm(1,p[1],p[2]) + rnorm(1,p[3],p[4]) }
	#param_sample_Simulate = function(p,len){ rnorm(len,p[1],p[2]) }

	#dats = foreach(i1=1:8)%do%{ p = i1; list(data=param_sample_Simulate(p,dats_len), params=p) }
	#dats = foreach(i1=0.5*(1:16))%do%{ p = i1; list(data=param_sample_Simulate(p,dats_len), params=p) }

	cf_ACT = c(1.257,1.957,1.357,1.175); CF_LEN = length(cf_ACT)

	N = ts_len; indics=matrix(0,N,CF_LEN); params_ACT=matrix(0,N,CF_LEN); vals_ACT=array(0,N); 
	for(i in 1:N) indics[i,] = 0.25*sample(4:16,CF_LEN)
	for(i in 1:N) vals_ACT[i] = param_to_val_Simulate(indic_to_param(indics[i,],cf_ACT))
	Q_CNT = 8; BNDS = PPI_get_params_bounds(dats)

	#cf_tmp=1.6
	len = N
	params = matrix(0,len,CF_LEN); qb = matrix(0,len,Q_CNT); p=1:CF_LEN
	for(i in 1:len) {
		p = indic_to_param(indics[i,],cf_tmp)
		params[i,] = ifelse(p<BNDS[[1]],BNDS[[1]],ifelse(p>BNDS[[2]],BNDS[[2]],p))
	}
	tmp_qb = foreach(i=1:len)%dopar%{ 
		if(i%%100==0)print(paste('dats:',dats_len,'N:',ts_len,'len step:',i)); 
		s = paramed_pdf_interpol(dats,params[i,])
		c(-Inf,as.numeric(quantile(s,seq(0,1,len=Q_CNT)))[2:(Q_CNT-1)],Inf)
	}; 
	for(i in 1:len) qb[i,] = tmp_qb[[i]]

	m = my_bayes(qb,vals_ACT)
	print(m)
	m
}


dats_len <<- 20000; ts_len <<- 2000
param_sample_Simulate <<- function(p,len){ rnorm(len,p[1],p[2]) + rnorm(len,p[3],p[4]) }
dats <<- foreach(i1=1:8,.combine=c)%do%{ foreach(i2=1:8,.combine=c)%do%{ foreach(i3=1:8,.combine=c)%do%{ foreach(i4=1:8)%do%{ p = c(i1,i2,i3,i4); list(data=param_sample_Simulate(p,dats_len), params=p) } } } }

#try_bayes1(c(2,2))
o = optim(c(1.1,1.1,1.1,1.1), try_bayes1)







cf_tmp=(1:6)*runif(6); print(cf_tmp)
cf_tmp=0.01*c(-0.3884127,1.0017027,3.0186372,-0.4102008,1.1826995,5.6524539)
pms_tmp = foreach(i=1:TS_LEN,.combine=rbind)%do%indic_to_param(INDICES[i,],cf_tmp)
foreach(i=1:P_CNT,.combine=c)%do%{ r=range(pms_tmp[,i]);}
try_bayes1(0.01*c(-0.5,1,3,-0.4,2,5))


source('/mnt/G/RISK/ANTON/R/R12.R')
options(stringsAsFactors = FALSE)

#try_bayes1 = function(dats_len,ts_len,cf_tmp){



# indic_to_param ?
param_sample_Simulate <<- function(p,len){ rnorm(len,p[1],p[2]) + rnorm(len,p[3],p[4]) }
param_to_val_Simulate <<- function(p) { rnorm(1,p[1],p[2]) + rnorm(1,p[3],p[4]) }
	
MY_COUNT <<- 10000; TS_LEN <<- 1000; CF_ACT = c(1.257,1.957,1.357,1.175); CF_LEN = length(CF_ACT); Q_CNT = 8; 
MY_DISTRIBS <<- foreach(p1=1:9,.combine=c)%dopar%{ foreach(p2=1:9,.combine=c)%do%{ foreach(p3=1:9,.combine=c)%do%{ foreach(p4=1:9)%do%{ p = c(p1,p2,p3,p4); list(data=param_sample_Simulate(p,MY_COUNT), params=p) } } } }
MY_PARAMS <<- foreach(x=MY_DISTRIBS,.combine=rbind)%do%x$params
MY_PARAMS_BND <<- list(c(1,7),c(1,8),c(1,9),c(1,8)) #MY_PARAMS_BND <<- foreach(i=1:CF_LEN)%do%range(MY_DIST_PARAMS[,i])

if(1==0){
	#indic_to_param = function(ind,cf) { c(sum(ind*cf[1:2]),sum(ind*cf(3:4))) }
	indic_to_param = function(ind,cf) { sum(ind*cf[1:2]) }
	param_sample_Simulate <<- function(p,len){ rnorm(len,0,p[1]) }
	param_to_val_Simulate <<- function(p) { rnorm(1,0,p[1]) }
		
	MY_COUNT <<- 10000; TS_LEN <<- 5000; CF_ACT = c(1.257,1.957); CF_LEN = length(CF_ACT); Q_CNT = 8; 
	MY_DISTRIBS <<- foreach(p1=1:9,.combine=c)%dopar%{ foreach(p2=1:9)%do%{ p = c(p1,p2); list(data=param_sample_Simulate(p,MY_COUNT), params=p) } }
	MY_PARAMS <<- foreach(x=MY_DISTRIBS,.combine=rbind)%do%x$params
	MY_PARAMS_BND <<- list(c(1,7),c(1,8)) #MY_PARAMS_BND <<- foreach(i=1:CF_LEN)%do%range(MY_DIST_PARAMS[,i])
}

INDICES <<- matrix(0,TS_LEN,CF_LEN)   # fundamental market indices
for(i in 1:TS_LEN) 
	INDICES[i,] = 0.25*sample(4:16,CF_LEN)
VALS_ACT <<- array(0,TS_LEN)    # stock price
for(i in 1:TS_LEN) 
	VALS_ACT[i] = param_to_val_Simulate(indic_to_param(INDICES[i,],CF_ACT))
INDICES_BND <<- foreach(i=1:dim(INDICES)[2])%do%range(INDICES[,i])


#try_bayes1(c(2,2))
#o = optim(c(1.1,1.1,1.1,1.1), try_bayes1, method='SANN', control = list(maxit = 100))
#o = optim(c(1.1,1.1), try_bayes1, method='SANN', control = list(maxit = 1000))
#o = optim(c(1.1,1.1), try_bayes1, control = list(reltol=0.00000001))

cf_steps = 25
calc_pts = as.matrix(expand.grid(foreach(i=1:CF_LEN)%do%seq(1,2,len=cf_steps)))
calcs = foreach(i=1:dim(calc_pts)[1],.combine=c)%dopar%{ print(i); try_bayes1(as.numeric(calc_pts[i,])); }
h=hash(st=cf_steps, pts=calc_pts, calcs=calcs)
save(h,file='my_distribs_calcs.RData')

load('my_distribs_calcs.RData')

calc_pts[which.min(rowSums(t(t(calc_pts)-CF_ACT)^2)),]
calcs[which.min(rowSums(t(t(calc_pts)-CF_ACT)^2))]
calc_pts[which.min(calcs),]
calcs[which.min(calcs)]


library(LSD)
kde2dplot(calc_pts[,1],calc_pts[,2],ncol=250,nlevels=900,grid=150)

library(plot3D)
m = matrix(calcs,cf_steps,cf_steps)
persp3D(x=seq(1,2,len=cf_steps),y=seq(1,2,len=cf_steps),z=m,phi=15,theta=60,border=1,axes=TRUE,ticktype='detailed')
persp3D(x=seq(1,2,len=cf_steps),y=seq(1,2,len=cf_steps),z=m,phi=270,theta=360,border=1,axes=TRUE,ticktype='detailed')
m[which(seq(1,2,len=cf_steps)==calc_pts[131,1]),which(seq(1,2,len=cf_steps)==calc_pts[131,2])]











library(quantmod)
source('R12.R')
getSymbols(c('DGS10','SP500','USD3MTD156N'),src='FRED')
snp = diff(log(get('SP500'))); trs = get('DGS10'); lbr = get('USD3MTD156N')
ids = merge.xts(100*snp,trs,lbr); ids = na.locf(ids)[index(ids)>'2004-08-13',]

L=100000; s=2; lapl = function(n,r) { re=rexp(n,r); ifelse(runif(n)>0.5,re,-re) }
mix_gen = function(l,p,s){ ifelse(runif(l)>p,lapl(l,1/s),rnorm(l,0,s)) }

#if(1==0){
	#indic_to_param = function(ind,cf) { c(sum(ind*cf[1:3]),sum(ind*cf[4:6])) }
	indic_to_param = function(ind,cf) { c(sum(ind[2:3]*cf[1:2]),sum(ind*cf[3:5])) }
	param_sample_Simulate <<- function(p,len){ mix_gen(len,p[1],p[2]) }
	param_to_val_Simulate <<- function(p) { mix_gen(1,p[1],p[2]) }
		
	#MY_COUNT <<- 20000; TS_LEN <<- 2000; CF_ACT = array(0,6); CF_LEN = length(CF_ACT); P_CNT = 2; I_CNT = 3; Q_CNT = 8; 
	MY_COUNT <<- 20000; TS_LEN <<- 2000; CF_ACT = array(0,5); CF_LEN = length(CF_ACT); P_CNT = 2; I_CNT = 3; Q_CNT = 8; 
	MY_DISTRIBS <<- foreach(p1=seq(0,1,len=10),.combine=c)%dopar%{ foreach(p2=seq(0.001,10,len=20))%do%{ p = c(p1,p2); list(data=param_sample_Simulate(p,MY_COUNT), params=p) } }
	MY_PARAMS <<- foreach(x=MY_DISTRIBS,.combine=rbind)%do%x$params
	MY_PARAMS_BND <<- foreach(i=1:P_CNT)%do%range(MY_PARAMS[,i])
#}

INDICES <<- matrix(0,TS_LEN,I_CNT)   # fundamental market indices
for(i in 1:TS_LEN) 
	INDICES[i,] = ids[i,]
VALS_ACT <<- array(0,TS_LEN)    # stock price
for(i in 1:TS_LEN) 
	VALS_ACT[i] = ids[i,'SP500']
INDICES_BND <<- foreach(i=1:dim(INDICES)[2])%do%range(INDICES[,i])


o = optim(0.01*c(-0.5,1,3,-0.4,2,5), try_bayes1)
#o = optim(0.01*c(-0.5,1,3,-0.4,2,5), try_bayes1, method='SANN', control = list(maxit = 1000000))
save(o,file='~/o_simple.RData')
save(o,file='~/o_SANN.RData')
save(o,file='~/o_SANN2.RData')



try_bayes1(c(0.03749185,0.00457618,-0.05092291,1.7650444,0.10776348))
try_bayes1(c(-0.00001,0.0001,0.00001,-0.05092291,1.7650444,0.10776348))
try_bayes1(c(-0.0260524,0.05749185,0.02457618,-0.05092291,1.7650444,0.10776348))

#[1] -0.04005240  0.17799185 -0.02542382  0.01007709  0.16299172  0.36476348
#[1] "bayes"
#[1] 0.06479565         newer date 0.06625061

#[1] -0.00105240  0.03799185  0.00357618 -0.04692291  0.13799172  0.11376348
#[1] "bayes"
#[1] 0.01908908 -- *8 = 0.15271262     

#[1] -0.00605240  0.03749185  0.00457618 -0.05092291  0.14399172  0.10776348
#[1] "bayes"
#[1] 0.01889161 -- *8 = 0.1511329     

p0 = c(-0.04005240,0.17799185,-0.02542382,0.01007709,0.16299172,0.36476348)
steps0 = expand.grid(foreach(i=1:6)%do%c(-1,0,1))
#steps0 = expand.grid(foreach(i=1:6)%do%sample(c(-1,0,1),2))

steps = t(p0+0.01*t(steps0))
res = foreach(i=1:dim(steps)[1],.combine=c)%dopar%{ print(i); try_bayes1(as.numeric(steps[i,])) }
nsteps = steps[which(res<0.06479565),]
res = res[which(res<0.06479565)]

j = which.min(res)
steps2 = p0+1*(nsteps[j,]-p0)
try_bayes1(as.numeric(nsteps[j,]))




#save(ptt,file='/mnt/G/RISK/ANTON/R/ptt.RData')
#ptt=get(load('/mnt/G/RISK/ANTON/R/ptt.RData'))    

#--# need to test (using 5 fixed random direction cuts) how does the surface change between snp[1:1300] and snp[1300:2600]

s0 = c(-0.00605240,0.03749185,0.00457618,-0.05092291,0.14399172,0.10776348)  # try_bayes1(s0)
b0 = 0.01889161

s0 = as.numeric(array(0,5)); b0 = try_bayes1(s0)
ptt_0 = foreach(x=seq(0.2,2,len=20))%do%find_steps_steps0(b0, s0, x)  #ptt_0_old=ptt_0
ptt_0 = foreach(x=seq(0.01,0.3,len=20))%do%find_steps_steps0(b0, s0, x)  #ptt_0_old=ptt_0
ptt_sec = foreach(i=1:8)%do%find_steps_vector(b0,s0,0.005,sample(-1:1,6,replace=TRUE),20)

# res = ptt_05$res_full; res_st = ptt_05$steps_full
res = ptt_0[[1]]$res_full; res_st = ptt_0[[1]]$steps_full
if(length(ptt_0)>1) for(i in 2:length(ptt_0)) { res = c(res, ptt_0[[i]]$res_full); res_st = rbind(res_st, ptt_0[[i]]$steps_full) }
res_st = res_st[res!=1000000,]; res = res[res!=1000000]
res_st = res_st[floor(res*10^7)!=floor(b0*10^7),]; res = res[floor(res*10^7)!=floor(b0*10^7)]
# res_st = res_st[res<0.1,]; res = res[res<0.1]


ptt_mins = foreach(i=1:length(res))%do%{
	tmp_pt = list(res=res[i], step=res_st[i,])
	run_my_opt(tmp_pt,tmp_pt); 
}
ptt_steps = foreach(x=ptt_mins,.combine=rbind)%do%as.numeric(c(x$x$res,x$x$res*8,x$x$step)); rownames(ptt_steps)=c()
plot(sort(ptt_steps[,2])[-67])
save(ptt_mins,file='/mnt/G/RISK/ANTON/R/ptt_steps.RData') # load('/mnt/G/RISK/ANTON/R/ptt_steps.RData')
b0 = min(ptt_steps[,1]); s0 = ptt_steps[which.min(ptt_steps[,1]),3:7]


tmp_pt = list(res=res[which.min(res)], step=res_st[which.min(res),])
tmp_pt1 = run_my_opt(tmp_pt,tmp_pt); 



TS_LEN=600
INDICES <<- matrix(0,TS_LEN,I_CNT)   # fundamental market indices
for(i in 1:TS_LEN) 
	INDICES[i,] = ids[2000+i,]
VALS_ACT <<- array(0,TS_LEN)    # stock price
for(i in 1:TS_LEN) 
	VALS_ACT[i] = ids[2000+i,'SP500']
INDICES_BND <<- foreach(i=1:dim(INDICES)[2])%do%range(INDICES[,i])

try_bayes1(pt$step)
try_bayes1(p0)
try_bayes1((p0+as.numeric(pt$step))/2)






r=ids[,1]; L=2600
qb=matrix(0,L-30,8)
tmp_qb = foreach(i=31:L)%dopar%{ 
	s = rnorm(10000,0,sd(r[(i-30):(i-1)]))
	c(-Inf,as.numeric(quantile(s,seq(0,1,len=8)))[2:(Q_CNT-1)],Inf)
}; 
for(i in 1:(L-30)) qb[i,] = tmp_qb[[i]]
my_bayes_Cells(qb,as.numeric(r[31:L]))*8
my_bayes(qb,as.numeric(r[31:L]))*8



#res = ptt_sec$res_full; res_st = ptt_sec$steps_full
res = ptt_sec[[1]]$res_full; res_st = ptt_sec[[1]]$steps_full
for(i in 2:length(ptt_sec)) { res = c(res, ptt_sec[[i]]$res_full); res_st = rbind(res_st, ptt_sec[[i]]$steps_full) }
res_st = res_st[res!=1000000,]; res = res[res!=1000000]
ys = rowSums(t(t(res_st)-s0)^2)^0.5
plot(ys[order(ys)],res[order(ys)])







[1] "0.0173606698356618, -0.001, 0.3, 0.299"                        
[2] "0.0173606698356618, 0.0533684210526316, 0.3, 0.353368421052632"
[3] "0.0173606698356618, -0.08, 0.3, -0.08"                         
[4] "0.0173606698356618, 0.197368421052632, 0.3, 0.497368421052632" 
[5] "0.0173606698356618, 0.0513684210526316, 0.3, 0.351368421052632"


try_bayes1(c(0.299,0.353368421052632,-0.08,0.497368421052632,0.351368421052632))

getSymbols("SP500",src='FRED')
rs = get("SP500"); rs = rs[!is.na(rs) & index(rs)>'1990-01-01']; dr = diff(log(rs)); dr = dr[!is.na(dr)]



























r = calc_exp5(1000000,get_corr_3,proport_price_func,50,2,0.9,list(0.9,0.15))$ts[[1]][[1]]
r = calc_exp5(50000,get_corr_3,function(prc){ sum(prc)/sqrt(length(prc)) },50,2,0.9,list(0.9,0.15))$ts[[1]][[1]]
save(r,file='/mnt/G/RISK/ANTON/R/r_test1.RData')

load(file='/mnt/G/RISK/ANTON/R/r_test1.RData')

hist(r,br=200)
hist(r[r!=0],br=1000)
hist(r[abs(r)<0.5],br=1000)
x0=sort(r[r< -0.1])
x = -x0[x0<0]; y=(((1:length(x0))/length(x0))[x0<0]); plot(log(x),log(y))

r0=r

r=p[abs(p)<200]
#p1 <- hist(0.1*r[abs(r)<0.5],br=100,plot=FALSE)                     # centered at 4
p1 <- hist(0.058*r,br=80,plot=FALSE)                     # centered at 4
p2 <- hist(diff(log(rs)),br=80,plot=FALSE)                     # centered at 6
plot(p1$mids[p1$mids>0.01],p1$density[p1$mids>0.01],col='red', ylim=c(0,20),xlim=c(0.01,0.1),t='l',lwd=2)
lines(p2$mids[p2$mids>0.01],p2$density[p2$mids>0.01],col='blue', ylim=c(0,20),xlim=c(0.01,0.1),t='l',lwd=2)



N=50; rp0 = rep(0, N); covmat=cor2cov(matrix(rep(c(1,rep(0.9,N)),N+1)[1:(N*N)],N,N), rep(0.01,N)); 
p1 = unlist(foreach(i=1:500)%dopar%{ foreach(j=1:500)%do%proport_price_func(rmnorm(1, rp0, covmat))})

N=50; sc = 1/sqrt(N); rp0 = rep(0, N); covmat=cor2cov(matrix(rep(c(1,rep(0.0,N)),N+1)[1:(N*N)],N,N), rep(1,N)); 
p1 = unlist(foreach(i=1:500)%dopar%{ foreach(j=1:500)%do%{ sum(rmnorm(1, rp0, covmat))*sc } })
N=50; sc = 1/sqrt(N); rp0 = rep(0, N); covmat=cor2cov(matrix(rep(c(1,rep(0.9999999,N)),N+1)[1:(N*N)],N,N), rep(1,N)); 
p2 = unlist(foreach(i=1:500)%dopar%{ foreach(j=1:500)%do%{ sum(rmnorm(1, rp0, covmat))*sc } })
#save(p,file='/mnt/G/RISK/ANTON/R/p_test1.RData')
#load(file='/mnt/G/RISK/ANTON/R/p_test1.RData')
#p_hist = p[abs(p)<200]
#hist(p_hist,br=200,xlim=c(-0.01,0.01))
hist(p2,br=100)

ps2N = p
x0=sort(ps2N[ps2N< -0.001])
x = -x0[x0<0]; y=(((1:length(x0))/length(x0))[x0<0]); plot(log(x),log(y))

N=50; rp0 = rep(0, N); covmat=cor2cov(matrix(rep(c(1,rep(0.9,N)),N+1)[1:(N*N)],N,N), rep(0.3,N)); 
p1 = unlist(foreach(i=1:500)%dopar%{ foreach(j=1:500)%do%proport_price_func(rmnorm(1, rp0, covmat))})

N=50; rp0 = rep(0, N); covmat=cor2cov(matrix(rep(c(1,rep(0.0,N)),N+1)[1:(N*N)],N,N), rep(0.3,N)); 
p2 = unlist(foreach(i=1:500)%dopar%{ foreach(j=1:500)%do%{ rmnorm(1, rp0, covmat)*sc } })

ph1 <- hist(p1[abs(p1)<200],br=200,plot=FALSE) 
ph2 <- hist((p2/(1-abs(p2)))[abs(p1)>0.005],br=200,plot=FALSE)   
plot(ph1$mids,ph1$density,col='red', xlim=c(-20.5,20.5),t='l',lwd=2)
lines(ph2$mids,ph2$density,col='blue', xlim=c(-20.5,20.5),t='l',lwd=2)


hist(unlist(lapply(1:100000,function(x){mean(rnorm(15*15))})),br=100)

xs3_FIX = c(0.3,0.5,0.7,0.9,0.99); xs4_FIX = c(0.005,0.06,0.007,0.08,0.09,0.01,0.011,0.012,0.015,0.017,0.02,0.022,0.025)
calc_data_params_lists_SEQ(list(100,get_corr_3,list(100,200,300,400,500),list(1,2,3,4,5,6,7,8,9),list(0.6,0.7,0.8,0.9,0.93,0.96,0.99)),calc_exp3,'/mnt/G/RISK/ANTON/R/perc_corr/res-')



xs3_FIX = c(0.7); xs4_FIX = c(0.01)
calc_data_params_lists_SEQ(list(1000,get_corr_3,100,6,0.8),calc_exp3,'/mnt/G/RISK/ANTON/R/perc_corr/res-')
calc_data_params_lists_SEQ(list(3000,get_corr_3,100,6,0.8),calc_exp3,'/mnt/G/RISK/ANTON/R/perc_corr/res-')
calc_data_params_lists_SEQ(list(2000,get_corr_3,100,2,0.9),calc_exp3,'/mnt/G/RISK/ANTON/R/perc_corr/res-')

#calc_data_params_lists_SEQ(list(1000,get_corr_3,list(100,400,700,1000,1300),list(1,4,7,10),list(0.9,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98),list(0.9,0.93,0.96,0.99)),calc_exp2,'/mnt/G/RISK/ANTON/R/perc_corr/res-')


calc_data_params_lists_SEQ_WITHFIX(list(100,get_corr_3,200,2,0.99),list(c(0.1,0.2),c(0.1,0.2)),calc_exp3,'/mnt/G/RISK/ANTON/R/perc_corr/res-')
f2 = load_perc_corr_grid_files_DOPAR(100,get_corr_3,200,2,0.99,calc_exp3,'/mnt/G/RISK/ANTON/R/perc_corr/res-')



f1 = load_perc_corr_grid_files_DOPAR(1000,get_corr_3,100,6,0.8,0.7,0.01,calc_exp3,'/mnt/G/RISK/ANTON/R/perc_corr/res-')
f2 = load_perc_corr_grid_files_DOPAR(3000,get_corr_3,100,6,0.8,0.7,0.01,calc_exp3,'/mnt/G/RISK/ANTON/R/perc_corr/res-')
f3 = load_perc_corr_grid_files_DOPAR(2000,get_corr_3,100,2,0.9,0.7,0.01,calc_exp3,'/mnt/G/RISK/ANTON/R/perc_corr/res-')


fls_res1 = fls_res
fls_res2 = fls_res
fls_res = load_perc_corr_grid_files_DOPAR(100,get_corr_3,list(100,200,300),list(1,2,3,4,5,6,7,8,9),list(0.6,0.7,0.8,0.9,0.93,0.96,0.99),c(0.3,0.5,0.7,0.9,0.99),c(0.005,0.06,0.007,0.08,0.09,0.01,0.011,0.012,0.015,0.017,0.02,0.022,0.025),calc_exp3,'/mnt/G/RISK/ANTON/R/perc_corr/res-')


yho=as.xts(get.hist.quote(i='^gspc',quote='Close'))[1000:5000]; W=30; p=diff(log(as.numeric(yho[1:W])))

e=ecdf(p); #plot(sort(p),(1:(W-1))/(W-1))




ps = unlist(foreach(i=1:1000)%dopar%{ foreach(j=1:50)%do%{tick = 0.0001; r0 = runif(100); r = r0; s = mean(r); p = 0; while(abs(s)>tick) { p=p+s; r=r*r0; s=mean(r) }; p }})
save(ps,file='res_runif.RData')
load(file='res_runif.RData')
psN = unlist(foreach(i=1:1000)%dopar%{ foreach(j=1:50)%do%{tick = 0.0001; r0 = rnorm(100)/10; r = r0; r0 = abs(r0); s = mean(r); p = 0; while(abs(s)>tick) { p=p+s; r=r*r0; s=mean(r) }; p }})
psN_c = unlist(foreach(i=1:1000)%dopar%{ foreach(j=1:50)%do%{tick = 0.0001; r0 = (rnorm(1)*0.01 + 0.99*rnorm(100))/10; r = r0; r0 = abs(r0); s = mean(r); p = 0; while(abs(s)>tick) { p=p+s; r=r*r0; s=mean(r) }; p }})

psN_ss = unlist(foreach(i=1:1000)%dopar%{ foreach(j=1:50)%do%{tick = 0.0001; r0 = (0.1 + 0.9*rnorm(100))/10; s_p=sum(r0[r0>0]); s_m=-sum(r0[r0<0]); if(s_p==0 || s_m==0) 0 else log(s_p/s_m) }})

ps_eq = unlist(foreach(i=1:1000)%dopar%{ foreach(j=1:50)%do%{tick = 0.0001; r0 = 1.9999*(runif(100)-0.5); r = r0; r0 = abs(r0); s = mean(r); p = 0; while(abs(s)>tick) { p=p+s; r=r*r0; s=mean(r) }; p }})

ps2 = unlist(foreach(i=1:1000)%dopar%{ foreach(j=1:100)%do%{tick = 0.0001; r0 = 2*(runif(1)*0.9999 + 0.0001*runif(100)-0.5); r = r0; r0 = abs(r0); s = mean(r); p = 0; while(abs(s)>tick) { p=p+s; r=r*abs(s); s=mean(r) }; p }})

BIG_NUM=1000000000000; N=50; rp0 = rep(0, N); covmat=cor2cov(matrix(rep(c(1,rep(0.5,N)),N+1)[1:(N*N)],N,N), rep(0.3,N)); ps2N = unlist(foreach(i=1:500)%dopar%{ foreach(j=1:500)%do%{tick = 0.000001; r = rmnorm(1, rp0, covmat); s = mean(r); p = 0; while(abs(s)>tick && abs(s)<BIG_NUM) { p=p+s; r=r*abs(s); s=mean(r) }; if(abs(s)>=BIG_NUM && 1==0) 0 else p }})


save(psN,file='res_rnorm.RData')
load(file='res_rnorm.RData')
show_tail_comparison_RANGE(list(ps),70,10000)


hist(ps2N[abs(ps2N)<100],br=600)
hist(ps2N,br=200)


#x0=1/runif(1:10000); #show_tail_comparison_RANGE(list(ps_eq),-100000,-1)
ps2N = p
x0=sort(ps2N[ps2N< -0.1])
x = -x0[x0<0]; y=(((1:length(x0))/length(x0))[x0<0]); plot(log(x),log(y))


plot(x,y)

h=hist(ps2N,br=200,plot=FALSE)
plot(log(-h$mids[h$mids<0]),log(h$counts[h$mids<0]))

hist(atan(rnorm(1000000))*2/pi,br=100)

plot(atan(seq(-30,30,l=10000)))

# take five 30-s from one of my 100-s, then plot g(each) on one plot, to see how they correspond:  just to check Shur a bit:)


yho=as.xts(get.hist.quote(i='^gspc',quote='Close'))[1000:5000]; 
y0=get_yhoo_shifted(yho,100,200)
y0_rng=range(y0)
e0_shur = shur_cdfg(y0,'plain'); r0=range(e0_shur); e0=ecdf(e0_shur); x0=seq(r0[1],r0[2],by=(r0[2]-r0[1])/200)

params = lapply(fls_res,function(x) { x$params }) 
#shurs = lapply(fls_res,function(x) { shur_cdfg(x$data,'plain') })
#shurs = foreach(m=fls_res)%dopar%{ c(shur_cdfg(m$data[1:30],'plain'),shur_cdfg(m$data[31:60],'plain'),shur_cdfg(m$data[61:90],'plain')) }
shurs = foreach(m=fls_res)%dopar%{ foreach(i=1:10), .combine=c)%do%shur_cdfg(sample(m$data,30),'plain') }


##
show_hist_comparison(list(f1[[1]]$data,f2[[1]]$data),30,50000)
show_hist_comparison(foreach(m=fls_res)%dopar%{ foreach(i=1:10)%do%shur_cdfg(sample(m$data,30),'plain') },30,50000)
show_hist_comparison(foreach(i=1:10)%do%shur_cdfg(sample(fls_res[[201]]$data,30),'plain'),30,50000)
show_hist_comparison(foreach(i=1:10)%do%shur_cdfg(sample(y0,20),'plain'),30,50000)
x=seq(0,0.0002,by=0.00001); plot(x,list_arr_avg(foreach(i=1:5)%do%ecdf(shur_cdfg(sample(f1[[1]]$data,500),'plain'))(x)),t='l',col='red')
x=seq(0,0.0002,by=0.00001); lines(x,list_arr_avg(foreach(i=1:5)%do%ecdf(shur_cdfg(sample(f2[[1]]$data,200),'plain'))(x)),t='l',col='blue')
x=seq(0,0.0002,by=0.00001); lines(x,list_arr_avg(foreach(i=1:5)%do%ecdf(shur_cdfg(sample(f2[[1]]$data,500),'plain'))(x)),t='l',col='darkgreen')
x=seq(0,0.0002,by=0.00001); lines(x,list_arr_avg(foreach(i=1:5)%do%ecdf(shur_cdfg(sample(f3[[1]]$data,500),'plain'))(x)),t='l',col='orange')

tss=list(f1[[1]]$data,f2[[1]]$data,f2[[1]]$data,f3[[1]]$data)
lsts=foreach(l=list(sample(f1[[1]]$data,500),sample(f2[[1]]$data,500),sample(f2[[1]]$data,500),sample(f3[[1]]$data,500)))%dopar%shur_cdfg(l,'plain')
show_tail_comparison_NONLOG(lsts,0,0.01)
show_hist_comparison(lsts,50,50000)

show_hist_comparison(,50,50000)
hs = foreach(ts=tss)%do%hist(ts,br=seq(-1,1,by=0.001
###


ranges = lapply(shurs,range)
ecdfs = lapply(shurs,ecdf)
get_ecdf = function(i,step) { x=seq(ranges[[i]][1],ranges[[i]][2],by=step); list(params=params[[i]], cdf=list(x=x,y=ecdfs[[i]](x))) }
#plot(curr_ecdfs[[1]]$cdf$x,curr_ecdfs[[1]]$cdf$y)

curr_ecdfs = lapply(1:length(ecdfs),function(x){ get_ecdf(x,0.0001) })

e0_shur = shur_cdfg(y0,'plain'); r0=range(e0_shur); e0=ecdf(e0_shur)
diffs = unlist(lapply(1:length(curr_ecdfs),function(x){ sum((e0(curr_ecdfs[[x]]$cdf$x)-curr_ecdfs[[x]]$cdf$y)^2) }))
diffs = unlist(lapply(1:length(curr_ecdfs),function(x){ ii=curr_ecdfs[[x]]$cdf$x<0.01; sum((e0(curr_ecdfs[[x]]$cdf$x[ii])-curr_ecdfs[[x]]$cdf$y[ii])^2) }))
#plot(curr_ecdfs[[55]]$cdf$y,e0(curr_ecdfs[[55]]$cdf$x)); 
optim_N = which(diffs==min(diffs))
plot(curr_ecdfs[[optim_N]]$cdf$x,e0(curr_ecdfs[[optim_N]]$cdf$x),t='l',col='red'); lines(curr_ecdfs[[optim_N]]$cdf$x,curr_ecdfs[[optim_N]]$cdf$y, col='black');
params[optim_N]

for(i in 1:length(params)) lines(curr_ecdfs[[i]]$cdf$x,curr_ecdfs[[i]]$cdf$y, col='black'); 

plot(curr_ecdfs[[1534]]$cdf$x,e0(curr_ecdfs[[830]]$cdf$x), ,t='l',col='red'); 


range(diffs)
which(diffs==min(diffs))
ii=which(abs(diffs-min(diffs))<0.1)
plot(curr_ecdfs[[optim_N]]$cdf$x,e0(curr_ecdfs[[optim_N]]$cdf$x),t='l',col='red'); 
for(i in ii) lines(curr_ecdfs[[i]]$cdf$x,curr_ecdfs[[i]]$cdf$y, col='black');
lines(curr_ecdfs[[optim_N]]$cdf$x,e0(curr_ecdfs[[optim_N]]$cdf$x),t='l',lwd=3,col='red'); 
lines(curr_ecdfs[[optim_N]]$cdf$x,curr_ecdfs[[optim_N]]$cdf$y,lwd=2, col='blue');

optim_N = which(diffs==min(diffs))
#pmin=unlist(params[optim_N])

#pmin=c(2e+02,7e+00,9e-01,5e-01,7e-03) # - first
#pmin=c(100.000,3.000,0.700,0.990,0.007) # - second
pmin=c(200.000,8.000,0.960,0.700,0.005) # - third
#pmin=c(1e+02,7e+00,9e-01,9e-01,7e-03) # - all 3 
xs3_FIX = pmin[4]; xs4_FIX = pmin[5]
dopar_rank = 1000
calc_data_params_lists_SEQ(list(50000,get_corr_3,pmin[1],pmin[2],pmin[3]),calc_exp3,'/mnt/G/RISK/ANTON/R/perc_corr/res-')
fl_test = load_perc_corr_grid_files_DOPAR(50000,get_corr_3,pmin[1],pmin[2],pmin[3],pmin[4],pmin[5],calc_exp3,'/mnt/G/RISK/ANTON/R/perc_corr/res-')

pmins_list=list(c(2e+02,7e+00,9e-01,5e-01,7e-03),c(100.000,3.000,0.700,0.990,0.007),c(200.000,8.000,0.960,0.700,0.005),c(1e+02,7e+00,9e-01,9e-01,7e-03))
ress=list()
for(p in pmins_list)
	ress=c(ress,list(load_perc_corr_grid_files_DOPAR(50000,get_corr_3,p[1],p[2],p[3],p[4],p[5],calc_exp3,'/mnt/G/RISK/ANTON/R/perc_corr/res-')[[1]]$data))


#source('/mnt/G/RISK/ANTON/R/R12.R'); yho=as.xts(get.hist.quote(i='^gspc',quote='Close'))[1000:5000];y0=get_yhoo_shifted(yho,1000,1100);y0_rng=range(y0); e0_shur = shur_cdfg(y0,'plain',SH_TRESH); r0=range(e0_shur); e0=ecdf(e0_shur); x0=seq(r0[1],r0[2],by=(r0[2]-r0[1])/200); e0_vals=e0(x0)
shur_opt_func = function(x){	
	tryCatch({
		
		#dopar_rank = 200
		#x1 = max(floor(x[1]),2)
		#x2 = max(floor(x[2]),1)
		#x3 = min(max(x[3],0.00001),0.99)
		#x4 = min(max(x[4],0.00001),0.999)
		#x5 = max(x[5],0.000001)
		x1 = truncate_pc_params(x)

		calc_data_params_lists_SEQ_WITHFIX(list(800,get_corr_3,x1[1],x1[2],x1[3]),list(x1[4],x1[5]),calc_exp3,'/mnt/G/RISK/ANTON/R/perc_corr/res-MC-')
		fl_test = load_perc_corr_grid_files_DOPAR(800,get_corr_3,x1[1],x1[2],x1[3],calc_exp3,'/mnt/G/RISK/ANTON/R/perc_corr/res-MC-')
		system('rm /mnt/G/RISK/ANTON/R/perc_corr/res-MC-*')
		res = sum((e0_vals-ecdf(shur_cdfg(fl_test[[1]]$data,'plain',SH_TRESH))(x0))^2)
		print(x1)
		print(paste('Diff:',res))
		res
	}, error=function(ex){ print(ex) })	
}


y = optim(c(1e+02,7e+00,9e-01,9e-01,7e-03),shur_opt_func,control=list(parscale=c(100.0,1,0.1,0.1,0.001), abstol=0.03))
y2 = optim(c(70,2,8e-01,8e-01,3e-03),shur_opt_func,control=list(parscale=c(100.0,1,0.1,0.1,0.001), abstol=0.03))
#y = optim(c(1e+02,7e+00,9e-01,9e-01,7e-03),shur_opt_func,method='SANN',control=list(parscale=c(100.0,1,0.1,0.1,0.001), temp=100, trace=TRUE, REPORT = 10))

p=truncate_pc_params(c(61.868035000,6.841555090,0.967643109,0.951758109,0.007075661)) # y0=get_yhoo_shifted(yho,100,200)
p=truncate_pc_params(c(-942.79680587,0.05833911,1.07504707,1.23190613,0.01323370)) # y0=get_yhoo_shifted(yho,1000,1100)


plot_my_series(c(compare_pc(list(p ),'scdf',x0,SH_TRESH),list(list(x=x0,y=e0_vals))),2)
plot_my_series(compare_pc(list(p ),'hist',60,SH_TRESH),1)


h = compare_pc(list(p ),'hist',200,SH_TRESH)[[1]]
sum(h$y[which(h$x< -0.03)])

a=compare_pc(list(p ),'hist',x0,SH_TRESH)
shur_opt_func(c(200.000,8.000,0.960,0.700,0.005))

errss0 = sd(unlist(foreach(i=1:20)%do%shur_opt_func(p)))
errss = unlist(foreach(ll=400*(1:5))%do%sd(unlist(foreach(i=1:20)%do%shur_opt_func_LEN(p,ll))))
errss_small = unlist(foreach(ll=10*(1:6))%do%sd(unlist(foreach(i=1:50)%do%shur_opt_func_LEN(p,ll))))

dff = foreach(x1=c(30,60,90), .combine=c)%do%{
	foreach(x2=c(3,6,9), .combine=c)%do%{
		foreach(x3=c(0.8,0.9), .combine=c)%do%{
			tmp_y = optim(c(x1,x2,x3,0.85,0.01),shur_opt_func,control=list(parscale=c(100.0,1,0.1,0.1,0.001), abstol=0.03))
			list(list(params=tmp_y$par, val=tmp_y$value))
		}
	}
}


source('/mnt/G/RISK/ANTON/R/R12.R');    SH_PCNT = 1;   p=c(100,1,0.9,0.95,0.01)
ps1 = 10; ps2 = 2; ps3 = 0.99; ps4 = 0.8; ps5 = 1
#ps1 = list(10); ps2 = as.list(1); ps3 = list(0.8); ps4 = 0.8; ps5 = 1
calc_data_params_lists_SEQ_WITHFIX(list(10000,get_corr_3,ps1,ps2,ps3),list(ps4,ps5),calc_exp4,'/mnt/G/RISK/ANTON/R/perc_corr/res-') 




ts = load_perc_corr_grid_files_DOPAR(10000,get_corr_3,ps1,ps2,ps3,calc_exp4,'/mnt/G/RISK/ANTON/R/perc_corr/res-')
params = lapply(ts,function(x) { round(x$params,10) })
tss = lapply(ts,function(x) { x$data })
unlist(ps1); unlist(ps2); unlist(ps3); unlist(ps4); unlist(ps5); 
show_hist_comparison(foreach(r=tss)%do%r[r!=0],70)
show_tail_comparison(foreach(r=tss)%do%r[r!=0],-1)



#2.000000000 3.000000000 0.990000000 0.776118498 0.005905029
p=c(76.0000000,7.0000000,0.9608400,0.8960400,0.0076084)
#sh_pcnt=0.75; source('/mnt/G/RISK/ANTON/R/R12.R'); yho=as.xts(get.hist.quote(i='^gspc',quote='Close'))[1000:5000]; y0=get_yhoo_shifted(yho,100,200); y0_rng=range(y0); e0_shur = shur_cdfg(y0,'plain',sh_pcnt); r0=range(e0_shur); e0=ecdf(e0_shur); x0=seq(r0[1],r0[2],by=(r0[2]-r0[1])/200); e0_vals=e0(x0)






source('/mnt/G/RISK/ANTON/R/R12.R');    SH_PCNT = 1;   p=c(100,1,0.9,0.95,0.01)
#ps1 = list(100,200,300); ps2 = as.list(1:8); ps3 = list(0.8,0.85,0.9,0.92,0.94,0.96,0.98,0.99); ps4 = c(0.7,0.8,0.9,0.99); ps5 = 0.2*p[5]*(1:25)
ps1 = list(100); ps2 = as.list(1:8); ps3 = list(0.8,0.85,0.9,0.92,0.94,0.96,0.98,0.99); ps4 = c(0.7,0.8,0.9,0.99); ps5 = 0.2*p[5]*(1:25)
calc_data_params_lists_SEQ_WITHFIX(list(25000,get_corr_3,ps1,ps2,ps3),list(ps4,ps5),calc_exp4,'/mnt/G/RISK/ANTON/R/perc_corr/res-') 
ts = load_perc_corr_grid_files_DOPAR(25000,get_corr_3,ps1,ps2,ps3,calc_exp4,'/mnt/G/RISK/ANTON/R/perc_corr/res-')
params = lapply(ts,function(x) { round(x$params,10) })
tss = lapply(ts,function(x) { x$data })
unlist(ps1); unlist(ps2); unlist(ps3); unlist(ps4); unlist(ps5); 

masks = list(
list(idx=c(2,3), msk=c(1,0.99)),
list(idx=c(2,3), msk=c(1,0.92)),
list(idx=c(2,3), msk=c(4,0.99)),
list(idx=c(2,3), msk=c(4,0.92))
)

sidxs = foreach(mask=masks)%do%which(unlist(lapply(params,function(x){as.logical(min(x[mask$idx]==mask$msk))})))
shurs = foreach(sidx=sidxs)%do%{ foreach(i=sidx)%dopar%{ 
			print(i)
			tmp_data = shur_cdfg(tss[[i]][1:1500],'plain') 
			#list(params=params[[i]], range=range(tmp_data), ecdf=ecdf(tmp_data))  }}
			list(params=params[[i]], range=range(tmp_data), data=tmp_data)  }}

YHO = as.xts(get.hist.quote(i='^gspc',quote='Close'));

## ---\/  piece  \/-- ##



WND = 100;   y0=get_yhoo_shifted(YHO,length(YHO)-WND,length(YHO)); e0_shur = shur_cdfg(y0,'plain');
#yho_opt=getOptionChain('spy')



curp = as.numeric(YHO[length(YHO)])




#--  --#
if(1==0){
idxs = sample(length(tss),50); pair_sdiffs = c(); pair_diffs = c(); samp_idxs = foreach(i=1:50)%dopar%sample(idxs,2)
shur_ths = seq(0.3,1,by=0.1)
res = foreach(tresh=shur_ths)%do%{ foreach(i=samp_idxs, .combine=rbind)%dopar%{ 
	print(paste(tresh,i))
	data.frame(sdiff=distrib_diff(tss[[i[1]]][1:100],tss[[i[2]]][1:100],'Shur',2000,tresh),diff=distrib_diff(tss[[i[1]]],tss[[i[2]]],'CDF',2000,0))
}}
i=8; print(shur_ths[i]); plot(res[[i]]$diff*1000,res[[i]]$sdiff*1000)
cors = unlist(lapply(res,function(x){cor(x$diff,x$sdiff)})); plot(cors)

i=which.max(res[[5]]$diff); i=samp_idxs[[i]]
x1=tss[[i[1]]][1:1500]; x2=tss[[i[2]]][1:1500]; diff_type='Shur'; diff_len=2000; diff_param=0.7
plot(rng_seq,ecdf(sx1)(rng_seq),t='l',col='red')
lines(rng_seq,ecdf(sx2)(rng_seq),t='l',col='red')
}
#--  --#



diffs_s = foreach(ssrs=shurs)%do%unlist(foreach(x=ssrs)%dopar%{ 1000*distrib_diff(x$data,e0_shur,'TwoShurs',2000,1) })
#     sum(0.01*(x$ecdf(x0)-e0_vals)^2))
diffs_s=diffs
pdiffs_s = foreach(sidx=sidxs)%do%{ pctl = -0.01; pdiffs = unlist(foreach(r=tss[sidx])%do%{ length(which(r<=pctl))/length(r) }) }
save(diffs_s,file='diffs_tmp.RData')
save(pdiffs_s,file='pdiffs_s_tmp.RData')
load('diffs_tmp.RData')
load('pdiffs_s_tmp.RData')

show_ids = c(1,2,3,4)
for(i in 1:length(show_ids)){
	diffs = diffs_s[[show_ids[i]]]; pdiffs = pdiffs_s[[show_ids[i]]]; 
	diff_tresh = 100; diffs=diffs[order(pdiffs)]; pdiffs=pdiffs[order(pdiffs)]; if(i==1) plot(diffs[which(diffs<diff_tresh)],pdiffs[which(diffs<diff_tresh)],col=global_cols[i],t='p',lwd=1,xlim=c(0,diff_tresh),ylim=c(0,0.5)) else lines(diffs[which(diffs<diff_tresh)],pdiffs[which(diffs<diff_tresh)],t='p',lwd=1,col=global_cols[i])
}
















#symbs_in = c('UNRATE','USD3MTD156N','DGS10','EXCHUS','EXUSEU','MNFCTRMPCIMSA','PERMIT'); forecast_wnd=100
#symbs_in = c('SP500'); forecast_wnd=100
fit_shurDist_tofacs = function(symbs_in, forecast_wnd){
	getSymbols(symbs_in,src='FRED')
	facs = foreach(symb = symbs_in, .combine=merge.xts)%do%{
		tmp_data = coredata(get(symb))
		tmp_dates = paste(format(index(get(symb)),'%Y-%m'),'01',sep='-')
		as.xts(aggregate(tmp_data[!is.na(tmp_data)],by=list(tmp_dates[!is.na(tmp_data)]),FUN=mean)[,2], order.by=as.Date(unique(tmp_dates)))
	}
	facs = facs[unlist(lapply(index(facs),function(x){as.logical(min(!is.na(facs[x,])))})),]
	
	yho_idx = index(YHO)
	res_ps = foreach(idx=index(facs), .combine=rbind)%do%{
		print(idx)
		y0 = diff(log(as.numeric(YHO[yho_idx>=idx & yho_idx<=idx+forecast_wnd])))
		e0_shur = shur_cdfg(y0,'plain');
		diffs = foreach(ssrs=shurs)%do%unlist(foreach(x=ssrs)%dopar%{ 1000*distrib_diff(x$data,e0_shur,'TwoShurs',2000,1) })
		#c(idx,as.numeric(params[[unlist(foreach(i=1:length(sidxs))%do%sidxs[[i]][which(diffs[[i]]==min(diffs[[i]]))])[1]]]))
		fit_idxs = unlist(foreach(i=1:length(sidxs))%do%sidxs[[i]][which(abs(diffs[[i]]-min(diffs[[i]]))<0.01)])
		qs = unlist(lapply(tss[fit_idxs],function(x){ quantile(x,0.05) }))
		#foreach(p=0.01*(1:10), .combine=c)%do%{ foreach }
		c(as.character(idx),facs[idx,],length(fit_idxs),mean(qs),sd(qs)/mean(qs))
	}

	res_ps = as.xts(foreach(i=1:dim(res_ps)[1],.combine=rbind)%do%as.numeric(res_ps[i,2:11]),order.by=as.Date(res_ps[,1]))
	colnames(res_ps) = c('unemp','libor','tres','usdchi','usdeur','prod','permit','len','mean','sd')
	res_ps
}




source('/mnt/G/RISK/ANTON/R/R12.R');   
res = tss[fit_idxs]
save(res,file='res_tmp.RData')
save(e0_shur,file='e0_shur_tmp.RData')
save(res_ps,file='res_ps_tmp.RData')

load('res_tmp.RData')
show_hist_comparison(res,100)
load('e0_shur_tmp.RData')
load('res_ps_tmp.RData')
r = res_ps
unlist(foreach(i=1:7)%do%{r0=range(r[,i]); rm=r0[2]-r0[1]; r[,i]=r[,i]/rm})
unlist(foreach(i=1:7)%do%{r0=range(r[,i]); r0[2]-r0[1]; })
rd=diff(r)
idx=1:(length(r$mean)-1)
plot(as.numeric(r$libor),as.numeric(r$mean))

a1 = r$mean; a2 = r[,1:7]; a3 = lm(a1 ~ a2); a3$coefficients #plot(a3)
names(r)


fi=1:4; mr = foreach(i=1:(dim(res_x)[1]-1),.combine=rbind)%do%{ foreach(j=(i+1):dim(res_x)[1],.combine=rbind)%do%{di=as.numeric(res_x[i,]); dj=as.numeric(res_x[j,]); data.frame(f=sum((di[fi]-dj[fi])^2),v=abs(di[9]-dj[9]))} }; plot(mr$f,mr$v)


#--- \/ --OPTIONS-- \/ ---#

get_option_prices = function(s_in,ts_idx,w_in,sample_sz){
	r=tss[[ts_idx]]; r=unlist(lapply(1:sample_sz,function(x){ sum(sample(r,w_in)) })); rng_r = range(r)
	br = seq(rng_r[1],rng_r[2],len=floor(length(r)/200)); hist_r = hist(r,br=br,plot=FALSE); hrd = hist_r$counts/length(r); hrm = hist_r$mids

	call_pnl_func = function(p_in){ list(x=hrm, y=unlist(lapply(1:length(hrm),function(i){ if(hrm[i]<s_in) -p_in*hrd[i] else hrd[i]*(hrm[i]-s_in-p_in) }))) }
	call_opt = optimize(function(x){ abs(sum(call_pnl_func(x)$y)) },c(-10,10)); 

	put_pnl_func = function(p_in){ list(x=hrm, y=unlist(lapply(1:length(hrm),function(i){ if(hrm[i]<s_in) (s_in-hrm[i]-p_in)*hrd[i] else -p_in*hrd[i] }))) }
	put_opt = optimize(function(x){ abs(sum(put_pnl_func(x)$y)) },c(-10,10)); 

	data.frame(call=call_opt$minimum, put=put_opt$minimum)
}


get_putcall_quote0 = function(ps_in,w_in){
	tmp_idxs = unlist(foreach(i=1:length(sidxs))%do%sidxs[[i]][which(abs(diffs_s[[i]]-min(diffs_s[[i]]))<0.1)])
	m = curp/10
	foreach(i=1:length(ps_in), .combine=rbind)%do%{ 
		p_in = ps_in[i]
		print(p_in)
		pps = foreach(j=tmp_idxs, .combine=rbind)%dopar%get_option_prices(log(p_in/curp),j,w_in,200000)
		puts = (exp(pps$put)-1)*m; calls = (exp(pps$call)-1)*m
		#puts = pps$put; calls = pps$call
		data.frame(R1=round(sd(calls)/mean(calls),4),
			   SD1=round(sd(calls),4),
			   Call=round(mean(calls),4),
			   P=p_in,
			   Put=round(mean(puts),4),
			   SD2=round(sd(puts),4),
			   R2=round(sd(puts)/mean(puts),4))		
	}
}

#pps = get_putcall_quote0(c(1750),c(8))
strikes = c(167,168,169,186,187,188,189,190)
#tickers1 = paste('SPY131129XXXX00',strikes,'000',sep='')
#tickers2 = paste('SPY131206XXXX00',strikes,'000',sep='')
pps1 = get_putcall_quote0(strikes*10,8)
pps2 = get_putcall_quote0(strikes*10,13)
pps1
pps2

#yho_opt$puts[gsub('XXXX','P',tickers2),]

#--- //// --OPTIONS-- //// ---#






curl -i -X POST -H 'Content-Type: application/json' -d '{"seriesid":["LEU0254555900", "APU0000701111"],"startyear":"2002", "endyear":"2012"}' http://api.bls.gov/publicAPI/v1/timeseries/data/ -o tmp_data.txt

source('/mnt/G/RISK/ANTON/R/R12.R')
symbs = c('LNS14000003','LNS13008636','EIUIR','EIUIQ'); 
datas = xts()
foreach(symb=symbs)%do%{
	r = dynCurlReader()
	pf=paste('{"seriesid":["',symb,'"],"startyear":"2004", "endyear":"2013"}',sep='')
	curlPerform(url='http://api.bls.gov/publicAPI/v1/timeseries/data/', httpheader=c("Content-Type"="application/json"), postfields=pf, writefunction=r$update)
	d = strsplit(gsub('\n','',gsub('codeR,textrevised','',gsub('codeR,textRevised','',gsub('codeP,textpreliminary','pre',gsub(',footnotes','',gsub('year','',gsub('value','',gsub('period','',gsub('periodName','',gsub(':','',gsub('"','',gsub('\\[','',gsub('\\]','',gsub('\\{','',gsub('\\}','',gsub('"data"','',substring(r$value(),regexpr('"data"',r$value())[1]))))))))))))))))),',')[[1]]
	tmp_data = t(matrix(d,nrow=4))[,c(1,2,4)]
	tmp_data = tmp_data[!grepl('pre',tmp_data[,3]),]
	tmp_data = xts(as.numeric(tmp_data[,3]),order.by=as.Date(paste(tmp_data[,1],gsub('M','',tmp_data[,2]),'01',sep='-')))
#	tmp_data = xts(tmp_data[,3],order.by=as.Date(paste(tmp_data[,1],gsub('M','',tmp_data[,2]),'01',sep='-')))
	datas = if(which(symbs==symb)[1]==1) tmp_data else merge.xts(datas,tmp_data)
}





YHO_WND=30; sh_pcnt=0.75; sh_len=1500; yho=as.xts(get.hist.quote(i='^gspc',quote='Close')); y0=get_yhoo_shifted(yho,length(yho)-YHO_WND,length(yho)); e0_shur = shur_cdfg(y0,'plain',sh_pcnt); e0=ecdf(e0_shur); 
curp = as.numeric(yho[length(yho)])
diffs_s = foreach(i=1:length(sidxs))%do%{
	ssrs=shurs[[i]]; x0=range(unlist(foreach(r=ssrs)%do%r$range)); x0=seq(x0[1],x0[2],len=20000); e0_vals=e0(x0)
	unlist(foreach(x=ssrs)%dopar%sum(0.01*(x$ecdf(x0)-e0_vals)^2))
}
pdiffs_s = foreach(sidx=sidxs)%do%{ pctl = -0.005; pdiffs = unlist(foreach(r=tss[sidx])%do%{ length(which(r<=pctl))/length(r) }) }
pps1 = get_putcall_quote0(c(1760,1820),6)

YHO_WND=50; sh_pcnt=0.75; sh_len=1500; yho=as.xts(get.hist.quote(i='^gspc',quote='Close')); y0=get_yhoo_shifted(yho,length(yho)-YHO_WND,length(yho)); e0_shur = shur_cdfg(y0,'plain',sh_pcnt); e0=ecdf(e0_shur); 
curp = as.numeric(yho[length(yho)])
diffs_s = foreach(i=1:length(sidxs))%do%{
	ssrs=shurs[[i]]; x0=range(unlist(foreach(r=ssrs)%do%r$range)); x0=seq(x0[1],x0[2],len=20000); e0_vals=e0(x0)
	unlist(foreach(x=ssrs)%dopar%sum(0.01*(x$ecdf(x0)-e0_vals)^2))
}
pdiffs_s = foreach(sidx=sidxs)%do%{ pctl = -0.005; pdiffs = unlist(foreach(r=tss[sidx])%do%{ length(which(r<=pctl))/length(r) }) }
pps2 = get_putcall_quote0(c(1760,1820),6)

YHO_WND=80; sh_pcnt=0.75; sh_len=1500; yho=as.xts(get.hist.quote(i='^gspc',quote='Close')); y0=get_yhoo_shifted(yho,length(yho)-YHO_WND,length(yho)); e0_shur = shur_cdfg(y0,'plain',sh_pcnt); e0=ecdf(e0_shur); 
curp = as.numeric(yho[length(yho)])
diffs_s = foreach(i=1:length(sidxs))%do%{
	ssrs=shurs[[i]]; x0=range(unlist(foreach(r=ssrs)%do%r$range)); x0=seq(x0[1],x0[2],len=20000); e0_vals=e0(x0)
	unlist(foreach(x=ssrs)%dopar%sum(0.01*(x$ecdf(x0)-e0_vals)^2))
}
pdiffs_s = foreach(sidx=sidxs)%do%{ pctl = -0.005; pdiffs = unlist(foreach(r=tss[sidx])%do%{ length(which(r<=pctl))/length(r) }) }
pps3 = get_putcall_quote0(c(1760,1820),6)







show_hist_comparison(tss[unlist(foreach(i=1:length(sidxs))%do%sidxs[[i]][which(diffs_s[[i]]<0.2)])],80)


show_hist_comparison(tss[unlist(foreach(i=1:length(sidxs))%do%print(which(diffs_s[[i]]<0.1))],100)
tssq = tss[unlist(foreach(i=1:length(sidxs))%do%sidxs[[i]][which(diffs_s[[i]]<0.2)])]; for(r in tssq)print(length(which(r<=-0.01))/length(r) )












unlist(foreach(i=1:length(sidxs))%do%sd(pdiffs_s[[i]][which(abs(diffs_s[[i]]-min(diffs_s[[i]]))<0.1)]))
sd(unlist(foreach(i=1:length(sidxs))%do%pdiffs_s[[i]][which(abs(diffs_s[[i]]-min(diffs_s[[i]]))<0.1)]))
mean(unlist(foreach(i=1:length(sidxs))%do%pdiffs_s[[i]][which(abs(diffs_s[[i]]-min(diffs_s[[i]]))<0.1)]))
hist(unlist(foreach(i=1:length(sidxs))%do%pdiffs_s[[i]][which(abs(diffs_s[[i]]-min(diffs_s[[i]]))<0.1)]),br=5)

unlist(foreach(i=1:length(sidxs))%do%sidxs[[i]][which(abs(diffs_s[[i]]-min(diffs_s[[i]]))<0.1)])



pps=unlist(foreach(i=1:5000)%dopar%get_opt_price(-0.005,2701,2,50000)) # sd(pps) = 0.02304285
pps2=unlist(foreach(i=1:50)%dopar%get_opt_price(-0.005,2701,2,500000)) # sd(pps2) = 0.005986325,  same mean

pps3=unlist(foreach(i=unlist(foreach(i=1:length(sidxs))%do%sidxs[[i]][which(abs(diffs_s[[i]]-min(diffs_s[[i]]))<0.1)]))%dopar%get_opt_price(-0.01,i,2,500000))
pps4=unlist(foreach(i=unlist(foreach(i=1:length(sidxs))%do%sidxs[[i]][which(abs(diffs_s[[i]]-min(diffs_s[[i]]))<0.1)]))%dopar%get_opt_price(-0.01,i,14,500000))
pps5=unlist(foreach(i=unlist(foreach(i=1:length(sidxs))%do%sidxs[[i]][which(abs(diffs_s[[i]]-min(diffs_s[[i]]))<0.1)]))%dopar%get_opt_price(-0.01,i,6,500000))
pps6=unlist(foreach(i=unlist(foreach(i=1:length(sidxs))%do%sidxs[[i]][which(abs(diffs_s[[i]]-min(diffs_s[[i]]))<0.1)]))%dopar%get_opt_price(1-curp/1750,i,6,500000))

pps_call=unlist(foreach(i=unlist(foreach(i=1:length(sidxs))%do%sidxs[[i]][which(abs(diffs_s[[i]]-min(diffs_s[[i]]))<0.1)]))%dopar%get_opt_price(1-curp/1750,i,6,500000))
pps_put=unlist(foreach(i=unlist(foreach(i=1:length(sidxs))%do%sidxs[[i]][which(abs(diffs_s[[i]]-min(diffs_s[[i]]))<0.1)]))%dopar%get_opt_price(1-curp/1750,i,6,500000))

s_in = -0.005; ts_idx=2701; w_in=2; sample_sz=40000; 
	r=tss[[ts_idx]]; r=unlist(lapply(1:sample_sz,function(x){ sum(sample(r,w_in)) })); rng_r = range(r)
	br = seq(rng_r[1],rng_r[2],len=floor(length(r)/200)); hist_r = hist(r,br=br,plot=FALSE); hrd = hist_r$counts/length(r); hrm = hist_r$mids
	pnl_func = function(p_in){ list(x=hrm, y=unlist(lapply(1:length(hrm),function(i){ if(hrm[i]<s_in) -p_in*hrd[i] else hrd[i]*(hrm[i]-s_in) }))) }
	#optim(1,function(x){ abs(sum(pnl_func(x)$y)) })
	optimize(function(x){ abs(sum(pnl_func(x)$y)) },c(-10,10))$minimum 
rr=pnl_func(0.491792)
plot(rr$x,rr$y,br=br)

optim(1,function(x1) { abs(sum(pnl_func(x1,x)$y)) })
optimize(function(x1) { abs(sum(pnl_func(x1,x)$y)) },c(-10,10))









### nicht ### \\/\/\/\//


source('/mnt/G/RISK/ANTON/R/R12.R'); p=c(100,1,0.9,0.95,0.01)
#ps1 = list(100,200); ps2 = as.list(1:5); ps3 = list(0.6,0.7,0.8,0.9,0.93,0.96,0.99); ps4 = p[4]-0.1*(0:4); ps5 = p[5]*(1:5)
ps1 = 100; ps2 = as.list(1:4); ps3 = list(0.8,0.9,0.93,0.96,0.99); ps4 = 0.99; ps5 = 0.25*p[5]*(1:12)
calc_data_params_lists_SEQ_WITHFIX(list(10000,get_corr_3,ps1,ps2,ps3),list(ps4,ps5),calc_exp4,'/mnt/G/RISK/ANTON/R/perc_corr/res-') 
ts = load_perc_corr_grid_files_DOPAR(10000,get_corr_3,ps1,ps2,ps3,calc_exp4,'/mnt/G/RISK/ANTON/R/perc_corr/res-')
params = lapply(ts,function(x) { round(x$params,10) })
tss = lapply(ts,function(x) { x$data })
unlist(ps1); unlist(ps2); unlist(ps3); unlist(ps4); unlist(ps5); 


sidx = 1:length(tss)
sidx = which(unlist(lapply(params,function(x){as.logical(min(c(x[2])==c(2)))}))); 
sh_pcnt=0.75; pctl = -0.01; 
yho=as.xts(get.hist.quote(i='^gspc',quote='Close'))[1000:5000]; 

sbo = foreach(idx=1:4)%do%{ foreach(r=tss[which(unlist(lapply(params,function(x){as.logical(min(c(x[2])==c(idx)))})))])%dopar%{ shur_cdfg(r[1:1000],'plain',pcnt=sh_pcnt) } }

gr = foreach(ssrs=sbo)%do%{
	#ssrs = sbo[[idx]] #foreach(r=tss[sidx])%dopar%{ shur_cdfg(r[1:1000],'plain',pcnt=sh_pcnt) }
	x0=range(unlist(lapply(ssrs,range))); x0=seq(x0[1],x0[2],len=20000); ssrs_ecdf = foreach(r=ssrs)%dopar%{ list(x=x0, y=ecdf(r)(x0)) }; y0=get_yhoo_shifted(yho,100,200); e0_shur = shur_cdfg(y0,'plain',sh_pcnt); e0=ecdf(e0_shur); e0_vals=e0(x0)

	pdiffs = unlist(foreach(r=tss[sidx])%do%{ length(which(r<=pctl))/length(r) }); diffs = unlist(lapply(ssrs_ecdf,function(x){ sum(0.01*(x$y-e0_vals)^2) })); diffs=diffs[order(pdiffs)]; pdiffs=pdiffs[order(pdiffs)]; 
	list(x=diffs[which(diffs<100)], y=pdiffs[which(diffs<100)]) 
}

foreach(ti in 1:4) else lines(diffs[which(diffs<100)],pdiffs[which(diffs<100)],t='l',col=global_cols[idx])

plot(sort(diffs))
plot(diffs[order(diffs)[1:20]],pdiffs[order(diffs)[1:20]])
plot(diffs[order(diffs)],pdiffs[order(diffs)])




idx = which(unlist(lapply(params,function(x){as.logical(min(c(x[1],x[2],x[3],x[4],x[5])==c(100,1,0.93,0.55,0.05)))}))); 
show_hist_comparison(tss[idx],200)
show_tail_comparison(tss[idx],-0.001)


hist(tss[[149]])

 

r = tss[[idx]]; 
sr = foreach(i=1+100*(0:99))%dopar%{ shur_cdfg(r[i:(i+30)],'plain',pcnt=0.75) }
x0=range(unlist(lapply(sr,range))); x0=seq(x0[1],x0[2],len=200); sr_e = foreach(r=sr)%dopar%{ list(x=x0, y=ecdf(r)(x0)) }
diffs = unlist(lapply(sr_e,function(x){ sum((x$y-sr_e[[1]]$y)^2) }))
plot_my_series(sr_e,-1)



#f1=list_arr_avg(foreach(i=1:1)%do%ecdf(sample(t2,30))(x0)); f2=list_arr_avg(foreach(i=1:1)%do%ecdf(sample(t2,30))(x0)); f3=list_arr_avg(foreach(i=1:1)%do%ecdf(sample(t2,30))(x0))
f1=list_arr_avg(foreach(i=1:1)%do%ecdf(shur_cdfg(sample(ts[1:30],30),'plain',sh_pcnt))(x0)); f1=e0(x0)
f2=list_arr_avg(foreach(i=1:1)%do%ecdf(shur_cdfg(sample(ts[131:160],30),'plain',sh_pcnt))(x0))
f3=list_arr_avg(foreach(i=1:1)%do%ecdf(shur_cdfg(sample(ts[1:300],300),'plain',sh_pcnt))(x0))
#f1 = ecdf(shur_cdfg(ts[1:30],'plain'))(x0); f2 = ecdf(shur_cdfg(ts[1:40],'plain'))(x0); f3 = ecdf(shur_cdfg(ts[1:50],'plain'))(x0)
plot(x0,f1,t='l',ylim=range(c(f1,f2,f3)),col='red',lwd=3); lines(x0,f2,col='darkgreen',lwd=3); lines(x0,f3,col='blue',lwd=3)


sh3=ecdf(shur_cdfg(sample(ts[301:600],300),'plain'))(x0)
diff0 = sum((sh3-ecdf(shur_cdfg(ts[131:160],'plain'))(x0))^2)
ii=seq(1,161,by=10); diffs = lapply(ii,function(x){ print(x); sum((sh3-list_arr_avg(foreach(i=1:x)%do%ecdf(shur_cdfg(sample(ts[131:160],15),'plain'))(x0)))^2) })
plot(ii,unlist(diffs)-diff0,xlim=c(2,max(ii)))
plot(ii,unlist(diffs),xlim=c(2,max(ii)))

hs = lapply(list(get_yhoo_shifted(yho,100,130), get_yhoo_shifted(yho,150,180), get_yhoo_shifted(yho,180,210)), shur_cdfg)
hs = lapply(ress, shur_cdfg)

plot(hs[[1]][[1]],hs[[1]][[2]],t='l',col=cols[1], xlim=range(unlist(lapply(hs,function(x){ range(x$x) }))), ylim=range(unlist(lapply(hs,function(x){ range(x$y) }))))
for(i in 2:length(hs)) lines(hs[[i]][[1]],hs[[i]][[2]],t='l',col=cols[i])


plot(h1, col=rgb(0,0,1,1/4), xlim=range(c(range(h1$breaks),range(h2$breaks),range(h3$breaks))))  # first histogram
plot(h2, col=rgb(1,0,0,1/4), add=T)  # second











res = optimize_over_4_lists(list(list(100,400,700,1000),list(1,4,7,10),list(0.9,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98),list(0.9,0.93,0.96,0.99)),function(x){e0=ecdf(load_param_file(x)); abs((e0(0.01)-e0(-0.01))-(e(0.01)-e(-0.01)))})


a=list(1,2,3); b=list(3,4,5); c=list(8,9,10); 

enlist_all_combs = function(lst){ 
	tmp_res = if(length(lst)>0) foreach(x=lst[[1]])%do%{ foreach(l=enlist_all_combs(lst[-1]))%do%{ c(x,l) } } else list()
print(tmp_res)
	res = list() 
	for(l in tmp_res) res=c(res,l) 
	res 
}

enlist_all_combs(list(a,b,c))




exp_params = list(
	list(700,7,0.97,0.9),
	list(700,7,0.97,0.99)
)

ress = list()
for(p in exp_params)
	ress = c(ress,list(calc_data_params_vals(list(1000,get_corr_3,p[[1]],p[[2]],p[[3]],p[[4]]),calc_exp1,'/mnt/G/RISK/ANTON/R/perc_corr/2000/res-')))


#hs = show_hist_comparison(ress,60,50000)
hs = show_tail_comparison(ress,-0.005)











