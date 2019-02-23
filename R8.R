rm(list = ls())
n = 2^22; n_i = 1500
scores = array(1,n)
for(i in 1:n_i){
	hlds = which(scores>=1)
	wnrs = sample.int(length(hlds),length(hlds)/2)
	scores[hlds[wnrs]] = scores[hlds[wnrs]]+1
	scores[hlds[-wnrs]][1:length(wnrs)] = scores[hlds[-wnrs][1:length(wnrs)]]-1

	if(i%%10==0){
		print(i)
		print(length(hlds))
	}
}

h_step = 4; h = hist(scores[scores!=0],br=seq(range(scores)[1],range(scores)[2]+h_step,by=h_step),plot=FALSE)
plot(h)
hm = h$mids; hc = h$counts; hc=hc[hm>100]; hm=hm[hm>100]; plot(log(hc),log(hm),t='l');
hm = h$mids; hc = h$counts; hm=hm[hc<2000]; hc=hc[hc<2000]; plot(hm,hc,t='l'); 
plot(log(hc),log(hm),t='l');
#plot(rev(sort(scores[scores!=0])),t='l')
#y=rev(sort(scores[scores!=0])); x=1:length(y); plot(log(x),log(y),t='l',xlim=c(1,8),ylim=c(4,5))


x=0.01*(1:100); plot(x,afunc(x))


a = array(0,10000)
p = 0.1
for(i in 1:100000) {
	idx = sample(length(a),round(p*length(a)),replace=TRUE)
	a[idx] = a[idx]+1
}
hist(a,br=40)



tt = 0.0001; xx = 0.0001


flow_in = 0.8; # total flow is 1, flow_in is width fo the flow, its concentration over [0,1]
flow_aval = 0.4
afunc = function(x) x^2
r0func = function(x) if(x<0.5) 0 else flow_aval

res_len = round(1/xx); sample_len = round(flow_in*res_len); inflow_conc = 1/sample_len; afunc_idxs = afunc((1:res_len)/res_len)%/%xx
res = array(0, res_len)

for(i in 1:100){
	inflow_idx = sample(res_len,sample_len,replace=TRUE)
	res[inflow_idx] = res[inflow_idx] + inflow_conc
	for(j in 1:res_len) ...
}




k_in = 5; b_in = 0.18
rnd = c(); res = c()
res = foreach(t1=1:10, .combine=c)%dopar%{ foreach(t2=1:10000, .combine=c)%do%{
	n = 0; 
	s = 1; s_new = 0
	while(s>0) {
		rnd = runif(s*k_in)
		s_new = 0
		for(i in 1:s) for(j in 1:k_in) if(rnd[i*j] < b_in) { n = n+1; s_new = s_new+1 }
		s = s_new
	}	
	n
} }

h = hist(res,br=100,plot=FALSE); plot(log(h$mids[h$counts>0]),log(h$counts[h$counts>0]))
x0 = seq(min(res),max(res),len=10000); e0 = 1-ecdf(res)(x0); plot(log(x0),log(e0))
plot(x0,e0)

hist(res[which(res>10)],br=100)





source('/mnt/G/RISK/ANTON/R/R12.R')
exp_len = 20000; cells_N = 2; s = 0.15; d = 0.999; b = 2; sns = sample(1:cells_N,exp_len*10,replace=TRUE); 
v = array(0,cells_N); avs = array(0,0); pc = 0; pc = 1/b; falls_sample = array(0,0); falls_d = array(0,0)
started = FALSE; i=1
while(length(avs) < exp_len){
	idx = sns[i]
	v[idx] = v[idx] + s
	av_tmp = 0
	if(v[idx] > 1){
		idxs = which(v>1)
		while(length(idxs)>0){
			av_tmp = av_tmp + 1
			v[idxs[1]] = v[idxs[1]] - 1; 
			falls_sample = sample(1:cells_N,b)
			falls_d = runif(b) < d
			v[falls_sample][falls_d] = v[falls_sample][falls_d] + pc
			idxs = which(v>1)
		}
	}
	
	if(av_tmp > 0)
		started = TRUE
	if(started)
		avs = c(avs,av_tmp)
	i = i + 1
}



x0 = seq(min(avs),max(avs),len=10000); e0 = 1-ecdf(avs)(x0); plot(log(x0),log(e0))




plot(x0,e0)





















