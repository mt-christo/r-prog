source('/var/www/R/run/env_init.R') 
source('/var/www/R/lib/mc_lib.R') 
pmwAnton <<- odbcConnect("pmwRisk",uid=pmwAnton_uid,pwd=pmwAnton_pwd)
library(mnormt)
library(doMC) 
library(bindata)
library(mvtBinaryEP)
library(MBESS)
library(sn)
library(corpcor)
registerDoMC(25);


# FUNCTION: separate time series by quantiles of another time series
# base_ts - base time series 
# perc_brakes values are in [0,1], delimiting state percentiles, for example c(0, 0.25, 0.5, 0.75, 1)
# ts - xts object being analyzed (could be multi-column)
q_separate = function(state_ts,perc_brakes,ts){
	res = hash()
	res$quantiles = as.numeric(unlist(lapply(perc_brakes, function(x){quantile(state_ts,x)})))
	res$series = lapply(2:length(res$quantiles), function(x){ts[index(state_ts)[which(state_ts > res$quantiles[x-1] & state_ts < res$quantiles[x])],]})
	res$corr_matrices = lapply(res$series, FUN=cor)
	res$means = lapply(res$series, FUN=mean)
	res$sds = lapply(res$series, FUN=sd)
	res
}

val_separate = function(state_ts,min_value,max_value,ts){
	ts[index(state_ts)[which(state_ts > min_value & state_ts < max_value)]]
}

F_step = function(y0,x1,x2) function(x) if(x < x1) y0 else if(x < x2) y0 + (x - x1)*(1 - y0)/(x2 - x1) else 1
G_step = function(y0,x1,x2) function(x) if(x < x1) 1 else if(x < x2) 1 - (x - x1)*(1 - y0)/(x2 - x1) else y0

S_IDX = sqlQuery(pmwAnton,"exec [Crestline].[Risk_PortfolioReturns_GetBenchmarkReturns] 'SPTR_INDEX','19950101','20130101'"); S_IDX=xts(S_IDX[,3],order.by=as.Date(S_IDX[,2]))
C_IDX = sqlQuery(pmwAnton,"exec [Crestline].[Risk_PortfolioReturns_GetBenchmarkReturns] 'DLJHTR_INDEX','19950101','20130101'"); C_IDX=xts(C_IDX[,3],order.by=as.Date(C_IDX[,2]))
R_IDX = sqlQuery(pmwAnton,"exec [Crestline].[Risk_PortfolioReturns_GetBenchmarkReturns] 'USGG10Y_INDEX','19950101','20130101'"); R_IDX=xts(R_IDX[,3],order.by=as.Date(R_IDX[,2]))

hfri = sqlQuery(pmwAnton,"exec [Crestline].[Risk_PortfolioReturns_GetBenchmarkReturns] 'HFRIEDI_INDEX','19950101','20130101'"); hfri = xts(hfri[,3],order.by=as.Date(hfri[,2]))


Q3 = q_separate(S_IDX,seq(0,1,l=4),na.omit(merge.xts(S_IDX,C_IDX,R_IDX)))
Q5 = q_separate(S_IDX,seq(0,1,l=6),na.omit(merge.xts(S_IDX,C_IDX,R_IDX)))
Q7 = q_separate(S_IDX,seq(0,1,l=8),na.omit(merge.xts(S_IDX,C_IDX,R_IDX)))





# // not used
if(1==0){
	model_norm_hist = function(test_arr,br_num){
		hist(rnorm(100000,mean(test_arr),sd(test_arr)),plot=FALSE,br=br_num)
	}
	ind = sqlQuery(pmwAnton,"exec [Crestline].[Risk_PortfolioReturns_GetBenchmarkReturns] 'HFRIEDI_INDEX','19950101','20130101'"); ind = xts(ind[,3],order.by=as.Date(ind[,2]))
	a=str1(s_idx,seq(0,1,0.25),c_idx)
	x_rng=c(); y_rng = c(); hs = lapply(a,function(x){hist(x,10,plot=FALSE)}); model_hs = lapply(a,function(x){model_norm_hist(as.numeric(x),10)}); for(i in 1:length(a)) {y_rng=c(y_rng,range(hs[[i]]$density),range(model_hs[[i]]$density)); x_rng=c(x_rng,range(hs[[i]]$mids),range(model_hs[[i]]$mids));}
	cols = rainbow(length(hs))
	plot_func = plot; for(i in 2:length(hs)){ plot_func(hs[[i]]$mids,hs[[i]]$density, col=cols[i], xlim=range(unlist(a)), ylim=range(y_rng), t='l', lwd=3); plot_func=lines; plot_func(model_hs[[i]]$mids,model_hs[[i]]$density, col=cols[i], xlim=range(unlist(a)), ylim=range(y_rng), t='l'); }




	res = unlist(foreach(i=1:10)%dopar%{foreach(j=1:300)%do%test1(f_s,f_b,f_cm,function(s,bms) bms%*%c(0.5,0.55,0.05),1)}); 
	res = unlist(foreach(i=1:4)%dopar%{foreach(j=1:3000)%do%test1(function(n) Q3$quantiles[2]+runif(n)*diff(Q3$quantiles)[2],f_b,f_cm,function(s,bms) bms%*%c(1,1,-1),1)}); 
	res = unlist(foreach(i=1:10)%dopar%{foreach(j=1:600)%do%test1(function(n) Q3$quantiles[1]+runif(n)*diff(Q3$quantiles)[1],f_b,f_cm,function(s,bms) bms%*%c(0.5,0.55,0.05),1)}); 

	h1 = hist(res,br=seq(-0.15,0.3,l=50), plot=FALSE)
	h2 = hist(val_separate(S_IDX,Q3$quantiles[1],Q3$quantiles[2],hfri),br=seq(-0.15,0.3,l=50), plot=FALSE); 
	plot(h1$mids,h1$density,t='l',lwd=2,col='red',ylim=range(h1$density,h2$density))
	lines(h2$mids,h2$density,t='l',lwd=2,col='blue',ylim=range(h1$density,h2$density))
	abline(h=0)

	plot(h1$mids,h1$counts/sum(h1$counts),t='l',lwd=2,col='red')
	lines(h2$mids,h2$counts/sum(h2$counts),t='l',lwd=2,col='blue')


	mc_anton_var_steps(hidden_mc_error_func,c(-0.5,-0.5,-0.5),1:3,array(0.1,3),1000,array(0.001,3))
	#0.843966675 -0.004529345 -0.648564761
	#c(0.1,0.1,0.5)    -     -0.5196497 -0.4279712  0.5089078
	#c(-0.5,-0.5,-0.5)    -    0.63743635  0.58187040 -0.01201581
	#c(0.5,0.5,0.5)         -      0.5899192  0.6580959 -0.0125547
	#
	#

	#ks.test(res,as.numeric(val_separate(S_IDX,-0.03,0.08,hfri)))
	#hq = q_separate(S_IDX,seq(0,1,l=4),na.omit(hfri))
	#hist(hq$series[[2]])
	#hist(val_separate(S_IDX,-0.01,0.3,hfri),br=20)

	c1 = 0.4; len = 20
	m = matrix(c1,len,len); diag(m)=1
	a=sign(ep(mu=0.09,rho=0.03,n=20,nRep=10)$y-0.5)

	a=rmvbin(10000,rep(0.5,len),bincorr=m)
	cor(a[,1],a[,2])

	a=sign(rmnorm(10000,rep(0,len),m))
	a=rmnorm(10000,rep(0,len),m)
	cor(a[,5],a[,12])

	s = as.numeric(S_IDX)
	plot(s)
	hist(s)
	


	getSymbols(c("USD3MTD156N","DSWP10","DGS10","SP500"),src='FRED'); 
	#s = get("DSWP10"); s=s[!is.na(s)] 
	snp = get("SP500"); snp=snp[!is.na(t)]; 
	t = get("DGS10"); t=t[!is.na(t)]; 
	l = get("USD3MTD156N"); l=l[!is.na(l)]; 
	#s=s[index(s)%in%index(t) & index(s)%in%index(l)]
	#t=t[index(t)%in%index(s) & index(t)%in%index(l)]
	#l=l[index(l)%in%index(t) & index(l)%in%index(s)]
	t0=t[index(t)%in%index(l)]
	l0=l[index(l)%in%index(t)]

	t0 = snp
	t = as.numeric(t0)*0.01; t = t[!is.na(t0)]
	len = length(index(t)) - 1

	ma30 = unlist(lapply(1:len,function(x) mean(t[max(1,x-30):x])))
	ma60 = unlist(lapply(1:len,function(x) mean(t[max(1,x-60):x])))
	signal = ma30 > ma60
	dt = diff(log(t))
	pos = array(0,len) 
	skip = 0
	for(i in 1:len) 
		if(skip > 0) skip = skip - 1 else {
			pos[i] = if(signal[i]) 1 else -1
			if(i > 5 && max(exp(cumsum(dt[(i-5):i]*pos[(i-5):i])) < 0.95))  # take min of returns -1 -2 -3 -4 -5
				skip = 5
		}
	pnl = xts(pos*(exp(dt)-1),order.by=index(t0)[!is.na(t0)][1:len])
	pnl_monthly = apply.monthly(pnl,function(x) exp(sum(log(1+x)))-1)
	hist(pnl_monthly,br=15)
	hist(pnl,br=50)
	

class(ret*sig)
rsi_monthly = apply.monthly(ret*sig,function(x) exp(sum(log(1+x)))-1)
hist(rsi_monthly,br=60)



library(quantmod)
library(TTR)
library(gamlss)


getSymbols("^GSPC", from="2000-01-01", to="2014-01-20")
rsi <- RSI(Cl(GSPC))
sigup <- ifelse(rsi < 30, 1, 0)
sigdn <- ifelse(rsi > 70, -1, 0)
sigup <- lag(sigup,1) # Note k=1 implies a move *forward*
sigdn <- lag(sigdn,1) # Note k=1 implies a move *forward*
sigup[is.na(sigup)] <- 0
sigdn[is.na(sigdn)] <- 0
sig <- sigup + sigdn
ret <- ROC(Cl(GSPC))
ret[1] <- 0
eq_all <- exp(cumsum(ret*sig))



library(quantmod)
library(TTR)
library(ghyp) # library(gamlss)
library(sn)

y10 = read.csv('/mnt/G/RISK/R-OUTPUT/10Y_TRSY.csv')[,1:3]; y10 = xts(y10[,2:3],order.by=as.Date(y10[,1])); 
colnames(y10) = c('p','y')
y2 = read.csv('/mnt/G/RISK/R-OUTPUT/2Y_TRSY.csv')[,1:3]; y2 = xts(y2[,2:3],order.by=as.Date(y2[,1])); 
colnames(y2) = c('p','y')
y2 = y2[index(y2)%in%index(y10)]; y10 = y10[index(y10)%in%index(y2)]; 

d = as.numeric(y10$y - y2$y)
## h = hist(diff(d),br=300,plot=FALSE); plot(h$mids,h$density,t='l',xlim=c(-0.3,0.3))
dmon = apply.monthly(diff(y10$y - y2$y), sum)
h = hist(dmon,br=50,plot=FALSE); plot(h$mids,h$density,t='l',xlim=c(-2,3))

##
quantile_to_avg = unlist(lapply(1:length(d),function(x) mean(d[x] >= d[max(1,x-500):max(1,x-1)])))

sigup <- ifelse(quantile_to_avg < 0.4, 1, 0)
sigdn <- ifelse(quantile_to_avg > 0.6, -1, 0)
sigup[is.na(sigup)] <- 0
sigdn[is.na(sigdn)] <- 0

sig <- sigup + sigdn
ret <- diff(4*y2$p - y10$p) 
ret[1] <- 0
pnl = xts(ret*sig,order.by=index(y2))
pnl_monthly = apply.monthly(pnl, sum)
#hist(pnl_monthly,br=40)

#a = fitDist(as.numeric(pnl_monthly),type='realline')
#pdf.plot(a,min=-30,max=10,step=0.1)
#h_data = hist(pnl_monthly,br=40,plot=FALSE)
#lines(h_data$mids, h_data$density, t='p')

r = as.numeric(pnl_monthly)
ghyp_r = fit.ghypuv(r)
fit_r = rghyp(100000,ghyp_r)

hr = hist(r,br=100,plot=FALSE)
hfr = hist(fit_r,br=1000,plot=FALSE)

plot(hfr$mids,hfr$density,t='l',col='blue',xlim=c(-40,10))
plot(hfr$mids,hfr$density,t='l',col='blue',xlim=c(-0.1,0.2))
lines(hr$mids,hr$density,t='l',col='black')



skew_normal = function(n,rfr,s,a) {
	r = rmsn(n,0,s,a)
	d = density(r)
	r - d$x[which.max(d$y)] + rfr
}

# parameters [s,e] are measures in rfr's
skew_normal_stud = function(n,rfr,w0,skew0,s0,e1,s1) {
	r0 = skew_normal(n,rfr,(s0*rfr)^2,skew0)
	r_tail = rnorm(n,e1*rfr,(s1*rfr)^2)
	ifelse(runif(n) < w0, r0, r_tail)
}


r = 0.7*skew_normal_stud(100000,0.01,0.9,2,2,6,10) + 0.3*skew_normal_stud(100000,0.01,0.9,0,1,-20,20)
hist(r,br=100)


hist(rmsn(10000,0,0.001^2,2),br=40)

t0 = y10$p
t = as.numeric(t0)*0.01; t = t[!is.na(t0)]
len = length(index(t)) - 1

ma1 = unlist(lapply(1:len,function(x) mean(t[max(1,x-20):x])))
ma2 = unlist(lapply(1:len,function(x) mean(t[max(1,x-60):x])))
signal = ma1 > ma2
dt = diff(log(t))
pos = array(0,len) 
skip = 0
for(i in 1:len) 
	if(skip > 0) skip = skip - 1 else {
		pos[i] = if(signal[i]) 1 else -1
		if(i > 5 && max(exp(cumsum(rev(dt[(i-5):i]*pos[(i-5):i]))) < 0.99))  # take min of returns -1 -2 -3 -4 -5
			skip = 5
	}
pnl = xts(pos*(exp(dt)-1),order.by=index(t0)[!is.na(t0)][1:len])
pnl_monthly = apply.monthly(pnl,function(x) exp(sum(log(1+x)))-1)
pnl_monthly = pnl_monthly[abs(pnl_monthly)<0.2]
hist(pnl_monthly,br=50)

r = as.numeric(pnl_monthly)
hr = hist(r,br=10,plot=FALSE)
plot(hr$mids,hr$density,t='l',col='black')




library(sn)
f = mst.fit(y=r, plot.it=FALSE)
a = fitDist(as.numeric(pnl_monthly),type='realline')
pdf.plot(a,min=-0.5,max=1,step=0.001)
h_data = hist(pnl_monthly,br=100,plot=FALSE)
lines(h_data$mids, h_data$density, t='p')







y10n = as.numeric(y10$p)
rsi <- RSI(y10n)
sigup <- ifelse(rsi < 40, 1, 0)
sigdn <- ifelse(rsi > 60, -1, 0)
#sigup <- lag(sigup,1) # Note k=1 implies a move *forward*
#sigdn <- lag(sigdn,1) # Note k=1 implies a move *forward*
sigup[is.na(sigup)] <- 0
sigdn[is.na(sigdn)] <- 0
sig <- sigup + sigdn
ret <- diff(y10n)
ret[1] <- 0
pnl = xts(ret*sig,order.by=index(y2))
pnl_monthly = apply.monthly(pnl, sum)
hist(pnl_monthly,br=50)






	signal = as.numeric((diff(l)>0))[2:(len-1)]
	trades = as.numeric(diff(l)[3:len])
	trades[!signal]=-trades[!signal]

	hist(trades,br=seq(-1,1,l=100),xlim=c(-0.5,0.5),ylim=c(0,4000))  # same as below?
	#hist(diff(s),br=seq(-0.5,0.5,l=200))
	
	trades = ((l-t)/250+diff(t)-diff(l))[2:len]; h=hist(trades,br=seq(-1,1,l=200),plot=FALSE)
	trades_rate = (l-t)[2:len]/250; h2=hist(trades_rate,br=seq(-1,1,l=200),plot=FALSE)
	trades_market = (diff(l)-diff(t))[2:len]; h3=hist(trades_market,br=seq(-1,1,l=200),plot=FALSE)
	plot(h$mids,h$density,t='l',col='blue')
	plot(h2$mids,h2$density,t='l',col='red')
	plot(h3$mids,h3$density,t='l',col='orange')

hist(s/250-diff(s),br=100)
	




# We will need the quantmod package for charting and pulling
# data and the TTR package to calculate RSI(2).
# You can install packages via: install.packages("packageName")
# install.packages(c("quantmod","TTR"))
library(quantmod)
library(TTR)

# Pull S&P500 index data from Yahoo! Finance
getSymbols("^GSPC", from="2000-01-01", to="2014-01-20")

# Calculate the RSI indicator
rsi <- RSI(Cl(GSPC))

# Create the long (up) and short (dn) signals
sigup <- ifelse(rsi < 30, 1, 0)
sigdn <- ifelse(rsi > 70, -1, 0)

# Lag signals to align with days in market,
# not days signals were generated
#sigup <- Lag(sigup,1) # Use lag() to avoid Toby's error
#sigdn <- Lag(sigdn,1) # Use lag() to avoid Toby's error
sigup <- lag(sigup,1) # Note k=1 implies a move *forward*
sigdn <- lag(sigdn,1) # Note k=1 implies a move *forward*

# Replace missing signals with no position
# (generally just at beginning of series)
sigup[is.na(sigup)] <- 0
sigdn[is.na(sigdn)] <- 0

# Combine both signals into one vector
sig <- sigup + sigdn

# Calculate Close-to-Close returns
ret <- ROC(Cl(GSPC))
ret[1] <- 0

# Calculate equity curves
eq_up <- exp(cumsum(ret*sigup))
eq_dn <- exp(cumsum(ret*sigdn*-1))
eq_all <- exp(cumsum(ret*sig))

# Replicate Michael's nice chart
plot.zoo( cbind(eq_up, eq_dn),
ylab=c("Long","Short"), col=c("green","red"),
main="" )

# Wait a few seconds before making next chart...
#Sys.sleep(5)

# Create a chart showing the S&P500
#chartSeries(GSPC, type="line")

# Add the total equity line
#addTA(eq_all)






}
# \\ not used





###  state_gen - state value generator (function(x/*runif(1)*/,n)); if result has length>1, then state is dynamic 
###  benchmarks_gen - generates matrix of correlated benchmark series (function(s/*state value*/,corr_matrix/*correlation matrix*/,n/*number of steps*/))
###  corr_matrix_func - matrix of correlations between benchmarks, as function of "s"
###  index_gen - resulting index generator (function(s,bms/*benchmark values matrix*/)
###  n - length of time series
# state_gen=f_s; benchmarks_gen=f_b; corr_matrix_func=f_cm; index_gen=f_ig; n=1000
test1 = function(state_gen,benchmarks_gen,corr_matrix_func,index_gen,n){
	s = state_gen(n)

	res = array(0,0)
	if(length(s) == 1)
		res = index_gen(s, benchmarks_gen(s, corr_matrix_func(s), n))
	else
		for(i in 1:length(s)) res = c(res, index_gen(s[i], benchmarks_gen(s[i], corr_matrix_func(s[i]), 1)))
	res
}

f_s = function(n){ 
	Q3$quantiles[2] + runif(n)*diff(Q3$quantiles)[2]
}

f_b = function(s, corr_matrix, n){
	q_idx = max(which(Q3$quantiles <= s))
	BM_Es = Q3$means[[q_idx]]
	BM_SDs = Q3$sds[[q_idx]]
	t(t(rmnorm(n, c(0,0,0), corr_matrix))*BM_SDs + BM_Es)
}

f_cm = function(s){
	limit_m = matrix(c(1,1,-1,1,1,-1,-1,-1,1),3,3)
	m = Q3$corr_matrices[[2]]
	qs = Q3$quantiles 
	q_idx = max(which(qs <= s))

	if(q_idx == 2) 
		m else
	if(q_idx < 2) 
		m + (limit_m - m)*(qs[2] - s)/(qs[2] - qs[1]) else
		m + (limit_m - m)*(s - qs[3])/(qs[4] - qs[3])
}

f_ig_MergerArb = function(s, bms){  
	comb_x = function(s,c,r) (0.5 - s/2)*(1 + r - c)
	p0 = function(s,c,r) F_step(0.05,0.5,0.9)(comb_x(s,c,r))
	x1 = function(s,c,r) F_step(-1,0.4,0.8)(comb_x(s,c,r))
	x2 = function(s,c,r) x1(s,c,r) + 0.2*comb_x(s,c,r)
	fail_corr = function(s,c,r) F_step(0,0.5,1)(comb_x(s,c,r))

	# portfolio bucket composition:  c(good, medium, bad)
	quality_shifts = c(0,-0.1,-0.2)
	weights = c(0.333,0.333,0.333)
	spreads = c(0.04,0.06,0.09)
	ttes = c(3,6,6)
	amounts = c(10,10,10)
	recovery = c(-0.10,-0.15,-0.20)
	
	revenues = list()
	for(i in 1:length(weights)){
		s_adj = s - quality_shifts[i]		
		tmp_revs = array(weights[i]/amounts[i], amounts[i])

		# suppose that bms[] = c(e, c, r)
		fail_probab = G_step(p0(s_adj,bms[2],bms[3]),x1(s_adj,bms[2],bms[3]),x2(s_adj,bms[2],bms[3]))(bms[1])
		fail_idxs = sign(ep(mu=1-fail_probab,rho=fail_corr(s_adj,bms[2],bms[3]),n=amounts[i],nRep=1)$y-0.5)==-1  #fail_idxs = runif(amounts[i]) < fail_probab
		tmp_revs[fail_idxs] = tmp_revs[fail_idxs]*recovery[i]
		tmp_revs[!fail_idxs] = tmp_revs[!fail_idxs]*spreads[i]/ttes[i]

		revenues = c(revenues, list(tmp_revs))
	}
	
	return(sum(unlist(revenues)))
}


#spreads = c(0.04,0.06,0.09)
#ttes = c(3,6,6)
#price_corr = function(s,c,r) F_step(0,0.5,1)(comb_x(s,c,r))

f_ig_Distressed = function(s, bms){  
	comb_x = function(s,c,r) (0.5 - s/2)*(1 + r - c)
	p0 = function(s,c,r) F_step(0.05,0.5,0.9)(comb_x(s,c,r))
	x1 = function(s,c,r) F_step(-1,0.4,0.8)(comb_x(s,c,r))
	x2 = function(s,c,r) x1(s,c,r) + 0.2*comb_x(s,c,r)
	default_corr = function(s,c,r) F_step(0,0.5,1)(comb_x(s,c,r))

	# portfolio bucket composition:  c(good, medium, bad)
	quality_shifts = c(0,-0.1,-0.2)
	weights = c(0.333,0.333,0.333)
	tranche_weights = matrix(0.333,3,3) # columns are buckets, rows are tranches (senor/junior/equity), cell value is tranche weight in the bucket
	amounts = c(10,10,10)
	recovery = matrix(c(0.8,0.8,0,0.4,0.4,0,0.05,0.05,0),3,3)
	
	revenues = list()
	for(i in 1:length(weights)){
		s_adj = s - quality_shifts[i]		
		tmp_revs = array(weights[i]/amounts[i], amounts[i])

		# suppose that bms[] = c(e, c, r)
		fail_probab = G_step(p0(s_adj,bms[2],bms[3]),x1(s_adj,bms[2],bms[3]),x2(s_adj,bms[2],bms[3]))(bms[1])
		fail_idxs = sign(ep(mu=1-fail_probab,rho=default_corr(s_adj,bms[2],bms[3]),n=amounts[i],nRep=1)$y-0.5)==-1  

		tmp_revs[fail_idxs] = tmp_revs[fail_idxs]*(sum(recovery[,i]*tranche_weights[,i])-1)
		tmp_revs[!fail_idxs] = tmp_revs[!fail_idxs]*sum(tranche_weights[,i]*c(bms[2],bms[2],bms[1]))
  
		revenues = c(revenues, list(tmp_revs))
	}
	
	return(sum(unlist(revenues)))
}

# s=0.06; bms=c(0.06,0,0)
f_ig_LongShort = function(s, bms){  
	qs = Q3$quantiles
	
	BETA_GOOD_E = 0.6; BETA_GOOD_3S = 0.1;
	BETA_BAD_E = -0.6; BETA_BAD_3S = 0.1;
	ALPHA_E = 0.02; ALPHA_S = 0.02

	comb_x = function(s,c,r) s #(0.5 - s/2)*(1 + r - c)   	
	bad_distrib_prob = function(s,c,r) G_step(0,-1,Q3$quantiles[2])(comb_x(s,c,r))/2
	alpha_skewness = function(s,c,r) F_step(-1,Q3$quantiles[2],Q3$quantiles[3])(comb_x(s,c,r))*2
	
	b_d_p = bad_distrib_prob(s,bms[2],bms[3])
	a_s = alpha_skewness(s,bms[2],bms[3])

	res_beta = s * if(runif(1) < b_d_p) rnorm(1,mean=BETA_BAD_E,sd=BETA_BAD_3S/3) else rnorm(1,mean=BETA_GOOD_E,sd=BETA_GOOD_3S/3)
	res_alpha = ALPHA_E + ALPHA_S*rmsn(1,Omega=1,alpha=a_s)

	return(res_beta + res_alpha)
}


f_ig_CTA = function(s, bms){
	STOP_LOSS_MIN = -0.03; STOP_LOSS_MAX = -0.02; STOP_LOSS_STRENGTH = 0.5
	DIRECTION = 1 # 1 for Long, -1 for Short

	stop_loss = runif(1, STOP_LOSS_MIN, STOP_LOSS_MAX)
	#s_rnd = rnorm(1, s, abs(s)*TRADE_SIGMA)
	#s_rnd = sample(F_IG_SAMPLE, 1)
	#s_rnd = rmsn(1, C_PARAMS['xi'], Omega=C_PARAMS['omega'], alpha=C_PARAMS['alpha'], tau=C_PARAMS['tau'])
	s_rnd = rnorm(1, C_PARAMS['condMean'], C_PARAMS['condVar'])
	
	#print(paste('s_rnd: ',s_rnd))
	#print(paste('stop_loss: ',stop_loss))
	if(s_rnd > stop_loss || runif(1) > STOP_LOSS_STRENGTH)
		s_rnd
	else
		stop_loss*(1 + (1-STOP_LOSS_STRENGTH)*rexp(1,2/abs(s_rnd)))
}

condNormal <- function(x.given, mu, sigma, given.ind, req.ind){ 
	# Returns conditional mean and variance of x[req.ind] 
	# Given x[given.ind] = x.given 
	# where X is multivariate Normal with 
	# mean = mu and covariance = sigma 

	B <- sigma[req.ind, req.ind] 
	C <- sigma[req.ind, given.ind, drop=FALSE] 
	D <- sigma[given.ind, given.ind] 
	CDinv <- C %*% solve(D) 
	cMu <- c(mu[req.ind] + CDinv %*% (x.given - mu[given.ind])) 
	cVar <- B - CDinv %*% t(C) 
	list(condMean=cMu, condVar=cVar) 
} 


f_ig_YIELD = function(s, bms){
	CENTER_E = 0; CENTER_SD = 0.002
	TAIL_PROB = 0.2
	TAIL_DISTANCE = 0.015; TAIL_SD = 4*CENTER_SD
	tail_try = runif(1)
	if(tail_try < TAIL_PROB) rnorm(1,CENTER_E+TAIL_DISTANCE*if(tail_try < TAIL_PROB*0.5) 1 else -1,TAIL_SD) else rnorm(1,CENTER_E,CENTER_SD)	
}


f_ig_FI = function(s, bms){
	MOMENTUM_ALLOC = 0.7
	MOMENTUM_ALLOC*f_ig_CTA(0,0) + (1 - MOMENTUM_ALLOC)*f_ig_YIELD(0,0)
}

# Used for Convertible Arb
f_ig_SPREAD = function(s, bms){
	DURATION = 1
	LEVERAGE = 2
	SPREAD_E = 0.16 
	SPREAD_SD = 0.02; NO_RISE_E = 0.2; NO_DROP_E = 0.01
	MARGIN_CALL = -0.25; MARGIN_WARNING = -0.2
	MG_LEVERAGE = 2
	
	pnl = LEVERAGE*DURATION*rnorm(1,0,SPREAD_SD)
	pnl = -if(runif(1) > (SPREAD_E + NO_DROP_E)/(NO_RISE_E - NO_DROP_E)) abs(pnl) else -abs(pnl)
	SPREAD_E/12 + if(pnl > MARGIN_WARNING) pnl else 
			if(pnl < MARGIN_WARNING && pnl > MARGIN_CALL) pnl*(1+(MG_LEVERAGE-1)*(MARGIN_WARNING-pnl)/(MARGIN_WARNING-MARGIN_CALL)) else pnl*MG_LEVERAGE
	#pnl = rnorm(1,0,SPREAD_SD)
	#pnl = SPREAD_E + if(runif(1) > (SPREAD_E + NO_DROP_E)/(NO_RISE_E - NO_DROP_E)) abs(pnl) else -abs(pnl)
}
# sp = unlist(foreach(i=1:200)%dopar%{ foreach(j=1:100)%do%f_ig_SPREAD(0,0) }); hist(sp,br=100)
# plot(ecdf((unlist(foreach(i=1:200)%dopar%{ foreach(j=1:100)%do%f_ig_SPREAD(0,0) }))))

if(FALSE){
	## CTA ##
	S_E = 0; S_SD = 0.05
	Es = c(0,0,0,0)
	SDs = 0.01*c(5,5,5,5)	
	CORRs = c(0.5,0.5,0.5,0.5)
	S_VAL = 0

	cta = foreach(i=length(SDs), .combine='+')%do%{
		A <- sqrt(make.positive.definite(cor2cov(matrix(c(1,CORRs[i],CORRs[i],1),2), c(S_SD, SDs[i])), tol=0.00001))
		C_PARAMS = unlist(condNormal(x.given=S_VAL, mu=c(S_E,Es[i]), sigma=A, given=1, req=2))
		unlist(foreach(i=1:100)%dopar%{ foreach(j=1:300)%do%f_ig_CTA(S_VAL,0)})
	}
	#omega = make.positive.definite(cor2cov(matrix(c(1,CORRs[i],CORRs[i],1),2), c(S_SD, SDs[i])), tol=0.00001)
	#sn <- makeSECdistr(dp=list(x=c(S_E,C_E), Omega=0.5*(omega+t(omega)), alpha=c(0,0)), family="SN"); #plot(sn)

	#plot(density(unlist(foreach(i=1:100)%dopar%{ foreach(j=1:300)%do%f_ig_CTA(S_VAL,0)}),br=1000))
	hist(cta,br=100)
	plot(ecdf(cta))

	#h1 = hist(rmsn(100000, C_PARAMS['xi'], Omega=C_PARAMS['omega'], alpha=C_PARAMS['alpha'], tau=C_PARAMS['tau']),br=30,plot=FALSE)
	h1 = hist(rnorm(100000,C_E,C_SD),br=30,plot=FALSE)
	h2 = hist(rnorm(100000,C_PARAMS['condMean'],C_PARAMS['condVar']),br=30,plot=FALSE)
	#h2 = hist(rnorm(100000,C_PARAMS['condMean'],sqrt(C_PARAMS['condVar'])),br=30,plot=FALSE)
	plot(h1$mids,h1$density,t='l',col='blue')
	lines(h2$mids,h2$density,t='l',col='red')
	## -- CTA ##


	## FI ##
	
	
	S_E = 0; S_SD = 0.05
	T_E = 0; T_SD = 0.02	
	CORR = 0.9
	S_VAL = 0

	A <- sqrt(make.positive.definite(cor2cov(matrix(c(1,CORR,CORR,1),2), c(S_SD, T_SD)), tol=0.00001))
	C_PARAMS = unlist(condNormal(x.given=S_VAL, mu=c(S_E,T_E), sigma=A, given=1, req=2))
	fi = unlist(foreach(i=1:50)%dopar%{ foreach(j=1:300)%do%f_ig_FI(S_VAL,0)})
	hist(fi,br=100)



	hist(unlist(foreach(i=1:100)%dopar%{ foreach(j=1:100)%do%f_ig_YIELD(0,0) }),br=100)

	hist(unlist(foreach(i=1:500)%dopar%{ foreach(j=1:100)%do%f_ig_SPREAD(0,0) }),br=300)

	r=rnorm(100000,0,1); rs=runif(100000); nrs=0.9; hist(ifelse(rs>nrs,r,abs(r)),br=100)

	## -- FI ##






	#A <- A %*% t(A) 
	condNormal(x.given=S_VAL, mu=c(S_E,C_E), sigma=A, given=1, req=2) 

}

# error function, unsed for Monte Carlo simulation
hidden_regime_mc_error_func = function(p1,p2,p3){
	f_ig_tmp = function(s,bms) bms%*%c(p1,p2,p3)
	res = unlist(foreach(i=1:4)%dopar%{foreach(j=1:5000)%do%test1(f_s,f_b,f_cm,f_ig_tmp,1)})
	h1 = hist(res,br=seq(-0.15,0.3,l=50), plot=FALSE)
	h2 = hist(val_separate(S_IDX,Q3$quantiles[2],Q3$quantiles[3],hfri),br=seq(-0.15,0.3,l=50), plot=FALSE); 
	sum((h1$density-h2$density)^2)
}

#s=Q3$quantiles[1] + runif(1)*diff(Q3$quantiles)[1]; cm=f_cm(s); b=f_b(s,cm,1); f_ig(s,b); bms=b
#res = unlist(foreach(i=1:10)%dopar%{foreach(j=1:3000)%do%test1(f_s,f_b,f_cm,f_ig,1)}); 

res = unlist(foreach(i=1:10)%dopar%{foreach(j=1:200)%do%test1(function(n) Q3$quantiles[1]+runif(n)*diff(Q3$quantiles)[1],f_b,f_cm,f_ig_MergerArb,1)}); 
res = unlist(foreach(i=1:10)%dopar%{foreach(j=1:200)%do%test1(function(n) Q3$quantiles[1]+runif(n)*diff(Q3$quantiles)[1],f_b,f_cm,f_ig_Distressed,1)}); 
res = unlist(foreach(i=1:30)%dopar%{foreach(j=1:200)%do%test1(function(n) Q3$quantiles[2]+runif(n)*diff(Q3$quantiles)[2],f_b,f_cm,f_ig_LongShort,1)}); 
hist(res,br=seq(-0.3,0.3,by=0.005))


















