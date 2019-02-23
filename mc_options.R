library(mnormt)
library(doMC) 
#library(bindata)
library(mvtBinaryEP)
library(cubature)
library(rgl)
registerDoMC(8);
source('/var/www/R/lib/mc_lib.R')


option_price_simple = function(spot,strike,option_type) {
	if((option_type=="Call" && spot>strike) || (option_type=="Put" && spot<strike)) abs(spot-strike) else 0 
}

# spot1 can be array  OR  spot2 can be array;  both can be numbers
option_price_compound = function(spot1,strike1,type1,spot2,strike2,type2) {
	if((length(spot1)>1 && length(spot2)==1) || (length(spot1)==1 && length(spot2)>=1) || (length(spot1)==1 && length(spot2)==1)) {
		res = array(0,length(spot1));
		idx = ((type1=="Call" & spot1>strike1) | (type1=="Put" & spot1<strike1)) & ((type2=="Higher" & spot2>strike2) | (type2=="Lower" & spot2<strike2))
		res[idx] = abs(spot1-strike1)[idx]
		res
	} else 
		NA
}


# option_type = "Call" || "Put"
mc_option_simple = function(curr_spot,mean_return,vol,strike,ttm,r,option_type,calc_size) {
	n = 5; m = calc_size;
	prices = matrix(rnorm(n*m*ttm, mean=mean_return, sd=vol), ttm)
	exp(-r*ttm) * mean(unlist(foreach(i = 1:(n-1)) %dopar% {
					foreach(j = 1:m) %do% 
						option_price_simple(curr_spot * exp(sum(prices[,i*m+j])), strike, option_type)}))
	
}


# type1 = "Call" || "Put",  type2 = "Higher" || "Lower"
mc_option_bivariate = function(curr_spot1,mean_return1,vol1,strike1,type1,curr_spot2,mean_return2,vol2,strike2,type2,ttm,r,corr,calc_size) {
	n = 5; m = calc_size;

	corr_vals = t(t(rmnorm(n*m*ttm,c(0,0),matrix(c(1,corr,corr,1),2)))*c(vol1,vol2) + c(mean_return1,mean_return2))

	prices1 = matrix(corr_vals[,1],ttm)
	prices2 = matrix(corr_vals[,2],ttm)

	exp(-r*ttm) * mean(unlist(foreach(i = 1:(n-1)) %dopar% {
					foreach(j = 1:m) %do% 
						option_price_compound(curr_spot1 * exp(sum(prices1[,i*m+j])), strike1, type1,
									curr_spot2 * exp(sum(prices2[,i*m+j])), strike2, type2)}))
}

# calculate bivariate normal values on the grid; length[e, s]=2
calc_bvn_grid = function(e,s,corr_in,grid_x,grid_y) {
	corr = if(corr_in==1) 0.9999999 else if(corr_in==0) 0.0000001 else corr_in
	rs = 1/s
	sq = function(x) {x^2}
	lx = length(grid_x); ly = length(grid_y); res = array(0,lx*ly)
	for(i in 1:ly) {
		y = grid_y[i]
		res[(i*lx-lx+1):(i*lx)] = 
			rs[1]*rs[2]*exp(-0.5*(sq(grid_x-e[1])*sq(rs[1]) + sq(y-e[2])*sq(rs[2]) - 2*corr*rs[1]*rs[2]*(grid_x-e[1])*(y-e[2])) / (1-corr^2))/(2*pi*sqrt(1-corr^2))
	}
	res
}

calc_option_bivariate = function(s1,e1,v1,x1,type1,s2,e2,v2,x2,type2,ttm,r,corr,calc_size) {
	min1 = x1/s1-1;	max1 = x1/s1-1; min2 = x2/s2-1; max2 = x2/s2-1
	if(type1=='Call') max1 = max1 + 10*sqrt(ttm)*v1 else min1 = min1-10*sqrt(ttm)*v1 
	if(type2=='Higher') max2 = max2 + 10*sqrt(ttm)*v2 else min2 = min2-10*sqrt(ttm)*v2
	grid1 = seq(min1,max1,len=calc_size); d1=grid1[2]-grid1[1] 
	grid2 = seq(min2,max2,len=calc_size); d2=grid2[2]-grid2[1] 
	l1 = length(grid1); l2 = length(grid2); opt_prc = array(0,l1*l2)
	for(i in 1:l2) opt_prc[(i*l1-l1+1):(i*l1)] = option_price_compound(s1*exp(grid1),x1,type1,s2*exp(grid2[i]),x2,type2)
	probs = calc_bvn_grid(exp(ttm*c(e1,e2))-1, sqrt(ttm)*c(v1,v2), corr, grid1, grid2)

	sum(probs*opt_prc*d1*d2)
}



calc_option_bivariate_getImpliedCorr = function(s1,e1,v1,x1,type1,s2,e2,v2,x2,type2,ttm,r,calc_size_Option,option_price) {
	#mc_anton_var_steps = function(func,params_in,var_idxs,var_steps,steps_lim,var_steps_min)
	opt_error = function(corr_in) {abs(calc_option_bivariate(s1,e1,v1,x1,type1,s2,e2,v2,x2,type2,ttm,r,corr_in,calc_size_Option)-option_price)}
	#print(option_price)
	optim(0.001,opt_error,method='Brent',lower=-1,upper=1)$par
}





   

#  \/\/\/  DEBUG  \/\/\/

if(1==0){

	# calc_option_bivariate = function(s1,e1,v1,x1,type1,s2,e2,v2,x2,type2,ttm,r,corr,calc_size)
	calc_option_bivariate_getImpliedCorr(100,0.019/250,0.2598/sqrt(250),95,'Put',99.14,0,0.14194/sqrt(250),97.8,'Higher',260/360*250,0.0023/250,200,1.25)
        calc_option_bivariate_getImpliedCorr(100,0.019/250,0.3/sqrt(250),95,'Put',99.14,0,0.1/sqrt(250),97.8,'Higher',260/360*250,0.0023/250,200,1.25)

	calc_option_bivariate_getImpliedCorr(100,0.019/250,0.2912/sqrt(250),105,'Call',99.14,0,0.14194/sqrt(250),98,'Lower',260/360*250,0.0023/250,200,1.19)



	calc_option_bivariate(100,0,0.2912/sqrt(250),105,'Call',99.14,0,0.14194/sqrt(250),98,'Lower',260,(0.0023-0.019)/250,1,200)



	mc_option_simple(curr_spot=100, mean_return=1.05^(1/365)-1, vol=1.20^(1/365)-1, strike=105, ttm=300, r=1.02^(1/250)-1, option_type='Call')
	mc_option_simple(curr_spot=100, mean_return=1.05^(1/365)-1, vol=1.20^(1/365)-1, strike=105, ttm=300, r=1.02^(1/250)-1, option_type='Put')

	mc_option_simple(curr_spot1=100, mean_return1=1.05^(1/365)-1, vol1=1.20^(1/365)-1, strike1=105, type1='Call', 
			 curr_spot2=100, mean_return2=1.05^(1/365)-1, vol2=1.20^(1/365)-1, strike2=105, type2='Higher', ttm=300, r=1.02^(1/250)-1,0.5)

	calc_option_bivariate(100, 1.05^(1/365)-1, 1.20^(1/365)-1, 105, 'Call', 100, 1.05^(1/365)-1, 1.20^(1/365)-1, 105, 'Higher', ttm=300, r=1.02^(1/250)-1,0.5)
	mc_option_bivariate(100, 1.05^(1/365)-1, 1.20^(1/365)-1, 105, 'Call', 100, 1.05^(1/365)-1, 1.20^(1/365)-1, 105, 'Higher', ttm=300, r=1.02^(1/250)-1,0.5)


	mc_option_simple(100, 1.05^(1/365)-1, 1.20^(1/365)-1, 105, 'Lower', 100, 1.05^(1/365)-1, 1.20^(1/365)-1, 105, 'Higher', ttm=300, r=1.02^(1/250)-1,0.5)
	mc_option_simple(100, 1.05^(1/365)-1, 1.20^(1/365)-1, 105, 'Lower', 100, 1.05^(1/365)-1, 1.20^(1/365)-1, 105, 'Lower', ttm=300, r=1.02^(1/250)-1,0.5)



	#s1=100;e1=0;v1=0.2912/sqrt(250);x1=105;type1='Call';s2=99.14;e2=0;v2=0.14194/sqrt(250);x2=98;type2='Lower';ttm=260;r=(0.0023-0.019)/250;corr=1;calc_size=200
	mc_option_bivariate(s1,e1,v1,x1,type1,s2,e2,v2,x2,type2,ttm,r,corr,20000)
	calc_option_bivariate(s1,e1,v1,x1,type1,s2,e2,v2,x2,type2,ttm,r,corr,200)
	plot(0.01+(1:40)*0.1/40,unlist(foreach(i=1:40)%dopar%calc_option_bivariate_getImpliedCorr(s1,e1,v1,x1,type1,s2,e2,v2,x2,type2,ttm,r,300,0.01+i*0.1/40)))

	
	#xs = array(0,l1*l2); ys = array(0,l1*l2)
	#for(i in 1:l2) {
	#	xs[(i*l1-l1+1):(i*l1)] = grid1
	#	ys[(i*l1-l1+1):(i*l1)] = grid2[i]
	#}
	#plot3d(xs,ys,probs)

	for(l in seq(20,400,by=10)){
		#l=50

		grid1 = seq(0,0.07,len=l); d1=grid1[2]-grid1[1]
		grid2 = seq(0,0.07,len=l); d2=grid2[2]-grid2[1]

		grid1 = seq(x1/s1-1,x1/s1-1+10*sqrt(ttm)*v1,len=l); d1=grid1[2]-grid1[1]
		grid2 = seq(x2/s2-1,x2/s2-1+10*sqrt(ttm)*v2,len=l); d2=grid2[2]-grid2[1]

		l1 = length(grid1); l2 = length(grid2); xs = array(0,l1*l2); ys = array(0,l1*l2)
		for(i in 1:l2) {
			xs[(i*l1-l1+1):(i*l1)] = grid1
			ys[(i*l1-l1+1):(i*l1)] = grid2[i]
			
		}
		probs = calc_bvn_grid(exp(ttm*c(e1,e2))-1,sqrt(ttm)*c(v1,v2),corr,grid1,grid2)
		#print(sum(probs*d1*d2))
		plot3d(xs,ys,probs)
	}
}



















