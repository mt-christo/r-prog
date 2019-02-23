
get_spread = function(x1_in,x2_in){
	x1 = x1_in; x2 = x2_in
	i2 = which.min(x2); i1 = which(x1>x2[i2])
	while(length(i1)>0){
		x1 = x1[-i1[1]]; x2 = x2[-i2]
		i2 = which.min(x2); i1 = which(x1>x2[i2])
	}
	min(x2) - max(x1)
}

K = 5000; ts1 = rnorm(K,0,1); ts2=rnorm(K,0,1)
res = array(0,0)
for(i in 1:50){
	res = c(res,get_spread(ts1,ts2))
	ts1 = c(ts1[-1],rnorm(1,0,1)); ts2 = c(ts2[-1],rnorm(1,0,1))
}

plot(res)
