library(hash)

## Environment ##

#Agents parameters
N=1000; 
agents_params = hash(fund_cf=0.1; trend_cma=0.5; trend_ct=2; corc_c0v=0.3; corc_c1v=3; part_fund=0.3; part_trend=0.4; part_corc=0.3)
agents = hash(types=array(0,N), pos=array(0,N), cash=array(0,N))   #type=1 - fundamental, type=2 - trend, type=3 - correction; fund_cf - relational to starting CASH

#Market parameters
market_params = hash(fund_prc0=10; fund_sigB=0.1; fund_cj=0.5; fund_lamp=0.1; fund_lamm=0.1)
tmp_pf0 = market_params$fund_prc0   #just to make it shorter.. \/\/\/
market = hash(p_real = hash(p=tmp_pf0; p_ts=array(tmp_pf0,1); p_log=log(tmp_pf0); p_log_ts=array(log(tmp_pf0),1));
		p_fund = hash(p=tmp_pf0; p_ts=array(tmp_pf0,1); p_log=log(tmp_pf0); p_log_ts=array(log(tmp_pf0),1)))

#Experiment params
TOTAL_STEPS = 10000

## ##



#Code

agents$types[1:(part_fund*N)]=1; agents$types[(part_fund*N)+(1:(part_trend*N))]=2; agents$types[(part_fund*N)+(part_trend*N)+(1:(part_corc*N))]=3;

for(t in 1:TOTAL_STEPS){
	p_fund = p_fund_Increment(p_fund,t)

	



}





p_fund_Increment = function(p_fund_in, t){
	res = p_fund_in
	new_log_p = ...
	res$p_log = new_log_p
	res$p_log_ts = c(res$p_log_ts,new_log_p)
	res$p = exp(new_log_p)
	res$p_ts = c(res$p_ts,res$p)
	res
}














