source('R4.R')
source('R121.R')
library(foreach)

params1 = list(100,0.005*1:200,0,0,0)
params2 = list(0,0)
#N = 1500000
N = 50000

source('R110.R')
#for(k in 1:13)
	#calc_data_params_lists_SEQ_WITHFIX(c(N,get_corr_snow_Memory_CRYSTAL,params1),params2,calc_exp43_Snow_Memory,paste('RData/50k/res_CRYSTAL43-',k,'-',sep='')) 

#calc_data_params_lists_SEQ_WITHFIX(c(N,get_corr_snow_Memory_CRYSTAL,params1),params2,calc_exp43_Snow_Memory,paste('RData/res_CRYSTAL43-',sep='')) 



