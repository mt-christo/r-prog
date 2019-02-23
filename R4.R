run_func_with_params = function(calc_func,params){
#print('run_func_with_params Started')
	len = length(params)
	#print(length(params[[1]]))
	#print(length(params[[2]]))
	if(len == 0) 
		0 else
	if(len == 1) 
		calc_func(params[[1]]) else
	if(len == 2) 
		calc_func(params[[1]],params[[2]]) else
	if(len == 3) 
		calc_func(params[[1]],params[[2]],params[[3]]) else
	if(len == 4) 
		calc_func(params[[1]],params[[2]],params[[3]],params[[4]]) else
	if(len == 5) 
		calc_func(params[[1]],params[[2]],params[[3]],params[[4]],params[[5]]) else
	if(len == 6) 
		calc_func(params[[1]],params[[2]],params[[3]],params[[4]],params[[5]],params[[6]]) else
	if(len == 7) 
		calc_func(params[[1]],params[[2]],params[[3]],params[[4]],params[[5]],params[[6]],params[[7]]) else
	if(len == 8) 
		calc_func(params[[1]],params[[2]],params[[3]],params[[4]],params[[5]],params[[6]],params[[7]],params[[8]]) else 
	if(len == 9) 
		calc_func(params[[1]],params[[2]],params[[3]],params[[4]],params[[5]],params[[6]],params[[7]],params[[8]],params[[9]]) else 
	if(len == 10) 
		calc_func(params[[1]],params[[2]],params[[3]],params[[4]],params[[5]],params[[6]],params[[7]],params[[8]],params[[9]],params[[10]]) else 
	if(len == 11) 
		calc_func(params[[1]],params[[2]],params[[3]],params[[4]],params[[5]],params[[6]],params[[7]],params[[8]],params[[9]],params[[10]],params[[11]]) else 0
}

run_func_with_params_WITHFIX = function(calc_func,params,params_FIX){
#print('run_func_with_params Started')
	len = length(params)
	print(length(params))
	#print(length(params[[2]]))
	if(len == 0) 
		0 else
	if(len == 1) 
		calc_func(params[[1]],params_FIX) else
	if(len == 2) 
		calc_func(params[[1]],params[[2]],params_FIX) else
	if(len == 3) 
		calc_func(params[[1]],params[[2]],params[[3]],params_FIX) else
	if(len == 4) 
		calc_func(params[[1]],params[[2]],params[[3]],params[[4]],params_FIX) else
	if(len == 5) 
		calc_func(params[[1]],params[[2]],params[[3]],params[[4]],params[[5]],params_FIX) else
	if(len == 6) 
		calc_func(params[[1]],params[[2]],params[[3]],params[[4]],params[[5]],params[[6]],params_FIX) else
	if(len == 7) 
		calc_func(params[[1]],params[[2]],params[[3]],params[[4]],params[[5]],params[[6]],params[[7]],params_FIX) else
	if(len == 8) 
		calc_func(params[[1]],params[[2]],params[[3]],params[[4]],params[[5]],params[[6]],params[[7]],params[[8]],params_FIX) else 
	if(len == 9) 
		calc_func(params[[1]],params[[2]],params[[3]],params[[4]],params[[5]],params[[6]],params[[7]],params[[8]],params[[9]],params_FIX) else 
	if(len == 10) 
		calc_func(params[[1]],params[[2]],params[[3]],params[[4]],params[[5]],params[[6]],params[[7]],params[[8]],params[[9]],params[[10]],params_FIX) else 
	if(len == 11) 
		calc_func(params[[1]],params[[2]],params[[3]],params[[4]],params[[5]],params[[6]],params[[7]],params[[8]],params[[9]],params[[10]],params[[11]],params_FIX) else 0
}

#params=list(ts_len,actors_count,wgh_in,params,avalanche_func);calc_func=generate_snow_arrs;filename_prefix=filename_prefix
calc_data_params_vals = function(params, calc_func, filename_prefix){
	#fn = paste(filename_prefix,paste(params,collapse='-'),'.RData',sep='')
print('calc_data_params_vals Started')
	fn = paste(filename_prefix,gsub('/','',paste(params[unlist(lapply(params,class))!='function'],collapse='-')),'.RData',sep='')
	print(fn)
	if(!file.exists(fn)){
		res = 1
		save(res,file=fn)
		res = run_func_with_params(calc_func,params)
		save(res,file=fn)
		res
	} else
		get(load(fn))
}

calc_data_params_vals_WITHFIX = function(params, params_FIX, calc_func, filename_prefix){
	#fn = paste(filename_prefix,paste(params,collapse='-'),'.RData',sep='')
print('calc_data_params_vals Started')
	fn = paste(filename_prefix,gsub('/','',paste(params[unlist(lapply(params,class))!='function'],collapse='-')),'.RData',sep='')
	print(fn)
	if(!file.exists(fn)){
		res = 1
		save(res,file=fn)
		res = run_func_with_params_WITHFIX(calc_func,params,params_FIX)
		save(res,file=fn)
		res
	} else
		get(load(fn))
}

calc_data_params_lists = function(params, calc_func, filename_prefix){
print('calc_data_params_lists Started')
	if(max(unlist(lapply(params,length))) == 1){
		fn = paste(filename_prefix,gsub('/','',paste(unlist(params[unlist(lapply(params,class))!='function']),collapse='-')),'.RData',sep='')
		if(!file.exists(fn)){
print(1)
print(fn)
			res = 1	
			save(res,file=fn)
#print('yo!')
#print(params)
print(2)
			res = run_func_with_params(calc_func,params)
			save(res,file=fn)
		}
	} else{
print(3)
		i = which(unlist(lapply(params,length))>1)[1]
#		for(param in params[[i]]){
		foreach(param = params[[i]]) %dopar% {
			params_new = params
			params_new[[i]] = c(param)
#print(params_new[[i]])
print(4)
			calc_data_params_lists(params_new, calc_func, filename_prefix)
			'done'
		}
	}
}


# params=list(10000,get_corr_3,ps1,ps2,ps3); params_FIX=list(ps4,ps5); calc_func=calc_exp4; filename_prefix='/mnt/G/RISK/ANTON/R/perc_corr/res-'
calc_data_params_lists_SEQ_WITHFIX = function(params, params_FIX, calc_func, filename_prefix){
print('calc_data_params_lists_SEQ Started')
	if(max(unlist(lapply(params,length))) == 1){
		fn = paste(filename_prefix,gsub('/','',paste(unlist(params[unlist(lapply(params,class))!='function']),collapse='-')),'.RData',sep='')
		if(!file.exists(fn)){
print(fn)
			res = 1	
			save(res,file=fn)
			res = run_func_with_params_WITHFIX(calc_func, params, params_FIX)
			save(res,file=fn)
		}
	} else{
		i = which(unlist(lapply(params,length))>1)[1]
		foreach(param = params[[i]]) %do% {
			params_new = params
			params_new[[i]] = c(param)
			calc_data_params_lists_SEQ_WITHFIX(params_new, params_FIX, calc_func, filename_prefix)
			'done'
		}
	}
}

calc_data_params_lists_WITHFIX = function(params, params_FIX, calc_func, filename_prefix){
print('calc_data_params_lists_SEQ Started')
	if(max(unlist(lapply(params,length))) == 1){
		fn = paste(filename_prefix,gsub('/','',paste(unlist(params[unlist(lapply(params,class))!='function']),collapse='-')),'.RData',sep='')
		if(!file.exists(fn)){
print(fn)
			res = 1	
			save(res,file=fn)
			res = run_func_with_params_WITHFIX(calc_func, params, params_FIX)
			save(res,file=fn)
		}
	} else{
		i = which(unlist(lapply(params,length))>1)[1]
		foreach(param = params[[i]]) %dopar% {
			params_new = params
			params_new[[i]] = c(param)
			calc_data_params_lists_SEQ_WITHFIX(params_new, params_FIX, calc_func, filename_prefix)
			'done'
		}
	}
}

#analyze_plot_tss(list(l1,l2),ts_loglog_reg1,list(10, 100, c(0.001,40), 50, c(-1)),rainbow(2),1)
#tss=list(l1,l2); analyze_func=ts_loglog_reg1; analyze_params=list(10, 100, c(0.001,40), 50, c(-1)); cols=rainbow(2); lw_in=1
analyze_plot_tss = function(tss, analyze_func, analyze_params, cols, lw_in){
	tss_analyzed = list()
	for(i in 1:length(tss))
		tss_analyzed[[i]] = run_func_with_params(analyze_func,c(list(tss[[i]]),analyze_params))

	x_lim = c(10000000,-10000000); y_lim = c(10000000,-10000000)
	for(i in 1:length(tss_analyzed)) {
		rngx = range(tss_analyzed[[i]]$x); rngy = range(tss_analyzed[[i]]$y)
		x_lim = c(min(x_lim[1],rngx[1]),max(x_lim[2],rngx[2]))
		y_lim = c(min(y_lim[1],rngy[1]),max(y_lim[2],rngy[2]))
	}

	plot(tss_analyzed[[1]]$x, tss_analyzed[[1]]$y, xlim=x_lim, ylim=y_lim, t='l', lwd=lw_in, col=cols[1])
	if(length(tss)>1)
		for(i in 2:length(tss))
			lines(tss_analyzed[[i]]$x, tss_analyzed[[i]]$y, xlim=x_lim, ylim=y_lim, t='l', lwd=lw_in, col=cols[i])
}

analyze_calc_tss = function(tss, analyze_func, analyze_params, cols, lw_in){
	tss_analyzed = list()
	for(i in 1:length(tss))
		tss_analyzed[[i]] = run_func_with_params(analyze_func,c(tss[[i]],analyze_params))

	tss_analyzed
}


