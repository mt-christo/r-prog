call_func_with_params = function(func,params){
	if(length(params)==1)
		func(params[1])
	else if(length(params)==2)
		func(params[1],params[2])
	else if(length(params)==3)
		func(params[1],params[2],params[3])
	else if(length(params)==4)
		func(params[1],params[2],params[3],params[4])
	else if(length(params)==5)
		func(params[1],params[2],params[3],params[4],params[5])
	else if(length(params)==6)
		func(params[1],params[2],params[3],params[4],params[5],params[6])
	else if(length(params)==7)
		func(params[1],params[2],params[3],params[4],params[5],params[6],params[7])
	else if(length(params)==8)
		func(params[1],params[2],params[3],params[4],params[5],params[6],params[7],params[8])
}

#'func' returns a numeric value, the Error
mc_anton = function(func,params_in,var_idxs,var_steps,steps_lim){
	params = params_in
	old_params = array(-57,length(params_in))
	steps = 1
	while(steps < steps_lim && min(old_params==params) == FALSE){
		old_params = params
		for(i in 1:length(var_idxs)){
			params_p = params
			params_p[var_idxs[i]] = params_p[var_idxs[i]]+var_steps[i]
			params_m = params
			params_m[var_idxs[i]] = params_m[var_idxs[i]]-var_steps[i]
#print(call_func_with_params(func,params))
#stop()
			calcs = c(call_func_with_params(func,params_m),call_func_with_params(func,params),call_func_with_params(func,params_p))
print(calcs)
			if(which.min(calcs) == 1)
				params = params_m
			else if(which.min(calcs) == 3)
				params = params_p
			steps = steps+1
		}
		print(params)
	}

	params
}

mc_anton_var_steps = function(func,params_in,var_idxs,var_steps,steps_lim,var_steps_min){
	params = params_in
	old_params = array(-57,length(params_in))
	steps = 1
	while(steps < steps_lim && (min(old_params==params) == FALSE || max(var_steps>=var_steps_min) == TRUE)){
		old_params = params
		new_params_list = list()
		params_calcs = c()
		for(i in 1:length(var_idxs)){
			params_p = params
			params_p[var_idxs[i]] = params_p[var_idxs[i]]+var_steps[i]
			params_m = params
			params_m[var_idxs[i]] = params_m[var_idxs[i]]-var_steps[i]
#print(call_func_with_params(func,params))
#stop()
			new_params_list = c(new_params_list,list(params_m),list(params),list(params_p))
			params_calcs = c(params_calcs,call_func_with_params(func,params_m),call_func_with_params(func,params),call_func_with_params(func,params_p))

			steps = steps+1
		}

		print(params_calcs)			
		params = new_params_list[[which.min(params_calcs)]]
		if(min(old_params==params) == TRUE) for(i in var_idxs) var_steps[i] = var_steps[i]*0.75
		print(params)
		print(paste('step:',steps))
	}
	
	params
}





if(1==0){


				#mc_anton(function(x){5+0.01*(x-3.15)^2},c(20),c(1),c(0.1),10000)
				mc_anton(function(v,m,s,r0,t_num,dt){sum((cumsum(vasicek(v,m,s,r0,t_num,dt))[c(250,250*5,250*10)]*dt-c(99,98,97))^2)},c(1,1,1,0.1,2500,0.04),c(1,2,3),c(0.1,0.1,0.1),100)
				#mc_anton(function(v,m,s,r0,t_num,dt){print(cir(v,m,s,r0,t_num,dt))},c(1,1,1,0.1,2500,0.04),c(1,2,3),c(0.1,0.1,0.1),100)
				#cir(1,1,1,1,2500,0.004)


				# v-mean rev speed; m-mean rev level; s-volatility; r0-start rate; t_num-number of steps; dt-time step
				vasicek = function(v,m,s,r0,t_num,dt){
					res = array(0,t_num)
					res[1] = r0; 
					w = rnorm(t_num); 
					sqrt_dt = sqrt(dt)
					for(t in 2:t_num)
						res[t] = res[t-1] + v*(m-res[t-1])*dt + s*sqrt_dt*w[t]
					res[res<0] = 0
					res
				}

				vasicek_mc = function(v,m,s,r0,t_num,dt,exp_length){
					res = array(0,t_num)
					for(i in 1:exp_length) res = res+vasicek(v,m,s,r0,t_num,dt)
					res = res/exp_length	
				}

				# v-mean rev speed; m-mean rev level; s-volatility; r0-start rate; t_num-number of steps; dt-time step
				#2*v*m>s*s
				cir = function(v,m,s,r0,t_num,dt){
					res = array(0,t_num)
					res[1] = r0; 
					w = rnorm(t_num); 
					sqrt_dt = sqrt(dt)
					for(t in 2:t_num){
						res[t] = res[t-1] + v*(m-res[t-1])*dt + s*sqrt(res[t-1])*sqrt_dt*w[t]
						if(res[t]<0)
							stop(paste(res[t-1],v,m,dt,s,sqrt_dt,w[t],sep=', '))
					}
					res
				}

				list_range = function(l){
					res = c(1000000000,-1000000000)
					for(i in 1:length(l)) res = c(min(res[1],range(l[[i]])[1]),max(res[2],range(l[[i]])[2]))
					res
				}

				plot_list = function(l){
					rng = list_range(l)
					plot(l[[1]],ylim=rng,t='l'); for(i in 2:length(l)) lines(l[[i]])
				}

				t_dist = function(l,k){
					res = c()
					for(l_tmp in l) res=c(res,l_tmp[k])
					res
				}

				vs=list()
				for(i in 1:1000) vs=c(vs,list(cir(1.5,0.8,0.5,0.03,2500,1)))
				#plot_list(vs)
				hist(t_dist(vs,8),breaks=100)

				#r1=4.0303
				#r5=6.4746
				#r10=7.4304

}
