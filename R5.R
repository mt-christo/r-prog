take_index_from_each = function(arrs, arr_index){
	res = list()
	for(i in 1:length(arrs))
		res[[i]] = arrs[[i]][arr_index]
	res
}

take_each_index_from_arr = function(arr, arr_indexes){
	res = list()
	for(i in 1:length(arr_indexes))
		res[[i]] = arr[arr_indexes[[i]]]
	res
}

my_hist_to_pdf = function(h){
    x = (h$breaks[2:length(h$breaks)])[h$counts!=0]
    y = h$counts[h$counts!=0]
    dd = diff(x)
    dd = (c(dd[1],dd)+c(dd,dd[length(dd)]))*0.5
    list(x,y/dd)
}

log_log_xy_excl = function(x1, y1, excl, signs){
    idx1 = abs(x1)>excl[1] & abs(x1)<excl[2] & x1!=0 & sign(x1)%in%signs;
    list(sign(x1[idx1])*log(abs(x1[idx1])), log(y1[idx1]))
}

smooth_x = function(x, point_weight){
    df = diff(x)
    x + (1-point_weight)*0.5*(c(df,0)-c(0,df))
}

maxify_x = function(y, step_num){
	step = (length(y)+1)/step_num
	agg_tab = aggregate(y,list(ceiling((1:length(y))/step)),FUN=max); 
	list(floor(seq(step,length(y),l=step_num)),agg_tab[,2])
}
maxify_x(1:101,6)

equalify_x = function(x, y, num_steps){
    xrng = range(x)
    x_idx = 1+round(x*(num_steps-1)/(xrng[2]-xrng[1]))
    x_idx_rng = range(x_idx)
    y_agg = aggregate(y,by=list(x_idx),FUN=mean)
    list(seq(xrng[1],xrng[2],l=num_steps)[(x_idx_rng[1]:x_idx_rng[2])%in%y_agg[,1]],y_new = y_agg[,2])
}
equalify_x = function(x, y, num_steps){
	xrng = range(x)
	steps = ceiling(num_steps*(x-xrng[1])/(xrng[2]-xrng[1]))
	steps[steps==0] = 1
	x_agg = aggregate(x,by=list(steps),FUN=mean)
	y_agg = aggregate(y,by=list(steps),FUN=mean)
	list(x_new = x_agg[,2],y_new = y_agg[,2])
}
x=1:5; y=c(1,2,3,2,1); equalify_x(x,y,2)

ts_loglog_reg_allFields = function(ts, diff_step, h_breaks_num, excl_in, eq_steps, tail_signs){
    p = my_hist_to_pdf(hist(diff(ts, diff_step), breaks=h_breaks_num, plot=FALSE))
    x = p[[1]]; y = p[[2]];
    #tail_signs = c(-1, 1);
    xy = log_log_xy_excl(x, y, excl_in, tail_signs)
    xy = if(eq_steps>0) equalify_x(xy[[1]],xy[[2]],eq_steps) else xy
    reg = lm(xy[[2]]~xy[[1]])
    res = hash()
    res$ts=ts; res$x=x; res$y=y; res$logx=xy[[1]]; res$logy=xy[[2]]; res$reg=reg;
    res
}

#ts=tss[[1]]; diff_step=10; h_breaks_num=100; excl_in=c(0.001,40); eq_steps=30; tail_signs=c(-1)
ts_loglog_reg1 = function(ts, diff_step, h_breaks_num, excl_in, eq_steps, tail_signs){
    res_all = ts_loglog_reg_allFields(ts, diff_step, h_breaks_num, excl_in, eq_steps, tail_signs)
    res = hash()
    res$x = res_all$logx; res$y = res_all$logy
    res
}

#ts=tss[[1]]; diff_step=10; h_breaks_num=100; excl_in=c(0.001,40); eq_steps=30; tail_signs=c(-1)
ts_simple_reg1 = function(ts, diff_step, h_breaks_num, excl_in, eq_steps, tail_signs){
    res_all = ts_loglog_reg_allFields(ts, diff_step, h_breaks_num, excl_in, eq_steps, tail_signs)
    res = hash()
    res$x = res_all$x; res$y = res_all$y
    res
}














