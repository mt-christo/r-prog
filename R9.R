library(doMC); library(hash); source("R4.R"); source("R5.R"); source("R6.R"); filename_prefix='RData/snow-plain-dbf-'; filename_ALS_prefix='RData/snow-plain-dbf-ALS-'; 
registerDoMC(8);
#registerDoMC(3);

snow1D_STEP <- function(c_in,av_cells_in,d_in,b_in){
    a = c_in; Excess=0; arr_len = length(a);
    for(p in av_cells_in){
	a[p] = a[p]-1
	av_recs = ceiling(runif(b_in)*arr_len)
	av_flake = 1.0/b_in
	dempf_scores = runif(b_in)
	for(i in 1:b_in) if(dempf_scores[i]<d_in) a[av_recs[i]] = a[av_recs[i]]+av_flake else Excess = Excess+av_flake
    }
    list(a,Excess)    
}


snow1D_Linear <- function(c_in,cell_idx,d_in,b_in,f_in,avalanche_func){
    a=c_in; a[cell_idx]=a[cell_idx]+f_in
    Excess=0; Num_Avalanche=0; Av_Array=c(length(which(a>1))); av_cells=c()
    step_i = 1
    while(max(a)>1){
	av_cells = which(a>1)
	av_res = avalanche_func(a,av_cells,d_in,b_in)
	a = av_res[[1]]
	Excess = Excess+av_res[[2]]
	Num_Avalanche = Num_Avalanche+length(av_cells)
	Av_Array = c(Av_Array,length(av_cells))
	step_i = step_i+1
    } 
    list(a,Excess,Num_Avalanche,Av_Array)
}


generate_cors = function(steps_count,actors_count,d_in,b_in,f_in,avalanche_func){
    actors = array(0,actors_count)
    s1 = array(0,steps_count)
    s2 = array(0,steps_count)
    s3 = array(0,steps_count)
    s4 = c(0)
    for(i in 1:steps_count){
        sn = snow1D_Linear(actors,ceiling(runif(1)*actors_count),d_in,b_in,f_in,avalanche_func)
        actors = sn[[1]]
        s1[i] = sn[[2]]
        s2[i] = sn[[3]]
        s3[i] = sum(actors)
        s4 = c(s4,cumsum(sn[[4]]))
        if(i%%1000==0) print(i)
    }

    list(s1,s2,s3,s4)
}

# N-length; c-cells array; d-demphing factor; b-branching index; f-flux;
#N=500; c=array(1,N); d=0.95; b=2; f=0.15;
#step_count = 100000
#ex = array(0,0); na = array(0,0); aa = array(0,0)
#for(i in 1:step_count){
#	lst = snow1D_Linear(c,ceiling(runif(1)*N),d,b,f,snow1D_STEP)
#	c = lst[[1]]
#	ex = c(ex,lst[[2]])
#	na = c(na,lst[[3]])
#	aa = c(aa,lst[[4]])
#	print(paste('Step',i))
#}

# steps_count=30000; actors_count=100; d_in=0.9; b_in=2; f_in=0.15; avalanche_func=snow1D_STEP
generate_ALS_ts = function(steps_count,actors_count,d_in,b_in,f_in,avalanche_func,fn_prefix){
    cors_list = calc_data_params_vals(list(steps_count,actors_count,d_in,b_in,f_in,avalanche_func),generate_cors,fn_prefix)
    #cors_list = generate_cors(steps_count,actors_count,d_in,b_in,f_in,avalanche_func)
    cors = cors_list[[4]]
    cors[cors>actors_count] = actors_count
    cors_len = length(cors)
    r_cors = sign(runif(cors_len)-0.5)*cors
    idx = c()
    for(i in 1:actors_count){
        idx = i>cors
        r_cors[idx] = r_cors[idx] + sign((runif(cors_len)-0.5))
        if(i%%10==0)
            print(i)
    }
    r_cors = r_cors/actors_count
    return(hash(mass=cors_list[[3]], ts=cumsum(r_cors)[ceiling(actors_count/f_in):cors_len]))
}


steps_cnt = 750000
steps_list = list(); for(i in 1:(8+16)) steps_list=c(steps_list,list(steps_cnt+i))
steps_list1 = list(); for(i in 1:8) steps_list1=c(steps_list1,list(steps_cnt+i))
#ts_res = generate_ALS_ts(steps_cnt,300,0.95,2,0.1,snow1D_STEP,filename_prefix)

#a=calc_data_params_lists(list(steps_cnt,list(300,600),0.95,list(2,3),list(0.1,0.15),snow1D_STEP), generate_cors, filename_prefix)
#a = calc_data_params_lists(list(steps_list,500,0.95,2,0.15,snow1D_STEP), generate_cors, filename_prefix)
tss=calc_data_params_lists(list(steps_list,500,0.95,2,0.15,snow1D_STEP,filename_prefix), generate_ALS_ts, filename_ALS_prefix)

tss=foreach(cnt=steps_list)%do%calc_data_params_vals(list(cnt,500,0.95,2,0.15,snow1D_STEP,filename_prefix), generate_ALS_ts, filename_ALS_prefix)$ts
mss=foreach(cnt=steps_list)%do%calc_data_params_vals(list(cnt,500,0.95,2,0.15,snow1D_STEP,filename_prefix), generate_ALS_ts, filename_ALS_prefix)$mass

dl=50
dtss=foreach(ts=tss)%do%{ ts[dl:length(ts)]-ts[1:(length(ts)-dl+1)]; }
dtss=foreach(ts=tss)%do%{ diff(ts,dl); }
total_diff=array(0.0,0); for(d in dtss) total_diff=c(total_diff,d)
#hist(dtss[[1]],br=1000)

brr=seq(-25,25,0.1)
plot(hist(dtss[[1]],plot=FALSE,br=brr), col=rgb(0,0,1,1/4))  # first histogram
plot(hist(dtss[[2]],plot=FALSE,br=brr), col=rgb(1,0,0,1/4), add=T)  # second
plot(hist(dtss[[3]],plot=FALSE,br=brr), col=rgb(0,1,0,1/4), add=T)  # second
plot(hist(dtss[[4]],plot=FALSE,br=brr), col=rgb(0,0,1,1/4), add=T)  # first histogram
plot(hist(dtss[[5]],plot=FALSE,br=brr), col=rgb(1,0,0,1/4), add=T)  # second
plot(hist(dtss[[6]],plot=FALSE,br=brr), col=rgb(0,1,0,1/4), add=T)  # second
plot(hist(dtss[[7]],plot=FALSE,br=brr), col=rgb(0,0,1,1/4), add=T)  # first histogram
plot(hist(dtss[[8]],plot=FALSE,br=brr), col=rgb(1,0,0,1/4), add=T)  # second

hist_dl = function(dl_in=100, brr_in=seq(-25,25,0.1)){
	dl=dl_in;
	dtss=foreach(ts=tss)%do%{ ts[dl:length(ts)]-ts[1:(length(ts)-dl+1)]; }
	#dtss=foreach(ts=tss)%do%{ diff(ts,dl); }
	total_diff=dtss[[1]]#array(0.0,0); for(d in dtss) total_diff=c(total_diff,d)
	#plot(hist(total_diff,plot=FALSE,br=brr), col=rgb(1,0,0,1/4))  # second
	brr=brr_in;  
	h=hist(total_diff,plot=FALSE,br=brr)
}

plot_pdf_dl = function(dl_in=100, brr_in=seq(-25,25,0.1), log_lims=c(0,100), do_plot=FALSE){
	h = hist_dl(dl_in,brr_in)
	p = my_hist_to_pdf(h); x=p[[1]]; y=p[[2]]

	#exp_ys = list(); for(i in 1:5) exp_ys=c(exp_ys,list(exp(-abs(x)*1*i)))
	#norm_ys = list(); for(i in 1:5) norm_ys=c(norm_ys,list(exp(-x*x*1*i)))
	xy=log_log_xy_excl(x,y,log_lims,c(-1)); i0=which(xy[[1]]==max(xy[[1]]));

	if(do_plot)
		plot(xy[[1]],xy[[2]],t='l',lwd=1,col='black') 
	else lines(xy[[1]],xy[[2]],t='l',lwd=1,col='black'); 

	#for(l in exp_ys) { tmp_xy=log_log_xy_excl(x,l,c(0,100),c(-1)); lines(tmp_xy[[1]]-tmp_xy[[1]][i0]+xy[[1]][i0],tmp_xy[[2]]-tmp_xy[[2]][i0]+xy[[2]][i0],t='l',lwd=1,col='red'); }
	#for(l in norm_ys) { tmp_xy=log_log_xy_excl(x,l,c(0,100),c(-1)); lines(tmp_xy[[1]]-tmp_xy[[1]][i0]+xy[[1]][i0],tmp_xy[[2]]-tmp_xy[[2]][i0]+xy[[2]][i0],t='l',lwd=1,col='blue'); }
}

plot_cdf_dl = function(dl_in=100, brr_in=seq(-25,25,0.1), log_lims=c(0,100), do_plot=FALSE){
	h = hist_dl(dl_in,brr_in)
	x = h$mids
	y = cumsum(h$counts)/sum(h$counts)
	if(do_plot)
		plot(x,y,t='l',lwd=1,col='black') 
	else lines(x,y,t='l',lwd=1,col='black'); 
}

plot_qq = function(dl_in=100, brr_in=seq(-25,25,0.1), log_lims=c(0,100), do_plot=FALSE, ddf){
	h = hist_dl(dl_in,brr_in)
	x = h$mids
	y = cumsum(h$counts)/sum(h$counts)
	if(do_plot)
		plot(qt(y,ddf,0),x,t='l',lwd=1,col='black',xlim=c(-20,20),ylim=c(-20,20)) 
	else lines(qt(y,ddf,0),x,t='l',lwd=1,col='black',xlim=c(-20,20),ylim=c(-20,20));
}

plot_qq(50,do_plot=TRUE,ddf=3)
plot_qq(50,ddf=4)
plot_qq(50,ddf=5)
plot_qq(50,ddf=6)
plot_qq(50,ddf=7)

#for(l in exp_ys) { tmp_xy=log_log_xy_excl(x,l,c(0,100),c(-1)); lines(tmp_xy[[1]]-tmp_xy[[1]][i0]+xy[[1]][i0],tmp_xy[[2]]-tmp_xy[[2]][i0]+xy[[2]][i0],t='l',lwd=1,col='red'); }
#for(l in norm_ys) { tmp_xy=log_log_xy_excl(x,l,c(0,100),c(-1)); lines(tmp_xy[[1]]-tmp_xy[[1]][i0]+xy[[1]][i0],tmp_xy[[2]]-tmp_xy[[2]][i0]+xy[[2]][i0],t='l',lwd=1,col='blue'); }

lm_xlim1=c(1.5,4); lm_xlim2=c(4,10); xy_lm1=log_log_xy_excl(x,y,lm_xlim1,c(-1)); xy_lm2=log_log_xy_excl(x,y,lm_xlim2,c(-1))
abline(lm(xy_lm1[[2]]~xy_lm1[[1]])); abline(v=-log(lm_xlim1[1])); abline(v=-log(lm_xlim1[2])); abline(lm(xy_lm2[[2]]~xy_lm2[[1]])); abline(v=-log(lm_xlim2[1])); abline(v=-log(lm_xlim2[2]));

plot(x,y,t='l')


range(dtss[[1]])
range(dtss[[2]])
range(dtss[[3]])
range(dtss[[4]])
range(dtss[[5]])
range(dtss[[6]])
range(dtss[[7]])
range(dtss[[8]])

plot(tss[[8]][ceiling(seq(1,length(mss[[1]]),3000))])



ts = ts_res$ts
p = my_hist_to_pdf(hist(ts[50:length(ts)]-ts[1:(length(ts)-49)],breaks=100,plot=FALSE)); x=p[[1]]; y=p[[2]]; xy=log_log_xy_excl(x,y,c(0,10000),c(-1))
#p = my_hist_to_pdf(hist(diff(ts,50),breaks=100,plot=FALSE)); x=p[[1]]; y=p[[2]]; xy=log_log_xy_excl(x,y,c(0,10000),c(-1))
plot(xy[[1]],xy[[2]])
plot(x,y)


x=seq(1,100,0.01); xcomp=which(x==15)
y1=x^(-1.37)
y2=exp(-x); #y2=y2*y1[xcomp]/y2[xcomp]
y3=exp(-x*x); #y3=y3*y1[xcomp]/y3[xcomp]
plot(x,y1,t='l',col='red'); lines(x,y2,col='blue'); lines(x,y3,col='orange')
plot(log(x),log(y1),t='l',col='red'); plot(log(x),log(y2),col='blue'); plot(log(x),log(y3),col='orange')












