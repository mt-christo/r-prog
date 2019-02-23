snow1D_R___ <- function(arr,arr_length,i,wgh,br_length){
    a = arr
    a[i] = a[i]+wgh
    Excess = 0
    Num_Avalanche = 0
    Av_Array = c(0)
    av_lengths = array(0,2)
    if(a[i]>=1){
            a[i] = a[i]-1
            nb = ceiling(runif(2)*br_length)
            l1 = if(nb[1]>0 && nb[1]<=arr_length) Recall(a,arr_length,nb[1],0.5,br_length) else list(a,0.5,0,c(0))
            l2 = if(nb[2]>0 && nb[2]<=arr_length) Recall(l1[[1]],arr_length,nb[2],0.5,br_length) else list(l1[[1]],0.5,0,c(0))
            a = l2[[1]]
            Excess = l1[[2]]+l2[[2]]
            Num_Avalanche = 1+l1[[3]]+l2[[3]]
            av_lengths = c(length(l1[[4]]),length(l2[[4]]))
            Av_Array = c(1,array(0,max(av_lengths)))
            ii = 0        
            for(i in 2:length(Av_Array)){
            ii = i-1
            if(ii<=av_lengths[1]) Av_Array[i]=Av_Array[i]+l1[[4]][ii]
            if(ii<=av_lengths[2]) Av_Array[i]=Av_Array[i]+l2[[4]][ii]
            }
    }
    list(a,Excess,Num_Avalanche,Av_Array)
}

snow_1D_STEP_plain___ <- function(arr,av_points,br_length){
    a = arr; Excess=0; arr_len = length(a); arr_br_len = round(arr_len*(1+br_length))
    for(p in av_points){
	a[p] = a[p]-1
	av_recs = ceiling(runif(2)*arr_br_len)
	if(av_recs[1]<=arr_len) a[av_recs[1]] = a[av_recs[1]]+0.5 else Excess = Excess+0.5
	if(av_recs[2]<=arr_len) a[av_recs[2]] = a[av_recs[2]]+0.5 else Excess = Excess+0.5
    }
    list(a,Excess)    
}

snow1D_L___ <- function(arr,i,wgh,params,avalanche_func){
#print('snow1D_L started')
#print(arr)
#print(i)
    a=arr; a[i]=a[i]+wgh
    Excess=0; Num_Avalanche=0; Av_Array=c(length(which(a>1))); av_points=c()
    step_i = 1
    while(max(a)>1){
	av_points = which(a>1)
	av_res = run_func_with_params(avalanche_func,c(list(a),list(av_points),params))
	a = av_res[[1]]
	Excess = Excess+av_res[[2]]
	Num_Avalanche = Num_Avalanche+length(av_points)
	Av_Array = c(Av_Array,length(av_points))
	step_i = step_i+1
#print(step_i)
    } 
    list(a,Excess,Num_Avalanche,Av_Array)
}



if(1==0){
	library(hash); source("~/R/R4.R"); source("~/R/R5.R"); source("~/R/R6.R");
	ts_len=10000;actors_count=100;wgh_in=0.3;params=list(0.1);avalanche_func=snow_1D_STEP_plain;filename_prefix='~/R/test-res-plain'
	params=list(ts_len,actors_count,wgh_in,params,avalanche_func);calc_func=generate_snow_arrs;filename_prefix=filename_prefix
	ts_len=params[[1]];actors_count=params[[2]];wgh_in=params[[3]];avalanche_func=params[[5]];params=params[[4]]
}
#ts_len=params[[1]];actors_count=params[[2]];wgh_in=params[[3]];avalanche_func=params[[5]];params=params[[4]]
generate_snow_arrs = function(ts_len,actors_count,wgh_in,params,avalanche_func){
print('generate_snow_arrs Started')
    arr = array(0,actors_count)
    s1 = array(0,ts_len)
    s2 = array(0,ts_len)
    s3 = array(0,ts_len)
    s4 = c(0)
    for(i in 1:ts_len){
        sn = snow1D_L(arr,ceiling(runif(1)*actors_count),wgh_in,params,avalanche_func)
        arr = sn[[1]]
        s1[i] = sn[[2]]
        s2[i] = sn[[3]]
        s3[i] = sum(arr)
        s4 = c(s4,cumsum(sn[[4]]))
        if(i%%1000==0) print(i)
    }

    list(s1,s2,s3,s4)
}


#ts_len=10000;actors_count=100;wgh_in=0.3;params=list(101);avalanche_func=snow_1D_STEP_plain;filename_prefix='~/R/test-res-plain'
generate_ALS_ts = function(ts_len,actors_count,wgh_in,params,avalanche_func,filename_prefix){
print('generate_ALS_ts Started')
    cors = calc_data_params_vals(list(ts_len,actors_count,wgh_in,params,avalanche_func),generate_snow_arrs,filename_prefix)[[4]]
print('generate_ALS_ts - filename loaded')
    cors[cors>actors_count] = actors_count
print(1)  
    cors_len = length(cors)
print(2)  
    r_cors = sign(runif(cors_len)-0.5)*cors
print(3)  
    idx = c()
print(4)  
    for(i in 1:actors_count){
print(i)  
        idx = i>cors
        r_cors[idx] = r_cors[idx] + sign((runif(cors_len)-0.5))
        if(i%%100==0)
            print(i)
    }
    r_cors = r_cors/actors_count
    cumsum(r_cors)
}



if(1==0){

	library(hash); source("~/R/R4.R"); source("~/R/R5.R"); source("~/R/R6.R");

    len_in=10000; mlen_in=100; wgh_in=0.1; add_mlen_in=0.02
    a1 = array(0,mlen_in)
    s1 = array(0,len_in)
    s2 = array(0,len_in)
    s3 = array(0,len_in)
    s4 = c(0)
    for(i in 1:len_in){
        sn = snow1D_L(a1,ceiling(runif(1)*mlen_in),wgh_in,add_mlen_in,snow_1D_STEP_plain)
        a1 = sn[[1]]
        s1[i] = sn[[2]]
        s2[i] = sn[[3]]
        s3[i] = sum(a1)
        s4 = c(s4,sn[[4]])
        if(i%%1000==0) print(i)
    }

	plot(s3)
	hist(s2,br=100)









}















