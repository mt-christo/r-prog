get_corr_snow_graph_TOPPLE_CRYSTAL <- function(g_in,v_idxs,b_in){
    g_out = g_in
    vcnt = vcount(g_out)
    all_v_idxs = 1:vcnt
    front = array(0,0)

    for(i in v_idxs){
	for(j in sample(all_v_idxs,b_in))
		if(V(g_out)[j]$m > 1) {
		    V(g_out)[j]$m = V(g_out)[j]$m - 1
		    V(g_out)$m = V(g_out)$m + 1/vcnt
		    g_out = g_out + edge(i,j)
		    front = c(front,j)
		}
    }

    return(list(graph=g_out, front=front))    
}

get_corr_snow_graph_Increment_CRYSTAL <- function(g_in,v_idx1,v_idx2,b_in,da_in){
    g_out = g_in
    V(g_out)[v_idx1]$m = V(g_out)[v_idx1]$m + da_in
    if(length(v_idx2)>0)
	V(g_out)[v_idx2]$m = V(g_out)[v_idx2]$m - da_in

    front = array(0,0)
    if(V(g_out)[v_idx1]$m > 1+da_in)
	front = c(front,v_idx1)

    while(length(front) > 0){
	topple = get_corr_snow_graph_TOPPLE_CRYSTAL(g_out,front,b_in)
	g_out = topple$graph
	front = topple$front
    }
    return(g_out)
}

get_corr_snow_Memory_CRYSTAL = function(g_in_Global,N,a,b,d,da,d_topple){
	g = get_corr_snow_graph_Increment_CRYSTAL(g_in_Global - edges(g_in_Global)[[1]], sample(N,1), if(sum(V(g_in_Global)$m)>d*N)sample(N,1) else c(), b, da)
	return(list(graph = g, paths = shortest.paths(g)))
}


