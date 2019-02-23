library(portfolio)


pmwAnton <<- 0

aesDT <- function(...) {
 aes <- structure(list(...),  class = "uneval")
 rename_aes(aes)
}

ggplotDT <- function(...) {
 ggplot(, aesDT(...))
}


#end_date=params$EndDate
calcproformaRt <- function (ard,end_date,allocDate=NULL)
  {
    allocDate=isnull(allocDate,end_date)
    alloc=as.double(ard$Allocations[allocDate,])
    rt=xts(ard$Returns%*%alloc,index(ard$Returns))
    colnames(rt)=c('rt')
    rt=rt[index(rt)<=as.Date(end_date)]
    return (rt)

  }

#calculate proforma returns with allocation effects
calcproformaAllocRt <- function (ard,end_date)
  {
    rt=xts(apply(ard$Returns*ard$Allocations,FUN=sum,MARGIN=1),index(ard$Returns))
    colnames(rt)=c('rt')
    rt=rt[index(rt)<=as.Date(end_date)]
    return (rt)
  }


#need to use this until ggplot2 issues are fixed
tframe <- data.frame()

mrows <- 20 # number of rows in graphs if it is too big
order <- 1



#bench_codes=c('HFRIFOFC_INDEX')
#min_date='1990-01-01'
#max_date='9999-12-31'
#all=FALSE
graphW=900
graphH=650
def_start_date='1990-01-01'
def_forward_days=366


load_benchmarks_xts <- function (bench_codes,min_date,max_date,all=FALSE)
{
  brt=xts()
  i <- 1
  for(i in 1:length(bench_codes)) {
    tmp_b = sqlQuery(pmwAnton,paste("exec Crestline.Risk_PortfolioReturns_GetBenchmarkReturns '",bench_codes[i],"','",min_date,"','",max_date,"'",sep=""))
    message('LINEEEEEEEEEEEEE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
    message(paste("exec Crestline.Risk_PortfolioReturns_GetBenchmarkReturns '",bench_codes[i],"','",min_date,"','",max_date,"'",sep=""))
    xt=xts(tmp_b$Return_Value,tmp_b$Return_Date)
    if (i==1) brt=xt else brt=merge(brt,xt,all=all)
    if (length(xt)>0)
      {
        message('Benchmark:',bench_codes[i],':',head(index(xt),1),'::',tail(index(xt),1))
      }
  }
  colnames(brt)=bench_codes
  return(brt)
}

XTS_priority_overlay = function(xts_1, xts_2) {
	res = merge.xts(xts_1, xts_2)

	isna_12 = c()
	mrg = xts()
	wid = dim(xts_1)[2]

	if(wid == dim(xts_2)[2]) 
		for(i in 1:wid) {
			isna_12 = is.na(res[,i]) & !is.na(res[,wid+i])
			if(length(isna_12) > 0)
				res[isna_12,i] = res[isna_12,wid+i]
		}

	res = res[,1:wid]
	res
}


#returns_xts=tmp_rets
#benches_xts=bench_history
#returns_xts=tmp_rets; benches_xts=bench_history; include_p_u=add_dr_length;
XTS_cross_betas = function(returns_xts, benches_xts, bench_codes, include_p_u=FALSE, include_corr=FALSE) {
	res1 = data.frame(matrix(NA,dim(returns_xts)[2],dim(benches_xts)[2]))
	res2 = res1
	res3 = res1
	
	rxts = returns_xts[as.Date(index(returns_xts))%in%as.Date(index(benches_xts)),]
        bxts = benches_xts[as.Date(index(benches_xts))%in%as.Date(index(returns_xts)),]
	max_dates_match = as.Date(max(index(returns_xts)))==as.Date(max(index(benches_xts)))
			
	coeff = matrix()
	rs = xts()
	bs = xts()

	for(i in 1:dim(rxts)[2]) 
		for(j in 1:dim(bxts)[2]) {
			rs = as.double(rxts[,i])
			bs = as.double(bxts[,j])

			if(!NA%in%rs && !NA%in%bs && max_dates_match){
				coeff = coef(summary(lm(rs ~ bs)))
				corr_value = cor(rs,bs)
				if(dim(coeff)[1]>1){
					res1[i,j] = isnull(coeff[2,1],0)
					res2[i,j] = isnull(coeff[2,4],1)
					res3[i,j] = corr_value
				} else {
					res1[i,j] = 0
					res2[i,j] = 1
					res3[i,j] = 0
				}
				if(include_p_u)
					res1[i,j] = paste(res1[i,j],Return.annualized(rxts[,i],scale=12)-Return.annualized(bxts[,j],scale=12),sep=';')
			} 
		}

	for(i in 1:dim(res1)[2]) {
		names(res1)[i] = bench_codes[i]
		names(res2)[i] = bench_codes[i]
		names(res3)[i] = bench_codes[i]
	}

	res = hash()
	res$Betas = res1
	res$Prs = res2
	res$Corrs = res3

	res
}


get_grade_returns_XTS = function(data_list, allocs, grade_sources) {
	fund_sources_list = data_list$SourcesList
	fund_returns = data_list$Returns
	fund_sources = data_list$Sources

	ids = fund_sources_list$ID

	names = fund_sources_list$Name
	types = fund_sources_list$Type

	grade_ids = ids[which(paste(names,types)%in%paste(grade_sources$Name,grade_sources$Type))]
	calc_rets = fund_returns*(fund_sources%in%grade_ids)
	calc_allocs = fund_returns*(fund_sources%in%grade_ids)
	res = calc_rets[,1:2]*0	

	for (i in 1:dim(calc_rets)[2]) {
		res[,1] = res[,1] + calc_rets[,i]*allocs[i]
		res[,2] = res[,2] + (fund_sources[,i]%in%grade_ids)*allocs[i]
	}

	res
}

# compute a month difference as a difference between two monnb's
monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01")); 
                          lt$year*12 + lt$mon } 
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }

#rets_xts=ard$Returns[index(ard$Returns)<=calc_date,]; codes_list=ard$Funds$FundName; allocs=array(0,length(ard$Funds$FundName)); windows_IN=params$Windows; start_date=params$StartDate; bench_codes=bench_codes; bench_history=bench_history; add_dr_length=TRUE
#codes_list=funds_tab$FundDescription; windows_IN=windows; add_dr_length=FALSE;
calc_Portfolio_Return_Stats1 = function(params, rets_xts, codes_list, allocs, windows_IN, start_date, bench_codes, bench_history, add_dr_length=FALSE) {
	windows = c(windows_IN,"FromStart","From0")
	annualize_fromStart = FALSE

	fund_col_ct = 5 + 3*length(bench_codes) # 3* - because we have Betas & Prs & Corrs
	res = data.frame(matrix(NA,length(codes_list),2+fund_col_ct*length(windows))) 

	names(res)[1] = 'Name'
	names(res)[2] = 'Allocation'

	res[,'Name'] = codes_list
	res[,'Allocation'] = allocs

	wnd_indices = list()
	for(i in 1:length(windows))
		if(windows[i] == 'ytd'){
			ystart=as.Date(paste(format(index(rets_xts)[length(index(rets_xts))],'%Y'),'-01-01',sep=''))
			wnd_indices[[i]] = which(as.Date(index(rets_xts))>=ystart)
		} else if(windows[i] == 'FromStart'){
			yend = as.Date(index(rets_xts)[length(index(rets_xts))])
			ystart = as.Date(paste(format(start_date,'%Y'),'-',format(start_date,'%m'),'-01',sep=''))
			wnd_indices[[i]] = which(as.Date(index(rets_xts))>=ystart)
			if(mondf(ystart, yend) >= 12)
				annualize_fromStart = TRUE
		} else if(windows[i] == 'From0'){
			yend = as.Date(index(rets_xts)[length(index(rets_xts))])
			ystart = as.Date(index(rets_xts)[1])
			wnd_indices[[i]] = which(as.Date(index(rets_xts))>=ystart)
		} else
			wnd_indices[[i]] = (max(0,dim(rets_xts)[1]-as.numeric(windows[i])+1)):dim(rets_xts)[1]
	
	betas_prs_corrs = hash()
	betas =  data.frame()
	prs =  data.frame()

	tmp_idx = 0
	tmp_rets = c()
	sd_r = 0
	for(i in 1:length(windows)){
		message(windows[i])

		tmp_idx = 2+fund_col_ct*(i-1)+1
		tmp_rets = rets_xts[wnd_indices[[i]],]

		betas_prs_corrs = XTS_cross_betas(tmp_rets,bench_history,bench_codes,add_dr_length)
		betas = betas_prs_corrs$Betas
		prs = betas_prs_corrs$Prs
		corrs = betas_prs_corrs$Corrs
          
		for(j in 1:length(codes_list)){
			r = tmp_rets[,j]
			r[is.na(r)] = 0

			sd_r = as.numeric(sd(r)*sqrt(12))
			res[j,tmp_idx+1] = sd_r
			res[j,tmp_idx+2] = as.numeric(maxDrawdown(r))

			if(add_dr_length){
				drdown = findDrawdowns(r)
				#print(paste('Running ',codes_list[j]))
				#print(drdown$to)
				#print(length(r))
				ldrdown = length(drdown$to)
				if(ldrdown>0 && drdown$to[ldrdown]>length(r) && drdown$return[ldrdown]<0){
					res[j,tmp_idx+2] = paste(res[j,tmp_idx+2],';',drdown$length[length(drdown$to)],sep='')	
					res[j,tmp_idx+2] = paste(res[j,tmp_idx+2],';',Return.cumulative(r[drdown$from[ldrdown]:length(r)]),sep='')				
				}
			}
              
			res[j,tmp_idx+3] = sd_r*1.64
			res[j,tmp_idx+4] = sd_r*2.06

			for(k in 1:length(bench_codes)){
				res[j,tmp_idx+4 + 3*(k-1)+1] = if(is.na(betas[j,k]) || length(betas[j,k])==0) NA else betas[j,k]
				res[j,tmp_idx+4 + 3*(k-1)+2] = if(is.na(prs[j,k]) || length(prs[j,k])==0) NA else prs[j,k]
				res[j,tmp_idx+4 + 3*(k-1)+3] = if(is.na(corrs[j,k]) || length(corrs[j,k])==0) NA else corrs[j,k]
			}

			if(!windows[i]%in%c('ytd','FromStart','From0') && nchar(windows[i]) > 1) 
				res[j,tmp_idx] = as.numeric(Return.annualized(r,scale=12)) else 
			if(windows[i] == 'FromStart' && annualize_fromStart == TRUE)
				res[j,tmp_idx] = as.numeric(Return.annualized(r,scale=12)) else
				res[j,tmp_idx] = as.numeric(Return.cumulative(r))
		}

		if(!windows[i]%in%c('ytd','FromStart','From0')){
			names(res)[tmp_idx] = paste('m',windows[i],sep='')                
			names(res)[tmp_idx+1] = paste('m',windows[i],'_','Vol',sep='')    
			names(res)[tmp_idx+2] = paste('m',windows[i],'_','Dr',sep='')    
			names(res)[tmp_idx+3] = paste('m',windows[i],'_','VaR',sep='')    
			names(res)[tmp_idx+4] = paste('m',windows[i],'_','ES',sep='')    
			for(k in 1:length(bench_codes)){
				names(res)[tmp_idx+4 + 3*(k-1)+1] = paste('m',windows[i],'_',bench_codes[k],sep='')    
				names(res)[tmp_idx+4 + 3*(k-1)+2] = paste('m',windows[i],'_',bench_codes[k],'_S',sep='')    
				names(res)[tmp_idx+4 + 3*(k-1)+3] = paste('m',windows[i],'_',bench_codes[k],'_c',sep='')    
			}
		} else if(windows[i] == 'ytd'){
			names(res)[tmp_idx] = 'ytd'
			names(res)[tmp_idx+1] = paste('ytd_','Vol',sep='')
			names(res)[tmp_idx+2] = paste('ytd_','Dr',sep='')
			names(res)[tmp_idx+3] = paste('ytd_','VaR',sep='')    
			names(res)[tmp_idx+4] = paste('ytd_','ES',sep='')    
			for(k in 1:length(bench_codes)) {
				names(res)[tmp_idx+4 + 3*(k-1)+1] = paste('ytd_',bench_codes[k],sep='')
				names(res)[tmp_idx+4 + 3*(k-1)+2] = paste('ytd_',bench_codes[k],'_S',sep='')
				names(res)[tmp_idx+4 + 3*(k-1)+3] = paste('ytd_',bench_codes[k],'_c',sep='')
			}
		} else if(windows[i] == 'FromStart'){
			names(res)[tmp_idx] = 'FromStart'
			names(res)[tmp_idx+1] = paste('FromStart_','Vol',sep='')
			names(res)[tmp_idx+2] = paste('FromStart_','Dr',sep='')
			names(res)[tmp_idx+3] = paste('FromStart_','VaR',sep='')    
			names(res)[tmp_idx+4] = paste('FromStart_','ES',sep='')    
			for(k in 1:length(bench_codes)) {
				names(res)[tmp_idx+4 + 3*(k-1)+1] = paste('FromStart_',bench_codes[k],sep='')
				names(res)[tmp_idx+4 + 3*(k-1)+2] = paste('FromStart_',bench_codes[k],'_S',sep='')
				names(res)[tmp_idx+4 + 3*(k-1)+3] = paste('FromStart_',bench_codes[k],'_c',sep='')
			}
		} else if(windows[i] == 'From0'){
			names(res)[tmp_idx] = 'From0'
			names(res)[tmp_idx+1] = paste('From0_','Vol',sep='')
			names(res)[tmp_idx+2] = paste('From0_','Dr',sep='')
			names(res)[tmp_idx+3] = paste('From0_','VaR',sep='')    
			names(res)[tmp_idx+4] = paste('From0_','ES',sep='')    
			for(k in 1:length(bench_codes)) {
				names(res)[tmp_idx+4 + 3*(k-1)+1] = paste('From0_',bench_codes[k],sep='')
				names(res)[tmp_idx+4 + 3*(k-1)+2] = paste('From0_',bench_codes[k],'_S',sep='')
				names(res)[tmp_idx+4 + 3*(k-1)+3] = paste('From0_',bench_codes[k],'_c',sep='')
			}
		}
        }
        
	res
}


#get_fund_returns_table_XTS = function(calc_data, windows, bench_codes, alloc_date) {
#calc_data=ard; calc_mode='StrategyReturns'
#params=ard_params_tmp; calc_data=ard_in;
#get_fund_returns_table_XTS_FundReturns, params, ard, 'ReturnsTable_FundReturns', new_funds_list$FundsList
#calc_data = calc_data_next; calc_func = get_fund_returns_table_XTS_FundReturns; calc_mode = 'FundReturns';
#calc_data = ard; calc_func = get_fund_returns_table_XTS_FundReturns; calc_mode = 'FundReturns';
market_bench_codes = c('SPTR_INDEX','DLJHTR_INDEX','LBUSTRUU_INDEX')
market_bench_name='SPX_HY_USAGG'

#calc_mode='StrategyReturns'
get_fund_returns_table_XTS = function(params, calc_data, calc_mode) {
	bench_codes = market_bench_codes

	start_date = params$StartDate
	alloc_date = params$AllocationDate
	end_date = params$EndDate
	windows = params$Windows

	funds_tab = calc_data$Funds
	rets_xts = calc_data$Returns
	clss_xts = calc_data$Classes 
	clss_dict = calc_data$ClassesList

	message('date:',alloc_date)
	alloc_date_idx = which(as.Date(index(calc_data$Allocations))==alloc_date)

	allocs = as.numeric(calc_data$Allocations[alloc_date_idx,])
	alloc_funds_idx = which(allocs!=0)
	funds_tab = funds_tab[alloc_funds_idx,]

	#alloc_date_idx = alloc_date_idx - 1  # Need to take returns BEFORE alloc date
	rets_xts = rets_xts[index(rets_xts)<=end_date,alloc_funds_idx]	

#message('calc_data$Allocations: ',calc_data$Allocations)
#stop('Stopped 1')


	clss_xts = as.numeric(clss_xts[alloc_date_idx,alloc_funds_idx])
        
        
        
	allocs = allocs[alloc_funds_idx]
	fund_classnames = array('',length(allocs))
	fund_classcodes = array('',length(allocs))

	class_idx = c()
	for(j in 1:length(fund_classnames)) {
		class_idx = which(clss_dict$ID == clss_xts[j])
		fund_classnames[j] = as.character(clss_dict$Name)[class_idx]
		fund_classcodes[j] = as.character(clss_dict$Code)[class_idx]
	}


	bench_history = xts()
	tmp_b = data.frame()
	min_date = min(as.Date(index(rets_xts)))
	max_date = max(as.Date(index(rets_xts)))
        bench_history = get_cached_index_pack(params,bench_codes,market_bench_name)
        bench_history = bench_history[as.Date(index(bench_history))<=end_date,]
	res = hash()
	res$FundsList = calc_data$FundsList[alloc_funds_idx]


	if(calc_mode == 'FundsList'){
		# nothing to do
	} else if(calc_mode == 'FundReturns'){
		message("Return stats calculation for funds (starting): ",params$SessionID) 
		message(paste("Start date: ",start_date))
		res$ReturnsTable = cbind(ClassCode=fund_classcodes,Class=fund_classnames,calc_Portfolio_Return_Stats1(params,rets_xts, funds_tab$FundDescription, allocs, windows, start_date, bench_codes, bench_history))
		message("Return stats calculation for funds (finished)") 
	} else if(calc_mode == 'StrategyReturns'){
		class_rets_xts = xts()
		class_allocs = array(0,length(clss_dict))
		rets_xts_idx = index(rets_xts)
		weighted_rets = rets_xts
		for(i in 1:dim(rets_xts)[2])
			weighted_rets[,i] = weighted_rets[,i]*allocs[i]

		class_rets_xts_tmp = xts()
		for(i in 1:dim(clss_dict)[1]) {
			class_idx = which(clss_xts == clss_dict$ID[i])
			class_allocs[i] = sum(allocs[class_idx])
			class_rets_xts_COL = xts(rowSums(weighted_rets[,class_idx]), rets_xts_idx)*(if(class_allocs[i]!=0) 1/class_allocs[i] else 0)
# /\/\/\/\ do the same for betas!
			if(length(class_rets_xts)>0)
				class_rets_xts = merge.xts(class_rets_xts,class_rets_xts_COL)
			else
				class_rets_xts = class_rets_xts_COL
		}

		rm_idx = -which(class_allocs==0)
		if(length(rm_idx)==0)
			rm_idx = 1:length(clss_dict$Name)

		message("Return stats calculation for classes (starting):") 
		#clss_res_table = cbind(Class=clss_dict$Name[rm_idx],calc_Portfolio_Return_Stats1(class_rets_xts[,rm_idx], clss_dict$Name[rm_idx], class_allocs[rm_idx], windows, start_date, bench_codes))
		clss_res_table = cbind(ClassCode=clss_dict$Code[rm_idx],Class=clss_dict$Name[rm_idx],calc_Portfolio_Return_Stats1(params, class_rets_xts[,rm_idx], clss_dict$Name[rm_idx], class_allocs[rm_idx], windows, start_date, bench_codes, bench_history))
		message("Return stats calculation for classes (finished)") 
	
		#res = rbind(res,clss_res_table)

		# Total:
		message("Return stats calculation, totals (starting):") 
		totals = calc_Portfolio_Return_Stats1(params,xts(rowSums(weighted_rets), rets_xts_idx), c('Total'), c(1), windows, start_date, bench_codes, bench_history)
		tot_res_table = cbind(ClassCode=c('Total'),Class=c('Total'),totals)
		message("Return stats calculation, totals (finished)") 
		res$ReturnsTable = rbind(clss_res_table,tot_res_table)

		tot = totals[which(!colnames(totals)%in%c('Name','Allocation'))]
		nms = colnames(tot)
		l1=unlist(lapply(nms,function(x){strsplit(x,'_')[[1]][1]})); l1[is.na(l1)]=''
		l2=unlist(lapply(nms,function(x){strsplit(x,'_')[[1]][2]})); l2[is.na(l2)]='R'
		l3=unlist(lapply(nms,function(x){strsplit(x,'_')[[1]][3]})); l3[is.na(l3)]=''
		l4=unlist(lapply(nms,function(x){strsplit(x,'_')[[1]][4]})); l4[is.na(l4)]=''

		l2[l2=='SPTR'] = 'SPY'
		l2[l2=='DLJHTR'] = 'HY'
		l2[l2=='LBUSTRUU'] = 'BAG'
		l2[l4=='S'] = paste('p-',l2[l4=='S'],sep='')
		l2[l4=='c'] = paste('c-',l2[l4=='c'],sep='')

		#l2_4 = paste(l2,l4,sep='')
		l2_4 = l2
		tot_tab = matrix(NA,length(unique(l1)),length(unique(l2_4)))
		colnames(tot_tab) = unique(l2_4)
		rownames(tot_tab) = unique(l1)
		tot_tab = data.frame(tot_tab)
		colnames(tot_tab) = gsub('\\.','-',colnames(data.frame(tot_tab)))
		for(wnd in unique(l1))
			for(idx in unique(l2_4))
				tot_tab[wnd,idx]=tot[l1==wnd & l2_4==idx]*100

# c('SPTR_INDEX','DLJHTR_INDEX','LBUSTRUU_INDEX')

		#colnames(tot_tab) = c('Return','Volatility','Drawdown','SPY','p-SPY','c-SPY','HY','p-HY','c-HY','BAG','p-BAG','c-BAG')
		colnames(tot_tab)[1:3] = c('Return','Volatility','Drawdown','VaR','ExpShortfall')
		rownames(tot_tab) = c('12','24','36','60','120','YTD','CM','From0')
		tot_tab = tot_tab[-which(rownames(tot_tab)%in%c('YTD','From0')),-which(colnames(tot_tab)%in%c('p-SPY','p-HY','p-BAG','c-SPY','c-HY','c-BAG','BAG'))]
		res$Totals_Breakdown = tot_tab

		res$Fund_allocs = allocs	
	}

	res
}


get_fund_returns_table_XTS_FundsList = function(params, calc_data) {
	get_fund_returns_table_XTS(params, calc_data,'FundsList') 
	
}

get_fund_returns_table_XTS_FundReturns = function(params, calc_data) {
	get_fund_returns_table_XTS(params, calc_data,'FundReturns') 
	
}

get_fund_returns_table_XTS_StrategyReturns = function(params, calc_data) {
	get_fund_returns_table_XTS(params, calc_data,'StrategyReturns') 	
}

#portfolio_id=portf_id
#ma_portf_id
#funds_in=funds_rules
#rule_idx=i
#clss_dict_in=clss_dict
#start_date, end_date
#return_db_mode="Rule ID"
#skip_funds)

load_rule_data_to_XTS_old = function(portfolio_id, ma_portf_id, funds_in, rule_idx, clss_dict_in, start_date, end_date, return_db_mode, skip_funds) {
	message(paste("Starting rule index:",rule_idx))

	RTS_xts_OUT = NULL
	CLSS_xts_OUT = NULL
	SRC_xts_OUT = NULL
	
	CLSS_dict_OUT = clss_dict_in
	
	f = c()
	f1 = c()
	f2 = c()
	f3 = c()
	clss = c()
	clss_codes = c()


	message('LINE HIT!')
	for(i in 1:dim(funds_in)[1]) {
		if(funds_in[i,1]%in%skip_funds)
			f = array(0,0)
		else if(is.na(funds_in[i,rule_idx]) || funds_in[i,rule_idx] == -1)
			f = sqlQuery(pmwAnton,paste("exec [Crestline].[Risk_PortfolioReturns_Get_Fund_Rule_Return_Comment_R] 'aslepnev', '", gsub("'", "''", funds_in[i,1]), "','[VOID]','", format(start_date,"%Y%m%d"), "','", format(end_date,"%Y%m%d"), "', ", ma_portf_id, sep=""))
		else if (return_db_mode == "Rule comment")
			f = sqlQuery(pmwAnton,paste("exec [Crestline].[Risk_PortfolioReturns_Get_Fund_Rule_Return_Comment_R] 'aslepnev', '", gsub("'", "''", funds_in[i,1]), "','", funds_in[i,rule_idx], "','", format(start_date,"%Y%m%d"), "','", format(end_date,"%Y%m%d"), "', ", ma_portf_id, sep=""))
		else if (return_db_mode == "Rule ID")
			f = sqlQuery(pmwAnton,paste("exec [Crestline].[Risk_PortfolioReturns_Get_Fund_Rule_Return_R] 'aslepnev', '", gsub("'", "''", funds_in[i,1]), "','", funds_in[i,rule_idx], "','", format(start_date,"%Y%m%d"), "','", format(end_date,"%Y%m%d"), "', ", ma_portf_id, sep=""))



		#message(gsub("'","''",funds_in[i,1]))
		#message(paste("exec [Crestline].[Risk_PortfolioReturns_Get_Fund_Rule_Return_R] 'aslepnev', '",gsub("'","''",funds_in[i,1]),"','",funds_in[i,rule_idx],"','",format(start_date,"%Y%m%d"),"','",format(end_date,"%Y%m%d"),"', ",portfolio_id,sep=""))
		#message(f)
		if(!(funds_in[i,1]%in%skip_funds))
			message(paste("exec [Crestline].[R..R] 'aslepnev', '", gsub("'", "''", funds_in[i,1]), "','", funds_in[i,rule_idx], "','", format(start_date,"%Y%m%d"), "','", format(end_date,"%Y%m%d"), "', ", ma_portf_id, sep=""),' - ',range(f$ClassCode))

		# If empty XTS, assign NA/current month
		if(dim(f)[1] != 0) {
			f1 = as.xts(f$ReturnValue,as.Date(f$ReturnDate))
			f2 = as.xts(f$ReturnSource,as.Date(f$ReturnDate))
			f3 = as.xts(array(0,length(f$ReturnDate)),as.Date(f$ReturnDate))
			clss = as.character(f$Class)
			clss[is.na(clss)] = '[EMPTY]'
			clss_codes = as.character(f$ClassCode)
			clss_codes[is.na(clss_codes)] = '[EMPTY]'
		
			for(j in 1:length(clss))
				if (clss[j]%in%CLSS_dict_OUT$Class)
					f3[j] = which(CLSS_dict_OUT$Class == clss[j])
				else {
					CLSS_dict_OUT = rbind(CLSS_dict_OUT,data.frame(Class=c(clss[j]),ClassCode=c(clss_codes[j])))
					f3[j] = length(CLSS_dict_OUT$Class)
				}
		} else {
			#f1 = as.xts(NA,start_date)
			f1 = as.xts(NA,as.Date(paste(format(as.Date((as.numeric(start_date)+as.numeric(end_date))/2),'%Y-%m'),'-01',sep='')))
			#f1 = as.xts(0,as.Date(paste(format(as.Date((as.numeric(start_date)+as.numeric(end_date))/2),'%Y-%m'),'-01',sep='')))
			#message(f1)
			f2 = f1
			f3 = f1
		}

		RTS_xts_OUT = merge.xts(RTS_xts_OUT,f1)
		SRC_xts_OUT = merge.xts(SRC_xts_OUT,f2)
		CLSS_xts_OUT = merge.xts(CLSS_xts_OUT,f3)
	
	}

	message(paste("Finished rule index:",rule_idx))
	message("")
	list(RTS_xts_OUT,SRC_xts_OUT,CLSS_xts_OUT,CLSS_dict_OUT)
}

fundname_encode = function(str){
	str = gsub('/','ss1',str)
	str = gsub('\\\\','ss2',str)
	str = gsub('\\[','ss3',str)
	str = gsub('\\]','ss4',str)
	str = gsub('\\(','ss3',str)
	str = gsub('\\)','ss4',str)
	str = gsub(',','ss5',str)
	str = gsub('\\.','ss6',str)
	str = gsub("'","ss7",str)
	str = gsub('&','ss8',str)
	str = gsub('%','ss9',str)
	str = gsub('$','ss10',str)
	str = gsub('\\|','ss11',str)
	str = gsub('\\@','ss12',str)
	str = gsub('\\~','ss13',str)
	str = gsub('"','ss14',str)
	str = gsub(' ','ss15',str)
	str = gsub('-','ss16',str)
	tolower(str)
}

#portf_id, ma_portf_id, funds_rules, 2, data.frame(Class=c(),ClassCode=c()), start_date, end_date, "Rule ID", skip_funds
#portfolio_id=portf_id; funds_in=funds_rules; rule_idx=2; clss_dict_in=data.frame(Class=c(),ClassCode=c()); return_db_mode="Rule ID"
#portfolio_id=portf_id; funds_in=funds_rules; rule_idx=2; clss_dict_in=init_classesList; return_db_mode="Rule ID"
load_rule_data_to_XTS = function(params, portfolio_id, ma_portf_id, funds_in, rule_idx, clss_dict_in, start_date, end_date, return_db_mode, skip_funds) {
	message(paste("Starting rule index:",rule_idx))

	RTS_xts_OUT = NULL
	CLSS_xts_OUT = NULL
	SRC_xts_OUT = NULL
	
	CLSS_dict_OUT = clss_dict_in
	
	f = c()
	f1 = c()
	f2 = c()
	f3 = c()
	clss = c()
	clss_codes = c()






















	message('LINEEEEEEEEEEEEE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
	in_skipped = funds_in[,1]%in%skip_funds
	for(i in 1:dim(funds_in)[1]) {
#	for(i in which(!funds_in[i,1]%in%skip_funds)) {
		if(in_skipped[i]) f = array(0,0) else {
			fundname = funds_in[i,1]
			rule = funds_in[i,rule_idx]
			fundname_lock_file = paste(params$base_path,fundname_encode(fundname),rule,sep='and')
			system(paste('lockfile -1 ',fundname_lock_file,'.lock',sep=''))
	
			if(is.na(funds_in[i,rule_idx]) || funds_in[i,rule_idx] == -1)
				f = sqlQuery(pmwAnton,paste("exec [Crestline].[Risk_PortfolioReturns_Get_Fund_Rule_Return_Comment_R] 'aslepnev', '", gsub("'", "''", funds_in[i,1]), "','[VOID]','", format(start_date,"%Y%m%d"), "','", format(end_date,"%Y%m%d"), "', ", ma_portf_id, sep="")) else 
			if (return_db_mode == "Rule comment")
				f = sqlQuery(pmwAnton,paste("exec [Crestline].[Risk_PortfolioReturns_Get_Fund_Rule_Return_Comment_R] 'aslepnev', '", gsub("'", "''", funds_in[i,1]), "','", funds_in[i,rule_idx], "','", format(start_date,"%Y%m%d"), "','", format(end_date,"%Y%m%d"), "', ", ma_portf_id, sep="")) else 
			if (return_db_mode == "Rule ID")
				f = sqlQuery(pmwAnton,paste("exec [Crestline].[Risk_PortfolioReturns_Get_Fund_Rule_Return_R] 'aslepnev', '", gsub("'", "''", funds_in[i,1]), "','", funds_in[i,rule_idx], "','", format(start_date,"%Y%m%d"), "','", format(end_date,"%Y%m%d"), "', ", ma_portf_id, sep=""))

			system(paste('rm -f ',fundname_lock_file,'.lock',sep=''))
		}


		#message(gsub("'","''",funds_in[i,1]))
		#message(paste("exec [Crestline].[Risk_PortfolioReturns_Get_Fund_Rule_Return_R] 'aslepnev', '",gsub("'","''",funds_in[i,1]),"','",funds_in[i,rule_idx],"','",format(start_date,"%Y%m%d"),"','",format(end_date,"%Y%m%d"),"', ",portfolio_id,sep=""))
		#message(f)
		if(!in_skipped[i])
			message(paste("exec [Crestline].[R..R] 'aslepnev', '", gsub("'", "''", funds_in[i,1]), "','", funds_in[i,rule_idx], "','", format(start_date,"%Y%m%d"), "','", format(end_date,"%Y%m%d"), "', ", ma_portf_id, sep=""),' - ',range(as.character(f$ClassCode)))

		# If empty XTS, assign NA/current month
		if(dim(f)[1] != 0) {
#		        while (nrow(f)>1)
#		            {
#		              dub_ndx=which(f$ReturnDate[1:(nrow(f)-1)]==f$ReturnDate[2:nrow(f)])+1
#		              if (length(dub_ndx)) f=f[!(1:nrow(f))%in%dub_ndx,]
#		              else break
#		            }

			f1 = as.xts(f$ReturnValue,as.Date(f$ReturnDate))
			f2 = as.xts(f$ReturnSource,as.Date(f$ReturnDate))
			f3 = as.xts(array(0,length(f$ReturnDate)),as.Date(f$ReturnDate))
			clss = as.character(f$Class)
			clss[is.na(clss)] = '[EMPTY]'
			clss_codes = as.character(f$ClassCode)
			clss_codes[is.na(clss_codes)] = '[EMPTY]'
		
			for(j in 1:length(clss))
				if (clss[j]%in%CLSS_dict_OUT$Class)
					f3[j] = CLSS_dict_OUT$ID[which(CLSS_dict_OUT$Class == clss[j])]
				else {
					CLSS_dict_OUT = rbind(CLSS_dict_OUT,data.frame(ID=c(if(length(CLSS_dict_OUT$ID)>0) max(CLSS_dict_OUT$ID)+1 else 1),Class=c(clss[j]),ClassCode=c(clss_codes[j])))
					f3[j] = max(CLSS_dict_OUT$ID)#length(CLSS_dict_OUT$Class)  # last 2 lines changed on 20121031
				}
		} else {
			#f1 = as.xts(NA,start_date)
			f1 = as.xts(NA,as.Date(paste(format(as.Date((as.numeric(start_date)+as.numeric(end_date))/2),'%Y-%m'),'-01',sep='')))
			#f1 = as.xts(0,as.Date(paste(format(as.Date((as.numeric(start_date)+as.numeric(end_date))/2),'%Y-%m'),'-01',sep='')))
			#message(f1)
			f2 = f1
			f3 = f1
		}

		RTS_xts_OUT = merge.xts(RTS_xts_OUT,f1)
		SRC_xts_OUT = merge.xts(SRC_xts_OUT,f2)
		CLSS_xts_OUT = merge.xts(CLSS_xts_OUT,f3)
	
	}

	message(paste("Finished rule index:",rule_idx))
	message("")
	list(RTS_xts_OUT,SRC_xts_OUT,CLSS_xts_OUT,CLSS_dict_OUT)
}


transform_seqPorttf_to_rulePortf = function(funds_in) {
	res = funds_in[,1:3]
	rule_seqs = funds_in$RuleSequence

	for(i in 1:dim(res)[1]) {
		rule_seq_tab = sqlQuery(pmwAnton,paste("exec Crestline.Risk_PortfolioReturns_GetRuleSequence '", rule_seqs[i],"', null",sep=""))
		message('LINEEEEEEEEEEEEE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
		#message(rule_seq_tab)
		#message(i)
		for(j in 1:dim(rule_seq_tab)[1]) {
			if(dim(res)[2] < 3+j) {
				res[,3+j] = array(NA,dim(res)[1])
			}
			res[i,3+j] = rule_seq_tab$RuleID[j]
		}
	}
	
	res
}

# funds_in=funds_all_cols[,c("FundDescription","RuleSequence")]
transform_seqPorttf_to_rulePortf_SeqMap = function(funds_in) {
	res = data.frame(FundName=funds_in[,"FundDescription"])
	rule_seqs = funds_in$RuleSequence

	last_rule_seq = -1
	rule_seq_tab = data.frame()
	for(i in 1:dim(res)[1]) {
		if(rule_seqs[i] != last_rule_seq){
			message(paste("exec Crestline.Risk_PortfolioReturns_GetRuleSequence null, ", rule_seqs[i],sep=""))
			message('LINEEEEEEEEEEEEE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
			rule_seq_tab = sqlQuery(pmwAnton,paste("exec Crestline.Risk_PortfolioReturns_GetRuleSequence null, ", rule_seqs[i],sep=""))
			last_rule_seq = rule_seqs[i]
		}
		#message(rule_seq_tab)
		#message(i)
		for(j in 1:dim(rule_seq_tab)[1]) {
			if(dim(res)[2] < 1+j) {
				res[,1+j] = array(-1,dim(res)[1])
			}
			res[i,1+j] = rule_seq_tab$RuleID[j]
		}
	}
	
	res
}

#load_funds_rules_Returns(funds_rules_RR, skip_funds_RR, init_classesList_RR, portf_id, ma_portf_id, start_date, end_date, calc_data$Returns_RR, calc_data$Sources_RR, calc_data$Classes_RR, calc_data$FundsList)
#prev_Returns=NULL; prev_Sources=NULL; prev_Classes=NULL; prev_FundsList=NULL
#prev_Returns=calc_data$Returns; prev_Sources=calc_data$Sources; prev_Classes=calc_data$Classes; prev_FundsList=calc_data$FundsList
#funds_rules=funds_rules_RR; skip_funds=skip_funds_RR; init_classesList=init_classesList_RR; prev_Returns=calc_data$Returns_RR; prev_Sources=calc_data$Sources_RR; prev_Classes=calc_data$Classes_RR; prev_FundsList=calc_data$FundsList
#load_funds_rules_Returns(params, funds_rules, skip_funds,
#init_classesList=if(!is.null(calc_data$ClassesList)) init_classesList else data.frame(ID=c(), Class=c(), ClassCode=c())
#, portf_id, ma_portf_id, start_date, end_date,
#prev_returns=calc_data$Returns
#prev_Sources=calc_data$Sources
#prev_Classes=calc_data$Classes
#prev_FundsList=calc_data$FundsList
load_funds_rules_Returns = function(params, funds_rules, skip_funds, init_classesList, portf_id, ma_portf_id, start_date, end_date, prev_Returns, prev_Sources, prev_Classes, prev_FundsList) {
	xtss_list = load_rule_data_to_XTS(params, portf_id, ma_portf_id, funds_rules, 2, init_classesList, start_date, end_date, "Rule ID", skip_funds)
	rts_xts = xtss_list[[1]]
	src_xts = xtss_list[[2]]
	clss_xts = xtss_list[[3]]
	clss_dict = xtss_list[[4]]

	if(dim(funds_rules)[2] > 2)
		for(i in 3:dim(funds_rules)[2]) {
			xtss_list = load_rule_data_to_XTS(params, portf_id, ma_portf_id, funds_rules, i, clss_dict, start_date, end_date, "Rule ID", skip_funds)
			rts_xts = XTS_priority_overlay(rts_xts,xtss_list[[1]])
			src_xts = XTS_priority_overlay(src_xts,xtss_list[[2]])
			clss_xts = XTS_priority_overlay(clss_xts,xtss_list[[3]])
			clss_dict = xtss_list[[4]]
		}

	if (!is.null(prev_Returns)){
		rts_xts = merge.xts(rts_xts,prev_Returns)[,1:dim(rts_xts)[2]]
		src_xts = merge.xts(src_xts,prev_Sources)[,1:dim(src_xts)[2]]
		clss_xts = merge.xts(clss_xts,prev_Classes)[,1:dim(clss_xts)[2]]

		xts_idx = index(rts_xts)[index(rts_xts)>=start_date & index(rts_xts)<=end_date]
		rts_xts = rts_xts[xts_idx,]
		src_xts = src_xts[xts_idx,]
		clss_xts = clss_xts[xts_idx,]
	
		calc_data_rts_xts = prev_Returns[xts_idx,]
		calc_data_src_xts = prev_Sources[xts_idx,]
		calc_data_clss_xts = prev_Classes[xts_idx,]
	
		xts_tmp = xts()
		existing_idx = match(funds_rules$FundName,prev_FundsList)
		skipped_idx = match(skip_funds,prev_FundsList)
		eidx = -1
		for(i in 1:length(existing_idx)){
			eidx = existing_idx[i]

			if(!is.na(eidx) && eidx%in%skipped_idx){
				#rts_xts[,i] = calc_data_rts_xts[,eidx]
				#src_xts[,i] = calc_data_src_xts[,eidx]
				#clss_xts[,i] = calc_data_clss_xts[,eidx]
#print(eidx)

				idx_tmp = is.na(rts_xts[,i])
				rts_xts[idx_tmp,i] = calc_data_rts_xts[idx_tmp,eidx]
				src_xts[idx_tmp,i] = calc_data_src_xts[idx_tmp,eidx]
				clss_xts[idx_tmp,i] = calc_data_clss_xts[idx_tmp,eidx]
			}
		}
	}

	for(i in 1:dim(rts_xts)[2])
		rts_xts[,i][which(is.na(rts_xts[,i]))] = 0

	res_out = hash()
	res_out$Returns = rts_xts
	res_out$Sources = src_xts
	res_out$Classes = clss_xts
	res_out$ClassesList = data.frame(ID=clss_dict$ID,Name=clss_dict$Class,Code=clss_dict$ClassCode)
	res_out
}


#calc_data=tmp_portf; calc_mode = 'Allocations'
#calc_data=NULL; calc_mode = 'Returns'
#calc_data=calc_data_next; calc_mode = 'Returns'
#calc_mode = 'Returns'
#calc_mode = 'Allocations'
#load_all_rules_data_to_XTS_DB_SeqMap(params,calc_data,'Allocations')
load_all_rules_data_to_XTS_DB_SeqMap = function(params, calc_data, calc_mode) {
	portf_id = params$PortfolioID 
	ma_portf_id = params$MA_PortfolioID 
	start_date_PARAM = params$rtStart
	end_date = params$rtEnd
	windows = params$Windows

	default_seq_id = params$DefaultSequenceID
	fund_seq_map_id = params$FundSequenceMappingID

	#message(windows)


	start_date = end_date
	if('120'%in%windows) 
		start_date = end_date - 366*10 else 
	if('60'%in%windows) 
		start_date = end_date - 366*5 else 
	if('36'%in%windows) 
		start_date = end_date - 366*3 else 
	if('24'%in%windows) 
		start_date = end_date - 366*2 else 
	if('12'%in%windows) 
		start_date = end_date - 366 else 
		start_date = as.Date(paste(format(end_date,'%Y'),'-01-01',sep=''))

	if(start_date_PARAM < start_date || (!is.null(params$Force_StartDate) && params$Force_StartDate==1))
		start_date = start_date_PARAM

	start_date = as.Date(paste(format(start_date,'%Y'),'-',format(start_date,'%m'),'-01',sep=''))
	end_date = as.Date(paste(format(end_date,'%Y'),'-',format(end_date,'%m'),'-01',sep=''))

	res = hash()



	funds_hist_seq = if(calc_mode == 'Allocations' && (params$AllocationType == 'Last date' || params$AllocationType == 'XML some dates')) calc_data$GetPortfolio_Data  else if(!is.null(calc_data$XML_Rules_Table)) NULL else{
#		getportf_sql = paste("exec Crestline.Risk_PortfolioReturns_GetPortfolio null,", portf_id,",'", start_date,"','", end_date,"',", default_seq_id,",", fund_seq_map_id,  sep="")
		getportf_sql = paste("exec Crestline.Risk_PortfolioReturns_GetPortfolio null,", portf_id,",'", start_date,"','", params$AllocationDate,"',", default_seq_id,",", fund_seq_map_id,  sep="")
		getPortfolio_data = sqlQuery(pmwAnton, getportf_sql)
		message('LINEEEEEEEEEEEEE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
		message(getportf_sql)
		res$GetPortfolio_Data = getPortfolio_data
		getPortfolio_data 
	} 


	funds_all_cols = if(!is.null(calc_data$XML_Rules_Table)) NULL else unique(funds_hist_seq[,c("FundName","RuleSequence","FundDescription")])


	funds_rules = if(calc_mode == 'Allocations' && (params$AllocationType == 'Last date' || params$AllocationType == 'XML some dates'))
          calc_data$GetPortfolio_Funds_Rules else
           if(!is.null(calc_data$XML_Rules_Table)) calc_data$XML_Rules_Table else
             transform_seqPorttf_to_rulePortf_SeqMap(funds_all_cols[,c("FundDescription","RuleSequence")])
        
	res$GetPortfolio_Funds_Rules = funds_rules



	if(calc_mode == 'FundsList'){
		res$FundsList = funds_rules$FundName		
	} else if(calc_mode == 'Allocations'){
		message('Calculating allocations..N E W')

		funds_rules = if (!is.null(calc_data$XML_Rules_Table)) cbind(funds_rules, data.frame(FundDescription=funds_rules$FundName)) else cbind(funds_rules, data.frame(FundDescription=funds_rules$FundName))

		rts_xts = calc_data$Returns
		xts_idx = as.Date(index(rts_xts))

		alloc_xts = rts_xts
		alloc_xts[,] = 0
		realized_xts = alloc_xts
		xts_i = c()

		if(params$AllocationType == 'Last date' && !is.null(calc_data$XML_Allocs)){
			last_date_allocs = calc_data$XML_Allocs 
			match_f = match(last_date_allocs$FundName,funds_rules$FundName)
			for(i in 1:dim(last_date_allocs)[1]){
				#alloc_xts[,match_f[i]] = last_date_allocs$Allocation[i]

				alloc_xts[params$EndDate,match_f[i]] = last_date_allocs$Allocation[i]
				#alloc_xts[params$AllocationDate,match_f[i]] = last_date_allocs$Allocation[i]


				#realized_xts[,match_f[i]] = last_date_allocs$Allocation[i]
			}
		} else if(params$AllocationType == 'XML some dates' && !is.null(calc_data$XML_Allocs)){

			#realized_xts[,] = 0	
			all_date_allocs = calc_data$XML_Allocs
			allocs_idx = as.Date(colnames(all_date_allocs[-1]))
			#allocs_idx = allocs_idx[allocs_idx%in%index(alloc_xts)]
			#match_d = match(allocs_idx,xts_idx)
			match_f = match(all_date_allocs$FundName,funds_rules$FundName)

			#alloc_xts[allocs_idx,match_f] = t(all_date_allocs[,-1][,allocs_idx%in%index(alloc_xts)])
			alloc_xts[allocs_idx,match_f] = if(ncol(all_date_allocs)==2) all_date_allocs[,-1] else t(all_date_allocs[,-1][,allocs_idx%in%index(alloc_xts)])
			storage.mode(alloc_xts)="double"
		} else {
			funds_hist_seq = funds_hist_seq[order(funds_hist_seq$AsOf_Date,funds_hist_seq$FundName),]
			funds_hist_seq$AsOf_Date = as.Date(funds_hist_seq$AsOf_Date)
	
			match_d = match(funds_hist_seq$AsOf_Date,xts_idx)
			match_f = match(funds_hist_seq$FundDescription,funds_rules$FundName)
			for(i in 1:dim(funds_hist_seq)[1])
				if(!is.na(match_d[i])){
				    #xts_i = which(xts_idx == as.Date(funds_hist_seq$AsOf_Date[i]))
				    #xts_j =  which(funds_rules$FundName == funds_hist_seq$FundName[i])
				    #alloc_xts[xts_i,xts_j] = funds_hist_seq$Allocation[i]
				    #realized_xts[xts_i,xts_j] = funds_hist_seq$RealizedReturn[i]
			    
				    alloc_xts[match_d[i],match_f[i]] = funds_hist_seq$Allocation[i]
				    realized_xts[match_d[i],match_f[i]] = funds_hist_seq$RealizedReturn[i]
				}
		}   

		message('Finishing alloc calc')

		res$Allocations = alloc_xts
		res$RealizedReturns = realized_xts

		adates=index(alloc_xts)
		adates=adates[apply(alloc_xts,FUN=sum,MARGIN=1)!=0]
		res$adates=adates

		res$FundsList = funds_rules$FundName
	} else if (calc_mode == 'Returns'){
		#skip_funds = if(!is.null(calc_data$XML_Rules_Table)) {
		skip_funds = as.character(if(!is.null(calc_data$Funds)) {
				tmp_tab_old = calc_data$Funds[,colnames(calc_data$Funds)[colnames(calc_data$Funds)!='FundDescription']]
				old_list = unlist(lapply(1:dim(tmp_tab_old)[1],function(i){paste(as.matrix(tmp_tab_old[i,]),collapse='-')}))

				tmp_tab_new = funds_rules[,colnames(funds_rules)[colnames(funds_rules)!='FundDescription']]
				new_list = unlist(lapply(1:dim(tmp_tab_new)[1],function(i){paste(as.matrix(tmp_tab_new[i,]),collapse='-')}))

				funds_rules$FundName[new_list%in%old_list]} else c())
		skip_funds_RR = as.character(funds_rules$FundName[funds_rules$FundName%in%calc_data$FundsList])
		if (params$CacheInvalidateLevel>=1)
			skip_funds_RR = skip_funds_RR[!skip_funds_RR%in%c('Cash - USD','CASH')]
		skip_funds_AR = as.character(funds_rules$FundName[funds_rules$FundName%in%calc_data$FundsList])

		init_classesList = calc_data$ClassesList
		if(!is.null(calc_data$ClassesList)) colnames(init_classesList) = c('ID','Class','ClassCode')
		init_classesList_RR = calc_data$ClassesList_RR
		if(!is.null(calc_data$ClassesList_RR)) colnames(init_classesList_RR) = c('ID','Class','ClassCode')
		init_classesList_AR = calc_data$ClassesList_AR
		if(!is.null(calc_data$ClassesList_AR)) colnames(init_classesList_AR) = c('ID','Class','ClassCode')

		res_main = load_funds_rules_Returns(params, funds_rules, skip_funds, if(!is.null(calc_data$ClassesList)) init_classesList else data.frame(ID=c(), Class=c(), ClassCode=c()), portf_id, ma_portf_id, start_date, end_date, calc_data$Returns, calc_data$Sources, calc_data$Classes, calc_data$FundsList)

		RR_Rule = sqlQuery(pmwAnton,"[Crestline].[Risk_PortfolioReturns_GetRuleSequence] 'Realized Return'")
		AR_Rule = sqlQuery(pmwAnton,"[Crestline].[Risk_PortfolioReturns_GetRuleSequence] 'Attrib Return'")
		Cash_RR_Rule = sqlQuery(pmwAnton,"[Crestline].[Risk_PortfolioReturns_GetRuleSequence] 'Cash = Libor'")

		rules_RR_tab = t(matrix(-1,max(dim(RR_Rule)[1],dim(Cash_RR_Rule)[1]),dim(funds_rules)[1]))
		for(i in which(!funds_rules[,'FundName']%in%c('Cash - USD','CASH'))) 
			rules_RR_tab[i,1:length(RR_Rule$RuleID)] = RR_Rule$RuleID
		for(i in which(funds_rules[,'FundName']%in%c('Cash - USD','CASH')))
			if (params$CashRRMode==1)
				rules_RR_tab[i,1:length(Cash_RR_Rule$RuleID)] = Cash_RR_Rule$RuleID else
			rules_RR_tab[i,1:length(RR_Rule$RuleID)] = RR_Rule$RuleID
		funds_rules_RR = cbind(data.frame(FundName=funds_rules[,'FundName'],stringsAsFactors=FALSE),rules_RR_tab)

		res_RR = load_funds_rules_Returns(params, funds_rules_RR, skip_funds_RR, if(!is.null(calc_data$ClassesList_RR)) init_classesList_RR else data.frame(ID=c(), Class=c(), ClassCode=c()), portf_id, ma_portf_id, start_date, end_date, calc_data$Returns_RR, calc_data$Sources_RR, calc_data$Classes_RR, calc_data$FundsList)


		rules_AR_tab = t(matrix(-1,dim(AR_Rule)[1],dim(funds_rules)[1]))
		for(i in 1:nrow(funds_rules)) 
			rules_AR_tab[i,1:length(AR_Rule$RuleID)] = AR_Rule$RuleID
		funds_rules_AR = cbind(data.frame(FundName=funds_rules[,'FundName'],stringsAsFactors=FALSE),rules_AR_tab)

		res_AR = load_funds_rules_Returns(params, funds_rules_AR, skip_funds_AR, if(!is.null(calc_data$ClassesList_AR)) init_classesList_AR else data.frame(ID=c(), Class=c(), ClassCode=c()), portf_id, ma_portf_id, start_date, end_date, calc_data$Returns_AR, calc_data$Sources_AR, calc_data$Classes_AR, calc_data$FundsList)

		res$Returns = res_main$Returns
		res$Sources = res_main$Sources
		res$Classes = res_main$Classes
		res$ClassesList = res_main$ClassesList

		res$Returns_RR = res_RR$Returns
		res$Sources_RR = res_RR$Sources
		res$Classes_RR = res_RR$Classes
		res$ClassesList_RR = res_RR$ClassesList


		res$Returns_AR = res_AR$Returns
		res$Sources_AR = res_AR$Sources
		res$Classes_AR = res_AR$Classes
		res$ClassesList_AR = res_AR$ClassesList

		comps = sqlQuery(pmwAnton,"select * from Crestline.Risk_PortfolioReturns_RuleComponents")
		res$SourcesList = data.frame(ID=1:dim(comps)[1], Name=as.character(comps$ComponentName), Type=as.character(comps$ComponentType))

		res$Funds = if (!is.null(calc_data$XML_Rules_Table)) cbind(funds_rules, data.frame(FundDescription=funds_rules$FundName)) else cbind(funds_rules, data.frame(FundDescription=funds_all_cols$FundDescription))
		res$FundsList = funds_rules$FundName

		res$Funds_Codes = data.frame(FundName=funds_rules$FundName,FundCode=array('',length(funds_rules$FundName)),FundTicker=array('',length(funds_rules$FundName)),stringsAsFactors=FALSE)
		for(i in 1:dim(res$Funds_Codes)[1]){
#print(i)
			res$Funds_Codes[i,'FundCode'] = as.character(sqlQuery(pmwAnton,paste("select isnull((select max(Mapping_FUND_CODE_LINE) from Crestline.Risk_PortfolioReturns_FundsNameMapping where NAME='",gsub("'", "''", res$Funds_Codes[i,'FundName']) ,"'),'[EMPTY]') as FundCode",sep=''))$FundCode)
                        res$Funds_Codes[i,'FundTicker'] = as.character(sqlQuery(pmwAnton,paste("select isnull((select max(Mapping_FUND_CODE_CLASSIFICATION) from Crestline.Risk_PortfolioReturns_FundsNameMapping where NAME='",gsub("'", "''", res$Funds_Codes[i,'FundName']) ,"'),'[EMPTY]') as FundCode",sep=''))$FundCode)
}


		sl=res$SourcesList
		sl$Label='NA'
		sl$Label[sl$Type=='PORTFOLIO' & sl$Name!='SAME PORTFOLIO']='Line'
		sl$Label[sl$Type=='PORTFOLIO' & sl$Name=='SAME PORTFOLIO']='Portfolio'
		sl$Label[sl$Type=='BENCHMARK' & sl$Name=='0 const']='Zero'
		sl$Label[sl$Type=='BENCHMARK' & sl$Name=='Zero']='Zero'
		sl$Label[sl$Type=='BENCHMARK' & sl$Name=='CASH']='Cash'
		sl$Label[sl$Type=='BENCHMARK' & sl$Name=='RISK_BENCHMARK']='Benchmark'
		sl$Label[sl$Type=='BENCHMARK'   & sl$Name=='SPECIFIC_BENCHMARK']='Benchmark'
		sl$Label[sl$Type=='STRATEGY_BENCHMARK']='Benchmark'
		sl$Label[sl$Type=='PERTRAC'] = 'Pertrac'

		#sl$Label[sl$Type=='PERTRAC']='PERTRAC'
		#sl$Label[sl$Type=='PERTRAC' & sl$Type=='RISK_FUND']='Pertrac '
		#sl$Label[sl$Type=='PERTRAC' & sl$Type=='RISK_FUND']='Pertrac'

		res$SourcesList = sl

		labels = c('Portfolio','Line','Pertrac','Benchmark','Cash','Zero','NA')
		#colors = rev(rainbow(length(labels)))
		colors = c('#00FF00','#AAAAFF','#0000FF','#FFFF00','#FFAA00','#FF0000','#000000')
		
		colors[length(labels)] = '#000000'

		message("Colors ::")		
		message(rainbow(length(labels)))
		message(rev(rainbow(length(labels))))
		message(colors)

		res$ColorMap = data.frame(Label=labels, Color=colors, stringsAsFactors = FALSE)

		message(res$ColorMap)		
		message("Colors //")		
	}

	res
}

load_all_rules_data_to_XTS_FundsListMode = function(params, calc_data){
	load_all_rules_data_to_XTS_DB_SeqMap(params,calc_data,'FundsList')
}

load_all_rules_data_to_XTS_ReturnsMode = function(params, calc_data){
	load_all_rules_data_to_XTS_DB_SeqMap(params,calc_data,'Returns')
}

load_all_rules_data_to_XTS_AllocationsMode = function(params, calc_data){
	load_all_rules_data_to_XTS_DB_SeqMap(params,calc_data,'Allocations')
}

#file_id=desc
enter_SyncExec = function(file_id,params) {
	file_path = paste(params$base_path,file_id,sep='')
        
	data_filename = paste(file_path,'.RData',sep='')
	data_lock_filename = paste(file_path,'_DATA.lock',sep='')

	count_filename = paste(file_path,'.count',sep='')
	count_lock_filename = paste(file_path,'_COUNT.lock',sep='')

        message('Locking ',count_lock_filename)

	system(paste('lockfile -1 ',count_lock_filename,sep=''))

	if(file.exists(count_filename))
		message(paste('Count:',as.numeric(readLines(count_filename)[1])))

	if(!file.exists(count_filename)){
		message("Count file doesn't exist")
		count_con = file(count_filename,open='w')
		writeLines('1',count_con)
		close(count_con)
	} else {
		message("Count file exists")
		count_con = file(count_filename,open='r+')
		ct = as.numeric(readLines(count_con)[1])
		writeLines(paste(ct + 1),count_con)
		close(count_con)
	}

	message(paste('Count++1:',as.numeric(readLines(count_filename)[1])))

	system(paste('rm -f ',count_lock_filename,sep=''))

	message("WAITING!!")

	system(paste('lockfile -1 ',data_lock_filename,sep=''))

	data_filename
}

exit_SyncExec = function(file_id,params) {
	file_path = paste(params$base_path,file_id,sep='')

	data_filename = paste(file_path,'.RData',sep='')
	data_lock_filename = paste(file_path,'_DATA.lock',sep='')

        #message('Lock File:',data_lock_filename)
        #message('Data File:',data_filename)
        

	count_filename = paste(file_path,'.count',sep='')
	count_lock_filename = paste(file_path,'_COUNT.lock',sep='')

	system(paste('rm -f ',data_lock_filename,sep=''))
	system(paste('lockfile -1 ',count_lock_filename,sep=''))

	if(file.exists(count_filename))
		message(paste('Count:',as.numeric(readLines(count_filename)[1])))

	if(!file.exists(count_filename))
		writeLines('1',count_filename)
	else {
		ct = as.numeric(readLines(count_filename)[1])
		writeLines(paste(ct - 1),count_filename)
	}

	#message(paste('Count--1:',as.numeric(readLines(count_filename)[1])))


	if(readLines(count_filename)[1] == 0) {
		unlink(count_filename)
		if(params$CacheMode=='LIVE') {
			message("Deleting..")
			unlink(data_filename)
		}
	}	

	system(paste('rm -f ',count_lock_filename,sep=''))
}


#calc_data=tmp_portf; calc_func=load_all_rules_data_to_XTS_AllocationsMode
#calc_name='Bla'
syncronized_Calc_Read = function(calc_func, params, calc_data, calc_name, force_live, do_recalc) {
	desc = params2id(params,calc_name)
        data_filename = enter_SyncExec(desc,params)

	message('Searching for file: ',data_filename)
	h = hash()
        h$Data=NULL
	h$Parameters = params

	if(do_recalc && (force_live || !file.exists(data_filename))) {
		message("Calculating..")
                h$Data=tryCatch({calc_func(params, calc_data)
                        }, error=function(ex){message(paste('Error in calculation',desc,ex,sep=', '))})
		if(!is.null(h$Data))
	                save(h,file=data_filename)
	} else {
          message("Loading..")
	  if (file.exists(data_filename))
		load(data_filename)

	  if(do_recalc && is.null(h$Data)){
		message('Data loaded from ',desc,' is NULL, re-calculating..')
                h$Data=tryCatch({calc_func(params, calc_data)
                        }, error=function(ex){message(paste('Error in calculation',desc,ex,sep=', '))})
		if(!is.null(h$Data))
	                save(h,file=data_filename)
	  }
	}

	exit_SyncExec(desc,params)
	h
}


#startdate=start_date
#enddate=end_date
params2id <- function (params,calc_name)
  {
#    ks=c('PortfolioID','MA_PortfolioID','DefaultSequenceID','FundSequenceMappingID','rtStart','rtEnd','saveEnd','saveStart')
    ks=c('PortfolioID','MA_PortfolioID','DefaultSequenceID','FundSequenceMappingID','rtStart','rtEnd')

    if(calc_name%in%c('ReturnsTable_FundReturns','ReturnsTable_StrategyReturns')) ks = c(ks,'AllocationDate','EndDate')
    if(calc_name%in%c('ReturnsTable_FundsList')) ks = c(ks,'AllocationDate')

#    message('PARAMS2ID calculation. calc_name: ', calc_name,'params$CacheMode: ',params$CacheMode)

    if (params$CacheMode=='LIVE'
	|| params$CacheMode=='FULL'
	|| (params$CacheMode=='ALLOC_ONLY' && calc_name=='PortfolioAllocations')
	|| (params$CacheMode=='ALLOC_ONLY' && calc_name=='ReturnsTable_StrategyReturns')
	|| (params$CacheMode=='LAST_6M' && calc_name=='PortfolioReturns_last6m')
	|| (params$CacheMode=='LAST_6M' && calc_name=='ReturnsTable_FundReturns')
	|| (params$CacheMode=='LAST_6M' && calc_name=='ReturnsTable_StrategyReturns')
	|| (params$CacheMode!='CACHE' && calc_name == 'Portfolio_FundsList')
	|| (params$CacheMode!='CACHE' && calc_name == 'ReturnsTable_FundsList')
	)
      {
	message('LIVE calculation!')
        ks <- c(ks,'SessionID')
      }

    ks <- ks[!is.na(match(ks,keys(params)))]
        
    desc = paste(as.character(values(params,keys=ks)),collapse='_')
    desc = paste(calc_name,'_',desc)
    desc = gsub(' ','',desc)
    desc = gsub('\\(','',desc)
    desc = gsub('\\)','',desc)
    desc = gsub('"','',desc)
    desc = gsub(',','_',desc)
    return(desc)
  }

# calc_data = NULL; calc_func = load_all_rules_data_to_XTS_ReturnsMode; calc_name = 'PortfolioReturns'; funds_list = funds_data_tab; try_MA=TRUE
# calc_data = NULL; calc_func = load_all_rules_data_to_XTS_ReturnsMode; calc_name = 'PortfolioReturns'; funds_list = funds_data_tab; try_MA=FALSE
# calc_data = NULL; calc_func = load_all_rules_data_to_XTS_ReturnsMode; calc_name = 'PortfolioReturns'; funds_list = funds_data_tab; try_MA=FALSE
# calc_data = tmp_portf; calc_func = load_all_rules_data_to_XTS_AllocationsMode; calc_name = 'PortfolioAllocations'; funds_list = funds_data_tab; try_MA=FALSE
# calc_data = ard; calc_func = get_fund_returns_table_XTS_FundReturns; calc_name = 'ReturnsTable_FundReturns'; funds_list = new_funds_list$FundsList; try_MA=FALSE
get_cached_data_FundsListBased = function(calc_func, params, calc_data, calc_name, funds_list, try_MA) {
	desc_fl = paste(params2id(params,calc_name),'_Check_FundsList',sep='')
        enter_SyncExec(desc_fl,params)

	tryCatch({
		res = if(try_MA) syncronized_Calc_Read(calc_func, params, calc_data, calc_name, FALSE, FALSE)$Data else NULL
		message('Trying MA returns..')
		res = if(is.null(res) && try_MA){
				params_MA = copy(params)
				params_MA$FundSequenceMappingID = params$Default_FundSequenceMappingID
				params_MA$PortfolioID = params$MA_PortfolioID
				syncronized_Calc_Read(calc_func, params_MA, calc_data, calc_name, FALSE, FALSE)$Data} else res
		message('MA result is NULL? ',is.null(res))
		res = if(is.null(res)) syncronized_Calc_Read(calc_func, params, calc_data, calc_name, FALSE, TRUE)$Data else res

		calc_data_next = if(is.null(calc_data)) res else calc_data

		res_funds_list = if(class(funds_list)=='data.frame'){
					tmp_tab = res$Funds[order(res$Funds$FundName),colnames(res$Funds)[which(colnames(res$Funds)!='FundDescription')]]
					unlist(lapply(1:dim(tmp_tab)[1],function(i){paste(as.matrix(tmp_tab[i,]),collapse='-')}))
				} else res$FundsList[order(res$FundsList)]
		calc_funds_list = if(class(funds_list)=='data.frame'){
					tmp_tab = funds_list[order(funds_list$FundName),colnames(funds_list)[which(colnames(funds_list)!='FundDescription')]]
					calc_data_next$XML_Rules_Table = tmp_tab
					unlist(lapply(1:dim(tmp_tab)[1],function(i){paste(as.matrix(tmp_tab[i,]),collapse='-')}))
				} else funds_list[order(funds_list)]

		if(length(res_funds_list)!=length(calc_funds_list) || min(res_funds_list==calc_funds_list)==0 || params$CacheInvalidateLevel>=1){
			message('FundsList-s dont match, re-calculating.. ', calc_name,', ',length(res_funds_list)!=length(calc_funds_list),', ',paste(res_funds_list==calc_funds_list,res_funds_list,calc_funds_list),', ',min(res_funds_list==calc_funds_list),', ',min(res_funds_list!=calc_funds_list))

	#stop('Stopped 1')
			res = syncronized_Calc_Read(calc_func, params, calc_data_next, calc_name, TRUE, TRUE)$Data
		}
	}, error=function(ex){message(paste('Error in calculation_FundsListBased',ex,sep=', '))})
	

	exit_SyncExec(desc_fl,params)

	res
}

get_returns_Portfolio = function(params) {
  funds_list_updated_flag = FALSE
  alloc_xml_loaded_flag = FALSE

  res = if(params$CacheMode == 'ALLOC_ONLY'){
	xml_str_alloc = as.character(params$AllocationsTableXML)
	xml_str_rules = as.character(params$RulesTableXML)

	string_str_alloc_values = as.character(params$AllocationsValuesString)
	string_str_alloc_dates = as.character(params$AllocationsDatesString)
	string_str_alloc_fundslist = as.character(params$FundnamesString)

	funds_data_tab = data.frame()
	alloc_data_tab = data.frame()

	if(xml_str_rules%in%c('empty','')){
		tmp_funds_list = syncronized_Calc_Read(load_all_rules_data_to_XTS_FundsListMode,params,NULL,'Portfolio_FundsList',FALSE,TRUE)
		funds_data_tab = as.character(tmp_funds_list$Data$FundsList)
		funds_list_updated_flag = TRUE		
	} else {
		a=xmlParse(xml_str_rules)
		funds_data_tab = xmlToDataFrame(a)
		funds_data_tab = transform_seqPorttf_to_rulePortf_SeqMap(funds_data_tab[,c("FundDescription","RuleSequence")])
		funds_data_tab$FundName = gsub('/AMP/','&',funds_data_tab$FundName)
	}


	if(!xml_str_alloc%in%c('empty','')){
		a=xmlParse(xml_str_alloc)
		alloc_data_tab = xmlToDataFrame(a,stringsAsFactors=FALSE)
		alloc_data_tab$FundName = gsub('/AMP/','&',alloc_data_tab$FundName)

		if(params$AllocationType == 'Last date'){
			alloc_data_tab$Allocation = as.double(alloc_data_tab$Allocation) 
		} else if(params$AllocationType == 'XML some dates'){
			colnames(alloc_data_tab)[-1]=as.character(as.Date(unlist(lapply(colnames(alloc_data_tab)[-1], function(x) {gsub("c","",x)})),"%Y%m%d"))
		}

		alloc_xml_loaded_flag = TRUE
	} else if(!string_str_alloc_values%in%c('empty','')){
		alloc_data_tab = cbind(gsub('/AMP/','&',strsplit(string_str_alloc_fundslist,':)')[[1]]),data.frame(t(matrix(as.numeric(strsplit(string_str_alloc_values,':)')	[[1]]),length(strsplit(string_str_alloc_dates,':)')[[1]]),length(strsplit(string_str_alloc_fundslist,':)')[[1]])))),stringsAsFactors=FALSE)
		colnames(alloc_data_tab) = c('FundName',as.character(as.Date(unlist(lapply(strsplit(string_str_alloc_dates,':)')[[1]], function(x) {gsub("c","",x)})),"%Y%m%d")))
		if(params$AllocationType == 'Last date')
			colnames(alloc_data_tab)[2] = 'Allocation'

		alloc_xml_loaded_flag = TRUE
	}


	tmp_portf = get_cached_data_FundsListBased(load_all_rules_data_to_XTS_ReturnsMode, params, NULL, 'PortfolioReturns', funds_data_tab, TRUE)  
	if(alloc_xml_loaded_flag)
		tmp_portf$XML_Allocs = alloc_data_tab
	

	tmp_allocs = syncronized_Calc_Read(load_all_rules_data_to_XTS_AllocationsMode,params,tmp_portf,'PortfolioAllocations',FALSE,TRUE)$Data  


	tmp_portf$Allocations = tmp_allocs$Allocations
	tmp_portf$RealizedReturns = tmp_allocs$RealizedReturns
	
	tmp_portf
  } else if(params$CacheMode == 'LAST_6M'){ 
	new_funds_list = syncronized_Calc_Read(load_all_rules_data_to_XTS_FundsListMode,params,NULL,'Portfolio_FundsList',FALSE,TRUE)$Data

	params_end_date = as.Date(paste(format(params$EndDate,'%Y'),'-',format(params$EndDate,'%m'),'-01',sep=''))

	params_6mo = copy(params)
	params_6mo$rtEnd = seq(params_end_date, length = 2, by = "-7 months")[2]
	portf_6mo = get_cached_data_FundsListBased(load_all_rules_data_to_XTS_ReturnsMode, params_6mo, NULL, 'PortfolioReturns_6mo', new_funds_list$FundsList, FALSE)
	
	params_last6m = copy(params)
	params_last6m$rtStart = seq(params_end_date, length = 2, by = "-6 months")[2]
	params_last6m$Force_StartDate = 1
	portf_last6m = get_cached_data_FundsListBased(load_all_rules_data_to_XTS_ReturnsMode, params_last6m, NULL, 'PortfolioReturns_last6m', new_funds_list$FundsList, FALSE) # BGF 1:04
	
	funds_list_updated_flag = TRUE

	res = copy(portf_last6m)
	res$Returns = XTS_priority_overlay(portf_6mo$Returns,portf_last6m$Returns)
	res$Classes = XTS_priority_overlay(portf_6mo$Classes,portf_last6m$Classes)
	res$Sources = XTS_priority_overlay(portf_6mo$Sources,portf_last6m$Sources)

	tmp_allocs = syncronized_Calc_Read(load_all_rules_data_to_XTS_AllocationsMode,params,res,'PortfolioAllocations',FALSE,TRUE)$Data 
	res$Allocations = tmp_allocs$Allocations
	res$RealizedReturns = tmp_allocs$RealizedReturns

	res
  } else if(params$CacheMode == 'CACHE'){ 
	tmp_portf = syncronized_Calc_Read(load_all_rules_data_to_XTS_ReturnsMode,params,NULL,'PortfolioReturns',FALSE,TRUE)$Data
	tmp_allocs = syncronized_Calc_Read(load_all_rules_data_to_XTS_AllocationsMode,params,tmp_portf,'PortfolioAllocations',FALSE,TRUE)$Data

	if(length(tmp_portf$FundsList)!=length(tmp_allocs$FundsList) || min(tmp_portf$FundsList==tmp_allocs$FundsList)==0){
		message('FundsList-s dont match, re-calculating.. ', length(tmp_portf$FundsList),', ',length(tmp_allocs$FundName),', ',tmp_portf$FundsList==tmp_allocs$FundName,', ',min(tmp_portf$FundsList==tmp_allocs$FundName))
		message('res$FundsList: ',paste(tmp_portf$FundsList[1:3],collapse='--'))
		message('funds_list   : ',paste(tmp_allocs$FundName[1:3],collapse='--'))
		stop('FundsLists dont match! First: ',paste(tmp_portf$FundsList,collapse='-'),',      Second: ',paste(tmp_allocs$FundsList,collapse='-'))
	}

	tmp_portf$Allocations = tmp_allocs$Allocations
	tmp_portf$RealizedReturns = tmp_allocs$RealizedReturns

	tmp_portf
  } else if(params$CacheMode %in% c('FULL','LIVE')){ 
	funds_list_updated_flag = TRUE  # Because LIVE by default!

	tmp_portf = syncronized_Calc_Read(load_all_rules_data_to_XTS_ReturnsMode,params,NULL,'PortfolioReturns',FALSE,TRUE)$Data  
	tmp_allocs = syncronized_Calc_Read(load_all_rules_data_to_XTS_AllocationsMode,params,tmp_portf,'PortfolioAllocations',FALSE,TRUE)$Data

	tmp_portf$Allocations = tmp_allocs$Allocations
	tmp_portf$RealizedReturns = tmp_allocs$RealizedReturns

	tmp_portf
  }


  missing_dates = seq(max(index(res$Allocations)),max(params$rtEnd,params$AllocationDate),by='1 month')[-1]
  if(length(missing_dates)>0){
	missing_xts = as.xts(matrix(0,length(missing_dates),dim(res$Allocations)[2]),order.by=missing_dates)
	missing_xts_zero = as.xts(matrix(0,length(missing_dates),dim(res$Allocations)[2]),order.by=missing_dates)
	missing_xts_na = as.xts(matrix(NA,length(missing_dates),dim(res$Allocations)[2]),order.by=missing_dates)
	if(params$AllocationType == 'XML some dates'){
		all_date_allocs = res$XML_Allocs
		allocs_idx = as.Date(colnames(all_date_allocs[-1]))
		allocs_idx_missing = which(allocs_idx%in%missing_dates)
		match_f = match(all_date_allocs$FundName,res$FundsList)

		#missing_xts[allocs_idx,match_f] = if(ncol(all_date_allocs)==2) all_date_allocs[,-1] else t(all_date_allocs[,-1][,allocs_idx%in%index(missing_xts)])
		missing_xts[allocs_idx[allocs_idx_missing],match_f] = if(ncol(all_date_allocs)==2) all_date_allocs[,-1] else t(all_date_allocs[,-1][,allocs_idx_missing])
		storage.mode(missing_xts)="double"
		res$Allocations = rbind(res$Allocations,missing_xts)
	} else	
		res$Allocations = rbind(res$Allocations,missing_xts)

	res$Returns = rbind(res$Returns,missing_xts_zero)
	res$Returns_RR = rbind(res$Returns_RR,missing_xts_zero)
	res$Sources = rbind(res$Sources,missing_xts_na)
	res$Sources_RR = rbind(res$Sources_RR,missing_xts_na)
	res$Classes = rbind(res$Classes,as.xts(t(matrix(res$Classes[dim(res$Classes)[1],],dim(res$Allocations)[2],length(missing_dates))),order.by=missing_dates))
	res$Classes_RR = rbind(res$Classes_RR,as.xts(t(matrix(res$Classes_RR[dim(res$Classes_RR)[1],],dim(res$Allocations)[2],length(missing_dates))),order.by=missing_dates))	
  }
 

  allocSum=apply(X=abs(res$Allocations),MARGIN=1,FUN=sum)
  ndx=isnull(min(which(allocSum>0)),0)
  
  if (ndx==0)
    {
      stop('get_returns_Portfolio:: Empty portfolio!')
    }
  res$StartAllocNdx=ndx
  res$FundsList_Updated = funds_list_updated_flag
  res$AllocXML_Loaded = alloc_xml_loaded_flag
  res$FundsList_Updated = TRUE


  return(res)
}



#params=ard_params_tmp; ard=ard_in
get_returns_Table = function(params, ard) {
#  newParams=params
#  newParams$saveEnd=params$EndDate
#  newParams$saveStart=params$StartDate  # - why use newParams variable, while these change params variable as well?
#  params=ard_params_tmp

#stop('Stopped 1')

  new_funds_list = #if(ard$FundsList_Updated) data.frame(FundsList=ard$FundsList) else 
	if(is.null(params$AllocationsTableXML) || as.character(params$AllocationsTableXML)=='empty') syncronized_Calc_Read(get_fund_returns_table_XTS_FundsList, params, ard, 'ReturnsTable_FundsList', FALSE, TRUE)$Data  else if(ard$AllocXML_Loaded) data.frame(FundsList=(ard$XML_Allocs)$FundName) else {
	xml_str=as.character(params$AllocationsTableXML)
message(xml_str)
	a=xmlParse(xml_str)	
#	a=xmlParse(as.character(params$AllocationsTableXML))	
	data.frame(FundsList=gsub('/AMP/','&',xmlToDataFrame(a)$FundName))
  }

#message('funds list in the middle of get_returns_Table: ',paste(new_funds_list$FundsList,collapse='--'))
#message('funds list names in the middle of get_returns_Table: ',paste(names(new_funds_list),collapse='--'))

  tmp_funds_tab = if(params$CacheMode == 'CACHE') syncronized_Calc_Read(get_fund_returns_table_XTS_FundReturns, params, ard, 'ReturnsTable_FundReturns', FALSE, TRUE)$Data  else
	get_cached_data_FundsListBased(get_fund_returns_table_XTS_FundReturns, params, ard, 'ReturnsTable_FundReturns', new_funds_list$FundsList, FALSE)  

  tmp_strategy_tab = get_cached_data_FundsListBased(get_fund_returns_table_XTS_StrategyReturns, params, ard, 'ReturnsTable_StrategyReturns', new_funds_list$FundsList, FALSE)

  tmp_funds_tab$ReturnsTable[,'Allocation'] = tmp_strategy_tab$Fund_allocs

  res_tab = rbind(tmp_funds_tab$ReturnsTable, tmp_strategy_tab$ReturnsTable)

  if(length(grep('ytd_',names(res_tab)))>0)
	res_tab = res_tab[,-grep('ytd_',names(res_tab))]

  res_cols = names(res_tab)[-which(names(res_tab)=='From0_Dr')]
  if(length(grep('From0_',names(res_tab)))>0)
	res_tab = res_tab[,-which(names(res_tab)%in%res_cols[grep('From0',res_cols)])]

  res_tab = res_tab[order(res_tab$Class!='Total',res_tab$Class,res_tab$Name!=res_tab$Class),]

  res = tmp_strategy_tab  
  res$ReturnsTable = res_tab
	
  return(res)
}

get_proforma_Returns = function(params, ard) {
  return(syncronized_Calc_Read(roll_proformas, params, ard,'ProformaReturns',FALSE,TRUE)$Data)
}

#func=get_index_func;params=ndx;calc_params=NULL;cache=paste(name,'_BENCH_INDEX',sep='')
get_cached_calcs = function(func, params, calc_params,cache) {
  return(syncronized_Calc_Read(func,params,calc_params,cache,FALSE,TRUE)$Data)
}

#params=ndx

get_index_func <- function (params,calc_params)
  {
    load_benchmarks_xts(c(params$benchmark),'1900-01-01','9999-12-31')
  }

#name='HFRIFOFC_INDEX'

get_cached_index <- function (params,name)
  {
    ndx <- hash()
    ndx$benchmark=name
    ndx$SessionID=params$SessionID
    ndx$CacheMode=params$CacheMode
    ndx$base_path=params$base_path

    get_cached_calcs(get_index_func,ndx,NULL,paste(name,'_BENCH_INDEX',sep=''))
  }


get_index_pack <- function(params,calc_params)
{
  brt=xts()
  for (i in 1:length(calc_params))
    {
      ndx <- hash()
      ndx$benchmark=calc_params[i]
      ndx$SessionID=params$SessionID
      ndx$CacheMode=params$CacheMode
      ndx$base_path=params$base_path
      
      rt=get_cached_calcs(get_index_func,ndx,NULL,paste(calc_params[i],'_BENCH_INDEX',sep=''))
      brt=cbind(brt,rt)
    }

  brt=na.omit(brt)
}

get_index_pack_WithNA <- function(params,calc_params)
{
  brt=xts()
  for (i in 1:length(calc_params))
    {
      ndx <- hash()
      ndx$benchmark=calc_params[i]
      ndx$SessionID=params$SessionID
      ndx$CacheMode=params$CacheMode
      ndx$base_path=params$base_path
      
      rt=get_cached_calcs(get_index_func,ndx,NULL,paste(calc_params[i],'_BENCH_INDEX',sep=''))
      brt=cbind(brt,rt)
    }

  brt
}

#get_cached_index_pack(params,bench_codes,'FUND_FLAGS_BNCH_HISTORY')
get_cached_index_pack <- function(params, pack, pack_name, get_bnch_func=get_index_pack)
{
  ndx <- hash()
  ndx$SessionID=params$SessionID
  ndx$CacheMode=params$CacheMode
  ndx$base_path=params$base_path
  
  get_cached_calcs(get_bnch_func,ndx,pack,paste(pack_name,'_BENCH_INDEX',sep=''))
}
 
                    

#fun=betaFUN
#uwindows=c(12)
#ylabel='annualized.Return'
#cache='TEST'
#multiplier=100
#xml_roll_graph(params,ard,annRetFUN,'Annualized.Return',return_id,100)

#graph rolling portfolio level statistics
#          XML_RES=paste(XML_RES,xml_roll_graph(params,ard,corrFUN,paste('Correlation to ',params$benchmark,sep=''),paste(params$benchmark,'_',return_id,sep=''),100))
#fun=corrFUN
#ylabel=paste('Correlation to ',params$benchmark,sep='')
#cache=paste(params$benchmark,'_',return_id,sep='')
#multiplier=100,mark_significance=0,plot_range=0,uwindows=NULL
#params,ard,betaFUN,paste('Beta to ',params$benchmark,sep=''),paste(params$benchmark,'_',return_id,sep=''),100,1,0,uwindows
#fun=betaFUN
#cache='blaaa'
#label='blaaa'
#windows=uwindows
xml_roll_graph <- function (params,ard,fun,ylabel,cache,multiplier=100,mark_significance=0,plot_range=0,uwindows=NULL,show_table=FALSE,xml_name=NULL,end_date=NULL)
  {
    a=roll_proformas(params,ard)
    rollRt=a
    if (params$fixed_allocation)
      {
        b=proforma_port_full(ard,params$AllocationDate)
        rollRt=b
        cache=paste(cache,'.Fixed.',as.integer(params$AllocationDate),sep='')
      }
    calc_params=hash()
    calc_params$rollFun=fun
    calc_params$rollRt=rollRt
    if (is.null(uwindows))
      {
        uwindows=std_roll_windows
      }
    if (!is.null(params$benchmark)) # have a benchmark to run against
      {
        calc_params$indexRt=get_cached_index(params,params$benchmark)
      }
    
    res=xts()
    low=xts()
    high=xts()
    significant=xts()
    for (w in uwindows)
      {
        calc_params$w=w
        annRt=calcRollingMeasure(params, calc_params)
        res=cbind(res,annRt$measure)
        low=cbind(low,annRt$low)
        high=cbind(high,annRt$high)
        significant=cbind(significant,annRt$significant)
      }
    
    colnames(res)=paste(uwindows,'M',sep='')
    colnames(low)=paste('99% LOW ',colnames(res),sep='')
    colnames(high)=paste('99% HIGH ',colnames(res),sep='')
    colnames(significant)=colnames(significant)

    end_date=isnull(end_date,params$EndDate)
    

    res=res[as.Date(index(res))<=end_date & as.Date(index(res))>=params$StartDate,]
    low=low[as.Date(index(low))<=end_date  & as.Date(index(low))>=params$StartDate,]
    high=high[as.Date(index(high))<=end_date  & as.Date(index(high))>=params$StartDate, ]
    significant=significant[as.Date(index(significant))<=end_date  & as.Date(index(significant))>=params$StartDate,]

    plt=res
    colors=rainbow(ncol(plt))
    if (plot_range)
      {
        plt=merge(res,low,high)
        colnames(plt)=c(colnames(res),colnames(low),colnames(high))
        colors=c('green','blue','red')
      }
    
    message('Opening file:xml_roll_graph:',cache)
    plot_filename = open_file_for_binaryImage(png,graphW,graphH)
    empty <- FALSE
    if (sum(!is.na(plt)))
      {
        dc=CLTimeSeries(plt*multiplier,ylab=ylabel,main='',lwd=3,colors=colors,legend.loc='topleft',xlab=NULL)
    
        if (mark_significance)
          {
            i <- 1
            for (i in 1:ncol(res))
              {
                x=which(isnull(significant[,i],0)!=0)
                if (isnull(length(x),0))
                  {
                    points(x,res[x,i]*multiplier,pch=21,col=colors[i],bg=colors[i])
                  }
                
              }
          }
        

      } else
    {
      emptyPlot()
      empty <- TRUE
    }
    
        
        wlabel=ylabel
        if (plot_range)
          {
            wlabel=paste(uwindows[1],'M ',ylabel,' significance interval',sep='')
            message(wlabel)
          }
        
        addon_label=''
        if (params$fixed_allocation)
          {
            addon_label=paste('(',format(params$AllocationDate,'%m/%y'),' allocation)',sep='')
          }

    if (!length(xml_name)) xml_name=paste('Rolling ',wlabel,addon_label)
    
    XML_RES = paste('<DATA order="',order,'" Type="image" Name="',xml_name,'">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='')
    order <<- order+1
    return(XML_RES)
  }

sourceBar <- function (sources,label,ylab,xlab,xticks,names,colors)
{
  oldpar <- par()
  par(xpd=T, mar=par()$mar+c(0,0,0,4))
  b <- barplot(sources,xaxt='n',col=colors,space=0,ylab=ylab,border=NA,las=2,)
  sq=seq(ncol(sources), 1, by=-12)
  axis(1, at=sq,labels=xticks[sq],cex.axis=0.8)
  
  for (j in 1:ncol(sources))
    {
      if (j%%12==0)
        {
          sm <- 0
          for (i in 1:nrow(sources))
            {
              y <- sm
              sm=sm+sources[i,j]
              if (sources[i,j]>5)
                {
                  {
                    text(j-0.5,y+sources[i,j]/2,round(sources[i,j],0),srt=90,cex=0.9,font=2,col='black')
                  }
                }
            }
        }
    }

  legend(ncol(sources)+2, 80, names, cex=0.8, fill=colors)
  par(oldpar)
}

#method='cstd'
clustering_analysis <- function(params,ard,order=1,method='kmeans'){

  
  prt=calcproformaRt(ard,params$AllocationDate,params$AllocationDate)
#  get_grade_returns_XTS(ard,as.numeric(ard$Allocations[which(index(convertIndex(ard$Allocations,'Date'))==as.Date(paste(format(params$AllocationDate,'%Y-%m'),'-01',sep=''))),]),ard$SourcesList[19:21,])[,1]
  brt=get_cached_index(params,params$benchmark)
  index(brt)=as.Date(index(brt))
  
  prt=prt[which(index(prt)==params$StartDate):NROW(prt),]
  brt=brt[index(brt)<=index(prt)[nrow(prt)]]
  
  bp=which(index(brt)==params$StartDate)
  if(NROW(bp)>0){
    bstart=max(1,bp-params$w+1)
    brt=brt[bstart:NROW(brt),]
  }
  
  vol=c()
  mean=c()
  for(i in 1:(NROW(brt)-params$w+1)){
    vol[i]=sd(brt[(i:(i+params$w-1)),1],na.rm=TRUE)
    mean[i]=mean(brt[(i:(i+params$w-1)),1],na.rm=TRUE)
  }
    
  vol=as.xts(vol,index(brt)[params$w:NROW(brt)])
  mean=as.xts(mean,index(brt)[params$w:NROW(brt)])
  
  brt2=merge.xts(brt,vol,mean)
  brt2=brt2[complete.cases(brt2),]
  prt2=prt[index(prt)%in%index(brt2),]
    
  #plot_filename = open_file_for_binaryImage(png,graphW,graphH)

  label=''


  if(method == 'kmeans'){
   label='performance under k-means regimes'
    result=kmeans(brt2[,1:2],3)
    cluster1=as.numeric(which(result$cluster==order(result$center[,1],decreasing=TRUE)[1]))
    cluster2=as.numeric(which(result$cluster==order(result$center[,1],decreasing=TRUE)[2]))
    cluster3=as.numeric(which(result$cluster==order(result$center[,1],decreasing=TRUE)[3]))
#     legend1=paste("Benchmark Return: ",round(result$centers[1,1]*100,2),"% Benchmark Volatility: ",round(result$centers[1,2]*100,2),"%",sep="")
#     legend2=paste("Benchmark Return: ",round(result$centers[2,1]*100,2),"% Benchmark Volatility: ",round(result$centers[2,2]*100,2),"%",sep="")
#     legend3=paste("Benchmark Return: ",round(result$centers[3,1]*100,2),"% Benchmark Volatility: ",round(result$centers[3,2]*100,2),"%",sep="")
  }else if(method == 'std'){
    label='performance under rolling sigma regimes'
    cluster3=which(brt2[,1]<=brt2[,3]-brt2[,2])
    cluster2=which(brt2[,1]>brt2[,3]-brt2[,2] & brt2[,1]<brt2[,3]+brt2[,2])
    cluster1=which(brt2[,1]>=brt2[,3]+brt2[,2])
#     legend1=paste("Benchmark Return<=-Sigma",sep="")
#     legend2=paste("Benchmark Return>-Sigma & <Sigma",sep="")
#     legend3=paste("Benchmark Return>=Sigma",sep="")
  }else {
    label='performance under constant sigma regimes'
    c_mean=mean(brt2[,1],na.rm=TRUE)
    c_vol=sd(brt2[,1],na.rm=TRUE)
    cluster1=which(brt2[,1]>=c_mean+c_vol)
    cluster2=which(brt2[,1]>c_mean-c_vol & brt2[,1]<c_mean+c_vol)
    cluster3=which(brt2[,1]<=c_mean-c_vol)
  }
  
  color=rep(0,NROW(brt2))
  color[cluster1]='green'
  color[cluster2]='blue'
  color[cluster3]='red'
  
  means=round(c(mean(prt2[cluster1,1]),mean(prt2[cluster2,1]),mean(prt2[cluster3,1]))*100,1)
  vols=round(sqrt(c(var(prt2[cluster1,1]),var(prt2[cluster2,1]),var(prt2[cluster3,1]))*12)*100,1)
  vars=round((c(VaR(prt2[cluster1,1]),VaR(prt2[cluster2,1]),VaR(prt2[cluster3,1])))*100,1)
  ess=round((c(ES(prt2[cluster1,1]),ES(prt2[cluster2,1]),ES(prt2[cluster3,1])))*100,1)
  N=c(length(cluster1),length(cluster2),length(cluster3))
  PCT=round(N/nrow(prt2)*100,0)
  
  bmeans=round(c(mean(brt2[cluster1,1]),mean(brt2[cluster2,1]),mean(brt2[cluster3,1]))*100,1)
  bvols=round(sqrt(c(var(brt2[cluster1,1]),var(brt2[cluster2,1]),var(brt2[cluster3,1]))*12)*100,1)
  bess=round(c(ES(brt2[cluster1,1]),ES(brt2[cluster2,1]),ES(brt2[cluster3,1]))*100,1)
  bvars=round(c(VaR(brt2[cluster1,1]),VaR(brt2[cluster2,1]),VaR(brt2[cluster3,1]))*100,1)
  

  res=rbind(
    data.frame(Portfolio.Good=means[1],Benchmark.Good=bmeans[1],Portfolio.Normal=means[2],Benchmark.Normal=bmeans[2],Portfolio.Bad=means[3],Benchmark.Bad=bmeans[3]),
    data.frame(Portfolio.Good=vols[1],Benchmark.Good=bvols[1],Portfolio.Normal=vols[2],Benchmark.Normal=bvols[2],Portfolio.Bad=vols[3],Benchmark.Bad=bvols[3]),
    data.frame(Portfolio.Good=vars[1],Benchmark.Good=bvars[1],Portfolio.Normal=vars[2],Benchmark.Normal=bvars[2],Portfolio.Bad=vars[3],Benchmark.Bad=bvars[3]),
    data.frame(Portfolio.Good=ess[1],Benchmark.Good=bess[1],Portfolio.Normal=ess[2],Benchmark.Normal=bess[2],Portfolio.Bad=ess[3],Benchmark.Bad=bess[3]),
    data.frame(Portfolio.Good=as.character(N[1]),Benchmark.Good=paste(PCT[1],'%',sep=''),Portfolio.Normal=as.character(N[2]),Benchmark.Normal=paste(PCT[2],'%',sep=''),Portfolio.Bad=as.character(N[3]),Benchmark.Bad=paste(PCT[3],'%',sep=''))
    )

  rownames(res)=c('Monthly Expected\n   Return','Ann. Volatility','5pct VaR','5pct ES','Number of\n   Observations')



  
  plot_filename = open_file_for_binaryImage(png,graphW,graphH/3)
  par(mar=c(0,0,0,0),cex=1)
  plotTable(as.matrix(res),text.cex=0.9,label.cex=0.9)
  XML_RES = paste('<DATA order="',order,'" Type="image" Name="Portfolio ',label,'">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='')
  
  
  index(prt2)=as.Date(index(prt2))
  index(brt2)=as.Date(index(brt2))
  urt=merge.xts(prt2[,1],brt2[,1],all=FALSE)
  colnames(urt)=c('Portfolio','Benchmark')

  plot_filename = open_file_for_binaryImage(png,graphW,graphH)

  par(mar=c(0,0,0,0),cex=1)
  my.panel <- function(x, y, lwd, ..., pf = parent.frame())
    {
      abline(h = 0, col = "grey", lty = 2, lwd = 2)
      lines(x, y, type="h", col=color, lwd = lwd)
    }

  plot(urt*100, panel = my.panel,main='',
         yax.loc = "left", lwd = 3,
         blocks = list(start.time = crisis_start,
           end.time = crisis_end,
           col = "lightblue1"),major.format="%b%y")

#  Type="image" Name="Portfolio/Benchmark Returns ',label,'"
  XML_RES = paste(XML_RES,'<DATA order="',order+1,'" Type="image" Name="Portfolio/Benchmark Returns ',label,'">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='')
  
  return(XML_RES)

}

#---- HERE

#return_id=result_code
#return_id = 'Rolling.Correlation.Graph'

get_returns_Portfolio_PARTIAL = function(params,return_id) {

  #stop('Detecting version.. /var/www/R/lib/grt_ad.web.R')
 
  ard = get_returns_Portfolio(params)

  #save(list = c('ard','params'), file = paste("~/portfolio_data.RData",sep='')

  XML_RES = '<RESPONSE_PARAMS>'

  order <- 1
  
  if(return_id == 'image_1' || return_id == 'Returns-Sources pictures only - Return') {
    
          # Assuming that chart is built based on Allocations on End Date    
          # get benchmark returns used for graphs

          message('Loading FOF benchmark Return')
          brt=get_cached_index(params,'HFRIFOFC_INDEX')
                    
          prt=calcproformaRt(ard,params$EndDate,params$AllocationDate)
          prt=prt[index(prt)>=params$StartDate,]
          crt=merge(prt,brt,all=FALSE)
          colnames(crt)=c('Portfolio Proforma','HFRI FOF Conservative')
          
          message('Opening file')
		
          plot_filename = open_file_for_binaryImage(png,graphW,graphH)

          message('Creating image')
		
          CLWebPerformanceSummary(crt,main='')

          imgName="image_1"
          if (return_id=='Returns-Sources pictures only - Return') imgName="Agg returns plot"
          XML_RES = paste(XML_RES,'<DATA Type="image" Name="',imgName,'">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='')
  }else  if (return_id == 'historical_graph') {
    
          # Assuming that chart is built based on Allocations on End Date    
          # get benchmark returns used for graphs

          message('Loading FOF benchmark Return')
          brt=get_cached_index(params,'HFRIFOFC_INDEX')
                    
          prt=realized_port_rt(ard)
          prt=prt[index(prt)<=params$EndDate,]
          prt=prt[index(prt)>=index(ard$Allocations)[ard$StartAllocNdx],]
          prt=prt[index(prt)>=params$StartDate,]
          crt=merge(prt,brt,all=FALSE)

          if (isnull(nrow(crt),0)>0)
            {
              colnames(crt)=c('Portfolio Historical Gross','HFRI FOF Conservative')
              message('Gross Returns:')
              message(paste(index(prt),':',prt,collapse='\n'))
              message('Opening file')
            }
          plot_filename = open_file_for_binaryImage(png,graphW,graphH)
          
          message('Creating image')
          if (isnull(nrow(crt),0)>2)
            {
              CLWebPerformanceSummary(crt,main='')
            } else
          {
            emptyPlot()
          }
          
          XML_RES = paste(XML_RES,'<DATA Type="image" Name="historical_graph">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='')
              
  } else if (return_id == 'fund_scatter_plot graph') {


      w=params$w
      dndx = which(index(ard$Returns)==params$AllocationDate)
      fndx=which(ard$Allocations[dndx,]>0)
      allocations=as.double(ard$Allocations[dndx,fndx])
      frt=ard$Returns[,fndx]
      frt=frt[index(frt)<=as.Date(params$EndDate),]
      frt=tail(frt,w)

      brt=get_cached_index(params,'SPTR_INDEX')
      index(brt)=as.Date(index(brt))
      brt=brt[index(brt)<=as.Date(params$EndDate),]
      brt=tail(brt,w)
      
      plot_filename = open_file_for_binaryImage(png,graphW,graphH)

      
      if (nrow(brt)!=w || sum(index(brt)%in%as.Date(index(frt)))!=w)
        {
          emptyPlot(paste('Inconsistent returns\n (no benchmark data ?)'),cex=2) 
        } else
      {
        
      out=data.frame(ann.rt=as.double(Return.annualized(frt))*100,
        beta=as.double(var(frt,brt))/as.double(var(brt))*100,
        allocation=as.double(ard$Allocations[dndx,fndx])*100,
        fund.name=as.character(ard$Funds_Codes$FundName[fndx]),
        fund.ticker=as.character(ard$Funds_Codes$FundTicker[fndx]),
        strategy.code=as.character(ard$ClassesList$Code[ard$Classes[dndx,fndx]]),
        strategy.name=as.character(ard$ClassesList$Name[ard$Classes[dndx,fndx]]) 
        )

      out=out[out$beta>as.double(params$min_beta) & out$allocation>as.double(params$min_allocation),]

      
      p <- ggplot(out, aes(x=beta, y=ann.rt,size=allocation,color=strategy.code,label=fund.ticker)) +geom_point() +scale_size(range = c(5,20)) +
        geom_text(size = 4, colour = "black", vjust = -1)

      print(p)
    }
      
      XML_RES = paste(XML_RES,'<DATA Type="image" Name="fund_scatter_plot">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='')
              
   } else if (return_id == 'Fund Sharp ratio') {

      w=params$w
      dndx = which(index(ard$Returns)==params$AllocationDate)
      fndx=which(ard$Allocations[dndx,]>0)
      allocations=as.double(ard$Allocations[dndx,fndx])
      frt=ard$Returns[,fndx]
      frt=frt[index(frt)<=as.Date(params$EndDate),]
      frt=tail(frt,w)

      
      plot_filename = open_file_for_binaryImage(png,graphW,graphH)

      
      out=data.frame(ann.rt=as.double(Return.annualized(frt)*100),
                       ann.vol=as.double(sd.annualized(frt)*100),
                       allocation=as.double(ard$Allocations[dndx,fndx])*100,
                       fund.name=as.character(ard$Funds_Codes$FundName[fndx]),
                       fund.ticker=as.character(ard$Funds_Codes$FundTicker[fndx]),
                       strategy.code=as.character(ard$ClassesList$Code[ard$Classes[dndx,fndx]]),
                       strategy.name=as.character(ard$ClassesList$Name[ard$Classes[dndx,fndx]]) 
          )

#      out=out[out$allocation>as.double(params$min_allocation) & out$ann.rt>-20 & out$ann.rt<35 & out$ann.vol<20,]

      plot_filename = open_file_for_binaryImage(png,graphW,graphH)
        
      p <- ggplot(out, aes(x=ann.vol, y=ann.rt,size=allocation,color=strategy.code,label=fund.ticker)) +geom_point() +scale_size(range = c(5,20)) +
        geom_text(size = 4, colour = "black", vjust = -1)+geom_abline(slope=1,color='red',alpha=0.2)

      print(p)
      
      XML_RES = paste(XML_RES,'<DATA Type="image" Name="fund_sharp_ratio">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='')
              
  }   else if (return_id == 'strategy_scatter_plot graph') {

    w=params$w
    classData=class_proforma(params,ard,params$AllocationDate)
    classRt=classData$classRt
    index(classRt)=as.Date(index(classRt))
    classRt=classRt[as.Date(index(classRt))<=as.Date(params$EndDate),]
    
    frt=classRt[index(classRt)<=as.Date(params$EndDate),]
    frt=tail(frt,w)
    
    brt=get_cached_index(params,'SPTR_INDEX')
 index(brt)=as.Date(index(brt))
 brt=brt[index(brt)<=as.Date(params$EndDate),]
 brt=tail(brt,w)

    plot_filename = open_file_for_binaryImage(png,graphW,graphH)

 if (nrow(brt)!=w || sum(index(brt)%in%as.Date(index(frt)))!=w)
   {
     emptyPlot(paste('Inconsistent returns\n (no benchmark data ?)'),cex=2) 
   } else
    {
      
      
      out=data.frame(
        ann.rt=as.double(Return.annualized(frt))*100,
        beta=as.double(var(frt,brt))/as.double(var(brt))*100,
        allocation=as.double(classData$classAlloc)*100,
        strategy.name=gsub('\\s+','\\\n',as.character(ard$ClassesList$Name[classData$classNdx])),
        strategy.code=as.character(ard$ClassesList$Code[classData$classNdx])
        )
      
      out=out[out$beta>as.double(params$min_beta) & out$allocation>as.double(params$min_allocation),]
      
      
      p <- ggplot(out, aes(x=beta, y=ann.rt,size=allocation,color=strategy.code,label=strategy.name)) +geom_point() +scale_size(range = c(5,20)) +
        geom_text(size = 4, colour = "black", vjust =0.5 ) 
      print(p)
    }
    XML_RES = paste(XML_RES,'<DATA Type="image" Name="strategy_scatter_plot">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='')
              
  }   else if (return_id == 'Strategy Sharp ratio') {

    w=params$w
    classData=class_proforma(params,ard,params$AllocationDate)
    classRt=classData$classRt
    index(classRt)=as.Date(index(classRt))
    classRt=classRt[as.Date(index(classRt))<=as.Date(params$EndDate),]
    
    frt=classRt[index(classRt)<=as.Date(params$EndDate),]
    frt=tail(frt,w)
    
      
    out=data.frame(
      ann.rt=as.double(Return.annualized(frt))*100,
      ann.vol=as.double(sd.annualized(frt))*100,
      allocation=as.double(classData$classAlloc)*100,
      strategy.name=gsub('\\s+','\\\n',as.character(ard$ClassesList$Name[classData$classNdx])),
      strategy.code=as.character(ard$ClassesList$Code[classData$classNdx])
      )


    out=out[out$allocation>as.double(params$min_allocation) & out$ann.rt>-20 & out$ann.rt<35 & out$ann.vol<20,]

    plot_filename = open_file_for_binaryImage(png,graphW,graphH)

    p <- ggplot(out, aes(x=ann.vol, y=ann.rt,size=allocation,color=strategy.code,label=strategy.name)) +geom_point() +scale_size(range = c(5,20)) +
        geom_text(size = 4, colour = "black", vjust =0.5 ) +geom_abline(slope=1,color='red',alpha=0.2)
      print(p)
    XML_RES = paste(XML_RES,'<DATA Type="image" Name="strategy_sharp_ratio">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='')
              
  }   else if(return_id == 'source_graph') {

          sl=ard$SourcesList
          out=data.frame()
          labels = ard$ColorMap$Label
          ualloc=ard$Allocations
          ualloc=ualloc[as.Date(index(ualloc))<=params$EndDate,]
          i <- 1
          lj <- nrow(ualloc)
          for (i in 1:length(labels))
            {
              ndx=sl$ID[sl$Label==labels[i]]
              j <- 1
              for (j in 1:nrow(ualloc))
                {
                  out=rbind(out,
                    data.frame(dt=j,label_ndx=i,alloc=100*sum(ard$Allocations[lj,ard$Sources[j,]%in%ndx])))
                }
            }

          base_size <- 12

          wlabels=paste(dates_months[as.integer(substr(index(ualloc), 6, 7))], substr(index(ualloc), 
            3, 4), sep = "")
          
          breaks=seq(1,length(wlabels),3)
          colors <- rev(rainbow(length(labels)))#, start=0, end=.3)) #rev(heat.colors(length(labels)))
          colors[length(labels)]='#000000'
          
          message('Opening file')
		
          plot_filename = open_file_for_binaryImage(png,graphW,graphH)

          op <- ggplot(out, aes(x=factor(dt),y=alloc,fill=factor(label_ndx))) + geom_bar(position='fill') + scale_x_discrete(breaks=breaks,labels=wlabels[breaks]) +
            labs(x = "", y = "Allocation") +
              opts(axis.ticks = theme_blank(), axis.text.x = theme_text(size = base_size * 0.7, angle = 295, hjust = 0, colour = "black"))+
                scale_fill_manual(values = ard$ColorMap$Color,name='Return\nSource:',breaks=1:length(labels),labels=labels) 
        
          print(op)
                    
          XML_RES = paste(XML_RES,'<DATA Type="image" Name="image_1">',base64(close_file_get_binaryImage_RAW(plot_filename)),'</DATA>',sep='')
  } else if(return_id == 'Ann.Ret.Graph') {
          XML_RES=paste(XML_RES,xml_roll_graph(params,ard,annRetFUN,'Annualized.Return',return_id,100))
  } else if(return_id == 'Ann.Vol.Graph') {
          XML_RES=paste(XML_RES,xml_roll_graph(params,ard,annVolFUN,'Annualized.Volatility',return_id,100))
  } else if(return_id == 'Max.Drawdown.Graph') {
          XML_RES=paste(XML_RES,xml_roll_graph(params,ard,maxDrawdownFUN,'Max.Drawdown',return_id,100))
  } else if(return_id == 'Rolling.Correlation.Graph') {
          XML_RES=paste(XML_RES,xml_roll_graph(params,ard,corrFUN,paste('Correlation to ',params$benchmark,sep=''),paste(params$benchmark,'_',return_id,sep=''),100))
          XML_RES=paste(XML_RES,xml_roll_graph(params,ard,corrFUN,paste('Correlation to ',params$benchmark,sep=''),paste(params$benchmark,'_',return_id,sep=''),100,0,0,c(params$w)))
        } else if(return_id == 'Rolling.Beta.Graph') {
          uwindows=std_roll_windows
          XML_RES=paste(XML_RES,xml_roll_graph(params,ard,betaFUN,paste('Beta to ',params$benchmark,sep=''),paste(params$benchmark,'_',return_id,sep=''),100,1,0,uwindows))
          XML_RES=paste(XML_RES,xml_roll_graph(params,ard,betaFUN,paste('Beta to ',params$benchmark,sep=''),paste(params$benchmark,'_',return_id,sep=''),100,1,0,c(params$w)))
          for (w in uwindows)
            {
              XML_RES=paste(XML_RES,xml_roll_graph(params,ard,betaFUN,paste('Beta to ',params$benchmark,sep=''),paste(params$benchmark,'_',return_id,sep=''),100,1,1,c(w)))
            }
    
        } 
  else if(return_id == 'Proforma Beta' || return_id == 'Proforma Alloc Beta') {
    if (return_id == 'Proforma Beta') params$fixed_allocation=1 # need to fix allocations for Proforma Beta
    params$benchmark='SPTR_INDEX'
    uwindows=std_roll_windows
    XML_RES=paste(XML_RES,xml_roll_graph(params,ard,betaFUN,ylabel=paste('Beta to SPX',sep=''),cache=paste('SPTR_BETA','_',return_id,sep=''),multiplier=100,mark_significance=1,plot_range=0,uwindows=uwindows,show_table=FALSE,xml_name=return_id,end_date=max(params$AllocationDate,params$EndDate)))
  } else if(return_id == 'Regime.Change') {

    XML_RES <- paste(XML_RES,clustering_analysis(params,ard,order,method='cstd'))
    order <- order +2

    XML_RES <- paste(XML_RES,clustering_analysis(params,ard,order,method='std'))
    order <- order +2

    XML_RES <- paste(XML_RES,clustering_analysis(params,ard,order,method='kmeans'))
    order <- order +2


  } else if(return_id == 'Rolling.modVaR.Graph') {
          varP <<- params$p
          varMethod <<- params$method
          message(varP,' ',varMethod)
          label=paste(round(varP*100,2),'% ',varMethod,' VaR',sep='')
          cache=paste(varP,'_',varMethod,'_VaR',sep='')
          XML_RES=paste(XML_RES,xml_roll_graph(params,ard,varFUN,label,cache,100))
  } else if(return_id == 'Allocation.History' || return_id == 'Allocation.History-TAB') {
          classRts=get_cached_calcs(roll_class_proformas,params,ard,'Class.Proforma.Returns')
          calloc=classRts$classAlloc
          cnames=ard$ClassesList$Name
          adates=as.Date(index(ard$Allocations[ard$StartAllocNdx:nrow(ard$Allocations)]))
          pdates=tail(adates[adates<=params$EndDate],params$lookback_period)
          calloc=calloc[pdates,]
          pctiles=seq(0.1,0.9,0.1)
          ctiles=c()
          ord=order(cnames)
          calloc=calloc[,ord]
          cnames=cnames[ord]
          for (i in 1:length(cnames))
            {
              ctiles=rbind(ctiles,as.double(quantile(calloc[,i],pctiles)))
            }
          scores=calloc
          

          j <- 1
          for (j in 1:ncol(scores))
            { 
              i <- 1
              for (i in 1:nrow(scores))
                {
                  scores[i,j]=max(0,which(ctiles[j,]<=as.double(calloc[i,j])))+1
                }
            }
 
          if (return_id =='Allocation.History-TAB')
            {
              out = data.frame(cnames,round(as.double(calloc[nrow(calloc),])*100,1),as.double(scores[nrow(calloc),]),round(ctiles*100,1))
              colnames(out) = c('Strategy','Allocation','Score',paste(round(pctiles*100,0),'%',sep=''))

	      
	      #out = data.frame(a1=1:5,b1=paste('g',1:5,sep=''))
	      colnames(out) = gsub('&','@',gsub('%','pct',gsub('\\(','\\_',gsub('\\)','',gsub(' ','\\_',gsub('  ',' ',colnames(out)))))))
	      
	      	      
	      res = data.frame(c1=c(''),stringsAsFactors=FALSE)
	      col_num = 20
	      for(i in 2:col_num) res = cbind(res,c(''),stringsAsFactors=FALSE); colnames(res) = paste(paste('c',1:col_num,sep=''),sep='')
	      res[1,1:ncol(out)] = colnames(out)
	      out_mat = as.matrix(out)
	      for(i in 1:nrow(out)) res = rbind(res,c(as.character(out_mat[i,]),array(NA,ncol(res)-ncol(out))))
	      XML_RES = paste(XML_RES,dataframe_to_xml_NAMED(res,return_id),sep='')
	      
	      
	      
	      
            } else
          {
            data=data.frame() 
            ualloc=tail(calloc,params$w)
            uscores=tail(scores,params$w)
			
            for (j in 1:ncol(scores))
              {
                data=rbind(data,data.frame(strategy_id=j,date_id=1:nrow(uscores),score=as.double(uscores[,j]),allocation=as.double(ualloc[,j])))
              }
            data$score=as.factor(data$score)
            
            breaks=which(as.numeric(format(pdates,'%m'))%%3==0)
            labels=format(pdates[breaks],'%b%Y')
            
            baseColors=c('Blue','LightBlue','Yellow','Orange','Red')
            plts=colorRampPalette(baseColors)(10)
            
            clabels=cnames
            for (j in 1:ncol(calloc))
              {
              clabels[j]=paste(clabels[j],'[',round(min(calloc[,j])*100,1),'<',round(mean(calloc[,j])*100,1),'<',round(max(calloc[,j])*100,1),']',sep='')
            }
            
            p <- ggplot(data, aes(date_id,strategy_id)) + geom_tile(aes(fill = score),colour = "white") +            
              theme_grey(base_size = 7) + labs(x = "",y = "") +  
                theme( axis.text.x  = element_text(angle=90, vjust=0.5, size=8),axis.text.y=element_text(size=9,color='black'))+
                  scale_x_continuous(breaks=breaks,labels=labels,expand=c(0,0)) +
                    scale_y_continuous(breaks=1:length(clabels),labels=clabels,expand=c(0,0)) +
                      scale_fill_manual(values=plts,limits = 1:10)
            
            plot_filename = open_file_for_binaryImage(png,graphW*1.5,graphH*1.2)
            print(p)
            XML_RES = paste(XML_RES,'<DATA order="',order,'" Type="image" Name="Historical allocations">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='') 
            order <- order+1
          } 
  } else if(return_id == 'Class.Correlation.Graph') {
          classRts=get_cached_calcs(roll_class_proformas,params,ard,'Class.Proforma.Returns')
          ndx=isnull(which(classRts$dates==params$AllocationDate)[1],0)
          if (!ndx) stop('No class return data for ',params$AllocationDate)
          ucrt=classRts$classProformaRts[[ndx]]
          dndx=isnull(which(as.Date(index(classRts$classAllocs))==params$AllocationDate)[1],0)
          if (!dndx) stop('No class allocations for ',params$AllocationDate)
          ucrt=ucrt[as.Date(index(ucrt))<=as.Date(params$EndDate),]
          ucrt=ucrt[,as.vector(classRts$classAlloc[dndx,])!=0]
                    
          ucrt=tail(ucrt,params$w)
          crr=cor(ucrt)
          plot_filename = open_file_for_binaryImage(png,graphW,graphH)
          graphCorr(crr)
          
          XML_RES = paste(XML_RES,'<DATA order="',order,'" Type="image" Name="Portfolio ',params$w,'M Correlation">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='')
          order <- order+1
  } else if (return_id == 'VaR.Breakdown' || return_id == 'ES.Breakdown') {
            if (is.null(params$cleanoutliers) || is.null(params$method) || is.null(params$w)) stop ('Not enough parameters specified for Risk Breakdown calculations!')
            classRts=get_cached_calcs(roll_class_proformas,params,ard,'Class.Proforma.Returns')
            ndx=isnull(which(classRts$dates==params$AllocationDate)[1],0)
            if (!ndx) stop('No class return data for ',params$AllocationDate)
            ucrt=classRts$classProformaRts[[ndx]]
            ucrt=ucrt[as.Date(index(ucrt))<=as.Date(params$EndDate),]
            
            dndx=isnull(which(index(ard$Allocations)==as.Date(params$AllocationDate))[1],0)
            if (!dndx) stop('No allocation info for ',params$AllocationDate)
            fndx=which(ard$Allocations[dndx,]!=0)
            allocations=as.double(ard$Allocations[dndx,fndx])
            frt=ard$Returns[,fndx]
            frt=frt[index(frt)<=as.Date(params$EndDate),]
            frt=tail(frt,params$w)
            wused=nrow(frt)

            message('W=',params$w)
            message('P=',params$p)
            message('fndx=',paste(fndx,collapse=':'))
                        
            label=paste(wused,'M VaR',sep='')

            components=c()
            total=c()

            if (return_id=='VaR.Breakdown')
              {
                var=VaR(frt,p=params$p,weights=allocations,portfolio_method='component',clean=params$cleanoutliers,method=params$method)
                components=as.double(var$pct_contrib_VaR)
                total=as.double(var$VaR)
              } else
                {
                  label=paste(wused,'M ES',sep='')
                  es=ES(frt,p=params$p,weights=allocations,portfolio_method='component',clean=params$cleanoutliers,method=params$method)
                  components=as.double(es$pct_contrib_ES)
                  total=as.double(es$ES)
                }
            
            message(' L components=',length(components))
            message(' L fndx=',length(fndx))
            message(' method:',params$method)

            gframe=data.frame(Fund=as.character(ard$Funds$FundDescription[fndx]),
              FundCode=as.character(ard$Funds$FundName[fndx]),
              Strategy=as.character(ard$ClassesList$Name[ard$Classes[dndx,fndx]]),
              StrategyCode=as.character(ard$ClassesList$Code[ard$Classes[dndx,fndx]]),
              
              
              Contribution=components*100,
              Allocation=allocations*100
              )


            gsum=aggregate(gframe$Contribution,list(gframe$Strategy),FUN=sum)
            galloc=aggregate(gframe$Allocation,list(gframe$Strategy),FUN=sum)
            colnames(gsum)=c('Strategy','Contribution')
            colnames(galloc)=c('Strategy','Allocation')
            sframe <- merge(x=gsum,y=galloc,by='Strategy')
            sframe$label=paste(round(sframe$Contribution,0),' ',sframe$Strategy,sep='')

            gg=list(sframe=sframe)
            

            tframe <<- sframe
            p <- ggplot(data=tframe, aes(reorder(Strategy,Contribution),Contribution,fill=Strategy))
            
            p <- p + geom_bar() + coord_flip() +
              #opts(axis.text.y = 'Contribution')+
                geom_text(aes(label=Strategy,y=min(tframe$Contribution),hjust=0)) + labs(x = "", y = "")+  opts(legend.position = "none") +
                scale_x_discrete(breaks=tframe$Strategy, labels=round(tframe$Contribution,0))

            

            plot_filename = open_file_for_binaryImage(png,graphW,graphH)
            print(p)
            XML_RES = paste(XML_RES,'<DATA order="',order,'" Type="image" Name="',label,' Contribution by Strategy">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='')
            order <- order+1


            plot_filename = open_file_for_binaryImage(png,graphW,graphH)

            map.market(id=paste(sframe$Strategy,' ',round(sframe$Contribution,1),sep=''),
                       area=sframe$Allocation,
                        group=paste(sframe$Strategy,' ',round(sframe$Contribution,1),sep=''),
                        color=sframe$Contribution,main='',lab=c(TRUE,FALSE))

            

            XML_RES = paste(XML_RES,'<DATA order="',order,'" Type="image" Name="Strategy ',label,' Contribution Map">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='')
            order <- order+1
            
            gframe <- gframe[order(gframe$Contribution,decreasing=TRUE),]

            i <- 1 
            ngraph <- (nrow(gframe)%/%mrows)+1
            for (i in 1:ngraph)
              {
                tframe <<- gframe[((i-1)*mrows+1):min(nrow(gframe),(i*mrows)),]
                p <- ggplot(tframe, aes(reorder(Fund,Contribution),Contribution,fill=StrategyCode))
                p <- p + geom_bar() + coord_flip() +
                  geom_text(aes(label=Fund,y=min(tframe$Contribution),hjust=0)) + labs(x = "", y = "",main='Risk Contribution by fund')+  opts(legend.position = "right") +
                    scale_x_discrete(breaks=tframe$Fund, labels=round(tframe$Contribution,0))
                                
                plot_filename = open_file_for_binaryImage(png,graphW,graphH)
                print(p)
                message(i) 
                XML_RES = paste(XML_RES,'<DATA order="',order,'" Type="image" Name="Fund ',label,' contribution (',i,' of ',ngraph,')">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='') 
                order <- order+1
                 
              }
            
#            plot_filename = open_file_for_binaryImage(png,graphW,graphH)

#            map.market(id=paste(gframe$FundCode,' ',round(gframe$Allocation,1),':',round(gframe$Contribution,1),sep=''),
#                       area=gframe$Allocation,
#                        group=gframe$Strategy,
#                        color=gframe$Contribution,main='',lab=c(TRUE,TRUE))

#            XML_RES = paste(XML_RES,'<DATA order="',order,'" Type="image" Name="Fund ',label,' contribution map">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='')
            order <- order+1           
  } else if (return_id=='pca.VaR.Breakdown' || return_id=='pca.ES.Breakdown')
                {
                  if (is.null(params$pca.n) || is.null(params$pca_method) || is.null(params$w)) stop ('Not enough parameters specified for PCA Risk Breakdown calculations!')

                  dndx=max(isnull(which(index(ard$Allocations)==params$StartDate),0),ard$StartAllocNdx)
                  if (!dndx) stop('No allocation info for ',params$StartDate)
                  umeasure='var'
                  mlabel='VaR'
                  if (return_id=='pca.ES.Breakdown')
                    {
                      umeasure='es'
                      mlabel='ES'
                    }
                  message('NP=',params$pca.n)
                  xpca=rolled_component_analysis(rts=ard$Returns,allocations=ard$Allocations,p=params$p,clean=params$cleanoutliers,w=params$w,start_ndx=dndx,method=params$pca_method,np=params$pca.n,measure=umeasure,risk.method=params$method)
                  resNdx=isnull(which(as.Date(index(xpca))==as.Date(params$AllocationDate))[1],0)
                  if (!resNdx)
                    {
                      stop('pca.VaR.breakdown:Unable to find end Date ',params$AllocationDate)
                    }
                  

                  label=paste(params$w,'M PCA[',params$pca_method,'] ',mlabel,' Breakdown',sep='')

                  

                  gframe<<-data.frame(PC=colnames(xpca),
                                    Contribution=as.double(xpca[resNdx,]*100)
                    )
  
                  

                  p <- ggplot(data=gframe, aes(reorder(PC,Contribution),Contribution,fill=PC))
                  p <- p + geom_bar() + coord_flip() +
                    geom_text(aes(label=PC,y=min(gframe$Contribution),hjust=0)) + labs(x = "", y = "")+  opts(legend.position = "none") +
                      scale_x_discrete(breaks=gframe$PC, labels=round(gframe$Contribution,0))

                  message('Processing :',label)
                  

                  plot_filename = open_file_for_binaryImage(png,graphW,graphH)
                  print(p)
                  XML_RES = paste(XML_RES,'<DATA order="',1,'" Type="image" Name="',label,'">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='')


                  message('Done :',label)

                  cols=c(1,2,3)
                  add=c(ncol(xpca)-3,ncol(xpca)-2,ncol(xpca)-1,ncol(xpca))
                  add=add[!add%in%cols]
                  cols=c(cols,add)
                  cols=cols[cols<=ncol(xpca) & cols>0]
                  xplot=xpca[,cols]
                  message('COLS=',paste(',',cols))
                  
                  plot_filename = open_file_for_binaryImage(png,graphW,graphH)
                  plotPanelAll(xplot*100,main='')
                  XML_RES = paste(XML_RES,'<DATA order="',2,'" Type="image" Name="',label,' Top Risk Contributors">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='') 
 
                  xplot[xplot>3]=3
                  xplot[xplot< -3]=-3

                  plot_filename = open_file_for_binaryImage(png,graphW,graphH)
                  plotPanelAll(xplot*100,main='')
                  XML_RES = paste(XML_RES,'<DATA order="',3,'" Type="image" Name="',label,' Top Risk Contributors [Trimmed]">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='')
                
  } else if (return_id=='portfolio.risk.hate') {
		  library(timeDate)
		  library(stringr)
		  pmw_uid='pmwreader'
		  pmw_pwd='s3rvic$'
		  pmw=odbcConnect('pmw',pmw_uid,pmw_pwd)
		  con=odbcConnect('crestline',uid="root",pwd="tormoz2")
		  default_risk=0.5
		  default_hate=0
		  

		  ard$Allocations=ard$Allocations[as.Date(index(ard$Allocations))<=as.Date(params$EndDate) & as.Date(index(ard$Allocations))>=as.Date(params$StartDate),]

		  current_fund_name=ard$Funds_Codes[,1]
		  current_fund_cd=ard$Funds_Codes[,2]
		  n=NCOL(ard$Allocations)
		  month_seq=as.Date(timeSequence(from =params$StartDate, to = params$EndDate, by = "month"))
		  m=NROW(month_seq)
		  
		  month_summary=data.frame()
		  for(i in 1:n){
		    ind_fund_allo=ard$Allocations[,i]
		    ind_fund_cd=current_fund_cd[i]
		    ind_fund_name=current_fund_name[i]
		    sql=paste("select f.FundCD from dbo.vfund f inner join dbo.family fam on (fam.id=f.familyid) inner join dbo.vfund f1 on (f1.familyid=fam.id and f1.fundcd='",ind_fund_cd,"')",sep="")
		    fc_set=sqlQuery(pmw,sql)
		    sql=paste("select * from crestline.risk_hate_monthly_summary where Fund_CD in ('", paste(as.character(fc_set[,1]),collapse="','"),"')",sep="")
		    ink_month_summary=sqlQuery(con,sql)
		    if(NROW(ink_month_summary)>0){
		      month_summary=rbind(month_summary,cbind(ink_month_summary,rep(ind_fund_name,NROW(ink_month_summary))))
		    }
		  }
		  colnames(month_summary)[6]="Name"
		    
		  portfolio_summary=data.frame()
		  for(g in 1:m){
		    current_month=month_seq[g]
		    x=which(ard$Allocations[g,]!=0)
		    nl=NROW(x)
		    if(nl>0){
		      allo=as.numeric(ard$Allocations[g,x])
		      risk=c()
		      hate=c()
		      type=c()
		      for(i in 1:nl){
			live_fund=ard$Funds_Codes[x[i],1]
			y=which(month_summary[,6]==live_fund & month_summary[,2]==current_month)
			if(NROW(y)>0){
			  risk=c(risk,month_summary[y,3])
			  hate=c(hate,month_summary[y,4])
			  type=c(type,as.character(month_summary[y,5]))
			}else{
			  risk=c(risk,default_risk)
			  hate=c(hate,default_hate)
			  type=c(type,"Default")
			}
		      }
		      portfolio_summary[g,1]=as.character(current_month)
		      portfolio_summary[g,2]=sum(allo*risk)
		      portfolio_summary[g,3]=sum(allo*hate)
		      portfolio_summary[g,4]=ifelse(NROW(which(str_sub(type,1,5)=="Model"))!=0,sum(allo[which(str_sub(type,1,5)=="Model")]),0)
		      portfolio_summary[g,5]=ifelse(NROW(which(type=="Carry Over"))!=0,sum(allo[which(type=="Carry Over")]),0) 
		      portfolio_summary[g,6]=ifelse(NROW(which(type=="Default"))!=0,sum(allo[which(type=="Default")]),0)
		    }else{
		      portfolio_summary[g,1]=as.character(current_month)
		      portfolio_summary[g,2]=0
		      portfolio_summary[g,3]=0
		      portfolio_summary[g,4]=0
		      portfolio_summary[g,5]=0
		      portfolio_summary[g,6]=0   
		    }
		  }
		  colnames(portfolio_summary)[1]="Date"
		  colnames(portfolio_summary)[2]="Portfolio_Risk"
		  colnames(portfolio_summary)[3]="Portfolio_Hate"
		  colnames(portfolio_summary)[4]="%_of_Model"
		  colnames(portfolio_summary)[5]="%_of_Carry Over"
		  colnames(portfolio_summary)[6]="%_of_Default"
		   
		  portfolio_summary[,1]=as.Date(portfolio_summary[,1])



		  plot_filename = open_file_for_binaryImage(png,graphW,graphH)
		  p1 <- ggplot(portfolio_summary)
		  p1=p1 + geom_line(aes(x = Date, y = Portfolio_Risk),size=2) + 
		    geom_point(aes(x = Date, y = Portfolio_Risk),size=4) + 
		    scale_x_date(breaks="3 month") + 
		    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
		    labs(x="Date",y="Portfolio Risk",title=paste("Portfolio Adjusted Score")) +
		    geom_hline(yintercept=mean(portfolio_summary[,2]),color="red")
print(p1)
		  XML_RES = paste(XML_RES,'<DATA order="',1,'" Type="image" Name="','Portfolio risk">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='')


		  
		  plot_filename = open_file_for_binaryImage(png,graphW,graphH)
		  p2 <- ggplot(portfolio_summary)
		  p2=p2 + geom_line(aes(x = Date, y = Portfolio_Hate),size=2) + 
		    geom_point(aes(x = Date, y = Portfolio_Hate),size=4) + 
		    scale_x_date(breaks="3 month") + 
		    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
		    labs(x="Date",y="Portfolio Hate",title=paste("Portfolio Adjusted Hate")) +
		    geom_hline(yintercept=mean(portfolio_summary[,3]),color="red")
print(p2)
		  XML_RES = paste(XML_RES,'<DATA order="',2,'" Type="image" Name="','Portfolio hate">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='')

		  
		  plot_filename = open_file_for_binaryImage(png,graphW,graphH)
		  tempt=melt(portfolio_summary[,c(1,4,5,6)],id.var='Date')
		  p3 <- ggplot(tempt)
		  p3=p3 + geom_bar(aes(x = Date, y = value, fill=variable),position="stack",stat = "identity") + 
		    scale_x_date(breaks="3 month") + 
		    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
		    labs(x="Date",y="Allocation",title=paste("Model Breakdown"))
print(p3)
		  XML_RES = paste(XML_RES,'<DATA order="',3,'" Type="image" Name="','Breakdown">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='')

   } else    if (return_id == 'VaR.Sensitivity') {
              rollRt=get_proforma_Returns(params,ard)
              dndx=isnull(which(rollRt$dates==params$AllocationDate)[1],0)
              if (!dndx) stop('No allocation info for ',params$AllocationDate)
              rt=rollRt$res[[dndx]]
              rt=rt[index(rt)<=params$EndDate,]
              rt=tail(rt,params$w)
              plot_filename = open_file_for_binaryImage(png,graphW,graphH)
              dontcare=tryCatch({chart.VaRSensitivity(rt,main='',ylab='Tail Measure',clean=params$cleanoutliers,lwd=3)
                        }, error=function(ex){
                          emptyPlot(paste('Error in calculation - check the inputs!'),cex=2)
                        }
                       )

              XML_RES = paste(XML_RES,'<DATA order="',order,'" Type="image" Name="VaR Sensitivity (',length(rt),'M)">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='')
              order <- order+1
  } else if( return_id=='Return.Fund.Map') {
                dndx=isnull(which(index(ard$Allocations)==as.Date(params$AllocationDate))[1],0)
                if (!dndx) stop('No allocation info for ',params$AllocationDate)
                fndx=which(ard$Allocations[dndx,]!=0)
                allocations=as.double(ard$Allocations[dndx,fndx])
                frt=ard$Returns[,fndx]
                frt=frt[index(frt)<=as.Date(params$EndDate),]
                frt=tail(frt,params$w)
                rets=as.double(apply(frt,2,Return.annualized))
                risk=apply(frt,2,sd.annualized)

                
                gframe=data.frame(Fund=as.character(ard$Funds$FundDescription[fndx]),
                  FundCode=as.character(ard$Funds$FundName[fndx]),
                  Strategy=as.character(ard$ClassesList$Name[ard$Classes[dndx,fndx]]),
                  StrategyCode=as.character(ard$ClassesList$Code[ard$Classes[dndx,fndx]]),
                  Ann.Ret=rets,
                  Allocation=allocations,
                  Contrib=rets*allocations,
                  risk=risk
                  )

                salloc=aggregate(gframe$Allocation,list(gframe$Strategy),FUN=sum)
                scontrib=aggregate(gframe$Contrib,list(gframe$Strategy),FUN=sum)
                colnames(salloc)=c('Strategy','Allocation')
                colnames(scontrib)=c('Strategy','Contribution')
                sframe=merge(salloc,scontrib,by='Strategy')
                sframe$SRt=sframe$Contribution/sframe$Allocation

                plot_filename = open_file_for_binaryImage(png,graphW,graphH)
                
                map.market(id=sframe$Strategy,
                           area=sframe$Allocation,
                           group=paste(sframe$Strategy,' ',round(100*sframe$SRt,1),sep=''),
                           color=100*sframe$SRt,main='',lab=c(TRUE,FALSE))

                XML_RES = paste(XML_RES,'<DATA order="',order,'" Type="image" Name="Strategy Standalone ',params$w,'M Annualized Return Map">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='')
                order <- order+1

                plot_filename = open_file_for_binaryImage(png,graphW,graphH)
                
                map.market(id=sframe$Strategy,
                           area=sframe$Allocation,
                           group=paste(sframe$Strategy,' ',round(100*sframe$Contribution,1),sep=''),
                           color=100*sframe$Contribution,main='',
                           lab=c(TRUE,FALSE))

                XML_RES = paste(XML_RES,'<DATA order="',order,'" Type="image" Name="Strategy Contributed ',params$w,'M Annualized Return Map">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='')
                order <- order+1

                
                plot_filename = open_file_for_binaryImage(png,graphW,graphH)
                
                map.market(id=paste(gframe$FundCode,' ',round(100*gframe$Ann.Ret,1),sep=''),
                           area=gframe$Allocation,
                           group=gframe$Strategy,
                           color=100*gframe$Ann.Ret,main='',lab=c(TRUE,TRUE))

                XML_RES = paste(XML_RES,'<DATA order="',order,'" Type="image" Name="Fund ',params$w,'M Annualized Return Map">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='')
                order <- order+1
 

                plot_filename = open_file_for_binaryImage(png,graphW,graphH)
                
                map.market(id=paste(gframe$FundCode,' ',round(100*gframe$Contrib,1),sep=''),
                           area=gframe$Allocation,
                           group=gframe$Strategy,
                           color=100*gframe$Contrib,main='',lab=c(TRUE,TRUE))

                XML_RES = paste(XML_RES,'<DATA order="',order,'" Type="image" Name="Fund Contributed ',params$w,'M Annualized Return Map">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='')
                order <- order+1
                plot_filename = open_file_for_binaryImage(png,graphW,graphH)

                map.market(id=paste(gframe$FundCode,' ',round(100*gframe$risk,1),sep=''),
                           area=gframe$Allocation,
                           group=gframe$Strategy,
                           color=100*gframe$risk,main='',lab=c(TRUE,TRUE))

                
                XML_RES = paste(XML_RES,'<DATA order="',order,'" Type="image" Name="Fund ',params$w,'M Annualized Volatility Map">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='')
                order <- order+1
  } else if (return_id=='Beta.Comparison') {

     if (1==0)
       {
         bmap=read.csv(outfile('line.strategy.mapping.csv'))
         classRts=get_cached_calcs(roll_class_proformas,params,ard,'Class.Proforma.Returns')
         
         bbenchmark <- c()
         i <- 2
         bprt <- xts()
         for (i in 2:ncol(bmap))
           {
             brt=get_cached_index_pack(params,bmap[,i],paste('BETA_COMP.',colnames(bmap)[i],sep=''))
                    bbenchmark=c(bbenchmark,list(brt))
             bprt=cbind(bprt,reconstruct_classRt(ard,classRts,bmap[,c(1,i)],brt))
           }
         colnames(bprt)=colnames(bmap)[2:ncol(bmap)]
         prt=realized_port_rt(ard)
         bprt=cbind(PORTFOLIO=realized_port_rt(ard),bprt)
         cprt=bprt
         for (i in 1:ncol(cprt))
           {
             cprt[,i]=cprt[,1]-cprt[,i]
           }
         
         adates=as.Date(index(ard$Allocations))
                                        #                message('ADATES:::',paste(adates,collapse=','))
         pdates=tail(adates[adates<=params$AllocationDate],params$w)
         message('PDATES:::',params$w,':',paste(pdates,collapse=','))
         
         
         plot_filename = open_file_for_binaryImage(png,graphW,graphH)
         CLTimeSeries(cprt[pdates,]*100,lwd=3,legend.loc='topleft',xlab='',ylab='Return',main='')
         XML_RES = paste(XML_RES,'<DATA order="',order,'" Type="image" Name="Portfolio Return difference vs. beta portfolio">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='')
         order <- order+1
         
         
         plot_filename = open_file_for_binaryImage(png,graphW,graphH)
         CLTimeSeries(bprt[pdates,]*100,lwd=3,legend.loc='topleft',xlab='',ylab='Return',main='')
         XML_RES = paste(XML_RES,'<DATA order="',order,'" Type="image" Name="Portfolio Return vs. beta portfolio">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='')
         order <- order+1
       }
     
     # return attribution by strategy
     
     prt=realized_port_rt(ard)
     prt=prt[index(prt)<=params$EndDate,]
     prt=prt[index(prt)>=index(ard$Allocations)[ard$StartAllocNdx],]
     prt=prt[index(prt)>=params$StartDate,]
     

performance=roll_class_proformas(params,ard,proforma=FALSE)
windows=na.omit(as.numeric(params$Windows))
windows=windows[windows<=nrow(prt)]

classAlloc=performance$classAlloc[index(prt),]
classRt=performance$classRt[index(prt),]
classNames=performance$classNames


if (nrow(classAlloc)!=nrow(classAlloc) || nrow(prt)!=nrow(classAlloc))
  {
    stop('Inconsistent Returns!')
  }

classContrib=classAlloc*classRt

years=format(index(prt),'%Y')
uyears=unique(years)

# windows attribution

w <- windows[1]

period_labels=c(paste(windows,'M',sep=''))
all_periods=c()
for (w in windows)
  {
    all_periods=c(all_periods,list(tail(index(prt),w)))
  }

for (year in sort(uyears,decreasing=TRUE))
  {
    yprt=index(prt)[years==year]
    period_labels=c(period_labels,year)
    all_periods=c(all_periods,list(yprt))
  }

pr <- 1
for (pr in 1:length(period_labels))
  {
  period=all_periods[[pr]]
  label=period_labels[pr]
  if (length(period)<2)
    {
      # add logic for just one month !
    } else
  {

    
    res=data.frame(class=classNames,
      Ann.Rt=round(as.vector(Return.annualized(classRt[period,]))*100,2),
      Avg.Alloc=round(100*as.vector(apply(X=classAlloc[period,],FUN=mean,MARGIN=2)),2)
      )
    
    perRt=cumprod(1+prt[period,])
    header=paste(label,' total return:',round(tail(perRt-1,1)*100,1),' Ann:',round(Return.annualized(prt[period])*100,2),' [',format(period[1],'%b%y'),'-',format(tail(period,1),'%b%y'),']',sep='')
    message(header)
    perRt=cumprod(1+prt[period,])
    perRt=lag(perRt)
    perRt[1]=1
    perContrib=classContrib[period,]
    for (i in 1:nrow(perRt))
      {
        perContrib[i,]=perContrib[i,]*as.double(perRt[i])
      }
    
    totalContrib=apply(perContrib,FUN=sum,MARGIN=2) # $$ contribution per Strategy
    
    res$contrib=round(totalContrib*100,2)
    
    res$label=paste(res$class,':',res$Avg.Alloc,'/',res$Ann.Rt,sep='')
    message('ggplot1')


    out <<- res
    p <- ggplot(out, aes(reorder(label,contrib),contrib,fill=label))
    message('ggplot2')

    p <- p + geom_bar() + coord_flip()+opts(legend.position = "none") + labs(x = "", y = "")
    #+  scale_x_discrete(breaks=label, labels=contrib)
#      geom_text(aes(label=label,y=min(out$contrib),hjust=0)) + labs(x = "", y = "")+  opts(legend.position = "none")
#    +  scale_x_discrete(breaks=out$label, labels=out$contrib)

    plot_filename = open_file_for_binaryImage(png,graphW,graphH)
    print(p)
    XML_RES = paste(XML_RES,'<DATA order="',order,'" Type="image" Name="',header,'">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='')
    order <- order+1

    i <- 1

    if (nrow(perContrib)>6)
      for (i in 1:nrow(res))
      {
        
        plot_filename = open_file_for_binaryImage(png,graphW,graphH)

        my.panel <- function(x, y, lwd, ..., pf = parent.frame())
          {
            abline(h = 0, col = "grey", lty = 2, lwd = 2)
            lines(x, y, type="h", col='gray', lwd = lwd)
          }
        pxts =na.omit(merge(cumsum(perContrib[period,i]),classAlloc[period,i],cumprod(1+classRt[period,i])))
        colnames(pxts)=c('Contibution','Allocation','Cum.Return')
        
        plot(pxts*100, panel = my.panel,main='',
             yax.loc = "left", lwd = 3,
             blocks = list(start.time = crisis_start, end.time = crisis_end,col = "lightblue1"),
             major.format="%b%y")

        
        strategy_header=paste(label,' ',classNames[i],' Contribution History [',format(period[1],'%b%y'),'-',format(tail(period,1),'%b%y'),']',sep='')

        XML_RES = paste(XML_RES,'<DATA order="',order,'" Type="image" Name="',strategy_header,'">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='')
        order <- order+1

      }

    
  }
}


                                
  } else if (return_id == 'grid_1') {

		message('Starting GRID creation')

		grid_1 = get_returns_Table(params,ard)$ReturnsTable

		message('Finished table, starting conversion to XML')

		XML_RES = paste(XML_RES,dataframe_to_xml_NAMED(grid_1,'tab_PROFORMA'),sep='')

		message('Finished XML conversion')

  } else if (return_id == 'returns_grids') {

		message('Starting full returns grid creation')

		proforma_returns_grid = data.frame(Date=index(ard$Returns),coredata(ard$Returns))
		realized_returns_grid = data.frame(Date=index(ard$Returns_RR),coredata(ard$Returns_RR))

		message('Finished table, starting conversion to XML')

		XML_RES = paste(XML_RES,dataframe_to_xml_NAMED(proforma_returns_grid,'tab_PROFORMA_RETURNS'),sep='')
		XML_RES = paste(XML_RES,dataframe_to_xml_NAMED(realized_returns_grid,'tab_REALIZED_RETURNS'),sep='')
		XML_RES = paste(XML_RES,dataframe_to_xml_NAMED(ard$Funds,'tab_FUNDS_LIST'),sep='')

		message('Finished XML conversion')

  } else if (return_id == 'header_1') {
		XML_RES = paste(XML_RES,'<HEADER Type="text" Name="header_text">','Some header text.','</HEADER>',sep='')
  } else if (return_id == 'ALL') {
          	message('Starting GRID creation')

          	grid_1 = get_returns_Table(params,ard)$ReturnsTable

		message('Finished table, starting conversion to XML')

		XML_RES = paste(XML_RES,dataframe_to_xml_NAMED(grid_1,'tab_PROFORMA'),sep='')

		message('Finished XML conversion')

#		message('Starting full returns grid creation')
#
#		returns_full_grid = data.frame(Date=index(ard$Returns),coredata(ard$Returns))
#		sources_full_grid = data.frame(Date=index(ard$Sources),coredata(ard$Sources))
#
#		message('Finished table, starting conversion to XML')
#
#		XML_RES = paste(XML_RES,dataframe_to_xml_NAMED(returns_full_grid,'tab_FULL_RETURNS'),sep='')
#		XML_RES = paste(XML_RES,dataframe_to_xml_NAMED(sources_full_grid,'tab_FULL_SOURCES'),sep='')

		ard$SourcesList = rbind(c(0,'','','NA'),ard$SourcesList) # Just so that 0-s are interpreted correctly
		XML_RES = paste(XML_RES, dataframe_to_xml_NAMED(ard$SourcesList,'tab_SOURCES_LIST'), sep='')
		XML_RES = paste(XML_RES, dataframe_to_xml_NAMED(data.frame(ID=ard$SourcesList$ID, Color=substr(ard$ColorMap$Color[match(ard$SourcesList$Label,ard$ColorMap$Label)],1,7), stringsAsFactors = FALSE), 'tab_COLOR_MAPPING'), sep='')

		#message(paste(ard$ColorMap,sep='-'))   rbind(c(0,'','','NA'),ard$SourcesList)
		#message(paste(data.frame(ID=ard$SourcesList$ID,Color=substr(ard$ColorMap$Color[match(ard$SourcesList$Label,ard$ColorMap$Label)],1,7),stringsAsFactors = FALSE),sep='-'))

		#for(i in 1:dim(ard$Funds)[1])
		#	message(paste(ard$Funds[i,],sep='-'))

		#XML_RES = paste(XML_RES,dataframe_to_xml_NAMED(cbind(ard$Funds,Allocation=as.numeric(ard$Allocations[dim(ard$Allocations)[1],])),'tab_FUNDS_LIST'),sep='')
		XML_RES = paste(XML_RES,dataframe_to_xml_NAMED(cbind(ard$Funds,Allocation=as.numeric(ard$Allocations[params$AllocationDate,])),'tab_FUNDS_LIST'),sep='')

		message('Finished XML conversion')

		XML_RES = paste(XML_RES,'<HEADER Type="text" Name="header_text">','Some header text.','</HEADER>',sep='')
  } else if (return_id == 'Returns-Sources XTS only') {
		message('Starting full returns grid creation')

		for(i in 1:dim(ard$Sources)[2]) ard$Sources[which(is.na(ard$Sources[,i])),i]=0

		returns_full_grid = data.frame(Date=index(ard$Returns),coredata(ard$Returns))
		sources_full_grid = data.frame(Date=index(ard$Sources),coredata(ard$Sources))

		for(i in 1:dim(ard$Allocations)[1]) ard$Allocations[i,is.na(ard$Sources_RR[i,])] = 0
		allocs_full_grid = data.frame(Date=index(ard$Allocations),coredata(ard$Allocations))

		message('Finished table, starting conversion to XML')

		XML_RES = paste(XML_RES,dataframe_to_xml_NAMED(returns_full_grid,'tab_FULL_RETURNS'),sep='')
		XML_RES = paste(XML_RES,dataframe_to_xml_NAMED(sources_full_grid,'tab_FULL_SOURCES'),sep='')
		XML_RES = paste(XML_RES,dataframe_to_xml_NAMED(allocs_full_grid,'tab_FULL_ALLOCATIONS'),sep='')

  } else if (return_id == 'Realized Returns-Sources XTS only') {
		message('Starting full returns grid creation (realized)')

		realized_returns_grid = data.frame(Date=index(ard$Returns_RR),coredata(ard$Returns_RR))
		realized_sources_grid = data.frame(Date=index(ard$Sources_RR),coredata(ard$Sources_RR))

		XML_RES = paste(XML_RES,dataframe_to_xml_NAMED(realized_returns_grid,'tab_REALIZED_RETURNS'),sep='')
		XML_RES = paste(XML_RES,dataframe_to_xml_NAMED(realized_sources_grid,'tab_REALIZED_SOURCES'),sep='')

  } else if (return_id == 'Returns-Sources pictures only - Sources')
    {
      
      sl=ard$SourcesList
      out=data.frame()
      labels = ard$ColorMap$Label
      
      i <- 1
      lj <- which(index(ard$Allocations)==params$AllocationDate)#nrow(ard$Allocations)
      sources=matrix(0,length(labels),nrow(ard$Allocations))
      for (i in 1:length(labels))
        {
          ndx=sl$ID[sl$Label==labels[i]]
          j <- 1
          for (j in 1:nrow(ard$Allocations))
            {
              sources[i,j]=100*sum(ard$Allocations[lj,ard$Sources[j,]%in%ndx])
            }
        }
      
      message('Opening file')
      plot_filename = open_file_for_binaryImage(png,graphW,graphH)
message('params$ImageFormat ',return_id)
message(params$ImageFormat)
      sourceBar(sources,'Sources Of Return','Source','Date',xticks=format(index(ard$Allocations),'%b%y'),names=labels,colors=ard$ColorMap$Color)
      XML_RES = paste(XML_RES,'<DATA Type="image" Name="Sources plot">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='')
  } else if (return_id == 'Return table totals breakdown' || return_id=='Historical Return table totals Breakdown' || return_id=='Proforma Return table totals Breakdown')
    {
      message('Running ', return_id)
      pdata = get_returns_Table(params,ard)$Totals_Breakdown
      bh=get_cached_index_pack(params,market_bench_codes,market_bench_name)
      rth=trimmed_realized_rt(params,ard,TRUE)
      parth=calcproformaAllocRt(ard,params$EndDate)
      prt=calcproformaRt(ard,params$EndDate,params$AllocationDate)
      prt=prt[index(prt)<=params$EndDate,]
      prt=prt[index(prt)>=params$StartDate,]

      parth=parth[index(prt),]

      rth_merged=merge(rth,bh,all=TRUE)
      rt=rth_merged[index(rth_merged)%in%index(rth),]
      plotEmpty=0
      hcalendar=data.frame()
      if (isnull(nrow(rt),0)>11)
        {
          hdata = t(get_realized_returns_Table(rt,params$StartDate,params$AllocationDate,params$Windows))*100
          hcalendar=calcCalendarRt(rth)
        } else
      {
        hdata=pdata[1,]*0
        plotEmpty=1
      }

      plotAllocEmpty=0
      pacalendar=data.frame()
      rtpa_merged=merge(parth,bh,all=TRUE)
      part=rtpa_merged[index(rtpa_merged)%in%index(parth),]
      if (isnull(nrow(part),0)>11)
        {
          padata = t(get_realized_returns_Table(part,params$StartDate,params$AllocationDate,params$Windows))*100
          pacalendar=calcCalendarRt(part)
        } else
      {
        padata=pdata[1,]*0
        plotAllocEmpty=1
      }


      ulabels=sort(unique(c(rownames(pdata),rownames(hdata),rownames(padata))))
      colors=rainbow(length(ulabels))

      data=hdata
      iname='Returns historical totals breakdown'
      if (return_id == 'Return table totals breakdown')
        {
          data=pdata
          hcalendar=calcCalendarRt(prt)
          iname="Returns totals breakdown"
        } else if (return_id=='Proforma Return table totals Breakdown')
      {
        data=padata
        hcalendar=pacalendar
        iname=return_id
      }
      
      hdata_ylim = as.vector(hdata)
      hdata_ylim[is.na(hdata_ylim)] = 0
      pdata_ylim = as.vector(pdata)
      pdata_ylim[is.na(pdata_ylim)] = 0
      padata_ylim = as.vector(padata)
      padata_ylim[is.na(padata_ylim)] = 0

      ylim=c(min(min(hdata_ylim),min(pdata_ylim),min(padata_ylim),0),max(max(hdata_ylim),max(pdata_ylim),max(padata_ylim)))
      colors=colors[match(rownames(data),ulabels)]
      
      
      plot_filename = open_file_for_binaryImage(png,graphW,graphH)

      if ((return_id=='Historical Return table totals Breakdown' && plotEmpty) ||
          (return_id=='Proforma Return table totals Breakdown'   && plotAllocEmpty )
          )
        {
          emptyPlot()
        } else
      {
        oldpar <- par()
        par(xpd=T, mar=par()$mar+c(0,0,0,3))
        mp=barplot(as.matrix(data),beside=T,main='',xlab=NA,ylab="Value",col=colors,axisnames = FALSE,ylim=ylim)
        xtext=c()
        for (j in 1:ncol(data))
          {
            xtext=c(xtext,' ',round(data[,j],1))
          }
        
        text(1:length(xtext)-0.5, par("usr")[3] - 0.25, srt = 45, adj = 1, labels = xtext, xpd = TRUE,cex=0.8)
        mtext(1, at = colMeans(mp), text = colnames(data), line = 1.3)
        legend((ncol(data)+1)*nrow(data)+1.2, ylim[2], rownames(data), cex=0.8, fill=colors)
        par(new=T)
        textplot(hcalendar,cex=1.2,show.rownames=FALSE,mar=c(0,0,0,0))
        par(oldpar)
      }

      XML_RES = paste(XML_RES,'<DATA Type="image" Name="',iname,'">',close_file_get_binaryImage_PARAMS(params,plot_filename),'</DATA>',sep='')
      
    } else if (return_id == 'Calculate and upload return- and beta- flags to DB') {
	

    } else if (return_id == 'Run optimization') {

	if(1==0){
		source("/var/www/R/run/env_init.R")
	        pmwAnton <<- odbcConnect(pmwName,uid=pmwAnton_uid,pwd=pmwAnton_pwd)
		load(paste(base_path,'params.RData',sep=''))
		ard = get_returns_Portfolio(params)
	}

	#rets_table = get_returns_Table(params,ard)$ReturnsTable

	x=xmlRoot(xmlParse(params$constraints_xml))

	constraints = data.frame()
	c_elements = list()

	for(c1 in xmlChildren(x)[-(1:2)]){
		tmp_const = as.character(lapply(c1[-length(xmlChildren(c1))],function(y){xmlValue(y)}))
		tmp_celem = xmlToDataFrame(xmlChildren(c1[length(xmlChildren(c1))][[1]]))
		constraints=rbind(constraints,tmp_const)
		if('Name'%in%colnames(tmp_celem))
			tmp_celem$Name = gsub('/COMMA/',',',gsub('/AMP/','&',gsub('/PERC/','%',tmp_celem$Name)))
		c_elements = c(c_elements, list(tmp_celem))
	}
	colnames(constraints) = c('Name','Type','Min','Max','MinStart','MinStep','MaxEnd','MaxStart','MaxStep','MaxEnd')

	dash_data_text = as.character(xmlValue(xmlChildren(x)[[1]]))
	dd_con = textConnection(dash_data_text)
	dash_data = read.csv(dd_con)
	close(dd_con)
	dash_data$FundName = gsub('/COMMA/',',',gsub('/AMP/','&',gsub('/PERC/','%',dash_data$FundName)))

	dash_map_text = as.character(xmlValue(xmlChildren(x)[[2]]))
	dd_con = textConnection(dash_map_text)
	dash_map = read.csv(dd_con)
	close(dd_con)
	dash_map$FundName = gsub('/COMMA/',',',gsub('/AMP/','&',gsub('/PERC/','%',dash_map$FundName)))


#	constraints
#	c_elements


	param_bench_names = c()
	param_mrt = xts()
	param_obj_coeff_BETA = c()
	tmp_id = 0
	param_betas = foreach(elem=c_elements[constraints$Type=="Beta"], .combine=rbind)%do%{ 
		foreach(j=1:dim(elem)[1], .combine=rbind)%do%{
			tmp_id = tmp_id+1
			param_bench_names = c(param_bench_names, elem$Index[j])
			param_mrt = merge.xts(param_mrt,get_cached_index(params,elem$Index[j]))
			param_obj_coeff_BETA = c(param_obj_coeff_BETA, elem$Optimization[j])
			data.frame(w=elem$Timeframe[j], max=elem$MAX[j], min=elem$MIN[j], id=tmp_id)
		}}
	param_bench_names = unique(param_bench_names)
	if(!is.null(param_betas)){
		param_betas$w = as.double(param_betas$w)
		param_betas$min = as.double(param_betas$min)
		param_betas$max = as.double(param_betas$max)
	}
	param_obj_coeff_BETA = as.numeric(param_obj_coeff_BETA)
	index(param_mrt) = as.Date(index(param_mrt))

	calculate_w_betas = function(returns_xts,bench_xts){
		rxts = returns_xts; 
		bxts = bench_xts; 
		rxts = rxts[as.Date(index(rxts))%in%as.Date(index(bxts)),]
		bxts = bxts[as.Date(index(bxts))%in%as.Date(index(rxts)),]
		for(i in 1:dim(rxts)[2]) rxts[is.na(rxts[,i]),i] = 0
		for(i in 1:dim(bxts)[2]) bxts[is.na(bxts[,i]),i] = 0

		foreach(i=1:dim(rxts)[2], .combine=rbind)%do%{
			rs = as.double(rxts[,i])
			res = foreach(j=1:dim(bxts)[2], .combine=cbind)%do%{
				bs = as.double(bxts[,j])
				res = data.frame(b=isnull(coef(summary(lm(rs ~ bs)))[2,1],0))
			}
			colnames(res) = colnames(bxts)
			res
		}
	}

	param_obj = array(0,length(ard$Funds$FundName))
	if(length(which(param_obj_coeff_BETA>0)) > 0)
		for(i in 1:dim(param_mrt)[2]){
			beta_dt = as.Date(seq(params$EndDate,by='-1 month',l=as.numeric(param_betas$w[i])))
			param_obj = param_obj + param_obj_coeff_BETA[i] * calculate_w_betas(ard$Returns[beta_dt,],param_mrt[beta_dt,i])
		}


	get_dash_data = function(fs,t,v) { 
		d = dash_data[dash_data$TableCode==t & dash_data$ValueCode==v,] 
		dm_idx = match(fs,dash_map$FundName)
		m_idx = match(fs,d$FundName)
		m_idx[is.na(m_idx)] = unlist(foreach(i=which(is.na(m_idx)))%do%which(fs%in%d$FundName & dash_map$Code[dm_idx]==dash_map$Code[dm_idx[i]]))
		res = d$Value_Number_MAIN[m_idx]
		res[is.na(res)] = 0
		res
	}
#fs=param_constraint_params$ticker;t=c_elements[[i]]$Table[j];v=c_elements[[i]]$Factor[j]

	allRt = ard$Returns[index(ard$Returns)<=params$optimization_through_date,]
	param_constraint_params = data.frame(ticker=ard$Funds$FundName, class=ard$ClassesList[ard$Classes[dim(ard$Classes)[1],],'Name'])
	param_constraints = foreach(i=which(constraints$Type=="Composite"), .combine=rbind)%do%{
		param_constraint_params = cbind(param_constraint_params, 
			foreach(j=1:dim(c_elements[[i]])[1], .combine=function(x,y){ x+y })%do%{
				res = as.numeric(c_elements[[i]]$Weight[j]) * as.numeric(if(c_elements[[i]]$Table[j]%in%c('ASSET EXPOSURE','GEOGRAPHY','HFRI by Fund','LEVERAGE','LIQUIDITY - Actual','LIQUIDITY - Stated','LIQUIDITY - Underlying','STRESS TEST','STRESS TEST-strategy')) get_dash_data(param_constraint_params$ticker,c_elements[[i]]$Table[j],c_elements[[i]]$Factor[j]) else if(c_elements[[i]]$Table[j] == 'Return') Return.annualized(tail(allRt,as.numeric(c_elements[[i]]$Timeframe[j]))) else if(c_elements[[i]]$Table[j] == 'Volatility') sd.annualized(tail(allRt,as.numeric(c_elements[[i]]$Timeframe[j]))) else if(c_elements[[i]]$Table[j] == 'Drawdown') maxDrawdown(tail(allRt,as.numeric(c_elements[[i]]$Timeframe[j]))))
				param_obj = param_obj + as.numeric(c_elements[[i]]$Optimization[j]) * res
				res
			})
		colnames(param_constraint_params)[dim(param_constraint_params)[2]] = constraints$Name[i]
				
		data.frame(field=constraints$Name[i], min=constraints$Min[i], max=constraints$Max[i])
	}
	if(!is.null(param_constraints)){
		param_constraints$min = as.double(param_constraints$min)
		param_constraints$max = as.double(param_constraints$max)
	}

	param_crange = if('Allocations (interval)'%in%constraints$Type){
		alloc_tab = c_elements[constraints$Type=="Allocations (interval)"][[1]]
		res = alloc_tab[match(ard$Funds$FundName,alloc_tab$Name),c('MIN','MAX')]
		colnames(res) = c('min','max')
		res
	}
	if(!is.null(param_crange)){
		row.names(param_crange) = 1:dim(param_crange)[1]
		param_crange$min = as.double(param_crange$min)
		param_crange$max = as.double(param_crange$max)
	}
	
	param_forceFunds = if('Allocations (interval)'%in%constraints$Type){
		alloc_tab = c_elements[constraints$Type=="Allocations (interval)"][[1]]
		alloc_tab = alloc_tab[match(ard$Funds$FundName,alloc_tab$Name),]
		res = data.frame(id=c(which(alloc_tab$Force=='Exclude'),which(alloc_tab$Force=='Force')), force=c(array(0,length(which(alloc_tab$Force=='Exclude'))),array(1,length(which(alloc_tab$Force=='Force')))))
		colnames(res) = c('id','force')
		res
	}

	param_gbound = if('Strategy allocations'%in%constraints$Type){
		res = c_elements[constraints$Type=="Strategy allocations"][[1]][,c('MIN','MAX','Name')]
		colnames(res) = c('low','high','name')
		res = cbind(id=1:dim(res)[1],res)
		res
	}
	if(!is.null(param_gbound)){
		param_gbound$low = as.double(param_gbound$low)
		param_gbound$high = as.double(param_gbound$high)
	}

	param_group = if('Strategy allocations'%in%constraints$Type){
		res = data.frame(gid=match(param_constraint_params$class,param_gbound$name),id=1:dim(param_constraint_params)[1])
		res[order(res$gid),]
	}

	param_maxN = as.double(if('Portfolio stats'%in%constraints$Type){
		tmp_idx = which(c_elements[constraints$Type=='Portfolio stats'][[1]]$Name=='Number of funds')
		if(length(tmp_idx)>0) c_elements[constraints$Type=='Portfolio stats'][[1]][tmp_idx[1],'MAX'] else 1000000
	})

	param_minN = as.double(if('Portfolio stats'%in%constraints$Type){
		tmp_idx = which(c_elements[constraints$Type=='Portfolio stats'][[1]]$Name=='Number of funds')
		if(length(tmp_idx)>0) c_elements[constraints$Type=='Portfolio stats'][[1]][tmp_idx[1],'MIN'] else 0
	})

	adj_date = min(max(index(ard$Returns)),max(index(param_mrt)))
	if(params$optimization_through_date > adj_date){
		XML_RES = paste(XML_RES,'<MESSAGE Name="MESSAGE">Optimizatioon date was adjusted based on fund/benchmark return dates, has been set to: ',adj_date,'</MESSAGE>',sep='')
		params$optimization_through_date = adj_date
	} 
	params$EndDate = optimization_through_date

	param_rt = ard$Returns#[index(ard$Returns)<=]
	param_dt = as.Date(seq(as.Date(params$optimization_through_date),len=12,by='-1 month'))#as.Date(params$optimization_through_date); param_dt = c(as.Date("2013-09-01"),param_dt)
	param_max = 1

	anton_param = hash(
		betas = param_betas,
		constraints = param_constraints,
		constraint_params = param_constraint_params,
		crange = param_crange,
		forceFunds = param_forceFunds,
		dt = param_dt,
		gbound = param_gbound,
		group = param_group,
		maxN = param_maxN,
		minN = param_minN,
		mrt = param_mrt,
		obj = param_obj,
		rt = param_rt,
		max = param_max,
		bench_names = param_bench_names)
	
	for(k in keys(anton_param))
		if(is.null(anton_param[[k]]))
			del(k,anton_param)
	

		#a = chaseRt(anton_param)
		optim_res = modelChaserFunction(anton_param,copy(params),copy(ard))
		#param = anton_param

# data.frame(my=anton_param$constraint_params$ticker,alex=param$constraint_params$ticker[mtch])
#  mtch=c(7,34,4,18,16,44,39,17,33,2,20,14,1,28,9,26,8,40,32,11,24,12,27,13,30,5,35,3,19,25,6,10,36,23,31,15,21,45,22,29)
#  r=data.frame(name=param_constraint_params$ticker,myclass=param_constraint_params$class,alexclass=param$constraint_params$class[mtch],my=as.numeric(a$weights['2013-10-01',])*100, alex=as.numeric(res1$weights['2013-10-01',mtch])*100)
# rsub=r; rsub$name=substr(rsub$name,1,9); rsub[which(r$myclass!=r$alexclass),c('name','myclass','alexclass')];   
#foreach(g=unique(r$myclass), .combine=rbind)%do%data.frame(class=g,my=sum(r$my[which(r$myclass==g)]),alex=sum(r$alex[which(r$myclass==g)]))
#foreach(g=unique(r$alexclass), .combine=rbind)%do%data.frame(class=g,my=sum(r$my[which(r$alexclass==g)]),alex=sum(r$alex[which(r$alexclass==g)]))
#anton_param$constraint_params[,anton_param$constraints$field]-param$constraint_params[mtch,anton_param$constraints$field]
#(param$rt[,mtch] - anton_param$rt)[,r$my!=r$alex]
#param$obj[mtch] - anton_param$obj
#cbind(anton_param$rt[,23],param$rt[,mtch[23]],anton_param$rt[,23]-param$rt[,mtch[23]])
#param$constraint_params[mtch,'HPerf'] - anton_param$constraint_params[,'HPerf']
#anton_param$constraint_params[,anton_param$constraints$field] - param$constraint_params[mtch,anton_param$constraints$field]
#anton_param$constraint_params[7,anton_param$constraints$field] - param$constraint_params[mtch,anton_param$constraints$field]



		XML_RES = paste(XML_RES,dataframe_to_xml_NAMED(optim_res,'tab_OPTIMIZER_DETAIL'),sep='')

		optim_totals = foreach(x=unique(optim_res[optim_res$Class!='','Class']), .combine=rbind)%do%cbind(data.frame(Name='',Class=x),foreach(y=which(optim_res$Class==x), .combine=function(x,y){x+y})%do%optim_res[y, !colnames(optim_res)%in%c('Name','Class')])
		XML_RES = paste(XML_RES,dataframe_to_xml_NAMED(optim_totals,'tab_OPTIMIZER_TOTALS'),sep='')

    } else {
	message('Warn:Unknown Report:',return_id)
	if (grep('-TAB',return_id)) # empty table
	{
		res=data.frame(a=1:2,b=1:2)
		colnames(res) = gsub('&','@',gsub('%','pct',gsub('\\(','\\_',gsub('\\)','',gsub(' ','\\_',gsub('  ',' ',colnames(res)))))))
		#XML_RES = paste(XML_RES,dataframe_to_xml_PARAMS(res,c('Name','Visible'),c('tab','FALSE')),sep='')	
		#message(dataframe_to_xml_NAMED(res,'tab'))
	} else stop('Unknown Report')
    }
  

  XML_RES = paste(XML_RES,'</RESPONSE_PARAMS>',sep='')
  
  XML_RES
}

#R=crt

CLWebPerformanceSummary <- function (R, rf = 0, main = NULL, method = c("ModifiedVaR","VaR","StdDev"), width = 0, event.labels = NULL, ylog = FALSE, wealth.index = FALSE, gap = 12, begin=c("first","axis"), legend.loc="topleft", colnames=c(),...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a wealth index chart, bars for monthly performance,
    # and underwater chart for drawdown.

    # Inputs:
    # R: a matrix, data frame, or timeSeries, usually a set of monthly returns.
    #   The first column is assumed to be the returns of interest, the next
    #   columns are assumed to be relevant benchmarks for comparison.
    # rf: this is the risk free rate.  Remember to set this to the same
    #   periodicity as the data being passed in.
    # method: Used to select the risk parameter to use in the chart.BarVaR.  May
    #   be any of:
    #     modVaR - uses CF modified VaR
    #     VaR - uses traditional Value at Risk
    #     StdDev - monthly standard deviation of trailing 12 month returns
    #

    # Outputs:
    # A stack of three related timeseries line charts

    # FUNCTION:
    begin = begin[1]
    colnames = colnames(R)
    ncols = ncol(R)

# This repeats a bit of code from chart.CumReturns, but it's intended
# to align the start dates of all three charts.  Basically, it assumes
# that the first column in the list is the column of interest, and 
# starts everything from that start date

    length.column.one = nrow(R[,1])
# find the row number of the last NA in the first column
    start.row = 1
    start.index = 0
    x = R

    if(ncols > 1) legend.loc = legend.loc    else legend.loc = NULL

    if(is.null(main))     main = paste(colnames[1],"Performance", sep=" ")

    if(ylog)
        wealth.index = TRUE


    colors=rainbow(ncols)
    # First, we lay out the graphic as a three row, one column format
#    plot.new()
    oldpar <- par()
    par(font.main=1.5, font.lab=2, font.axis=2, cex=2, cex.main=1.5,              
            cex.lab=1.5, cex.axis=3,lwd=3) 

    layout(matrix(c(1,2,3)),height=c(2,1,1.3),width=1)
    # to see the resulting layout, use layout.show(3)

    # mar: a numerical vector of the form c(bottom, left, top, right) which
    # gives the number of lines of margin to be specified on the four sides
    # of the plot. The default is c(5, 4, 4, 2) + 0.1

    # The first row is the cumulative returns line plot
    par(mar=c(1,4.5,4,3))
    
    chart.CumReturns(x, cex.axis=1.2,main = main, xaxis = FALSE, ylab = 'Cumulative Return', event.labels = event.labels, ylog = ylog, wealth.index = wealth.index, begin = begin, colors=colors,...)
    smartlegend( x="left", y= "top", inset=0,                             #smartlegend parameters
                legend = c(colnames), #legend parameters 
                fill=c(colors),                        #legend parameters
                bg = "gray",cex=1.2)                                             #legend parameters


    # The second row is the monthly returns bar plot
    par(mar=c(1,4.5,0,3))
#    chart.BarVaR(as.matrix(R[,1]), main = "", xaxis = FALSE, ylab = "Monthly Return", method = method)
    chart.TimeSeries(x, main = "", ylab = "Monthly Return",cex.axis=1.2,xaxis=FALSE,event.labels = NULL, ylog=FALSE, colors=colors,...)
    
    # The third row is the underwater plot
    par(mar=c(5,4.5,0,2))
    chart.Drawdown(x, main = "", xlab='',ylab = "From Peak", cex.axis=1.2,event.labels = NULL, ylog=FALSE, colors=colors, ...)
    
    # If we wanted to add a fourth row with the table of monthly returns
    # Unfortunately, the textplot function doesn't provide a lot of control over
    # formatting.  Also, it requires the gplots package.
    #par(mar=c(0,0,0,0))
    #textplot(table.Returns(as.matrix(R)),cex=.7,cmar=1.5,rmar=0.5,halign="center", valign="center")
    par(oldpar)
}


trimmed_realized_rt <- function (params,ard,trim=TRUE)
  {
    rt=realized_port_rt(ard)
    rt=rt[ard$StartAllocNdx:nrow(rt)]
    if (trim)
      {
        rt=rt[index(rt)<=params$EndDate]
        rt=rt[index(rt)>=params$StartDate]
      }
    return (rt)
  }
      
emptyPlot <- function (label='No Data',cex=4)
  {
    plot(1, type="n", axes=F, xlab="", ylab="")
    text(1,1,label,col='red',cex=cex)
  }


# param=anton_param; ard_params=copy(params); ard_in=copy(ard)
modelChaserFunction = function(param,ard_params,ard_in){
	library(RODBC)
	library(sn)
	library(PerformanceAnalytics)
	library(Rglpk)
	library(xtable)
	library(xts)
	library(hash)
	library(doMC)
	library(RColorBrewer)
	#rm (list=ls())
	#sys.source("grt.R", envir=attach(NULL, name="grt"))
	#sys.source("gr_return_chaser.R", envir=attach(NULL, name="gr_return_chaser"))

	options(stringsAsFactors = FALSE)

	con <- odbcConnect("crestline")
	ptr <- odbcConnect("pertrac",uid=ptr_uid,pwd=ptr_pwd)
	#pmw <- odbcConnect("pmwlive",uid=pmw_uid,pwd=pmw_pwd)

	pmw <- odbcConnect("pmw",uid='basscompanies\\adidych',pwd='RiskyBiz@1')

	prof_names = foreach(m=paste('m',c(12,24,36,60),sep=''),.combine=c)%do%paste(m,c('','_Vol','_Dr',paste('_',param$bench_names,sep='')),sep='')
	prof_nicks = prof_names
	prof_nicks[grep('_SPTR_INDEX',prof_nicks)] = gsub('_SPTR_INDEX','SPX',prof_nicks[grep('_SPTR_INDEX',prof_nicks)])
	prof_nicks[grep('_DLJHTR_INDEX',prof_nicks)] = gsub('_DLJHTR_INDEX','HY',prof_nicks[grep('_DLJHTR_INDEX',prof_nicks)])
	prof_nicks[grep('_HFRIFOF_INDEX',prof_nicks)] = gsub('_HFRIFOF_INDEX','HFRIFOF',prof_nicks[grep('_HFRIFOF_INDEX',prof_nicks)])
	prof_colnames_map = data.frame(name=prof_names,nick=prof_nicks)

	#prof_colnames_map = data.frame(name=c('m12','m12_Vol','m12_Dr','m12_SPTR_INDEX','m12_DLJHTR_INDEX','m12_HFRIFOF_INDEX',
#'m24','m24_Vol','m24_Dr','m24_SPTR_INDEX','m24_DLJHTR_INDEX','m24_HFRIFOF_INDEX',
#'m36','m36_Vol','m36_Dr','m36_SPTR_INDEX','m36_DLJHTR_INDEX','m36_HFRIFOF_INDEX',
#'m60','m60_Vol','m60_Dr','m60_SPTR_INDEX','m60_DLJHTR_INDEX','m60_HFRIFOF_INDEX'),
#nick=c('12R','12S','12DR','12SPX','12HY','12HFRIFOF',
#'24R','24S','24DR','24SPX','24HY','24HFRIFOF',
#'36R','36S','36DR','36SPX','36HY','36HFRIFOF',
#'60R','60S','60DR','60SPX','60HY','60HFRIFOF'))

#	param=anton_param; ard_params = params; 
	chaser_res=chaseRt(param)
	wgh = chaser_res$weights
	dash_nums = param$constraint_params[,!colnames(param$constraint_params)%in%c('ticker','class')]
	
	res = rbind(data.frame(Name=param$constraint_params$ticker, Class=param$constraint_params$class), data.frame(Name=prof_colnames_map$nick, Class=array("",length(prof_colnames_map$nick))), data.frame(Name=names(dash_nums), Class=array("",dim(dash_nums)[2])))

# alloc_date=ard_params$AllocationDate; allocs=as.numeric(ard_in$Allocations[ard_params$AllocationDate,])
	construct_data_col = function(alloc_date,allocs){
		ard_params_tmp = copy(ard_params)
		ard_params_tmp$AllocationDate = alloc_date
		market_bench_codes <<- param$bench_names

		prof = get_returns_Table(ard_params_tmp,ard_in)$ReturnsTable
		tmp_res = data.frame(Value=c(allocs, as.numeric(prof[prof$Class=='Total',colnames(prof)%in%prof_colnames_map$name]), as.numeric(unlist(foreach(j=1:dim(dash_nums)[2])%do%sum(dash_nums[,j]*allocs)))))
		colnames(tmp_res) = paste('c',alloc_date,sep='')
		tmp_res
	}

	res = cbind(res,construct_data_col(ard_params$AllocationDate,as.numeric(ard_in$Allocations[ard_params$AllocationDate,])))
	colnames(res)[length(colnames(res))] = 'M'

	ard_in$Allocations[index(wgh),] = wgh
	cbind(res,foreach(i=1:nrow(wgh), .combine=cbind)%dopar%{ #t(wgh[i,]) 
		construct_data_col(index(wgh)[i],as.numeric(wgh[index(wgh)[i],]))
		#ard_params_tmp = copy(ard_params)
		#ard_params_tmp$AllocationDate = as.Date(index(wgh)[i])
		#market_bench_codes <<- param$bench_names

		#prof = get_returns_Table(ard_params_tmp,ard_in)$ReturnsTable
		#tmp_res = data.frame(Value=c(as.numeric(wgh[i,]), as.numeric(prof[prof$Class=='Total',colnames(prof)%in%prof_colnames_map$name]), as.numeric(unlist(foreach(j=1:dim(dash_nums)[2])%do%sum(dash_nums[,j]*wgh[i,])))))
		#colnames(tmp_res) = paste('c',index(wgh)[i],sep='')
		#tmp_res
	})
}
