#system('rm /var/www/rtemp/*')
source('/var/www/R/run/env_init.R')

Sys.setenv(TZ="CST")

MC <- if (production) 20 else 6

registerDoMC(MC)
	    
if(1==0){
	source('/var/www/R/run/env_init.R')
	p_num=16	
	model_num=p_num
	last_date=as.Date('2013-01-01')
	base_path='/var/www/rtemp/'
	pmwAnton <<- odbcConnect("pmwRisk",uid=pmwAnton_uid,pwd=pmwAnton_pwd)

	for(i in 1:20){
		system('rm /var/www/rtemp/*')
		Sys.sleep(1)
	}
}


#p_num=1601; last_date=as.Date('2014-10-01'); model_num=NULL; end_date_prev=TRUE; run_portfValues=TRUE; run_proformaValues=FALSE
#last_date=report_date
# for(mm in as.Date(seq(as.Date('2013-11-01'),len=8,by='-1 month'))) { precalc_portfolio(17,mm,base_path); precalc_portfolio(14,mm,base_path); }

precalc_portfolio = function(p_num,last_date,base_path,model_num=NULL,end_date_prev=FALSE,run_portfValues=FALSE,run_proformaValues=FALSE){
	pmwAnton <<- odbcConnect("pmwRisk",uid=pmwAnton_uid,pwd=pmwAnton_pwd)

	params <- hash()
	params$PortfolioID = p_num
	params$MA_PortfolioID = isnull(model_num,p_num)
	params$DefaultSequenceID = '8'
	#params$FundSequenceMappingID = '173'    
	params$FundSequenceMappingID = as.character(sqlQuery(pmwAnton,"select MappingID from Crestline.Risk_PortfolioReturns_RuleSequence_Fund_Mapping_Dictionary where [Description]='Risk Override'"))
	params$Default_FundSequenceMappingID = as.character(sqlQuery(pmwAnton,"select MappingID from Crestline.Risk_PortfolioReturns_RuleSequence_Fund_Mapping_Dictionary where [Description]='Risk Override'"))
	params$StartDate = as.Date('2001-02-01')
	#params$EndDate = if(end_date_prev) as.Date(seq(as.Date(last_date),len=2,by='-1 month')[2]) else as.Date(last_date)
	params$EndDate = if(end_date_prev) min(as.Date(seq(as.Date(last_date),len=2,by='-1 month')[2]),seq(as.Date(format(Sys.Date(),format='%Y-%m-01')),by='-1 month',len=2)[2]) else as.Date(last_date)
        params$AllocationDate = as.Date(last_date)
	params$Windows = c('12','24','36','60','120','ytd')
	params$base_path = base_path
	params$benchmark = 'SPTR_INDEX'
	params$CacheMode = 'CACHE'
	params$CashRRMode = 1
	params$CacheInvalidateLevel = 1
	params$w = 12	    
	params$method = 'gaussian'
	params$pca_method = 'svd'
	params$pca.n = 10
	params$RR_Only = TRUE
	params$cleanoutliers = 'none'	    
	params$SessionID = 1
	params$p  = 0.95
	params$ImageFormat = 'String bytes'
	params$AllocationType = 'All dates'
	params$AllocationsTableXML = 'empty'
	params$RulesTableXML = 'empty'
	params$rtStart = as.Date('1990-01-01')

	params$rtEnd=as.Date(paste(substr(as.character(as.Date(sqlQuery(pmwAnton,"SELECT {fn NOW()}")[[1]])+0),1,7),'-01',sep=''))

	ard = get_returns_Portfolio(params)

	#added on 7/31/2014 - extend allocations beyond the rtEnd
	foreach(i=which(index(ard$Allocations)>params$rtEnd))%do%{ ard$Allocations[i,]=ard$Allocations[params$rtEnd,]; }
	foreach(i=which(index(ard$Returns)>params$rtEnd))%do%{ ard$Returns[i,]=ard$Returns[params$rtEnd,]; }

	tryCatch({
		portfolio_code = sqlQuery(pmwAnton,paste("select PortfolioDescription from Crestline.Risk_PortfolioReturns_Portfolios where PortfolioID=",p_num))[1,1]
		proforma = get_returns_Table(params,ard)$ReturnsTable
		funds_proforma = proforma[proforma$Name != proforma$Class,]
		classes_proforma = proforma[proforma$Name == proforma$Class & proforma$Name != "Total",]
		proforma = proforma[proforma$Name == "Total",]

		insert_total = function(col) { 
			query=paste("exec [Crestline].[Risk_PortfolioREPORTS_Dashboard_AddPortfolioValue_ToCache] '",col,"',",proforma[,col],",'",as.Date(last_date),"','",portfolio_code,"',",1,sep=""); 
			sqlQuery(pmwAnton,query); 
			print(query); 
		}

		insert_vals = function(vals,col,val_type) { 
			query=paste("exec [Crestline].[Risk_PortfolioREPORTS_Dashboard_AddProformaValue_ToCache] '",vals$Name,"','",val_type,"','",col,"',",vals[,col],",'",as.Date(last_date),"','",portfolio_code,"',",1,",",vals[,'Allocation'],sep=""); 
			sqlQuery(pmwAnton,query); 
			print(query); 
		}

		for(m in c("m12","m24","m36","ytd")){
			if(run_portfValues){
				insert_total(m)
				if(m != "ytd") for(b in c("Vol", "Dr", "SPTR_INDEX", "DLJHTR_INDEX", "LBUSTRUU_INDEX")) insert_total(paste(m,b,sep='_'))
			}		

			if(run_proformaValues){
				for(i in 1:dim(funds_proforma)[1]) insert_vals(funds_proforma[i,],m,'Fund return')
				if(m != "ytd") 
					for(b in c("Vol", "Dr", "SPTR_INDEX", "DLJHTR_INDEX", "LBUSTRUU_INDEX")) 
						for(i in 1:dim(funds_proforma)[1]) 
							insert_vals(funds_proforma[i,],paste(m,b,sep='_'),'Fund return')
	
				for(i in 1:dim(classes_proforma)[1]) insert_vals(classes_proforma[i,],m,'Strategy return')
				if(m != "ytd") 
					for(b in c("Vol", "Dr", "SPTR_INDEX", "DLJHTR_INDEX", "LBUSTRUU_INDEX")) 
						for(i in 1:dim(classes_proforma)[1]) 
							insert_vals(classes_proforma[i,],paste(m,b,sep='_'),'Strategy return')
			}
		}
	}, error=function(ex){})
		
	print(paste('Finished',p_num))
}

# last_date = as.Date('2012-11-01')
flags_calc = function(flags_startDate,last_date,base_path){
	if(1==0){
		source('/var/www/R/run/env_init.R')
		pmwAnton <<- odbcConnect("pmwRisk",uid=pmwAnton_uid,pwd=pmwAnton_pwd)
		MC <- 6
		registerDoMC(MC)
		last_date = as.Date('2013-01-01')
		flags_startDate = as.Date('2012-11-01')
	}

	flags_funds =  sqlQuery(pmwAnton,"select 
	l.Family_Code,
	f.funddisplayname,
	min(isnull(isnull(fon_exact.FundDisplayName,fon_family.FundDisplayName),'[NONE]')) as on_FundCd,
	min(isnull(isnull(foff_exact.FundDisplayName,foff_family.FundDisplayName),'[NONE]')) as off_FundCd
 from 
 (select distinct family_code from Crestline.Risk_PortfolioMonitoring_Flags) l 
 left join pmw.dbo.vfund f on (l.Family_Code=f.FundCd)
 left join pmw.dbo.family fam on (fam.ID=f.FamilyID)
 left join pmw.dbo.vfund fon_exact on (fon_exact.fundcd=l.Family_Code and fon_exact.onoffshore='onshore')
 left join pmw.dbo.vfund fon_family on (fon_family.FundCd=fam.ID and fon_family.onoffshore='onshore')
 left join pmw.dbo.vfund foff_exact on (foff_exact.fundcd=l.Family_Code and foff_exact.onoffshore='offshore')
 left join pmw.dbo.vfund foff_family on (foff_family.FamilyID=fam.ID and foff_family.onoffshore='offshore')
 group by 
	l.Family_Code,
	f.funddisplayname")

	p_num = as.character(sqlQuery(pmwAnton,"exec [Crestline].[Risk_PortfolioReturns_CreateTemporaryPortfolio] 'aslepnev'")[,1])
	str_proc_start = paste("exec [Crestline].[Risk_PortfolioReturns_AddPortfolioMember] ",p_num,", '", sep='')
	tmp_funds_list = c()
	for(i in 1:dim(flags_funds)[1]){
		if(flags_funds$on_FundCd[i]!='[NONE]' && !flags_funds$on_FundCd[i]%in%tmp_funds_list){
			cmd_str = paste(str_proc_start,gsub("'", "''",flags_funds$on_FundCd[i]),"', 0, null", sep="")
			#message(cmd_str)
			sqlQuery(pmwAnton,cmd_str)
			tmp_funds_list = c(tmp_funds_list,flags_funds$on_FundCd[i])
		} else message("Skipping fund..")

		if(flags_funds$off_FundCd[i]!='[NONE]' && !flags_funds$off_FundCd[i]%in%tmp_funds_list){
			cmd_str = paste(str_proc_start,gsub("'", "''",flags_funds$off_FundCd[i]),"', 0, null", sep="")
			#message(cmd_str)
			sqlQuery(pmwAnton,cmd_str)
			tmp_funds_list = c(tmp_funds_list,flags_funds$off_FundCd[i])
		} else message("Skipping fund..")
	}

	params <- hash()
	params$PortfolioID = p_num
	params$MA_PortfolioID = p_num
	params$DefaultSequenceID = '20' # "Pertrac only"
	#params$FundSequenceMappingID = '173'
	params$FundSequenceMappingID = as.character(sqlQuery(pmwAnton,"select MappingID from Crestline.Risk_PortfolioReturns_RuleSequence_Fund_Mapping_Dictionary where [Description]='Risk Override'"))
	params$Default_FundSequenceMappingID = as.character(sqlQuery(pmwAnton,"select MappingID from Crestline.Risk_PortfolioReturns_RuleSequence_Fund_Mapping_Dictionary where [Description]='Risk Override'"))
	params$StartDate = as.Date('2001-02-01')
	params$EndDate = last_date
        params$AllocationDate = last_date
	params$Windows = c('3','6','12','24','36')
	params$base_path = base_path
	params$benchmark = 'SPTR_INDEX'
	params$CacheMode = 'CACHE'
	params$CashRRMode = 1
	params$CacheInvalidateLevel = 1
	params$w = 12	    
	params$method = 'gaussian'
	params$pca_method = 'svd'
	params$pca.n = 10
	params$RR_Only = TRUE
	params$cleanoutliers = 'none'	    
	params$SessionID = 1
	params$p  = 0.95
	params$ImageFormat = 'String bytes'
	params$AllocationType = 'Last date'
	params$AllocationsTableXML = 'empty'
	params$RulesTableXML = 'empty'
	params$rtStart = as.Date('1990-01-01')

	params$rtEnd=as.Date(paste(substr(as.character(as.Date(sqlQuery(pmwAnton,"SELECT {fn NOW()}")[[1]])+0),1,7),'-01',sep=''))


	ard = get_returns_Portfolio(params)




	xts_fill_gaps = function(x,drop_na_end) {
		tmp_x = na.omit(x)
		if(length(tmp_x)==0) tmp_x else {
			sd = index(tmp_x)[1]; ed = index(tmp_x)[length(tmp_x)]; idxs = seq(sd,ed,by='1 month'); z = xts(array(NA,length(idxs)),order.by=idxs)		 
			#if(drop_na_end) for(i in length(x):1) { print(x[i]); print(i); if(is.na(x[i])) tmp_x = tmp_x[1:(i-1)] else break}
			ed = index(tmp_x)[length(tmp_x)]; idxs = seq(sd,ed,by='1 month'); pv = tmp_x[idxs[1]]
			for(i in idxs) { 
				di = as.Date(i); 
				if(is.na(tmp_x[di]) || length(tmp_x[di])==0) {
					#print(pv)
					z[di] = pv 
				} else z[di] = tmp_x[di]
				pv = z[di]
			}
		}
		z
	}


	bench_codes = sqlQuery(pmwAnton,"select * from Crestline.Risk_FundsAnalysis_Universe_Elements where UniverseID=(select UniverseID from Crestline.Risk_FundsAnalysis_Universes where UniverseName='FLAGS BENCHMARKS')")$ElementCode
	bench_codes_names = sqlQuery(pmwAnton,"select * from Crestline.Risk_FundsAnalysis_Universe_Elements where UniverseID=(select UniverseID from Crestline.Risk_FundsAnalysis_Universes where UniverseName='FLAGS BENCHMARKS')")
        bench_history = get_cached_index_pack(params,bench_codes,'FUND_FLAGS_BNCH_HISTORY',get_index_pack_WithNA)

	sqlQuery(pmwAnton,"delete from Crestline.Risk_PortfolioMonitoring_Flags_Prepared where flag_group='BETA'")
	sqlQuery(pmwAnton,"delete from Crestline.Risk_PortfolioMonitoring_Flags_Prepared where flag_group='Performance'")
	sqlQuery(pmwAnton,"delete from Crestline.Risk_PortfolioMonitoring_Flags_Prepared where flag_group='AUM'")

	aigs = sqlQuery(pmwAnton,"select * from Crestline.Risk_PortfolioMonitoring_Flags_Prepared where flag_code in ('aig*on','aig*off')")
	aiss = sqlQuery(pmwAnton,"select * from Crestline.Risk_PortfolioMonitoring_Flags_Prepared where flag_code='ais'")
	aums = sqlQuery(pmwAnton,"select * from Crestline.Risk_PortfolioMonitoring_Flags_Prepared where flag_code='aum'")
	m_aums = sqlQuery(pmwAnton,"select * from Crestline.Risk_PortfolioMonitoring_Flags_Prepared where flag_code='MEASURISK AUM  LONG'")

	aum_windows = c(1,3,6,12,'FromStart')
	aum_types = c('AIS','AIG','AUM','MAUM')
	aum_type_names = c('AMOUNT IN STRATEGY (AIS)','AMOUNT IN FUND (AIG)','AMOUNT UNDER MANAGEMENT (AUM)','MEASURISK AUM')


	flag_code = '';
	flag_value = c('');
	tmp_bench_name = ''


	foreach(calc_date=seq(flags_startDate,max(index(ard$Returns)),by='1 month')
#, .errorhandling='remove'
) %dopar% {
		pmwAnton2 = odbcConnect("pmwRisk",uid=pmwAnton_uid,pwd=pmwAnton_pwd)
		tryCatch({ 
			stats = calc_Portfolio_Return_Stats1(params, ard$Returns[index(ard$Returns)<=calc_date,], ard$Funds$FundName, array(0,length(ard$Funds$FundName)), params$Windows, params$StartDate, bench_codes, bench_history[as.Date(index(bench_history))<=calc_date,], TRUE)	
			#stats = calc_Portfolio_Return_Stats1(params, ard$Returns[index(ard$Returns)<=calc_date,], ard$Funds$FundName, array(0,length(ard$Funds$FundName)), c(12), params$StartDate, bench_codes, bench_history, TRUE)	
			message('..finished ',calc_date)
			funds_flags_dict = array('',0)
			for(fund_idx in 1:dim(ard$Funds)[1]) tryCatch({ 
				on_funds = flags_funds$Family_Code[flags_funds$on_FundCd == ard$Funds$FundName[fund_idx]]
				off_funds = flags_funds$Family_Code[flags_funds$off_FundCd == ard$Funds$FundName[fund_idx]]
				postfixes = c(array("'*on'",length(on_funds)),array("'*off'",length(off_funds)))
				onoff_funds = c(on_funds,off_funds)
	#print(postfixes)
	#print(onoff_funds)
	#print(ard$Funds$FundName[fund_idx])
	#if(ard$Funds$FundName[fund_idx] == 'Cerberus International SPV, Ltd') stop()
				for(match_idx in 1:length(onoff_funds)){
					postfix = postfixes[match_idx]
					postfix_q = gsub("'","",postfix)
					fund_code = onoff_funds[match_idx]
					query_start = paste("insert into Crestline.Risk_PortfolioMonitoring_Flags_Prepared select '",fund_code,"','",as.Date(calc_date),"'",sep="")
					#print(query_start); #stop();
					#print(fund_idx)
					#if(1==0){
					for(wnd in c(params$Windows,'FromStart')) {
						for(b_code in bench_codes){
							col_name = paste('m',wnd,'_',b_code,sep='')
							tmp_bench_name = bench_codes_names[bench_codes_names$ElementCode==b_code,'ElementName']
							if(col_name%in%colnames(stats) && !is.na(stats[fund_idx,col_name])){
								cell_val = strsplit(stats[fund_idx,col_name],';')[[1]]
					
								flag_code = paste("'BETA vs. ",tmp_bench_name," AT ",wnd,"M'+",postfix,sep="")
								query = paste(query_start,flag_code,flag_code,postfix,cell_val[1],"'BETA'","2",sep=",")
								#message(query); 
								if(!paste(fund_code,flag_code,sep='-')%in%funds_flags_dict) sqlQuery(pmwAnton2,query)
								funds_flags_dict = c(funds_flags_dict,paste(fund_code,flag_code,sep='-'))

								if(length(cell_val)>1){
									flag_code = paste("'PERFORMANCE vs. ",tmp_bench_name," AT ",wnd,"M'+",postfix,sep="")
									query = paste(query_start,flag_code,flag_code,postfix,cell_val[2],"'Performance'","2",sep=",")
									#message(query); 
									if(!paste(fund_code,flag_code,sep='-')%in%funds_flags_dict) sqlQuery(pmwAnton2,query)
									funds_flags_dict = c(funds_flags_dict,paste(fund_code,flag_code,sep='-'))
								}
							}
					
						}	

						col_name = paste('m',wnd,sep='')
						flag_code = paste("'CALCULATED RETURN AT ",wnd,"M'+",postfix,sep="")
						if(col_name%in%colnames(stats) && !is.na(stats[fund_idx,col_name])){
							query = paste(query_start,flag_code,flag_code,postfix,stats[fund_idx,col_name],"'Performance'","2",sep=",")
							#message(query); 
							if(!paste(fund_code,flag_code,sep='-')%in%funds_flags_dict) sqlQuery(pmwAnton2,query)
							funds_flags_dict = c(funds_flags_dict,paste(fund_code,flag_code,sep='-'))
						}

						col_name = 'FromStart_Dr'
						if(col_name%in%colnames(stats) && !is.na(stats[fund_idx,col_name]) && wnd=='FromStart'){
							cell_val = strsplit(stats[fund_idx,col_name],';')[[1]]
					
							if(length(cell_val)==3){
								flag_code = paste("'MONTHS IN DRAWDOWN'+",postfix,sep="")
								query = paste(query_start,flag_code,flag_code,postfix,cell_val[2],"'Performance'","2",sep=",")
								#message(query); 
								if(!paste(fund_code,flag_code,sep='-')%in%funds_flags_dict) sqlQuery(pmwAnton2,query)
								funds_flags_dict = c(funds_flags_dict,paste(fund_code,flag_code,sep='-'))

								flag_code = paste("'CALCULATED RETURN DRAWDOWN'+",postfix,sep="")
								query = paste(query_start,flag_code,flag_code,postfix,cell_val[3],"'Performance'","2",sep=",")
								#message(query); 
								if(!paste(fund_code,flag_code,sep='-')%in%funds_flags_dict) sqlQuery(pmwAnton2,query)
								funds_flags_dict = c(funds_flags_dict,paste(fund_code,flag_code,sep='-'))
							}
						}

						col_name = paste('m',wnd,'_Vol',sep='')
						flag_code = paste("'CALCULATED RETURN VOLATILITY AT ",wnd,"M'+",postfix,sep="")
						if(col_name%in%colnames(stats) && !is.na(stats[fund_idx,col_name])){
							query = paste(query_start,flag_code,flag_code,postfix,stats[fund_idx,col_name],"'Performance'","2",sep=",")
							#message(query); 
							if(!paste(fund_code,flag_code,sep='-')%in%funds_flags_dict) sqlQuery(pmwAnton2,query)
							funds_flags_dict = c(funds_flags_dict,paste(fund_code,flag_code,sep='-'))
						}
					}
					#}	




					#postfix = postfixes[match_idx]
					#fund_code = onoff_funds[match_idx]
					#query_start = paste("insert into Crestline.Risk_PortfolioMonitoring_Flags_Prepared select '",fund_code,"','",as.Date(calc_date),"'",sep="")
					#print(query_start); #stop();
					#print(fund_idx)

				

					for(wnd in aum_windows){
						#fundcd = flags_funds$Family_Code[10]
						#wnd = aum_windows[2] 

						if(wnd == 'FromStart')
							for(aum_type in aum_types){
								xts_aum = if(aum_type=='AIS') aiss[aiss$Family_Code==fund_code,c('Flag_Date','Flag_Value')] else
									if(aum_type=='AUM') aums[aums$Family_Code==fund_code,c('Flag_Date','Flag_Value')] else
									if(aum_type=='MAUM') m_aums[m_aums$Family_Code==fund_code,c('Flag_Date','Flag_Value')] else
									aigs[aigs$Family_Code==fund_code & grepl(postfix_q,aigs$Flag_Code),c('Flag_Date','Flag_Value')]
								xts_aum = as.xts(xts_aum$Flag_Value,order.by=as.Date(xts_aum$Flag_Date))
								ldrdown = 0
								drdown = list()
								if(length(xts_aum)>0) xts_aum = exp(diff(log(xts_fill_gaps(xts_aum,TRUE))))-1
								if(as.Date(calc_date)%in%index(xts_aum)){								
									xts_aum[is.na(xts_aum) | is.nan(xts_aum) | !is.finite(xts_aum)] = 0
									drdown = findDrawdowns(xts_aum)
									ldrdown = length(drdown$to)
								}
	
								if(ldrdown>0 && drdown$to[ldrdown]>length(xts_aum) && drdown$return[ldrdown]<0){
									flag_code = paste("'",aum_type_names[aum_types==aum_type],", TIME IN DRAWDOWN'+",if(aum_type=='AIG') postfix else "''",sep="")
									query = paste(query_start,flag_code,flag_code,postfix,drdown$length[length(drdown$to)],"'AUM'","2",sep=",")
									#message(query); 
									if(!paste(fund_code,flag_code,sep='-')%in%funds_flags_dict) sqlQuery(pmwAnton2,query)
									funds_flags_dict = c(funds_flags_dict,paste(fund_code,flag_code,sep='-'))
	
									flag_code = paste("'",aum_type_names[aum_types==aum_type],", DRAWDOWN'+",if(aum_type=='AIG') postfix else "''",sep="")
									query = paste(query_start,flag_code,flag_code,postfix,Return.cumulative(xts_aum[drdown$from[ldrdown]:length(xts_aum)]),"'AUM'","2",sep=",")
									#message(query); 
									if(!paste(fund_code,flag_code,sep='-')%in%funds_flags_dict) sqlQuery(pmwAnton2,query)	
									funds_flags_dict = c(funds_flags_dict,paste(fund_code,flag_code,sep='-'))
								}
							}
						else {
							wnd_start = seq(as.Date(calc_date), length = as.numeric(wnd)+1, by = "-1 month")[as.numeric(wnd)+1]
							wnd_index = seq(wnd_start, as.Date(calc_date), by = "1 month")
							for(aum_type in aum_types){
								xts_aum = if(aum_type=='AIS') aiss[aiss$Family_Code==fund_code,c('Flag_Date','Flag_Value')] else
									if(aum_type=='AUM') aums[aums$Family_Code==fund_code,c('Flag_Date','Flag_Value')] else
									if(aum_type=='MAUM') m_aums[m_aums$Family_Code==fund_code,c('Flag_Date','Flag_Value')] else
									aigs[aigs$Family_Code==fund_code & grepl(postfix_q,aigs$Flag_Code),c('Flag_Date','Flag_Value')]
								xts_aum = as.xts(xts_aum$Flag_Value,order.by=as.Date(xts_aum$Flag_Date))
	
								if(min(c(wnd_start,as.Date(calc_date))%in%index(xts_aum))==TRUE){
									flag_code = paste("'",aum_type_names[aum_types==aum_type]," CHANGE AT ",wnd,"M'+",if(aum_type=='AIG') postfix else "''",sep="")
									query = paste(query_start,flag_code,flag_code,postfix,as.numeric(xts_aum[calc_date])-as.numeric(xts_aum[wnd_start]),"'AUM'","2",sep=",")
									#message(query); 
									if(!paste(fund_code,flag_code,sep='-')%in%funds_flags_dict) sqlQuery(pmwAnton2,query)
									funds_flags_dict = c(funds_flags_dict,paste(fund_code,flag_code,sep='-'))

									if(as.numeric(xts_aum[wnd_start]) != 0){
										flag_code = paste("'",aum_type_names[aum_types==aum_type]," CHANGE AT ",wnd,"M, %'+",if(aum_type=='AIG') postfix else "''",sep="")
#										query = paste(query_start,flag_code,flag_code,postfix,100*(as.numeric(xts_aum[calc_date])-as.numeric(xts_aum[wnd_start]))/as.numeric(xts_aum[wnd_start]),"'AUM'","2",sep=",")
										query = paste(query_start,flag_code,flag_code,postfix,(as.numeric(xts_aum[calc_date])-as.numeric(xts_aum[wnd_start]))/as.numeric(xts_aum[wnd_start]),"'AUM'","2",sep=",")
										#message(query); 
										if(!paste(fund_code,flag_code,sep='-')%in%funds_flags_dict) sqlQuery(pmwAnton2,query)
										funds_flags_dict = c(funds_flags_dict,paste(fund_code,flag_code,sep='-'))
									}
								}
							}
						}
					}
				}
			}, error=function(ex){sqlQuery(pmwAnton2,paste("exec Crestline.Risk_Admin_Log_InsertMessage 'Calculate AUM,BETA and Performance flags', 'Error', 'Error in foreach statement for date: ",calc_date," and for fund: ",ard$Funds$FundName[fund_idx],": ",ex, "'",sep=""))})	
		}, error=function(ex){sqlQuery(pmwAnton2,paste("exec Crestline.Risk_Admin_Log_InsertMessage 'Calculate AUM,BETA and Performance flags', 'Error', 'Error in foreach statement for date: ",calc_date,", ",ex, "'",sep=""))})		
	}


	sqlQuery(pmwAnton,paste("[Crestline].[Risk_PortfolioReturns_DeletePortfolio] ",p_num,",0",sep=''))



	#beta_data = get_portfolio_calculated_Flags(params,ard,'209')
	

}

load_diff = function(){
	sqlQuery(pmwAnton,"delete from Crestline.Risk_Flags_FlagListDiff_DailyDiff")

	filename1 = tempfile()
	filename2 = tempfile()
	filename3 = tempfile()
	
	t = sqlQuery(pmwAnton,"Crestline.Risk_Flags_FlagListDiff_GetLastTwoTrailingRevisions")

	for (fund_name in unique(t[,'Fund1'])){
		write.table(t[t$Fund1==fund_name,c('Str1')],filename1,col.names=FALSE,row.names=FALSE)
		write.table(t[t$Fund2==fund_name,c('Str2')],filename2,col.names=FALSE,row.names=FALSE)
	
		a=system(paste("diff ",filename1," ",filename2," > ",filename3,sep=""))
		if(a==1){
			diff_lines = readLines(filename3)
			sqlQuery(pmwAnton,paste("insert into Crestline.Risk_Flags_FlagListDiff_DailyDiff select 'DIFF for ",fund_name,"'",sep=""))
			for(i in 1:length(diff_lines))
				sqlQuery(pmwAnton,paste("insert into Crestline.Risk_Flags_FlagListDiff_DailyDiff select '",diff_lines[i],"'",sep=""))
		}
	}
	
	unlink(filename1)
	unlink(filename2)
	unlink(filename3)	
}

PORTF_LIST <<- c(2,3,4,5,7,9,10,11,13,14,15,16,17,18,1193,1198,5001,5009)
PORTF_DATAFRAME <<- data.frame(id=c(PORTF_LIST,1590,1591,1592,1593,1597,1599,1600,1601,1604,1605,1608,1609,2861,4528,4915,6858,6865,6868,6862),
	ma=c(PORTF_LIST,16,18,9,7,3,17,3,14,5001,5009,17,15,5001,15,17,9,7,3,17))

#sqlcon=pmwAnton
#sqlcom=alerts
run_task = function(task_name, sqlcon, messagecon){
	sqlQuery(messagecon,paste("exec Crestline.Risk_Admin_Log_InsertMessage '",task_name,"', 'Trace', 'Started'", sep=""))
	print(paste('Running ',task_name,'...',sep=''))
        report_date = as.Date(sqlQuery(sqlcon,'select getdate()')[1,1])
        report_date = as.Date(paste(format.Date(report_date,'%Y'),format.Date(report_date,'%m'),'01',sep='-'))
        message('Report Date:',report_date)
        

	if(task_name == '[Crestline].[Risk_PortfolioREPORTS_Dashboard_UpdateDataFromLine]') {
		sqlQuery(sqlcon,"delete from Crestline.Risk_PortfolioREPORTS_Dashboard_FundSpecificData where dataset_id=1")
		sqlQuery(sqlcon,"exec [Crestline].[Risk_PortfolioREPORTS_Dashboard_UpdateDataFromLine] @Sections='GEOGRAPHY'")
		sqlQuery(sqlcon,"exec [Crestline].[Risk_PortfolioREPORTS_Dashboard_UpdateDataFromLine] @Sections='LEVERAGE'")
		sqlQuery(sqlcon,"exec [Crestline].[Risk_PortfolioREPORTS_Dashboard_UpdateDataFromLine] @Sections='HFRI by Fund'")
		sqlQuery(sqlcon,"exec [Crestline].[Risk_PortfolioREPORTS_Dashboard_UpdateDataFromLine] @Sections='STRESS TEST-strategy'")
		sqlQuery(sqlcon,"exec [Crestline].[Risk_PortfolioREPORTS_Dashboard_UpdateDataFromLine] @Sections='ASSET EXPOSURE'")
		sqlQuery(sqlcon,"exec [Crestline].[Risk_PortfolioREPORTS_Dashboard_UpdateDataFromLine] @Sections='Historical Alloc'")
	}
	else if(task_name == 'Clear DB fund returns cache')
		sqlQuery(sqlcon,"delete from Crestline.Risk_PortfolioReturns_DBReturnsCache")
	else if(task_name == '[Crestline].[Risk_PortfolioMonitoring_FundReturns_Import]')
		sqlQuery(sqlcon,"[Crestline].[Risk_PortfolioMonitoring_FundReturns_Import]")
	else if(task_name == 'MA portfolios pre-calculation'){
		sqlQuery(sqlcon,"delete from Crestline.Risk_PortfolioREPORTS_Dashboard_DashboardCache_ForComparison_Gateway")

		PORTF_DATAFRAME_MODEL = rbind(cbind(PORTF_DATAFRAME,mod=PORTF_DATAFRAME$ma),cbind(PORTF_DATAFRAME,mod=-1),cbind(PORTF_DATAFRAME, mod=-2))
		#PORTF_DATAFRAME_DATES = foreach(m=seq(report_date,len=6,by='-1 month'),.combine=rbind)%do%cbind(PORTF_DATAFRAME, dat=as.character(m))
		PORTF_DATAFRAME_DATES = foreach(m=c(rev(seq(report_date,len=3,by='1 month')[-1]),seq(report_date,len=6,by='-1 month')),.combine=rbind)%do%cbind(PORTF_DATAFRAME, dat=as.character(m))

		foreach(i=1:dim(PORTF_DATAFRAME_MODEL)[1], .errorhandling='remove')%dopar%
	        	precalc_portfolio(PORTF_DATAFRAME_MODEL$id[i],report_date,base_path,PORTF_DATAFRAME_MODEL$mod[i])
			
		foreach(i=1:dim(PORTF_DATAFRAME_DATES)[1], .errorhandling='remove')%dopar%
			precalc_portfolio(PORTF_DATAFRAME_DATES$id[i],as.Date(PORTF_DATAFRAME_DATES$dat[i]),base_path,end_date_prev=TRUE,run_portfValues=TRUE,run_proformaValues=TRUE)

		sqlQuery(sqlcon,"exec [Crestline].[Risk_PortfolioREPORTS_Dashboard_LoadData_FromGateway_ToComparison]")
	}
	else if(task_name == 'Dashboard comparison data precalc'){
		foreach(i=PORTF_DATAFRAME$id, .errorhandling='remove') %dopar% {
			sqlcon <<- odbcConnect(pmwName,uid=pmwAnton_uid,pwd=pmwAnton_pwd)
			#for(m in as.Date(seq(report_date,len=2,by='-1 month'))){
			for(m in c(rev(seq(report_date,len=8,by='-1 month')),seq(report_date,len=3,by='1 month')[-1])){
				#query = paste("declare @portf varchar(100) select @portf = ','+portfoliodescription+',' from Crestline.Risk_PortfolioReturns_Portfolios where portfolioid=",i," exec [Crestline].[Risk_PortfolioREPORTS_Dashboard_GetDashboard_Multiple_CalcCache] @DataSet_Description='Risk cleaned', @DataSet_Description2='Risk cleaned', @ReportDates_String=',",format(as.Date(m), format="%Y%m%d"),",', @PortfolioCodes=@portf, @UseNewest=1",sep="")

				#query = paste("exec [Crestline].[Risk_PortfolioREPORTS_Dashboard_GetDashboard_CalcCache_Portfolio_Date] @ReportDate=',",paste(format(as.Date(c(seq(report_date,len=8,by='-1 month'),seq(report_date,len=3,by='1 month')[-1])), format="%Y%m%d"),collapse=","),",', @PortfolioCodes=',",sqlQuery(sqlcon,paste("select portfoliodescription from Crestline.Risk_PortfolioReturns_Portfolios where portfolioid=",i))[1,1],",'",sep="")

				query = paste("exec [Crestline].[Risk_PortfolioREPORTS_Dashboard_GetDashboard_CalcCache_Portfolio_Date] @ReportDate='",as.Date(m),"', @PortfolioID=",i,sep="")

				print(query)
				a=sqlQuery(sqlcon,query)
				#a[1,]
			}
		}
	}
	else if(task_name == 'Update fund universes')
		sqlQuery(sqlcon,"[Crestline].[Risk_FundsAnalysis_UpdateFundUniverses]")
	else if(task_name == 'Calculate AUM,BETA and Performance flags'){
		#sqlQuery(sqlcon,paste("exec Crestline.Risk_Admin_Log_InsertMessage '",task_name,"', 'Trace', 'Starting flags calculation..'", sep=""))
		flags_calc(as.Date('2012-01-01'),report_date,base_path)

		families_list = sqlQuery(sqlcon,'select distinct family_code from Crestline.Risk_PortfolioMonitoring_Flags_Prepared')[,1]		

		sqlQuery(sqlcon,paste("exec Crestline.Risk_Admin_Log_InsertMessage '",task_name,"', 'Trace', '..flags finished, starting report cache precalc..'", sep=""))
		for(fam in families_list){
			r0 = sqlQuery(sqlcon,paste("Crestline.[Risk_Flags_GetFlagInfo_TMP_Group_Report_FillCache] 'RISK','",report_date,"',365,'",fam,"'",sep=""),errors=FALSE)
			if(r0 == -1)
				sqlQuery(sqlcon,paste("exec Crestline.Risk_Admin_Log_InsertMessage '",task_name,"','Error','Report_FillCache problem with ",fam,"'",sep=""))
			else
				sqlQuery(sqlcon,paste("exec Crestline.Risk_Admin_Log_InsertMessage '",task_name,"','Trace','Report_FillCache done for ",fam,"'",sep=""))
		}

		sqlQuery(sqlcon,paste("exec Crestline.Risk_Admin_Log_InsertMessage '",task_name,"', 'Trace', '.. report cache finished, starting alert cache precalc, Manual..'", sep=""))
		for(fam in families_list){
			r0 = sqlQuery(sqlcon,paste("[Crestline].[Risk_Flags_GetFlagInfo_AlertRiskFlags_FillCache] @ConditionTypeID=0, @Family_Code_IN='",fam,"'",sep=""),errors=FALSE)
			if(r0 == -1)
				sqlQuery(sqlcon,paste("exec Crestline.Risk_Admin_Log_InsertMessage '",task_name,"','Error','AlertRiskFlags_FillCache, Manual problem with ",fam,"'",sep=""))
			else
				sqlQuery(sqlcon,paste("exec Crestline.Risk_Admin_Log_InsertMessage '",task_name,"','Trace','AlertRiskFlags_FillCache, Manual done for ",fam,"'",sep=""))
		}

		
		sqlQuery(sqlcon,paste("exec Crestline.Risk_Admin_Log_InsertMessage '",task_name,"', 'Trace', '.. report cache finished, starting alert cache precalc, Percentile..'", sep=""))
		for(fam in families_list){
			r0 = sqlQuery(sqlcon,paste("[Crestline].[Risk_Flags_GetFlagInfo_AlertRiskFlags_FillCache] @ConditionTypeID=1, @Family_Code_IN='",fam,"'",sep=""),errors=FALSE)
			if(r0 == -1)
				sqlQuery(sqlcon,paste("exec Crestline.Risk_Admin_Log_InsertMessage '",task_name,"','Error','AlertRiskFlags_FillCache, Percentile problem with ",fam,"'",sep=""))
			else
				sqlQuery(sqlcon,paste("exec Crestline.Risk_Admin_Log_InsertMessage '",task_name,"','Trace','AlertRiskFlags_FillCache, Percentile done for ",fam,"'",sep=""))
		}
	}
	else if(task_name == 'Load monitoring sheet diff to DB') 
		load_diff()
	else if(task_name == 'Run alerts from tool') {
		alerts = sqlQuery(sqlcon,paste("select * from Crestline.Risk_RiskAlerts_AlertItems where isactive=1",sep=""))
		for(i in 1:nrow(alerts)) {
			alert_value = run_alert_item(alerts[i,"ItemID"],sqlcon,report_date,'HTML')
			if(!is.null(alert_value)){
				sqlQuery(sqlcon,paste("insert into Crestline.AlertRiskFlags_AlertsTab select 1,'Alerts tool: percentile alert for ",alert_value$index," as of ",alert_value$date,"','",alert_value$html,"',getdate()",sep=""))
				print(paste('Alert:',alert_value$index))
			}
		}
	} else if(task_name == 'Reschedule flags alerts 10 min') 
		sqlQuery(sqlcon,"declare @a datetime = dateadd(d,-1,dateadd(mi,10,getdate()))
exec [vm-clportal].alerts.dbo.procRefreshSite 103,@a
exec [vm-clportal].alerts.dbo.procRefreshSite 111,@a
exec [vm-clportal].alerts.dbo.procRefreshSite 112,@a
exec [vm-clportal].alerts.dbo.procRefreshSite 123,@a
exec [vm-clportal].alerts.dbo.procRefreshSite 131,@a
set @a = case datepart(dw,getDate()) when 2 then dateadd(wk,-1,dateadd(mi,10,getDate())) else dateadd(dw,2-datepart(dw,getDate()),dateadd(mi,10,getDate())) end   
exec [vm-clportal].alerts.dbo.procRefreshSite 114,@a
exec [vm-clportal].alerts.dbo.procRefreshSite 149,@a
exec [vm-clportal].alerts.dbo.procRefreshSite 150,@a
exec [vm-clportal].alerts.dbo.procRefreshSite 169,@a
exec [vm-clportal].alerts.dbo.procRefreshSite 175,@a
exec [vm-clportal].alerts.dbo.procRefreshSite 176,@a
exec [vm-clportal].alerts.dbo.procRefreshSite 177,@a
set @a = dateadd(n,90,@a)   
exec [vm-clportal].alerts.dbo.procRefreshSite 170,@a")
	else if(task_name == 'Reschedule flags alerts 20 hr') 
		sqlQuery(sqlcon,"declare @a datetime = dateadd(d,-1,dateadd(hh,20,getdate()))
exec [vm-clportal].alerts.dbo.procRefreshSite 103,@a
exec [vm-clportal].alerts.dbo.procRefreshSite 111,@a
exec [vm-clportal].alerts.dbo.procRefreshSite 112,@a
exec [vm-clportal].alerts.dbo.procRefreshSite 123,@a
exec [vm-clportal].alerts.dbo.procRefreshSite 131,@a
set @a = case datepart(dw,getDate()) when 2 then dateadd(wk,-1,dateadd(hh,20,getDate())) else dateadd(dw,2-datepart(dw,getDate()),dateadd(hh,20,getDate())) end   
exec [vm-clportal].alerts.dbo.procRefreshSite 114,@a
exec [vm-clportal].alerts.dbo.procRefreshSite 149,@a
exec [vm-clportal].alerts.dbo.procRefreshSite 150,@a
exec [vm-clportal].alerts.dbo.procRefreshSite 169,@a
exec [vm-clportal].alerts.dbo.procRefreshSite 175,@a
exec [vm-clportal].alerts.dbo.procRefreshSite 176,@a
exec [vm-clportal].alerts.dbo.procRefreshSite 177,@a
set @a = dateadd(n,90,@a)   
exec [vm-clportal].alerts.dbo.procRefreshSite 170,@a")


	print(paste('Finished ',task_name,'...',sep=''))
	sqlQuery(messagecon,paste("exec Crestline.Risk_Admin_Log_InsertMessage '",task_name,"', 'Trace', 'Finished'", sep=""))
}

#secs_step=60; secs_total=61
wait_for_log_message_today = function(msg_start_substring,secs_step,secs_total){
	time_spent = 0; 
	log_record_found = toString(sqlQuery(pmwAnton,paste("select max(convert(datetime,floor(convert(float,dateadd(hh,4,message_ts))))) as MaxFinishDate from Crestline.Risk_Admin_Log_Messages where sourceid=3 and messagebody like '",msg_start_substring,"%'",sep=""))$MaxFinishDate)==Sys.Date()
	while(time_spent<secs_total && !log_record_found){
		log_record_found = toString(sqlQuery(pmwAnton,paste("select max(convert(datetime,floor(convert(float,dateadd(hh,4,message_ts))))) as MaxFinishDate from Crestline.Risk_Admin_Log_Messages where sourceid=3 and messagebody like '",msg_start_substring,"%'",sep=""))$MaxFinishDate)==Sys.Date()
		Sys.sleep(secs_step)
		time_spent = time_spent + secs_step
	}

	if(!log_record_found){
		sqlQuery(pmwAnton,paste("exec Crestline.Risk_Admin_Log_InsertMessage 'General R execution messages', 'Error', 'R script execution stopped, because ''",msg_start_substring,"'' message didn''t appear in log file for the time expected'",sep=""))
		stop(paste("Wait time has passed, but '",msg_start_substring,"' message didn't appear in log file",sep=""))
	}
}

pmwAnton <<- odbcConnect(pmwName,uid=pmwAnton_uid,pwd=pmwAnton_pwd)
alerts <- odbcConnect("alerts",uid=pmwAnton_uid,pwd=pmwAnton_pwd)


run_task('Reschedule flags alerts 20 hr',alerts,pmwAnton)

run_task('[Crestline].[Risk_PortfolioMonitoring_FundReturns_Import]',pmwAnton,pmwAnton)
run_task('[Crestline].[Risk_PortfolioREPORTS_Dashboard_UpdateDataFromLine]',pmwAnton,pmwAnton)
run_task('Dashboard comparison data precalc',pmwAnton,pmwAnton)

run_task('Clear DB fund returns cache',pmwAnton,pmwAnton)

wait_for_log_message_today("Finished loading classification spreadsheet", 60, 60*60*5)
run_task('MA portfolios pre-calculation',pmwAnton,pmwAnton)

wait_for_log_message_today("Finished loading flags data", 60, 60*60*5)
run_task('Calculate AUM,BETA and Performance flags',pmwAnton,pmwAnton)

run_task('Load monitoring sheet diff to DB',pmwAnton,pmwAnton)

#if (production) 
run_task('Reschedule flags alerts 10 min',alerts,pmwAnton)



#if (production) if(as.POSIXlt(Sys.Date())$wday == 1) 
run_task('Update fund universes',pmwAnton,pmwAnton)

run_task('Run alerts from tool',pmwAnton,pmwAnton)
	





