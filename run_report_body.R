# rm(list = ls(all = TRUE))
sink()

Sys.setenv(TZ="CST")

message('Run.R:: Start ',format(Sys.time(), "%a %b %d %X %Y"))

if(1==0) source("/var/www/R/run/env_init.R")
if(1==0) for(i in 1:100) { system('rm -f /var/www/rtemp/*'); Sys.sleep(0.1); print(i); }

if(POST$report == 'port_rt') {

	if (1==0)
	  {
	    #base_path='/var/www/rtemp_dev/'
	    source("/var/www/R/run/env_init.R")
	    		
	    report='port_rt'
	    portfolio_id='16'
	    ma_portfolio_id='16'
	    session_id='674'
	    end_date=as.Date('2013-03-01')
	    alloc_date=as.Date('2013-01-01')
	    start_date=as.Date('2001-02-01')
	    windows = c('12','24','36','60','120','ytd')
	    benchmark_universe='-1'
	    result_code='Allocation.History-TAB'
	    default_seq_id='8'
	    fund_seq_map_id='395'#as.character(sqlQuery(pmwAnton,"select max(MappingID) from Crestline.Risk_PortfolioReturns_RuleSequence_Fund_Mapping where [Description]='Risk Override'"))
	    default_fund_seq_map_id='392'#as.character(sqlQuery(pmwAnton,"select max(MappingID) from Crestline.Risk_PortfolioReturns_RuleSequence_Fund_Mapping where [Description]='Risk Override'"))
	    cache_mode='CACHE'#'CACHE'#'ALLOC_ONLY'
	    cash_RR_mode=0
	    w=12
	    benchmark='SPTR_INDEX'
	    method='gaussian'
	    pca_method='svd'
	    cleanoutliers='none'
	    pca.n=10
	    r_r_only=TRUE
	    p=0.95
	    image_format='String bytes'
	    allocation_type='All dates'#'All dates'#'Last date'#'XML some dates'
	    allocations_table = "empty"
	    allocations_values_string = ""
	    allocations_dates_string = ""
	    rules_table = "empty"
	    fundnames_string = ""
            lookback_period=9999999
            fixed_allocation=0
	    optimization_alloc_date = as.Date('2013-01-01')




	  } else if (1==0)
	  {
	    message('ARGS=',SERVER$args)
	    message('Request=',SERVER$the_request)
	    
	    message('End Date:',GET$end_date)
	    report=GET$report
	    portfolio_id = GET$portfolio_id
	    session_id = GET$session_id
	    end_date = as.Date(GET$end_date)
	    alloc_date = as.Date(GET$alloc_date)
	    start_date = as.Date(GET$start_date)
		                                #windows = formData[[4]]
	    windows = c('12','24','36','60','120','ytd')
	    result_code = GET$result_code
	    default_seq_id = GET$default_seq_id
	    fund_seq_map_id = GET$fund_seq_map_id
	    cache_mode='FULL'
	    w=GET$w
	    benchmark=GET$benchmark
	    method=GET$method
	    pca_method=GET$pca_method
	    
	    cleanoutliers=GET$cleanoutliers
	    p=GET$p
	    ma_portfolio_id = if (GET$ma_portfolio_id == -1) portfolio_id else GET$ma_portfolio_id
	    image_format = GET$image_format
	    allocation_type = GET$allocation_type
            lookback_period=GET$lookback_period
            
	  } else 
	{
	  message('ARGS=',SERVER$args)
	  message('Request=',SERVER$the_request)
	  message('POST=',POST)
	  
	  message('End Date:',POST$end_date)
	  report=POST$report
	  portfolio_id = POST$portfolio_id
	  session_id = POST$session_id
	  end_date = as.Date(POST$end_date)
          alloc_date  = end_date
           if (!is.null(POST$alloc_date))
             alloc_date=as.Date(POST$alloc_date)
           else
             {
               end_date=paste(format(end_date-20,'%Y-%m'),'-01',sep='')
             }
	  start_date = as.Date(POST$start_date)
		                                #windows = formData[[4]]
	  windows = c('12','24','36','60','120','ytd') 
	  benchmark_universe=if (is.null(POST$benchmark_universe)) '-1' else POST$benchmark_universe
	  result_code = POST$result_code
	  default_seq_id = POST$default_seq_id
	  fund_seq_map_id = POST$fund_seq_map_id
	  default_fund_seq_map_id = POST$default_fund_seq_map_id
	  cache_mode = POST$cache_mode
	  cash_RR_mode = if (is.null(POST$cash_RR_mode)) 0 else POST$cash_RR_mode
	  cache_invalidate_level = if (is.null(POST$cache_invalidate_level)) 0 else POST$cache_invalidate_level 
	  w=as.numeric(POST$w)
	  benchmark=POST$benchmark
	  method=POST$method
	  pca_method=POST$pca_method
	  cleanoutliers=POST$cleanoutliers
	  p=as.numeric(POST$p)
	  ma_portfolio_id = if (POST$ma_portfolio_id == 0) portfolio_id else POST$ma_portfolio_id
	  image_format = POST$image_format
	  allocation_type = POST$allocation_type
	  #allocations_table = gsub('/AMP/','&',as.character(POST$allocations_table))

	  allocations_table = if (is.null(POST$allocations_table)) 'empty' else POST$allocations_table
	  allocations_values_string = POST$allocations_values_string
	  allocations_dates_string = POST$allocations_dates_string
	  rules_table = if (is.null(POST$rules_table)) 'empty' else POST$rules_table
	  fundnames_string = POST$fundnames_string

	  pca.n=as.integer(POST$pca.n)
	  r_r_only=POST$r_r_only
          lookback_period=POST$lookback_period
          fixed_allocation=POST$fixed_allocation

          constraints_xml=POST$constraints_xml
	  optimization_alloc_date = POST$optimization_alloc_date
	  optimization_through_date = POST$optimization_through_date

	  min_allocation = POST$min_allocation
	  min_beta = POST$min_beta
	}
	 

	    message('Starting script:',format(Sys.time(), "%a %b %d %X %Y"))
	    message('report=',report,'\n',
		    'portfolio_id =', portfolio_id,'\n',
		    'session_id = ',session_id,'\n',
		    'end_date = ',end_date,'\n',
		    'alloc_date = ',alloc_date,'\n',
		    'start_date = ',start_date,'\n',
		    'result_code =', result_code,'\n',
		    'default_seq_id =', default_seq_id,'\n',
		    'fund_seq_map_id = ',fund_seq_map_id,'\n',
		    'cache_mode= ',cache_mode,'\n',
		    'cash_RR_mode= ',cash_RR_mode,'\n',
		    'cache_invalidate_level= ',cache_invalidate_level,'\n',
		    'allocation_type= ',allocation_type,'\n',
		    'base_path=',base_path,'\n',
		    'image_format=',image_format,'\n',
		    'ma_portfolio_id=',ma_portfolio_id,'\n',
		    'benchmark=',benchmark,'\n',
		    'benchmark_universe=',benchmark_universe,'\n',
		    'w=',w,'\n',
		    'p=',p,'\n',
		    'pca.n=',pca.n,'\n',
		    'pca_method=',pca_method,'\n',
                    'len(allocations_table)=',nchar(allocations_table),'\n',
                    'len(rules_table)=',nchar(rules_table),'\n',
                    'lookback_period=',lookback_period,'\n',
                    'fixed_allocation=',fixed_allocation,'\n',
                    'constraints_xml=',constraints_xml,'\n',
		    'optimization_alloc_date=',optimization_alloc_date,'\n',
		    '<=====>\n'


		    )
	    message('windows:',paste(windows,collapse=','))

	message('XML nchar: ',nchar(allocations_table))
        pmwAnton <<- odbcConnect(pmwName,uid=pmwAnton_uid,pwd=pmwAnton_pwd)
	    #pmwAnton <<- odbcConnect("pmw",uid=pmw_uid,pwd=pmw_pwd)
	    message('windows:',paste(windows,collapse=','))
	    params <- hash()
	    params$PortfolioID = portfolio_id
	    params$MA_PortfolioID = ma_portfolio_id
	    params$DefaultSequenceID = default_seq_id
	    params$FundSequenceMappingID = fund_seq_map_id
	    params$Default_FundSequenceMappingID = default_fund_seq_map_id
	    params$StartDate = start_date
	    params$EndDate = end_date
	    params$AllocationDate = alloc_date
	    params$Windows = windows
	    params$BenchmarkUniverse=benchmark_universe
	    params$base_path=base_path
	    params$benchmark=trim(isnull(benchmark,'SPTR_INDEX'))
	    params$CacheMode = cache_mode

	    params$CashRRMode = cash_RR_mode
	    params$CacheInvalidateLevel = cache_invalidate_level
	    params$w=isnull(w,24)
	   if (isnull(params$w,0)<12) params$w=12
	    
	    params$method=isnull(method,'gaussian')
	    params$pca_method=isnull(pca_method,'svd')
	    params$pca.n=isnull(pca.n,10)
	    params$RR_Only=r_r_only
	    params$cleanoutliers=isnull(cleanoutliers,'none')
	    
	    params$SessionID=as.numeric(session_id)
	    params$p=isnull(p,0.95)
	    params$ImageFormat=image_format
	    params$AllocationType=allocation_type
	    params$AllocationsTableXML=allocations_table
	    params$RulesTableXML=rules_table
	    
	    params$AllocationsValuesString=allocations_values_string
	    params$AllocationsDatesString=allocations_dates_string
	    params$FundnamesString=fundnames_string

	    params$rtStart=as.Date('1990-01-01')
	#    params$rtEnd=as.Date(paste(substr(as.character(as.Date(sqlQuery(pmwAnton,"SELECT {fn NOW()}")[[1]])+366),1,7),'-01',sep=''))
	    params$rtEnd=as.Date(paste(substr(as.character(as.Date(sqlQuery(pmwAnton,"SELECT {fn NOW()}")[[1]])+0),1,7),'-01',sep=''))
            params$lookback_period=isnull(lookback_period,9999999999)
            params$fixed_allocation=isnull(fixed_allocation,0)

	    params$constraints_xml=constraints_xml
	    params$optimization_alloc_date = optimization_alloc_date
	    params$optimization_through_date = optimization_through_date

	    params$min_allocation = min_allocation
	    params$min_beta = min_beta
        
	    if (result_code=='Rolling.modVaR.Graph')
	      {
		if (params$method=='kernel') params$method='gaussian'
	      }
	    
	    
	    return_id=result_code
#message('SAVING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!params!@!!!!!!!!!!!!!!!!!!!!!!!!!!!')
            save(list=c('params','result_code'),file=paste(params$base_path,'params.RData',sep=''))
#           load(paste(base_path,'params.RData',sep=''))

	    #source("/var/www/R/lib/grt_ad_web.R")
	    #XML_RES = ''###############get_returns_Portfolio_PARTIAL(params,result_code)
	    XML_RES = get_returns_Portfolio_PARTIAL(params,result_code)
	    message('Finishing script')
	    cat(gsub('&','@',XML_RES))
	    message('RUN_AD!!!')

	    file=paste(base_path,"run_AD.txt",sep='')
	    sink(file)
	    cat(gsub('&','@',XML_RES))
	    sink()
	    
	    
} else {
  cat('Unknown Report:',report,'\n')
}

message('Run.R:: End ',format(Sys.time(), "%a %b %d %X %Y"),'\n\n',
         result_code,'::Returned String Length',nchar(XML_RES))

#sink(outfile(paste('WEB/',result_code,sep='')))
#print(XML_RES)
#sink()

DONE
