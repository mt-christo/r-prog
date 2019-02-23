#!/usr/bin/Rscript

source('R121.R')
library(timeDate)
update_ids0('/root/r-prog/')

indic_to_param = function(ind,cf) { cf[1]+ind*cf[2] }
params1 = list(100,0.005*1:200,0,0,0); gparams = as.matrix(expand.grid(params1))

load('ids0xts.RData')

snp = ids0[,'SP500']; trs = ids0[,'DGS10']; lbr = ids0[,'USD3MTD156N']; vix0=ids0[,'VIX.Close']/(100*(12*30)^0.5)
ids = merge.xts(snp,trs,lbr)[!is.na(snp),]; 
ids = na.locf(ids)[index(ids)>'2007-05-13',] # VIX is not included in Indices list, used just for sd!
vix0 = na.locf(vix0)[index(vix0)>'2007-05-13',]
ids = ids[index(ids)%in%index(vix0),]
vix0 = vix0[index(vix0)%in%index(ids),]

idss = ids[,'USD3MTD156N'] 
INDICES = idss;

pass_date = function(date, Q_CNT, tail_max, update_htm, is_first_date = FALSE){
	counts_filename = sub('REPLACEME',tail_max,'/var/www/html/foutput/curr_countsREPLACEME.RData')
	row_mask = sub('REPLACEME',tail_max,'<!-- add new row REPLACEME-->')
	return_mask = sub('REPLACEME',tail_max,'<!--return here REPLACEME-->')
	counts_mask = sub('REPLACEME',tail_max,'<!--counts here REPLACEME-->')
	expect_mask = sub('REPLACEME',tail_max,'<!--expectation here REPLACEME-->')

	curr_date = as.Date(date)

	indices_val = as.numeric(INDICES[curr_date,])
	vix_val = as.numeric(vix0[curr_date])
	VALUE_ACT = as.numeric(ids[curr_date,'SP500'])

	optim_cf = c(0.90508959, -0.02026926)#c(0.90521717, -0.01395737)#c(0.890292317, 0.009454073)
	param_fit = which.min(as.numeric(rowSums(t(t(gparams[,2]) - indic_to_param(indices_val,optim_cf))^2)))
	distrib_fit = vix_val * get(load(paste('RData/50k/nk/res_CRYSTAL43-nk-',paste(gparams[param_fit,],collapse='-'),'.RData',sep='')))
	distrib_fit = c(distrib_fit,-distrib_fit)
	distrib_quant = quantile(distrib_fit,seq(0,tail_max,len=Q_CNT))
	tail_max_value = as.numeric(distrib_quant[Q_CNT])

	plot_df = distrib_fit[abs(distrib_fit)<5]*100
	rng = range(plot_df)
	h = hist(plot_df,br=seq(rng[1],rng[2],len=(rng[2]-rng[1])/0.15),plot=FALSE)
	h$counts = h$density*100

	img_path = paste('distrib_',curr_date,tail_max,'.png',sep='')
	#png(paste('Output_res\\',img_path,sep=''))
	png(paste('/var/www/html/foutput/',img_path,sep=''))
	plot(h,xlim=if(tail_max==1) c(-7,7) else c(-14,tail_max_value*100),ylim=if(tail_max==1) c(0,100) else c(0,h$counts[max(which(h$mids<=tail_max_value*100))]),xlab='Return, %',ylab='Probability density, %',border='black',col='lightgray',main='')
	dev.off()
	if(is_first_date){
		curr_date = as.Date(curr_date) + 1
		while(isHoliday(as.timeDate(curr_date)))
			curr_date = as.Date(curr_date) + 1
		
		curr_counts = data.frame(Date=curr_date) 
		new_counts = array(0,Q_CNT-1)
		curr_counts = cbind(curr_counts, t(distrib_quant), t(new_counts))
		#save(curr_counts, file='Output_res\\curr_counts.RData')
		save(curr_counts, file=counts_filename)

		#conn = file('Output_res\\forecast_page.htm')
		conn = file('/var/www/html/foutput/forecast_page.htm')

		#htm_lines = c('<p style="font-size: 20px;font-family: sans-serif;">Tomorrow\'s S&P index return quantile forecasts (% to current date):</p>'
		#		,'<table border="1" cellspacing="0" cellpadding="2" style="font-family: sans-serif; text-align:center;" width="1800px">'
		#		,'<tr style="font-size: 18px;"><td>Date of forecast</td><td>5%</td><td>10%</td><td>15%</td><td>20%</td><td>25%</td><td>30%</td><td>35%</td><td>40%</td><td>45%</td><td>50%</td><td>55%</td><td>60%</td><td>65%</td><td>70%</td><td>75%</td><td>80%</td><td>85%</td><td>90%</td><td>95%</td><td>Distribution lo-res</td><td>Last return, %</td><td>Hit counts so far</td></tr>'
		#		,'<!-- add new row -->'
		#		,'</table>')

		#htm_lines = c('<!DOCTYPE HTML>'
		#	,'<head>'
		#	,'<link rel="stylesheet" type="text/css" href="forecast_style.css">'
		#	,'</head>'
		#	,'<BODY>'
		#	,'<TABLE class="bordered"><caption><p>History of my daily forecast of probability density distribution for S&P500 index return (percentiles)<p></caption>'
		#	,'<THEAD><tr><th>Date of forecast</th><th>5%</th><th>10%</th><th>15%</th><th>20%</th><th>25%</th><th>30%</th><th>35%</th><th>40%</th><th>45%</th><th>50%</th><th>55%</th><th>60%</th><th>65%</th><th>70%</th><th>75%</th><th>80%</th><th>85%</th><th>90%</th><th>95%</th><th>Density, lo-res</th><th><center>Realized <br>S&P500 return, %</th><th>Hit counts so far</th><TBODY></tr></THEAD>'
			#,'<THEAD><tr><th>Date of forecast</th><th>5%</th><th>10%</th><th>15%</th><th>20%</th><th>25%</th><th>30%</th><th>35%</th><th>40%</th><th>45%</th><th>50%</th><th>55%</th><th>60%</th><th>65%</th><th>70%</th><th>75%</th><th>80%</th><th>85%</th><th>90%</th><th>95%</th><th>Last return, %</th><th>Hit counts so far</th><TBODY></tr></THEAD>'
		#	,'<!-- add new row -->'
		#	,'</TBODY>'
		#	,'</table>'
		#	,'<br><center><p style="color:gray">Anton Slepnev, slepnev @ yandex.ru</p>'
		#	,'</BODY></HTML>')
		htm_lines = c('<!DOCTYPE HTML>'
			,'<head>'
			,'<link rel="stylesheet" type="text/css" href="forecast_style.css">'
			,'</head>'
			,'<BODY>'
			,'<br>'
			,'<center><table border="0" width="60%" style="text-align:JUSTIFY;font-size:14px">'
			,'<tr><td>Let\'s imagine a network of 100 market analysts, each producing a forecast for the stock price return for tomorrow, Close/Close. While doing that, each analyst looks at forecasts made by other analysts, and slightly corrects his own forecast based on what he sees. Then, they do some trading with each other, each based on his own forecast. What kind of final return will collectively be produced?'
			,'<br>'
			,'<br>Let\'s make two basic assumptions: 1) if their forecasts all happen to be the same, the return will go to positive/negative infinity, and 2) if their forecasts happen to be all mixed and different, then return will stick around zero. Then, if their forecasts were <i>somewhat</i> correlated, the result will be somewhere in between, depending on <i>to what extent</i> their forecasts were correlated. Then let\'s imagine that, after some weird modeling, we come up with a simple <b>one-parameter</b> model that determines such correlation structure in probabilistic terms. Which, in turn, determines the resulting return distribution.'
			,'<br><br>That\'s what I did. The table below is an attempt to test the results of straight factor fitting of my model parameter X to a couple of major world economic indicators, in Bayesian terms - judging distribution assumptions by counting the quantiles hit by realized return values. Fitting was based on data from 5/13/2007 to 6/10/2015. The result was a factor model: my lone X vs. market indicators.'
			,'<br><br>Now starting 6/11/2015, I do the following every day:'
			,'<ul>'
			,'<li>take the market indicators from last night</li> '
			,'<li>calculate my X</li>'
			,'<li>build the return distribution</li>'
			,'<li>publish its 5% quantiles in the table below - that\'s my prediction for the day</li>'
			,'<li>after the markets close and I know the actual S&P return, I check which quantile was hit and increase the quantile counter.</li> '
			,'</ul>'
			,'Let\'s see what is happening:</tr></td></table></center><br><br>'
			,'<div class="korpus">'
			,'<input type="radio" name="odin" checked="checked" id="vkl1"/><label for="vkl1">5% quantiles</label>'
			,'<input type="radio" name="odin" id="vkl2"/><label for="vkl2">Lower 5% decomposition, 1% step</label>'
			#,'<input type="radio" name="odin" checked="checked" id="vkl1"/><label for="vkl1">10% quantiles</label>'
			#,'<input type="radio" name="odin" id="vkl2"/><label for="vkl2">Lower 10% decomposition, 1% step</label>'
			,'<div><br>'
			,'<TABLE class="bordered">'
			,'<THEAD><tr><th>Date of forecast</th><th>5%</th><th>10%</th><th>15%</th><th>20%</th><th>25%</th><th>30%</th><th>35%</th><th>40%</th><th>45%</th><th>50%</th><th>55%</th><th>60%</th><th>65%</th><th>70%</th><th>75%</th><th>80%</th><th>85%</th><th>90%</th><th>95%</th><th>Density, lo-res</th><th><center>Realized <br>S&P500 return, %</th><th><center>Hit counts so far</th><th><center>Expectation<br>(each cell)</th><TBODY></tr></THEAD>'
			#,'<THEAD><tr><th>Date of forecast</th><th>10%</th><th>20%</th><th>30%</th><th>40%</th><th>50%</th><th>60%</th><th>70%</th><th>80%</th><th>90%</th><th>Density, lo-res</th><th><center>Realized <br>S&P500 return, %</th><th><center>Hit counts so far</th><TBODY></tr></THEAD>'
			,'<!-- add new row 1-->'
			,'</TBODY>'
			,'</table>'
			,'</div>'
			,'<div><br>'
			,'<TABLE class="bordered">'
			,'<THEAD><tr><th>Date of forecast</th><th>1%</th><th>2%</th><th>3%</th><th>4%</th><th>5%</th><th><center>Density, lo-res</th><th><center>Realized <br>S&P500 return, %</th><th><center>Hit counts so far</th><th><center>Expectation<br>(each cell)</th><TBODY></tr></THEAD>'
			,'<!-- add new row 0.05-->'
			#,'<THEAD><tr><th>Date of forecast</th><th>1%</th><th>2%</th><th>3%</th><th>4%</th><th>5%</th><th>6%</th><th>7%</th><th>8%</th><th>9%</th><th><center>Density, lo-res</th><th><center>Realized <br>S&P500 return, %</th><th><center>Hit counts so far</th><TBODY></tr></THEAD>'
			#,'<!-- add new row 0.1-->'
			,'</TBODY>'
			,'</table>'
			,'</div></div>'
			,'<br><center><p style="color:gray">Anton Slepnev, slepnev @ yandex.ru</p>'
			,'</BODY></HTML>')
		
		if(update_htm) writeLines(htm_lines, conn)
		close(conn)
	} else {
		#load('Output_res\\curr_counts.RData')
		load(counts_filename)
		last_quants = as.numeric(curr_counts[dim(curr_counts)[1],2:(1+Q_CNT)])
		new_counts = as.numeric(curr_counts[dim(curr_counts)[1],(2+Q_CNT):dim(curr_counts)[2]]) + diff(VALUE_ACT<last_quants)
		
		curr_date = as.Date(curr_date) + 1
		while(isHoliday(as.timeDate(curr_date)))
			curr_date = as.Date(curr_date) + 1
		
		curr_counts = rbind(curr_counts, 
					  cbind(data.frame(Date=curr_date), 
					 	  t(distrib_quant), 
						  t(new_counts)))
		#save(curr_counts, file='Output_res\\curr_counts.RData')
		save(curr_counts, file=counts_filename)
	}	

	#conn = file('Output_res\\forecast_page.htm')
	conn = file('/var/www/html/foutput/forecast_page.htm')
	htm_lines = readLines(conn)
	htm_lines = gsub(return_mask, format(round((exp(VALUE_ACT)-1)*100,2), nsmall=2), htm_lines)#digits=3,scientific=FALSE), htm_lines)
	htm_lines = gsub(counts_mask, paste('<center>&nbsp',paste(new_counts,collapse='&nbsp&nbsp'),'&nbsp</center>',sep=''), htm_lines)
	exp_value = round(tail_max*nrow(curr_counts)/length(new_counts),1)
	htm_lines = gsub(expect_mask, if(exp_value == 0) '' else exp_value, htm_lines)
	j = which(htm_lines==row_mask)
	writeLines(c(htm_lines[1:j],
		paste('<tr><td>',curr_date,'</td><td>',
			paste(format(round(((exp(distrib_quant)[if(tail_max==1) -c(1,Q_CNT) else -1])-1)*100,2), nsmall=2)
			#paste(format(as.numeric((exp(distrib_quant)[if(tail_max==1) -c(1,Q_CNT) else -1])-1)*100,digits=1,scientific=FALSE)
				,sep='',collapse='</td><td>'),'<td><center><a href="',img_path,'"><img src="',img_path,'" style="width:30px;height:20px;"></a></td><td><center>',return_mask,'</td><td>',counts_mask,'</td><td><center>',expect_mask,'</td></tr>',sep=''),
				#,sep='',collapse='</td><td>'),'<td>',format((exp(VALUE_ACT)-1)*100,digits=2,scientific=FALSE),'</td><td>',paste(new_counts,collapse=' '),'</td></tr>',sep=''),
		htm_lines[(j+1):length(htm_lines)]), conn)
	close(conn)

	print(curr_date)
}


pass_date(as.Date('2015-06-10'), 21, 1, TRUE, TRUE)
for(date in index(ids)[(which(index(ids)==as.Date('2015-06-10'))+1):dim(ids)[1]]) pass_date(date, 21, 1, FALSE, FALSE)
#pass_date(as.Date('2015-06-10'), 11, 1, TRUE, TRUE)
#for(date in index(ids)[(which(index(ids)==as.Date('2015-06-10'))+1):dim(ids)[1]]) pass_date(date, 11, 1, FALSE, FALSE)

pass_date(as.Date('2015-06-10'), 6, 0.05, FALSE, TRUE)
for(date in index(ids)[(which(index(ids)==as.Date('2015-06-10'))+1):dim(ids)[1]]) pass_date(date, 6, 0.05, FALSE, FALSE)
#pass_date(as.Date('2015-06-10'), 11, 0.1, FALSE, TRUE)
#for(date in index(ids)[(which(index(ids)==as.Date('2015-06-10'))+1):dim(ids)[1]]) pass_date(date, 11, 0.1, FALSE, FALSE)

##date = index(ids)[dim(ids)[1]]
#if(!date%in%get(load('Output_res\\curr_counts.RData'))[,1])
##for(i in index(ids))
	##if(!as.Date(i)%in%get(load('/var/www/html/FOutput/curr_counts.RData'))[,1])
		##pass_date(as.Date(i), FALSE)







