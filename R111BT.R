source('R121.R')
library(timeDate)
update_ids0('/root/r-prog/')

indic_to_param = function(ind,cf) { cf[1]+ind*cf[2] }
params1 = list(100,0.005*1:200,0,0,0); gparams = as.matrix(expand.grid(params1))

load('ids0xts.RData')

snp = ids0[,'SP500']; 
trs = ids0[,'DGS10']; 
lbr = ids0[,'USD3MTD156N']; 
vix0=ids0[,'VIX.Close']/(100*(252)^0.5); 
snpindex = ids0[,'SP500INDEX']; 

ids = merge.xts(snp,trs,lbr,snpindex)[!is.na(snp),]; 
ids = na.locf(ids)[index(ids)>'2007-05-13',] # VIX is not included in Indices list, used just for sd!
vix0 = na.locf(vix0)[index(vix0)>'2007-05-13',]
ids = ids[index(ids)%in%index(vix0),]
vix0 = vix0[index(vix0)%in%index(ids),]

idss = ids[,'USD3MTD156N'] 
INDICES = idss;

#date=run_dates[100]; Q_CNT=21; tail_max=1; update_htm=TRUE; is_first_date=TRUE; fill_bt = TRUE
pass_date = function(date, Q_CNT, tail_max, update_htm, is_first_date = FALSE, fill_bt = FALSE){
	curr_date = as.Date(date)

	indices_val = as.numeric(INDICES[curr_date,])
	vix_val = as.numeric(vix0[curr_date])
	VALUE_ACT = as.numeric(ids[curr_date,'SP500'])

	optim_cf = c(0.88483370, -0.01748615)
	param_fit = which.min(as.numeric(rowSums(t(t(gparams[,2]) - indic_to_param(indices_val,optim_cf))^2)))
	distrib_fit = vix_val * get(load(paste('RData/50k/nk/res_CRYSTAL43-nk-',paste(gparams[param_fit,],collapse='-'),'.RData',sep='')))
	distrib_fit = c(distrib_fit,-distrib_fit)

	for(i in c(1,2,4,8)){
		for(j in c(0.005,0.008,0.01)){
FWINDOW = i
SHIFT = j
f_fit = distrib_fit*sqrt(FWINDOW)
f_norm = rnorm(length(distrib_fit), 0, vix_val*sqrt(FWINDOW))
snp_curr = as.numeric(snpindex[curr_date]); 
snp_fut = as.numeric(snpindex[index(ids)[which(index(ids)==curr_date) + FWINDOW]])

bt_res[[paste(i)]][[paste(j)]][curr_date,'fn'] <<- mean(ifelse(f_fit< -SHIFT, -SHIFT - f_fit, 0))
bt_res[[paste(i)]][[paste(j)]][curr_date,'bsn'] <<- mean(ifelse(f_norm < -SHIFT, -SHIFT - f_norm, 0))
bt_res[[paste(i)]][[paste(j)]][curr_date,'pn'] <<- if(snp_fut >= snp_curr*(1-SHIFT)) 0 else snp_curr*(1-SHIFT) - snp_fut
bt_res[[paste(i)]][[paste(j)]][curr_date,'spcurr'] <<- snp_curr
bt_res[[paste(i)]][[paste(j)]][curr_date,'spfut'] <<- snp_fut
print(i)
print(j)
		}
	}
	
	print(curr_date)
}

run_dates <- index(ids)[which(index(ids)==as.Date('2012-05-10')):dim(ids)[1]]
bt <- xts(order.by=run_dates)
bt <- cbind(bt, "fn" = NA, "bsn" = NA, "pn" = NA, "spcurr" = NA, "spfut" = NA) 
bt_res = list()
bt_res[[paste(1)]]=list(); 
	bt_res[[paste(1)]][[paste(0.005)]] = xts(order.by=run_dates); bt_res[[paste(1)]][[paste(0.005)]] = cbind(bt_res[[paste(1)]][[paste(0.005)]], "fn" = NA, "bsn" = NA, "pn" = NA, "spcurr" = NA, "spfut" = NA) 
	bt_res[[paste(1)]][[paste(0.008)]] = xts(order.by=run_dates); bt_res[[paste(1)]][[paste(0.008)]] = cbind(bt_res[[paste(1)]][[paste(0.008)]], "fn" = NA, "bsn" = NA, "pn" = NA, "spcurr" = NA, "spfut" = NA) 
	bt_res[[paste(1)]][[paste(0.01)]] = xts(order.by=run_dates); bt_res[[paste(1)]][[paste(0.01)]] = cbind(bt_res[[paste(1)]][[paste(0.01)]], "fn" = NA, "bsn" = NA, "pn" = NA, "spcurr" = NA, "spfut" = NA) 
bt_res[[paste(2)]]=list()
	bt_res[[paste(2)]][[paste(0.005)]] = xts(order.by=run_dates); bt_res[[paste(2)]][[paste(0.005)]] = cbind(bt_res[[paste(2)]][[paste(0.005)]], "fn" = NA, "bsn" = NA, "pn" = NA, "spcurr" = NA, "spfut" = NA) 
	bt_res[[paste(2)]][[paste(0.008)]] = xts(order.by=run_dates); bt_res[[paste(2)]][[paste(0.008)]] = cbind(bt_res[[paste(2)]][[paste(0.008)]], "fn" = NA, "bsn" = NA, "pn" = NA, "spcurr" = NA, "spfut" = NA) 
	bt_res[[paste(2)]][[paste(0.01)]] = xts(order.by=run_dates); bt_res[[paste(2)]][[paste(0.01)]] = cbind(bt_res[[paste(2)]][[paste(0.01)]], "fn" = NA, "bsn" = NA, "pn" = NA, "spcurr" = NA, "spfut" = NA) 
bt_res[[paste(4)]]=list()
	bt_res[[paste(4)]][[paste(0.005)]] = xts(order.by=run_dates); bt_res[[paste(4)]][[paste(0.005)]] = cbind(bt_res[[paste(4)]][[paste(0.005)]], "fn" = NA, "bsn" = NA, "pn" = NA, "spcurr" = NA, "spfut" = NA) 
	bt_res[[paste(4)]][[paste(0.008)]] = xts(order.by=run_dates); bt_res[[paste(4)]][[paste(0.008)]] = cbind(bt_res[[paste(4)]][[paste(0.008)]], "fn" = NA, "bsn" = NA, "pn" = NA, "spcurr" = NA, "spfut" = NA) 
	bt_res[[paste(4)]][[paste(0.01)]] = xts(order.by=run_dates); bt_res[[paste(4)]][[paste(0.01)]] = cbind(bt_res[[paste(4)]][[paste(0.01)]], "fn" = NA, "bsn" = NA, "pn" = NA, "spcurr" = NA, "spfut" = NA) 
bt_res[[paste(8)]]=list()
	bt_res[[paste(8)]][[paste(0.005)]] = xts(order.by=run_dates); bt_res[[paste(8)]][[paste(0.005)]] = cbind(bt_res[[paste(8)]][[paste(0.005)]], "fn" = NA, "bsn" = NA, "pn" = NA, "spcurr" = NA, "spfut" = NA) 
	bt_res[[paste(8)]][[paste(0.008)]] = xts(order.by=run_dates); bt_res[[paste(8)]][[paste(0.008)]] = cbind(bt_res[[paste(8)]][[paste(0.008)]], "fn" = NA, "bsn" = NA, "pn" = NA, "spcurr" = NA, "spfut" = NA) 
	bt_res[[paste(8)]][[paste(0.01)]] = xts(order.by=run_dates); bt_res[[paste(8)]][[paste(0.01)]] = cbind(bt_res[[paste(8)]][[paste(0.01)]], "fn" = NA, "bsn" = NA, "pn" = NA, "spcurr" = NA, "spfut" = NA) 


pass_date(run_dates[1], 21, 1, TRUE, TRUE, fill_bt = TRUE)
for(date in run_dates[2:length(run_dates)]) pass_date(date, 21, 1, FALSE, FALSE, fill_bt = TRUE)

save(bt, file = 'bt_all.RData')











