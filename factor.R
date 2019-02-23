library(doMC) 
registerDoMC(20)
#install.packages(c('robust','leaps','lars','sn','strucchange'))
#install.packages("factorAnalytics", repos="http://R-Forge.R-project.org")
library(factorAnalytics)
#library(pcaMethods)
a = get(load('/mnt/G/RISK/R-OUTPUT/stock.data.1.RData'))

r = foreach(x=a,.combine=merge.xts)%dopar%{ 
		xr = x[[2]]$PX_LAST 
		xd = x[[2]]$date 
		nn = !is.na(xd) 
		if(TRUE %in% nn){ 
			y = xts(diff(log(xr[nn])), order.by=as.Date(xd[nn])[-1]) 
			colnames(y) = x[[1]] 
			y 
		} else NULL 
	}

foreach(i=1:dim(r)[2]) %do% if(NA%in%r[,i]) r[is.na(r[,i]),i]=0

r1 = r[(dim(r)[1]-60):dim(r)[1],]


ks = seq(5,90,by=10)
fits = foreach(k=ks)%dopar%fitStatisticalFactorModel(r1,k,na.rm=TRUE)
foreach(f=fits,.combine=c)%do%try({ mean(as.numeric(f$r2))})

f = pca(r1,method='ppca',nPcs=20)

f = fitStatisticalFactorModel(r1[,1:125],20,na.rm=TRUE)
f = fitStatisticalFactorModel(r1,20,na.rm=TRUE)
sort(as.numeric(f$r2))
mean(f$r2)






library(pcaMethods)

library(XML)
library(RColorBrewer)
library(plyr)
library(quantmod)
library(doMC) 
registerDoMC(20)
library(factorAnalytics)

#l <- readHTMLTable('http://en.wikipedia.org/wiki/List_of_S%26P_500_companies')[[1]]
#l <- as.vector(l$Ticker)
######l <- l[c(-59, -71, -80, -124, -141, -147, -275, -283, -292, -299, -309, -316, -360, -378, -381, -406, -439, -470, -471)]
#getSymbols(l, from='1990-01-01', auto.assign=TRUE)
#r_s = foreach(x=l,.combine=merge.xts)%do%diff(log(get(x)[,4]))  #   save(r_s,file='snp500_returns.RData')     
load('snp500_returns.RData')

foreach(i=1:dim(r_s)[2]) %do% if(NA%in%r_s[,i]) r_s[is.na(r_s[,i]),i]=0

r1 = r_s[(dim(r_s)[1]-60):dim(r_s)[1],]
r1 = r1[,c('F.Close','GM.Close','MSFT.Close')]

f = pca(r1,method='ppca',nPcs=10)
s = f@scores; ld = f@loadings; md = s%*%t(ld)
r22 = foreach(i=1:dim(r1)[2],.combine=c)%do%((sd(md[,i])/sd(r1[,i]))^2)
sum(foreach(i=1:dim(r1)[2],.combine=c)%do%(sd(md[,i])^2))/sum(foreach(i=1:dim(r1)[2],.combine=c)%do%(sd(r1[,i])^2))


f = fitStatisticalFactorModel(r1,20,na.rm=TRUE)
#sort(as.numeric(f$r2))
mean(f$r2)

s = f$factors; ld = f$loadings; md = s%*%ld
r22 = foreach(i=1:dim(r1)[2],.combine=c)%do%((sd(md[,i])/sd(r1[,i]))^2)
sum(foreach(i=1:dim(r1)[2],.combine=c)%do%(sd(md[,i])^2))/sum(foreach(i=1:dim(r1)[2],.combine=c)%do%(sd(r1[,i])^2))



library(tseries)
carz = get.hist.quote('CARZ'); vgt = get.hist.quote('VGT'); snp = get.hist.quote('^gspc')

res = matrix(0,ncol(r1)*nrow(r1),5); colnames(res) = c('date','name','return','fac1','fac2')
idx = 1; cn = colnames(r1); r_ind = index(r1)
carz = as.numeric(exp(diff(log(carz)))[index(r1),'Close'])-1; vgt = as.numeric(exp(diff(log(vgt)))[index(r1),'Close'])-1; snp = as.numeric(exp(diff(log(snp)))[index(r1),'Close'])-1
b_f = lm(r1[,1] ~ snp)$coef[2]; b_gm = lm(r1[,2] ~ snp)$coef[2]; b_m = lm(r1[,3] ~ snp)$coef[2]; 
tmp = foreach(i=1:ncol(r1))%do%{ foreach(j=1:nrow(r1))%do%{ 
	res[idx,]=c(r_ind[j],cn[i],r1[j,i],
		if(i==1)1 else if(i==2)1 else 2,
		if(i==1)2 else if(i==2)2 else 1); idx = idx+1; 
} }
 
res_df = base::data.frame(date=as.Date(as.numeric(res[,'date'])),name=res[,'name'],return=as.numeric(res[,'return']),fac1=as.numeric(res[,'fac1']), fac2=as.numeric(res[,'fac2']),stringsAsFactors=FALSE)
f = fitFundamentalFactorModel(data=res_df,exposure.names=c('fac1','fac2'),datevar='date',returnsvar='return',assetvar='name')

ffr = f$factor.returns
nam=names(r1)[180];  rr = res_df[res_df$name==nam,]; ex=ffr$fac1*rr$fac1+ffr$fac2*rr$fac2; sd(ex)/sd(as.numeric(rr$return))


res_df$fac1=carz[index(r1)]; res_df$fac2=vgt[index(r1)]






library(hash)
data(Stock.df)

# there are 447 assets
exposure.names <- c("BOOK2MARKET", "LOG.MARKETCAP") # exposure.names <- c("GICS.SECTOR")
exposure.names <- c("LOG.MARKETCAP") # exposure.names <- c("GICS.SECTOR")
test.fit <- function(st,en) fitFundamentalFactorModel(data=st,en,
                                       datevar = "DATE", returnsvar = "RETURN",
                                       assetvar = "TICKER", wls = TRUE,
                                       regression = "classic",
                                       covariance = "classic", full.resid.cov = TRUE,
                                       robust.scale = TRUE)
dates = rev(sort(unique(stock$DATE)))

fa1 = run_fa(stock[stock$TICKER%in%unique(stock$TICKER)[1:200] & stock$DATE%in%dates[-1],],c("BOOK2MARKET", "LOG.MARKETCAP", "STOCKHOLDERS.EQUITY", "NET.INCOME", 'VOLUME', 'SHARES.OUT', 'COMMON.EQUITY', 'LTDEBT'))

fa2 = run_fa(stock[stock$TICKER%in%unique(stock$TICKER)[1:200] & stock$DATE%in%dates,],c("BOOK2MARKET", "LOG.MARKETCAP", "STOCKHOLDERS.EQUITY", "NET.INCOME", 'VOLUME', 'SHARES.OUT', 'COMMON.EQUITY', 'LTDEBT'))

range(index(fa1$a))
range(index(fa2$a))
a1 = fa1$a[index(fa1$a)%in%index(fa2$a),]
a2 = fa2$a[index(fa2$a)%in%index(fa1$a),]

run_fa = function(st,en){
	data1 = st
	f=test.fit(data1,en)

	a = f$factor.returns[,-1]
	r0_l_s = foreach(n=names(a),.combine=cbind)%dopar%{ foreach(d=unique(data1$DATE),.combine=c)%do%{ r=data1[data1$DATE==d,'RETURN']; cap=data1[data1$DATE==d,n]; mean(r[cap>quantile(cap,0.5)]) - mean(r[cap<quantile(cap,0.5)]) } }
	r0_lc = foreach(n=names(a),.combine=cbind)%dopar%{ foreach(d=unique(data1$DATE),.combine=c)%do%{ r=data1[data1$DATE==d,'RETURN']; cap=data1[data1$DATE==d,n]; mean(r[cap>quantile(cap,0.75)]) } }
	r0 = foreach(n=names(a),.combine=cbind)%dopar%{ foreach(d=unique(data1$DATE),.combine=c)%do%{ r=data1[data1$DATE==d,'RETURN']; cap=data1[data1$DATE==d,n]; sum(cap*r) } }
	r0_q = foreach(n=names(a),.combine=cbind)%dopar%{ foreach(d=unique(data1$DATE),.combine=c)%do%{ r=data1[data1$DATE==d,'RETURN']; cap=data1[data1$DATE==d,n]; sum(ecdf(cap)(cap)*r) } }
	r0_qc = foreach(n=names(a),.combine=cbind)%dopar%{ foreach(d=unique(data1$DATE),.combine=c)%do%{ r=data1[data1$DATE==d,'RETURN']; cap=data1[data1$DATE==d,n]; sum((ecdf(cap)(cap)-0.5)*r) } }

	hash(a = a,
	     r0_l_s = r0_l_s,
	     r0_lc = r0_lc,
	     r0 = r0,
	     r0_q = r0_q,
	     r0_qc = r0_qc)
}


for(i in 1:dim(a)[2]) print(paste(names(a)[i],cor(r0[,i],a[,i])))
cor(a)/cor(r0)
cor(r0_lc,r0_q)


cor(r1 - r2*cov(r2,r1)/var(r2),a1) #plot(bhl,a)
cor(r1,a1)

r1a = r1 - r2*cov(r2,r1)/var(r2)
cor(r2 - r1*cov(r1,r2)/var(r1),a2) #plot(bhl,a)
cor(r2 - r1a*cov(r1a,r2)/var(r1a),a2) #plot(bhl,a)
cor(r2,a2) #plot(bhl,a)
cor(r1,r2)


orth = function(x){ tmp = x; for(i in 2:dim(tmp)[2]) tmp[,i] = tmp[,i] - foreach(j=1:(i-1),.combine='+')%do%(tmp[,j]*as.numeric(cov(tmp[,j],tmp[,i])/var(tmp[,j]))); tmp }

diag(cor(a,orth(r0)))




data=stock[stock$TICKER%in%unique(stock$TICKER)[1:6],]; exposure.names=c("BOOK2MARKET", "LOG.MARKETCAP"); datevar = "DATE"; returnsvar = "RETURN"; assetvar = "TICKER"; wls = TRUE; regression = "classic"; covariance = "classic"; full.resid.cov = TRUE; robust.scale = TRUE; standardized.factor.exposure = FALSE





function (data, exposure.names, datevar, returnsvar, assetvar, 
    wls = TRUE, regression = "classic", covariance = "classic", 
    full.resid.cov = FALSE, robust.scale = FALSE, standardized.factor.exposure = FALSE, 
    weight.var) 
{
    assets = unique(data[[assetvar]])
    timedates = as.Date(unique(data[[datevar]]))
    data[[datevar]] <- as.Date(data[[datevar]])
    if (length(timedates) < 2) 
        stop("At least two time points, t and t-1, are needed for fitting the factor model.")
    if (!is(exposure.names, "vector") || !is.character(exposure.names)) 
        stop("exposure argument invalid---must be character vector.")
    if (!is(assets, "vector") || !is.character(assets)) 
        stop("assets argument invalid---must be character vector.")
    wls <- as.logical(wls)
    full.resid.cov <- as.logical(full.resid.cov)
    robust.scale <- as.logical(robust.scale)
    standardized.factor.exposure <- as.logical(standardized.factor.exposure)
    if (!match(regression, c("robust", "classic"), FALSE)) 
        stop("regression must one of 'robust', 'classic'.")
    if (!match(covariance, c("robust", "classic"), FALSE)) 
        stop("covariance must one of 'robust', 'classic'.")
    #this.call <- match.call()
    if (match(returnsvar, exposure.names, FALSE)) 
        stop(paste(returnsvar, "cannot be used as an exposure."))
    numTimePoints <- length(timedates)
    numExposures <- length(exposure.names)
    numAssets <- length(assets)
    which.numeric <- sapply(data[, exposure.names, drop = FALSE], 
        is.numeric)
    exposures.numeric <- exposure.names[which.numeric]
    exposures.factor <- exposure.names[!which.numeric]
    if (length(exposures.factor) > 1) {
        stop("Only one nonnumeric variable can be used at this time.")
    }
    if (standardized.factor.exposure == TRUE) {
        if (is.na(weight.var)) {
            stop("Need to assign weight variable")
        }
        weight = by(data = data, INDICES = as.numeric(data[[datevar]]), 
            function(x) x[[weight.var]]/sum(x[[weight.var]]))
        data[[weight.var]] <- unlist(weight)
        for (i in exposures.numeric) {
            standardized.exposure <- by(data = data, INDICES = as.numeric(data[[datevar]]), 
                function(x) ((x[[i]] - mean(x[[weight.var]] * 
                  x[[i]])) * 1/sd(x[[weight.var]] * x[[i]])))
            data[[i]] <- unlist(standardized.exposure)
        }
    }
    regression.formula <- paste("~", paste(exposure.names, collapse = "+"))
    if (length(exposures.factor)) {
        regression.formula <- paste(regression.formula, "- 1")
        data[, exposures.factor] <- as.factor(data[, exposures.factor])
        exposuresToRecode <- names(data[, exposure.names, drop = FALSE])[!which.numeric]
        contrasts.list <- lapply(seq(length(exposuresToRecode)), 
            function(i) function(n, m) contr.treatment(n, contrasts = FALSE))
        names(contrasts.list) <- exposuresToRecode   }    else {
        contrasts.list <- NULL
    }
    regression.formula <- eval(parse(text = paste(returnsvar, 
        regression.formula)))
    ols.robust <- function(xdf, modelterms, conlist) {
        if (length(exposures.factor)) {
            zz <- xdf[[exposures.factor]]
            xdf[[exposures.factor]] <- if (is.ordered(zz)) 
                ordered(zz, levels = sort(unique.default(zz)))     else factor(zz)
        }
        model <- lmRob(modelterms, data = xdf, contrasts = conlist, 
            control = lmRob.control(mxr = 200, mxf = 200, mxs = 200))
        sdest <- sqrt(diag(model$cov))
        names(sdest) <- names(model$coef)
        coefnames <- names(model$coef)
        alphaord <- order(coefnames)
        model$coef <- model$coef[alphaord]
        sdest <- sdest[alphaord]
        c(length(model$coef), model$coef, model$coef/sdest, model$resid)
    }
    ols.classic <- function(xdf, modelterms, conlist) {
        model <- try(lm(formula = modelterms, data = xdf, contrasts = conlist, 
            singular.ok = FALSE))
        if (is(model, "Error")) {
            mess <- geterrmessage()
            nn <- regexpr("computed fit is singular", mess)
            if (nn > 0) {
                cat("At time:", substring(mess, nn), "\n")
                model <- lm(formula = modelterms, data = xdf, 
                  contrasts = conlist, singular.ok = TRUE)           }            else stop(mess)
        }
        tstat <- rep(NA, length(model$coef))
        tstat[!is.na(model$coef)] <- summary(model, cor = FALSE)$coef[, 
            3]
        alphaord <- order(names(model$coef))
        c(length(model$coef), model$coef[alphaord], tstat[alphaord], 
            model$resid)
    }
    wls.robust <- function(xdf, modelterms, conlist, w) {
        assign("w", w, pos = 1)
        if (length(exposures.factor)) {
            zz <- xdf[[exposures.factor]]
            xdf[[exposures.factor]] <- if (is.ordered(zz)) 
                ordered(zz, levels = sort(unique.default(zz)))           else factor(zz)
        }
        model <- lmRob(modelterms, data = xdf, weights = w, contrasts = conlist, 
            control = lmRob.control(mxr = 200, mxf = 200, mxs = 200))
        sdest <- sqrt(diag(model$cov))
        names(sdest) <- names(model$coef)
        coefnames <- names(model$coef)
        alphaord <- order(coefnames)
        model$coef <- model$coef[alphaord]
        sdest <- sdest[alphaord]
        c(length(model$coef), model$coef, model$coef/sdest, model$resid)
    }
    wls.classic <- function(xdf, modelterms, conlist, w) {
        assign("w", w, pos = 1)
        model <- try(lm(formula = modelterms, data = xdf, contrasts = conlist, 
            weights = w, singular.ok = FALSE))
        if (is(model, "Error")) {
            mess <- geterrmessage()
            nn <- regexpr("computed fit is singular", mess)
            if (nn > 0) {
                cat("At time:", substring(mess, nn), "\n")
                model <- lm(formula = modelterms, data = xdf, 
                  contrasts = conlist, weights = w)
            }
            else stop(mess)
        }
        tstat <- rep(NA, length(model$coef))
        tstat[!is.na(model$coef)] <- summary(model, cor = FALSE)$coef[, 
            3]
        alphaord <- order(names(model$coef))
        c(length(model$coef), model$coef[alphaord], tstat[alphaord], 
            model$resid)
    }
    if (!wls) {
        if (regression == "robust") {
            FE.hat <- by(data = data, INDICES = as.numeric(data[[datevar]]), 
                FUN = ols.robust, modelterms = regression.formula, 
                conlist = contrasts.list)
        }
        else {
            FE.hat <- by(data = data, INDICES = as.numeric(data[[datevar]]), 
                FUN = ols.classic, modelterms = regression.formula, 
                conlist = contrasts.list)
        }
    }    else {
        if (regression == "robust") {
            resids <- by(data = data, INDICES = as.numeric(data[[datevar]]), 
                FUN = function(xdf, modelterms, conlist) {
                  lmRob(modelterms, data = xdf, contrasts = conlist, 
                    control = lmRob.control(mxr = 200, mxf = 200, 
                      mxs = 200))$resid
                }, modelterms = regression.formula, conlist = contrasts.list)
            resids <- apply(resids, 1, unlist)
            weights <- if (covariance == "robust") 
                apply(resids, 1, scaleTau2)^2
            else apply(resids, 1, var)
            weights <- weights^-1
            FE.hat <- by(data = data, INDICES = as.numeric(data[[datevar]]), 
                FUN = wls.robust, modelterms = regression.formula, 
                conlist = contrasts.list, w = weights)
        }
        else {
            resids <- by(data = data, INDICES = as.numeric(data[[datevar]]), 
                FUN = function(xdf, modelterms, conlist) {
                  lm(formula = modelterms, data = xdf, contrasts = conlist, 
                    singular.ok = TRUE)$resid
                }, modelterms = regression.formula, conlist = contrasts.list)

            resids <- apply(resids, 1, unlist)


            resids0 <- by(data = data, INDICES = as.numeric(data[[datevar]]), 
                FUN = function(xdf, modelterms) {
                  lm(formula = modelterms, data = xdf)$resid
                }, modelterms = regression.formula)
	resids0_c = foreach(r=resids0)%do%r$coeff

            resids <- apply(resids0, 1, unlist)




            weights <- if (covariance == "robust") 
                apply(resids, 1, scaleTau2)^2           else apply(resids, 1, var)
            weights <- weights^-1
            FE.hat <- by(data = data, INDICES = as.numeric(data[[datevar]]), 
                FUN = wls.classic, modelterms = regression.formula, 
                conlist = contrasts.list, w = weights)

xdf=data1; modelterms = regression.formula; conlist = contrasts.list; 
fe_hat = FE.hat

function(xdf, modelterms, conlist, w) {
        assign("w", w, pos = 1)
        model <- try(lm(formula = modelterms, data = xdf, contrasts = conlist, 
            weights = w, singular.ok = FALSE))
        if (is(model, "Error")) {
            mess <- geterrmessage()
            nn <- regexpr("computed fit is singular", mess)
            if (nn > 0) {
                cat("At time:", substring(mess, nn), "\n")
                model <- lm(formula = modelterms, data = xdf, 
                  contrasts = conlist, weights = w)
            }
            else stop(mess)
        }
        tstat <- rep(NA, length(model$coef))
        tstat[!is.na(model$coef)] <- summary(model, cor = FALSE)$coef[, 
            3]
        alphaord <- order(names(model$coef))
        c(length(model$coef), model$coef[alphaord], tstat[alphaord], 
            model$resid)
    }




        }
    }
    if (length(exposures.factor) > 0) {
        numCoefs <- length(exposures.numeric) + length(levels(data[, 
            exposures.factor]))
        ncols <- 1 + 2 * numCoefs + numAssets
        fnames <- c(exposures.numeric, paste(exposures.factor, 
            levels(data[, exposures.factor]), sep = ""))
        cnames <- c("numCoefs", fnames, paste("t", fnames, sep = "."), 
            assets)
    }
    else {
        numCoefs <- 1 + length(exposures.numeric)
        ncols <- 1 + 2 * numCoefs + numAssets
        cnames <- c("numCoefs", "(Intercept)", exposures.numeric, 
            paste("t", c("(Intercept)", exposures.numeric), sep = "."), 
            assets)
    }
    FE.hat.mat <- matrix(NA, ncol = ncols, nrow = numTimePoints, 
        dimnames = list(as.character(timedates), cnames))
    for (i in 1:length(FE.hat)) {
        names(FE.hat[[i]])[1] <- "numCoefs"
        nc <- FE.hat[[i]][1]
        names(FE.hat[[i]])[(2 + nc):(1 + 2 * nc)] <- paste("t", 
            names(FE.hat[[i]])[2:(1 + nc)], sep = ".")
        if (length(FE.hat[[i]]) != (1 + 2 * nc + numAssets)) 
            stop(paste("bad count in row", i, "of FE.hat"))
        names(FE.hat[[i]])[(2 + 2 * nc):(1 + 2 * nc + numAssets)] <- assets
        idx <- match(names(FE.hat[[i]]), colnames(FE.hat.mat))
        FE.hat.mat[i, idx] <- FE.hat[[i]]
    }
    coefs.names <- colnames(FE.hat.mat)[2:(1 + numCoefs)]
    f.hat <- xts(x = FE.hat.mat[, 2:(1 + numCoefs)], order.by = timedates)
    gomat <- apply(coredata(f.hat), 2, function(x) abs(x - median(x, 
        na.rm = TRUE)) > 4 * mad(x, na.rm = TRUE))
    if (any(gomat, na.rm = TRUE)) {
        cat("\n\n*** Possible outliers found in the factor returns:\n\n")
        for (i in which(apply(gomat, 1, any, na.rm = TRUE))) print(f.hat[i, 
            gomat[i, ], drop = FALSE])
    }
    tstats <- xts(x = FE.hat.mat[, (2 + nc):(1 + 2 * nc)], order.by = timedates)
    resids <- xts(x = FE.hat.mat[, (2 + 2 * numCoefs):(1 + 2 * 
        numCoefs + numAssets)], order.by = timedates)
    if (covariance == "robust") {
        if (kappa(na.exclude(coredata(f.hat))) < 1e+10) {
            Cov.factors <- covRob(coredata(f.hat), estim = "pairwiseGK", 
                distance = FALSE, na.action = na.omit)
        }
        else {
            cat("Covariance matrix of factor returns is singular.\n")
            Cov.factors <- covRob(coredata(f.hat), distance = FALSE, 
                na.action = na.omit)
        }
        resid.vars <- apply(coredata(resids), 2, scaleTau2, na.rm = T)^2
        D.hat <- if (full.resid.cov) 
            covOGK(coredata(resids), sigmamu = scaleTau2, n.iter = 1)
        else diag(resid.vars)
    }
    else {
        Cov.factors <- covClassic(coredata(f.hat), distance = FALSE, 
            na.action = na.omit)
        resid.vars <- apply(coredata(resids), 2, var, na.rm = TRUE)
        D.hat <- if (full.resid.cov) {
            covClassic(coredata(resids), distance = FALSE, na.action = na.omit)
        }
        else {
            diag(resid.vars)
        }
    }
    B.final <- matrix(0, nrow = numAssets, ncol = numCoefs)
    colnames <- coefs.names
    B.final[, match("(Intercept)", colnames, 0)] <- 1
    numeric.columns <- match(exposures.numeric, colnames, 0)
    B.final[, numeric.columns] <- as.matrix(data[(data[[datevar]] == 
        timedates[numTimePoints]), exposures.numeric])
    rownames(B.final) = assets
    colnames(B.final) = colnames(f.hat)
    if (length(exposures.factor) > 0) {
        B.final[, grep(exposures.factor, x = colnames)][cbind(seq(numAssets), 
            (data[data[[datevar]] == timedates[numTimePoints], 
                exposures.factor]))] <- 1
    }
    cov.returns <- B.final %*% Cov.factors$cov %*% t(B.final) + 
        if (full.resid.cov) {
            D.hat$cov
        }
        else {
            D.hat
        }
    mean.cov.returns = tapply(data[[returnsvar]], data[[assetvar]], 
        mean)
    Corr.returns = cov2cor(cov.returns)
    Cov.returns <- list(cov = cov.returns, mean = mean.cov.returns, 
        eigenvalues = eigen(cov.returns, only.values = TRUE, 
            symmetric = TRUE)$values)
    if (!(length(exposures.factor) > 0)) {
        colnames(f.hat)[1] <- "Intercept"
    }
    if (covariance == "robust") {
        if (kappa(na.exclude(coredata(f.hat))) < 1e+10) {
            Corr.factors <- covRob(coredata(f.hat), estim = "pairwiseGK", 
                distance = FALSE, na.action = na.omit, corr = TRUE)
        }
        else {
            cat("Covariance matrix of factor returns is singular.\n")
            Corr.factors <- covRob(coredata(f.hat), distance = FALSE, 
                na.action = na.omit, corr = TRUE)
        }
        Corr.D <- if (full.resid.cov) {
            cov2cor(D.hat$cov)
        }
        else {
            NULL
        }
    }
    else {
        Corr.factors <- covClassic(coredata(f.hat), distance = FALSE, 
            na.action = na.omit, corr = TRUE)
        Corr.D <- if (full.resid.cov) {
            cov2cor(D.hat$cov)
        }
        else {
            NULL
        }
    }
    output <- list(returns.cov = Cov.returns, factor.cov = Cov.factors, 
        resids.cov = D.hat, returns.corr = Corr.returns, factor.corr = Corr.factors, 
        resids.corr = Corr.D, resid.variance = resid.vars, factor.returns = f.hat, 
        residuals = resids, tstats = tstats, call = this.call, 
        data = data, asset.names = assets, beta = B.final, datevar = datevar, 
        returnsvar = returnsvar, assetvar = assetvar, exposure.names = exposure.names)
    class(output) <- "FundamentalFactorModel"
    return(output)
}





library(PerformanceAnalytics)
#install.packages('xlsx')
library(doMC)
library(xlsx)
library(tseries)
#library(quantmod)
options(stringsAsFactors=FALSE)

#a=read.xlsx('/mnt/G/RISK/R-OUTPUT/ValueAct 13F.xlsx','ValueAct 13F') # save(a,file='/mnt/G/RISK/ANTON/ValueAct13F_xlsx.RData') 

#load('/mnt/G/RISK/ANTON/ValueAct13F_xlsx')
#n1 = which(grepl('Name',a[,1]))
#n2 = which(grepl('Total ',a[,1]))
#md = foreach(i=1:length(n1))%do%list(Date=as.Date(paste(1,a[n1[i]-1,1]),"%d %B %Y"),Data=as.data.frame(a[(n1[i]+1):(n2[i]-1),],stringsAsFactors=FALSE))

a=read.xlsx('/mnt/G/RISK/R-OUTPUT/ValueAct_allWeights.xlsx','Sheet1') # save(a,file='/mnt/G/RISK/ANTON/ValueAct13F_xlsx') 
#load('/mnt/G/RISK/ANTON/ValueAct13F_xlsx.RData')
a=rbind('March 2005',a)
n1 = which(grepl('Name',a[,1]))
md = foreach(i=2:length(n1))%do%list(Date=seq(as.Date(paste(1,a[n1[i-1]-1,1]),"%d %B %Y"),by='1 month',len=2)[2],Data=as.data.frame(a[(n1[i-1]+1):(n1[i]-2),],stringsAsFactors=FALSE))


stc = sort(unique(foreach(x=md,.combine=c)%do%x$Data[,3]))
stc_n = foreach(x=md,.combine=rbind)%do%x$Data[,c(1,3)]; 
stc_n = as.matrix(stc_n[!duplicated(stc_n[,1:2]),]); stc_n = stc_n[order(stc_n[,1]),]


b = read.csv('/mnt/G/RISK/R-OUTPUT/valueact_price_data.csv'); 
b1 = read.csv('/mnt/G/RISK/R-OUTPUT/valueact_price_data1.csv')
names(b)[2:10] = c("0376152D.US","0882814D.US","0991739D.US","2297161Q.US","2764557Q.LN","3286728Q.US","3414924Q.US","3449016Q.US", "4107485Q.US")
names(b) = gsub('.US',' US',gsub('.LN',' LN',names(b))); names(b1) = gsub('.US',' US',gsub('.LN',' LN',names(b1)))
names_b = names(b); names_b1 = names(b1)
stc_names = as.character(stc_n[,2]); stc_names[stc_names%in%c("3414924Q","0376152D")] = c("3414924Q US","0376152D US") 
stc_match = match(stc_names,c(names_b,names_b1)); d_dates = as.Date(b[,1],format='%m/%d/%Y'); d1_dates = as.Date(b1[,1],format='%Y-%m-%d')
rets0 = foreach(i=1:length(stc_match),.combine=merge.xts)%do%
	if(is.na(stc_match[i])) xts(array(NA,dim(b)[1]),order.by=d_dates) else
	if(stc_names[i]%in%names_b) xts(b[,stc_names[i]],order.by=d_dates) else 
		xts(b1[,stc_names[i]],order.by=d1_dates)
names(rets0) = stc_names
rets0[,'IT/B US'] = rets0[,'IT US']
#rets0[,'RTRSF US'] = rets0[,'TRINF US']


alloc_dates = index(merge.xts(rets0,xts(array(NA,length(md)),order.by=as.Date(foreach(m=md,.combine=c)%do%m$Date))[,c()]))  
allocs0 = foreach(s=stc_n[,1],.combine=cbind)%do%xts(array(NA,length(alloc_dates)),order.by=as.Date(alloc_dates)) 
colnames(allocs0)=1:dim(stc_n)[1] 
tmp = foreach(m=md)%do%{ a_date=m$Date; allocs0[a_date,]=0; allocs0[a_date,match(m$Data[,3],as.character(stc_n[,2]))] = as.numeric(m$Data[,2]) }
names(allocs0)=names(rets0)

# lag_n=calc_grid[l,1]; reweigh=calc_grid[l,2]
calc_rets_lag = function(lag_n,reweigh){
	# lag_n = -1
	allocs = allocs0
	rets = rets0

	idx=index(allocs) 
	if(lag_n>0) { 
		allocs = allocs[idx[1:(dim(allocs)[1]-lag_n)],] 
		allocs = xts(allocs,order.by=idx[(lag_n+1):length(idx)[1]])
	} else if(lag_n<0){ 
		allocs = allocs[idx[(-lag_n+1):dim(allocs)[1]],] 
		allocs = xts(allocs,order.by=idx[1:(length(idx)+lag_n)])
	}

	allocs = merge.xts(allocs,xts(array(0,dim(rets)[1]),order.by=index(rets))[,c()])
	rets = merge.xts(rets,xts(array(0,dim(allocs)[1]),order.by=index(allocs))[,c()])
	allocs = allocs[index(allocs)>=index(na.omit(allocs[,1]))[1]]
	rets = rets[index(rets)>=index(na.omit(allocs[,1]))[1]]

	for(i in 1:ncol(rets)){ midx = index(na.omit(rets[,i])); rets[index(rets)<=max(midx),i] = na.locf(rets[index(rets)<=max(midx),i]) }

	allocs = na.locf(allocs)
	rets = na.locf(rets)

	tmp = foreach(i=1:dim(allocs)[2])%do%{ allocs[is.na(allocs[,i]),i] = 0 }  # all preceeding NAs should be = 0

# Allocs for dates with no returns will = 0
	i_allocs = index(allocs)
	tmp = foreach(j=1:length(index(allocs)))%do%{ i=i_allocs[j]; print(i); allocs[i,is.na(rets[i,])]=0; allocs[i,]=allocs[i,]/sum(allocs[i,]) }

# (Optional) Allocs need to be prev-return adjusted   allocs[as.Date(as.Date('2013-09-25'):as.Date('2013-10-15')),c0]
	if(reweigh){
		tmp = foreach(j=2:length(i_allocs))%do%{ i0=i_allocs[j-1]; i1=i_allocs[j]; print(i1); a0=as.numeric(allocs[i0,]); a1=as.numeric(allocs[i1,]); ai0=a0>0; ai1=a1>0; a0=a0[ai0]; a1=a1[ai1]; r0=as.numeric(rets[i0,ai0]); r1=as.numeric(rets[i1,ai1]); if(min(ai0==ai1)==1) { allocs[i1,ai0]=a0*(r1/r0)/sum(a0*(r1/r0)) }}
	}

#  c0=match(c('ADBE.US','ALSN.US','CBG.US','CF.US','COL.US','KAR.US','MCRS.US','MOS.US','MSCI.US','MSFT.US','MSI.US','VLO.US','VRX.US','WSH.US'),names(rets))
# c0=match(c('X4107485Q.US','ACXM.US','ADS.US','DISCA.US','DRC.US','EFX.US','EYE.US','GVHR.US','HSH.US','IT.US','NDZ.US','TRI.US','ZZ.US'),names(rets))


	list(allocs=allocs, rets=rets)
}


source('/var/www/R/run/env_init.R')
prc_N = 24
registerDoMC(prc_N)





pmwAnton <<- odbcConnect(pmwName,uid=pmwAnton_uid,pwd=pmwAnton_pwd)
rets_fund = sqlQuery(pmwAnton,paste("select DATEADD(mm, DATEDIFF(m,0,[Date]),0) as [Date], [Return] from [SR-CRESTSQL].PerTrac.dbo.Performance where [ID]=94901",sep=""))
rets_fund = xts(rets_fund$Return, order.by=rets_fund$Date)

#charts.PerformanceSummary(rets_all, colorset=rainbow6equal, lwd=2, ylog=TRUE, width=12)
#mcps_wnd(na.omit(merge.xts(fund=rets_fund, port=tot_rets, port15=tot_rets_sh15, port30=tot_rets_sh30)), colorset=rainbow6equal, lwd=2, ylog=TRUE)

# x=a_r[[1]]; allocs_in=x$res$allocs; rets_in=x$res$rets
get_tot_rets = function(rets_in,allocs_in) {
	tmp_edl = exp(diff(log(rets_in))) 
	tmp_allocs = xts(allocs_in[1:(dim(allocs_in)[1]-1)],order.by=index(allocs_in)[2:dim(allocs_in)[1]])
	tmp_tot = xts(foreach(i=index(tmp_edl),.combine=c)%do%{ 
		tmp_r = as.numeric(tmp_edl[i,]); 
		ina = !is.na(tmp_r) #& tmp_r>0.5 & tmp_r<1.5
		a0=as.numeric(tmp_allocs[i,])  
		sum(as.numeric(tmp_edl[i,ina])*a0[ina])-1},order.by=index(tmp_edl))
	tmp_tot[!is.na(tmp_tot) & tmp_tot!=-1]
}

lagz = c(10,20,30,40,-1,-20,-40,-60); calc_grid = expand.grid(lagz,c(TRUE,FALSE))

a_r = foreach(l=1:nrow(calc_grid), .errorhandling='remove')%dopar%list(l=l,res=calc_rets_lag(calc_grid[l,1],calc_grid[l,2]))
a_tots = foreach(x=a_r)%dopar%get_tot_rets(x$res$rets,x$res$allocs)

a_all = list(a_r=a_r, a_tots=a_tots)
save(a_all,file='/mnt/G/RISK/ANTON/a_all.RData')
#load('/mnt/G/RISK/ANTON/a_all.RData'); a_r=a_all$a_r; a_tots=a_all$a_tots
# write.csv


for(i in 1:8){
	png(paste('/mnt/G/RISK/R-OUTPUT/lagReturns ',lagz[i],'.png',sep=''),750,650)
	rr = a_tots[[i]]
	dts = format.Date(index(rr),format='%Y-%m-01')
	r_m = xts(aggregate(rr,dts,function(x){ prod(1+x)-1 }),order.by=as.Date(unique(dts)))
	idx = index(r_m)[index(r_m)>=min(index(r_m)[r_m!=0])]
	index(rets_fund) = as.Date(index(rets_fund))
	tmp_tot = na.omit(merge.xts(Fund=rets_fund[idx], Lagged=r_m[idx]))
	mcps(na.omit(tmp_tot), lbls=c('Fund vs ',paste('Diff with lag',calc_grid[i,1])), colorset=rainbow6equal, lwd=2, ylog=TRUE)
	dev.off()
}


chart.RollingPerformance(a_tots[[1]],30)
mcps(na.omit(merge.xts(Fund=rets_fund, Lag_15=a_tots[[4]])), lbls=c('Fund','Diff with lag 15'), colorset=rainbow6equal, lwd=2, ylog=TRUE)

8,7,6,5,4,2

rr1 = a_tots[[8]]
rr2 = a_tots[[7]]
rr3 = a_tots[[6]]
rr4 = a_tots[[5]]
rr5 = a_tots[[4]]
rr6 = a_tots[[2]]
dts = format.Date(index(rr1),format='%Y-%m-01'); r_m1 = xts(aggregate(rr1,dts,function(x){ prod(1+x)-1 }),order.by=as.Date(unique(dts)))
dts = format.Date(index(rr2),format='%Y-%m-01'); r_m2 = xts(aggregate(rr2,dts,function(x){ prod(1+x)-1 }),order.by=as.Date(unique(dts)))
dts = format.Date(index(rr3),format='%Y-%m-01'); r_m3 = xts(aggregate(rr3,dts,function(x){ prod(1+x)-1 }),order.by=as.Date(unique(dts)))
dts = format.Date(index(rr4),format='%Y-%m-01'); r_m4 = xts(aggregate(rr4,dts,function(x){ prod(1+x)-1 }),order.by=as.Date(unique(dts)))
dts = format.Date(index(rr5),format='%Y-%m-01'); r_m5 = xts(aggregate(rr5,dts,function(x){ prod(1+x)-1 }),order.by=as.Date(unique(dts)))
dts = format.Date(index(rr6),format='%Y-%m-01'); r_m6 = xts(aggregate(rr6,dts,function(x){ prod(1+x)-1 }),order.by=as.Date(unique(dts)))
#r_m = xts(aggregate(r,dts,function(x){ sum(x) }),order.by=as.Date(unique(dts)))
idx = index(r_m1)[index(r_m1)>=min(index(r_m1)[r_m1!=0])]
index(rets_fund) = as.Date(index(rets_fund))
rc = na.omit(merge.xts(Fund=rets_fund[idx], Faster_60=r_m1[idx], Faster_40=r_m2[idx], Faster_20=r_m3[idx], Exact=r_m4[idx], Lag_20=r_m6[idx], Lag_40=r_m5[idx]))
names(rc)=c('Fund','Faster_60','Faster_40','Faster_20','Exact','Lag_20','Lag_40')
mcps(rc, lbls=c('Fund','Diff'), colorset=rainbow6equal, lwd=2, ylog=TRUE)
#save(rc,file='/mnt/G/RISK/ANTON/rc.RData')

oli = index(rc)[match(sort(as.numeric(abs(rc[,1]-rc[,2])),decreasing=TRUE)[1:5],abs(rc[,1]-rc[,2]))]

allocs = a_r[[4]]$res$allocs
rets = a_r[[4]]$res$rets




prod(1+c(0.0201911512,0.0542525067,-0.0151863404))


snp = as.xts(get.hist.quote('^gspc')$Close); rets00 = rets
snp = na.locf(merge.xts(snp,rets00[,c()])); rets00 = na.locf(merge.xts(rets00,snp[,c()]))
snp_log = na.omit(diff(log(snp))); rets_log = diff(log(rets00)); names(rets_log)=names(allocs)
#snp_log = snp_log[index(snp_log)%in%index(rets_log)]
rets_tab = foreach(n=names(rets_log))%dopar%{ 
	idx0_num = (-90:90); idx0_bool = array(TRUE,length(idx0_num))
	
	idx = which(allocs[,n]>0)[1] 
	if(!is.na(idx)){
		idx = idx0_num + idx
		idx0_bool = idx>0 & idx<=dim(rets)[1]; idx = index(allocs)[idx[idx0_bool]]

		#tmp_r = merge.xts(rets_log[idx,n],xts(array(NA,length(idx)),order.by=idx)[,c()])
		#tmp_r = na.locf()

		tmp_r = na.omit(rets_log[,n])
		idx1 = idx%in%index(snp_log) & idx%in%index(tmp_r); idx0_bool[idx0_bool] = idx1; idx = idx[idx1]
		betas0 = as.xts(foreach(j=idx,.combine=c)%do%{ 
			s0=snp_log[which(index(snp_log)==j)-60:0] 
			r0_idx = which(index(tmp_r)==j)-60:0
			if(length(r0_idx[r0_idx>0])>1){
				r0=tmp_r[r0_idx[r0_idx>0]] 
				r0=r0[!is.na(r0)]; s0=s0[!is.na(s0) & index(s0)%in%index(r0)]; r0=r0[index(r0)%in%index(s0)]
				as.numeric(cov(r0,s0)/var(s0))} else NA},order.by=idx)
		
		r0 = tmp_r[idx]-snp_log[idx]*betas0
		list(t=idx0_num[idx0_bool], dt=index(na.omit(r0)), d=exp(cumsum(na.omit(r0))))
	} else NA
}

len = length(rets_tab); rt1=rets_tab[!is.na(rets_tab)]
rets_tab = foreach(r=rt1,.combine=rbind)%do%{ res=array(NA,len); res[91+r$t]=r$d; res }
rets_tab_dts = foreach(r=rt1,.combine=rbind)%do%{ res=array(NA,len); res[91+r$t]=r$dt; res }
rownames(rets_tab) = names(rets0)
qs_tab = apply(crets_tab,2,function(x){ quantile(x[!is.na(x)],c(0.25,0.75))})
plot(-90:90,qs_tab[1,],t='l',lwd=2,col='blue',ylim=c(0.5,1.5)); lines(-90:90,qs_tab[2,],col='red')

save(rets_tab,file='/mnt/G/RISK/ANTON/rets_tab.RData')
save(qs_tab,file='/mnt/G/RISK/ANTON/qs_tab.RData')

load('/mnt/G/RISK/ANTON/rets_tab.RData')
#load('/mnt/G/RISK/ANTON/qs_tab.RData')
qs_tab = apply(rets_tab,2,function(x){ quantile(x[!is.na(x)],c(0.25,0.75))})
plot(-90:90,qs_tab[1,],t='l',lwd=2,col='blue',ylim=c(0.5,1.5),ylab='25% quantile vs. 75% quantile',main='Cumulative return for 180 days'); lines(-90:90,qs_tab[2,],col='red')



#	list(t=(-90:90)[ii], d=as.numeric(rets_log[idx[ii],n])) }
#rets_log = foreach(i=1:dim(rets)[2],.combine=merge.xts)%do%na.omit(diff(log(rets[,i])))
cs = foreach(i=-90:90)%dopar%{ 

	tmp_allocs = xts(allocs_in[1:(dim(allocs_in)[1]-1)],order.by=index(allocs_in)[2:dim(allocs_in)[1]])
	tmp_tot = xts(foreach(i=index(tmp_edl),.combine=c)%do%{ 
		tmp_r = as.numeric(tmp_edl[i,]); 
		ina = !is.na(tmp_r) #& tmp_r>0.5 & tmp_r<1.5
		a0=as.numeric(tmp_allocs[i,])  
		sum(as.numeric(tmp_edl[i,ina])*a0[ina])-1},order.by=index(tmp_edl))
	tmp_tot[!is.na(tmp_tot) & tmp_tot!=-1]



	print(i)
	x = foreach(j=1:length(rets_log),.combine=c)%do%rets_log[[j]][i+91]
	s0 = snp_log[index(snp_log)%in%index(x)]; x = x[index(x)%in%index(snp_log)]; idx = index(s0)
	betas0 = as.xts(foreach(j=idx,.combine=c)%do%cov(s0[idx<=j],x[idx<=j])/var(s0[idx<=j]),order.by=idx)
	exp(cumsum(na.omit(x-s0*betas0)))
}
cs_q = as.xts(foreach(i=1:dim(cs)[1],.combine=rbind)%do%quantile(as.numeric(cs[i,!is.na(cs[i,])]),c(0.25,0.5,0.75)),order.by=index(cs))

chart.TimeSeries(cs_q, colorset=rainbow6equal, ylim=c(0,3), legend.loc = "topleft", main='Individual quantiles')




dr = r_m[idx]-rets_fund[idx]
hist(dr)



mcps = function (R, lbls, Rf = 0, main = NULL, geometric = TRUE, methods = "none", 
    width = 0, event.labels = NULL, ylog = FALSE, wealth.index = FALSE, 
    gap = 12, begin = c("first", "axis"), legend.loc = "topleft", 
    p = 0.95, ...) 
{
    begin = begin[1]
    #x = checkData(R)
    x = R
    colnames = colnames(x)
    ncols = ncol(x)
    length.column.one = length(x[, 1])
    start.row = 1
    start.index = 0
    while (is.na(x[start.row, 1])) {
        start.row = start.row + 1
    }
    x = x[start.row:length.column.one, ]
    if (ncols > 1) 
        legend.loc = legend.loc
    else legend.loc = NULL
    if (is.null(main)) 
        main = paste(colnames[1], "Performance", sep = " ")
    if (ylog) 
        wealth.index = TRUE
    op <- par(no.readonly = TRUE)
    layout(matrix(c(1, 2, 3),3,3), heights = c(2, 1, 1.3), widths = c(1,1,1))
    #par(mfrow=c(3,1))
    par(mar = c(1, 4, 4, 2), cex=1.1)
    chart.CumReturns(x, xaxis = FALSE, legend.loc = legend.loc, 
        event.labels = event.labels, ylog = ylog, wealth.index = wealth.index, 
        begin = begin, geometric = geometric, ylab = "Cumulative Return", main=lbls[1],
        ...)
    par(mar = c(1, 4, 1, 2))
    freq = periodicity(x)
    switch(freq$scale, seconds = {
        date.label = "Second"
    }, minute = {
        date.label = "Minute"
    }, hourly = {
        date.label = "Hourly"
    }, daily = {
        date.label = "Daily"
    }, weekly = {
        date.label = "Weekly"
    }, monthly = {
        date.label = "Monthly"
    }, quarterly = {
        date.label = "Quarterly"
    }, yearly = {
        date.label = "Annual"
    })
    ts = rollapply(R[,2],12,function(x){ exp(sum(log(1+x)))-1 }) - rollapply(R[,1],12,function(x){ exp(sum(log(1+x)))-1 })
    #ts = merge.xts(ts,rollapply(R[,3],12,function(x){ exp(sum(log(1+x)))-1 }) - rollapply(R[,1],12,function(x){ exp(sum(log(1+x)))-1 }))
    #ts = merge.xts(ts,rollapply(R[,4],12,function(x){ exp(sum(log(1+x)))-1 }) - rollapply(R[,1],12,function(x){ exp(sum(log(1+x)))-1 }))
    chart.TimeSeries(ts,  xaxis = FALSE, width = width, 
        ylab = 'Annualized return', main=lbls[2], methods = methods, 
        event.labels = NULL, ylog = FALSE, gap = gap, 
        ...)
#    chart.BarVaR(rollapply(R[,2],12,sum), main = "", xaxis = FALSE, width = width, 
#        ylab = paste(date.label, "Return"), methods = methods, 
#        event.labels = NULL, ylog = FALSE, gap = gap, p = p, 
#        ...)
    par(mar = c(5, 4, 0, 2))
    chart.Drawdown(x, geometric = geometric, main = "", ylab = "Drawdown", 
        event.labels = NULL, ylog = FALSE, ...)
    par(op)
}

mcps(na.omit(merge.xts(Fund=rets_fund, Lag_15=tot_rets_sh15)), lbls=c('Fund','Diff with lag 15'), colorset=rainbow6equal, lwd=2, ylog=TRUE)











r = a_tots[[2]]
dts = format.Date(index(r),format='%Y-%m-01')
#r_m = xts(aggregate(r,dts,function(x){ prod(1+x)-1 }),order.by=as.Date(unique(dts)))
r_m = xts(aggregate(r,dts,function(x){ sum(x) }),order.by=as.Date(unique(dts)))
idx = index(r_m)[index(r_m)>=min(index(r_m)[r_m!=0])]
r0 = na.omit(merge.xts(Fund=rets_fund[idx], Lag=r_m[idx]))
d1 = abs(r0[,1]/r0[,2]); d2 = abs(r0[,2]/r0[,1]); r0[d1>8 | d2>8,]=0
mcps(r0, lbls=c('Fund','Diff with lag 15'), colorset=rainbow6equal, lwd=2, ylog=TRUE)





rets=a_r[[4]]$res$rets
allocs=a_r[[4]]$res$allocs
idxn=1:dim(stc_n)[1]; idx0=index(rets0)
idx0 = idx0[idx0%in%index(rets) & idx0%in%index(allocs) ]
rets_c = rets[idx0,idxn]; colnames(rets_c)=stc_n[idxn,2]
allocs_c = allocs[idx0,idxn]; colnames(allocs_c)=stc_n[idxn,2]
edl=exp(diff(log(rets_c))); 
tmp = foreach(i=idx0)%do%{ edl[i,as.numeric(allocs_c[i,])==0] = NA }

dr = foreach(i=1:dim(rets)[2],.combine=rbind)%do%{
	tmp_r = na.omit(edl[,i])
	min_idx = index(tmp_r)[which.min(tmp_r)]
	max_idx = index(tmp_r)[which.max(tmp_r)]
	c(names(rets0)[i],as.character(min_idx),allocs[min_idx,i]*100,(tmp_r[min_idx]-1)*100,as.character(max_idx),allocs[max_idx,i]*100,(tmp_r[max_idx]-1)*100)
}
rownames(dr)=c()
dr = dr[dr[,2]>0,]; dr = dr[order(as.numeric(apply(dr[,c(4,7)],1,function(x) max(abs(as.numeric(x)))))),] 
colnames(dr)=c('name','date','min alloc','max drop','date','max alloc','max rise')
write.csv(dr,file='/mnt/G/RISK/R-OUTPUT/max_stock_movements2.csv')


drd = dr[order(as.numeric(apply(dr[,c(4,7)],1,function(x) max(abs(as.numeric(x))))),decreasing=TRUE),]

for(k in 1:10){
	ticker = gsub(' US','.US',drd[k,1])
	print(ticker)
	print(as.numeric(0.01*floor(10000*allocs[as.Date((as.Date(drd[k,2])-4):(as.Date(drd[k,2])+4)),ticker])))
	print(as.numeric(rets[as.Date((as.Date(drd[k,2])-4):(as.Date(drd[k,2])+4)),ticker]))
	print(as.numeric(0.01*floor(10000*allocs[as.Date((as.Date(drd[k,5])-4):(as.Date(drd[k,5])+4)),ticker])))
	print(as.numeric(rets[as.Date((as.Date(drd[k,5])-4):(as.Date(drd[k,5])+4)),ticker]))
}


#drd[6,1] = paste('X',drd[6,1],sep='')
for(k in 1:10){
	ticker = gsub(' US','.US',drd[k,1])
	if(as.numeric(drd[k,3])>2){
		print(as.numeric(drd[k,3]))
		rd = exp(na.omit(diff(log(rets[,ticker]))))
		print(rd[index(rd)[order(as.numeric(rd),decreasing=FALSE)[1:5]]])
	}
}

for(k in 1:10){
	ticker = gsub(' US','.US',drd[k,1])
	if(as.numeric(drd[k,6])>2){
		print(as.numeric(drd[k,6]))
		rd = exp(na.omit(diff(log(rets[,ticker]))))
		print(rd[index(rd)[order(as.numeric(rd),decreasing=TRUE)[1:5]]])
	}
}




edl=exp(diff(log(rets))); allocs_act=xts(allocs[1:(dim(allocs)[1]-1)],order.by=index(allocs)[2:dim(allocs)[1]])
r = xts(foreach(i=index(edl),.combine=c)%do%{ 
	tmp_r = as.numeric(edl[i,]); 
	ina = !is.na(tmp_r) & tmp_r>0.5 & tmp_r<1.5
	a0=as.numeric(allocs_act[i,])  
	sum(as.numeric(edl[i,ina])*a0[ina])-1},order.by=index(edl))
r = r[!is.na(r) & r!=-1]  
prod(1+r)

# idx = index(allocs)[index(allocs)>=as.Date('2013-10-01') & index(allocs)<as.Date('2013-12-31')]
# r0 = r[index(r)>=as.Date('2013-09-30') & index(r)<as.Date('2013-12-30')]

# r1 = r1[!is.na(r1) & r1!=-1]

#r1 = r; 
# save(r,file='/mnt/G/RISK/ANTON/r.RData')  load('/mnt/G/RISK/ANTON/r.RData')
# save(r1,file='/mnt/G/RISK/ANTON/r1.RData')  load('/mnt/G/RISK/ANTON/r1.RData')
# allocs1 = allocs; save(allocs1,file='/mnt/G/RISK/ANTON/allocs1.RData');   load('/mnt/G/RISK/ANTON/allocs1.RData')
# edl1 = edl; save(edl1,file='/mnt/G/RISK/ANTON/edl1.RData');   load('/mnt/G/RISK/ANTON/edl1.RData')


r0 = r - r1
i = index(r0)[r0==range(r0)[1]]
range((allocs-allocs1)[i,]); 
a0 = as.numeric(allocs[i,]); e0=as.numeric(edl[i,]); a1 = as.numeric(allocs1[i,]); e1=as.numeric(edl1[i,]);





rr = r
dts = format.Date(index(rr),format='%Y-%m-01')
r_m0 = xts(aggregate(rr,dts,function(x){ prod(1+x)-1 }),order.by=as.Date(unique(dts)))

#write.csv(rets0[idx0,idxn],file='/mnt/G/RISK/R-OUTPUT/test_portfError_Rets.csv')
#write.csv(allocs[idx0,idxn],file='/mnt/G/RISK/R-OUTPUT/test_portfError_Allocs.csv')
MCGRAW-HILL COMPANIES INC	MHP		MHP US	'MHP US',
MDS INC	MDZ		MDZ US	'MDZ US',
SARA LEE CORP	SLE		0778724D US	'0778724D US',
THOMSON CORP	TOC		TOC US	'TOC US',




rr = get_tot_rets(rets_c,allocs_c)
dts = format.Date(index(rr),format='%Y-%m-01')
r_m1 = xts(aggregate(rr,dts,function(x){ prod(1+x)-1 }),order.by=as.Date(unique(dts)))


r12 = get_tot_rets(rets,allocs)[idx0]

r_m0-r_m1
r12-r
rets_fund[idx0]


mcps_wnd = function (R, Rf = 0, main = NULL, geometric = TRUE, methods = "none", 
    width = 0, event.labels = NULL, ylog = FALSE, wealth.index = FALSE, 
    gap = 12, begin = c("first", "axis"), legend.loc = "topleft", 
    p = 0.95, ...) 
{
    begin = begin[1]
    #x = checkData(R)
    x = R
    colnames = colnames(x)
    ncols = ncol(x)
    length.column.one = length(x[, 1])
    start.row = 1
    start.index = 0
    while (is.na(x[start.row, 1])) {
        start.row = start.row + 1
    }
    x = x[start.row:length.column.one, ]
    if (ncols > 1) 
        legend.loc = legend.loc
    else legend.loc = NULL
    if (is.null(main)) 
        main = paste(colnames[1], "Performance", sep = " ")
    if (ylog) 
        wealth.index = TRUE
    op <- par(no.readonly = TRUE)
    #layout(matrix(c(1, 2, 3),3,3), heights = c(2, 1, 1.3), widths = c(1,1,1))
    par(mfrow=c(3,3))
    par(mar = c(1, 4, 4, 2))
    chart.CumReturns(R[,c(1,2)], main = main, xaxis = FALSE, legend.loc = legend.loc, 
        event.labels = event.labels, ylog = ylog, wealth.index = wealth.index, 
        begin = begin, geometric = geometric, ylab = "Cumulative Return", 
        ...)
    par(mar = c(1, 4, 4, 2))
    chart.CumReturns(R[,c(1,3)], main = main, xaxis = FALSE, legend.loc = legend.loc, 
        event.labels = event.labels, ylog = ylog, wealth.index = wealth.index, 
        begin = begin, geometric = geometric, ylab = "Cumulative Return", 
        ...)
    par(mar = c(1, 4, 4, 2))
    chart.CumReturns(R[,c(1,4)], main = main, xaxis = FALSE, legend.loc = legend.loc, 
        event.labels = event.labels, ylog = ylog, wealth.index = wealth.index, 
        begin = begin, geometric = geometric, ylab = "Cumulative Return", 
        ...)
    par(mar = c(1, 4, 1, 2))
    freq = periodicity(x)
    switch(freq$scale, seconds = {
        date.label = "Second"
    }, minute = {
        date.label = "Minute"
    }, hourly = {
        date.label = "Hourly"
    }, daily = {
        date.label = "Daily"
    }, weekly = {
        date.label = "Weekly"
    }, monthly = {
        date.label = "Monthly"
    }, quarterly = {
        date.label = "Quarterly"
    }, yearly = {
        date.label = "Annual"
    })
    ts = rollapply(R[,2],12,function(x){ exp(sum(log(1+x)))-1 }) - rollapply(R[,1],12,function(x){ exp(sum(log(1+x)))-1 })
    ts = merge.xts(ts,rollapply(R[,3],12,function(x){ exp(sum(log(1+x)))-1 }) - rollapply(R[,1],12,function(x){ exp(sum(log(1+x)))-1 }))
    ts = merge.xts(ts,rollapply(R[,4],12,function(x){ exp(sum(log(1+x)))-1 }) - rollapply(R[,1],12,function(x){ exp(sum(log(1+x)))-1 }))
    chart.TimeSeries(ts[,1],  xaxis = FALSE, width = width, 
        ylab = paste(date.label, "Return"), methods = methods, 
        event.labels = NULL, ylog = FALSE, gap = gap, 
        ...)
    par(mar = c(1, 4, 1, 2))
    chart.TimeSeries(ts[,2],  xaxis = FALSE, width = width, 
        ylab = paste(date.label, "Return"), methods = methods, 
        event.labels = NULL, ylog = FALSE, gap = gap, 
        ...)
    par(mar = c(1, 4, 1, 2))
    chart.TimeSeries(ts[,3],  xaxis = FALSE, width = width, 
        ylab = paste(date.label, "Return"), methods = methods, 
        event.labels = NULL, ylog = FALSE, gap = gap, 
        ...)
#    chart.BarVaR(rollapply(R[,2],12,sum), main = "", xaxis = FALSE, width = width, 
#        ylab = paste(date.label, "Return"), methods = methods, 
#        event.labels = NULL, ylog = FALSE, gap = gap, p = p, 
#        ...)
    par(mar = c(5, 4, 0, 2))
    chart.Drawdown(R[,c(1,2)], geometric = geometric, main = "", ylab = "Drawdown", 
        event.labels = NULL, ylog = FALSE, ...)
    par(mar = c(5, 4, 0, 2))
    chart.Drawdown(R[,c(1,3)], geometric = geometric, main = "", ylab = "Drawdown", 
        event.labels = NULL, ylog = FALSE, ...)
    par(mar = c(5, 4, 0, 2))
    chart.Drawdown(R[,c(1,4)], geometric = geometric, main = "", ylab = "Drawdown", 
        event.labels = NULL, ylog = FALSE, ...)
    par(op)
}


chart.RollingPerformance

msrm = sqlQuery(pmwAnton,'select * from Crestline.MySQL_RM')
nrm = names(msrm)
write.csv(paste(nrm[grep('BETA_',nrm)],collapse="','"),file='/mnt/G/RISK/ANTON/BETA_nrm.txt')
write.csv(paste(nrm[grep('STRESS_',nrm)],collapse="','"),file='/mnt/G/RISK/ANTON/STRESS_nrm.txt')



