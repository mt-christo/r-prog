source("R4.R"); source("R5.R"); source("R6.R");
library(hash)
#install.packages("segmented")
library(segmented)

calc_fd = function(arr, fd_num){
	fd = array(0,fd_num); 
	len = length(arr)
	y_step = 0
	y_range = diff(range(arr))
	for(i in 1:fd_num){
		s = ceiling(seq(1,len,l=i+1))
		y_step = y_range/i
		for(j in 1:i) 
			fd[i] = fd[i] + ceiling(diff(range(arr[s[j]:s[j+1]]))/y_step)
	}
	fd
}

detrendize = function(x,y){
	y-lm(y~x)$fitted.values
}

dot_fd = function(ar,fdns){
	zar = ar-min(ar); dr = max(zar)*1.001; res = array(0,length(fdns))
	for(i in 1:length(fdns)){
		res[i] = dim(aggregate(zar,list(floor(zar*fdns[i]/dr)),FUN=length))[1]
		if(i%%100==0) print(fdns[i])
	}
	hash(keys=c('x','y'),values=list(1/fdns,res))
}


logseq = function(x,y,len){
	exp(seq(log(x),log(y),l=len))
}

c=read.csv('../Ticks/AA.txt'); 

ts=c$Close[10000:150000]

library(doMC); registerDoMC(8); 




st = 0.05*1:100000
ts=sin(st)+sin(5*st)+sin(10*st)+sin(15*st)+sin(20*st)
ts=cumsum(sign(runif(2^11)-0.5))
f=Im(fft(ts))
c=read.csv('../Ticks/AA.txt')
k2=1:(2^11)
k3=10000+k2

f=Im(fft(c$Close[k3]))
x=1:1000; y=abs(f+rev(f))[1:round(length(f)*0.5)][x]
eq = equalify_x(log(x),log(y),50)
plot(eq[[1]],eq[[2]])
lines(lm(eq[[2]]~eq[[1]]))
plot(abs(a+rev(f))[1:round(length(f)*0.5)])

fun=plot
regs1=c()
len=2^11
for(i in 1:400){
	ts=cumsum(sign(runif(len)-0.5)); f=Im(fft(ts)); x=(len*0.1):(len*0.4); y=abs(f+rev(f))[x]; eq = equalify_x(log(x),log(y),3); 
	#fun(eq[[1]],eq[[2]],t='l',xlim=c(4,8),ylim=c(3,6),col='yellow'); abline(c(i,-1))
	#fun=lines
	regs1=c(regs1,lm(eq[[1]]~eq[[2]])$coeff[2])
}

fun=plot
len=2^10; 
rnb=rainbow(5)
regs2=c()
for(i in 1:5){
	ts=log(c$Close[(i*len):((i+1)*len)]); f=Im(fft(ts)); x=(len*0.01):(len*0.3); y=abs(f)[x]; #m=maxify_x(y,20); 
	#eq = equalify_x(log(m[[1]]),log(m[[2]]),5) 
eq = equalify_x(log(x),log(y),100)
	fun(eq[[1]],eq[[2]],t='l'
,xlim=c(1,9)
,ylim=c(-4,4)
,col=rnb[i]); abline(c(i,-1))
	fun=lines
	regs2=c(regs2,lm(eq[[2]]~eq[[1]])$coeff[2])
}
regs2
plot(regs1,regs2,xlim=c(-3,-0.5),ylim=c(-3,-0.5),)
rect(-2,-2,-1,-1)
rect(-1.3,-1.3,-0.9,-0.9)




i=1; len=2^12; ts=log(c$Close[(i*len):((i+1)*len)]); f=Im(fft(ts)); x=(len*0.01):(len*0.3); y=abs(f)[x]; ts2=cumsum(sign(runif(len)-0.5)); f2=Im(fft(ts2)); x2=(len*0.001):(len*0.5); y2=abs(f2)[x2]; 
#m=maxify_x(y,100); 
#plot(x,y)
#plot(log(x),log(y))
eq = equalify_x(log(x),log(y),30); eq2 = equalify_x(log(x2),log(y2),30); plot(eq[[1]],eq[[2]],xlim=c(min(c(eq[[1]],eq2[[1]])),max(c(eq[[1]],eq2[[1]]))),ylim=c(min(c(eq[[2]],eq2[[2]])),max(c(eq[[2]],eq2[[2]]))),lwd=2,col='blue',t='l'); lines(eq2[[1]],eq2[[2]],lwd=2,col='red'); abline(lm(eq[[2]]~eq[[1]])); abline(lm(eq2[[2]]~eq2[[1]]))


plot(x[m[[1]]],m[[2]])#,xlim=c(2,8),ylim=c(-4,4))
plot(log(x[m[[1]]]),log(m[[2]]),xlim=c(2,8),ylim=c(-4,4))

 eq = equalify_x(log(x[m[[1]]]),log(m[[2]]),5); plot(eq[[1]],eq[[2]],xlim=c(2,8),ylim=c(-4,4))






	for(i in 750:850){
		s = ceiling(seq(1,len,l=i+1))
		y_step = y_range/i
		for(j in 1:i) 
			fd[i] = fd[i] + ceiling(diff(range(arr[s[j]:s[j+1]]))/y_step)
		print(i)
		print(fd[i])
	}


len=100; exp_num=100; end_t=10; fdn=end_t; regs1=c(); regs2=c(); func=plot; t=1:len; for(i in 1:exp_num) {ts2=log(c$Close[(1:len)+i*len]); ts1=cumsum(runif(len)-0.5); fd2=calc_fd(ts2-lm(ts2~t)$fitted.values,fdn); fd1=calc_fd(ts1-lm(ts1~t)$fitted.values,fdn); x=log(1:end_t); y2=log(fd2[1:end_t]); y1=log(fd1[1:end_t]); regs1=c(regs1,lm(y1~x)$coef[2]); regs2=c(regs2,lm(y2~x)$coef[2]); func(x-x[1],y1-y1[1],xlim=c(0,4),ylim=c(-3,0),t='l',col='red'); func=lines; func(x-x[1],y2-y2[1],col='blue')}

plot(regs1,regs2,xlim=c(-0.7,-0.3),ylim=c(-0.7,-0.3))
rect(-0.6,-0.6,-0.4,-0.4)
plot(log(1:fdn),log(fd),t='l'); abline(v=log(end_t)); abline(lm(log(fd[1:end_t])~log(1:end_t))) #plot(fd)


len=4000; fdn=20; 
fdres=calc_fd(1:len,fdn); x=log(1/(1:fdn)); y=log(fdres); eq=equalify_x(x,y,100); x=eq[[1]]; y=eq[[2]]; plot(x,y); abline(lm(y~x)); print(lm(y~x)$coef[2])

plot(x,y); abline(lm(y~x)); print(lm(y~x)$coef[2])

regs0=list()
for(len in 1000*(1:5)) for(fdn in 10*(1:4)) {regs=c(); ts=diff(cumsum(runif(len)-0.5)); for(ii in 1:6){fdres=calc_fd(ts,fdn); x=log(1/(1:fdn)); y=log(fdres); regs=c(regs,lm(y~x)$coef[2]); print(paste(len,fdn,ii))}; regs0=c(regs0,list(as.numeric(regs)))}

regs1=list(); start_i=1; 
for(len in 1000*(1:5)) for(fdn in 10*(1:4)) {regs=c(); ts=diff(log(c$Close[start_i:(start_i+len)])); start_i=start_i+len; for(ii in 1:6){fdres=calc_fd(,fdn); x=log(1/(1:fdn)); y=log(fdres); regs=c(regs,lm(y~x)$coef[2]); print(paste(len,fdn,ii,start_i))}; regs1=c(regs1,list(as.numeric(regs)))}

sds1=unlist(lapply(regs1,FUN=sd)); means1=unlist(lapply(regs1,FUN=mean)); sds0=unlist(lapply(regs0,FUN=sd)); means0=unlist(lapply(regs0,FUN=mean)); plot(-means1,ylim=c(0,2),lwd=2); lines(sds1,lwd=2); lines(-means0); lines(sds0)



regs1=list(); start_i=1; lns=2000*(1:5); lns=array(8000,500); exps=10; fdns=30*(1:4); res_m=matrix(-57,length(lns),length(fdns)) 
for(i in 1:length(lns)){
	for(j in 1:length(fdns)){
		#ts=diff(log(c$Close[start_i:(start_i+lns[i])])); 
		#ts=detrendize(0:lns[i],log(c$Close[start_i:(start_i+lns[i])]));
		#ts=detrendize(0:lns[i],ts1[start_i:(start_i+lns[i])]);  
		ts=diff(ts1[start_i:(start_i+lns[i])]);  
		#ts=detrendize(1:lns[i],cumsum(sign(runif(lns[i])-0.5))); 
		#ts=cumsum(sign(runif(lns[i])-0.5))
		fdres=calc_fd(ts,fdns[j]) 
		x=log(1/(1:fdns[j])); y=log(fdres); #eq=equalify_x(x,y,20); x=eq[[1]]; y=eq[[2]];
		res_m[i,j]=lm(y~x)$coef[2]; 
		print(paste(lns[i],fdns[j],start_i))
	} 
	start_i=start_i+lns[i]
} 
regs1=res_m; sds1=unlist(lapply(1:dim(regs1)[1],FUN=function(x){sd(regs1[x,])})); means1=unlist(lapply(1:dim(regs1)[1],FUN=function(x){mean(regs1[x,])})); x=lns[1]*(1:length(lns)); y=-means1; plot(x,y,ylim=c(0,2),lwd=2,col='blue'); abline(h=max(sds1),col='blue'); abline(h=0);







ts1=tss[[1]][20000:length(tss[[1]])]
ts2=tss[[2]][20000:length(tss[[2]])]

regs1=list(); start_i=1; lns=2000*(1:5); lns=array(8000,250); exps=10; fdns=30*(1:4); res_m=matrix(-57,length(lns),length(fdns)) 
for(i in 1:length(lns)){
	for(j in 1:length(fdns)){
		#ts=diff(log(c$Close[start_i:(start_i+lns[i])])); 
		#ts=detrendize(0:lns[i],log(c$Close[start_i:(start_i+lns[i])]));
		#ts=detrendize(0:lns[i],ts1[start_i:(start_i+lns[i])]);  
		ts=diff(ts1[start_i:(start_i+lns[i])]);  
		#ts=detrendize(1:lns[i],cumsum(sign(runif(lns[i])-0.5))); 
		#ts=cumsum(sign(runif(lns[i])-0.5))
		fdres=calc_fd(ts,fdns[j]) 
		x=log(1/(1:fdns[j])); y=log(fdres); #eq=equalify_x(x,y,20); x=eq[[1]]; y=eq[[2]];
		res_m[i,j]=lm(y~x)$coef[2]; 
		print(paste(lns[i],fdns[j],start_i))
	} 
	start_i=start_i+lns[i]
} 
regs1=res_m; sds1=unlist(lapply(1:dim(regs1)[1],FUN=function(x){sd(regs1[x,])})); means1=unlist(lapply(1:dim(regs1)[1],FUN=function(x){mean(regs1[x,])})); 


regs1=list(); start_i=1; lns=2000*(1:5); lns=array(8000,250); exps=10; fdns=30*(1:4); res_m=matrix(-57,length(lns),length(fdns)) 
for(i in 1:length(lns)){
	for(j in 1:length(fdns)){
		#ts=diff(log(c$Close[start_i:(start_i+lns[i])])); 
		#ts=detrendize(0:lns[i],log(c$Close[start_i:(start_i+lns[i])]));
		#ts=detrendize(0:lns[i],ts1[start_i:(start_i+lns[i])]);  
		ts=diff(ts2[start_i:(start_i+lns[i])]);  
		#ts=detrendize(1:lns[i],cumsum(sign(runif(lns[i])-0.5))); 
		#ts=cumsum(sign(runif(lns[i])-0.5))
		fdres=calc_fd(ts,fdns[j]) 
		x=log(1/(1:fdns[j])); y=log(fdres); #eq=equalify_x(x,y,20); x=eq[[1]]; y=eq[[2]];
		res_m[i,j]=lm(y~x)$coef[2]; 
		print(paste(lns[i],fdns[j],start_i))
	} 
	start_i=start_i+lns[i]
} 
regs1=res_m; sds2=unlist(lapply(1:dim(regs1)[1],FUN=function(x){sd(regs1[x,])})); means2=unlist(lapply(1:dim(regs1)[1],FUN=function(x){mean(regs1[x,])})); 
x=lns[1]*(1:length(lns)); 
y1=-means1; y2=-means2; 

plot(x,y1,ylim=c(0,2),lwd=2,col='blue',t='l'); abline(h=max(sds1),col='blue');
lines(x,y2,lwd=2,col='darkgreen',t='l'); abline(h=max(sds2),col='darkgreen'); abline(h=0);

plot(hist(means1,plot=FALSE,br=10),xlim=c(-2,-1.5),ylim=c(0,300), col=rgb(0,0,1,1/4))  # first histogram
plot(hist(means2,plot=FALSE,br=15), col=rgb(1,0,0,1/4), add=T)  # second




regs2=list(); start_i=1; lns=2000*(1:5); lns=array(2000,400); exps=10; fdns=25*(1:4); res_m=matrix(-57,length(lns),length(fdns)) 
for(i in 1:length(lns)){
	for(j in 1:length(fdns)){
		ts=diff(log(c$Close[start_i:(start_i+lns[i])])); fdres=calc_fd(ts,fdns[j]) 
		x=log(1/(1:fdns[j])); y=log(fdres); #eq=equalify_x(x,y,20); x=eq[[1]]; y=eq[[2]];
		res_m[i,j]=lm(y~x)$coef[2]; 
		print(paste(lns[i],fdns[j],start_i))
	} 
	start_i=start_i+lns[i]
} 
regs1=res_m; sds2=unlist(lapply(1:dim(regs1)[1],FUN=function(x){sd(regs1[x,])})); means1=unlist(lapply(1:dim(regs1)[1],FUN=function(x){mean(regs1[x,])})); x2=lns[1]*(1:length(lns)); y2=-means1; lines(x2,y2,ylim=c(0,2),lwd=2,col='darkgreen'); abline(h=max(sds2),col='darkgreen'); abline(h=0); #lines(sds1,lwd=1,col='blue'); 




len=4000; plot_func=plot; cols=rainbow(100); for(j in 1:100){
ts = log(c$Close[(j*len):(len+j*len)])
fdn=20; f1=calc_fd(ts,fdn); x=log(1:fdn); y=log(f1); plot_func(x,y,t='b',col=cols[j]); plot_func=lines;
sects=seq(min(x),max(x),l=3); regs=c(); 
for(i in 2:length(sects)){
	ix = which(x>=sects[i-1] & x<=sects[i])
	if(length(ix)>0){
		tx=x[ix]; ty=y[ix]; tlm=lm(ty~tx); pr=predict(tlm,data.frame(x=tx))
		regs=c(regs,tlm$coef[2])
		segments(tx[1],pr[1],tx[length(tx)],pr[length(pr)],col=cols[j])
	}
}
}

plot(regs,t='l')







wnds=seq(1,40000,by=4000); plot_func=plot
for(i in 2:length(wnds)){
	ts = log(c$Close[wnds[i-1]:wnds[i]])
	fdn=50; f1=calc_fd(ts,fdn); x=log(1:fdn); y=log(f1); #plot(x,y)
	sects=seq(min(x),max(x),l=4); regs=c(); 
	for(i in 2:length(sects)){
		ix = which(x>=sects[i-1] & x<=sects[i])
		if(length(ix)>0){
			tx=x[ix]; ty=y[ix]; tlm=lm(ty~tx); #pr=predict(tlm,data.frame(x=tx))
			regs=c(regs,tlm$coef[2])
			#segments(tx[1],pr[1],tx[length(tx)],pr[length(pr)])
		}
	}
	
	plot_func(regs,t='l',ylim=c(1,2))
	plot_func=lines
}








start_idx=10000; start_idxs=seq(10000,50000,by=10000); cols=rainbow(length(start_idxs)); wnds=seq(4000,500,by=-100); regs2=c(); fds=c(); sds=c(); plot_func=plot; 
for(k in 1:length(start_idxs)){ 
	start_idx = start_idxs[k]
	regs2=c(); 
	for(j in 2:length(wnds)){
		ts = diff(log(c$Close[(start_idx-wnds[j]):start_idx]))
		fdn=20; f1=calc_fd(ts,fdn); x=log(1:fdn); y=log(f1); #plot(x,y)
		sects=seq(min(x),max(x),l=4); regs=c(); 
		for(i in 2:length(sects)){
			ix = which(x>=sects[i-1] & x<=sects[i])
			if(length(ix)>0){
				tx=x[ix]; ty=y[ix]; tlm=lm(ty~tx); #pr=predict(tlm,data.frame(x=tx))
				regs=c(regs,tlm$coef[2])
				#segments(tx[1],pr[1],tx[length(tx)],pr[length(pr)])
			}
		}
	
		#plot_func(regs,t='l',ylim=c(1,2))
		#plot_func=lines
		regs2=c(regs2,regs[1])
		#regs3=c(regs3,regs[3])
		#regs4=c(regs4,regs[4])
		#print(wnds[j])
	}
	plot_func(-wnds[2:length(wnds)],regs2,ylim=c(1,2),t='l',col=cols[k]); 
	plot_func=lines
	#fds=c(fds,mean(regs2))
	#sds=c(sds,sd(regs2))
	#print(start_idx)
}
plot(fds,ylim=c(0,2),t='b')
lines(sds)
abline(h=0)

start_idx=10000; start_idxs=seq(10000,10000,by=10000); cols=rainbow(length(start_idxs)); wnds=seq(4000,500,by=-100); regs2=c(); fds=c(); sds=c(); plot_func=plot; 
for(k in 1:length(start_idxs)){ 
	start_idx = start_idxs[k]
	regs2=c(); 
	for(j in 2:length(wnds)){
		ts = diff(log(c$Close[(start_idx-wnds[j]):start_idx]))
		fdn=20; f1=calc_fd(ts,fdn); x=(1:fdn); y=(f1); x=log(1:fdn); y=log(f1); 
		plot_func(x,y); plot_func=lines
	}
	#plot_func(-wnds[2:length(wnds)],regs2,ylim=c(1,2),t='l',col=cols[k]); 
	plot_func=lines
	#fds=c(fds,mean(regs2))
	#sds=c(sds,sd(regs2))
	#print(start_idx)
}
plot(fds,ylim=c(0,2),t='b')
lines(sds)
abline(h=0)







lines(-wnds[2:length(wnds)],regs3,col='darkgreen'); lines(-wnds[2:length(wnds)],regs4,col='blue')
plot(-wnds[2:length(wnds)],0.5*(regs2+regs3),ylim=c(1.3,1.8),t='l');
plot(-wnds[2:length(wnds)],regs3,ylim=c(1.3,1.8),t='l'); 








start_idx=20000;  plot_func=plot; fds=c(); sds=c()
for(start_idx in seq(10000,100000,by=4000)){  
	wnds=seq(4000,500,by=-500); plot_func=plot; regs2=c(); regs3=c(); regs4=c()
	for(j in 2:length(wnds)){
		ts = log(c$Close[(start_idx-wnds[j]):start_idx])
		fdn=20; f1=calc_fd(ts,fdn); x=log(1:fdn); y=log(f1); #plot(x,y)
		sects=seq(min(x),max(x),l=3); regs=c(); 
		for(i in 2:length(sects)){
			ix = which(x>=sects[i-1] & x<=sects[i])
			if(length(ix)>0){
				tx=x[ix]; ty=y[ix]; tlm=lm(ty~tx); #pr=predict(tlm,data.frame(x=tx))
				regs=c(regs,tlm$coef[2])
				#segments(tx[1],pr[1],tx[length(tx)],pr[length(pr)])
			}
		}
	
		#plot_func(regs,t='l',ylim=c(1,2))
		#plot_func=lines
		regs2=c(regs2,regs[1])
		#regs3=c(regs3,regs[3])
		#regs4=c(regs4,regs[4])
		print(wnds[j])
	}
	#plot_func(-wnds[2:length(wnds)],regs2,t='l',col='red'); 
	#plot_func(-wnds[2:length(wnds)],regs2,ylim=c(1.3,1.8),t='l',col='red'); 
#lines(-wnds[2:length(wnds)],regs3,col='darkgreen'); 
#lines(-wnds[2:length(wnds)],regs4,col='blue')
	#plot_func=lines
	fds=c(fds,mean(regs2))
	sds=c(sds,sd(regs2))
}
plot(fds,ylim=c(0,2))
lines(sds)
abline(h=0)




hist(ts,br=100)

a = dot_fd(runif(100),seq(1,10000,by=5)); plot(log(a$x),log(a$y))


a = dot_fd(diff(log(ts[1:2000])),seq(1,100000,by=20)); 
len=2000; res=list(); for(i in 1:30) res=c(res,list(dot_fd(diff(log(ts[(i*2*len):((i*2+1)*len)])),logseq(1,400000,100))))

len=2000; res=list(); res=foreach(i=1:30)%dopar%dot_fd(diff(log(ts[(i*2*len):((i*2+1)*len)])),round(logseq(1,400000,100)))

cols=rainbow(length(res)); plot_func=plot; for(i in 1:length(res)) {plot_func(log(res[[i]]$x),log(res[[i]]$y),t='l',col=cols[i],xlim=c(-7,-3),ylim=c(2,5)); plot_func=lines;}
cols=rainbow(10); plot_func=plot; for(i in 1:10) {plot_func(log(res[[i]]$x),log(res[[i]]$y),t='l',col=cols[i],xlim=c(-8,0),ylim=c(0,5)); plot_func=lines;}
cols=rainbow(30); plot_func=plot; for(i in 30:1) {plot_func(log(res[[i]]$x),log(res[[i]]$y),t='l',col=cols[i]); plot_func=lines;}
plot(a1$x,a1$y); 
plot(log(a$x),log(a$y),xlim=c(-12,-9),ylim=c(5.5,7)); 
abline(lm(log(a$y)~log(a$x)))


i=18; for(j in 1:25){x=log(res[[i]]$x); y=log(res[[i]]$y); a=loess(y~x,degree=1,span=0.06); x=a$x; y=a$fitted; sy=segmented(lm(y~x),seg.Z=~x,psi=list(x=-c(1,4,7,11)),control=seg.control(it=100))$fit; nidx=which(diff(c(x,57))!=0); dx=diff(x[nidx]); dsy=diff(sy[nidx]); h=hist(dsy/dx,br=30,plot=FALSE); print(which(h$counts>4)); }

i=18; for(j in 1:25){x=log(res[[i]]$x); y=log(res[[i]]$y); a=loess(y~x,degree=1,span=0.06); x=a$x; y=a$fitted; sy=y; nidx=which(diff(c(x,57))!=0); dx=diff(x[nidx]); dsy=diff(sy[nidx]); h=hist(dsy/dx,br=30,plot=FALSE); print(which(h$counts>4)); }#plot(h)}
plot(x,y,t='l',col='red'); lines(x,,col='blue')


x=log(res[[i]]$x); y=log(res[[i]]$y); a=loess(y~x,degree=1,span=0.06); x=a$x; y=a$fitted; sy=segmented(lm(y~x),seg.Z=~x,psi=list(x=-c(1,2,4,8,10)),control=seg.control(it=50))$fit; nidx=which(diff(c(x,57))!=0); dx=diff(x[nidx]); dsy=diff(sy[nidx]); h=hist(dsy/dx,br=10,plot=FALSE); print(which(h$counts>5)); 
x=log(res[[i]]$x); y=log(res[[i]]$y); a=loess(y~x,degree=1,span=0.06); x=a$x; y=a$fitted; sy=segmented(lm(y~x),seg.Z=~x,psi=list(x=-c(1,2,4,8,10)),control=seg.control(it=50))$fit; nidx=which(diff(c(x,57))!=0); dx=diff(x[nidx]); dsy=diff(sy[nidx]); h=hist(dsy/dx,br=10,plot=FALSE); print(which(h$counts>5)); 
x=log(res[[i]]$x); y=log(res[[i]]$y); a=loess(y~x,degree=1,span=0.06); x=a$x; y=a$fitted; sy=segmented(lm(y~x),seg.Z=~x,psi=list(x=-c(1,2,4,8,10)),control=seg.control(it=50))$fit; nidx=which(diff(c(x,57))!=0); dx=diff(x[nidx]); dsy=diff(sy[nidx]); h=hist(dsy/dx,br=10,plot=FALSE); print(which(h$counts>5)); 
x=log(res[[i]]$x); y=log(res[[i]]$y); a=loess(y~x,degree=1,span=0.06); x=a$x; y=a$fitted; sy=segmented(lm(y~x),seg.Z=~x,psi=list(x=-c(1,2,4,8,10)),control=seg.control(it=50))$fit; nidx=which(diff(c(x,57))!=0); dx=diff(x[nidx]); dsy=diff(sy[nidx]); h=hist(dsy/dx,br=10,plot=FALSE); print(which(h$counts>5)); 
x=log(res[[i]]$x); y=log(res[[i]]$y); a=loess(y~x,degree=1,span=0.06); x=a$x; y=a$fitted; sy=segmented(lm(y~x),seg.Z=~x,psi=list(x=-c(1,2,4,8,10)),control=seg.control(it=50))$fit; nidx=which(diff(c(x,57))!=0); dx=diff(x[nidx]); dsy=diff(sy[nidx]); h=hist(dsy/dx,br=10,plot=FALSE); print(which(h$counts>5)); 
x=log(res[[i]]$x); y=log(res[[i]]$y); a=loess(y~x,degree=1,span=0.06); x=a$x; y=a$fitted; sy=segmented(lm(y~x),seg.Z=~x,psi=list(x=-c(1,2,4,8,10)),control=seg.control(it=50))$fit; nidx=which(diff(c(x,57))!=0); dx=diff(x[nidx]); dsy=diff(sy[nidx]); h=hist(dsy/dx,br=10,plot=FALSE); print(which(h$counts>5)); 
x=log(res[[i]]$x); y=log(res[[i]]$y); a=loess(y~x,degree=1,span=0.06); x=a$x; y=a$fitted; sy=segmented(lm(y~x),seg.Z=~x,psi=list(x=-c(1,2,4,8,10)),control=seg.control(it=50))$fit; nidx=which(diff(c(x,57))!=0); dx=diff(x[nidx]); dsy=diff(sy[nidx]); h=hist(dsy/dx,br=10,plot=FALSE); print(which(h$counts>5)); 

i=1; x=log(res[[i]]$x); y=log(res[[i]]$y); a=loess(y~x,degree=1,span=0.06); x=a$x; y=a$fitted; s=segmented(lm(y~x),seg.Z=~x,psi=list(x=-c(1,2,4,8,10)),control=seg.control(it=50)); sy=y; nidx=which(diff(c(x,57))!=0); dx=diff(x[nidx]); dsy=diff(sy[nidx]); hist(dsy/dx,br=100)

ts0=cumsum(runif(20000)-0.5); x=1:length(ts0); r=dot_fd(diff(ts0),round(logseq(1,400000,100))); 
plot(log(r$x),log(r$y))



len=4096; res_count=50; res=list(); res=foreach(i=1:res_count)%dopar%dot_fd(diff(log(ts[(i*len):((i+1)*len)])),round(logseq(1,400000,100)))

ress = foreach(i=1:res_count)%dopar%{
	x=log(res[[i]]$x); y=log(res[[i]]$y); a=loess(y~x,degree=1,span=0.07); lx=a$x; ly=a$fitted;

	if(1==0){
		i=1; x=log(res[[i]]$x); y=log(res[[i]]$y); a=loess(y~x,degree=1,span=0.06); lx=a$x; ly=a$fitted; 
		#plot(x,y,t='l',col='blue'); lines(lx,ly,col='red',t='l'); 
		seg = segmented(lm(ly~lx),seg.Z=~lx,psi=list(lx=-c(2,4,7)),control=seg.control(it=50)); plot(x,y,t='l',col='blue'); lines(lx,seg$fitted,col='red',t='l'); 

		seg1 = seg2; print(slope(seg1)[[1]][,1])
		seg2 = segmented(lm(ly~lx),seg.Z=~lx,psi=list(lx=-c(1,2,4,6,8,10)),control=seg.control(it=50)); print(slope(seg2)[[1]][,1])
		plot(x,seg1$fitted,t='l',col='red'); lines(x,seg2$fitted,col='blue')

		lines(x,seg$fitted);

		i=28; x=log(res[[i]]$x); y=log(res[[i]]$y); a=loess(y~x,degree=1,span=0.07); lx=a$x; ly=a$fitted; segg=segmented(lm(ly~lx),seg.Z=~lx,psi=list(lx=-c(2,5,8)),control=seg.control(it=50)); plot(x,y,t='l',lwd=2); lines(lx,ly,col='blue'); lines(x,segg$fit,col='red',lwd=2)
		
	} 

	res1=foreach(j=1:100)%do%{
		segg=NA; tr=1; 
		while(tr<50 && is.na(segg)) 
			segg=tryCatch({
				as.numeric(slope(segmented(lm(ly~lx),seg.Z=~lx,psi=list(lx=-c(2,5,8)),control=seg.control(it=50)))[[1]][,1])
			}, error=function(ex){tr=tr+1; NA})#print('err!'); tr=tr+1; NA});  
		if(!is.na(segg)) segg else {print(paste('err!',i,j)); NA}
	} 
	print(paste('finishing',i))
	tmp_res = tryCatch({unlist(foreach(k=1:4)%do%{h=hist(unlist(lapply(res1,function(v)v[k])),seq(-2.5,0.2,l=200),plot=FALSE); if(length(h$mids)==1) res1[[1]][k] else h$mids[which.max(h$counts)]})}, error=function(ex) NA)
	print(paste('-- finishing',i))
	tmp_res
}

plot_func=plot; for(r in ress){ plot_func(r,ylim=c(-1.1,0.1),t='l'); plot_func=lines}
plot(unlist(lapply(ress,function(x)x[3])),t='l')
r=ress[[10]]; plot(r,ylim=c(-2,1),t='l'); 


for(i in 1:6) print(paste('MEAN: ',mean(unlist(lapply(res1,function(v)v[i]))),'SD:',sd(unlist(lapply(res1,function(v)v[i])))/mean(unlist(lapply(res1,function(v)v[i])))))
for(i in 1:6) {h=hist(unlist(lapply(res1,function(v)v[i])),br=100000,plot=FALSE); print(h$mids[which(h$counts>8)]);}

hist(unlist(lapply(res1,function(v)v[5])),br=100)

[1] -0.000229005
[1] -0.3655001
[1] -0.5965
[1] -0.4225
[1] -0.1089
[1] -0.7173

[1] -0.0002290005
[1] -0.3655
[1] -0.5965
[1] -0.4225
[1] -0.1089
[1] -0.7173


[1] -0.000229005
[1] -0.3655001
[1] -0.5965
[1] -0.4225
[1] -0.1089
[1] -0.7172999



x=log(res[[i]]$x); y=log(res[[i]]$y); a=loess(y~x,degree=1,span=0.06); x=a$x; y=a$fitted; sy=segmented(lm(y~x),seg.Z=~x,psi=list(x=-c(1,2,4,8,10)),control=seg.control(it=50))$fit; h=hist(diff(sy)/diff(x),br=10,plot=FALSE); print(which(h$counts>5)); plot(x,y,col='red',t='l'); lines(x,sy,col='blue');
x=log(res[[i]]$x); y=log(res[[i]]$y); a=loess(y~x,degree=1,span=0.06); x=a$x; y=a$fitted; sy=segmented(lm(y~x),seg.Z=~x,psi=list(x=-c(1,2,4,8,10)),control=seg.control(it=50))$fit; h=hist(diff(sy)/diff(x),br=10,plot=FALSE); print(which(h$counts>5)); plot(x,y,col='red',t='l'); lines(x,sy,col='blue');
x=log(res[[i]]$x); y=log(res[[i]]$y); a=loess(y~x,degree=1,span=0.06); x=a$x; y=a$fitted; sy=segmented(lm(y~x),seg.Z=~x,psi=list(x=-c(1,2,4,8,10)),control=seg.control(it=50))$fit; h=hist(diff(sy)/diff(x),br=10,plot=FALSE); print(which(h$counts>5)); plot(x,y,col='red',t='l'); lines(x,sy,col='blue');
x=log(res[[i]]$x); y=log(res[[i]]$y); a=loess(y~x,degree=1,span=0.06); x=a$x; y=a$fitted; sy=segmented(lm(y~x),seg.Z=~x,psi=list(x=-c(1,2,4,8,10)),control=seg.control(it=50))$fit; h=hist(diff(sy)/diff(x),br=10,plot=FALSE); print(which(h$counts>5)); plot(x,y,col='red',t='l'); lines(x,sy,col='blue');

s1=segmented(lm(y~x),seg.Z=~x,psi=list(x=-c(4,6,10)))
s1=segmented(lm(y~x),seg.Z=~x,psi=list(x=-(2:11)))

s1=segmented(lm(y~x),seg.Z=~x,psi=list(x=NA),control=seg.control(stop.if.error=FALSE,n.boot=0)); plot(s1,col='red');

data("down")

fit.glm<-glm(cases/births~age, weight=births, family=binomial, data=down)
fit.seg<-segmented(fit.glm, seg.Z=~age,psi=25)

g=glm(y~x)
s1=segmented(g,seg.Z=~x,psi=-5); plot(s1,col='red');

s2=segmented(lm(y~x),seg.Z=~x,psi=list(x=-c(3,7,11)))
s3=segmented(lm(y~x),seg.Z=~x,psi=list(x=-c(1,2,6)))
plot(s1,col='red'); abline(v=-4); abline(v=-6); abline(v=-10); 

plot(s2,col='green'); lines(s3,col='blue')

plot(loess(y1~x1,iterations=5),t='l')
x1=t$x; y1=t$y; plot(loess(y1~x1,degree=1),t='l')

x=log(res[[1]]$x); y=log(res[[1]]$y); a=loess(y~x,degree=1,span=0.08); plot(a$x,a$fitted,t='l'); lines(x,y)
a=loess(t$y~t$x,degree=1,method="model.frame"); plot(a$x,a$fitted,t='l'); lines(t$x,t$y)


a = dot_fd(log(ts[1:2000]),seq(1,10000,by=100)); plot(log(a$x),log(a$y))
a = dot_fd((ts[1:2000]),seq(1,10000,by=5)); plot(log(a$x),log(a$y))






x=seq(-10,10,l=128); y=exp(-x^2); #plot(x,y);
mid_shift=function(n)ceiling(c(n/2+seq(1,n/2,by=1),seq(1,n/2,by=1)));
mid_shift(6)
idx=mid_shift(length(x)); plot(x,abs(fft(y))[idx]); lines(x,y)
len=4096; 

ts1=log(c$Close[200000+(1:len)]);
ts2=cumsum(runif(len)-0.5); 

ts=ts1; x=1:length(ts); y=ts; lts=loess(y~x,degree=2,span=0.005); idx=1:100; lx=lts$x; ly=lts$fitted; dly=diff(ly);
#plot(x[idx],y[idx],t='l',lwd=2); lines(lx[idx],ly[idx],col='blue')
plot(dly[idx])
f=abs(fft(dly))[mid_shift(len)]; x=(1:len)-len/2; y=f[x!=0 & x!=1]; x=x[x!=0 & x!=1]; x=log(abs(x))*sign(x); y=log(y); plot(x,y)

stupid_reg=function(ts_in){
ts=ts_in; x=1:length(ts); y=ts; lts=loess(y~x,degree=2,span=0.01); idx=1:1000; lx=lts$x; ly=lts$fitted; dly=diff(ly); f=abs(fft(dly))[mid_shift(len)]; x=(1:len)-len/2; y=f[x!=0 & x!=1]; x=x[x!=0 & x!=1]; x=log(abs(x))*sign(x); y=log(y); eq = equalify_x(x,y,20); ex=eq[[1]]; ey=eq[[2]]; hash(x=ex,y=ey,reg=lm(ey[16:20]~ex[16:20]))}

t1=stupid_reg(ts1); t2=stupid_reg(cumsum(runif(len)-0.5)); print(t2$reg$coef[2])
plot(ex,ey); abline()
plot(dly[idx])


f=abs(fft(ts))[mid_shift(len)]; x=(1:len)-len/2; y=f[x!=0 & x!=1]; x=x[x!=0 & x!=1]; 
x=log(abs(x))*sign(x); y=log(y); eq = equalify_x(x,y,100); 
plot(eq[[1]],eq[[2]])
plot(x,y)













