source('R12.R') 
params1 = list(100,0.4,2:14,0.999,0.02*1:45)
params2 = list(0.999,1)

g = expand.grid(params1)
foreach(i=1:dim(g)[1])%dopar%{
  name_png = paste('pics2/res-',paste(g[i,c(3,5)],collapse='-'),'.png',sep='')
  name_rdata = paste('RData/res_5_NONCLUSTER-150000-',paste(g[i,],collapse='-'),'.RData',sep='')
  print(name_png)
  print(name_rdata)
  png(name_png)
  hist(get(load(name_rdata))[[1]]$ts,br=3000,xlim=c(-4,4))
  dev.off()
}

