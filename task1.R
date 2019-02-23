#source('R102.R')
while(TRUE){
  source('R111BT.R');
  Sys.sleep(3*60*60);
  a = Sys.time()
  save(a, file=paste(gsub(':','',Sys.time()),'.time',sep=''))
} 
