sink('log.txt')
bprint <<- function(msg) { }
png('res0.png')
source('R102.R')
#plot(sin(0.5*(1:25)))
dev.off()
sink()
