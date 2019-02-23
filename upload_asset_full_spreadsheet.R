library(RODBC)
library(moments)
library(stats)
library(pcurve)
library(lmtest)
library(xtable)
library(tseries)
library(PerformanceAnalytics)
library(gplots)
library(graphics)
library(plotrix)
library(RPMG)
library(RColorBrewer)
library(reshape) 
library(fpc) 
library(colorspace)
library(corrplot)
library(ellipse)
library(RColorBrewer)


sys.source("R/grt.R", envir=attach(NULL, name="grt"))
sys.source("R/gr_portfolio_report.R",envir=attach(NULL, name="gr_portfolio_report"))

con <- odbcConnect("crestline")

ptr <- odbcConnect("pertrac",uid=ptr_uid,pwd=ptr_pwd)


sql='select year(now())*100+month(now()) now';
today=sqlQuery(con, sql)$now[1]

today=as.character(formData$date)
ReportDate_Dashboard <- today



UPLOAD_TO_DB = TRUE
deleteRows_FromTestDatabase(ReportDate_Dashboard,con,UPLOAD_TO_DB,"assets")

## needs to be moved to a separate script
#assets_full_data = readLines(paste(outdir,"asset_full.csv",sep=""))
assets_full_data = readLines(formData$path)

assets_full_data_line = c()
PortName = ""
RowName = ""
for(i in 1:length(assets_full_data)) {
  assets_full_data_line = unlist(strsplit(assets_full_data[i],","))
  if (grepl(" AGG",assets_full_data_line[1])){
    RowName = gsub("\"","",assets_full_data_line[1])
    PortName = unlist(strsplit(RowName," "))[1]
    insertRow_ToTestDatabase(ReportDate_Dashboard,con,UPLOAD_TO_DB,PortName,"assets",RowName,"MULTISTRAT EXPOSURE",as.double(gsub("\"","",assets_full_data_line[2])))
    insertRow_ToTestDatabase(ReportDate_Dashboard,con,UPLOAD_TO_DB,PortName,"assets",RowName,"STRATEGY LOSS EXPOSURE",as.double(gsub("\"","",assets_full_data_line[7])))
  }
}

print("Loaded data!")




