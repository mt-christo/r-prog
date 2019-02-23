library(zoo)
library(PerformanceAnalytics)
library(plotrix)
library(ghyp)
library(xts)
library(gplots)
library(RColorBrewer)
library(colorspace)
library(reshape) 
library(fpc)
library(ellipse)
library(hash)
library(ggplot2)
library(fastICA)
library(pcaMethods)
library(xtsExtra)
library(stringr)





#number of processes to be used depending on task priority
procInsane=30 
procHigh=22 
procMed=10
procLow=5



grcolors=brewer.pal(8,"Dark2")
grangle=c(0,35,90,180,125)
grdensity=c(NA,25)
grcols=rep(grcolors,length(grangle)*length(grdensity))
grangls=c()
grdens=c()

for (i in 1:length(grcolors)*length(grdensity))
{
  grangls=c(grangls,rep(grangle[1],length(grcolors)))
  grangle=c(grangle[2:length(grangle)],grangle[1])
}

for (i in 1:length(grangle)*length(grcolors))
{
  grdens=c(grdens,rep(grdensity[1],length(grcolors)))
  grdensity=c(25,25)
}


#barplot(rep(10,14),col=grcols,angle=grangls,density=grdens,border=grcols)
#        legend=rep(10,10),args.legend = list(x = "topleft")
#        )




riskDB='Crestline.'
outdir='/mnt/G/RISK/R-OUTPUT/'

#!!! Think twice before changing it to multiple portfolios, the logic in get_fund_rt might need to be changed if you do so
crestlineRTportfolios <- c('COF')#('CAPF')#c('CP')#,'COF','BGF','CAPF')

crestlineStandalonePortfolios <- c('CP','COF','CPLUS','CPOF','CEA','CEAOF',
                                     'CAPF','BGF','LSP','CMH','CRAKD'
                                  )
                                   
crestlineSubPortfolios <- c('EMNON','EMNOFF')


crestlinePortfolios <- c(crestlineStandalonePortfolios,crestlineSubPortfolios)

                            
ptr_uid='riskreader'
ptr_pwd='r15k***'

pmw_uid='pmwreader'
pmw_pwd='s3rvic$'

pmwAnton_uid='risk'
pmwAnton_pwd='risk'

pmwReference_uid='readonly'
pmwReference_pwd='readonly'

Alerts_uid='basscompanies\\aslepnev'
Alerts_pwd='crestline2'


#pmwReference <- odbcConnect("pmwReference",uid=pmwReference_uid,pwd=pmwReference_pwd)




#'CAPF'

realized_RTline <- paste(",'",crestlineRTportfolios,"'",sep='',collapse='')
realized_RTline <- substr(realized_RTline,2,nchar(realized_RTline))

setRealizedRtLine <- function (newLine)
  {
    realized_RTline <<- newLine
  }

generate_dates <- function (ystart,yend)
  {
    dates <- c()
    for (i in ystart:yend)
      {
        for (j in 1:12)
          {

            pref <- '0'
            if (j>9)
              {
                pref <- ''
              }
            dates=c(dates,paste(i,pref,j,sep=''))
          }
      }
    dates
  }


crisis_start <- c('1987-10-01','1990-07-01','1992-07-01','1994-02-01','1997-06-01','1998-03-01','1998-07-01','1999-01-01','1999-07-01','1999-11-01','2000-03-01','2000-09-01','2001-09-01','2002-05-01','2007-07-01','2007-10-01','2011-08-01')
crisis_end <- c('1987-10-01','1990-10-01','1993-03-01','1994-05-01','1997-12-01','1998-10-01','1998-10-01','1999-05-01','1999-08-01','2000-01-01','2000-05-01','2001-03-01','2001-09-01','2002-09-01','2007-08-01','2009-02-01','2011-09-01')


agg_dates <- generate_dates(1980,2100)

#tickers=use_index
#minsize=36
#lastdate=''
#id=FALSE
#dates=index(cRt)
#mandatory=FALSE
#supress=TRUE
#all=TRUE
#fill=FALSE)
#tickers=c(''US0001M_INDEX'')
get_benchmarks <- function (con,tickers,minsize=24,lastdate='',id=FALSE,dates=c(),mandatory=FALSE,supress=FALSE,all=FALSE,fill=TRUE)
  {
    ret <- zoo()
    index <- 1
    names <- c()
    ntickers=c()
    ticker <- tickers[1]
    shift <- c('US0001M_INDEX','USGG1M_INDEX','USGG3M_INDEX')

    for (ticker in tickers)
      {
        message("GET::",ticker)
        if (id)
          {
            sql <- paste('select year(rt.dt)*100+month(rt.dt) dt, avg(rt) rt from rt where rt.fund_id=',ticker,' group by dt order by dt',sep='')
          } else
        {
          sql <- paste('select year(rt.dt)*100+month(rt.dt) dt, avg(rt) rt from rt, fund f where f.fund_id=rt.fund_id and f.ticker="',ticker,'" group by dt order by dt',sep='')
        }
       # print(sql)

        rt <- sqlQuery(con,sql)
        zrt <- zoo(rt$rt,rt$dt)
        
        if (sum(shift==ticker)>0)
          {
            if (length(zrt)>1)
              {
                zrt <- zoo(as.double(zrt[1:(length(zrt)-1)]),index(zrt)[2:(length(zrt))])
                zrt=(1+zrt)^(1/12)-1
                message("SHIFTED")
              }
            else
              {
                zrt=zoo()
              }
            
          }

        if (length(dates))
          {
            zrt <- sub_rt(zrt,dates,ticker,fill,supress)
          } else
        {
          if (length(which(index(zrt)==lastdate))==0 && nchar(lastdate)>0)
              {
                if (mandatory)
                  {
                    stop ('Inconsistent returns for ',ticker);
                  }
                next
              }
            
            if (length(index(zrt))<minsize || length(zrt)==0)
              {
                if (mandatory)
                  {
                    stop ('Inconsistent returns for ',ticker);
                  }
                
                next
              }
            
        
            if (index>1 && length(intersect(index(zrt),index(ret)))<minsize)
              {
                if (mandatory)
                  {
                    stop ('Inconsistent returns for ',ticker);
                  }

                next
              }
          }
        
        if (index==1)
          {
            ret=zrt
          }
        else
          {
            ret=merge(ret,zrt,all=all)
          }
        index <- index+1
        

        name=''
        if (!id)
          {
            sql <- paste('select name from fund f where  f.ticker="',ticker,'"',sep='')
            name <- as.character(sqlQuery(con,sql)$name[1])
          } else
        {
          sql <- paste('select name from fund f where  f.fund_id="',ticker,'"',sep='')
          name <- as.character(sqlQuery(con,sql)$name[1])
#          name <- as.character(ticker)
        }
#        print(paste('NAME=',name));
        
        names <- c(names,name)
        ntickers <- c(ntickers,ticker)
        
      }
    if (length(names)>1)
      {
        #cat(paste(',',names,sep=''),'\n')
        colnames(ret)=names
      }

    attr(ret,'name')=names
    attr(ret,'tickers')=ntickers
    
    ret
  }

merge_returns <- function (art,brt)
{
  
  dates <- sort(union(art$dt,brt$dt))
  rt <- zoo()
  if (length(dates)!=0)
    {
      rt <- zoo(0,dates)
      
      i <- 1
      for ( i in 1:length(rt))
        {
          
          fnd <- which(art$dt==index(rt)[i])
          if (length(fnd))
            {
              rt[i]=art$rt[fnd[1]]
            }       else
          {
            fnd <- which(brt$dt==index(rt)[i])
            rt[i]=brt$rt[fnd[1]]
          }
        }
    }
  rt

}

rt2e <- function (rt)
  {
    ert <- rt
    for (i in 1:length(rt))
      {
        ert[i]=log(1+rt[i])
      }
    ert
  }


#clCon=con
#pid=0
#con=ptr
#id=cl_id
#portfolio_id=use_port_id
#modelid=99197
#id=ids$id[i]
#use_index_proxy=TRUE
#model_id=-1
#id=onshore_fund_id
#model_id=0
# onrt=get_fund_rt(con,ptrcon,onshore_fund_id,-1,-1,0,0,-9999999999999999999999999999999999999999999999999,1)
   
# res <- get_fund_rt(con,ptr,cl_id,pa_id)
# clCon=con
#con= ptr
#id = 99133112
#use_index_proxy=TRUE
#id=ids$id[i]
#portfolio_id=hportfolio_id


get_fund_rt <- function(clCon,con,id,pid=-1,modelid=-1,portfolio_id=0,portfolio_only=0,min_investment=-99999999999999999999999999999,no_proxy=0,use_index_proxy=FALSE,onshore_offshore=FALSE)
  {
    if (is.na(pid))
      {
        pid=0
      }
    
    res <- zoo()
    if (!is.na(id))
      {

        mapid=0
        sql <- paste('select f1.map_to_id id from fund f1 where f1.fund_id=',id)
        mapid=isnull(sqlQuery(clCon,sql)$id,0)
      
        
                                        # albourne realized returns
        sql <- paste('select year(rt.dt)*100+month(rt.dt) dt , avg(rt) rt from  allocation rt,fund f, portfolio p
                  where rt.fund_id=f.fund_id and f.map_to_id=',id,'
                        and p.portfolio_id=rt.portfolio_id and p.name like "%ALBOURNE%" and rt.rt is not null
                  group by rt.dt
                  order by rt.dt')
        
        brt <- sqlQuery(clCon,sql)
        
        # crestline realized returns specific to offshore/onshore
        sql <- paste('select year(rt.dt)*100+month(rt.dt) dt , sum(allocation*rt)/sum(allocation) rt from  allocation rt,fund f, portfolio p
                  where rt.fund_id=f.fund_id and f.map_to_id=',mapid,'
                        and p.portfolio_id=rt.portfolio_id and p.ticker in (',realized_RTline,') and allocation>',min_investment,' and (rt.rt is not null)
                  group by rt.dt
                  order by rt.dt')
        
        creal = sqlQuery(clCon,sql)

        
        crt = data.frame(dt=c(),rt=c())
        
        if (!no_proxy)
          {
            # albourne proxies 
            sql <- paste('select year(rt.dt)*100+month(rt.dt) dt , avg(rt) rt from  rt,fund f where rt.fund_id=f.fund_id and f.map_to_id=',id,' and rt.rt is not null
                  group by rt.dt
                  order by rt.dt')
            
            crt <- sqlQuery(clCon,sql)

          }
        
                                        #model after - mix of onshore and offshore returns (if exists)
        if (modelid<0)
          {
            sql <- paste('select f2.model_after_id id from fund f1, fund f2 where f1.fund_id=',id,' and f1.map_to_id=f2.fund_id')
            modelid=isnull(sqlQuery(clCon,sql)$id,0)
          }
        
       # crestline realized returns mixed across offshore/onshore
        sql <- paste('select year(rt.dt)*100+month(rt.dt) dt , sum(allocation*rt)/sum(allocation) rt from  allocation rt,fund f, portfolio p, fund f2
                  where rt.fund_id=f.fund_id and f.map_to_id=f2.fund_id and f2.model_after_id=',modelid,'
                        and p.portfolio_id=rt.portfolio_id and p.ticker in (',realized_RTline,') and allocation>',min_investment,' and (rt.rt is not null)
                  group by rt.dt
                  order by rt.dt')
        creal2 = sqlQuery(clCon,sql)


        #all returns across fund families
        sql <- paste('select year(rt.dt)*100+month(rt.dt) dt , avg(rt) rt
                  from  rt,fund f, fund f2 where rt.fund_id=f.fund_id and f.map_to_id=f2.fund_id and f2.model_after_id=',modelid,' and rt.rt is not null
                  group by rt.dt
                  order by rt.dt')
        
        zrt <- sqlQuery(clCon,sql)
        
        # use index returns (lowest quaality of returns)
        index_proxy = data.frame(dt=c(),rt=c())
        if (use_index_proxy && isnull(modelid,0)>0)
          {
            
            sql <- paste('select hf_index from fund where fund_id=',modelid)
            index <- isnull(as.character(sqlQuery(clCon,sql)[1,1]),'')
            if (!nchar(isnull(index,'')))
                {
                  message("NO PROXY => USING CASH RETURNS!")
                  index_zoo <- get_benchmarks(clCon,tickers=c('USGG1M_INDEX'),0,'',FALSE,c(),TRUE)
                  index_proxy=data.frame(dt=index(index_zoo),rt=as.vector(index_zoo))
                } else
                {
                  sql <- paste('select year(rt.dt)*100+month(rt.dt) dt, rt from rt rt, fund f where f.ticker="',index,'" and f.fund_id=rt.fund_id and rt.rt is not null order by rt.dt',sep='')
                  index_proxy=sqlQuery(clCon,sql)
                }
            message('INDEX=>',index)
          }

        
        #actual realized returns in crestline portfolios
        if (portfolio_id>0 || portfolio_id==-2)
          {

           # returns specific to a portfolio
            sql <- paste('select year(rt.dt)*100+month(rt.dt) dt , sum(allocation*rt)/sum(allocation) rt
                   from  allocation rt,fund f, portfolio p, fund f2
                  where rt.fund_id=f.fund_id and f.map_to_id=f2.fund_id and f2.model_after_id=',modelid,' and allocation>',min_investment,'
                     and (rt.rt is not null)
                        and rt.portfolio_id=',portfolio_id,' 
                  group by rt.dt
                  having sum(allocation)>0
                  order by rt.dt')

          } else
        {
                                        #mix of returns across all portfolios
          sql <- paste('select year(rt.dt)*100+month(rt.dt) dt , sum(allocation*rt)/sum(allocation) rt from  allocation rt,fund f, portfolio p, fund f2
                  where rt.fund_id=f.fund_id and f.map_to_id=f2.fund_id and f2.model_after_id=',modelid,'
                        and p.portfolio_id=rt.portfolio_id and p.ticker in (',realized_RTline,') and allocation>',min_investment,' and (rt.rt is not null)
                  group by rt.dt
                  order by rt.dt')
        }
        
        art <- sqlQuery(clCon,sql)
        
        if (portfolio_only>0)
          {
            if (portfolio_id>0) # just returns specific to portfolio
              {
                res=zoo(art$rt,art$dt)
              } else
            {
              res <- merge_returns(art,creal) # returns specific to the type of fund and then across every portfolio
            }
          } else
        {
          
          if (pid<0)
            {
              sql <- paste('select pertrac_id pid from fund where fund_id=',id);
              pid=sqlQuery(clCon,sql)$pid
              if (isnull(pid,0)==0)
                {
                  sql <- paste('select pertrac_id pid from fund where fund_id=',mapid);
                  pid=sqlQuery(clCon,sql)$pid
                  if (isnull(pid,0)==0)
                    {
                      sql <- paste('select pertrac_id pid from fund where fund_id=',modelid);
                      pid=sqlQuery(clCon,sql)$pid
                      if (isnull(pid,0)==0)
                        {
                          pid=0
                        }
                    }
                }
              message('PID=',pid,'\n')
            }
          
          
          
                                        # names
          clName <- as.character(sqlQuery(clCon,paste('select name from fund where fund_id=',id))$name[1])
          message('FUNDRT::',clName)
          message (paste("Crestline:",art$dt[1],'-',art$dt[nrow(art)]))
          message (paste("Crestline2:",creal$dt[1],'-',creal$dt[nrow(creal)]))
          message (paste("Crestline3:",creal2$dt[1],'-',creal2$dt[nrow(creal2)]))
          message (paste("IndexProxy:",index_proxy$dt[1],'-',index_proxy$dt[nrow(index_proxy)]))
                    
          
          ptrName <- as.character(sqlQuery(con,paste("select mastername from mastername where id=",pid))$mastername[1])
          
                                        # realized crestline returns  returns 
          res=merge_returns(art,creal)
          res=merge_returns(art,creal2)
          
          # realized Albourne 
          res=merge_returns(data.frame(dt=index(res),rt=as.vector(res)),brt)
                
          # now loading pertrac
          sql <- paste("select DATEPART(YYYY,date)*100+DATEPART(MM,date) dt, p.[return] rt from Performance  p where p.id=",pid,'order by dt',sep='');
          raw_prt <- sqlQuery(con, sql)
          
          if (!(length(raw_prt)))
            {
              message (paste("NO PERTRAC DATA FOR ",id,' ',clName,pid))
              raw_prt=data.frame(dt=c(),rt=c())
            } else
          {
              message (paste("PERTRAC:",clName,':',raw_prt$dt[1],'-',raw_prt$dt[nrow(raw_prt)]))
          }

          #adding pertract
          res <- merge_returns(data.frame(dt=index(res),rt=as.vector(res)),raw_prt)

          # now adding proxies if they exists
          
          res <- merge_returns(data.frame(dt=index(res),rt=as.vector(res)),crt) #albourne proxies
          res <- merge_returns(data.frame(dt=index(res),rt=as.vector(res)),zrt) # any returns available
          res <- merge_returns(data.frame(dt=index(res),rt=as.vector(res)),index_proxy)  # proxy returns
        }
      }
    res

  }

get_class_benchmark <- function (clCon,classification='MAIN_RISK')
{
  sql <- paste('select class, benchmark, short_name  from class_benchmark where classification="',classification,'"',sep='')
  sqlQuery(clCon,sql)
}


get_fund_info <- function (clCon,portfolio_id=0)
{
  sql="select * from fund where ticker is not null and type='FUND'"
  a=sqlQuery(clCon,sql)
}

get_portfolioLeverage <- function (clCon)
{
  sql <- 'select pl.portfolio_id,p.ticker, year(pl.dt)*100+month(pl.dt) dt, pl.aum, pl.leverage from portfolio p, portfolio_leverage pl where p.portfolio_id=pl.portfolio_id'
  sqlQuery(clCon,sql)
}

  
  


pRanks <- c(0.25,0.5,0.75)

getfund_pLevel <- function(p)
  {
    level=length(pRanks)+1
    if (!is.na(p))
      {
        for (i in 1:length(pRanks))
          {
            if (p<pRanks[i])
              {
                level=i
                break;
              }
          }
      } else
    {
      level=length(pRanks)+2
    }
    level
  }


get_fund_alloc <- function(clCon,id,port_id,model_id=0,dt='')
  {
    fnd=''
    fnd2=''
    fld='f1.map_to_id'
    fdt=''
    
     if (model_id>0)
       {
         fnd <- ',fund f2 '
         fnd2=' and f2.fund_id=f1.map_to_id'
         fld='f2.model_after_id'
       }

    if (nchar(dt))
      {
        fdt=paste(' and a.dt="',dt,'"',sep='')
      }
   
    
    sql <- paste('select year(a.dt)*100+month(a.dt) dt , sum(allocation) rt from  allocation a, fund f1',fnd,'
                   where a.portfolio_id=',port_id,' and a.fund_id=f1.fund_id and ',fld,'=',id,' ',fnd2,' ',fdt,'
                  group by a.dt
                  order by a.dt')

    #message(sql)
        
    art <- tozoo(sqlQuery(clCon,sql))

    art
    
  }

get_port_alloc <- function(clCon,port_id,model_id=0,dt='')
  {
    fnd=''
    fnd2=''
    fld='f1.map_to_id'
    fdt=''
    
     if (model_id>0)
       {
         fnd <- ',fund f2 '
         fnd2=' and f2.fund_id=f1.map_to_id'
         fld='f2.model_after_id'
       }

    if (nchar(dt))
      {
        fdt=paste(' and a.dt="',dt,'"',sep='')
      }
   
    
    sql <- paste('select ',fld,' id ,year(a.dt)*100+month(a.dt) dt , sum(allocation) rt from  allocation a, fund f1',fnd,'
                   where a.portfolio_id=',port_id,' and a.fund_id=f1.fund_id ',fnd2,' ',fdt,'
                  group by ',fld,',a.dt
                  order by ',fld,',a.dt')

    message(sql)
        
    art <- sqlQuery(clCon,sql)

    art
    
  }




remove_pts <- function (y, rm)
  {
    removal=array(0,length(y)-length(rm))
    j <- 1
    for (i in 1:length(y))
      {
        if (!length(which(rm==i)))
          {
            removal[j] <- i
            j=j+1
          }
      }

    z <- y[c(removal)]
  }

remove_pts_mx<- function (y, rm)
  {
    removal=array(0,nrow(y)-length(rm))
    j <- 1
    for (i in 1:nrow(y))
      {
        if (!length(which(rm==i)))
          {
            removal[j] <- i
            j=j+1
          }
      }

    z <- y[c(removal),]
  }


get_index_crestline <- function (con,classification)
  {
    sql <- paste('select trim(class) class , trim(strategy) strategy from index_crestline where classification="',classification,'"',sep='')
    crestline<- sqlQuery(con,sql)

    sql <- paste('select trim(ticker) ticker, trim(strategy) strategy, trim(class) class from index_index where classification="',classification,'"',sep='')
    index<- sqlQuery(con,sql)

    return (list(crestline=crestline,index=index))
  }


get_prices <- function (con,id)
  {
    sql <- paste('select year(dt)*10000+month(dt)*100+day(dt) dt, pc from price where fund_id=',id,' order by dt');
    rt <- sqlQuery(con,sql)
    rt
  }

get_ticker_prices <- function (con,id,return_type=16)
  {
    sql <- paste('select year(dt)*10000+month(dt)*100+day(dt) dt, rt from fund f, rt  where f.ticker="',id,'" and f.fund_id=rt.fund_id and return_type_id=',return_type,' order by dt',sep='')
    rt <- sqlQuery(con,sql)
    
    tozoo(rt)
  }

get_id_prices <- function (con,id,return_type=16)
  {
    sql <- paste('select year(dt)*10000+month(dt)*100+day(dt) dt, rt from rt   where fund_id=',id,' and return_type_id=',return_type,' order by dt',sep='')
    rt <- sqlQuery(con,sql)
    tozoo(rt)
  }

get_stock_index <- function(con,index)
  {
    sql <- paste('select distinct si.fund_id, f2.name from stock_index si, fund f, fund f2 where f2.fund_id=si.fund_id and si.benchmark_id=f.fund_id and f.name="',index,'" order by f2.name',sep='')
    #message(sql)
    rt <- sqlQuery(con,sql)
    rt
  }



get_attribute <- function (con,id,tag)
  {
    
    sql <- paste('select fa.value from fund_attribute fa, tag where fund_id=',id,' and fa.tag_id=tag.id and tag.name="',tag,'"',sep='');
    tag <- sqlQuery(con,sql)$value[1]
    tag
  }

tozoo <- function(grt)
  {
    rt <- zoo(grt$rt,grt$dt)
    rt
  }

CLTimeSeries <-
function (R, reference.grid = TRUE, xaxis = TRUE, type = "l", lty = 1, lwd = 1, main = NULL, ylab=NULL, xlab="Date", date.format.in="%Y-%m-%d", date.format = "%m/%y", xlim = NULL, ylim = NULL, event.lines = NULL, event.labels = NULL, period.areas = NULL, event.color = "darkgray", period.color = "lightgray", colorset = (1:12), pch = (1:12), darken = FALSE , legend.loc = NULL, ylog = FALSE, ...)
{ # @author Peter Carl

    y = R

    # Set up dimensions and labels
    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)
    #rownames = rownames(y)
    rownames = as.Date(time(y))

    # Re-format the dates for the xaxis
#     rownames = format(strptime(as.Date(rownames),format = date.format.in), date.format)
    rownames = format(strptime(rownames,format = date.format.in), date.format)
    # If the Y-axis is ln
    logaxis = ""
    if(ylog) {
        logaxis = "y"
    }

    if(is.null(ylab)) {
        if(ylog) 
            ylab = "ln(Value)"
        else
            ylab = "Value"
    }

    # Set color for key elements, easy to darken for the printer
    if(darken)
        elementcolor = "darkgray" #better for the printer
    else
        elementcolor = "lightgray" #better for the screen

    plot.new()

    if(is.null(xlim[1])) # is.na or is.null?
        xlim = c(1,rows)
    if(is.null(ylim[1])){
        ylim = range(y, na.rm=TRUE)
    }
    plot.window(xlim, ylim, xaxs = "r", log = logaxis)
    dimensions = par("usr")

    # Draw any areas in the background
    if(!is.null(period.areas)) {
        period.ind = NULL
        for(period in 1:length(period.areas)){
            period.ind = list(grep(period.areas[[period]][1], rownames), grep(period.areas[[period]][2], rownames))
            rect(period.ind[1], dimensions[3], period.ind[2], dimensions[4], col = period.color, border=NA)
        }
    }

    # The default label and grid placement is ok, but not great.  We set up
    # indexes for each to improve placement.
    # @todo: measuring the length of data set and setting sensible ticks needs improvement

    if(xlim[2]>=200)
        tickspace=24
    if(xlim[2]>=100)
        tickspace=12
    if(xlim[2]>=50)
        tickspace=6
    else
        tickspace=4

    lab.ind = seq(1, rows, by = tickspace/2)
    grid.ind = seq(1, rows, by = tickspace)
    # lab.ind = seq(1,rows,length=rows/divisor)

    # Draw the grid
    if (reference.grid) {
        grid(nx = NA, ny = NULL ,col = elementcolor)
        #grid(col="darkgray")
        abline(v=grid.ind, col = elementcolor, lty = "dotted")
    }

    # Draw a solid reference line at zero
    abline(h = 0, col = elementcolor)

    # Add event.lines before drawing the data
    # This only labels the dates it finds
    if(!is.null(event.lines)) {
        event.ind = NULL
        for(event in 1:length(event.lines)){
            event.ind = c(event.ind, grep(event.lines[event], rownames))
        }
        number.event.labels = ((length(event.labels)-length(event.ind) + 1):length(event.labels))

        abline(v = event.ind, col = event.color)
        if(!is.null(event.labels)) {
            text(x=event.ind,y=ylim[2], label = event.labels[number.event.labels], offset = .2, pos = 2, cex = 0.7, srt=90, col = event.color)
        }
    }

    # Expand the attributes to #columns if fewer values are passed in
    # (e.g., only one), to allow the user to pass in line, type, or
    # symbol variations.
    if(length(lwd) < columns)
        lwd = rep(lwd,columns)
    if(length(lty) < columns)
        lty = rep(lty,columns)
    if(length(pch) < columns)
        pch = rep(pch,columns)

    for(column in columns:1) {
        lines(1:rows, y[,column], col = colorset[column], lwd = lwd[column], pch = pch[column], lty = lty[column], type = type, ...)
    }

    if (xaxis) {
        axis(1, at = lab.ind, lab=rownames[lab.ind], cex.axis = 0.8, col = elementcolor)
        title(xlab = xlab)
        # use axis(..., las=3) for vertical labels.
    }

    # set up y-axis
    axis(2, cex.axis = 0.8, col=elementcolor, ylog=ylog)
    box(col = elementcolor)

    if(!is.null(legend.loc)){
        # There's no good place to put this automatically, except under the graph.
        # That requires a different solution, but here's the quick fix
        legend(legend.loc, inset = 0.02, text.col = colorset, col = colorset, cex = .8, border.col = elementcolor, lwd = 2, bg = "white", legend = columnnames)
    }

    # Add the other titles
    if(is.null(main))
        main=columnnames[1]
    title(ylab = ylab)
    title(main = main)

}


#ptr=object@con[[1]]
#pa_id=10067
getPertrac <- function (ptr,pa_id,cutoff='9999/12/31',date=1)
  {
    sql <- paste("select mastername from mastername where id=",pa_id)
    res <- sqlQuery(ptr, sql)
    fName <- as.character(res$mastername)[1]
    mult=100
    addon=1
    if (date==0)
      {
        mult=1
        addon=0
      }

 
    sql <- paste("select DATEPART(YYYY,date)*",mult*100,"+DATEPART(MM,date)*",mult,"+",addon," dt, [return] rt, fundsmanaged aum from Performance  p where p.id=",pa_id,
                 " and date<'",cutoff,"'", 
                 " order by p.date",sep='')

#    message(sql)
    res <- sqlQuery(ptr, sql)

    message("getPertrac:: rows - ",nrow(res))
    ndx=c()
    if (isnull(nrow(res),0)>1) # sometimes pertrac has duplicate returns - we are picking latest ones
      {
        ndx=c(!res$dt[2:nrow(res)]==res$dt[1:(nrow(res)-1)],TRUE)
      } else if (isnull(nrow(res),0)==1)
        {
          ndx=c(TRUE)
        }
        
    
    man <- list( 
                dt=res$dt[ndx],
                rt=res$rt[ndx],
                aum=res$aum[ndx],
                name=fName
                )
    return(man)

  }


rfIndex = 'USGG1M_INDEX'      # risk free return
liborIndex = 'US0001M_INDEX'  #libor rate

albourneAC <- c(
'ALACACTIVE_INDEX',
'ALACEHASIAPAC_INDEX',
'ALACCAI_INDEX',
'ALACCTA_INDEX',
'ALACDSI_INDEX',
'ALACEMFIHY_INDEX',
'ALACEHEMERGING_INDEX',
'ALACEHEUR_INDEX',
'ALACFSV_INDEX',
'ALACFEMNI_INDEX',
'ALACGLASSETALLOC_INDEX',
'ALACEHGLOBAL_INDEX',
'ALACMACRO_INDEX',
'ALACEHJAPAN_INDEX',
'ALACFMORTGAGE_INDEX',
'ALACMUTIDIVCERSIFIED_INDEX',
'ALACEVENTMULT_INDEX',
'ALACFI_INDEX',
'ALACQEMNI_INDEX',
'ALACFIHY_INDEX',
'ALACERISKARB_INDEX',
'ALACEHBIOHEATH_INDEX',
'ALACEHCOMMODITIES_INDEX',
'ALACEHREALESTATE_INDEX',
'ALACEHTECHNOLOGY_INDEX',
'ALACEHUTILENERGY_INDEX',
'ALACSAEMNI_INDEX',
'ALACEHUS_INDEX',
'ALACVOL_INDEX')


hfri_fof <- c('HFRIFOFC_INDEX',
              'HFRIFOFD_INDEX',
              'HFRIFOFM_INDEX',
              'HFRIFOFS_INDEX',
              'HFRIFOF_INDEX',
              'HFRIFWI_INDEX'
              )

hfri_composite <- c('HFRIDSI_INDEX',
                    'HFRIMAI_INDEX',
                    'HFRIREGD_INDEX',
                    'HFRISEN_INDEX',
                    'HFRIEMNI_INDEX',
                    'HFRIENHI_INDEX',
                    'HFRISHSE_INDEX',
                    'HFRISTI_INDEX',
                    'HFRIEM_INDEX',
                    'HFRIEMA_INDEX',
                    'HFRIEMG_INDEX',
                    'HFRIEMLA_INDEX',
                    'HFRICIS_INDEX',
                    'HFRIEHI_INDEX',
                    'HFRIEDI_INDEX',
                    'HFRIMI_INDEX',
                    'HFRIMTI_INDEX',
                    'HFRIRVA_INDEX',
                    'HFRIFIMB_INDEX',
                    'HFRICAI_INDEX',
                    'HFRIFIHY_INDEX',
                    'HFRIFI_INDEX',
                    'HFRISRE_INDEX')

hfri_composite_standalone<- c('HFRIDSI_INDEX',
                              'HFRIMAI_INDEX',
                              'HFRIREGD_INDEX',
                              'HFRISEN_INDEX',
                              'HFRIEMNI_INDEX',
                              'HFRIENHI_INDEX',
                              'HFRISHSE_INDEX',
                              'HFRISTI_INDEX',
                              'HFRIEM_INDEX',
                              'HFRICIS_INDEX',
                              'HFRIEHI_INDEX',
                              'HFRIEDI_INDEX',
                              'HFRIMI_INDEX',
                              'HFRIMTI_INDEX',
                              'HFRIRVA_INDEX',
                              'HFRIFIMB_INDEX',
                              'HFRICAI_INDEX',
                              'HFRIFIHY_INDEX',
                              'HFRIFI_INDEX',
                              'HFRISRE_INDEX')


hfrx_composite=c('HFRXAR_INDEX',
'HFRXCA_INDEX',
'HFRXDS_INDEX',
'HFRXED_INDEX',
'HFRXEH_INDEX',
'HFRXEMN_INDEX',
'HFRXEWE_INDEX',
'HFRXEWG_INDEX',
'HFRXEWJ_INDEX',
'HFRXEW_INDEX',
'HFRXGL_INDEX',
'HFRXMA_INDEX',
'HFRXMD_INDEX',
'HFRXM_INDEX',
'HFRXRVA_INDEX'
)

tremont_composite <- c(
'HEDGCONV_INDEX',
'HEDGDEDS_INDEX',
'HEDGDIST_INDEX',
'HEDGDRIV_INDEX',
'HEDGEDMS_INDEX',
'HEDGEMGM_INDEX',
'HEDGFIAR_INDEX',
'HEDGFUTR_INDEX',
'HEDGGLMA_INDEX',
'HEDGLSEQ_INDEX',
'HEDGMSTR_INDEX',
'HEDGNEUT_INDEX',
'HEDGRISK_INDEX',
'HILEA_INDEX',
'HILEAA_INDEX',
'HILEAAEU_INDEX',
'HILEAE_INDEX',
'HILEAEU_INDEX',
'HILEAL_INDEX',
'INVXCHF_INDEX',
'INVXCONV_INDEX',
'INVXCVCH_INDEX',
'INVXCVEU_INDEX',
'INVXDEDS_INDEX',
'INVXDRCH_INDEX',
'INVXDREU_INDEX',
'INVXDRIV_INDEX',
'INVXDRJP_INDEX',
'INVXDSEU_INDEX',
'INVXDSJP_INDEX',
'INVXEMGM_INDEX',
'INVXEUR_INDEX',
'INVXFIAR_INDEX',
'INVXFICH_INDEX',
'INVXFUTR_INDEX',
'INVXGLMA_INDEX',
'INVXGMCH_INDEX',
'INVXGMEU_INDEX',
'INVXGMJP_INDEX',
'INVXJPY_INDEX',
'INVXLSCH_INDEX',
'INVXLSEQ_INDEX',
'INVXLSEU_INDEX',
'INVXLSJP_INDEX',
'INVXMFCH_INDEX',
'INVXMFEU_INDEX',
'INVXMFJP_INDEX',
'INVXMSCH_INDEX',
'INVXMSEU_INDEX',
'INVXMSJP_INDEX',
'INVXMSTR_INDEX',
'INVXNAV_INDEX',
'INVXNEUT_INDEX',
'INVXNTCH_INDEX',
'INVXNTEU_INDEX',
'INVXNTJP_INDEX',
'SECTAH_INDEX',
'SECTAHCH_INDEX',
'SECTAHEU_INDEX',
'SECTAHJP_INDEX',
'SECTCONV_INDEX',
'SECTCVEU_INDEX',
'SECTCVJP_INDEX',
'SECTDEDS_INDEX',
'SECTDREU_INDEX',
'SECTDRIV_INDEX',
'SECTDRJP_INDEX',
'SECTDSEU_INDEX',
'SECTEMEU_INDEX',
'SECTEMGM_INDEX',
'SECTEMJP_INDEX',
'SECTFIAR_INDEX',
'SECTFIEU_INDEX',
'SECTFUTR_INDEX',
'SECTGLMA_INDEX',
'SECTGMEU_INDEX',
'SECTLSCH_INDEX',
'SECTLSEQ_INDEX',
'SECTLSEU_INDEX',
'SECTLSJP_INDEX',
'SECTMFCH_INDEX',
'SECTMFEU_INDEX',
'SECTMFJP_INDEX',
'SECTMSCH_INDEX',
'SECTMSEU_INDEX',
'SECTMSJP_INDEX',
'SECTMSTR_INDEX',
'SECTNEUT_INDEX',
'SECTNTEU_INDEX',
'SECTNTJP_INDEX')

hennesy_composite=c(
  'HHFDBIOT_INDEX',
'HHFDCONV_INDEX',
'HHFDCORR_INDEX',
'HHFDDIST_INDEX',
'HHFDEMRG_INDEX',
'HHFDEURO_INDEX',
'HHFDEVDR_INDEX',
'HHFDFINL_INDEX',
'HHFDGLOB_INDEX',
'HHFDGRTH_INDEX',
'HHFDHNES_INDEX',
'HHFDHYLD_INDEX',
'HHFDINTL_INDEX',
'HHFDLEVR_INDEX',
'HHFDLTAM_INDEX',
'HHFDMCRO_INDEX',
'HHFDMULT_INDEX',
'HHFDNCOR_INDEX',
'HHFDNEUT_INDEX',
'HHFDOPPT_INDEX',
'HHFDPACF_INDEX',
'HHFDREGD_INDEX',
'HHFDRISK_INDEX',
'HHFDSHRT_INDEX',
'HHFDTECH_INDEX',
'HHFDTEME_INDEX',
'HHFDVALU_INDEX')

eurekahedge_composite<- c(
                          'EHFI005_INDEX',
                          'EHFI007_INDEX',
                          'EHFI1_INDEX',
                          'EHFI10_INDEX',
'EHFI100_INDEX',
'EHFI102_INDEX',
'EHFI103_INDEX',
'EHFI104_INDEX',
'EHFI105_INDEX',
'EHFI106_INDEX',
'EHFI107_INDEX',
'EHFI109_INDEX',
'EHFI113_INDEX',
'EHFI114_INDEX',
'EHFI115_INDEX',
'EHFI116_INDEX',
'EHFI117_INDEX',
'EHFI12_INDEX',
'EHFI124_INDEX',
'EHFI125_INDEX',
'EHFI126_INDEX',
'EHFI128_INDEX',
'EHFI129_INDEX',
'EHFI13_INDEX',
'EHFI130_INDEX',
'EHFI133_INDEX',
'EHFI134_INDEX',
'EHFI135_INDEX',
'EHFI137_INDEX',
'EHFI139_INDEX',
'EHFI14_INDEX',
'EHFI140_INDEX',
'EHFI142_INDEX',
'EHFI143_INDEX',
'EHFI144_INDEX',
'EHFI145_INDEX',
'EHFI148_INDEX',
'EHFI15_INDEX',
'EHFI152_INDEX',
'EHFI154_INDEX',
'EHFI156_INDEX',
'EHFI16_INDEX',
'EHFI164_INDEX',
'EHFI165_INDEX',
'EHFI17_INDEX',
'EHFI173_INDEX',
'EHFI175_INDEX',
'EHFI18_INDEX',
'EHFI184_INDEX',
'EHFI188_INDEX',
'EHFI19_INDEX',
'EHFI190_INDEX',
'EHFI191_INDEX',
'EHFI192_INDEX',
'EHFI197_INDEX',
'EHFI199_INDEX',
'EHFI2_INDEX',
'EHFI20_INDEX',
'EHFI203_INDEX',
'EHFI204_INDEX',
'EHFI212_INDEX',
'EHFI213_INDEX',
'EHFI215_INDEX',
'EHFI221_INDEX',
'EHFI222_INDEX',
'EHFI225_INDEX',
'EHFI226_INDEX',
'EHFI240_INDEX',
'EHFI242_INDEX',
'EHFI244_INDEX',
'EHFI245_INDEX',
'EHFI247_INDEX',
'EHFI25_INDEX',
'EHFI250_INDEX',
'EHFI251_INDEX',
'EHFI252_INDEX',
'EHFI253_INDEX',
'EHFI254_INDEX',
'EHFI255_INDEX',
'EHFI258_INDEX',
'EHFI259_INDEX',
'EHFI261_INDEX',
'EHFI263_INDEX',
'EHFI265_INDEX',
'EHFI266_INDEX',
'EHFI268_INDEX',
'EHFI27_INDEX',
'EHFI270_INDEX',
'EHFI272_INDEX',
'EHFI282_INDEX',
'EHFI285_INDEX',
'EHFI286_INDEX',
'EHFI287_INDEX',
'EHFI288_INDEX',
'EHFI289_INDEX',
'EHFI3_INDEX',
'EHFI38_INDEX',
'EHFI4_INDEX',
'EHFI40_INDEX',
'EHFI45_INDEX',
'EHFI49_INDEX',
'EHFI5_INDEX',
'EHFI50_INDEX',
'EHFI51_INDEX',
'EHFI522_INDEX',
'EHFI529_INDEX',
'EHFI538_INDEX',
'EHFI54_INDEX',
'EHFI542_INDEX',
'EHFI544_INDEX',
'EHFI55_INDEX',
'EHFI57_INDEX',
'EHFI6_INDEX',
'EHFI60_INDEX',
'EHFI61_INDEX',
'EHFI64_INDEX',
'EHFI68_INDEX',
'EHFI69_INDEX',
'EHFI7_INDEX',
'EHFI78_INDEX',
'EHFI8_INDEX',
'EHFI80_INDEX',
'EHFI84_INDEX',
'EHFI85_INDEX',
'EHFI86_INDEX',
'EHFI9_INDEX',
'EHFI90_INDEX',
'EHFI91_INDEX',
'EHFI92_INDEX',
'EHFI93_INDEX',
'EHFI94_INDEX',
'EHFI95_INDEX',
'EHFI96_INDEX',
'EHFI98_INDEX',
'EHFI7_INDEX')


use_full_index <- c(hfri_composite,hfri_fof,tremont_composite,hennesy_composite,eurekahedge_composite)
market_index <- c('DLJLTR_INDEX','SPTR_INDEX','DLJHTR_INDEX','DLJWTRHU_INDEX','WHYITR_INDEX','VIX_INDEX',
                  'SPTRCOND_INDEX','SPTRCONS_INDEX','SPTRENRS_INDEX','SPTRFINL_INDEX','SPTRHLTH_INDEX','SPTRINDU_INDEX','SPTRINFT_INDEX','SPTRMATR_INDEX','SPTRTELS_INDEX','SPTRUTIL_INDEX',
                  'SPGCINTR_INDEX','SPGSENTR_INDEX','SPGSGRTR_INDEX','SPGSPMTR_INDEX','SPGCINTR_INDEX','SPGSCITR_INDEX',
                  'MMVALT_INDEX','MLCPT_INDEX','MSCP_INDEX',
                  'MXEUG_INDEX','MXMS_INDEX','MXEF_INDEX','MXLA_INDEX','MXME_INDEX','MXFMAF_INDEX','MXGB_INDEX','MXEUG_INDEX','MXAU_INDEX',
                  'RU30INTR_INDEX',
                  'LBUSTRUU_INDEX','LUATTRUU_INDEX','LD07TRUU_INDEX','LGCPTRUH_INDEX','LUACTRUU_INDEX','LF98TRUU_INDEX','LUGITRUU_INDEX'
                  )

bank_funding_index <- c('C0425Y_INDEX','C0435Y_INDEX','C0685Y_INDEX','C0705Y_INDEX','C0725Y_INDEX','C1525Y_INDEX','C1965Y_INDEX','C1975Y_INDEX','C1985Y_INDEX','C4995Y_INDEX','C5025Y_INDEX','C5205Y_INDEX','C5305Y_INDEX','C5315Y_INDEX','C5325Y_INDEX','C5335Y_INDEX')

#portfolio_id=8
#start='20101201'
#end='20111201'
#ignoreot=FALSE
#dates=portfolio_dates
#removeCash=TRUE
#field='class'


dbDateFmt <- function (date)
  {
    return(paste(substr(date,1,4),'-',substr(date,5,6),'-01',sep=''))
  }

get_portfolioRt <- function (con,portfolio_id, start, end='99991201',dates=c(),field='current_class',ignoreot=TRUE,supress=TRUE,use_class_proxy=FALSE,removeCash=FALSE)
{
  eq='1=1'
  if (length(dates))
    {
      start=dates[1]
      end=dates[length(dates)]
      ldates=paste(",",dates,"",sep='',collapse='')
      ldates=substr(ldates,2,nchar(ldates))
      eq=paste('year(dt)*100+month(dt) in (',ldates,')',sep='')
    } else
  {
    if (nchar(start)>0)
      {
        eq=paste("dt>'",dbDateFmt(start),"' and dt<='",dbDateFmt(end),"'",sep='')
      }
  }


  sql<-paste("select year(dt)*100+month(dt) dt,sum(allocation*rt) rt from allocation a where portfolio_id=",portfolio_id,"  and ",eq," group by dt order by dt",sep='') ;

  
  message(sql)
#  print(sqlQuery(con, sql))
  port_rt <- sqlQuery(con, sql)
  zprt <- zoo(port_rt$rt,port_rt$dt)

  if (removeCash)
    {
      message('Removing Cash Contribution:')
      sql<-paste("select year(dt)*100+month(dt) dt,sum(allocation*rt) rt from allocation a, fund f1, fund f2  where a.portfolio_id=",portfolio_id," and a.fund_id=f1.fund_id and f1.map_to_id=f2.fund_id and f2.model_after_id=309 and ",eq," group by dt order by dt",sep='') ;
      message(sql)
      cash_rawrt <- sqlQuery(con, sql)
      cash_rt<- zoo(cash_rawrt$rt,cash_rawrt$dt)
      zprt=zprt-cash_rt
    }

  if (supress) # cutoff the dates if portfolio haven't started there yet
    {
      dates=dates[dates>=index(zprt)[1]]
    }
  
  if (length(dates))
    {
     zprt <- sub_rt(zprt,dates,paste('PORTFOLIO ',portfolio_id))
    }

  
  sql<-paste("select ticker,name from portfolio where portfolio_id=",portfolio_id);
  sres <- sqlQuery(con, sql)
  name <- as.character(sres$name)
  ticker <- as.character(sres$ticker)

  sql <- 'drop table if exists tmp_alloc'
  sqlQuery(con,sql)

  sql <- paste('create temporary table tmp_alloc 
select a.fund_id, a.dt,  a.allocation, a.rt from allocation a, fund f, fund f1
where  portfolio_id=',portfolio_id,' and a.fund_id=f.fund_id and f.map_to_id=f1.fund_id')
  
#and f1.port_id is null
#
#union all
#
#select a1.fund_id, a.dt, a1.allocation*a.allocation allocation, a1.rt rt 
#from allocation a, allocation a1, fund f, fund f1
#where  a.portfolio_id=',portfolio_id,'and a.fund_id=f.fund_id and f.map_to_id=f1.fund_id
#and f1.port_id is not null and f1.port_id=a1.portfolio_id and a1.dt=a.dt')
    sqlQuery(con,sql)
    message(sql)
    

  message('GOING THROUGH CLASS RETURN')

  sql<-paste("select distinct f2.fund_id id, year(a.dt)*100+month(a.dt) dt, sum(a.allocation) alloc, sum(a.allocation*a.rt) rt
               from tmp_alloc a, fund f, fund f1, fund f2
               where ",eq," and f.fund_id=a.fund_id and f.map_to_id=f1.fund_id and f2.fund_id=f1.model_after_id
               group by f2.fund_id, a.dt",sep='') 
  clids <- sqlQuery(con, sql)

  if (removeCash)
    {
      clids$rt[clids$id==309]=0
    }

  sql<-paste("select distinct f2.fund_id id, f2.name,f2.hf_index,f2.class, f2.pm1,f2.",field," uclass, trim(upper(f2.story08)) story08, f2.ticker
               from tmp_alloc a, fund f, fund f1, fund f2
               where ",eq," and f.fund_id=a.fund_id and f.map_to_id=f1.fund_id and f2.fund_id=f1.model_after_id",sep='')
  ids <- sqlQuery(con, sql)

  ids$story08=as.character(ids$story08)
  ids$story08[is.na(ids$story08)]='UNDEFINED'

  res <- list(zprt=zprt,name=name,ids=ids,ticker=ticker)
  attr(res,'historical_returns')=zprt
  attr(res,'proforma_returns')=zprt
  attr(res,'h_ids')=ids
  attr(res,'clids')=clids

  return(res)
}


#portfolio_id=port_id
#field='class'
#dates=pdates
#len_proforma=36
#ignoreot=TRUE
#proforma_portfolio=FALSE
#dates=pdates
#use_class_proxy=TRUE
#portfolio_id=6
#dates=c(tomorrow)
#len_proforma=36
#


get_portfolioPRt <- function(con,ptr,portfolio_id, dates, field='current_class',len_proforma=24,ignoreot=TRUE,proforma_portfolio=FALSE,use_class_proxy=FALSE,cash_ovveride=TRUE)
  {

    sql<-paste("select name,ticker from portfolio where portfolio_id=",portfolio_id)
    port_name=sqlQuery(con, sql)
    if (nrow(port_name)!=1)
      {
        stop('Unknown portfolio ',portfolio_id)
      }

    orig_dates=dates

    if (proforma_portfolio)
      {
        sql=paste('select distinct year(dt)*100+month(dt) dt from allocation where portfolio_id=',portfolio_id)
        port_dates=sqlQuery(con, sql)
        dates=port_dates$dt
      }
    
    ldates=paste(",",dates,"",sep='',collapse='')
    ldates=substr(ldates,2,nchar(ldates))

    sql <- 'drop table if exists tmp_alloc'
    invisible(sqlQuery(con,sql))

    sql <- paste('create temporary table tmp_alloc 
select a.fund_id, a.dt,  a.allocation, a.gross_long, a.gross_short from allocation a, fund f, fund f1
where  portfolio_id=',portfolio_id,' and a.fund_id=f.fund_id and f.map_to_id=f1.fund_id
and f1.port_id is null

union all

select a1.fund_id, a.dt, a1.allocation*a.allocation allocation, a1.gross_long, a1.gross_short 
from allocation a, allocation a1, fund f, fund f1, fund f2 
where  a.portfolio_id=',portfolio_id,'and a.fund_id=f.fund_id and f.map_to_id=f1.fund_id and f1.model_after_id=f2.fund_id
and f2.port_id is not null and f2.port_id=a1.portfolio_id and a1.dt=a.dt')
    sqlQuery(con,sql)
    message(sql)
    
    
    sql<-paste("select distinct upper(f2.ticker) ticker, f2.fund_id id, f2.name, f2.",field," class
               from tmp_alloc a, fund f, fund f1, fund f2
               where year(a.dt)*100+month(a.dt)  in (",ldates,") and f.fund_id=a.fund_id and f.map_to_id=f1.fund_id and f2.fund_id=f1.model_after_id",sep='')
        
    ids <- sqlQuery(con, sql)
        message(sql)
    
    res <- list()
    if (nrow(ids))
      {

        sql<-paste("select f2.fund_id id, year(a.dt)*100+month(a.dt) dt,sum(a.allocation) allocation, sum(a.allocation*a.gross_long)/sum(a.allocation) gross_long, sum(a.allocation*a.gross_short)/sum(a.allocation) gross_short
               from tmp_alloc a, fund f, fund f1, fund f2
               where year(a.dt)*100+month(a.dt) in (",ldates,") and f.fund_id=a.fund_id and f.map_to_id=f1.fund_id and f2.fund_id=f1.model_after_id
               group by f2.fund_id,dt
               order by f2.fund_id,dt",sep='')
        
        allocs <- sqlQuery(con, sql)
        dates=sort(unique(allocs$dt))

        fund_rts <- list()
        use_port_id=portfolio_id
        if (proforma_portfolio)
          {
            use_port_id=0
          }

        i <- 12
        for (i in 1:length(ids$id))
          {
            message("PORT ID::",portfolio_id,' ','FUND ID::',ids$id[i])
                    
            if (ids$name[i]=='CASH' && cash_ovveride)
              {
                ticker = 'US0001M_INDEX'
                message('CASH RETURNS => ',ticker)
                rt <- get_benchmarks(con,tickers=c(ticker),0,'',FALSE,c(),TRUE)
              } else
            if (isnull(ids$ticker[i],'')=='BLUE HORIZON I')
              {
                message('BLUE HORIZON ==>>!!!')
                rt <- get_fund_rt(con,ptr,ids$id[i],-1,-1,use_port_id,0,-9999999999999999999999999999999999999,0,TRUE)
                rt[1:length(rt)]=0
              } else
            if (isnull(ids$ticker[i],'')=='CRECOVERY')
            {
              message('RECOVERY ==>> HFRIDSI_INDEX')
              rt <- get_benchmarks(con,tickers=c('HFRIDSI_INDEX'),0,'',FALSE,c(),TRUE)
            }
           else
             {
               rt <- get_fund_rt(con,ptr,ids$id[i],-1,-1,use_port_id,0,-9999999999999999999999999999999999999,0,TRUE)
               if (isnull(ids$ticker[i],'')%in% c('FARALLONLIQTRUST','FARALLONLIQTRUSTOFFSHORE'))
                 {
                   message('FARALLONLIQTRUST  ==>> HFRIDSI_INDEX for pre 2012 returns')
                   rt2 <- get_benchmarks(con,tickers=c('HFRIDSI_INDEX'),0,'',FALSE,c(),TRUE)
                   rt=rbind(rt2[index(rt2)<'201201'],rt[index(rt)>'201112'])
                 }
               
             }
            fund_rts=c(fund_rts,list(rt))
          }

       #get_fund_rt(con,ptr,ids$id[12],-1,-1,portfolio_id)
        #sum(allocs[allocs$dt=='200901',]$allocation)
        
        res=reclassifyRt(fund_rts,allocs,ids,dates,len_proforma,ignoreot,orig_dates,proforma_portfolio)
        attr(res,'dtallocs')=allocs
      }
    attr(res,'allocation')=ids
    attr(res,'name')=as.character(port_name$name[1])
    attr(res,'ticker')=as.character(port_name$ticker[1])
    attr(res,'portfolio_id')=portfolio_id
    return(res)
  }

#rts=fund_rts
#len=36
#

reclassifyRt <- function(rts,allocs,ids,dates,len=24,ignoreot=TRUE,orig_dates,proforma_portfolio)
  {
    res=list()
    lag <- 1         # use returns prior to the allocation
    zclass=zoo(0,dates)
    classes=sort(unique(as.character(ids$class)))
    if (length(classes)==1)
      {
        if (classes[1]!='CASH')
          {
            classes=c(classes,'CASH')
          } else
        {
          classes=c(classes,'EMPTY')
        }
      }
    classes=sort(classes)
    classAlloc=zclass;
    sum=0
    for (i in 1:length(classes))
      {
        if (i>1)
          {
            classAlloc=merge(classAlloc,zclass)
          }
      }

    if (length(classes)>1)
      {
        colnames(classAlloc)=classes;
      }
    classIds <- list()
    classRts <- list()

    message('Reclassify:Running')
    i <- 1
    for (i in 1:length(classes))
      {
        message('CLASS::',classes[i])
        id_list=which(ids$class==classes[i])
        classIds=c(classIds,list(id_list))
        clRts <- list()
        
        d <- 1
        for (d in 1:(length(dates)))
          {
            dt=dates[d]
            dtalloc=allocs[allocs$dt==dt,]
            ndx=which(agg_dates==dt)[1]
            if (isnull(ndx,0)==0)
              {
                stop('Unknown Date ',dt)
              }
            
            req_dates <- agg_dates[(ndx-len+1-lag):(ndx-lag)]
            
            if (proforma_portfolio)
              {
                tdt=orig_dates[length(orig_dates)]
                tndx=which(agg_dates==tdt)[1]
                if (isnull(tndx,0)==0)
                  {
                    stop('Unknown Date ',tdt)
                  }
                
                req_dates <- agg_dates[(tndx-len+1):tndx]
              }
            
            prt <- zoo(0,req_dates)
            dtalloc=allocs[allocs$dt==dates[d],]
            if (nrow(dtalloc)==0)
              {
                message('CLASS REORG::No allocations', classes[i],' on ',dates[d])
              } else
            {
              done=0
              j <- id_list[1]
              for (j in id_list)
                {
                  
                  andx=which(dtalloc$id==ids$id[j])[1]
                  if (isnull(andx,0)>0) # there is an allocation
                    {
                      classAlloc[d,i]=as.double(classAlloc[d,i])+dtalloc$alloc[andx]
                      if (classes[i]=='OT' || classes[i]=='VOL HEDGE')
                        {
                          message('CLASS REORG::Zeroed Returns', classes[i],' on ',dates[d])
                        } else
                      {
                        lrt <- rts[[j]]
                        frt <- sub_rt(lrt,req_dates,classes[i],TRUE,TRUE)

                        for (m in 1:length(prt))
                          {
                            prt[m]=as.double(prt[m])+as.double(frt[m])*as.double(dtalloc$alloc[andx])
                          }
                      }
                    }
                }
            }
            clRts <- c(clRts,list(prt))
          }
        classRts <- c(classRts,list(clRts))
      }

    i <- 1
    for (i in 1:nrow(classAlloc))
      {
        sm=sum(classAlloc[i,])
        if (abs(sm-1)>0.01 && sm!=0)
          {
            stop('Class Allocation is not summing up to 1 ',index(classAlloc)[i],' ',sm,'\n')
          }
      }
    
    res=list(classAlloc=classAlloc,classRts=classRts,ids=ids,rts=rts,dates=dates)
    return(res)
    
  }

#create class returns as standalone
#orealizedRt=realizedRt
#realizedRt=orealizedRt[[ndx]]
reconstructClassRt <- function(realizedRt,field='class')
  {
    cn=isnull(which(colnames(realizedRt$ids)==field)[1],0)
    if (!cn){
      stop('reconstructClassRt:: unable to locale field "',field,'"')
    }
      
     
    classes=sort(unique(as.character(realizedRt$ids[,cn])))
    clid=attr(realizedRt,'clids')
    dates=sort(unique(clid$dt))
    message('Reconstruct Class Returns::',realizedRt$name)
    zrt=zoo(0,dates)
    classRt=zoo()
    classSRt=zoo()
    classAlloc=zoo()
    
    class <- classes[1]
    for (class in classes)
      {
        message('Reconstruct:',class)
                 
        rt <- zrt
        at <- zrt
        srt <- zrt
        
        cids <- realizedRt$ids[realizedRt$ids[,cn]==class,]$id
        for (id in cids)
          {
            frt <- clid[clid$id==id,]
            dt <- dates[1]
            
            for (j in 1:length(dates))
              {
                dt=dates[j]
                ndx <- isnull(which(frt$dt==dt)[1],0)
                if (ndx>0)
                  {
                    rt[j]=as.double(rt[j])+frt$rt[ndx]
                    at[j]=as.double(at[j])+frt$alloc[ndx]
                  }
              }
          }

        for (j in 1:length(dates))
          {
            if(at[j]>0)
              {
                srt[j]=rt[j]/at[j]
              }
          }
        
        classRt=merge(classRt,rt)
        classSRt=merge(classSRt,srt)
        classAlloc=merge(classAlloc,at)
      }

    colnames(classRt)=classes
    colnames(classSRt)=classes
    colnames(classAlloc)=classes

    classCRt=classSRt*0

    #calculate cumulative standalone performance for each class

    for (j in 1:ncol(classCRt))
      {
        crt=1;
        for (i in 1:length(dates))
          {
            crt=crt*(1+as.double(classSRt[i,j]))
            classCRt[i,j]=crt
          }
      }
    

    for (i in 1:length(dates))
      {
        sm=sum(classAlloc[i,])
        if (sm<0.99 ||            sm>1.01)
          {
            stop("Class Allocation doesnt sum to 1! ",sm,' ',dates[i])
          }
      }
    
    res=list(classes=classes,classRt=classRt,classAlloc=classAlloc,classSRt=classSRt,classCRt=classCRt)
    return(res)
  }
  
#rts=portRts[[16]]
#date='200812'
proformaRt <- function(rts,date,closest=FALSE,prn=FALSE,outfile='')
  {
    ndx=isnull(which(rts$dates==date)[1],0)
    if (ndx==0)
      {
        message('Proforma Rt: Unable to find date ',date);
        if (!closest)
          {
            stop('')
          } else
        {
          ndx=isnull(which(rts$dates>date)[1],0)
          if (ndx==0)
            {
              stop('Proforma Rt: Unable to find dates above  ',date);
            }
          message('Proforma Rt: Using ',rts$dates[ndx])
        }
      }


    rt=list();
    
    i <- 1
    for (i in 1:(ncol(rts$classAlloc)))
      {
        
        if (i==1)
          {
            rt=rts$classRts[[i]][[ndx]]
          } else
        {
          j <- 1
          for (j in 1:length(rt))
            {
              rt[j]=as.double(rt[j])+as.double(rts$classRts[[i]][[ndx]][j])
              #if (length(rt)==j)
              #  {
                  #cat(colnames(rts$classAlloc)[i],'|',as.double(rts$classRts[[i]][[ndx]][j]),'\n')
              #  }
            }
        }
       # print(rt)
      }
    rt
  }

#date='200403'
projectRt <- function(rts,date,addon=-1)
  {
    dndx=isnull(which(rts$dates==date)[1],0)
    if (dndx==0)
      {
        message('Project Rt: Unable to find date ',date);
        stop('')
      }
    
    rt=list();
    ids=attr(rts,'allocation')
    allocs=attr(rts,'dtallocs')
    dtrt=rts$rts
    dtalloc=allocs[allocs$dt==date,]
    use_id=c()
    for (i in 1:nrow(dtalloc))
      {
        ndx=isnull(which(ids$id==dtalloc$id[i]),0)
        if (ndx<1)
          {
           
            message('projectRt::Unable to locate fund id for ',dtalloc$id[i],' FUND assumed to have 0 return - NEED TO BE FIXED !!!')
          } else
        {
          use_id=c(use_id,ndx)
        }
      }

    
    
    use_dates=rts$dates[dndx:(length(rts$dates)+addon)]
    prt=zoo(as.vector(array(0,length(use_dates))),as.vector(use_dates))

    for (i in 1:length(use_dates))
      {
        rrt=0
        j <- 1
        for (j in 1:length(use_id))
          {
            
            drt=dtrt[[use_id[j]]]
            ndx=isnull(which(index(drt)==use_dates[i])[1],0)
            if (ndx<1)
              {
                if (as.character(ids$name)[use_id[j]]!='Int Rate Swaps')
                  {
                    message('projectRt:: ',j,' Unable to locate return for fund id ',as.vector(ids$id)[use_id[j]],' ',as.character(ids$name)[use_id[j]],' date ',use_dates[i],' RETURN ASSUMED TO BE 0! -- NEED TO INVESTIGATE !!!')
                  }
              } else
            {
              rrt=rrt+as.double(dtalloc$allocation[j])*as.double(drt[ndx])
              dtalloc$allocation[j]=dtalloc$allocation[j]*(1+as.double(drt[ndx]))
            }
#            message(dtalloc$allocation[j],drt[ndx],'  ',rrt)
          }
        for (j in 1:length(use_id))
          {
            dtalloc$allocation[j]=dtalloc$allocation[j]/(1+rrt)
          }
        prt[i]=rrt
        
      }

    prt
  }

#rts=portRt
#dates=portRt$dates[1:length(portRt$dates)-1]
projectRts <- function(rts,dates)
  {
    prts=list()
    for (i in 1:length(dates))
      {
        message(i)
        prts=c(prts,list(projectRt(rts,dates[i])))
      }

    res=list(rts=prts,dates=dates)
    
  }




calclim <- function(zd)
{
  ylim <- c(0,0)
  for (i in 1:ncol(zd))
    {
      for (j in 1:ncol(zd))
        {
          if (ylim[1]>zd[i,j])
            {
              ylim[1]=zd[i,j]
            }
          
          if (ylim[2]<zd[i,j])
            {
              ylim[2]=zd[i,j]
            }
        }
    }
  ylim
}

calcSumlim <- function(zd)
{
  ylim <- c(0,0)
  for (i in 1:nrow(zd))
    {
      sm=sum(zd[i,])
      if (ylim[1]>sm)
        {
          ylim[1]=sm
        }
          
      if (ylim[2]<sm)
        {
          ylim[2]=sm
        }
    }
  ylim
}

plotBarText <- function(zData,scale=1,rnd=0,ylim=c(0,1))
{
  for (i in 1:nrow(zData))
    {
      sm <- 0
      for (j in 1:ncol(zData))
        {
          sm=sm+as.double(zData[i,j])
          y <- sm-as.double(zData[i,j]/2)
          if (abs(as.double(zData[i,j])/ylim[2])>0.05)
            {
              text(i-0.5,y,round(zData[i,j]*scale,rnd),srt=90,cex=0.7,font=2)
            }
          }
    }
}


lvgproject <- function(beta, benchmark, lvg, lvg_roll=12, lag=1,betalag=0,lvgfactor=1,cutoff_last=1)
  {

    dates=zoo()
    if (lvg_roll==0)
      {
        dates=index(beta)[(1+betalag):(length(beta)-cutoff_last)]
      }     else
      {
        lvg_index=lvg_roll+lag+1
        dates=index(lvg[lvg_index:(length(lvg)-cutoff_last)])
      }
    
    
    bindex=which(index(beta)==dates[1])[1]

    if (is.na(bindex))
      {
        stop("Can't find dates for Beta!")
      }
    
    
    ndata <- merge(beta,benchmark,all=TRUE)
    ndata <- ndata[1:(nrow(ndata)-cutoff_last)]
    ndata <- ndata[index(ndata)>=dates[1],]
    
    res=zoo(0,dates)
    nbetas=zoo(0,dates)

    i <- 1
    for (i in 1:nrow(ndata))
      {
        if (sum(is.na(ndata[i,]))>0)
          {
            stop('Inconsistent Returns while calculating projected betas ',index(ndata)[i])
          }

        nbeta=0
        if (lvg_roll==0)
          {
            nbeta <- as.double(beta[bindex+i-betalag-1])
          } else
        {
          nbeta <- lvgfactor*(as.double(lvg[lvg_index-lag+i-1])/mean(lvg[i:(lvg_index-lag+i-2)]))*as.double(beta[bindex+i-betalag-1])
        }
        nbetas[i]=nbeta
        res[i]=nbeta*as.double(ndata[i,2])
        
      }

    out=list(betas=nbetas,res=res)
    out
  }

replaceNA <- function (inzoo,replace=0)
  {
    out=inzoo

    if (!is.null(nrow(inzoo)))
      {
        for (i in 1:nrow(inzoo))
          {
            for (j in 1:ncol(inzoo))
              {
                if (is.na(inzoo[i,j]))
                  {
                    out[i,j]=replace
                  }
                
              }
          }
      } else
    {
      for (i in 1:length(inzoo))
          {
            if (is.na(inzoo[i]))
                  {
                    out[i]=replace
                  }
          }
    }
    

    out
  }
  
   
cumulativePerformance <- function (inzoo)
  {

    out=inzoo*0

    if (!is.null(nrow(inzoo)))
      {
        for (j in 1:ncol(inzoo))
          {
            cumulative=1
            for (i in 1:nrow(inzoo))
              {
                cumulative=cumulative*(1+as.double(inzoo[i,j]))
                out[i,j]=cumulative
              }
          }
      } else
    {
      cumulative=1
      for (i in 1:length(inzoo))
          {
            cumulative=cumulative*(1+as.double(inzoo[i]))
            out[i]=cumulative
          }
    }
    

    out

    
  }

field <- function (table,field)
  {
    index=which(table$id==field)[1]
    res=NA
    if (!is.na(index))
      {
        res=table$value[index]
      }
    res
  }

rangefield <- function (table,field)
  {
    index=which(table$id==field)[1]
    res1=NA
    res2=NA
    if (!is.na(index))
      {
        res1=as.double(as.character(table$value[index]))
        res2=as.double(as.character(table$value2[index]))
      }

    res=list(res1=res1,res2=res2)
    res
  }

rangetxtfield <- function (table,field)
  {
    index=which(table$id==field)[1]
    res1=NA
    res2=NA
    if (!is.na(index))
      {
        res1=as.character(table$value[index])
        res2=as.character(table$value2[index])
      }

    res=list(res1=res1,res2=res2)
    res
  }



fields <- function (table,field,removezero=FALSE)
  {

    res <- zoo()

    if (!is.na(field))
      {
    
        index= table$id==field
        res <- zoo()
        
        
        if (length(index)>0)
          {
            dt=table$dt[index]
            val=table$value[index]
            res <- zoo(val,dt)
          }
      }

    if (removezero && length(res)>0)
      {
        res=res[res!=0]
      }
    res
  }


field <- function (table,field)
  {
    index=which(table$id==field)[1]
    res=NA
    if (!is.na(index))
      {
        res=table$value[index]
      }
    as.double(res)
  }

lastfield <- function (fields)
  {
    res <- list(
                dt=index(fields)[length(fields)],
                val=as.double(fields[length(fields)])
                )
    res
     
  }

dtfield <- function (fields,dt)
  {
    index=which(index(fields)==dt)[1]
    res=NA
    if (!is.na(index))
      {
        res=as.double(fields[index])
      }

    res

  }

addlist <- function(list,element,nonzero=1)
  {
    
    if (!is.na(element) && nchar(element)>0)
      {
        if (nonzero)
          {
            if (element!=0)
              {
                list=c(list,element)
              }
          } else
        {
            list=c(list,element)
        }
      }
    list
  }

addchange <- function(list1,element,list2)
  {
    if (!is.na(element) && length(list1)>0 && nchar(element)>0 && element!=0)
      {
        list2=c(list2,element/list1[length(list1)]-1)
      }
    
    list2
  }



calc_change <- function(lst,element,depth,limit,rel=0)
  {
    val=NA
    excess1=NA
    excess2=NA

    if (length(lst)>= depth && !is.na(element))
      {
        val=element/lst[length(lst)-depth+1]-1
        if (rel)
          {
            val=element-lst[length(lst)-depth+1]
          } else
        {
          if (lst[length(lst)-depth+1]!=0)
            {
              val=element/lst[length(lst)-depth+1]-1
            }
        }
        excess1=max(val-limit,0)
        excess2=max(limit-val,0)    
      }

    res=list(val=val,excess1=excess1,excess2=excess2)
    res
    
  }

calc_changerange <- function(lst,element,depth,limit,rel=1)
  {
    val=NA
    excess=NA

    if (length(lst)>= depth && !is.na(element))
      {
        if (lst[length(lst)-depth+1]!=0)
          {
            val=element/lst[length(lst)-depth+1]-1
            excess=maxnull(limit$res1-val,val-limit$res2,0)
          }
      }

    res=list(val=val,excess=excess)
    res
    
  }


isnull <- function(value,isna=0,isnotval=value)
  {
    res=isna
    if (length(value)>0 && !is.na(value))
      {
        res=isnotval
      }
    res
  }

maxnull <- function(a,b,c=0)
{
   res=max(isnull(a),isnull(b),isnull(c))
   res
}

fmt <- function (raw)
  {
  rs <- gsub('_','\\\\_',raw)
  rs <- gsub('\\&','\\\\&',rs)
  rs <- gsub('\\$','\\\\$',rs)
  rs <- gsub('>','\\\\textgreater ',rs)
  rs <- gsub('<','\\\\textless ',rs)
  rs <- gsub('%','\\\\\\%',rs)
  rs <- gsub('#','\\\\\\#',rs)
  rs <- gsub('\\^','\\\\\\^',rs)
  
  
  rs
}

#for use in sweave
sfmt <- function (raw)
  {
    rs <- gsub('\\\\','\\\\\\\\',fmt(raw))
    return(rs)
  }

#val=0.01
#fmt='2%'

fmt_val <- function(val,fmt='')
  {
    fmt=isnull(fmt,'')
    if (nchar(as.character(val))==0 || is.na(val) || val =='NA')
      {
        'NA'
      } else
    {
      if (fmt=='$')
        {
          res=''
          l=sprintf('%.0f',val)
          while (nchar(l)>0)
            {
              if (nchar(l)<3)
                {
                  rl=l
                }
              else
                {
                  rl=substr(l,nchar(l)-2,nchar(l))
                }
              
              if (nchar(res)>0)
                {
                  res=paste(rl,',',res,sep='')
                } else
              {
                res=rl
              }
              
              if (nchar(l)<3)
                {
                  l=''
                } else
              {
                l=substr(l,1,nchar(l)-3)
              }
              
            }
          res
        }
      else
        if (fmt=='' || fmt=='N')
          {
            sprintf('%.0f',as.double(val))
          } else
        if (fmt=='F')
          {
            sprintf('%.2f',as.double(val))
          } else
      if (fmt=='BPS')
        {
          sprintf('%.0f',as.double(val)*10000)
        } else
      if (fmt=='TXT')
        {
         as.character(val)
        } else
      if (fmt=='%%')
        {
          paste(sprintf('%.1f',as.double(val)*100),sep='')
        } else
      if (fmt=='%%%')
        {
          paste(sprintf('%.0f',as.double(val)*100),'%',sep='')
        } else
      if (fmt=='%%%%')
        {
          paste(sprintf('%.0f',as.double(val)*100),sep='')
        } else
      if (fmt=='2%')
        {
          paste(sprintf('%.2f',as.double(val)*100),sep='')
        } else      
      if (fmt=='FF')
        {
          paste(sprintf('%.4f',as.double(val)),sep='')
        } else
      
      {
        paste(sprintf('%.2f',as.double(val)*100),'%',sep='')
      }
       
    }
    
  }

fmtVal <- function(val,fmt='')
  {
    fmt(fmt_val(val,fmt))
  }

fmtValArr <- function(val,fmt='')
  {
    res=c()
    for (vv in val)
      {
        res=c(res,fmt(fmt_val(vv,fmt)))
      }
    res
  }




setNA <- function(z)
  {
    for (i in 1:length(z))
      {
        z[i]=NA
      }

    z
  }

prevVal <- function (val,list)
{
  vl=val
  if (is.null(val) || is.na(val) || val ==0 )
    {
      if (length(list)>0)
        {
          vl=list[length(list)]
        }
    }
 
  vl
}


addframe <- function (frame, add,no_update=0)
  {
   # message(add$id)
   index=isnull(which(as.character(frame$id)==as.character(add$id))[1],0)
    if (index==0) # an update, not an addon
      {
        frame=data.frame(
          id=c(as.character(frame$id),as.character(add$id)),
          value=c(as.vector(frame$value),isnull(add$res1,NA)),
          value2=c(as.vector(frame$value2),isnull(add$res2,NA)),
          style=c(as.vector(frame$style),isnull(as.character(add$style),'')),
          fmt=c(as.vector(frame$fmt),isnull(as.character(add$fmt),'%'))
          )
      } else
      {
        if (!no_update)
          {
            frame$value[index]=add$res1
            frame$value2[index]=add$res2
          }
      }

   frame

  }

#frame=flags
#addlist=common_flags

addframes <- function (frame, addlist,no_update=0)
{
  new_frame=frame

  if (nrow(addlist)>0)
    {
      i <- 1
      for (i in 1:nrow(addlist))
        {
          add=addlist[i,]
          new_frame=addframe(new_frame,add,no_update)
        }
    }

  new_frame
  
  
}

# full=r_percentiles
#addon=vdt
addpercentile <- function(full,addon)
  {
    names <- attr(attributes(addon),'')
    name <- names[1]
    added_names <- c()
    for (name in names)
      {
        val <- attr(addon,name)
        if (!is.null(val) && !is.na(val))
          {
            ndx <- isnull(which(full$id==name)[1],0)
            added_names <- c(added_names,name)
            if (ndx==0 || isnull(length(full$id),0)==0)
              {
                full$id=c(full$id,name)
                full$val=c(full$val,list(val))
              } else
            {
              full$val[[ndx]]=c(full$val[[ndx]],val)
            }

          }
      }

    #repeat values that were missing 

    for (i in 1:length(full$id))
      {
        ndx <- isnull(which(added_names==full$id[i])[1],0)
        
        if (ndx==0) # value wasn't added
          {
            full$val[[i]]=c(full$val[[i]],full$val[[i]][length(full$val[[i]])])
          }
      }

    full
  }

prnlimit <- function(table1,field)
  {
    res=rangefield(table1,field)

    res$res1=isnull(res$res1,'')
    res$res2=isnull(res$res2,'')

    res

  }

prntxtlimit <- function(table1,field)
  {
    res=rangetxtfield(table1,field)

    res$res1=isnull(res$res1,'')
    res$res2=isnull(res$res2,'')

    res

  }


attrval <- function(vl,name)
  {
    res=NA
    if (sum(names(attributes(vl))==as.character(name)))
      {
        res=attr(vl,name)
      }
    
    res
  }


cmp_txt <- function (order, a, b)
  {

    if (isnull(a,'')=='' || isnull(b,'')=='')
      {
        0
      }
    else
      {
        i1=which(order==a)[1]
        i2=which(order==b)[1]
        
        if (isnull(i1,0)==0 || isnull(i2,0)==0)
      {
        messages("Values to be compared (",a,",",b,") cant be found during comparison")
        -1
      }
        else
          {
            i2-i1
          }
      }
  }


prepare_allocations <- function (con,field, portfolio_id)
{

  junk <- sqlQuery(con, "drop table if exists tmp_alloc")
  junk <- sqlQuery(con, paste("create temporary table tmp_alloc
SELECT  a.dt,  a.allocation, a.rt, a.leverage, a.gross_long, a.gross_short, f1.model_after_id fund_id, f.map_to_id 
from allocation a, fund f, fund f1
where a.fund_id=f.fund_id and f1.port_id IS NULL and f.map_to_id=f1.fund_id  and a.portfolio_id=",portfolio_id," 

union all

select  a.dt,a.allocation*a1.allocation allocation, a.rt, a1.leverage, a1.gross_long, a1.gross_short, f3.model_after_id fund_id,  f2.map_to_id 
from   allocation a, allocation a1, fund f1, fund f1a, fund f2, fund f3
where a.fund_id  = f1.fund_id and f1.map_to_id=f1a.fund_id and f1a.port_id is not NULL and a1.portfolio_id=f1a.port_id  and a.portfolio_id=",portfolio_id," 
  and a.dt=a1.dt and a1.fund_id=f2.fund_id and f2.map_to_id=f3.fund_id",sep=''))


 junk <- sqlQuery(con, "create index tmp_alloc_ndx1 on tmp_alloc (dt,fund_id)");
 junk <- sqlQuery(con, "create index tmp_alloc_ndx2 on tmp_alloc (dt,map_to_id)");

 junk <- sqlQuery(con, "drop table if exists tmp_alloc_agg")
 junk <- sqlQuery(con, "create temporary table tmp_alloc_agg
SELECT dt,  fund_id, sum(allocation) allocation,sum(rt*allocation)/sum(allocation) rt,
  sum(leverage*allocation)/sum(allocation) leverage, sum(allocation*gross_long)/sum(allocation) gross_long,
   sum(allocation*gross_short)/sum(allocation) gross_short
from tmp_alloc a
group by dt, fund_id")



 sql<-paste("SELECT distinct f.",field," class from fund f, tmp_alloc_agg a
             where f.fund_id=a.fund_id
             order by f.",field,"
            ",sep='')

 classes<- as.character(sqlQuery(con, sql)$class)

 classes 

}


#zrt=cbenchmarks[,bndx]
#dates=index(y)
#fill=TRUE


sub_rt <- function (zrt,dates,ticker='',fill=FALSE,suppress=FALSE)
  {
    trt=zoo(0,dates)
    i <- 1
    for (i in 1:length(dates))
      {
     ndx=isnull(which(dates[i]==index(zrt))[1],0)
        if (ndx==0)
          {
            msg=paste('SUBRT::Inconsistent returns  unable to locate return for ',dates[i],' Ticker:',ticker)
            if (fill)
              {
                if (!suppress)
                  {
                    message(msg)
                  }
                trt[i]=0
              } else
            {
              if (!suppress)
                {
                  stop (msg)
                } else
              {
                trt[i]=NA
              }
              
            }
          } else
        {
          trt[i]=zrt[ndx]
        }
      }
    trt
  }
###

level1=0.69
level2=0.49
pvalue_level=0.05

cl_portfolio_id <- '12,9,7,15,8,6,11,10,18,14,13'

####
#fields=c('expl')
#start_dt=paste(today,'01',sep='')

get_latest_attributes <- function (con, field, start_dt)
{
  
  
  sql=paste('drop table if exists mx',sep='');
  invisible(sqlQuery(con, sql))
  
  sql=paste('create temporary table mx 
        select fund_id, max(dt) dt 
        from fund_attribute fa, tag where tag.id=fa.tag_id and tag.name="',field,'" group by fund_id',sep='');
  invisible(sqlQuery(con, sql))

  sql=paste('drop table if exists mx_value',sep='');
  invisible(sqlQuery(con, sql))
     
     
  sql=paste('create temporary table mx_value 
        select f2.model_after_id fund_id, mx.dt, fa.value from mx, fund_attribute fa, tag, fund f1, fund f2 
            where tag.id=fa.tag_id and tag.name="',field,'" and fa.dt=mx.dt and fa.fund_id=mx.fund_id
            and fa.fund_id=f1.fund_id and f1.map_to_id=f2.fund_id',sep='')
     invisible(sqlQuery(con, sql))
  
  sql='select mx.fund_id,fund.name, year(mx.dt)*100+month(mx.dt) dt, mx.value from mx_value mx, fund where mx.fund_id=fund.fund_id'
  vals=sqlQuery(con, sql)

   sql=paste('select distinct f3.fund_id, f3.name
        from fund f1, fund f2, fund f3, allocation  a where
     a.fund_id=f1.fund_id and f1.map_to_id=f2.fund_id and f2.model_after_id=f3.fund_id
      and a.dt>"',start_dt,'"
     and  not exists (select * from mx_value mx where f3.fund_id=mx.fund_id)
    ',sep='')

   nodata=sqlQuery(con, sql)

   res=list(vals=vals,nodata=nodata)
                                      
}

get_attributes <- function (con, fields,nonzero=FALSE)
{
  lfields=paste(",'",fields,"'",sep='',collapse='')
  lfields=substr(lfields,2,nchar(lfields))
  addon=''
  if (nonzero)
    {
        addon=' and fa.value <>0'
    }
  sql=paste('
        select f2.model_after_id fund_id, year(fa.dt)*100+month(fa.dt) dt, upper(trim(tag.name)) name, fa.value 
        from fund_attribute fa, tag, fund f, fund f2
        where tag.id=fa.tag_id and tag.name in (',lfields,')
          and fa.fund_id=f.fund_id and f.map_to_id=f2.fund_id',addon,'
        order by fa.dt desc
')
  vals=sqlQuery(con, sql)
  
}

prn_checks <- function (con,checks,today) 
{
  tndx=which(agg_dates==today)[1]
  funds=c()
  labels=c()
  prn='|p{4cm}'
  ncol=1;
  for (i in 1:length(checks))
    {
      funds=unique(union(union(funds,checks[[i]]$data$nodata$fund_id),checks[[i]]$data$vals$fund_id));
      labels=c(labels,checks[[i]]$label)
      prn=paste(prn,'|c|c',sep='')
      ncol=ncol+2
    }
  
  
  funds=paste(",",funds,"",sep='',collapse='')
  funds=substr(funds,2,nchar(funds))

  sql=paste('select fund_id, ifnull(ticker,name) name, current_class from fund where fund_id in (',funds,') order by name')
  names=sqlQuery(con,sql)

  cat ('\\begin{longtable}{',prn,'|}')
  cat ('\\multicolumn{',ncol,'}{c}{{\\bfseries \\tablename\\ \\thetable{} Fund Data used in Reports}} \\\\\\hline\n')
  cat ('\\bfseries Name',paste('&\\multicolumn{2}{|c|}{\\bfseries ',labels,'}',sep='',collapse=''),' \\\\\\hline\n')
  cat ('\\endfirsthead\n')
  cat ('\\multicolumn{',ncol,'}{c}{{\\bfseries \\tablename\\ \\thetable{} Fund Data used in Reports (continued)}} \\\\\\hline\n')
  cat ('Name',paste('&\\multicolumn{2}{c}{',labels,'}',sep='',collapse=''),' \\\\\\hline\n')
  cat ('\\endhead\n')
  cat ("\\hline \\multicolumn{",ncol,"}{|r|}{{Continued on next page}} \\\\ \\hline\n")
  cat ('\\hline\n\\endfoot\n')    
  cat ('\\hline\n\\endlastfoot\n')

  i <- 1
  for (i in 1:nrow(names))
    {
      if (isnull(as.character(names$current_class[i]),'')!= 'RUNOFF')
        {
        
          cat(fmt(names$name[i]));
          j <- 1
          for (j in 1:length(checks))
            {
              ndx=isnull(which(checks[[j]]$data$vals$fund_id==names$fund_id[i])[1],0)
              if (ndx>0)
                {
                  dndx=which(agg_dates==checks[[j]]$data$vals$dt[ndx])[1]
                  prn=fmt(checks[[j]]$data$vals$dt[ndx])
                  if (tndx-dndx>3)
                    {
                      prn=paste('\\color{red}{',prn,'}')
                    } else
                  if (tndx-dndx>2)
                    {
                      prn=paste('\\color{blue}{',prn,'}')
                    }

                  prn2=fmt(round(checks[[j]]$data$vals$value[ndx],2))
                  if (checks[[j]]$data$vals$value[ndx] <= 0)
                    {
                      prn2=paste('\\color{red}{',prn2,'}')
                    } else
                  if (checks[[j]]$data$vals$value[ndx]<1)
                    {
                      prn2=paste('\\color{blue}{',prn2,'}')
                    }
                  
                  cat('&',prn,'&',prn2);
            }
              else
                {
                  cat('&\\color{red}{NA}&\\color{red}{NA}')
                }
            }
          cat ('\\\\\\hline\n')
          
        }
    }

  cat ('\\end{longtable}\n\\clearpage\n')
      
}


#market_rt=ml_indexes
#windows=c(36)
#ytd=FALSE
#rt=rt1
rt_stats <- function(rt,windows,ml_indexes,ytd=TRUE,itd=FALSE)
  {
    ret=c()
    vol=c()
    drawdown=c()
    full_betas=c()
    full_pvalues=c()
    
    use_start=c()
    ytd_start=1
    if (ytd)
      {
        ytd_date=(as.integer(substr(index(rt)[length(rt)],1,4))-1)*100+12
        ytd_start=which(index(rt)>ytd_date)[1]
      }

    if (itd)
      {
        use_start=c(use_start,1)
      }

    win=windows[1]
    for (win in windows)
      {
        start=1
        if (win>0 && length(rt)>=win)
          {
            start=length(rt)-win+1
          }
        use_start=c(use_start,start)
      }

    if (ytd)
      {
        use_start=c(use_start,ytd_start)
      }

    start=use_start[1]
    for (start in use_start)
      {
        use_rt=rt[start:length(rt)]
        dates <- agg_dates[agg_dates <= index(use_rt)[length(use_rt)]]
        dates <- dates[dates>= index(use_rt)[1]]

        if (length(dates)!=length(use_rt) || sum(dates!=index(use_rt)))
          {
            stop('rt_stats:: Inconsistent returns -- gaps in the return stream')
          }


        if (length(use_rt)<12)
          {
            ret=c(ret,Return.cumulative(as.vector(use_rt)))
          } else
          {
            ret=c(ret,isnull(Return.annualized(as.vector(use_rt),scale=12),-1))
          }
        
        
        vol=c(vol,isnull(as.double(sd.annualized(as.vector(use_rt),scale=12)),0))
        drawdown=c(drawdown,isnull(as.double(maxDrawdown(as.vector(use_rt))),0))

        betas <- c()
        pvalues <- c()

        j <- 1
        for (j in 1:ncol(ml_indexes))
          {
            market_rt=sub_rt(ml_indexes[,j],dates)
            slope=coef(summary(lm(use_rt~market_rt)))
            if (length(slope)>4)
              {
                betas <- c(betas,isnull(slope[2,1],0))
                pvalues <- c(pvalues,isnull(slope[2,4],1))
              } else
              {
                betas <- c(betas,0)
                pvalues <- c(pvalues,1)
              } 
          }

        full_betas=c(full_betas,list(betas))
        full_pvalues=c(full_pvalues,list(pvalues))
      }

    res <- list(ret=ret,vol=vol,drawdown=drawdown,betas=full_betas,p_values=full_pvalues)
    res
  }

#len=36

#
rtMatrix <- function(portRts,len,today,tomorrow)
  {
    dates <- agg_dates[agg_dates<tomorrow]
    dates <- dates[(length(dates)-len+1):length(dates)]
    dtallocs <- attr(portRts,'dtallocs')
    dndx <- isnull(which(index(portRts$classAlloc)==today)[1],0)
    if (!dndx)
      {
        stop ('rtMatrix: unable to find info as of ',today);
      }

    classAllocs <- c()
    fundAllocs <- c()
    fundIds <- c()
    classRt <- c()
    fundRt <- c()
    fundNames <- c()
    classNames <- c()
    classres=zoo()

    classnames <- colnames(portRts$classAlloc)
    classIds <- c()
    mres=zoo()
    i <- 3
    for (i in 1:length(classnames))
      {
        alloc <- as.double(portRts$classAlloc[dndx,i])
        if (alloc!=0)
          {
            rt <- portRts$classRts[[i]][[dndx]]
            rt <- sub_rt(rt,dates)
            for (m in 1:length(rt))
              {
                rt[m] <- as.double(rt[m])/alloc 
              }
            classAllocs <- c(classAllocs,alloc)
            classRt <- c(classRt,list(rt))
            classres <- merge(classres,rt)
            classNames <- c(classNames,classnames[i])
            
            clIdx=which(portRts$ids$class==classnames[i])
            if (length(clIdx)==0)
              {
                stop('prnPortfolioFundPerformance: unable to locate funds from ',classnames[i],' class')
              }
            
            j <- 9
            for (j in clIdx)
              {
                id <- portRts$ids$id[j]
                fundAlloc <- dtallocs[dtallocs$id==id,]
                allocNdx <- isnull(which(fundAlloc$dt==today)[1],0)
                message("rtMatrix:: Processing ",portRts$ids$name[j])
                if (allocNdx>0 && fundAlloc$allocation[allocNdx]!=0)
                  {
                    rt <- portRts$rts[[j]]
                    rt <- sub_rt(rt,dates)
                    fundIds <- c(fundIds,id)
                    fundRt <- c(fundRt,list(rt))
                    fundNames <- c(fundNames,as.character(portRts$ids$name[j]))
                    fundAllocs <- c(fundAllocs,fundAlloc$allocation[allocNdx])
                    classIds <- c(classIds,length(classNames))
                    mres=merge(mres,rt)
                  }
                
              }
                
            
          }  
      }


    if (length(fundNames)>1)
      {
        colnames(mres)=fundNames
      }

    if (length(classNames)>1)
      {
        colnames(classres)=classNames
      }
    
    res <- list(classIds=classIds,classRt=classRt,fundRt=fundRt,fundIds=fundIds,classNames=classNames,fundNames=fundNames,classAllocs=classAllocs,fundAllocs=fundAllocs,mRes=mres,cRes=classres)
    res
  }



portRiskStat <- function(rt,allocation,windows,level=0.05)
  {
    za <- qnorm(level)
    res=c()

    for (win in windows)
      {
        cov <- array(0,c(length(rt),length(rt)))
        dvar <- array(0,length(rt))
        start=length(rt[[1]])-win+1
        end=length(rt[[1]])
        portRt <- array(0,end-start+1)
        for (i in 1:length(rt))
          {
            portRt=portRt+as.vector(rt[[i]][start:end])*allocation[i]
            for (j in 1:length(rt))
              {
                cov[i,j]=var(rt[[i]][start:end],rt[[j]][start:end])
              }
          }

        portS=sd(portRt)
        portVaR=-(mean(portRt)+za*portS)
        fundVaR=c()
        mVaR=c()
        beyondVaR=c()
        portCor=c()
        art=c()
        asd=c()
        for (i in 1:length(rt))
          {
            dvar[i]=-(mean(rt[[i]][start:end])+za/portS*as.double(allocation%*%cov[i,]))*allocation[i]
            fundVaR=c(fundVaR,-(mean(rt[[i]][start:end])+za*sd(rt[[i]][start:end])))
            mportRt=portRt-as.vector(rt[[i]][start:end])*allocation[i]
            mportRt=mportRt/(1-allocation[i])
            mportVaR=-(mean(mportRt)+za*sd(mportRt))
            mVaR=c(mVaR,portVaR-mportVaR)
            beyondVaR=c(beyondVaR,ES(as.vector(rt[[i]][start:end]),method='gaussian',p=level))
            portCor=c(portCor,cor(portRt,rt[[i]][start:end]))
            art=c(art,Return.annualized(as.vector(rt[[i]][start:end]),scale=12))
            asd=c(asd,sd.annualized(as.vector(rt[[i]][start:end]),scale=12))
          }
        cvar=dvar/portVaR
        res <- c(res,list(list(cvar=cvar,fundVaR=fundVaR,mVaR=mVaR,art=art,asd=asd,portCor=portCor,portVaR=portVaR,beyondVaR=beyondVaR,
                               beyondPortVaR=ES(as.vector(portRt),method='gaussian',p=level)

                              )))
      }
    res
  }


#matrix=rMatrix$cRes
#allocation=rMatrix$classAllocs
#windows=c(36,24,12)
#level=0.05
#fit.func=fit.tuv
#portRiskStatGH <- function(matrix,allocation,windows,level=0.05,delta=0.0001,fit.func=fit.ghypuv)
#  {
#    res=c()#
#
#    n=isnull(ncol(matrix),1)
#    win <- windows[1]
#    for (win in windows)
#      {
#        start=length(matrix[,1])-win+1
#        end=length(matrix[,1])
#        uMatrix=matrix[start:end,]
#        portRt=zoo(uMatrix%*%allocation,index(uMatrix))
#        fit.mv <- fit.func(as.vector(portRt),silent=TRUE)
#        portVaR=qghyp(level,fit.mv)
#        portVaRuse=ghyp.fit.info(fit.mv)$converged
#        fundVaR=c()
#        fundVaRuse=c()
        
#        mVaR=c()
#        mVaRuse=c()
#        beyondVaR=c()
#        beyondVaRuse=c()
#        portCor=c()
#        dvar <- array(0,n)
#        cVaR=c()
#        cVaRuse=c()
#        
#
#        i <- 1
#        for (i in 1:n)
#          {
            #Fund VaR
#            fit.fmv <- fit.func(uMatrix[,i],silent=TRUE)
#            fundVaRuse=c(fundVaRuse,ghyp.fit.info(fit.fmv)$converged)
#            fVaR=qghyp(level,fit.fmv)
#            fundVaR=c(fundVaR,fVaR)
#            beyondVaR=c(beyondVaR,ESghyp(level,fit.fmv)-fundVaR)
#            portCor=c(portCor,as.double(cor(portRt,uMatrix[,i])))

            #marginal VaR
#            mportRt=portRt-as.vector(uMatrix[,i])*allocation[i]
#            mportRt=mportRt/(1-allocation[i])
#            fit.mmv <- fit.func(mportRt,silent=TRUE)
#            mVaR=c(mVaR,portVaR-qghyp(level,fit.mmv))
#            mVaRuse=c(mVaRuse,ghyp.fit.info(fit.mmv)$converged)#

            #Component VaR, use 1bps for derivative calcs
#            cvarConverged=portVaRuse
#            UpmportRt=mportRt*(1-allocation[i]-delta)+as.vector(uMatrix[,i])*(allocation[i]+delta)
#            fit.mmv <- fit.func(UpmportRt,silent=TRUE)
#            cvarConverged=cvarConverged && ghyp.fit.info(fit.mmv)$converged
#            UVaR=qghyp(level,fit.mmv)

#            DownmportRt=mportRt*(1-allocation[i]+delta)+as.vector(uMatrix[,i])*(allocation[i]-delta)
#            fit.mmv <- fit.func(DownmportRt,silent=TRUE)
#            cvarConverged=cvarConverged && ghyp.fit.info(fit.mmv)$converged
#            DVaR=qghyp(level,fit.mmv)#

#            cVaR=c(cVaR,(UVaR-DVaR)/(2*delta)*allocation[i]/portVaR)
#            cVaRuse=c(cVaRuse,cvarConverged)
#          }
#        res <- c(res,list(list(cvar=cvar,fundVaR=fundVaR,mVaR=mVaR,portCor=portCor,portVaR=portVaR,beyondVaR=beyondVaR,beyondPortVaR=VaR.Beyond(portRt,p=1-level))))
#      }
#    res
# }


#es <- list(classRt=classRt,fundRt=fundRt,fundIds=fundIds,classNames=classNames,fundNames=fundNames,classAllocs=classAllocs,fundAllocs=fundAllocs)

portfolioRiskStat <- function(rMatrix,windows,level=0.05)
  {
    res <- list(
                classStat=portRiskStat(rMatrix$classRt,rMatrix$classAllocs,windows,level),
                fundStat=portRiskStat(rMatrix$fundRt,rMatrix$fundAllocs,windows,level)
                )
    res
  }


fmtColors <- function (val,levels,colors)
{
  res=''
  for (i in 1:length(levels))
    {
      if (val<levels[i])
        {
          res=paste('\\color{',colors[i],'}',sep='')
          break
        }      
    }
  res
}

fmtInvColors <- function (val,levels,colors)
{
  res=''
  for (i in 1:length(levels))
    {
      if (val>levels[i])
        {
          res=paste('\\color{',colors[i],'}',sep='')
          break
        }      
    }
  res
}

#frame=hfrx_rt
#maxCol=maxPage
breakFrame <- function(frame,maxCol)
  {
    res=c()
    j <- 1

    while (j<ncol(frame)+1)
      {
        message(j)
        end <- min(j+maxCol-1,ncol(frame))
        res <- c(res,list(frame[,j:end]))
        j <- end+1
      }
    res
    
  }

#frame=source_rt
#lag=1

changeFrame <- function(frame,lag)
  {
    res <- frame[(lag+1):nrow(frame),]
    if (lag!=0)
      {
        for (i in nrow(res):1)
          {
            res[i,] <- as.vector(frame[i+lag,])-as.vector(frame[i,])
          }
      }
    res
  }


subSpace <- function(txt)
  {
    res=txt
    if (nchar(txt)>1)
      {
        for (i in 2:nchar(txt))
          {
            if (substr(txt,i,i)==' ')
              {
                res=substr(txt,1,i-1)
                break
              }
            
          }
      }
    res
  }

get_quantiles <- function(rt,quantiles)
  {
    zt <- zoo(0,1:length(rt))
    res <- zoo()
    for (j in 1:length(quantiles))
      {
        res <- merge(res,zt)
      }

    colnames(res)=quantiles

    for (i in 1:length(rt))
      {
        for (j in 1:length(quantiles))
          {
            res[i,j]=quantile(rt[[i]],quantiles[j],na.rm=TRUE)
          }
      }

    res
  }

#calculate firm balance sheet leverage, for standalone and full basis
#ids=names
calc_portLeverage <- function (portLeverage,ids,dates)
  {
    res=list()
    id <- ids[1]
    for (id in ids)
      {
        leverage=portLeverage[portLeverage$ticker==id,]
        aumLine=c()
        lvgLine=c()
        for (dt in dates)
          {
            dtLine=leverage[leverage$dt==dt,]
            if (nrow(dtLine)>0)
              {
                aumLine=c(aumLine,isnull(as.double(dtLine$aum[1]),NA))
                lvgLine=c(lvgLine,isnull(as.double(dtLine$leverage[1]),NA))
              } else
            {
              aumLine=c(aumLine,NA)
              lvgLine=c(lvgLine,NA)
            }
          }
        Line=list(name=id,lvg=lvgLine,aum=aumLine)
        res=c(res,list(Line))
      }
    return (res)
  }

#-----

CL_returns_regimes<- function (yraw,xraw,quantiles)
  {
    y=as.vector(yraw)
    x=as.vector(xraw)
    
    if (length(y)!=length(x))
      {
        stop ('Length Should be the same')
      }
    
    
    len=length(quantiles)

    xres <- array(0,len)
    xcnt <- array(0,len)
    xstd <- array(0,len)
    xmean <- array(0,len)
    
    yres <- array(0,len)
    ycnt <- array(0,len)
    ystd <- array(0,len)
    ymean <- array(0,len)
    
    cutoff <- quantile(x,quantiles)
    
    for (i in 1:length(x))
      {
        for (j in 1:len)
          {
             if (x[i]<=cutoff[j] && (j==1 || x[i]>cutoff[j-1]))
              {
                xcnt[j]=xcnt[j]+1
                xres[j]=xres[j]+x[i]
                ycnt[j]=ycnt[j]+1
                yres[j]=yres[j]+y[i]
                break;
              }
          }
      }
    
    xfrq <- xcnt/length(x)
    yfrq <- ycnt/length(x)
    ymean <- yres/ycnt
    xmean <- xres/xcnt
    
    for (i in 1:length(x))
      {
        for (j in 1:len)
          {
            if (x[i]<=cutoff[j])
              {
                ystd[j]=ystd[j]+(y[i]-ymean[j])^2
                xstd[j]=xstd[j]+(x[i]-xmean[j])^2
                break;
              }
          }
      }

    for (j in 1:len)
      {
        if (xstd[j]!=0 || xcnt[j]>1)
          {
            xstd[j]=sqrt(xstd[j]/(xcnt[j]-1))
          }
        
        if (ystd[j]!=0 || ycnt[j]>1)
          {
            ystd[j]=sqrt(ystd[j]/(ycnt[j]-1))
          }
      }

    res <- list(
                )
    attr(res,'ystd')=ystd
    attr(res,'xstd')=xstd
    attr(res,'yfrq')=yfrq
    attr(res,'xfrq')=xfrq
    attr(res,'ymean')=ymean
    attr(res,'xmean')=xmean
    attr(res,'cutoff')=cutoff
        
    res
  }


library(plotrix)

#            CL_graph_regimes(res,'xmean','ymean',file_name)

CL_graph_regimes <- function (res,xattr='xmean',yattr='ymean',file_name='',names=c('Index','Fund'),scale=1)
{
  old_par <- par()
  if (nchar(file_name))
    {
      pdf(file_name,paper='special',width=12.5984252,heigh=5.511811024,onefile=FALSE,family='Helvetica')
      par(las=1,
          font.main=1.5, font.lab=1.5, font.axis=1.5, cex=1.5, cex.main=1.5,              
          cex.lab=1.5, cex.axis=1.5,lwd=2, las=1) 
    }
  
  plarr <- array(0,c(2,length(attr(res,xattr))))
  plarr[1,]=attr(res,xattr)
  plarr[2,]=attr(res,yattr)

#  ymean=plarr[2,]
#  plarr[2,]=ymean*max(abs(plarr[1,]),na.rm=TRUE)/max(abs(plarr[2,]),na.rm=TRUE)
#  nymean=plarr[2,]
#  yloc=seq(min(nymean,na.rm=TRUE), max(nymean,na.rm=TRUE), by=(max(nymean,na.rm=TRUE)-min(nymean,na.rm=TRUE))/10)
#  ylbl=round(seq(min(ymean,na.rm=TRUE), max(ymean,na.rm=TRUE), by=(max(ymean,na.rm=TRUE)-min(ymean,na.rm=TRUE))/10)*100,2)
  
  colors <- c('red','blue')
  
  
  grp <- barp(plarr*100,col=colors,xlab='Quantile')
  grp$y[is.na(grp$y)]=0
 # axis (4, at=yloc, labels=ylbl)
  es=data.frame(x=max(ncol(plarr)-2,0),y=plarr[1,1]/2)
  
  
  legend(es$x, es$y, names, col = colors,
       text.col = "green4",fill=colors,
       merge = TRUE, bg = 'gray90')

  if (nchar(file_name))
    {
      invisible(dev.off())
      par(old_par)
    }
}


calc_hrt <- function (x1,x2,rel=FALSE)
  {
    res=NA
    if (rel)
      {
        res=as.double(x2)-as.double(x1)
      } else
    {
      res=as.double(x2)/as.double(x1)-1
    }

    res
    
  }

#res=round(classRes[,1]*100,2)
#title='CP Risk Contribution by Class'
#max=10
#all=TRUE
#res=classRes[,1]*100
#toplabel='Top Contributors'
#bottomlabel='Bottom Contributors'
#

plotTopBaR <- function (res,max=10,all=FALSE,toplabel='',bottomlabel='',toponly=FALSE, bottomonly=FALSE, ord=c(),labels=c(),...)
  {
    if (!length(ord))
      {
        ord=order(as.vector(res),decreasing=TRUE)
      }
    y=as.vector(res)[ord]
    x=index(res)[ord]
    colors=c('red','blue')

    maxn=max
    if (all || length(y)<maxn*2+1 || toponly || bottomonly)
      {
       if (length(y)>maxn)
        {
         if (toponly)
	     {
		 y=y[1:maxn]
	       x=x[1:maxn]
           } else 
	  if (bottomonly)
	    {
	        y=y[(length(y)-maxn+1):length(y)][maxn:1]
              x=x[(length(y)-maxn+1):length(y)][maxn:1]
	    }
        }
        
	  par(mar=(c(10,3,3,0)))
        grp <- barplot(y,col=colors,names.arg=NULL,space=0,las=2,cex.axis=0.9,...)
        axis(1,1:length(y)-0.5,las=2,cex.axis=0.8,col='black',substr(x,1,20))
        axis(3,1:length(y)-0.5,las=2,cex.axis=0.7,col='black',round(y,2))
#        if (length(labels))
#          {
#            for (i in 1:length(labels))
#              {
#                text(i-0.5,y[i],labels[i],srt=90,cex=1,font=1,col='black')
#              }
#          }
       
      }
    else
      {
        y1=y[1:maxn]
        x1=substr(x,1,20)[1:maxn]
        
        split.screen(c(1,2))        # split display into two screens
        screen(1)
        par(mar=(c(10,3,7,0.5)))
        grp <- barplot(y1,col=colors,names.arg=NULL,space=0,las=2,main=toplabel,cex.axis=0.9)
        axis(3,1:maxn-0.5,las=2,cex.axis=0.7,col='black',round(y1,2))
        axis(1,1:maxn-0.5,las=2,cex.axis=0.8,col='black',x1)
        box()
        screen(2)
        par(mar=(c(10,3,7,0.5)))
        y1=y[(length(y)-maxn+1):length(y)][maxn:1]
        x1=substr(x,1,20)[(length(y)-maxn+1):length(y)][maxn:1]
        grp <- barplot(y1,col=colors,names.arg=NULL,space=0,las=2,main=bottomlabel,cex.axis=0.9)
	  axis(3,1:maxn-0.5,las=2,cex.axis=0.7,col='black',round(y1,2))
        axis(1,1:maxn-0.5,las=2,cex.axis=0.8,col='black',x1)
        box()
	  close.screen(all = TRUE)


        

                       
      }
    return(ord)
  }

dates_months=c(
  'Jan',
  'Feb',
  'Mar',
  'Apr',
  'May',
  'Jun',
  'Jul',
  'Aug',
  'Sep',
  'Oct',
  'Nov',
  'Dec'
  )

fmt_date <- function(dt)
{
  fm=paste(dates_months[as.integer(substr(dt,5,6))],substr(dt,3,4),sep='')
  fm
}

fmt_ptr_name <- function (class, subclass=NA,pctile=NA)
{
  name=paste('PTRACK::',class,sep='')
   if (!is.na(subclass))
      {
        name=paste(name,'(',subclass,')',sep='')
      }

  if (!is.na(pctile))
    {
      name=paste(name,' ',pctile*100,'% pcntile',sep='')
    }

  name
}

calc_rtstats <- function(rt, rt_dates, m_dates,bench_rt,benchmark=FALSE)
  {
    rtsrt=c()
    btsrt=c()
    i <- 1
    for (i in 1:length(rt_dates))
      {
        if (length(rt_dates[[i]])<=length(rt))
          {
            rttr=rt[rt_dates[[i]]]
            if (length(rttr)!=length(rt_dates[[i]]))
              {
                return (list(use=0,res=list()))
              }
            rtsrt=c(rtsrt,list(rttr))
            if (!benchmark)
              {
                bttr=bench_rt[rt_dates[[i]]]
                if (length(bttr)!=length(rt_dates[[i]]))
                  {
                    stop("Benchmark doesn't have enough dates!")
                  }
                btsrt=c(btsrt,list(bttr))
              }
          }
      }
    
    rtsm=c()
    btsm=c()
    for (i in 1:length(m_dates))
      {
        if (length(rt)>=length(m_dates[[i]]))
          {
            rttr=rt[m_dates[[i]]]
            if (length(rttr)!=length(m_dates[[i]]))
              {
                return (list(use=0,rtres=list()))
              }
            rtsm=c(rtsm,list(rttr))
            if (!benchmark)
              {
                bttr=bench_rt[m_dates[[i]]]
                if (length(bttr)!=length(m_dates[[i]]))
                  {
                    stop("Benchmark doesn't have enough dates!")
                  }
                btsm=c(btsm,list(bttr))
              }
            
          }
      }
        
    rrtres=data.frame(window=integer(),rt=double(),bdiffrt=double(),brt=double())

    if (length(rtsrt)>0)
      {
        i <- 1
        for (i in 1:length(rtsrt))
          {
            crt=0
            brt=NA
            if (length(rtsrt[[i]])<12)
                  {
                    crt=Return.cumulative(rtsrt[[i]])
                    if (!benchmark)
                      {
                        brt=Return.cumulative(btsrt[[i]])
                      }
                  } else
            {
              crt=Return.annualized(rtsrt[[i]])
              if (!benchmark)
                {
                  brt=Return.annualized(btsrt[[i]])
                }
            }
            
            rrtres=merge(rrtres,cbind(window=length(rtsrt[[i]]),rt=crt,brt=brt,bdiffrt=crt-brt),all=TRUE)
          }
      }

    rtres=data.frame(
      window=integer(),
      sd=double(),
      maxdrawdown=double(),
      postdrawrt=double(),
      rtdraw=double(),
      upr=double(),
      sharpe=double(),
      cfvar=double(),
      var=double(),
      omega=double(),
      bomega=double(),
      omegaratio=double(),
      bmaxdraw=double(),
      brtdraw=double(),
      bpostdrawrt=double(),
      bdiffmaxdraw=double(),
      bdiffrtdraw=double(),
      bdiffpostdrawrt=double(),
      alpha=double(),
      beta=double(),
      badjdiff=double(),
      r2=double(),
      pbeta=double(),
      palpha=double(),
      trackingerror=double(),
      activepremium=double(),
      inforatio=double(),
      bcfvar=double(),
      bvar=double(),
      bcfvarratio=double(),
      bvarratio=double()
      )


    if (length(rtsm)>0)
      {
        i <- 1
        for (i in 1:length(rtsm))
          {
            # calculating drawdowns and comparing it to the benchmark
            draws=findDrawdowns(rtsm[[i]])
            maxdraw=0
            rtdraw=Inf
            postdrawrt=0
            
            if (isnull(length(draws$return),0)>0)
              {
                if (length(draws$return)>1)
                  {
                    draws=sortDrawdowns(draws)
                  }
                
                maxdraw=draws$return[1]
                rtdraw=0
                if (length(rtsm[[i]])>draws$trough[1])
                  {
                    postdrawrt=Return.cumulative(rtsm[[i]][(draws$from[1]+1):length(rtsm[[i]])])
                    rtdraw=abs(postdrawrt/maxdraw)
                  }
              }
                
            bmaxdraw=NA
            brtdraw=NA
            bpostdrawrt=NA
            alpha=NA
            beta=NA
            badjdiff=NA
            r2=NA
            pbeta=NA
            palpha=NA


            if (!benchmark)
              {
                bdraws=findDrawdowns(btsm[[i]])
                bmaxdraw=0
                brtdraw=Inf
                bpostdrawrt=0
                
                if (isnull(length(bdraws$return),0)>0)
                  {
                    if (length(bdraws$return)>1)
                      {
                        bdraws=sortDrawdowns(bdraws)
                      }
                    
                    bmaxdraw=bdraws$return[1]
                    brtdraw=0
                    if (length(btsm[[i]])>bdraws$trough[1])
                      {
                        bpostdrawrt=Return.cumulative(btsm[[i]][(bdraws$from[1]+1):length(btsm[[i]])])
                        btdraw=abs(bpostdrawrt/bmaxdraw)
                      }
                  }
                
                y=as.vector(rtsm[[i]])
                x=as.vector(btsm[[i]])
                sregress=summary(lm(y~x))
                cf=coef(sregress)
                if (length(cf)==8)
                  {
                    alpha=cf[1,1]
                    beta=cf[2,1]
                    badjdiff=Return.annualized(rtsm[[i]]-btsm[[i]]*beta)
                    r2=sregress$r.squared
                    pbeta=cf[2,4]
                    palpha=cf[1,4]
                  }
                
              }

            omega=Omega(rtsm[[i]],method='simple')
            cfvar=VaR.CornishFisher(rtsm[[i]],0.95,modified=TRUE)
            var=VaR.traditional(rtsm[[i]],p=0.95)
            bcfvar=NA
            bvar=NA
            bomega=NA
            trackingerror=NA
            activepremium=NA
            inforatio=NA
            if (!benchmark)
              {
                
                bcfvar=VaR.CornishFisher(btsm[[i]],0.95,modified=TRUE)
                bvar=VaR.traditional(btsm[[i]],p=0.95)
                bomega=Omega(btsm[[i]],method='simple')
                trackingerror=TrackingError(rtsm[[i]],btsm[[i]])
                activepremium=ActivePremium(rtsm[[i]],btsm[[i]])
                inforatio=InformationRatio(rtsm[[i]],btsm[[i]])

              }

            rtres=merge(rtres,
              cbind(
                    sd=as.double(sd.annualized(rtsm[[i]])),
                    maxdrawdown=maxdraw,
                    postdrawrt=postdrawrt,
                    rtdraw=rtdraw,
                    upr=UpsidePotentialRatio(rtsm[[i]]),
                    sharpe=SharpeRatio.annualized(rtsm[[i]]),
                    cfvar=cfvar,
                    var=var,
                    bcfvar=bcfvar,
                    bvar=bvar,
                    bcfvarratio=cfvar/bcfvar,
                    bvarratio=var/bvar,
                    window=length(rtsm[[i]]),
                    omega=omega,
                    bomega=bomega,
                    omegaratio=omega/bomega,
                    bmaxdraw=bmaxdraw,
                    brtdraw=brtdraw,
                    bpostdrawrt=bpostdrawrt,
                    bdiffmaxdraw=maxdraw-bmaxdraw,
                    bdiffrtdraw=rtdraw/brtdraw,
                    bdiffpostdrawrt=postdrawrt-bpostdrawrt,
                    alpha=alpha,
                    beta=beta,
                    badjdiff=badjdiff,
                    r2=r2,
                    pbeta=pbeta,
                    palpha=palpha,
                    trackingerror=trackingerror,
                    activepremium=activepremium,
                    inforatio=inforatio
                    
                    ),
              all=TRUE
              )

          }
      }

    
    return (list(use=1,rtres=rrtres,mrtres=rtres))
    
  }

pertrac_percentiles <- function(con,ptr,qtiles)
{

  sql <- 'select * from pertrac_info order by class, sub_class';

  ptrInfo=sqlQuery(con, sql)
  classes=as.character(unique(ptrInfo$class))

  for (k in 1:length(classes))
    {
      z=ptrInfo[ptrInfo$class==classes[k],]
      if (nrow(z)>1)
        {
          fields=paste('(',z$fields,')',collapse=' OR ')
          ptrInfo=merge(ptrInfo,cbind(class=as.character(classes[k]),fields=fields),all=TRUE)          
        }
    }
  

  k <- 1
  classRts=c()
  for (k in 1:nrow(ptrInfo))
    {
      name=paste('PTRACK::',ptrInfo$class[k],sep='')
      
      if (!is.na(ptrInfo$sub_class[k]))
        {
          name=paste(name,'(',ptrInfo$sub_class[k],')',sep='')
        }
      
      message('PROCESSING ',name)
      
      sql<-paste("SELECT DATEPART(YYYY,date)*100+DATEPART(MM,date) dt, p.[return] performance 
                FROM  Performance p, UserCheck1 m,masterName mn
                where m.id=p.id and day(p.date)>20 and m.C58=0 and mn.MasterName not like 'CL %' and m.id=mn.id and
                (",as.character(ptrInfo$fields[k]),")
              order by p.date",sep='')
      
      classRt=sqlQuery(ptr,sql)
      
      if (nrow(classRt)>0)
        {
          dates=sort(unique(classRt$dt))
          rts=array(NA,c(length(dates),length(qtiles)))
          fundcnt=c(0,length(dates))
          i <- 1
          for (i in 1:length(dates))
            {
              rtd=classRt[classRt$dt==dates[i],]
              fundcnt[i]=nrow(rtd)
              rts[i,]=quantile(rtd$performance,probs = qtiles,na.rm=FALSE,names=FALSE,type = 7)
            }
          classRts=c(classRts,list(list(rts=rts,dates=dates,fundcnt=fundcnt)))
        } else
      {
        classRts=c(classRts,list(list(rts=list(),dates=list(),fundcnt=list())))
      }
    }

  res=list(ptrInfo=ptrInfo,classes=classes,classRts=classRts)
}

fmt_reg <- function(val,fmt_val,p_value)
  {
    res=fmt_val
    if (p_value<0.05)
      {
        res=paste('{\\color{red}',fmt_val,'}',sep='')
      }
    res
  }

#curr=933.5
#fs=curr
#hs=curr
#fml='V-20%'
stress_value <- function(curr, fs, hs, fml)
  {
    res=fs
    color='yellow'
    if (nchar(fml))
      {
        if (fml=='H')
          {
            res=hs
            color='magenta'
          } else
        if (fml=='C')
          {
            color='red'
            res=curr
          } else
        if (fml=='A')
          {
            color='green'
            res=0.5*(fs+hs)
          } else
        if (substr(fml,1,1)=='V')
          {
            message("RED!")
            color='red'
            if (substr(fml,1,2)=='VA')
              {
                res=isnull(as.double(substr(fml,3,nchar(fml))),0)
              } else 
            if (substr(fml,nchar(fml),nchar(fml))=='%')
              {
                if (nchar(fml)>2)
                  {
                    change=isnull(as.double(substr(fml,2,nchar(fml)-1))/100,0)
                    res=curr*(1+change)
                  }
              } else
            {
             if (nchar(fml)>1)
               {
                 change=isnull(as.double(substr(fml,2,nchar(fml))),0)
                 res=curr+change
               }
              
            }
          } else
        {
          if (fml!='F')
            {
              message("Unknown formula for mixing forecast and historical percentiles::",fml," Using forecast extremes")
            }
        }
        
      }
    val=list(res=res,color=color)
    return (val)
  }

stress_divergence <- function(curr, fb, fg, fml)
  {
    res=fg-fb
    
    if (nchar(fml))
      {
        if (substr(fml,1,1)=='A')
          {
            res=fg/fb-1
          }

#        if (nchar(fml)>1)
#          {
#            scale=substr(fml,2,nchar(fml))
#            res=res/scale
#          }
        
      }
    
    res
  }

stress_pct <- function(curr, val, fml)
  {
    res=val-curr
    
    if (nchar(fml))
      {
        if (substr(fml,1,1)=='A')
          {
            res=val/curr-1
          }
      }
    
    res
  }

#dates
#field='class'
get_bench_mapping <- function (con,dates,field='class')
  {
    sql <- 'select year(fb.dt)*100+month(fb.dt) dt, fb.ticker,f.fund_id, fb.classification, fb.benchmark
            from fund_benchmark fb, fund f
            where fb.ticker=f.ticker
            order by fb.classification,f.fund_id, dt';
    fbench=sqlQuery(con,sql)

    sql <- 'select classification, class, benchmark, short_name  from class_benchmark
            order by classification, class';
    
    cbench=sqlQuery(con,sql)

    ubenchmarks=unique(c(as.character(fbench$benchmark),as.character(cbench$benchmark)))

    if (!length(ubenchmarks))
      {
        stop("get_bench_mapping::No benchmarks to be found!")
      }
    
    brt=get_benchmarks(con,ubenchmarks,24,'',FALSE,dates,TRUE,TRUE,TRUE)
    classification=sort(unique(as.character(cbench$classification)))

    sql <- paste('select fund_id, ticker, ',field,' class from fund where type="FUND" and ticker is not null')
    funds <- sqlQuery(con,sql)
    
    cf <- classification[1]
    res=data.frame(classification=classification)
    j <- 0
    cbench$index=0

    for (i in 1:nrow(cbench))
        {
          ndx=isnull(which(attr(brt,'tickers')==as.character(cbench$benchmark[i]))[1],0)
          if (!ndx)
            {
              stop("get_bench_mapping:: Can't find benchmark for class ",cbench$class[i],' ',cbench$benchmark[i])
            }
          cbench$index[i]=ndx
        }
    
    for (cf in classification)
    {

      j <- j+1
      message("get_bench_mapping:: Processing ",cf)
      fcf=fbench[as.character(fbench$classification)==cf,]
      ccf_ndx=which(as.character(cbench$classification)==cf)
      ccf=cbench[ccf_ndx,]




      i <- 1
      cfres=c()
      for (i in 1:nrow(funds))
        {
          ndx=isnull(which(as.character(ccf$class)==as.character(funds$class[i]))[1],0)
          if (ndx==0)
            {
              stop("get_bench_mapping:: Can't find benchmark for class ",funds$class[i]," Fund ",funds$ticker[i])
            }
          fbench_index=as.character(ccf$benchmark[ndx])
          fndx=isnull(which(fcf$fund_id==funds$fund_id[i])[1],0)
          if (fndx>0)
            {
              fbench_index=as.character(fcf$benchmark[fndx])
            }
          fbench_ndx=isnull(which(attr(brt,'tickers')==fbench_index)[1],0)
          if (!fbench_ndx)
            {
              stop("get_bench_mapping:: No data for benchmark ",fbench_index)
            }

          cfres=c(cfres,fbench_ndx)
        }
      res$index[j]=list(cfres)
    }

    out=list(brt=brt,res=res,funds=funds,cbench=cbench,classification=classification)
  }

#create class returns using benchmarks only
#realizedRt=prt
#oldrealizedRt=realizedRt
reconstructClassBenchmarkRt <- function(realizedRt,bmap)
  {
    classes=sort(unique(as.character(realizedRt$ids$class)))
    clid=attr(realizedRt,'clids')
    dates=sort(unique(clid$dt))
    dates=dates[dates%in%index(bmap$brt)]
    message('Reconstruct Class Benchmark Returns::',realizedRt$name)

    clf=bmap$classification[1]
    fres=data.frame(classification=bmap$classification)
    fres$cres=NA
    fres$fres=NA

    clf <- 1
    out=data.frame(classification=bmap$classification)
    out$res=list(list())
    for (clf in 1:length(bmap$classification))
      {
        cbench=bmap$cbench
        cbench=cbench[as.character(cbench$classification)==bmap$classification[clf],]
        zrt=zoo(0,dates)
        classRt=zoo()
        classSRt=zoo()
        classfRt=zoo()
        classfSRt=zoo()

        
        classAlloc=zoo()
        class=classes[1]
        portRt=zrt
        portfRt=zrt
        
        for (class in classes)
          {
            rt <- zrt
            ft <- zrt
            at <- zrt
            srt <- zrt
            sft <- zrt

            
            class_ndx=isnull(which(as.character(cbench$class)==class)[1],0)
            if (!class_ndx)
              {
                stop("reconstructClassBenchmarkRt:: Unable to locate data for class ",class)
              }

            crt=sub_rt(bmap$brt[,cbench$index[class_ndx]],dates,FALSE)
            cids <- realizedRt$ids[realizedRt$ids$class==class,]$id
            id=cids[1]
            for (id in cids)
              {
                frt <- clid[clid$id==id,]
                dt <- dates[1]

                fndx=isnull(which(bmap$funds$fund_id==id)[1],0)
                if (!fndx)
                  {
                    stop("reconstructClassBenchmarkRt:: unable to locate fund mapping info for ID=",id)
                  }
                ndx=bmap$res$index[[clf]][fndx]
                use_rt=sub_rt(bmap$brt[,ndx],dates,FALSE)

                j <- 1
                for (j in 1:length(dates))
                  {
                    dt=dates[j]
                    ndx <- isnull(which(frt$dt==dt)[1],0)
                    if (ndx>0)
                      {
                        ft[j]=as.double(ft[j])+use_rt[j]*frt$alloc[ndx]
                        at[j]=as.double(at[j])+frt$alloc[ndx]
                        portfRt[j]=as.double(portRt[j])+use_rt[j]*frt$alloc[ndx]
                      }
                  }
              }

            rt=crt*at
            portRt=portRt+rt
            
            for (j in 1:length(dates))
              {
                if(at[j]>0)
                  {
                    srt[j]=rt[j]/at[j]
                    sft[j]=ft[j]/at[j]
                  }
              }
        
            classRt=merge(classRt,rt)
            classSRt=merge(classSRt,srt)
            classAlloc=merge(classAlloc,at)
            classfRt=merge(classfRt,ft)
            classfSRt=merge(classfSRt,sft)

            
          }
        
        colnames(classRt)=classes
        colnames(classSRt)=classes
        colnames(classAlloc)=classes
        colnames(classfSRt)=classes
        colnames(classfRt)=classes

        classCRt=classSRt*0
        classfCRt=classSRt*0
        
        
      #calculate cumulative standalone performance for each class
        
       for (j in 1:ncol(classCRt))
         {
           crt=1;
           fcrt=1;
           for (i in 1:length(dates))
             {
               crt=crt*(1+as.double(classSRt[i,j]))
               fcrt=fcrt*(1+as.double(classfSRt[i,j]))
               classCRt[i,j]=crt
               classfCRt[i,j]=fcrt
             }
         }
    

        for (i in 1:length(dates))
          {
            sm=sum(classAlloc[i,])
            if (sm<0.99 ||            sm>1.01)
              {
                stop("Class Allocation doesnt sum to 1! ",sm,' ',dates[i])
              }
          }
        
        res=list(classes=classes,classRt=classRt,classAlloc=classAlloc,classSRt=classSRt,classCRt=classCRt,
                                 classfRt=classfRt,classfSRt=classfSRt,classfCRt=classfCRt,
                 portRt=portRt,portfRt=portfRt
          )
        out$res[clf]=list(res)
      }
    out
  }


toxts <- function(rts,asDate=FALSE)
  {
    if (asDate)
      {
        return(xts(rts,as.Date(paste(substr(index(rts),1,4),'-',substr(index(rts),5,6),'-01',sep=''),format='%Y-%m-%d')))
      } else
    {
      return(xts(rts,as.POSIXct(paste(substr(index(rts),1,4),'-',substr(index(rts),5,6),'-01',sep=''),format='%Y-%m-%d')))
    }
    
  }

dtposix <- function(date)
  {
     return(as.POSIXct(paste(substr(date,1,4),'-',substr(date,5,6),'-01',sep=''),format='%Y-%m-%d'))
  }

dtposixFull <- function(date)
  {
     return(as.POSIXct(paste(substr(date,1,4),'-',substr(date,5,6),'-',substr(date,7,8),sep=''),format='%Y-%m-%d'))
  }





rtAnnualized <- function (rt)
{
  Return.annualized(as.vector(rt),scale=12)
}

#R=out
#stolen form performance Analytics
CLPerformanceSummary <- function (R, rf = 0, main = NULL, method = c("ModifiedVaR","VaR","StdDev"), width = 0, event.labels = NULL, ylog = FALSE, wealth.index = FALSE, gap = 12, begin=c("first","axis"), legend.loc="topleft", colnames=c(),...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a wealth index chart, bars for monthly performance,
    # and underwater chart for drawdown.

    # Inputs:
    # R: a matrix, data frame, or timeSeries, usually a set of monthly returns.
    #   The first column is assumed to be the returns of interest, the next
    #   columns are assumed to be relevant benchmarks for comparison.
    # rf: this is the risk free rate.  Remember to set this to the same
    #   periodicity as the data being passed in.
    # method: Used to select the risk parameter to use in the chart.BarVaR.  May
    #   be any of:
    #     modVaR - uses CF modified VaR
    #     VaR - uses traditional Value at Risk
    #     StdDev - monthly standard deviation of trailing 12 month returns
    #

    # Outputs:
    # A stack of three related timeseries line charts

    # FUNCTION:
    begin = begin[1]
    x = R
    colnames = colnames(x)
    ncols = ncol(x)

# This repeats a bit of code from chart.CumReturns, but it's intended
# to align the start dates of all three charts.  Basically, it assumes
# that the first column in the list is the column of interest, and 
# starts everything from that start date

    length.column.one = length(x[,1])
# find the row number of the last NA in the first column
    start.row = 1
    start.index = 0
    while(is.na(x[start.row,1])){
        start.row = start.row + 1
    }
    x = x[start.row:length.column.one,]

    if(ncols > 1)
        legend.loc = legend.loc
    else
        legend.loc = NULL

    if(is.null(main))
        main = paste(colnames[1],"Performance", sep=" ")

    if(ylog)
        wealth.index = TRUE

    colors=rainbow(ncols)
    # First, we lay out the graphic as a three row, one column format
#    plot.new()
    layout(matrix(c(1,2,3)),height=c(2,1,1.3),width=1)
    # to see the resulting layout, use layout.show(3)

    # mar: a numerical vector of the form c(bottom, left, top, right) which
    # gives the number of lines of margin to be specified on the four sides
    # of the plot. The default is c(5, 4, 4, 2) + 0.1

    # The first row is the cumulative returns line plot
    par(mar=c(1,4,4,2))
    chart.CumReturns(x, main = main, xaxis = FALSE, ylab = NULL, event.labels = event.labels, ylog = ylog, wealth.index = wealth.index, begin = begin, colors=colors,...)
    smartlegend( x="left", y= "top", inset=0,                             #smartlegend parameters
                legend = c(colnames), #legend parameters 
                fill=c(colors),                        #legend parameters
                bg = "gray",cex=1)                                             #legend parameters


    # The second row is the monthly returns bar plot
    par(mar=c(1,4,0,2))
#    chart.BarVaR(as.matrix(R[,1]), main = "", xaxis = FALSE, ylab = "Monthly Return", method = method)
#    chart.TimeSeries(x, main = "", ylab = "Monthly Return", xaxis=FALSE,event.labels = NULL, ylog=FALSE, colors=colors,...)
      chart.RollingPerformance(x, width=12,main = "", ylab = "12m Return", xaxis=FALSE,event.labels = NULL, ylog=FALSE, colors=colors,...)
    
    # The third row is the underwater plot
    par(mar=c(5,4,0,2))
    chart.Drawdown(x, main = "", ylab = "From Peak", event.labels = NULL, ylog=FALSE, colors=colors, ...)
    
    # If we wanted to add a fourth row with the table of monthly returns
    # Unfortunately, the textplot function doesn't provide a lot of control over
    # formatting.  Also, it requires the gplots package.
    #par(mar=c(0,0,0,0))
    #textplot(table.Returns(as.matrix(R)),cex=.7,cmar=1.5,rmar=0.5,halign="center", valign="center")
}

CLPerformanceSummaryText <- function (R,text, rf = 0, main = NULL, method = c("ModifiedVaR","VaR","StdDev"), width = 0, event.labels = NULL, ylog = FALSE, wealth.index = FALSE, gap = 12, begin=c("first","axis"), legend.loc="topleft", colnames=c(),...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a wealth index chart, bars for monthly performance,
    # and underwater chart for drawdown.

    # Inputs:
    # R: a matrix, data frame, or timeSeries, usually a set of monthly returns.
    #   The first column is assumed to be the returns of interest, the next
    #   columns are assumed to be relevant benchmarks for comparison.
    # rf: this is the risk free rate.  Remember to set this to the same
    #   periodicity as the data being passed in.
    # method: Used to select the risk parameter to use in the chart.BarVaR.  May
    #   be any of:
    #     modVaR - uses CF modified VaR
    #     VaR - uses traditional Value at Risk
    #     StdDev - monthly standard deviation of trailing 12 month returns
    #

    # Outputs:
    # A stack of three related timeseries line charts

    # FUNCTION:
    begin = begin[1]
    colnames = colnames(R)
    ncols = ncol(R)

# This repeats a bit of code from chart.CumReturns, but it's intended
# to align the start dates of all three charts.  Basically, it assumes
# that the first column in the list is the column of interest, and 
# starts everything from that start date

    length.column.one = length(R[,1])
# find the row number of the last NA in the first column
    start.row = 1
    start.index = 0
    while(is.na(R[start.row,1])){
        start.row = start.row + 1
    }
    x = R[start.row:length.column.one,]

    legend.loc = NULL

    if(ncols > 1) legend.loc = legend.loc
    

    if(is.null(main))
        main = paste(colnames[1],"Performance", sep=" ")

    if(ylog)
        wealth.index = TRUE

    colors=rainbow(ncols)
    # First, we lay out the graphic as a three row, one column format
#    plot.new()
    layout(matrix(c(1,2,3)),height=c(2,1,1.3),width=1)
    # to see the resulting layout, use layout.show(3)

    # mar: a numerical vector of the form c(bottom, left, top, right) which
    # gives the number of lines of margin to be specified on the four sides
    # of the plot. The default is c(5, 4, 4, 2) + 0.1

    # The first row is the cumulative returns line plot
    par(mar=c(1,4,4,2))
    chart.CumReturns(x, main = main, xaxis = FALSE, ylab = 'Cumulative Return', event.labels = event.labels, ylog = ylog, wealth.index = wealth.index, begin = begin, colors=colors,...)
    smartlegend( x="left", y= "top", inset=0,                             #smartlegend parameters
                legend = c(colnames), #legend parameters 
                fill=c(colors),                        #legend parameters
                bg = "gray",cex=0.7)
                                        #legend parameters
    par(new=T)
    message('Plot Text')
    textplot(text,cex=1.4)
    


    message('Start Roll')
    
    # The second row is the monthly returns bar plot
    par(mar=c(1,4,0,2))
#    chart.BarVaR(as.matrix(R[,1]), main = "", xaxis = FALSE, ylab = "Monthly Return", method = method)
    chart.RollingPerformance(x, main = "", ylab = "12M Ann.Return", xaxis=FALSE,event.labels = NULL, ylog=FALSE, colors=colors,...)

    message('End Roll')
    
    # The third row is the underwater plot
    par(mar=c(5,4,0,2))
    chart.Drawdown(x, main = "", ylab = "Drawdown", event.labels = NULL, ylog=FALSE, colors=colors, ...)
    
    # If we wanted to add a fourth row with the table of monthly returns
    # Unfortunately, the textplot function doesn't provide a lot of control over
    # formatting.  Also, it requires the gplots package.
    #par(mar=c(0,0,0,0))
    #textplot(table.Returns(as.matrix(R)),cex=.7,cmar=1.5,rmar=0.5,halign="center", valign="center")
}


insert_rt <- function (con,name,rt)
  {
    sql=paste('select fund_id from fund where name="',name,'" and type="BENCHMARK"',sep='')
    res=sqlQuery(con, sql)
    if (nrow(res)==0)
      {
        sql1=paste('insert into fund (name,type) values("',name,'","BENCHMARK")',sep='')
        invisible(sqlQuery(con, sql1))
        res=sqlQuery(con, sql)
      }
    id=res$fund_id[1]
    invisible(sqlQuery(con,paste("delete from rt where fund_id=",id)))

    i <- 1
    for (i in 1:nrow(rt))
      {
        sql=paste('insert into rt (fund_id, dt, rt, return_type_id) values(',id,',\'',as.character(index(rt))[i],'\',',as.double(rt[i]),',7);',sep='');
        message(sql)
        invisible(sqlQuery(con,sql))
      }
    
    
  }

outfile <- function (name)
  {
   out=paste(outdir,name,sep='')
   return(out)
 }


#inrt=m_benchmarks[,j]
#ofrq=1
#nfrq=3
convRt <- function (inrt,ofrq,nfrq)
  {
    out=c()
    if (ofrq>nfrq)
      {
        for (i in 1:length(inrt))
          {
            r=(1+inrt[i])^(nfrq/ofrq)-1
            out=c(out,rep(r,ofrq/nfrq))
          }
      } else
    {
      ratio=nfrq/ofrq
      blocks=(length(inrt)%/%ratio)
      
      for (i in 1:blocks)
        {
          start=length(inrt)-ratio*(blocks-i+1)+1
          end=length(inrt)-ratio*(blocks-i)
          out=c(out,Return.cumulative(inrt[start:end]))
        }
    }
      
    return(out)     
  }


#stolen from performance analytics to fix corr bugs
#R.fund=m_assets[,1]
#R.style=benchmarks

CLfit <- function (R.fund, R.style, model = FALSE, method = c("constrained", 
    "unconstrained", "normalized"), leverage = FALSE, selection = c("none", 
    "AIC"), ...) 
{
    method = method[1]
    selection = selection[1]
    style.rows = dim(R.style)[1]
    style.cols = dim(R.style)[2]
    fund.rows = dim(R.fund)[1]
    fund.cols = dim(R.fund)[2]
    style.colnames = colnames(R.style)
    fund.col <- 1
    for (fund.col in 1:fund.cols) {
        if (method == "constrained") {
            column.result = style.QPfit(R.fund = R.fund[, fund.col, 
                drop = FALSE], R.style = R.style, leverage = leverage)
            if (fund.col == 1) {
                result.weights = column.result$weights
                result.R2 = column.result$R.squared
                result.adjR2 = column.result$adj.R.squared
            }
            else {
                result.weights = cbind(result.weights, column.result$weights)
                result.R2 = cbind(result.R2, column.result$R.squared)
                result.adjR2 = cbind(result.adjR2, column.result$adj.R.squared)
            }
        }
        else if (method == "unconstrained" | method == "normalized") {
            column.lm = lm(as.vector(R.fund[, fund.col]) ~ 0 + ., data = R.style)
            if (selection == "AIC") {
                column.result = step(column.lm)
                if (fund.col == 1) 
                  column.weights = data.frame(matrix(rep(0, length(style.colnames) * 
                    fund.cols), nrow = length(style.colnames), 
                    ncol = fund.cols), row.names = style.colnames)
                column.coef = as.data.frame(coef(column.result))
                if (length(coef(column.result)) > 0) {
                  row.loc = match(rownames(column.coef), rownames(column.weights))
                  for (i in 1:length(row.loc)) column.weights[row.loc[i], 
                    fund.col] = column.coef[i, 1]
                }
            }
            else {
                column.result = column.lm
                column.weights = as.data.frame(coef(column.lm))
            }
            rownames(column.weights) = colnames(R.style)
            colnames(column.weights) = colnames(R.fund)[fund.col]
            R2 = as.data.frame(summary(column.result)$r.squared)
            adjR2 = as.data.frame(summary(column.result)$adj.r.squared)
            colnames(R2) = colnames(R.fund)[fund.col]
            colnames(adjR2) = colnames(R.fund)[fund.col]
            rownames(R2) = "R-squared"
            rownames(adjR2) = "Adj R-squared"
            if (method == "normalized") {
                column.weights = column.weights/sum(column.weights)
            }
            if (fund.col == 1) {
                result.weights = column.weights
                result.R2 = R2
                result.adjR2 = adjR2
            }
            else {
                result.weights = cbind(result.weights, column.weights)
                result.R2 = cbind(result.R2, R2)
                result.adjR2 = cbind(result.adjR2, adjR2)
            }
        }
        else stop("Method is mis-specified.  Select from \"constrained\", \"unconstrained\", or  \"normalized\"")
    }
    result = list(weights = result.weights, R.squared = result.R2, 
        adj.R.squared = result.adjR2)
    return(result)
}


CLRollingStyle <- function (R.fund,R.style,method='constrained',width=24,main=NULL,leverage=FALSE,space=0,graphit=TRUE,...)
  {

  R.fund = checkData(R.fund[, 1, drop = FALSE])
  R.style = checkData(R.style)
  columns.fund = ncol(R.fund)
  columns.style = ncol(R.style)
  columnnames.fund = colnames(R.fund)
  columnnames.style = colnames(R.style)
  merged.assets = na.omit(merge(R.fund, R.style))
  
  start=1
  end=width

   result= xts:::rollapply.xts(merged.assets, FUN = function(x, 
                                                method, leverage) {
    t(CLfit(R.fund = x[, 1, drop = FALSE], R.style = x[, 
                                                 -1, drop = FALSE], method = method, leverage = leverage)$weights)
  }, width = width, method = method, leverage = leverage, by = 1, 
    by.column = FALSE, na.pad = FALSE, align = "right")
  
  if (is.null(main)) {
    freq = periodicity(R.fund)
    switch(freq$scale, minute = {
      freq.lab = "minute"
    }, hourly = {
            freq.lab = "hour"
        }, daily = {
            freq.lab = "day"
        }, weekly = {
            freq.lab = "week"
        }, monthly = {
            freq.lab = "month"
        }, quarterly = {
            freq.lab = "quarter"
        }, yearly = {
            freq.lab = "year"
        })
        main = paste(colnames(R.fund)[1], " Rolling ", width, 
            "-", freq.lab, " Style Weights", sep = "")
    }
    colnames(result) = columnnames.style
    if (graphit)
      {
        chart.StackedBar(result, main = main, space = space, ...)
      }
    return(result)
}

#R.style=benchmarks

CLRollingStyleProforma <- function (prts,R.style,pdates,method='constrained',main=NULL,leverage=FALSE,space=0,graphit=TRUE,...)
  {

  R.style = checkData(R.style)
  columns.style = ncol(R.style)
  columnnames.style = colnames(R.style)
  dates=c()  
  
  i <- 20
  result=xts()
  for (i in 1:length(prts))
    {
      y=prts[[i]]
      x=R.style[index(R.style)%in%index(y)]
      if (nrow(x)==nrow(y))
        {
          date=dtposix(pdates[i])
          dates=c(dates,date)
          res=xts(t(CLfit(R.fund =y , R.style = x, method = method, leverage = leverage)$weights),date)
          if (isnull(nrow(result),0))
            {
              result=rbind(result,res)
            } else {result=res}
        }
    }
           
  
  if (graphit)
      {
        chart.StackedBar(result, main = main, space = space, ...)
      }
    return(result)
}




# stolen from performance Analytics -- rolling graphs
CLRolling3 <- function (R, width = 12, Rf = 0, main = NULL, trim = TRUE,funcs=c('Return.annualized','StdDev.annualized','SharpeRatio.annualized'),labels=c(),event.labels = NULL, 
    legend.loc = NULL, ...) 
{
    x = checkData(R)
    colnames = colnames(x)
    ncols = ncol(x)
    if (is.null(main)) {
        freq = periodicity(R)
        switch(freq$scale, minute = {
            freq.lab = "minute"
        }, hourly = {
            freq.lab = "hour"
        }, daily = {
            freq.lab = "day"
        }, weekly = {
            freq.lab = "week"
        }, monthly = {
            freq.lab = "month"
        }, quarterly = {
            freq.lab = "quarter"
        }, yearly = {
            freq.lab = "year"
        })
        main = paste("Rolling", width, freq.lab, "Performance", 
            sep = " ")
    }
    if (!length(labels))
      {
        labels=funcs
      }
    op <- par(no.readonly = TRUE)
    layout(matrix(c(1, 2, 3)), height = c(1, 0.75, 1), width = 1)
    par(mar = c(1, 4, 4, 2))
    message(funcs[1])
    chart.RollingPerformance(R, width = width, main = main, xaxis = FALSE, 
        ylab = labels[1], FUN = funcs[1], 
        legend.loc = legend.loc, event.labels = event.labels, 
        ...)
    par(mar = c(1, 4, 0, 2))
    message(funcs[2])
    chart.RollingPerformance(R, width = width, main = "", xaxis = FALSE, 
        ylab = labels[2], FUN = funcs[2],
        event.labels = NULL, ...)
    par(mar = c(5, 4, 0, 2))
    message(funcs[3])
    chart.RollingPerformance(R, width = width, main = "", ylab = labels[3],
        FUN = funcs[3], event.labels = NULL, 
        ...)
    par(op)
}


# stolen from performance Analytics -- rolling graphs
CLTimeSeries3 <- function (R1,R2,R3,labels=c(),event.labels = NULL, 
    legend.loc = NULL, main='',...) 
{
  op <- par(no.readonly = TRUE)
    layout(matrix(c(1, 2, 3)), height = c(1, 0.75, 1), width = 1)
    par(mar = c(1, 4, 4, 2))
    chart.TimeSeries(R1,  main = main, xaxis = FALSE, 
        ylab = labels[1],
        event.labels = event.labels, legend.loc=legend.loc,
        ...)
    par(mar = c(1, 4, 0, 2))
    chart.TimeSeries(R2, main = "", xaxis = FALSE, 
        ylab = labels[2],
        event.labels = NULL, ...)
    par(mar = c(5, 4, 0, 2))
    chart.TimeSeries(R3,  main = "", ylab = labels[3],
        event.labels = NULL, 
        ...)
    par(op)
}


# stolen from performance Analytics -- rolling graphs
CLTimeSeries3Mod <- function (R1,R2,R3,labels=c(),event.labels = NULL, 
    legend.loc = NULL, main='',...) 
{

  xax=sort(unique(c(index(R1),index(R2),index(R3))))

  
  
  op <- par(no.readonly = TRUE)
    layout(matrix(c(1, 2, 3)), height = c(1, 0.75, 1), width = 1)
    par(mar = c(1, 4, 4, 2))
    chart.TimeSeries(R1[xax,],  main = main, xaxis = FALSE, 
        ylab = labels[1],
        event.labels = event.labels, legend.loc=legend.loc,
        ...)
    par(mar = c(1, 4, 0, 2))
    chart.TimeSeries(R2[xax,], main = "", xaxis = FALSE, 
        ylab = labels[2],
        event.labels = NULL, ...)
    par(mar = c(5, 4, 0, 2))
    chart.TimeSeries(R3[xax,],  main = "", ylab = labels[3],
        event.labels = NULL, 
        ...)
    par(op)
}

imgStart <- function(name,image_cnt,label)
  {
    picName <- paste(name,image_cnt,'.pdf',sep='')
    image_cnt=image_cnt+1

    cat("\\begin{figure}[htp]\n\\centering\n\\caption{",
        fmt(label),"}\n\\includegraphics[scale=0.7,width=22cm, height=14cm]{",picName,"}\n\\end{figure}\n\\clearpage\n",
        sep ='')
    
    pdf(picName,paper='special',width=12.5984252,heigh=5.511811024,onefile=FALSE,family='Helvetica')

    return(image_cnt)
  }

imgStart2 <- function(name,image_cnt,label)
  {
    picName <- paste(name,image_cnt,'.pdf',sep='')
    image_cnt=image_cnt+1

    cat("\\begin{figure}[htp]\n\\centering\n\\caption{",
        fmt(label),"}\n\\includegraphics[width=22cm, height=14cm]{",picName,"}\n\\end{figure}\n\\clearpage\n",
        sep ='')
    
    pdf(picName,paper='special',width=12.5984252,heigh=5.511811024,onefile=FALSE,family='Helvetica')
    return(image_cnt)
  }

imgStart3 <- function(name,image_cnt,label)
  {
    picName <- paste(name,image_cnt,'.pdf',sep='')
    image_cnt=image_cnt+1

    cat("\\begin{figure}[htp]\n\\centering\n\\caption{",
        fmt(label),"}\n\\includegraphics[width=22cm, height=14cm]{",picName,"}\n\\end{figure}\n\\clearpage\n",
        sep ='')
    
    pdf(picName,onefile=FALSE,paper='special',width=12,heigh=8,family='Helvetica')
    return(image_cnt)
  }

imgStartSmall <- function(name,image_cnt,w=12,height=6,family='Helvetica')
  {
    picName <- paste(name,image_cnt,'.pdf',sep='')
    image_cnt=image_cnt+1

    cat("\\includegraphics[width=",w,'in, height=',height,'in]{',picName,"}\n",sep ='')
    
    pdf(picName,onefile=FALSE,paper='special',width=w,height=height,family=family) 
    return(image_cnt)
  }

imgStartMini <- function(name,image_cnt,label,image2)
  {
    picName <- paste(name,image_cnt,'.pdf',sep='')
    image_cnt=image_cnt+1
    cat('
\\begin{figure}[ht] 
\\begin{minipage}[b]{0.6\\linewidth}\n\\centering\n')
    
    cat('\\caption{',
        fmt(label),'}\n\\includegraphics[scale=1.5]{',picName,'}\n\\end{minipage}\n',
        sep ='')
    cat('\\hspace{0.5cm}
\\begin{minipage}[b]{0.4\\linewidth}
\\centering
\\includegraphics{',image2,'}
\\end{minipage}
\\end{figure}
\\clearpage\n',sep='')
    
    pdf(picName,onefile=FALSE,paper='special',width=12,heigh=8,family='Helvetica')
    return(image_cnt)
  }


imgEnd <- function()
  {
    invisible(dev.off())
  }


get_portfolioLineRt <- function (con,portfolio_id, dates=c())
{
  eq='1=1'
  if (length(dates))
    {
      start=dates[1]
      end=dates[length(dates)]
      ldates=paste(",",dates,"",sep='',collapse='')
      ldates=substr(ldates,2,nchar(ldates))
      eq=paste('year(dt)*100+month(dt) in (',ldates,')',sep='')
    }
    
  sql<-paste("select year(dt)*100+month(dt) dt, rt from portfolio_rt a where portfolio_id=",portfolio_id,"  and ",eq," group by dt order by dt",sep='') ;
  message(sql)
  port_rt <- sqlQuery(con, sql)
  zprt <- zoo(port_rt$rt,port_rt$dt)

  return(zprt)
}


#       out=rt_roll_dates(name=name,rt=rt,dtstart=dtstart,period=period,years=years)

rt_roll_dates <- function(name,rt,dtstart,period,years=c())
  {
    out=data.frame(name=name)
    j <- 1
    while (j<= length(dtstart))
      {
        start=isnull(which(index(rt)==dtstart[j])[1],0)
        if (start)
          {
            end=min(start+period-1,length(rt))
            out=cbind(out,rt=as.double(Return.cumulative(rt[start:end])))
          }
        else
          {
            out=cbind(out,rt=NA) 
          }
        j <- j+1
      }
    if (length(years))
      {
        for (j in 1:length(years))
          {
            subrt=rt[substr(index(rt),1,4)==years[j]]
            if (length(subrt))
                {
                  out=cbind(out,rt=as.double(Return.cumulative(subrt)))
                } else
                {
                   out=cbind(out,rt=NA)
                }
          }
      }
      
    
    return(out)
  }

v2xts <- function(R)
{
  res=xts(R,Sys.Date()+1:length(R))
  return(res)
}


roll_regression <- function(rt,brt,len){
  dt=index(rt)[index(rt)%in%index(brt)]
  brt=brt[dt,]
  rt=rt[dt,]
  
  out=xts()
  
  zout=xts(rep(NA,length(dt)),dt)
  
  if (length(rt)>=len){
    udt=dt[len:length(dt)]
    zout=xts(rep(NA,length(udt)),udt)
    
    out=merge(alpha=zout,beta=zout,r2=zout,cor=zout,palpha=zout,pbeta=zout)
    
    i <- len
    for (i in len:length(dt)){
      sregress=summary(lm(rt[(i-len):i]~brt[(i-len):i]))
      cf=coef(sregress)
      ndx=i-len+1
      out[ndx,'alpha']=cf[1,1]
      out[ndx,'beta']=cf[2,1]
      out[ndx,'palpha']=cf[1,4]
      out[ndx,'pbeta']=cf[2,4]
      out[ndx,'r2']=sregress$r.squared
      out[ndx,'cor']=sqrt(sregress$r.squared)*sign(cf[2,1])
    }
      
  }
    
  return(out)
  
}


#calculates contribution of the component to total return
#componentRt=lrt
#componentAlloc=bucketRt$bucketAlloc[dateGroups[[j]],i]
#portRt=portRt[dateGroups[[j]]]

#componentRt=bucketFrqRt[,14]
#componentAlloc=bucketFrqAlloc[,14]
#portRt=portFrqRt
componentContrib <- function (componentRt,componentAlloc,portRt){
  if (length(componentRt)!=length(componentAlloc) ||length(componentRt)!=length(portRt))
    {
      stop("componentContrib:: inconsistent returns & allocations")
    }

  wealth=1
  contrib=0

  i <- 1
  for (i in 1:length(componentRt))
    {
      sign=1
      if (componentAlloc[i]<0)
        {
          sign=-1
        }
      contrib=contrib+wealth*as.double(componentAlloc[i])*as.double(componentRt[i])
      wealth=wealth*(1+as.double(portRt[i]))
      #message(i,' ',index(componentRt[i]),' ',contrib,' ',wealth)
    }
    
  return(contrib)
}

#portfolio returns
#RT=resRt
#weights=resAlloc
  
reconstructRt <- function (RT,weights)
{
  res=RT[,1]*weights[1]

  if (ncol(RT)>1)
    {
      for (j in 2:ncol(RT))
        {
          res=res+RT[,j]*weights[j]
        }
    }

  return(res)
    
  
}

#weights=out
#RT=param$rt
#RT=classSRt
#weights=classAlloc
reconstructRtTime <- function (RT,weights,setNA=0)
{
  nRt=RT[index(RT)%in%index(weights),]
  if (nrow(nRt)!=nrow(weights) || ncol(RT)!=ncol(weights))
    {
      stop ("reconstructRtTime::Incompatible Returns and Weights!")
    }
  res=xts(rep(0,nrow(nRt)),index(nRt))
  if (ncol(nRt)>0)
    {
      for (j in 1:ncol(nRt))
        {
          uweights=as.vector(weights[,j])
          wndx=which(uweights!=0)
          if (length(wndx))
            {
              uweights=uweights[wndx]
              urt=as.vector(nRt[wndx,j])
              urt[is.na(urt)]=setNA
              res[wndx]=as.vector(res[wndx])+urt*uweights
            }
          #print(res)
        }
      
    }

  return(res)
  
}


#reconstruct portfolio returns with allocation intervals only
#weights=res$weights
#weights=out
#RT=param$rt
reconstructRtTimeInterval <- function (RT,weights,fixedWeights=TRUE)
{
  j=which(index(RT)>index(weights)[1])[1]
  res=RT[j:nrow(RT),1]*0
  colnames(res)=c('RTime')
  ndx <- 1
  uweight=as.vector(weights[1,])
  wdates=index(res)
  if (j==1)
    {
      wdates=c(index(weights)[1],wdates)
    } else
  {
    wdates=c(index(RT)[j-1],wdates)
  }
  
  rweights=xts(array(0,c(length(wdates),ncol(RT))),wdates)
  colnames(rweights)=colnames(RT)
  rndx=1
  i <- 1
  for (i in 1:nrow(res))
    {
      if (ndx<nrow(weights))
        {
          if (index(res)[i]>index(weights)[ndx+1])
            {
              ndx=ndx+1
              uweight=as.vector(weights[ndx,])
            }
        }

      sign=rep(1,length(uweight))
      sign[uweight<0]=-1

      undx=which(uweight!=0)
      if (length(undx))
        {
          rweights[rndx,]=uweight
          useRT=as.vector(RT[j,undx])
          useRT[is.na(useRT)]=0
          rt=drop(useRT%*%uweight[undx])
          res[i]=rt
          
          if (!fixedWeights)
            {
              uweight=uweight*(1+as.vector(RT[j,]))/(1+rt)
            }
        }
      j <- j+1
      rndx <- rndx+1
    }
  
  if (ndx<nrow(weights))
    {
      if (index(rweights)[rndx]>=index(weights)[ndx+1])
        {
          rweights[rndx,]=as.vector(weights[ndx+1,])
        } else
      {
        rweights[rndx,]=uweight
      }
    } else
  {
    rweights[rndx,]=uweight
  }
  
  return(list(rt=res,weights=rweights))
  
}

#RT=optimizerW$benchRt
#weights=optimizerW$weights
#windows=c(12,24,36)
#FUN=sd.annualized


applyFUNTime <- function (RT,weights,FUN,windows)
{
  res=xts()
  dts=index(weights)
  for (w in windows)
    {
      out=c()
      odts=c()
      if (isnull(length(dts),0))
        {
          for (i in 1:length(dts))
            {
              rtndx=0 
              ndx=which(index(RT)<dts[i])
              if (length(ndx))
                {
                  rtndx=isnull(max(ndx),0)
                }                  
              wndx=i
              if (rtndx>=w) # we have data
                {
                  start=rtndx-w+1
                  rts=RT[start:rtndx,]
                  prt=xts(rep(0,w),index(rts))
                  j <- 1
                  for (j in 1:ncol(RT))
                    {
                      prt=rts[,j]*as.double(weights[wndx,j])+prt
                    }
                  out=c(out,as.double(FUN(prt)))
                  odts=c(odts,i)
                }
            }
        }
      ores=xts(out,dts[odts])
      colnames(ores)=w
      res=merge(res,ores,all=TRUE)
      
    }
    return(res)
  
}


remove_na <- function(rt,na=0)
  {
    rt[is.na(rt)]=na
    return(rt)
  }
  
  
remove_lead_na <- function (rt)
{
  i <- 1
  for (i in 1:nrow(rt))
    {
      if (sum(!is.na(rt[i,])))
          {
            break
          }
          
    }

  return(rt[i:nrow(rt),])    
      
}

fill_na <- function(rt)
{
  i <- 1
  prev_val=NA
  for (i in 1:nrow(rt))
    {
      if (!is.na(rt[i]))
          {
            prev_val=as.double(rt[i])
          } else
      {
        if (!is.na(prev_val))
          {
            rt[i]=prev_val
          }
          
      }
          
    }
  return (rt)
}



store_rt <- function (con,mret,prefix='CALC_',type='HF_INDEX')
  {
    j <- 1
    for (j in 1:ncol(mret))
      {
        fname=paste(prefix,colnames(mret)[j],sep='')
        message('STORING::',fname)
        sql=paste('select fund_id from fund where name="',fname,'" and type="BENCHMARK"',sep='')
        res=sqlQuery(con, sql)
        if (nrow(res)==0)
          {
            sql1=paste('insert into fund (name,type,class) values("',fname,'","BENCHMARK","',type,'")',sep='')
            invisible(sqlQuery(con, sql1))
            res=sqlQuery(con, sql)
          }
        id=res$fund_id[1]
        sql=paste('delete from rt where fund_id=',id)
        invisible(sqlQuery(con, sql))

        srt=remove_lead_na(mret[,j])
        message(id,' found ',length(srt),' data points ',index(srt)[length(srt)])
        for (k in 1:length(srt))
          {
            sql=paste('insert into rt (fund_id,return_type_id,dt,rt) values(',id,',7,"',paste(substr(index(srt)[k],1,7),'-01',sep=''),'",',as.double(srt[k]),')',sep='')
            #message(sql)
            invisible(sqlQuery(con, sql))
          }
        
      }

  }

#rt=price_rt
store_daily_rt<- function (con,rt,prefix='CALC_',type='CALC_INDEX')
  {
    j <- 1
    for (j in 1:ncol(rt))
      {
        fname=paste(prefix,colnames(rt)[j],sep='')
        message('STORING::',fname)
        sql=paste('select fstock_id from fstock where name="',fname,'"',sep='')
        
        res=sqlQuery(con, sql)
        if (nrow(res)==0)
          {
            sql1=paste('insert into fstock (name,ticker,type) values("',fname,'","',fname,'","',type,'")',sep='')
            invisible(sqlQuery(con, sql1))
            res=sqlQuery(con, sql)
          }
        id=res$fstock_id[1]
        sql=paste('delete from fstock_history where fund_id=',id)
        invisible(sqlQuery(con, sql))

        srt=remove_lead_na(rt[,j])
        message(id,' found ',length(srt),' data points ',index(srt)[length(srt)])
        for (k in 1:length(srt))
          {
            sql=paste('insert into fstock_history (fstock_id,px_last,dt) values(',id,',',as.double(srt[k]),',"',as.character(index(srt)[k]),'")',sep='')
            #message(sql)
            invisible(sqlQuery(con, sql))
          }
        
      }

  }


applyCummReturn <- function (rts,applyFUN=apply.monthly,FUN=Return.cumulative)
{
  res=xts()
  for (i in 1:ncol(rts))
    {
      mrt=applyFUN(rts[,i],FUN)
      res=merge(res,mrt,all=TRUE)
    }
  colnames(res)=colnames(rts)
  return(res)
}

#fname='bloomberg_credit_spreads1.txt'
#prefix='DLY:ABS:'
#postfix=''

transformtickers <- function (tickers,prefix='',postfix='')
{
  tickers <- trim(toupper(tickers))
  tickers <- gsub('\\s+','_',tickers)
  tickers <- unique(sort(tickers))
  tickers <- paste(prefix,tickers,postfix,sep='')
}


getTickers <- function (fname='/dev/null',prefix='',postfix='')
{
  tickers <- as.vector(read.csv(fname,header=FALSE)[,1])
  tickers <- transformtickers(tickers,prefix,postfix)
}

#data=benchmarksAlloc
#fill_index=ndates
backfillData <- function (data,fill_index,backward=TRUE,forward=TRUE)
{
  out=data
  
  ndx=c()
  for (fndx in fill_index)
    {
      i=isnull(which(index(data)==fndx)[1],0)
      if (i) {ndx=c(ndx,i)}
    }
  
  
  if (length(ndx))
    {
      fullndx=1:nrow(data)
      fullndx=fullndx[!(fullndx%in%ndx)]
      
      if (length(fullndx))
        {
          i <- ndx[1]
          for (i in ndx)
            {
              filled=FALSE
              if (forward)
                {
                  fill_ndx=isnull(max(fullndx[fullndx<i]),0)
                  if (fill_ndx>0)
                    {
                      out[i,]=out[fill_ndx,]
                      filled=TRUE
                    }
                }

              if (backward && !filled)
                {
                  fill_ndx=isnull(min(fullndx[fullndx>i]),0)
                  if (fill_ndx>0)
                    {
                      out[i,]=out[fill_ndx,]
                      filled=TRUE
                    } 
                }
            } 
        }
    }
  return(out)
  
}
  
convert_ids <- function (ids)
{
  dates=sort(unique(ids$dt))
  id=sort(unique(ids$fund_id))
  z=toxts(zoo(0,dates))
  all=xts()
  for (i in 1:length(id))
    {
      all=merge(all,z)
    }
  colnames(all)=id

  i <- 1
  for (i in 1:nrow(ids))
    {
      dt=dtposix(ids$dt[i])
      ndx=which(colnames(all)==ids$fund_id[i])[1]
      all[dt,ndx]=ids$alloc[i]
    }
  return(all)
}
  
longest_strip <- function (x)
{
  out=0
  ndxs=which (x>0)
  if (length(ndxs))
    {
      out=1
      if (length(ndxs)>1)
        {
          maxout=1
          for (i in 2:length(ndxs))
            {
              if (ndxs[i]==ndxs[i-1]+1)
                {
                  maxout=maxout+1
                } else
              {
                if (out<maxout){ out=maxout}
                maxout=1
              }
            }
          if (out<maxout){ out=maxout}
        }
    }
  return(out)
}

mod_mad <- function (x)
{
  return(sum(abs(x-mean(x))/nrow(x)))
  
}

annmod_mad<- function (x)
{
  return (mod_mad(x)*sqrt(12))
  
}

#x=summary
#FUN=sd.annualized
#w=12
roll_fun <- function (x,FUN,w=12)
{
  res=xts()
  if (nrow(x)>=w)
    {
      dts=index(x)[w:nrow(x)]
      for (j in 1:ncol(x))
        {
          out=c()
          for (i in w:nrow(x))
            {
              out=c(out,as.double(FUN(x[(i-w+1):i,j])))
            }
          lres=xts(out,dts)
          res=merge(res,lres,all=TRUE)
        }
      colnames(res)=colnames(x)
    }
  return(res)
}

price2rt <- function (prices,keep.na=FALSE)
{
  rt=prices[2:nrow(prices),]

  for (i in 2:nrow(prices))
    {
      lrt=as.vector(prices[i,])/as.vector(prices[i-1,])-1
      lrt[lrt==Inf]=0
      lrt[lrt==-Inf]=0
       if (!keep.na)
         {
           lrt[is.na(lrt)]=0
         }
      rt[i-1,]=lrt
    }
  return(rt)
}

price2rtxts <- function (pr)
  {
    Returns = pr/xts:::lagts.xts(pr) - 1
  }


price2Subrt <- function (prices)
{
  rt=prices[2:nrow(prices),]

  for (i in 2:nrow(prices))
    {
      lrt=as.vector(prices[i,])-as.vector(prices[i-1,])
      lrt[lrt==Inf]=0
      lrt[lrt==-Inf]=0
      lrt[is.na(lrt)]=0
      rt[i-1,]=lrt
    }
  return(rt)
}


#save workspace
saveWSpace <- function (dir)
{
  cdir=getwd()
  setwd(dir)
  save.image(file='project.RData')
  setwd(cdir)
}

loadWSpace <- function (dir)
{
  cdir=getwd()
  setwd(dir)
  load(file='project.RData')
  setwd(cdir)
}

graphCorr <- function (crr,colors=c())
  {
    
    if (!length(colors))
      {
        colors =brewer.pal(n=10,'Spectral')[10:1]
        
      }
    par(mar=c(10,10,1,1))
    prncrr=t(crr[ncol(crr):1,])
    image(1:ncol(crr), 1:ncol(crr),prncrr, axes = FALSE, col = colors,xlab='',ylab='')
    
    axis(1, at=1:ncol(crr), labels=colnames(crr), las=2, lwd=1,cex.axis=0.7)
    axis(2, at=ncol(crr):1, labels=colnames(crr), las=1, lwd=1,cex.axis=0.7)
    text(rep(1:ncol(crr), ncol(crr)), rep(1:ncol(crr), each = ncol(crr)),round(prncrr*100))
  }


graphCorrEllipse <- function (crr,colors=c())
  {

    par(las=1,
          font.main=1.5, font.lab=1.5, font.axis=1.5, cex=0.7, cex.main=0.7,              
          cex.lab=1.5, cex.axis=1.5,lwd=2, las=1)
    
    if (!length(colors))
      {
        colors =brewer.pal(n=10,'Spectral')[10:1]
        
      }
    
    col = colors[as.vector(apply(crr, 2, rank))]
    plotcorr(crr,col=col,mar=rep(0,4),cex.axis=0.7,axes=FALSE,cex=0.8,cex.lab=0.8)
    
    
  
  }
#name='lambda'
key <- function (name,hash)
  {
 return (isnull(which(keys(hash)==name),0))
}

linear_maxdrawdown <- function (RT)
  {
  draw=0
  cm=1
  cmrt=RT*0
  for (i in 1:length(RT))
  {
    if (i==1)
       {
         cmrt[i]=(1+as.double(RT[i]))
       }else
    {
      cmrt[i]=cmrt[i-1]*(1+as.double(RT[i]))
    }
  }

 draw=0
 for (i in 1:(length(cmrt)-1))
  {
    for (j in (i+1):length(cmrt))
      {
        draw=max(as.double(cmrt[i])-as.double(cmrt[j]),draw)
      }
  }
 return (draw)
}

#dates=index(param$rt)
#shift=1
#weights=res$weights
fillWeights <- function(dates,weights,shift=1)
{
  zweights=as.vector(weights[1,]*0)
  udt=dates[which(dates>=index(weights)[1])]
  out=xts(array(0,c(length(udt),length(zweights))),udt)
  wdt=index(weights)
  wdt=wdt[wdt<=udt[length(udt)]]

  for (i in 1:length(udt))
    {
      dt=max(wdt[wdt<= udt[i]])
      out[i,]=as.vector(weights[dt,])
    }
  
  if (shift)
    {
      if (nrow(out)<=shift)
        {
          out=xts()
        } else
      {
        out=xts(out[1:(nrow(out)-shift),],index(out)[(shift+1):nrow(out)])
      }
    }

  return (out)
  
}

diffMonth <- function (d1,d2)
{
 a1=as.POSIXlt(d1)
 a2=as.POSIXlt(d2)
 return((a1$year-a2$year)*12+(a1$mon-a2$mon))
}

#stolen from PerformanceAnalytics

AnnualReturns <- function (R)
{
  firstyear = as.numeric(format(strptime(as.POSIXct(time(R)[1]), 
        "%Y-%m-%d"), "%Y"))
  
  lastyear = as.numeric(format(strptime(as.POSIXct(time(R)[length(R[, 
        1])]), "%Y-%m-%d"), "%Y"))
  
  year = format(strptime(as.POSIXct(time(R)), "%Y-%m-%d"),
    "%Y")
  rowlabels = (firstyear:lastyear)
  data=data.frame(array(NA,c(ncol(R),length(rowlabels))))
  
  colnames(data)=rowlabels
  rownames(data)=colnames(R)
  
  for (column in 1:ncol(R))
    {
      for (y in 1:length(rowlabels))
        {
          rt=Return.cumulative(R[year==rowlabels[y],column])
          data[column,y]=rt
        }
    }
  
  return(data)
}

#netfees

netFees <- function (rt,mgmFee=0.02,perFee=0.2)
  {
    mgmRt=rt-((1+mgmFee)^(1/12)-1)
    draws=findDrawdowns(mgmRt)
    netRt=mgmRt
    
    for (i in 1:length(draws$return))
      {
        if (draws$return[i]<0) # in drawdown
          {
            crt=Return.cumulative(mgmRt[(draws$from[i]):(min(nrow(rt),draws$to[i]))])
            if (crt>0)
              {
                netRt[draws$to[i]]=mgmRt[draws$to[i]]-crt*perFee
              }
          } else
        {
          range=draws$from[i]:(draws$to[i]-1)
          conseq=draws$from[i]+1<draws$to[i]
          if (i>1)
            {
              range=(draws$from[i]+1):(draws$to[i]-1)
            }
          
          if (i==1 || conseq)
            {
              netRt[range]=mgmRt[range]*(1-perFee)
            }
        }
        
      }
    return(netRt)
  }


#autocorrelation

autoCorrFun <- function (R){drop(cor(as.vector(R[1:(length(R)-1)]),as.vector(R[2:length(R)])))}

naFUN <- function (R,FUN){

  if (is.na(sum(R)))
    {
      return (NA)
    }
  
  lres=try(expr=as.double(FUN(R)),silent=TRUE)
  if (nchar(isnull(attr(lres,'class'),'')))
    {
      lres=NA
    }
 return(lres)
}

naSmoothingIndex <- function (R)
{
  naFUN(R,SmoothingIndex)
}
  
insertRow_ToTestDatabase = function(report_date_dashboard,conn,upload_to_db,portname,tabname,rowname,colname,val) {
  #message(paste("insert into dashboard_test select '",report_date_dashboard,"','",portname,"','",tabname,"','",rowname,"','",colname,"','",val,"'",sep=""))
  if (upload_to_db)
    sql=paste("insert into dashboard_test values ('",dbDateFmt(report_date_dashboard),"','",portname,"','",tabname,"','",rowname,"','",colname,"','",val,"')",sep="")
     message(sql)
    sqlQuery(conn,sql)
}
    
deleteRows_FromTestDatabase = function(report_date_dashboard,conn,upload_to_db,tabname) {
  #message(paste("delete from dashboard_test where reportdate='",report_date_dashboard,"' and tabname like '%",tabname,"%'",sep=""))
  sql=paste("delete from dashboard_test where reportdate='",dbDateFmt(report_date_dashboard),"' and tabname like '%",tabname,"%'",sep="")
  message(sql)
  if (upload_to_db)
    sqlQuery(conn,sql)
}

cleanRows_FromTestDatabase = function(conn,upload_to_db,tabname) {
  #message(paste("delete from dashboard_test where reportdate='",report_date_dashboard,"' and tabname like '%",tabname,"%'",sep=""))
  if (upload_to_db)
    sqlQuery(conn,paste("delete from dashboard_test where tabname like '%",tabname,"%'",sep=""))
}




rt2xts <- function (rt)
{
  xts(rt$rt,as.POSIXct(paste(rt$dt,'01'),format='%Y%m%d'))
}

aum2xts <- function (rt)
{
  xts(rt$aum,as.POSIXct(paste(rt$dt,'01'),format='%Y%m%d'))
}

bmonth <- function(rts)
  {
    xts(rts,as.POSIXct(paste(substr(index(rts),1,4),'-',substr(index(rts),6,7),'-01',sep=''),format='%Y-%m-%d'))
  }

#daily to monthly
dly2mnth_rt <- function (rt)
  {
    cm <- cumprod(1 + rt)
    ep <- endpoints(cm, "months")
    mrt <- remove_lead_na(bmonth(CalculateReturns(cm[ep],method='simple')))
    return (mrt)
  }

rollFunImage <- function (xman,FUN,windows,label,image_cnt)
  {
    urollwindows=windows[windows<nrow(xman)]
    if (length(urollwindows))
      {
        xres=xts()
        for (w in urollwindows)
          {
            xres=merge(xres,apply.rolling(xman,width=w,FUN=FUN),all=TRUE)
          }
        colnames(xres)=urollwindows
    
        image_cnt=imgStart('INCRollFP',image_cnt,label)
        chart.TimeSeries(xres,legend.loc='topleft',main='')
        imgEnd()
        image_cnt <- image_cnt+1

      }
    return(image_cnt)
  }

#y=lrt
#x=brt
graphRegress <- function (y,x,windows=c(12,24,36),significance=TRUE,label='')
  {
    allRt=merge(y,x,all=FALSE)
    if (nrow(allRt)<min(windows))
      {
        message('Graph_beta::Not enough Data')
      }

    r2=xts()
    bsig=xts()
    beta=xts()
    alpha=xts()
    asig=xts()
    uwindows=windows[nrow(allRt)>=windows]
    w <- uwindows[1]
    colors=rainbow(length(uwindows))
    
    for (w in uwindows)
      {
        wr2=c()
        wbeta=c()
        wbsig=c()
        walpha=c()
        wasig=c()
        i <- w
        message('graph_beta::Processing ',w)
        for (i in w:nrow(allRt))
          {
            res=summary(lm(allRt[(i-w+1):i,1]~allRt[(i-w+1):i,2]))
            wbeta=c(wbeta,coef(res)[2,1])
            walpha=c(walpha,coef(res)[1,1])
            wasig=c(wasig,coef(res)[1,4])
            wbsig=c(wbsig,coef(res)[2,4])
            wr2=c(wr2,res$r.squared)
          }

        bsig=merge(bsig,xts(wbsig,index(allRt[w:nrow(allRt)])),all=TRUE)
        asig=merge(asig,xts(wasig,index(allRt[w:nrow(allRt)])),all=TRUE)
        beta=merge(beta,xts(wbeta,index(allRt[w:nrow(allRt)])),all=TRUE)
        alpha=merge(alpha,xts(walpha,index(allRt[w:nrow(allRt)])),all=TRUE)
        r2=merge(r2,xts(wr2,index(allRt[w:nrow(allRt)])),all=TRUE)
      }

    colnames(bsig)=uwindows
    colnames(asig)=colnames(bsig)
    colnames(beta)=colnames(bsig)
    colnames(alpha)=colnames(bsig)
    colnames(r2)=colnames(bsig)

    op <- par(no.readonly = TRUE)
    layout(matrix(c(1, 2,3)), height = c(1,1,1), width = 1)

    par(mar = c(1, 4, 4, 2))
    CLTimeSeries(alpha*100,ylab='Alpha',main=label,lwd=3,colors=colors,xaxis=FALSE,)
    for (i in 1:ncol(asig))
      {
        x=which(asig[,i]<0.05)
        if (isnull(length(x),0))
          {
            points(x,alpha[x,i]*100,pch=21,col=colors[i],bg=colors[i])
          }

        x=which(asig[,i]<0.1 & asig[,i]>=0.05 )
        if (isnull(length(x),0))
          {
            points(x,alpha[x,i]*100,pch=2,col=colors[i],bg=colors[i])
          }


        
      }
    
    smartlegend( x="left", y= "top", inset=0,                             #smartlegend parameters
                legend = colnames(alpha), #legend parameters 
                fill=c(colors),                        #legend parameters
                bg = "gray")                                             #legend parameters
    
    par(mar = c(1, 4, 0, 2))
    CLTimeSeries(beta*100,ylab='Beta',main='',lwd=3,colors=colors,xaxis=FALSE)
    for (i in 1:ncol(bsig))
      {
        x=which(bsig[,i]<0.05)
        if (isnull(length(x),0))
          {
            points(x,beta[x,i]*100,pch=21,col=colors[i],bg=colors[i])
          }
      }
    par(mar = c(5, 4, 0, 2))
    CLTimeSeries(r2*100,ylab='R2',main='',lwd=3,colors=colors,xlab='')
    
    par(op)
  }


#nchar changed -- need to isnull value
ncharNull <- function (val)
  {
    return (nchar(isnull(val,'')))
  }

plotOutliers <- function (x,label,N=20)
{
  y=na.omit(x)
  y=y[order(y$val),]
  if (nrow(y)>N)
    {
      y=y[c(1:(N%/%2),(nrow(y)-(N%/%2)+1):nrow(y)),]
    }
  axis_color=rev(c(rep('green',N%/%2),rep('red',N%/%2)))
  
  p <- ggplot(y,aes(reorder(name,val),val))
  p <- p + geom_bar() + coord_flip() +
    geom_text(aes(label=name,y=min(y$val),hjust=0)) + labs(x = "", y = "")+  opts(legend.position = "none") +
      scale_x_discrete(breaks=y$name, labels=round(y$val,1))  + 
        opts(title=paste(label,sep='')) +  
          opts(plot.title = theme_text(size=14, lineheight=.8, face="bold"))  +
            opts(axis.text.y = theme_text(colour=axis_color))
  
  print(p)
  
}

graphComp <- function(crt,benchRt)
  {
    tbl=round(rbind(
      table.Arbitrary(crt,metrics=c('maxDrawdown','Return.annualized','Return.cumulative','StdDev.annualized','SmoothingIndex'),metricsNames=c('Max Drawdown','Annualized Return','Cumulative Return','Annualized Vol','Smoothing Index')),
      CAPM.beta(crt,benchRt))*100,2)
    tbl=rbind(tbl,round(c(cor(crt[,1],crt[,2]),cor(crt[,1],crt[,2]))*100,0))
    rownames(tbl)[nrow(tbl)]='Correlation'

    res=summary(lm(crt[,1]~crt[,2]))
    sigA=''
    if (coef(res)[1,4]<0.05)
      {
        sigA='!'
      }
    
    sigB=''
    if (coef(res)[2,4]<0.05)
      {
        sigB='!'
      }
    
    tbl=rbind(tbl,c(paste(round(coef(res)[1,1]*100,2),sigA,sep=''),''))
    rownames(tbl)[nrow(tbl)]='Alpha'
    
    tbl=rbind(tbl,c(paste(round(coef(res)[2,1]*100,2),sigB,sep=''),''))
    rownames(tbl)[nrow(tbl)]='Beta'

    a=CLPerformanceSummaryText(R=crt,text=tbl,main='')
    
    

  }


#windows=awindows
rollCalcs <- function(data,windows,FUN,na.omit=TRUE,minw=NA)
{
  res=xts()
  pndx=which(windows>0)
  mw=minw
  if (length(pndx))
    {
      mw=min(windows[windows>0])
    }
  if (na.omit)
    {
      data <- na.omit(data)
    }
  if (nrow(data)>=mw)
    {
      names=c()
      for (w in windows)
        {
          if (w>0)
            {
              if (nrow(data)>=w)
                {
                  x=rollapply(data,width=w,FUN=FUN,by.column=FALSE)
                  res=merge(res,x,all=TRUE)
                  names=c(names,w)
                }
            } else
              {
                if (is.na(minw))
                  {
                    minw=mw
                  }
                x=data[minw:nrow(data),1]
                x[1:length(x)]=NA
                for (j in minw:nrow(data))
                  {
                    x[j-minw+1]=FUN(data[1:j,])
                  }
                res=merge(res,x,all=TRUE)
                names=c(names,'ITD')
              }
        }      
      colnames(res)=names
    }
  return(res)
}

#data=rrt
#FUN=sortinoFUN

rollCalcFrame <- function (data,w,FUN=Return.annualized)
{
  res=xts()
  for (i in 1:ncol(data))
    {
      res=merge(res,rollapply(data[,i],width=w,FUN=FUN),all=TRUE)
    }
  return (res)
}



calcRollingBeta <- function (art,windows)
{
  beta=xts()
  cor=xts()
  sig=xts()
  alpha=xts()
  asig=xts()
  outperf=xts()
  availw=c()
  mw=min(windows[windows>0])
  
  for (w in windows)
    {
      wstart=w
      if (w<0)
        {
          wstart=mw
        }
            
      if (nrow(art)>=wstart)
        {
          lbeta=art[wstart:nrow(art),1]
          lbeta[,]=NA
          lsig=lbeta
          lalpha=lbeta
          lasig=lbeta
          loutperf=lbeta
          lcor=lbeta
          rndx=1

          for (i in wstart:nrow(art))
            {
              ndx=1:i
              if (w>0)
                {
                  ndx=(i-w+1):i
                }
              cf=coef(summary(lm(as.double(art[ndx,1])~as.double(art[ndx,2]))))
              lcor[rndx]=cor(as.double(art[ndx,1]),as.double(art[ndx,2]))
              lbeta[rndx]=cf[2,1]
              lalpha[rndx]=cf[1,1]
              lsig[rndx]=0
              if (isnull(cf[2,4]<=0.05,0))
                {
                  lsig[rndx]=1
                }
              
              lasig[rndx]=0
              if (isnull(cf[1,4]<=0.05,0))
                {
                  lasig[rndx]=1
                }

              loutperf[rndx]=as.double(Return.annualized(art[ndx,1]-art[ndx,2]))
              rndx <- rndx+1
            }

          beta=merge(beta,lbeta,all=TRUE)
          sig=merge(sig,lsig,all=TRUE)
          alpha=merge(alpha,lalpha,all=TRUE)
          asig=merge(asig,lasig,all=TRUE)
          outperf=merge(outperf,loutperf,all=TRUE)
          cor=merge(cor,lcor,all=TRUE)
          if (w<0)
            {
              availw=c(availw,'ITD')
            } else
              {
                availw=c(availw,w)
              }
        }
    }

  colnames(beta)=availw
  colnames(sig)=availw
  colnames(alpha)=availw
  colnames(outperf)=availw
  colnames(asig)=availw
  colnames(cor)=availw
  
  
  return(list(beta=beta,sig=sig,alpha=alpha,asig=asig,outperf=outperf,cor=cor))
}

dt_align <- function (data,dates,align=TRUE)
  {
    if (align)
      {
        miss=index(dates)[!index(dates)%in%index(data)]
        if (length(miss))
          {
            mxts <- xts(matrix(1*NA,length(miss),NCOL(data)),as.POSIXct(miss))
            data=rbind(data,mxts)
          }
      }
    return(data)
  }


#res=beta
#colors=acolors
plotSig <- function (res,sig,colors,ylab='',xlab='',mark_significance=1,show.legend=TRUE,legend.horiz='left',legend.vertical='top',mar=c(1,0.8,0,0),cex=0.7,width=NA,height=NA,kill.inf=TRUE,lwd=1)
  {
    par(mar=mar,cex=cex,mgp=c(0.1,0.1,0))
    if (kill.inf)
      {
        res[res==-Inf | res==Inf]=NA
      }
    
    CLTimeSeries(res,ylab=ylab,xlab='',main='',lwd=lwd,colors=colors)
    
    if (mark_significance)
      {
        if (isnull(ncol(sig),0)>0)
          {
            for (i in 1:ncol(sig))
              {
                x=which(sig[,i]!=0)
                if (isnull(length(x),0))
                  {
                    points(x,res[x,i],pch=21,col=colors[i],bg=colors[i])
                  }
              }
          }
      }

    if (show.legend)
      {
        smartlegend( x="left", y= "top", inset=0,                             #smartlegend parameters
                legend = colnames(res), #legend parameters 
                fill=c(colors),                        #legend parameters
                bg = "gray")                                             #legend parameters
      }
  }

#return panel from FOSS Blog
#all_Rt=y
plotPanel <- function (all_Rt,p=0.95,FUN="VaR",method="gaussian")
  {
    
    my.panel <- function(x, y, lwd, ..., pf = parent.frame())
      {
        abline(h = 0, col = "grey", lty = 2, lwd = 2)
        risk=y*NA
        if (length(y)>36) risk = apply.rolling(y/100, width = 36, FUN = FUN, p = p,method = method) 
        lines(x = .index(risk),y = coredata(risk*100), type="l", col="gray60")
        plus.minus.colors <- ifelse(y < 0, 'gray','green4')
        lines(x, y, type="h", col=plus.minus.colors, lwd = lwd)
      }

    y=all_Rt
    y[is.na(y)]=0
    par(mar=c(0,0,0,0),cex=0.8)

    plot(y*100, panel = my.panel,
         layout = matrix(c(rep(1,6),rep(2,6),rep(3,6),rep(4,6),rep(5,6),rep(6,6)), byrow = TRUE, ncol = 2),
         yax.loc = "left", lwd = 3, main = "",
         blocks = list(start.time = crisis_start,
                         end.time = crisis_end,
                         col = "lightblue1"),major.format="%b%y")
  }

plotDoublePanel <- function (all_Rt,p=0.95,FUN="VaR",method="gaussian")
  {
    
    my.panel <- function(x, y, lwd, ..., pf = parent.frame())
      {
        abline(h = 0, col = "grey", lty = 2, lwd = 2)
        risk=y*NA
        if (length(y)>36) risk = apply.rolling(y/100, width = 36, FUN = FUN, p = p,method = method) 
        lines(x = .index(risk),y = coredata(risk*100), type="l", col="gray60")
        plus.minus.colors <- ifelse(y < 0, 'gray','green4')
        lines(x, y, type="h", col=plus.minus.colors, lwd = lwd)
      }

    y=all_Rt
    y[is.na(y)]=0
    
    
    par(mar=c(0,0,0,0),cex=0.5)

    plot(y*100, panel = my.panel,
         layout = matrix(c(1,1,2,2), byrow = TRUE, ncol = 2),
         yax.loc = "left", lwd = 3, main = "",
         blocks = list(start.time = crisis_start,
                         end.time = crisis_end,
                         col = "lightblue1"),major.format="%b%y")
  }

plotPanelAll <- function (all_Rt,main='',...)
  {
    
    my.panel <- function(x, y, lwd, ..., pf = parent.frame())
      {
        abline(h = 0, col = "grey", lty = 2, lwd = 2)
        plus.minus.colors <- ifelse(y < 0, 'gray','green4')
        lines(x, y, type="h", col=plus.minus.colors, lwd = lwd)
      }

    plot(all_Rt, panel = my.panel,main=main,
         #layout = matrix(c(rep(1,2),rep(2,2),rep(3,2),rep(4,2),rep(5,2),rep(6,2)), byrow = TRUE, ncol = 2),
         yax.loc = "left", lwd = 3,
         blocks = list(start.time = crisis_start,
                         end.time = crisis_end,
                         col = "lightblue1"),major.format="%b%y",...)
  }


#components
#rt=frt
#allocation=allocations

#wrapper for pcamethods
#
component_analysis = function(rt,allocation,method='svd',np=NA,maxSteps=1000,norm_var=NA) 
  {
    if(is.na(np))
      {
        np=ncol(rt)
      }
    
    base=xts()
    allocs=c()

    resPCA <- pca(rt, method = method, center = FALSE, nPcs = np,maxSteps=maxSteps)
    base=resPCA@scores
    allocs=rep(0,ncol(base))
    for (i in 1:length(allocation))
      {
        allocs=allocs+resPCA@loadings[i,]*allocation[i]
      }

    if (!is.na(norm_var))
      {
        for (i in 1:ncol(base))
          {
            bvar=var(base[,i])
            if (bvar>0)
              {
                mult=sqrt(norm_var/bvar)
                base[,i]=base[,i]*mult
                allocs[i]=allocs[i]/mult
              }
          }
      }
        
    base=xts(base,index(rt))
    base=merge(base,xts(rt%*%allocation-base%*%allocs,index(base)))
    allocs=c(allocs,1)
    colnames(base)[ncol(base)]='ERR'
    names(allocs)[ncol(base)]='ERR'
    return(list(allocs=allocs,base=base))
}

# rolled component analysis
# rts=ard$Returns
# allocations=ard$Allocations

componentRisk <- function (type='var',rt,weights,p,clean,method)
  {
    components=c()
    total=c()
    if (type=='var')
      {
        var=VaR(rt,p=p,weights=weights,portfolio_method='component',clean=clean,method=method)
        components=as.double(var$pct_contrib_VaR)
        total=as.double(var$VaR)
      } else
        {
          es=ES(rt,p=p,weights=weights,portfolio_method='component',clean=clean,method=method)
          components=as.double(es$pct_contrib_ES)
          total=as.double(es$ES)
        }
    return(list(components=components,total=total))
 }

#allocations=ard$Allocations

rolled_component_analysis = function(rts,allocations,p,clean,w=24,start_ndx=NA,method='svd',np=NA,maxSteps=1000,measure='var',risk.method='gaussian',norm_var=NA) 
  {
    if(is.na(np))
      {
        np=ncol(rt)
      }

    ndx=max(isnull(start_ndx,0),w)
    
    base=xts()
    allocs=c()

    xz=xts(rep(0,nrow(allocations)-ndx+1),index(allocations)[ndx:nrow(allocations)])
    xpca=xts()
    for (i in 1:(np+1))
      {
        xpca=merge(xpca,xz)
      }

    colnames(xpca)=c(paste('PC',1:np,sep=''),'ERR')
    xpca_allocs=xpca
    

    i <- ndx
    for (i in ndx:nrow(allocations))
      {
        start=i-w+1
        end=i

        allocs=as.double(allocations[i,])
        rt=rts[start:end,allocs!=0]
        allocs=allocs[allocs!=0]

        if (length(allocs))
          {
            comps=component_analysis(rt,allocs,method=method,np=np,maxSteps=1000,norm_var=norm_var)
            cr=componentRisk(measure,comps$base,comps$alloc,p,clean,risk.method)
            dndx=i-ndx+1
            xpca[dndx,ncol(xpca)]=cr$components[length(cr$components)] #err
            uord=order(cr$components[1:(length(cr$components)-1)],decreasing=TRUE)
            xpca[dndx,1:(length(cr$components)-1)]=cr$components[uord]
          }
      }

    return(xpca)
  }


make.table <- function
(
	nr,	# number of rows
	nc	# number of columns
)
{
	savepar = par(mar = rep(1, 4))
	plot(c(0.5, nc*2 + 0.5), c(-0.5, -(nr + 0.5)), xaxs = 'i', yaxs = 'i', 
		type = 'n', xlab = '', ylab = '', axes = FALSE)
    savepar
}


draw.cell <- function
(
	title,				# text to draw in this cell
	r,					# row
	c,					# column
	text.cex = 1,		# size of text
	bg.col = 'white',	# background color
	frame.cell = T		# flag to draw border around this cell
)
{
	if(!frame.cell) bcol = bg.col else bcol = 'black'
    rect((2*(c - 1) + .5), -(r - .5), (2*c + .5), -(r + .5), col = bg.col, border = bcol)        
    
    if( c == 1) { # first column
    	text((2*(c - 1) + .5), -r, title, adj = 0, cex = text.cex)     
    } else if( r == 1 ) { # first row
    	text((2*(c - 1) + .5), -r, title, adj = 0, cex = text.cex)        
    } else {
    	text((2*c + .5), -r, title, adj = 1, cex = text.cex)
    }
}

plotTable <- function
(
	res,
        text.cex=1,
        label.cex=1,
        padding='   '
)
{
  oldpar = make.table(nrow(res)+1, ncol(res)+1)

  draw.cell( '', 1, 1, frame.cell=TRUE,text.cex=label.cex,bg='lightGrey')
  
  for(r in 1:nrow(res)) 
    {
      draw.cell( paste('  ',rownames(res)[r]), r+1, 1, frame.cell=TRUE,text.cex=label.cex,bg='lightGrey')
    }

    for(c in 1:ncol(res)) 
    {
       draw.cell( paste('  ',colnames(res)[c]), 1, c+1, frame.cell=TRUE,text.cex=label.cex,bg='lightGrey')
     }

  for(r in 1:nrow(res)) {
    for(c in 1:ncol(res)) {
      draw.cell( paste(res[r,c],padding,sep=''), r+1, c+1, frame.cell=TRUE,text.cex=text.cex
 )
   }
 }
  
}


fundRtFUNS <- c(
                 function (x) { if (nrow(x)<12) Return.cumulative(x[,1]) else Return.annualized(x[,1])},
                 function (x) { if (nrow(x)>11) sd.annualized(x[,1]) else NA},
                 function (x) { if (nrow(x)>11) cov(x[,1],x[,3])/var(x[,3]) else NA },
                 function (x) { if (nrow(x)>11) cov(x[,1],x[,4])/var(x[,4]) else NA },
                 function (x) { mean(na.omit(x[,5]))},
                 function (x) { if (nrow(x)>11) skewness(x[,1]) else NA},
                 function (x) { if (nrow(x)>11) PerformanceAnalytics::kurtosis(x[,1],method='excess') else NA},
                 function (x) { if (nrow(x)>11) Omega(x[,1]) else NA},
                 function (x) { if (nrow(x)>11) SmoothingIndex(x[,1]) else NA},
                 function (x) { maxDrawdown(x[,1])},
                 function (x) { if (nrow(x)>11) cor(x[,1],x[,2]) else NA}
                )
                 
fundRtLabels <- c(
                 'Ann.Ret',
                 'Ann.Vol',
                 'SPX Beta',
                 'HY Beta',
                 'Avg(AUM)',
                 'Skew',
                 'EKurtosis',
                 'Omega',
                 'Smoothing',
                 'Max Drawdown',
                 'Correlation'
                  )

fundRtMult <- c(
                 100,
                 100,
                 100,
                 100,
                 1/1000000,
                 1,
                 1,
                 1,
                 100,
                 100,
                 100
          )


                 
#  FUNS=fundRtFUNS
# labels= fundRtLabels
# rt=ubrt1
calcMeasuresCalendar <- function (rt,FUNS,labels,windows,process_years=TRUE)
  {
    start=c()
    end=c()
    names=c()
    for (w in windows)
      {
        if (w<0)
          {
            start=c(start,1)
            names=c(names,'ITD')
          } else
        if (nrow(rt)>=w)
          {
            start=c(start,nrow(rt)-w+1)
            names=c(names,paste(w,'M',sep=''))
          }
      }
    end=rep(nrow(rt),length(start))

    if (process_years)
      {
        years=format(index(rt),'%Y')
        uyears=rev(sort(unique(years)))
        
        for (y in uyears)
          {
            astart=min(which(years==y))
            aend=max(which(years==y))
            start=c(start,astart)
            end=c(end,aend)
            if (aend-astart+1!=12)
              {
                names=c(names,paste(y,'+',sep=''))
              } else
            {
              names=c(names,paste(y))
            }
          }
      }
    res=matrix(NA,length(FUNS),length(names))
    colnames(res)=names
    rownames(res)=labels

    for (j in 1:length(names))
      for (i in 1:length(labels))
        {
          res[i,j]=FUNS[[i]](rt[start[j]:end[j],])
        }

    return(list(calcs=res,
                labels=labels,
                names=names,
                windows=end-start+1
                ))
    
  }


CLMaxDrawdown <-function (R)
{
  res=0
  if (length(R)) res=-min(Drawdowns(R))
  return(res)
}

CLCalcReturn <- function (mval)
  {
    res=xts()
    for (j in 1:ncol(mval))
      {
        res=merge(res,na.omit(xts(mval[,j]/lag(mval[,j])-1,index(mval))),all=TRUE)
      }
    return(res)
  }

trim <- stringr::str_trim
