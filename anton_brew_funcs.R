plot_data_to_output = function(filename) {
	png(filename)
	plot(1:5,1:5)
	dev.off()
#	write.table(1:3,filename)
}

plot_data_to_binary = function(dat,path,img_format_func,file_extension) {
#	filename = paste(path,"img_",runif(1)*1000000000,".",file_extension,sep="")
	filename = tempfile()
	img_format_func(filename)
	plot(dat)
	dev.off()
	con=file(filename, "rb")
	img_bin = paste(readBin(con,what="raw",n=1e7),collapse="-")
	close(con)
	unlink(filename)
	return(img_bin)
}

open_file_for_binaryImage = function(img_format_func, wid, hei) {
	filename = tempfile()
	img_format_func(filename, width=wid, height=hei)
	return(filename)
}

close_file_get_binaryImage = function(filename) {
	dev.off()
	con = file(filename, "rb")
	img_bin = paste(readBin(con,what="raw",n=1e7),collapse="-")
	close(con)
	unlink(filename)
	return(img_bin)
}

close_file_get_binaryImage_RAW = function(filename) {

        dev.off()

        con = file(filename, "rb")

        #img_bin = readBin(con,what="raw",n=1e7)
        img_bin = readBin(con,what="raw",n=file.info(filename)$size)

        close(con)

        unlink(filename)

        return(img_bin)

}

close_file_get_binaryImage_PARAMS = function(params,filename) {
	if(params$ImageFormat == "base64")
		base64(close_file_get_binaryImage_RAW(filename))
	else if(params$ImageFormat == "Raw readBin")
		close_file_get_binaryImage_RAW(filename)
	else if(params$ImageFormat == "String bytes")
		close_file_get_binaryImage(filename)
        else close_file_get_binaryImage(filename)
        
}



dataframe_to_xml = function(df) {
	res = "<GRID>"
	rown = dim(df)[1]
	coln = dim(df)[2]
	cols = colnames(df)
	for(i in 1:rown){		
		res = paste(res,"<ROW>",sep="")
		for(j in 1:coln)
			res = paste(res,"<",cols[j],">",df[i,j],"</",cols[j],">",sep="")
		res = paste(res,"</ROW>",sep="")
			
	}
	res = paste(res,"</GRID>",sep="")

	res
}


dataframe_to_xml_NAMED = function(df, grid_name) {
	#res = paste('<DATA Type="dataframe" Name="',grid_name,'">',sep='')
	rown = dim(df)[1]
	coln = dim(df)[2]

	cols = colnames(df)
	cols_str_open = paste('<',cols,'>',sep='')
	cols_str_close = paste('</',cols,'>',sep='')
	if('Date'%in%cols)
		df$Date = as.character(df$Date)

	row_strs = array('',rown)
	for(i in 1:rown){		
		#row_str = paste('<ROW ID="',i,'">',sep='')
		#for(j in 1:coln)
		#	row_str = paste(row_str,"<",cols[j],">",df[i,j],"</",cols[j],">",sep="")
		
		row_strs[i] = paste('<ROW ID="',i,'">',paste(cols_str_open,df[i,],cols_str_close,sep='',collapse=''),'</ROW>',sep='')
		#if(i==1){ print(df[i,]); print(row_strs[i]); }

		#res = paste(res,row_str,"</ROW>",sep="")
			
	}
	#res = paste(res,"</DATA>",sep="")
	res = paste('<DATA Type="dataframe" Name="',grid_name,'">',paste(row_strs,collapse=''),"</DATA>",sep="")

	res
}


dataframe_to_xml_PARAMS = function(df, att_names, att_vals) {
	#res = paste('<DATA Type="dataframe" Name="',grid_name,'">',sep='')
	rown = dim(df)[1]
	coln = dim(df)[2]

	cols = colnames(df)
	cols_str_open = paste('<',cols,'>',sep='')
	cols_str_close = paste('</',cols,'>',sep='')
	if('Date'%in%cols)
		df$Date = as.character(df$Date)

	row_strs = array('',rown)
	for(i in 1:rown)		
		row_strs[i] = paste('<ROW ID="',i,'">',paste(cols_str_open,df[i,],cols_str_close,sep='',collapse=''),'</ROW>',sep='')

	res_atts = ''
	for(i in 1:length(att_names)) res_atts=paste(res_atts,' ',att_names[i],'="',att_vals[i],'" ',sep='')
	res = paste('<DATA Type="dataframe" ',res_atts,'>',paste(row_strs,collapse=''),"</DATA>",sep="")

	res
}





