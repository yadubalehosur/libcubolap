# calculated aggregate column value specified as formula
calcol<- function(M, D, mdf, mdd, filters) {
	col<- mdf$md_column
	start<-gregexpr("\\[", col)
	end<-gregexpr("]", col)

	for(i in 1:length(start[[1]])) {
		midi<-substr(col, start[[1]][i]+1, end[[1]][i]-1)
		mdfi<- getmeas(M$cfg, midi)
		if(!grepl("]", mdfi[[1]]$md_column)) 
			dxyi<- doseries(M$mycfg, D$mydata, mdfi[[1]], mdd, filters)$dxy
		else
			dxyi<- calcol(M, D, mdfi[[1]], mdd, filters)$dxy

		if(i > 1)
			dxy<- merge(dxy, dxyi)
		else
			dxy<- dxyi
		}
	ndim<-length(mdd)
	e<-''
	for(i in 1:length(start[[1]])) {
		e1<- substr(col, ifelse(i==1, 1, end[[1]][i-1]+1), start[[1]][i]-1)
		e<- paste0(e, e1, "dxy[,", ndim+i, "]")
		}
	e<- paste0(e, substr(col, end[[1]][i]+1, 999))
	dxy<- cbind(dxy, eval(parse(text=e)))
	start<-ndim+1
	end<-ncol(dxy)-1
	dxy[start:end]<- list(NULL)
	cnames<-c()
	for(i in 1:length(mdd))
		cnames[i]<- mdd[[i]]$md_name
	cnames[i+1]<-mdf$md_name
	colnames(dxy)<-cnames
	addxy(M$mycfg, dxy)
	series<- addseries(M$mycfg, mdf, mdd, filters)
	return(list(dxy=dxy,series=series))
	}
