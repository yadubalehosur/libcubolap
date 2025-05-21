# create where clause of SQL based on filters selected
makefilters<- function(mdf, mdd, filters) {
	if(is.null(filters)) return(NULL)
	filts=NULL ; j=0; having=NULL
	if(length(filters)>0)
	for(i in 1:length(filters)) {
		f<- makefilter(mdf, filters[[i]])
		measdim<- f$measdim
		filt<- f$filt
		if(measdim=='dim') {
			if(i == 1)
				filts<- filt
			else
				filts<- paste(filts, "and",  filt)
			}
		else {
			if(j == 0) {
				j<- 1
				having<- filt
				}
			else
				having<- paste(having, "and", filt)
			}
		}
	return(list(filts=filts, having=having))
	}

makefilter<- function(mdf, filter) {
	mdtl<- filter$mdtl
	mdtable<-mdtl$md_table
	mdcol<-mdtl$md_col
	if(!is.null(filter$fexcl) && filter$fexcl==T)
		exclude<- " not "
	else
		exclude<- ""
	filter<- filter$fval
	filt<- getfilt(filter, mdtl)

	mdsumcnt<-mdtl$md_sumcnt
	if(is.null(mdsumcnt)) {
		measdim<- 'dim'
		if(mdtable != 'time') {
			col<- paste0(mdtable, ".", mdcol)
			filt<- paste0(col, exclude, filt)
			}
		else {
			filt<- paste0(mdcol, '(', mdf$md_table, '.', mdf$md_timecol, ')', exclude, filt)
			}
		}
	else {
		measdim<- 'meas'
		filt<- paste("having", aggcol(mdtable, mdcol, mdsumcnt), filt)
		}
	return(list(measdim=measdim, filt=filt))
	}

filtertabs<- function(filters) {
	ftab<- c()
	if(!is.null(filters))
		for(i in 1:length(filters))
			ftab[i]<- filters[[i]]$mdtl$md_table
	return(ftab)
	}
getfilt<- function(filter, mdtl) {
	filt<- paste0(' in (')
	for(i in seq_along(filter)) {
		if(i > 1)
			filt<- paste(filt, ",")
		filt<- paste0(filt, '"', filter[i], '"')
		}
	filt<- paste(filt, ")")
	}
