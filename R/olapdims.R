# create where clause from selected dimensions and filters
olapdims<- function(M, mdf, mdd, filters) {
	dimcols<- makedimcols(mdf, mdd)
	atabs<- c(tabsrefered(mdf), filtertabs(filters))
	f<- makefilters(mdf, mdd, filters)
	j<- makejoins(M$mycfg, mdf, mdd, atabs)
	w<- makewhere(j$jo, f$filts)
	frm<- makefrom(mdf, mdd, atabs, j$sftabs)
	mdw<- mdf$md_where
	if(!is.null(mdw) & mdw != '') {
		if(!is.null(w))
			w<- paste(w, "and ", mdw)
		else
			w<- mdw
		}
	return(list(dimcols=dimcols, frm=frm, w=w))
	}

makefrom<- function(mdf, mdd, atabs, sftabs) {
	t<- c()
	j<- 1; t[1]<- mdf$md_table
	dtabs<- unique(c(sapply(mdd, `[[`, 'md_table'), atabs))
	dtabs<- unique(c(dtabs, sftabs))
	for(i in seq_along(dtabs))
		if(dtabs[[i]] != 'time') {
			j<- j+1
			t[j]<- dtabs[[i]]
			}
	t<- unique(t)
	f<- t[1]
	if(length(t)>1)
	for(i in 2:length(t))
		f<- paste(f, ",", t[i])
	f
	}

makewhere<- function(j, filters) {

	if(is.null(j))
		j<- filters
	else
		if(!is.null(filters))
			j<- paste(j, "and", filters)
	return(j)
	}

isacol<- function(colname) {
	grepl('^[A-Z][A-Z0-9_]*$', colname, ignore.case=T)
	}

makedimcol<- function(mdf, mdd) {
	if(mdd$md_table != 'time')
		paste0(ifelse(isacol(mdd$md_column), paste0(mdd$md_table, "."),  ''), mdd$md_column)
	else
		paste0(mdd$md_column, '(', mdf$md_table, '.', mdf$md_timecol, ')')
	}

makedimcols<- function(mdf, mdd) {
	if(is.null(mdd))
		return(mdf$md_column)
	else {
		cols<- makedimcol(mdf, mdd[[1]])
		if(length(mdd) > 1)
			for(i in 2:length(mdd))
				cols<- paste(cols, ",", makedimcol(mdf, mdd[[i]]))
		cols
		}
	}

tabsrefered<- function(mdf) {
	ftab<- mdf$md_table
	src<- paste(mdf$md_column, mdf$md_where)
	t<-gregexpr('[a-zA-Z][a-zA-Z0-9_]*\\.', src)[[1]]
	j<- 0
	rtabs<- c()
	if(t[1]>0) {
		for(i in seq_along(t)) {
			tab<- substr(src, t[i], t[i]+attr(t, "match.length")[i]-2)
			if(tab != ftab) {
				j<- j+1
				rtabs[j]<- tab
				}
			}
		return(unique(rtabs))
		}
	else
		return(NULL)
	}
