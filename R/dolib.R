# fill up graph properties structure
setgp<- function(mdd, mdf, series, timings, measures, dims, filters, gtype) {
	gp<-list()
	gp$gfdim<-length(mdd)
	gp$series<- series
	gp$nseries<- length(series)
	gp$title<- mdf[[1]]$md_name
	if(length(mdf) > 1)
	for(i in 2:length(mdf))
		gp$title<- paste0(gp$title, ", ", mdf[[i]]$md_name)
	gp$title<-paste0(gp$title, " by ", mdd[[1]]$md_name)
	if(gp$gfdim > 1)
	for(i in 2:length(mdd))
		gp$title<- paste0(gp$title, ", ", mdd[[i]]$md_name)
	gp$seriestimings<- timings
	gp$meas<-measures
	gp$dim<-dims
	gp$mdd<- mdd
	gp$mdf<- mdf
	gp$drills<- filters
	gp$gtype<- gtype

	gp
	}

getmddf<- function(M, D, measures, dims) {
	if(!is.null(dims))
		mdd<- getdims(M$cfg, dims)
	else
		mdd<- NULL
	if(!is.null(measures))
		mdf<- getmeas(M$cfg, measures)
	else
		mdf<- NULL
#	else {
#		mdf<- getdummymeas(dims)
#		measures<- c("dummy")
#		}
	return(list(mdd=mdd, mdf=mdf, measures=measures))
	}

