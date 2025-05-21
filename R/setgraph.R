#' @title setrepid
#' @description retrieve a saved report into the graph structure
#' @details returns the graph structure that stores all the graph properties retrived for a report
#' @param cfg is config/meta data database connection
#' @param repid is report id to be retrieved
#' @export
setrepid<- function(cfg, repid) {
	g<-	getxy(cfg, repid)
	# series
	series<- g$gp$gseries
	dims<- as.numeric(g$gp$xids)
	measures<-c()
	for(i in 1:length(series)) {
#		q<- paste('select ds_yid from data_series where ds_series_id=', series[i])
#		measures[i]<- dbGetQuery(configdb, q)$ds_yid
		m<- filter(cfg$data_series, .data$ds_series_id==!!series[i]) %>% select(.data$ds_yid)
		m<- as.data.frame(m)
		measures[i]<-m[1,1]
		}

	d<- dims
	mdd<- getdims(cfg, d)
	mdf<- getmeas(cfg, measures)

	gp<- list()
	gp$gfid<- g$gp$gfid
	gp$gtype<- g$gp$gtype
	gp$gfi<- g$gp$gfi
	gp$gfdim<- g$gp$gfdim
	gp$title<- g$gp$gftitle
	gp$series<- series
	gp$nseries<- length(series)
	gp$measures<-measures
	gp$dims<-dims
	gp$mdd<- mdd
	gp$mdf<- mdf

	f<- setfilters(cfg, series[1])

	return(list(dxy=g$dxy, gp=gp, f=f))
	}
	
setfilters<- function(cfg, dsid) {
# select ds_filtids from data_series where ds_series_id=id
	fhids<- as.data.frame(filter(cfg$data_series, .data$ds_series_id==dsid)  %>% select(.data$ds_filtids))
	if(is.null(fhids) | fhids == '') return(NULL)
	fhids<- strsplit(fhids[1,1], ',')[[1]]
	f<- list()
	for(i in 1:length(fhids)) {
		fhid<- as.integer(fhids[i])
		fitem<- as.data.frame(filter(cfg$filter_header, .data$fh_id==fhid) %>% select(.data$fh_itemid, .data$fh_excl))
		fvals<- as.data.frame(filter(cfg$filter_details, .data$fd_id==fhid) %>% select(.data$fd_value))
		fvals<- fvals[,1]
		f[[i]]<- list(fitem, fvals)
		}
	return(f)
	}

# unused function
#setgraph<- function(cfg, repfold, reprow) {
#	if(is.null(repfold))
#		return(NULL)
#	repl<- replist(cfg, repfold)
#	if(is.null(reprow))
#		reprow<-1
#	repid<-repl[reprow,1]
#	if(is.null(repid) || is.na(repid))
#		return(NULL)
#	setrepid(cfg, repid)
#	}
