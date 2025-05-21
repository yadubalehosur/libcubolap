#' @title dograph
#' @description create an aggregate dataframe from selected measures, dimensions and filters
#' @param M is database connection for Meta data
#' @param D is database connection fro data
#' @param measures are measures selected
#' @param dims are dimensions selected
#' @param filters are filters selected
#' @param gtype is graph type
#' 
#' @import dplyr
#' @import DBI
#' @import RMySQL
#' @import libcubmeta
#' @export
#'
dograph<- function(M, D, measures, dims, filters, gtype) {

	mddf<- getmddf(M, D, measures, dims) 
	mdd<- mddf$mdd; mdf<- mddf$mdf; measures<- mddf$measures

	series<- c()
	timings<- c()
	for(i in 1:length(measures)) {
		if(grepl("]", mdf[[i]]$md_column)) 
			timings[i]<- system.time(s<- calcol(M, D, mdf[[i]], mdd, filters))[3]
		else
			timings[i]<- system.time(s<- doseries(M, D$mydata, mdf[[i]], mdd, filters))[3]
		thisdxy<- s$dxy
		series[i]<- s$series
		if(i>1) {
			dxy<- merge(dxy, thisdxy, all=T)	
			}
		else
			dxy<- thisdxy
		}

	gp<- setgp(mdd, mdf, series, timings,  measures, dims, filters, gtype)
	#gp$gfid<- addgraph(gp)

	return(list(dxy=dxy, gp=gp))
	}

doseries<- function(M, my_data, mdf, mdd, filters) {
#	g<- isolate(rg$g)
	sel<- makeaggsel(M, mdf, mdd, filters)
	dxy<-dbGetQuery(my_data, sel)
	my_cfg<- M$mycfg
	if(ncol(dxy)<8)
		series<- addxy(my_cfg, dxy)
	addseries(my_cfg, series, M$uid, mdf, mdd, filters)
	if(!is.null(mdd))
		dimnames<- c(sapply(mdd, '[[', 'md_name'))
	if(!is.null(mdf))
		measnames<- mdf$md_name
	colnames(dxy)<- c(dimnames, measnames)
	return(list(dxy=dxy, series=series))
	}

aggcol<- function(tab, col, aggfun) {
		paste0(aggfun, '(', ifelse(isacol(col), paste0(tab, "."), ""), col, ')')
	}

makeagg<- function(mdf, mdd) {
	if(is.null(mdd))
		"count(*)"
	else
		aggcol(mdf$md_table, mdf$md_column, mdf$md_sumcnt)
	}

makeaggsel<- function(M, mdf, mdd, filters) {

	sp<- olapdims(M, mdf, mdd, filters)
	
	agg<- makeagg(mdf, mdd)

	sel<- paste("select", sp$dimcols, ",", agg , "from", sp$frm , ifelse(!is.null(sp$w), paste("where", sp$w), ''), "group by", sp$dimcols)
#	if(!is.null(f$having))
#		sel<- paste(sel, f$having)
	print(sel)
#	qry$sql<- sel
	sel
	}
