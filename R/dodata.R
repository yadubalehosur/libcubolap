#' @title dodata
#' @description create a dataframe that contain rows from selected measures, dimensions and filters
#' @param M is database connection for Meta data
#' @param D is database connection fro data
#' @param measures are measures selected
#' @param dims are dimensions selected
#' @param filters are filters selected
#' @param gtype is graph type
#' 
#' @import dplyr
#' @import RMySQL
#' @export
#'
dodata<- function(M, D, measures, dims, filters, gtype) {

	mddf<- getmddf(M, D, measures, dims) 
	mdd<- mddf$mdd
	mdf<- mddf$mdf
	measures<- mddf$measures

	dxy<- dodtls(M, D$mydata, mdf, mdd, filters)

	gp<- setgp(mdd, mdf, NULL, NULL,  measures, dims, filters, gtype)

	return(list(dxy=dxy, gp=gp))
	}

dodtls<- function(M, my_data, mdf, mdd, filters) {
	sel<- makedtlsel(M, mdf, mdd, filters)
	dxy<-dbGetQuery(my_data, sel)
	if(!is.null(mdd))
		dimnames<- c(sapply(mdd, '[[', 'md_name'))
	if(!is.null(mdf))
		measnames<- c(sapply(mdf, '[[', 'md_name'))
	colnames(dxy)<- c(dimnames, measnames)
	return(dxy)
	}

makedtlsel<- function(M, mdf, mdd, filters) {

	sp<- olapdims(M, mdf[[1]], mdd, filters)
	meascols<- paste(sapply(mdf, '[[', 'md_column'), collapse=",")
	sel<- paste("select", sp$dimcols, ",", meascols, "from", sp$frm , ifelse(!is.null(sp$w), paste("where", sp$w), ''))
	print(sel)
	sel
	}
