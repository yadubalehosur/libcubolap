#' @title savefilter
#' @description save a filter into database
#' @details filter_header and filter_details tables are written
#' @param my_cfg is meta data/config database connection
#' @param uid is the logged in user id
#' @param fil is the filter detail
#' @param fname is the name of the filter to be saved
savefilter<- function(my_cfg, uid, fil, fname) {
	fid<- savefilterhdr(my_cfg, uid, fil, fname) 
	savefilterdtl(my_cfg, fil, fid)
	}
savefilterhdr<- function(my_cfg, uid, fil, fname) {
	fmdid<- fil$mdtl$md_id
	fid<- next_filter_seq(my_cfg) 
	fexcl<- fil$fexcl
	ftype<- 8
	foper<- 61
	fuid<- uid
	ftime<- as.numeric(Sys.time())
	fh<- as.data.frame(cbind(fid[1], fname[1], ftype[1], foper[1], fmdid[1], fexcl[1], fuid[1], ftime[1]))
	colnames(fh)<- c('fh_id', 'fh_name', 'fh_type', 'fh_oper', 'fh_itemid', 'fh_excl', 'fh_user', 'fh_modifytm')
	dbWriteTable(my_cfg, "filter_header", fh, row.names=F, append=T)
	return(fid)
	}
savefilterdtl<- function(my_cfg, fil, fid) {
	cat('savefdtl', fid, '\n')
	fdtl<- as.data.frame(cbind(fid[1], fil$fval, seq_along(1:length(fil$fval))))
	colnames(fdtl)<- c('fd_id', 'fd_value', 'fd_seq')
	dbWriteTable(my_cfg, "filter_details", fdtl, row.names=F, append=T)
	return(fid)
	}
next_filter_seq<- function(my_cfg) {
	q<-"Update filter_seq set fi_seq=last_insert_id(fi_seq+1)\n"
	dbGetQuery(my_cfg, q)
	q<-"select last_insert_id() from filter_seq"
	seq<-dbGetQuery(my_cfg, q)
	return(as.integer(seq[1,1]))
	}
