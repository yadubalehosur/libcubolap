#' @title addgraph
#' @description addgraph saves a new graph into the databsae
#' @details add graph_series
#' @details add graph_def
#' @details add folder_tree
#' @details add graph_props
#' @param repname is name of report being saved
#' @param foldid is the folder id under which this report is being saved
#' @param g is the graph being saved
#' @param currfilt are the filters in use
#' @param M is the metadata/config database connection
#' 
#' @export 
addgraph<- function(repname, foldid, g, currfilt, M) {
	gp<- g$gp
	gfid<- addgs(M$mycfg, gp)
	addgf(M$mycfg, M$uid,  gfid, repname, foldid, gp)
	addft(M$mycfg, gfid, repname, foldid, gp)
	addgp(M$mycfg, gfid, gp)
	return(gfid)
	}

# addxy adds the data for the graph
# add data_xy
# generate next series id
# insert into data_xy(xy_series_id, xy_xval, xy_xval2, ..y_val)
#
addxy<- function(my_cfg, dxy) {
	sid<- next_series_seq(my_cfg)
	gfdim<-ncol(dxy)-1
	dxy2<-cbind(sid[1], dxy[gfdim+1], dxy[1])
	dxy2[,2]<-as.character(dxy2[,2])
	cnames<-c('xy_series_id', 'xy_yval', 'xy_xval')
	if(gfdim>=2) {
		dxy2<- as.data.frame(cbind(dxy2, dxy[,2:gfdim]))
		for(i in 2:gfdim)
			cnames[4+i-2]<- paste0('xy_xval', i)
		}
	colnames(dxy2)<-cnames
	dbWriteTable(my_cfg, "data_xy", dxy2, row.names=F, append=T)
	return(sid)
	}

addseries<- function(my_cfg, sid, uid, mdf, mdd, filters) {
	fhid<- c('')
	if(!is.null(filters)) {
		for(i in 1:length(filters))
			fhid[i]<- savefilter(my_cfg, uid, filters[[i]], as.character(sid))
		fhid<- paste(fhid, collapse=",")
		}

	if(is.null(mdd))
		xids<- mdf$md_id
	else {
		xids<- mdd[[1]]$md_id
		if(length(mdd)>=2)
		for(i in 2:length(mdd))
			xids<- paste0(xids, ",", mdd[[i]]$md_id)
		}
	if(!is.null(mdf))
		mdid<- mdf$md_id
	else
		mdid<- mdd[[1]]$md_id
	ds<- as.data.frame(cbind(as.integer(sid[1]), as.integer(mdid), xids[1], fhid[1]))
	colnames(ds)<- c('ds_series_id', 'ds_yid', 'ds_xids', 'ds_filtids')
	dbWriteTable(my_cfg, "data_series", ds, row.names=F, append=T)
	return(sid)
	}

gfid<- NULL
#
# graph_series
#   gs_id | gs_series_id | gs_series_num | gs_title | gs_series_flt_oper
#
addgs<- function(my_cfg, gp) {
	fltoper=88
	gfid<- next_graph_seq(my_cfg)
	series<- gp$series
	for(i in 1:length(series)) {
		gstitle<- gp$mdf[[i]]$md_name
		gs<- as.data.frame(cbind(gfid[1], series[i], i[1], gstitle[1], fltoper[1]))
		}
	colnames(gs)<- c('gs_id', 'gs_series_id', 'gs_series_num', 'gs_title', 'gs_series_flt_oper')
	dbWriteTable(my_cfg, "graph_series", gs, row.names=F, append=T)
	return(gfid)
	}
#
# graph_def
#  gf_uid | gf_id | gf_subtitle | gf_user_title | gf_dimn | gf_folder | gf_create_ts 
#
addgf<- function(my_cfg, uid, gfid, repname, foldid, gp) {
	gfdim<- gp$gfdim
	gfsubtitle<- NULL
	if(is.null(gfsubtitle)) gfsubtitle<- ''
	gftitle<- repname
	gfuid<- uid
	gfold<- foldid
	gfts<- Sys.time()

	gf<- as.data.frame(cbind(gfuid[1], gfid[1], gfsubtitle[1], gftitle[1], gfdim[1], gfold[1], gfts[1]))
	colnames(gf)<- c('gf_uid', 'gf_id', 'gf_subtitle', 'gf_user_title', 'gf_dimn', 'gf_folder', 'gf_create_ts')
	dbWriteTable(my_cfg, "graph_def", gf, row.names=F, append=T)
	return(gfid)
	}
#
# folder_tree
#  ft_id | ft_parid | ft_name | ft_seq | ft_status | ft_expand
#
addft<- function(my_cfg, ftid, ftname, foldid, gp) {
	ftstat<- 'A'
	ftseq<- 0
	ftx<- 1
	ftid<- ftid
	ftname<- ftname
	ftpar<- foldid
	ft<- as.data.frame(cbind(ftid[1], ftpar[1], ftname[1], ftseq[1], ftstat[1], ftx[1]))
	colnames(ft)<- c('ft_id', 'ft_parid', 'ft_name', 'ft_seq', 'ft_status', 'ft_expand')
	dbWriteTable(my_cfg, "folder_tree", ft, row.names=F, append=T)
	return(ftid)
	}

addgp<- function(my_cfg, gfid, gp) {
	gpname<- 'gtype'
	gptype<- gp$gtype
	gprops<- as.data.frame(cbind(gfid[1], gpname[1], gptype[1]))
	colnames(gprops)<- c('gp_id', 'gp_name', 'gp_value')
	dbWriteTable(my_cfg, "graph_props", gprops, row.names=F, append=T)
	return(gfid)
	}

delft<- function(my_cfg, g) {
	q<-paste('delete from folder_tree where ft_id=', g$gp$gfid)
	dbGetQuery(my_cfg, q)
	}
delgf<- function(my_cfg, g) {
	q<-paste('delete from graph_def where gf_id=', g$gp$gfid)
	dbGetQuery(my_cfg, q)
	}

delreport<- function(my_cfg, g) {
	delft(my_cfg, g)
	delgf(my_cfg, g)
	}

next_series_seq<- function(my_cfg) {
	q<-"Update series_seq set ss_seq=last_insert_id(ss_seq+1)\n"
	dbGetQuery(my_cfg, q)
	q<-"select last_insert_id() from series_seq"
	seq<-dbGetQuery(my_cfg, q)
	return(as.integer(seq[1,1]))
}

next_graph_seq<- function(my_cfg) {
	q<-"Update folder_id_seq set fldid_seq=last_insert_id(fldid_seq+1)\n"
	dbGetQuery(my_cfg, q)
	q<-"select last_insert_id() from folder_id_seq"
	seq<-dbGetQuery(my_cfg, q)
	return(as.integer(seq[1,1]))
}

#genrepid<- function(M, D, xr, repid) {
#	g<- NULL
##	xr$currfilters
#	if(!is.null(repid)) isolate({
#		g<- setrepid(M$cfg, repid)
#		setexplore(M$cfg, g)
#		xr$gtype<- g$gp$gtype
#		if(!is.null(xr$currfilters)) {
#			g<- genadhoc(M, D, xr)
#			g$gp$gtype<- xr$gtype
#			}
#		})
#	return(g)
#	}
