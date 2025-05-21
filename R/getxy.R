#' @title getxy
#' @description look up and return a saved report
#' @param cfg is the database connection for meta data (config) database
#' @param id is report id
#' @import magrittr
#' @importFrom utils head
#' @export
#' 
getxy<- function(cfg, id) {
	if(is.null(id) || is.na(as.integer(id))) {
		cat("id is **", id, "**\n")
		return(list(gp=NULL,dxy=NULL))
		}
	gp<-getgraph(cfg, id)
	xtypes<-gp$xtypes
	if(length(gp$gfdim) == 0 || is.na(gp$xtypes)) {
		cat(id, "is null", "\n")
		return(list(gp=NULL,dxy=NULL))
		}

	dxy<- data.frame()
#	xy<-"select"
#	xy<-paste(xy, ifelse(xtypes[1]=='int',"convert(xy_xval,signed)","xy_xval"))
	xy<- c()
	xy[1]<- 'xy_xval'
	if(gp$gfdim>1)
		for(l in 2:gp$gfdim) {
			xy[l]<- paste0('xy_xval', l)
#			xvalstr<-ifelse(xtypes[l] == 'int', paste0(",convert(xy_xval",l,",signed)"),paste0(",xy_xval",l))
#			xy<-paste0(xy,xvalstr)
			}
#	xy<-paste(xy, ",xy_yval from data_xy where xy_series_id=",sep="")
	xy[gp$gfdim+1]<- 'xy_yval'

	for(k in 1:length(gp$gseries)) {
#		xyq<-paste(xy,gp$gseries[k],sep="")
#		dataxy<-dbGetQuery(configdb, xyq)

		dataxy<- filter(cfg$data_xy, .data$xy_series_id==!!gp$gseries[k]) %>% selectcols(xy)
		dataxy<- as.data.frame(dataxy)

		if(nrow(dataxy) == 0)
			next

		#dataxy<-applyfilter(dataxy)

#		colnames(dataxy)<- c(xids,yname[k])
		colnames(dataxy)<- c(gp$xnames,gp$yname[k])
		if(nrow(dxy)==0)
			dxy<-dataxy
		else 
			dxy<-merge(dataxy,dxy,by=as.character(gp$xnames),all=T)
		}

	if(nrow(dxy) > 500)
		cat(id, "too big", nrow(dxy), "\n")
	dxy<-head(dxy, 500)

	return(list(gp=gp,dxy=dxy))
	}

#' importFrom rlang .data
#' @title getgraph
#' @details Returns graph structure for a report id
#' @param cfg is the meta data data base
#' @param id is the report id
#' @export
getgraph<- function(cfg, id) {
	xnames<-c()
	xids<-c()
	xtypes<-c()
	if(is.na(as.integer(id))) {
		cat("junk id", id, "\n")
		return("")
		}

#	gfq<-paste("select gf_dimn, gf_user_title from graph_def where gf_id=", id)
#	gf<-dbGetQuery(configdb, gfq)
	gf<- filter(cfg$graph_def, .data$gf_id==id)  %>% select(.data$gf_dimn, .data$gf_user_title)
	gf<- as.data.frame(gf)

	gfdim<-gf$gf_dimn
	gftitle<-gf$gf_user_title
	gtype<- filter(cfg$graph_props, .data$gp_id==id) %>% filter(.data$gp_name=="gtype") %>% select(.data$gp_value)
	gtype<- as.data.frame(gtype)$gp_value
	if(!is.null(gtype)) {
	if(gtype == 'B' | gtype == 'C') gtype = 'bar'
	else if(gtype == 'L') gtype = 'line'
	else if (gtype == 'P') gtype = 'pie'
#	else gtype ='dt'
	}
	else gtype = 'dt'

#	gtype<- gtype$gp_value

	gfdim<-gf$gf_dimn
# 	gfiq<- paste("select fi_itemid, fi_val from graph_filters where fi_gid=", id, "order by fi_seq")
#	gf<-dbGetQuery(configdb, gfq)
#	gfi<- filter(graph_filters, fi_gid==id) %>% select(fi_itemid, fi_val, fi_seq) %>% arrange(fi_seq)
#	gfi<- as.data.frame(gfi)
#	gfi<- gfi[,2]

#	gsq<-paste("select gs_series_id, gs_title from graph_series where gs_id=", id)
#	gs<-dbGetQuery(configdb, gsq)
	gs<- filter(cfg$graph_series, .data$gs_id==id) %>% select(.data$gs_series_id, .data$gs_title)
	gs<- as.data.frame(gs)
	
	gseries<-gs$gs_series_id
	yname<-gs$gs_title
	if(nrow(gs) > 0) {
#		ds<-paste("select ds_yid, ds_xids from data_series where ds_series_id=", gseries[1])
#		dseries<-dbGetQuery(configdb, ds)
		dseries<- filter(cfg$data_series, .data$ds_series_id==!!gseries[1])
		dseries<- dseries %>% select(.data$ds_yid, .data$ds_xids)
		dseries<- as.data.frame(dseries)

		xids<- unlist(strsplit(dseries$ds_xids,","))
		if(length(xids) > 0)
		for(x in 1:length(xids)) {
			if(substring(xids[x],1,1) == "G")
				next
#			q<-paste("select md_name,md_type from menu_dtls where md_id=", xids[x])
#			md<-dbGetQuery(configdb, q)
			md<- filter(cfg$menu_dtls, .data$md_id==!!xids[x]) %>% select(.data$md_name, .data$md_type)
			md<- as.data.frame(md)

			xnames[x]<-md[1,1]
			xtypes[x]<-md[1,2]
			}
		}
#	xnames<-unlist(xnames)
#	gprops<-list(gfid=id, gfdim=gfdim, gftitle=gftitle, gseries=gseries, gfi=gfi, yname=yname, xnames=xnames, xids=xids, filtids=filtids, xtypes=xtypes)
	gprops<-list(gfid=id, gfdim=gfdim, gftitle=gftitle, gseries=gseries, yname=yname, xnames=xnames, xids=xids, xtypes=xtypes, gtype=gtype)
	return(gprops)
	}

gettrn<- function(cfg, graph) {
	gp<-graph$gp
#	q<-paste0("select ds_yid from data_series where ds_series_id=",gp$gseries[1])
#	yid<-dbGetQuery(configdb,q)
	yid<- filter(cfg$data_series, .data$ds_series_id == gp$gseries[1]) %>% select(.data$ds_yid)
	yid<- as.data.frame(yid)

	yid<-yid[1,1]
#	q<-paste0("select md_table from menu_dtls where md_id=", yid)
#	mdtable<-dbGetQuery(configdb,q)
	mdtable<- filter(cfg$menu_dtls, .data$md_id==yid) %>% select(.data$md_table)
	mdtable<- as.data.frame(mdtable)

	mdtable<-mdtable[1,1]
	q<-paste0("select * from ", mdtable, " limit 100")
#	trntab<-dbGetQuery(configdb,q)
	trntab<- tbl(cfg, q)
	trntab<- as.data.frame(trntab)

	return(list(mdtable=mdtable, trntab=trntab))
	}
