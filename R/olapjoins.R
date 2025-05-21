# prepare all the table joins required for the SQL
makejoins<- function(my_cfg, mdf, mdd, atabs) {
	jtabs<- unique(c(sapply(mdd, `[[`, 'md_table'), atabs))
	omit<- c(mdf$md_table, 'time')
	jtabs<- setdiff(jtabs, omit)
	joins<-c(); j<- 0
	sftabs<-c()
	if(length(jtabs) > 0)
	for(i in 1:length(jtabs)) {
		tq<-paste0("select tj_col1,tj_col2 from table_joins where tj_tab1='",mdf$md_table,"' and tj_tab2 != 'time' and tj_tab2='", jtabs[i], "'")
		thisjoin<-dbGetQuery(my_cfg, tq)
		if(nrow(thisjoin)>0) {
			j<- j+1
			joins[j]<- paste0(mdf$md_table, ".", thisjoin$tj_col1, "=", jtabs[i], ".", thisjoin$tj_col2)
			}

		else if(nrow(thisjoin)<=0) { # try star flake
			tq<- paste0("select * from table_joins where tj_tab1='", mdf$md_table, "' and tj_tab2 in (select tj_tab1 from table_joins where tj_tab2='", jtabs[i], "') limit 1")
			thisjoin<-dbGetQuery(my_cfg, tq)
			if(nrow(thisjoin)>0) {
				j<- j+1
				sftab<- thisjoin$tj_tab2
				joins[j]<- paste0(mdf$md_table, ".", thisjoin$tj_col1, "=", sftab, ".", thisjoin$tj_col2)

				tq<-paste0("select tj_col1,tj_col2 from table_joins where tj_tab1='",sftab,"' and tj_tab2='", jtabs[i], "'")
				thisjoin<-dbGetQuery(my_cfg, tq)
				j<- j+1
				joins[j]<- paste0(sftab, ".", thisjoin$tj_col1, "=", jtabs[i], ".", thisjoin$tj_col2)

				sftabs<- c(sftabs, sftab)
				}

			else {
				cat('join not found for:', mdf$md_table, jtabs[i], '\n')
				return(NULL)
				}
			}
		}
	jo<-c()
	if(j > 0) {
		jo<- joins[1]
		if(j > 1)
		for(i in 2:j)
			jo<- paste(jo, "and", joins[i])
		}
	return(list(jo=jo, sftabs=sftabs))
	}
