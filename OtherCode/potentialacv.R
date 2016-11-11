tempprop=potacvinput[pa==input$practice_area,.(sum(pv),sum(sl_imps),sum(da_imps),sum(sl_contacts),sum(da_contacts)),by=.(month,state,county,pa)][,.(mean(V1),mean(V2),mean(V3),mean(V4),mean(V5)),by=.(state,county,pa)]
setnames(tempprop,c('state','county','pa','pv','slimps','daimps','slcontacts','dacontacts'))
tempprop=merge(tempprop,traffic[pa==input$practice_area,.(pa,statename,county,(core+others))],by.x=c('pa','state','county'),by.y=c('pa','statename','county'),all=T)
setnames(tempprop,'V4','sessions')
tempprop[,`:=`(sl_ips=slimps/sessions, da_ips = daimps/sessions, sl_cps=slcontacts/sessions,da_cps=dacontacts/sessions)]
tempprop[is.na(tempprop)]=0
setnames(tempprop,'state','statename')
tempprop=merge(tempprop,unique(censuscounty[,.(state,statename)]),by='statename')

potacv_pred=merge(tempprop,k$k[,.(state,county,total,potential_total)],by=c('state','county'))
potacv_pred[,`:=`(potential_sl_contacts=potential_total*sl_cps,potential_da_contacts=potential_total*da_cps)]
