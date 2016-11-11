allinput=allinput[date<='2016-08-31']
potacvinput=merge(allinput,avvomarkets[,.(marketid,state,county,pa)],by='marketid')
potacvinput[,county:=tolower(county)]
potacvinput[,state:=tolower(state)]



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

forpred=unique(potacvinput[,.(state,county,rt,pt,weekend)])
forpred=merge(forpred,potacv_pred[,.(statename,county,potential_sl_imps,potential_da_imps,potential_total)],by.x=c('state','county'),by.y=c('statename','county'))
forpred[,`:=`(date='2016-09-01',pv = potential_sl_imps/3, sl_imps=0,da_imps=0,sl_contacts=0,da_contacts=0,sl_acv=0,da_acv=0)]

potacvinput[,date:=as.Date(date)]
forpred[,date:=as.Date(date)]
fitinput=rbind(potacvinput[pa==input$practice_area,.(date,state,county,pt,rt,weekend,pv,sl_imps,da_imps,sl_contacts,da_contacts,sl_acv,da_acv,potential_sl_imps,potential_da_imps)],forpred[,.(date,state,county,pt,rt,weekend,pv,sl_imps,da_imps,sl_contacts,da_contacts,sl_acv,da_acv,potential_sl_imps,potential_da_imps)])
fitinput[,statecounty:=paste(state,county,sep=" | "),by=.(state,county)]
fitinput[,month:=substr(date,6,7)]
setkey(fitinput,statecounty,date)

#### ESTIMATE POTENTIAL CONTACTS for SL ####

#at least four days and nonzero acv
temp=fitinput[,sum(sl_acv>0,na.rm=T),by=.(date,statecounty)]
temp1=temp[,sum(V1>0),by=statecounty][V1>4]

#at least two pagetypes
temp=fitinput[,length(unique(pt)),by=statecounty]
temp2=temp[V1>=2]

#at least 2 devices
temp=fitinput[,length(unique(rt)),by=statecounty]
temp3=temp[V1>=2]

#list of markets that meet all 3 conditions
highstatecounty=intersect(temp1$statecounty,intersect(temp2$statecounty,temp3$statecounty))

highsl=fitinput[statecounty %in% highstatecounty]

models.highsl1 = highsl[,barebones_ols_predict (y=sl_contacts,X=model.matrix(~((I(pv^.3)+I(sl_imps^.3))*weekend)*(pt + rt)),Xp=model.matrix(~((I(pv^.3)+I(potential_sl_imps^.3))*weekend)*(pt+rt)),date=date),by=statecounty]

models.highsl2 = highsl[,barebones_ols_predict (y=sl_contacts,X=model.matrix(~((I(pv^.3)+I(sl_imps^.3))*weekend)*(pt)),Xp=model.matrix(~((I(pv^.3)+I(potential_sl_imps^.3))*weekend)*(pt)),date=date),by=statecounty]

models.highsl3 = highsl[,barebones_ols_predict(y=sl_contacts,X=model.matrix(~(I(pv^.3)+I(sl_imps^.3))*weekend),Xp=model.matrix(~(I(pv^.3)+I(potential_sl_imps^.3))*weekend),date=date),by=statecounty]


models.highsl1[,ndays:=.N,by=statecounty]
models.highsl1[,var:=rep(c('fit','date'),each=max(ndays)/2),by=statecounty]
models.highsl1[,num:=rep(1:(max(ndays)/2),2),by=statecounty]
del=dcast.data.table(data=models.highsl1,statecounty+num~var,fun.aggregate = sum,value.var = 'V1')

highsl[,cluster:=NA]
highsl[,potential_sl_contacts1:=models.highsl1[1:dim(models.highsl1)[1]/2,V1]]
highsl[,potential_sl_contacts2:=models.highsl2[1:dim(models.highsl2)[1]/2,V1]]
highsl[,potential_sl_contacts3:=models.highsl3[1:dim(models.highsl3)[1]/2,V1]]

highsltemp=highsl[,.(sum(potential_sl_contacts1),sum(potential_sl_contacts2),sum(potential_sl_contacts3),sum(sl_imps),sum(potential_sl_imps),sum(sl_contacts)),by=.(month,statecounty)]
highsltemp[,use:=1][V1<0,use:=2][V1<0 & V2<0,use:=3][use==3 & V3<0, use:=0]

del=merge(highsl,highsltemp[,.(month,statecounty,use)],all.x=T,by=c('month','statecounty'))
del[use==1,potential_sl_contacts:=potential_sl_contacts1]
del[use==2,potential_sl_contacts:=potential_sl_contacts2]
del[use==3,potential_sl_contacts:=potential_sl_contacts3]
del[use==0 & sl_imps>0 & potential_sl_imps>20,potential_sl_contacts:=potential_sl_imps/sl_imps * sl_contacts*.6]
del[use==0 & sl_imps==0,potential_sl_contacts:=0]
del[is.na(potential_sl_contacts),potential_sl_contacts:=0]
del$potential_sl_contacts1=NULL
del$potential_sl_contacts2=NULL
del$potential_sl_contacts3=NULL
del$use=NULL
highsl=copy(del)

#For each market, use 1,2 or 3 exclusively across all pt and rt
print('high traffic sl done')


lowsl=fitinput[match(statecounty,unique(highsl$statecounty),nomatch = 0)==0, ]
lowsl=cluster_markets(lowsl)
table(lowsl$cluster)

models.lowsl=lowsl[,barebones_ols_predict (y=sl_contacts,X=model.matrix(~((I(pv^.3)+I(sl_imps^.3))*weekend)*(pt + rt)),Xp=model.matrix(~((I(pv^.3)+I(potential_sl_imps^.3))*weekend)*(pt+rt)),date=date),by=cluster]

lowsl[,potential_sl_contacts:=models.lowsl[1:dim(models.lowsl)/2,V1]]

lowsl.redo=lowsl[,.(sum(sl_contacts),sum(potential_sl_contacts),length(unique(rt))),by=statecounty][V1>(1.1*V2) & V1>20 & V3>1][,statecounty]
if(is.data.table(lowsl.redo)){
  models.lowsl.redo.sl=lowsl[statecounty %in% lowsl.redo,barebones_ols_predict (y=sl_contacts,X=model.matrix(~((I(pv^.3)+I(sl_imps^.3))*weekend)*(pt + rt)),Xp=model.matrix(~((I(pv^.3)+I(potential_sl_imps^.3))*weekend)*(pt+rt)),date=date),by=statecounty]
  lowsl[statecounty %in% lowsl.redo,potential_sl_contacts:=models.lowsl.redo.sl[,V1]]
}
allsl=rbind(highsl,lowsl)
allsl[potential_sl_contacts<0,potential_sl_contacts:=0]

print('a1')

#
# sl.redo2=allsl[date<='2016-09-01' & date>='2016-08-04',.(sum(sl_contacts),sum(potential_sl_contacts),sum(sl_imps),sum(potential_sl_imps)),by=statecounty][((V2/V1 > 2) & (V4/V3 < 2) & V1>10) | (((V2/V1) > (1.25 * (V4/V3))) & V2>5) | ((V2/V1)>3 & V2>5)]
#allsl[statecounty %in% sl.redo2[,statecounty],alternatepotct:=quantilef(potential_sl_imps,potential_sl_contacts),by=statecounty]
#allsl[statecounty %in% sl.redo2[,statecounty],potential_sl_contacts:=ifelse(sl_imps>0 | sl_contacts>0,(potential_sl_imps/sl_imps)*sl_contacts*.7,alternatepotct),by=statecounty]
# allsl[statecounty %in% sl.redo2[,statecounty],potential_sl_contacts:=ifelse(sl_imps>0 | sl_contacts>0,(potential_sl_imps/sl_imps)*sl_contacts*.7,alternatepotct),by=statecounty]

print('cluster sl done')
print(str(Sys.time()))



#### ESTIMATE POTENTIAL CONTACTS for DA ####

#at least four days and nonzero acv
temp=allinput[,sum(da_acv>0,na.rm=T),by=.(date,statecounty)]
temp1=temp[,sum(V1>0),by=statecounty][V1>4]

#at least two pagetypes
temp=allinput[,length(unique(pt)),by=statecounty]
temp2=temp[V1>=2]

#at least 2 devices
temp=allinput[,length(unique(rt)),by=statecounty]
temp3=temp[V1>=2]

#list of markets that meet all 3 conditions
highstatecounty=intersect(temp1$statecounty,intersect(temp2$statecounty,temp3$statecounty))

highda=allinput[statecounty %in% highstatecounty]

models.highda1 = highda[,barebones_ols_predict (y=da_contacts,X=model.matrix(~((I(pv^.3)+I(da_imps^.3))*weekend)*(pt + rt)),Xp=model.matrix(~((I(pv^.3)+I(potential_da_imps^.3))*weekend)*(pt+rt))),by=statecounty]

models.highda2 = highda[,barebones_ols_predict (y=da_contacts,X=model.matrix(~((I(pv^.3)+I(da_imps^.3))*weekend)*(pt)),Xp=model.matrix(~((I(pv^.3)+I(potential_da_imps^.3))*weekend)*(pt))),by=statecounty]

models.highda3 = highda[,barebones_ols_predict(y=da_contacts,X=model.matrix(~(I(pv^.3)+I(da_imps^.3))*weekend),Xp=model.matrix(~(I(pv^.3)+I(potential_da_imps^.3))*weekend)),by=statecounty]


highda[,cluster:=NA]
highda[,potential_da_contacts1:=models.highda1[,V1]]
highda[,potential_da_contacts2:=models.highda2[,V1]]
highda[,potential_da_contacts3:=models.highda3[,V1]]

highdatemp=highda[,.(sum(potential_da_contacts1),sum(potential_da_contacts2),sum(potential_da_contacts3),sum(da_imps),sum(potential_da_imps),sum(da_contacts)),by=.(month,statecounty)]
highdatemp[,use:=1][V1<0,use:=2][V1<0 & V2<0,use:=3][use==3 & V3<0, use:=0]

del=merge(highda,highdatemp[,.(month,statecounty,use)],all.x=T,by=c('month','statecounty'))
del[use==1,potential_da_contacts:=potential_da_contacts1]
del[use==2,potential_da_contacts:=potential_da_contacts2]
del[use==3,potential_da_contacts:=potential_da_contacts3]
del[use==0 & da_imps>0 & potential_da_imps>20,potential_da_contacts:=potential_da_imps/da_imps * da_contacts*.6]
del[use==0 & da_imps==0,potential_da_contacts:=0]
del[is.na(potential_da_contacts),potential_da_contacts:=0]
del$potential_da_contacts1=NULL
del$potential_da_contacts2=NULL
del$potential_da_contacts3=NULL
del$use=NULL
highda=copy(del)


#For each market, use 1,2 or 3 exclusively across all pt and rt
print('high traffic da done')

lowda=allinput[match(statecounty,unique(highda$statecounty),nomatch = 0)==0, ]
lowda=cluster_markets(lowda)
table(lowda$cluster)

models.lowda=lowda[,barebones_ols_predict (y=da_contacts,X=model.matrix(~((I(pv^.3)+I(da_imps^.3))*weekend)*(pt + rt)),Xp=model.matrix(~((I(pv^.3)+I(potential_da_imps^.3))*weekend)*(pt+rt))),by=cluster]

lowda[,potential_da_contacts:=models.lowda[,V1]]

lowda.redo=lowda[,.(sum(da_contacts),sum(potential_da_contacts),length(unique(rt))),by=statecounty][V1>(1.1*V2) & V1>20 & V3>1][,statecounty]
if(is.data.table(lowda.redo)){
  models.lowda.redo.da=lowda[statecounty %in% lowda.redo,barebones_ols_predict (y=da_contacts,X=model.matrix(~((I(pv^.3)+I(da_imps^.3))*weekend)*(pt + rt)),Xp=model.matrix(~((I(pv^.3)+I(potential_da_imps^.3))*weekend)*(pt+rt))),by=statecounty]
  lowda[statecounty %in% lowda.redo,potential_da_contacts:=models.lowda.redo.da[,V1]]
}
allda=rbind(highda,lowda)
allda[potential_da_contacts<0,potential_da_contacts:=0]

print('a1')
#allda[statecounty==335 & date>='2015-10-01' & date<='2015-10-31',sum(potential_da_contacts)]

#redo if potential contacts are lower than actual contacts or potential contacts are too high
# redo=allda[,.(sum(da_contacts),sum(potential_da_contacts),sum(da_imps),sum(potential_da_imps)),by=statecounty][((V2/V1 > 2) & (V4/V3 < 2) & V1>10) | (V2/V1 > (1.25 * (V4/V3)) & V2>5)]
# if(is.data.table(redo)){
#
#   temp=allda[statecounty %in% redo[,statecounty],sum(da_acv>0,na.rm=T),by=.(date,statecounty)]
#   temp1=temp[,sum(V1>0),by=statecounty][V1>4]
#
#   #at least two pagetypes
#   temp=allda[statecounty %in% redo[,statecounty],length(unique(pt)),by=statecounty]
#   temp2=temp[V1>=2]
#
#   #at least 2 devices
#   temp=allda[statecounty %in% redo[,statecounty],length(unique(rt)),by=statecounty]
#   temp3=temp[V1>=2]
#
#   print('b2')
#
#   redomarkets=intersect(temp1$statecounty,intersect(temp2$statecounty,temp3$statecounty))
#   setkey(allda,statecounty,date)
#   models.da.redo=allda[statecounty %in% redomarkets,barebones_ols_predict (y=da_contacts,X=model.matrix(~((I(pv^.3)+I(da_imps^.3))*weekend)*(rt)),Xp=model.matrix(~((I(pv^.3)+I(potential_da_imps^.3))*weekend)*(rt))),by=statecounty]
#   allda[statecounty %in% redomarkets,potential_da_contacts2:=models.da.redo[,V1]]

da.redo2=allda[date<=process_date & date>=(process_date-27),.(sum(da_contacts),sum(potential_da_contacts),sum(da_imps),sum(potential_da_imps)),by=statecounty][((V2/V1 > 2) & (V4/V3 < 2) & V1>10) | (((V2/V1) > (1.25 * (V4/V3))) & V2>5) | ((V2/V1)>3 & V2>5)]
allda[statecounty %in% da.redo2[,statecounty],alternatepotct:=quantilef(potential_da_imps,potential_da_contacts),by=statecounty]
allda[statecounty %in% da.redo2[,statecounty],potential_da_contacts:=ifelse(da_imps>0 | da_contacts>0,(potential_da_imps/da_imps)*da_contacts*.7,alternatepotct),by=statecounty]


print('c3')
allda[statecounty==335 & date>='2015-10-01' & date<='2015-10-31',sum(potential_da_contacts)]

print('cluster da done')
print(str(Sys.time()))


#### COMBINE ALL PREDICTED data ####
tempalldata=merge(allsl,allda[,.(date,statecounty,pt,rt,potential_da_contacts)],by=c('date','statecounty','pt','rt'))
