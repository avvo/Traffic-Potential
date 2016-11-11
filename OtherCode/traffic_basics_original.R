mapping=fread('../Traffic/Data/zip_to_ad_region_mapping.csv',na.strings = c('NULL','#N/A'))
setnames(mapping,c('ad_region_id','ad_region_name'),c('regionid','region'))
mapping$zip=paste('00',mapping$zip,sep='')
mapping$zip=substring(mapping$zip,nchar(mapping$zip)-4,nchar(mapping$zip))
mapping[statename=='Dist. of Columbia',statename:='District of Columbia']

county=fread('../Traffic/Data/Avvo County with Census County equivalent.csv')
county[,avvocounty:=tolower(avvocounty)]
data("county.map")
data("state.map")
setDT(county.map)
setDT(state.map)
county.map[tolower(NAME) %in% c('baltimore','st. louis','franklin','richmond','roanoke') & LSAD=='city',NAME:=paste(NAME,LSAD)]
censuscounty=merge(unique(county.map[,.(STATE,NAME,region)]),unique(state.map[,.(STATE,region)]),by='STATE',all=T)
setnames(censuscounty,c('statefip','county','fip','state'))
censuscounty=merge(censuscounty,unique(mapping[,.(tolower(statename),state)]),by.x='state',by.y='V1',all=T)
setnames(censuscounty,c('statename','statefip','county','fip','state'))
censuscounty[,county:=tolower(county)]
censuscounty=merge(censuscounty,county,by=c('county','state'),all=T)

#### geo ####
fips=fread('../Traffic/Data/county_fips.txt')
fclass=fread('../Traffic/Data/fips_class_codes.txt')
setnames(fips,'stateabb','state')
#fips=merge(fips,fclass,by='class')
fips$county=tolower(gsub(' County','',fips$county))
fips=merge(fips,county,by=c('county','state'))

citizen=fread('../Traffic/Data/Citizen Status 2014 Zip Code level.csv',na.strings = c('NULL','#N/A'))
setnames(citizen,c('zcta','zip','cit.total','cit.usborn','cit.foreignborn','cit.naturalized','cit.no'))
citizen$zip=NULL

race=fread('../Traffic/Data/2014_race.csv',na.strings = c('NULL','#N/A'))
setnames(race,c('zcta','eth.total','eth.white','eth.black','eth.native','eth.asian','eth.pacific','eth.other','eth.two','two1','two2'))
race[,eth.two:=eth.two+two1+two2]
race[,`:=`(two1=NULL,two2=NULL)]

s=fread('../Traffic/Data/2014_social_characteristics.csv',na.strings = c('NULL','#N/A'))
removecols=c(grep('ANCESTRY',names(s)),grep('PLACE OF BIRTH',names(s)),grep('RESIDENCE 1 YEAR AGO',names(s)),grep('WORLD REGION OF BIRTH OF FOREIGN BORN',names(s)),grep('YEAR OF ENTRY',names(s)))
s=s[,-removecols,with=F]
snames=fread('../Traffic/Data/colnames_for_2015_social_characteristics.csv')
setnames(s,snames[,original],snames[,new])
s[,`:=`(hh.avg_size=NULL,cit.naturalized=NULL)]
s=s[,names(s)!='x',with=F]
setkey(s,zcta)


#### Internal traffic
i=readimpala('internal_traffic.sql')
#i=load('Data/internal_data.RData')
#setnames(i,c('month','marketid','statename','county','region','pa','parentpa','block','page','medium','visits','sl','slprice','da','daprice'))
setnames(i,c('marketid','pa','county','statename','visits'))
i[,statename:=tolower(statename)]
i[,county:=tolower(county)]
i=merge(i,unique(censuscounty[,.(statename,state)]),by=c('statename'))
i=merge(i,county,by.x=c('state','county'),by.y=c('state','avvocounty'))
#traffic=i[,.(sum(visits)),by=.(marketid,pa,county,state,month,page)][,mean(V1),by=.(marketid,pa,county,state,page)]
traffic=i[,.(sum(visits)),by=.(pa,county,state)]
setnames(traffic,c('V1'),c('visits'))
traffic=traffic[,sessions:=visits/3]
#traffic=dcast.data.table(traffic,state+county+pa,value.var = 'avg',fun.aggregate = sum)
traffic=merge(traffic,unique(censuscounty[,.(state,statename)]),by='state')
setnames(traffic,'others','noncore')

d=fread('../Traffic/Data/census_data_master_061016.csv',na.strings = c('NULL','#N/A','-'))
setnames(d,c('zcta','state','x','county','regionid','region','pop.total','pop.under5','pop.5_9','pop.10_14','pop.15_19','pop.20_24','pop.25_29','pop.30_34','pop.35_39','pop.40_44','pop.45_49','pop.50_54','pop.55_59','pop.60_64','pop.65_69','pop.70_74','pop.75_79','pop.80_84','pop.85over','pop.male','pop.female','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','age_male','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','age_female','x','x','x','eth.hispanic','eth.nonhispanic','hh.total','hh.avg_size','hh.avg_family_size','x','x','x','x','x','x','edu.lhs','edu.hs','edu.some_college','edu.ba','edu.grad','x','x','x','x','x','emp.yes','emp.no','emp.military','emp.na','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','x','hh.median_income','x','x','x','hic.yes','hic.private','hic.public','hic.no','x','x','x','x','x','x','mar.never','mar.yes','mar.separated','mar.widowed','mar.divorced','x','x','x','pov.below','pov.above','est.total','est.1_4','est.1000plus','est.10_19','est.100_249','est.20_49','est.250_499','est.5_9','est.50_99','est.500_999','x','est.allyear','est.partyear','rev.under100000','rev.249000','rev.499000','rev.999999','rev.1000000'))
d=d[,names(d)!='x',with=F]
d[,hh.median_income:=as.numeric(as.vector(hh.median_income))]
d=merge(d,citizen,by='zcta')
d=merge(d,race,by='zcta')
d=merge(d,s,by='zcta')
d[is.na(d)]=0


#### Make county level data ####
ctemp=d[,-c('zcta','pop.under5','pop.5_9','pop.10_14','pop.15_19','pop.20_24','pop.25_29','pop.30_34','pop.35_39','pop.40_44','pop.45_49','pop.50_54','pop.55_59','pop.60_64','pop.65_69','pop.70_74','pop.75_79','pop.80_84','pop.85over','age_male','age_female','hh.avg_size','hh.avg_family_size','hh.median_income',"fer.unmarried_birth_pml","fer.age15_50_birth_pml","fer.age15_19_birth_pml","fer.age20_34_birth_pml","fer.age35_50_birth_pml",'region','regionid'),with=F]
ctempsum=ctemp[,lapply(.SD,sum,na.rm=T),by=.(state,county)]
cweightedavg=d[,.(sum(pop.under5*pop.total)/sum(pop.total),sum(pop.5_9*pop.total)/sum(pop.total),sum(pop.10_14*pop.total)/sum(pop.total),sum(pop.15_19*pop.total)/sum(pop.total),sum(pop.20_24*pop.total)/sum(pop.total),sum(pop.25_29*pop.total)/sum(pop.total),sum(pop.30_34*pop.total)/sum(pop.total),sum(pop.35_39*pop.total)/sum(pop.total),sum(pop.40_44*pop.total)/sum(pop.total),sum(pop.45_49*pop.total)/sum(pop.total),sum(pop.50_54*pop.total)/sum(pop.total),sum(pop.55_59*pop.total)/sum(pop.total),sum(pop.60_64*pop.total)/sum(pop.total),sum(pop.65_69*pop.total)/sum(pop.total),sum(pop.70_74*pop.total)/sum(pop.total),sum(pop.75_79*pop.total)/sum(pop.total),sum(pop.80_84*pop.total)/sum(pop.total),sum(pop.85over*pop.total)/sum(pop.total)                  ,sum(age_male*pop.male)/sum(pop.male),sum(age_female*pop.female)/sum(pop.female),sum(hh.avg_size*hh.total)/sum(hh.total),sum(hh.avg_family_size*hh.total)/sum(hh.total),sum(hh.median_income*hh.total)/sum(hh.total),sum(fer.unmarried_birth_pml*pop.female)/sum(pop.female),sum(fer.age15_50_birth_pml*pop.female)/sum(pop.female),sum(fer.age15_19_birth_pml*pop.female)/sum(pop.female),sum(fer.age20_34_birth_pml*pop.female)/sum(pop.female),sum(fer.age35_50_birth_pml*pop.female)/sum(pop.female)),by=.(state,county)]
setnames(cweightedavg,c('state','county','pop.under5','pop.5_9','pop.10_14','pop.15_19','pop.20_24','pop.25_29','pop.30_34','pop.35_39','pop.40_44','pop.45_49','pop.50_54','pop.55_59','pop.60_64','pop.65_69','pop.70_74','pop.75_79','pop.80_84','pop.85over','age_male','age_female','hh.avg_size','hh.avg_family_size','hh.median_income','fer.unmarried_birth_pml','fer.age15_50_birth_pml','fer.age15_19_birth_pml','fer.age20_34_birth_pml','fer.age35_50_birth_pml'))
ctylevel=merge(cweightedavg,ctempsum,by=c('state','county'))
#ctylevel[,county:=tolower(county)]
#write.csv(ctylevel[1:2],'Data/colnames for final data.csv',row.names = F)

datanames=fread('../Traffic/Data/colnames for final data.csv')
setnames(ctylevel,datanames[,column.name],datanames[,new.column.name])
ctylevel=ctylevel[,names(ctylevel)!='x',with=F]




####
source('km.R')

temp=ctylevel[,.(pop.base_count,hh.median_income,mar.yes/pop.base_count,(mar.separated+mar.divorced)/pop.base_count,state,county)]
#### K-means
k=km(temp,centers=25)

k$realcenters/apply(k$realcenters,1,sum);k$realcenters;table(k$x$V2)

####Hierarachical clustering
ktemp=temp[,-c('state','county'),with=F]
ktemp=standardize(ktemp,full=F)
h1=hclust(dist(ktemp))


#### Random forest
