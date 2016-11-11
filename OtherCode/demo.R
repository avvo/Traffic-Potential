library(cluster)
library(fpc)
tempd=ctylevel[,.(cit.not_citizen/pop.base_count,hh.median_income)]
plot(tempd,pch='+')
del=km(tempd,5)
plotcluster(del[[1]][,-3,with=FALSE],del[[2]]$cluster,method = 'anc',clnum=5,pch='+')


input=data.table(ncluster=5,statename='washington',practice_area='Immigration',variables=paste('pop.base_count','hh.median_income',sep=','))
