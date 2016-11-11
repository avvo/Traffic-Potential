shinyServer(function(input, output) {
  cluster_table = reactive({
    del = strsplit(input$variables, ',')[[1]]
    temp = data.table(sapply(del, function(x)
      ctylevel[, eval(parse(text = x))]), ctylevel[, .(state, county)])
    temp[, county := tolower(county)]
    k = km(temp, minnum=4, centers = as.numeric(input$ncluster),nstart=3,iter.max=20)
#    setnames(k$k, 'V2', 'cluster')

  k$k = merge(k$k,traffic[pa == input$practice_area, .(state, county, others, core)],all.x = T,by = c('state', 'county'))

  #Add population count if not already present
  if(is.na(match('pop.base_count',del))){
    getpop=ctylevel[,.(pop.base_count,state,county)]
    getpop[,county:=tolower(county)]
    k$k=merge(k$k,getpop,by=c('state','county'))
  }
    k$k[is.na(k$k)]=0
    k$k[, total := others + core]
    k$k[, total.pp := total/pop.base_count]
    k$k[, core.pp := core/pop.base_count]
    target_visits_total = k$k[, quantile(total.pp, .7, na.rm = TRUE, type = 3), by =cluster]
    setnames(target_visits_total, c('cluster', 'target.total.pp'))
    target_visits_core = k$k[, quantile(core.pp, .7, na.rm = TRUE, type = 3), by =cluster]
    setnames(target_visits_core, c('cluster', 'target.core.pp'))
    target_visits=merge(target_visits_core,target_visits_total,by='cluster')

    k$k = merge(k$k, target_visits_total, by = 'cluster')
    k$k = merge(k$k, target_visits_core, by = 'cluster')
    k$k[, potential_total := max(total.pp, target.total.pp, na.rm = T)*pop.base_count, by = .(state, county)]
    k$k[, potential_core := max(core.pp, target.core.pp, na.rm = T)*pop.base_count, by = .(state, county)]

    ### start potential acv
    traffic2=melt.data.table(traffic,id=c(1,2,3,6),variable='pt',value='sessions')
    tempprop=potacvinput[pa==input$practice_area,.(sum(potslimps),sum(potdaimps),sum(potslacv),sum(potdaacv)),by=.(month,statename,county,pa,pt)][,.(mean(V1),mean(V2),mean(V3),mean(V4)),by=.(statename,county,pa,pt)]
    setnames(tempprop,c('statename','county','pa','pt','slimps','daimps','slacv','daacv'))
    tempprop=merge(tempprop,traffic2[pa==input$practice_area],by=c('pa','statename','county','pt'),all=T)
    tempprop[,`:=`(sl_ips=slimps/sessions, da_ips = daimps/sessions, sl_aps=slacv/sessions,da_aps=daacv/sessions)]
    tempprop[is.na(tempprop)]=0
#    tempprop=merge(tempprop,unique(censuscounty[,.(state,statename)]),by='statename')

    potacv_pred=merge(tempprop,k$k[,.(state,county,total,potential_total)],by=c('state','county'),all=T)
    potacv_pred[total>30,`:=`(potential_sl_acv=potential_total*sl_aps*(1-log(potential_total/total,100)),potential_da_acv=potential_total*da_aps*(1-log(potential_total/total,100)))]
    potacv_pred[total<=30,`:=`(potential_sl_acv=slacv,potential_da_acv=daacv)]

    potacv_pred2=dcast.data.table(data=potacv_pred,statename+county~)


    potacv_pred[,`:=`(potential_acv=potential_sl_acv+potential_da_acv,acv=slacv+daacv)]
    k$k=merge(k$k,potacv_pred[,.(state,county,slacv,potential_sl_acv,daacv,potential_da_acv,acv,potential_acv)],by=c('state','county'))
    ### end potential acv

    #average monthly visits and potential monthly visits by cluster
    list(cbind(k$k,pa=input$practice_area), k$realcenters)
  })

  filecontent = reactive({
    cat("DEM:",input$dem)
    if(input$dem==TRUE)
      out=cluster_table()[[1]] else
        out=cluster_table()[[1]][,-c(input$variables),with=F]
    print(head(out))
  })

    visits = reactive({
    k = cluster_table()[[1]]
    centers = cluster_table()[[2]]
    visits = k[, .(round(sum(total, na.rm = T)), round(sum(core, na.rm =T)), round(sum(potential_total, na.rm = T)), round(sum(potential_core, na.rm =T)), round(sum(acv, na.rm =T)), round(sum(potential_acv, na.rm =T))), by = cluster]
    setnames(visits,c(
        'cluster',
        'actual_visits',
        'actual_core_visits',
        'potential_visits',
        'potential_core_visits',
        'acv',
        'potential_acv'
      )
    )
    # In table, show names of county in selected states
    countynamestemp = k[k$state %in% state.abb[tolower(state.name) %in% input$statename]]
    countynames = by(countynamestemp$county,
                     countynamestemp$cluster,
                     paste,collapse = ',')
    countynames = data.table(cluster = names(countynames),
                             names = as.vector(countynames))
    setnames(countynames, 'names', paste(paste(input$statename, collapse =','), 'counties'))
    #get counts of counties nationwide in each cluster
    tempout = data.table(centers, count = table(k$cluster))
    setnames(tempout, 'count.V1', 'cluster')
    tempout[, cluster := as.numeric(cluster)]
    countynames[, cluster := as.numeric(cluster)]
    temp1 = merge(tempout, visits, by = 'cluster')
    temp2 = merge(temp1, countynames, by = 'cluster', all = T)
  })


  output$clustermeans <- renderDataTable({
    print('CLUSTERMEANS')
    visits()
  }, options = list(paging = F, pagelength = input$input$ncluster))



  output$downloadData <- downloadHandler(
    filename = function()
      paste(gsub(' ','_',input$practice_area),'_counties.csv',sep=''),content = function(file)
      write.csv(filecontent(),
        file,
        row.names = F,
        quote = F
      )
  )


  output$visit_summary = renderTable({
    visits = visits()[, .(
      cluster,
      actual_visits,
      actual_core_visits,
      potential_visits,
      potential_core_visits,
      acv,
      potential_acv
    )]
    summ = visits[, sapply(.SD, sum, na.rm = T)][-1]
    names(summ) = c('actual', 'actual_core', 'potential', 'potential_core','acv','potential_acv')
    summ = data.frame(t(summ))
    summ$actual_core_perc = round(summ$actual_core / summ$actual, 2)
    summ$potential_core_perc = round(summ$potential_core / summ$potential, 2)
    summ$change = round((summ$potential / summ$actual) - 1, 2)
    summ$core_change = round((summ$potential_core / summ$actual_core) -1, 2)
    summ$acv_change = round((summ$potential_acv / summ$acv)-1,2)
    out = as.data.frame(matrix(c(summ$actual,summ$potential,summ$change
                                 ,summ$actual_core,summ$potential_core,summ$core_change
                                 ,summ$actual_core_perc,summ$potential_core_perc,NA
                                 ,summ$acv,summ$potential_acv,summ$acv_change),nrow=3))
    dimnames(out) = list(c('actual', 'potential', 'change percent'),
                         c('total', 'core', 'core percent','acv'))
    out
  })

  output$map <- renderPlot({
    k=cluster_table()[[1]]
    k[, county := tolower(county)]
    temp1 = merge(
      k,
      censuscounty[, .(state, avvocounty, fip)],
      by.x = c('state', 'county'),
      by.y = c('state', 'avvocounty'),
      all = T
    )
    setnames(temp1, c('county', 'state'), c('county.name', 'state.abb'))
    temp2 = merge(temp1, county.map[, .(region, lat, long, group)], by.x = 'fip', by.y = 'region')
    s2 = unique(mapping[tolower(statename) %in% input$statename, state])
    ggplot() + geom_polygon(
      data = temp2[state.abb %in% s2],
      aes(x = long, y = lat, group = group),
      color = 'black',
      fill = rainbow(as.numeric(input$ncluster) * 2, s = .6)[temp2[state.abb %in% s2, cluster]*2 - 1]
    ) + coord_map()
  })
})
