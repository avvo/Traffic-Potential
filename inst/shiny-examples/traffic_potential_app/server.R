shinyServer(function(input, output,session) {


#input=data.frame(ncluster=5,practice_area='Others',statename='washington',variables="cit.not_citizen,hh.median_income")
#input$variables=as.vector(input$variables)
#input$practice_area=as.vector(input$practice_area)

    preselectterms=reactive({
    switch(as.numeric(input$preselect),
{
  cat('DEL',input$preselect)
  updateSelectInput(session,'variables',selected='cit.not_citizen,hh.median_income,eth.hispanic,edu.enrolled')
    updateSelectInput(session,'practice_area',selected='Immigration')
    updateRadioButtons(session,'ncluster',selected=35)
},
{updateSelectInput(session,'variables',selected='mar.yes,hh.married_couple,hh.median_income,fer.birth_count,fer.unmarried_birth_pml,emp.no')
  updateSelectInput(session,'practice_area',selected='Divorce & Separation')
  updateRadioButtons(session,'ncluster',selected=35)
},
{updateSelectInput(session,'variables',selected='age_male,emp.no/pop.base_count,hh.median_income,pov.below/pop.base_count,hh.male_only,eth.white+eth.asian')
  updateSelectInput(session,'practice_area',selected='Criminal Defense')
  updateRadioButtons(session,'ncluster',selected=35)
},
{updateSelectInput(session,'variables',selected='(pop.15_19+pop.20_24+pop.25_29),emp.no/pop.base_count,hh.median_income,pov.below/pop.base_count,hh.male_only,eth.white')
  updateSelectInput(session,'practice_area',selected='DUI & DWI')
  updateRadioButtons(session,'ncluster',selected=35)
},
{updateSelectInput(session,'variables',selected='hh.median_income,emp.military,eth.white,hh.families,(pop.25_29+pop.20_24+pop.15_19+pop.10_14+pop.5_9)')
  updateSelectInput(session,'practice_area',selected='Real Estate')
  updateRadioButtons(session,'ncluster',selected=35)
},
{updateSelectInput(session,'variables',selected='(pop.15_19+pop.20_24+pop.25_29),emp.no/pop.base_count,hh.median_income,pov.below/pop.base_count,hh.male_only,eth.white')
  updateSelectInput(session,'practice_area',selected='Others')
  updateRadioButtons(session,'ncluster',selected=35)
}
)
  })

  cluster_table = reactive({
    del1=preselectterms()
#    del2=demogoutput()
    vars = strsplit(input$variables, ',')[[1]]
    if(input$practice_area=='all')
      inputpa=unique(traffic$pa) else
        inputpa=strsplit(input$practice_area, ',')[[1]]
    temp = data.table(sapply(vars, function(x)
      ctylevel[, eval(parse(text = x))]), ctylevel[, .(state, county)])
    temp[, county := tolower(county)]
    k = km(temp, minnum=4, centers = as.numeric(input$ncluster),nstart=3,iter.max=20)
    #    setnames(k$k, 'V2', 'cluster')

    traffic2=melt.data.table(traffic[pa %in% inputpa][,setdiff(names(traffic),'pa'),with=F],id=c(1,2,5),variable='pt',value='sessions')

    temptraffic=traffic2[,.(sum(sessions,na.rm=T)),by=.(state,county,statename,pt)]
    setnames(temptraffic,'V1','sessions')
    k$k = merge(k$k,temptraffic,all.x = T,by = c('state', 'county'))


#Add population count if not already present
  if(is.na(match('pop.base_count',strsplit(vars,',')[[1]]))){
    getpop=ctylevel[,.(pop.base_count,state,county)]
    getpop[,county:=tolower(county)]
    k$k=merge(k$k,getpop,by=c('state','county'))
  }
    k$k[is.na(k$k)]=0
    k$k[, sessions.pp := sessions/pop.base_count]
    target_visits = k$k[, quantile(sessions.pp, .7, na.rm = TRUE, type = 3), by =.(cluster,pt)]
    setnames(target_visits, 'V1', 'target.sessions.pp')

    k$k = merge(k$k, target_visits, by = c('cluster','pt'))
    k$k[, potential_sessions := max(sessions.pp, target.sessions.pp, na.rm = T)*pop.base_count, by = .(state, county,pt)]

    ### start potential acv
    tempprop=potacvinput[,.(sum(potslimps),sum(potdaimps),sum(potslacv),sum(potdaacv)),by=.(month,statename,county,pt)][,.(mean(V1),mean(V2),mean(V3),mean(V4)),by=.(statename,county,pt)]
    setnames(tempprop,c('statename','county','pt','slimps','daimps','slacv','daacv'))
    tempprop=merge(tempprop,temptraffic,by=c('statename','county','pt'),all=T)
    tempprop[,`:=`(sl_ips=slimps/sessions, da_ips = daimps/sessions, sl_aps=slacv/sessions,da_aps=daacv/sessions)]
    tempprop[is.na(tempprop)]=0
#    tempprop=merge(tempprop,unique(censuscounty[,.(state,statename)]),by='statename')

    print('a1')
    print(sum(k$k$sessions,na.rm=T))
    potacv_pred=merge(tempprop[,.(statename,county,pt,slacv,daacv,sl_aps,da_aps)],k$k[,.(statename,state,county,pt,sessions,potential_sessions)],by=c('statename','county','pt'),all=T)
    print('b2')
    print(sum(k$k$sessions,na.rm=T))
    potacv_pred[sessions>30,`:=`(potential_sl_acv=potential_sessions*sl_aps*(1-log(potential_sessions/sessions,3000)),potential_da_acv=potential_sessions*da_aps*(1-log(potential_sessions/sessions,4000)))]
    print('c3')
    print(sum(k$k$sessions,na.rm=T))
    potacv_pred[sessions<=30,`:=`(potential_sl_acv=slacv,potential_da_acv=daacv)]


    print('d4')
    print(sum(k$k$sessions,na.rm=T))
    potacv_pred[,`:=`(potential_acv=potential_sl_acv+potential_da_acv,acv=slacv+daacv)]
    print('e5')
    k$k=merge(k$k,potacv_pred[,.(state,county,pt,slacv,potential_sl_acv,daacv,potential_da_acv,acv,potential_acv)],by=c('state','county','pt'))
    ### end potential acv
k$k=k$k[!is.na(k$k[,pt])]

print(sum(k$k$sessions,na.rm=T))

#average monthly visits and potential monthly visits by cluster
    list(cbind(k$k,pa=input$practice_area), k$realcenters)
  })

    visits = reactive({
    v = cluster_table()[[1]]
    centers = cluster_table()[[2]]
    visits1 = v[, .(round(sum(sessions, na.rm = T)), round(sum(potential_sessions, na.rm = T)),round(sum(acv, na.rm =T)), round(sum(potential_acv, na.rm =T))), by = .(cluster)]
    setnames(visits1,c(
      'cluster',
      'sessions',
      'potential_sessions',
      'acv',
      'potential_acv'
    ))
    visits2 = v[, .(round(sum(sessions, na.rm = T)), round(sum(potential_sessions, na.rm = T)),round(sum(acv, na.rm =T)), round(sum(potential_acv, na.rm =T))), by = .(cluster,pt)][pt=='core']
    setnames(visits2,c(
        'cluster',
        'pt',
        'sessions_core',
        'potential_sessions_core',
        'acv_core',
        'potential_acv_core'
      ))
    visits=merge(visits1,visits2[,-c('pt'),with=F],by='cluster')

    # In table, show names of county in selected states
    countynamestemp = unique(v[state %in% state.abb[tolower(state.name) %in% input$statename],.(county,cluster)])
    countynames = by(unique(countynamestemp$county),
                     countynamestemp$cluster,
                     paste,collapse = ',')
    countynames = data.table(cluster = names(countynames),
                             names = as.vector(countynames))
    setnames(countynames, 'names', paste(paste(input$statename, collapse =','), 'counties'))
    #get counts of counties nationwide in each cluster
    tempout = data.table(centers, count = table(v$cluster))
    setnames(tempout, 'count.V1', 'cluster')
    tempout[, cluster := as.numeric(cluster)]
    countynames[, cluster := as.numeric(cluster)]
    temp1 = merge(tempout, visits, by = 'cluster')
    temp2 = merge(temp1, countynames, by = 'cluster', all = T)[!is.na(pt)]
  })


  output$clustermeans <- renderDataTable({
  visits()}, options = list(paging = F, pagelength = input$input$ncluster))



  output$visit_summary = renderTable({
    visits = visits()[,c('sessions','potential_sessions','acv','potential_acv','sessions_core','potential_sessions_core','acv_core','potential_acv_core'),with=F]
    summ = visits[, sapply(.SD, sum, na.rm = T)]
    summ = data.frame(t(summ))
    summ$sessions_change = round((summ$potential_sessions / summ$sessions) -1, 2)
    summ$sessions_core_change = round((summ$potential_sessions_core / summ$sessions_core) -1, 2)
    summ$sessions_core_percent = round((summ$sessions_core / summ$sessions), 2)
    summ$potential_sessions_core_percent = round((summ$potential_sessions_core / summ$potential_sessions), 2)
    summ$acv_change = round((summ$potential_acv / summ$acv) -1, 2)
    summ$acv_core_change = round((summ$potential_acv_core / summ$acv_core) -1, 2)
    summ$acv_core_percent = round((summ$acv_core / summ$acv), 2)
    summ$potential_acv_core_percent = round((summ$potential_acv_core / summ$potential_acv), 2)

    out = (matrix(c(summ$sessions, summ$potential_sessions, summ$sessions_change
                    ,summ$sessions_core, summ$potential_sessions_core, summ$sessions_core_change
                    ,summ$sessions_core_percent, summ$potential_sessions_core_percent, NA
                    ,summ$acv, summ$potential_acv, summ$acv_change
                    ,summ$acv_core, summ$potential_acv_core, summ$acv_core_change
                    ,summ$acv_core_percent, summ$potential_acv_core_percent, NA
    ),nrow=3))

    dimnames(out) = list(c('actual', 'potential', 'change percent'),
                         c('sessions', 'core sessons', 'sessions core %','acv','core acv','acv core %'))
out
  })



  output$downloadData <- downloadHandler(
    filename = function()
      paste(gsub(' ','_',input$practice_area),'_counties.csv',sep=''),content = function(file)
      write.csv(cluster_table()[[1]],
        file,
        row.names = F,
        quote = F
      )
  )


  output$map <- renderPlot({
    k=unique(cluster_table()[[1]][,.(state,county,cluster)])
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
