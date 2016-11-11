shinyServer(function(input, output,session) {


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
}
)
  })

  cluster_table = reactive({
    del1=preselectterms()
#    del2=demogoutput()
    vars = strsplit(input$variables, ',')[[1]]
    temp = data.table(sapply(vars, function(x)
      ctylevel[, eval(parse(text = x))]), ctylevel[, .(state, county)])
    temp[, county := tolower(county)]
    k = km(temp, minnum=4, centers = as.numeric(input$ncluster),nstart=3,iter.max=20)
    #    setnames(k$k, 'V2', 'cluster')


  k$k = merge(k$k,traffic[pa == input$practice_area,],all.x = T,by = c('state', 'county'))
  #Add population count if not already present
  if(is.na(match('pop.base_count',strsplit(vars,',')[[1]]))){
    getpop=ctylevel[,.(pop.base_count,state,county)]
    getpop[,county:=tolower(county)]
    k$k=merge(k$k,getpop,by=c('state','county'))
  }
    k$k[is.na(k$k)]=0
    k$k[, sessions.pp := sessions/pop.base_count]
    target_visits = k$k[, quantile(sessions.pp, .7, na.rm = TRUE, type = 3), by =.(cluster)]
    setnames(target_visits, 'V1', 'target.sessions.pp')

    k$k = merge(k$k, target_visits, by = c('cluster'))
    k$k[, potential_sessions := max(sessions.pp, target.sessions.pp, na.rm = T)*pop.base_count, by = .(state, county)]

    ### start potential acv
    tempprop=potacvinput[pa==input$practice_area,.(sum(potslimps),sum(potdaimps),sum(potslacv),sum(potdaacv)),by=.(month,statename,county,pa)][,.(mean(V1),mean(V2),mean(V3),mean(V4)),by=.(statename,county,pa)]
    setnames(tempprop,c('statename','county','pa','slimps','daimps','slacv','daacv'))
    tempprop=merge(tempprop,traffic[pa==input$practice_area],by=c('pa','statename','county'),all=T)
    tempprop[,`:=`(sl_ips=slimps/sessions, da_ips = daimps/sessions, sl_aps=slacv/sessions,da_aps=daacv/sessions)]
    tempprop[is.na(tempprop)]=0
#    tempprop=merge(tempprop,unique(censuscounty[,.(state,statename)]),by='statename')

    potacv_pred=merge(tempprop[,.(statename,county,slacv,daacv,sl_aps,da_aps)],k$k[,.(statename,state,county,sessions,potential_sessions)],by=c('statename','county'),all=T)
    potacv_pred[sessions>30,`:=`(potential_sl_acv=potential_sessions*sl_aps*(1-log(potential_sessions/sessions,100)),potential_da_acv=potential_sessions*da_aps*(1-log(potential_sessions/sessions,100)))]
    potacv_pred[sessions<=30,`:=`(potential_sl_acv=slacv,potential_da_acv=daacv)]


    potacv_pred[,`:=`(potential_acv=potential_sl_acv+potential_da_acv,acv=slacv+daacv)]
    k$k=merge(k$k,potacv_pred[,.(state,county,slacv,potential_sl_acv,daacv,potential_da_acv,acv,potential_acv)],by=c('state','county'))
    ### end potential acv
#k$k=k$k[!is.na(k$k[,pt])]

#average monthly visits and potential monthly visits by cluster
    list(cbind(k$k,pa=input$practice_area), k$realcenters)
  })

    visits = reactive({
    v = cluster_table()[[1]]
    centers = cluster_table()[[2]]
    visits = v[, .(round(sum(sessions, na.rm = T)), round(sum(potential_sessions, na.rm = T)),round(sum(acv, na.rm =T)), round(sum(potential_acv, na.rm =T))), by = .(cluster)]
    setnames(visits,c(
      'cluster',
      'sessions',
      'potential_sessions',
      'acv',
      'potential_acv'
    ))

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
    visits()
    }, options = list(paging = F, pagelength = input$input$ncluster))



  output$visit_summary = renderTable({
    visits = visits()[,c('sessions','potential_sessions','acv','potential_acv'),with=F]
    summ = visits[, sapply(.SD, sum, na.rm = T)]
    summ = data.frame(t(summ))
    summ$sessions_change = round((summ$potential_sessions / summ$sessions) -1, 2)
    summ$acv_change = round((summ$potential_acv / summ$acv) -1, 2)

    out = (matrix(c(summ$sessions, summ$potential_sessions, summ$sessions_change
                    ,summ$acv, summ$potential_acv, summ$acv_change
    ),nrow=3))

    dimnames(out) = list(c('actual', 'potential', 'change percent'),
                         c('sessions', 'acv'))
out
  })



  output$downloadData <- downloadHandler(
    filename = function()
      paste(gsub(' ','_',input$practice_area),'_counties.csv',sep=''),content = function(file)
      write.csv(cluster_table()[[1]][,-match(strsplit(input$variables,',')[[1]],names(cluster_table()[[1]])),with=F],
        file,
        row.names = F,
        quote = F
      ))



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
