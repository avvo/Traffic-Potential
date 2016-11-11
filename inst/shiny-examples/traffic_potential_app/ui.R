ui <- dashboardPage(
  dashboardHeader(title = "Avvo Potential Traffic"),
  dashboardSidebar(
    radioButtons("preselect",
                 "Preselec:",
                 choices=list("Immigration"=1,"Divorce & Separation"=2,"Criminal Defense"=3,"DUI & DWI"=4,"Real Estate"=5)),

    radioButtons("ncluster",
                "Number of clusters:",
                choices=list("5"=5,"8"=8,"15"=15,"25"=25,"35"=35),selected=5),

    textInput("practice_area",
              label="Practice Area:",
                value='all'),

        selectInput("statename",
                label="State:",
                multiple=T,
                selected='washington',
                choices=as.list(tolower(state.name))),

    textInput("variables",
              label="variables:",
              value='cit.not_citizen,hh.median_income,(eth.white+eth.black)/eth.base_count'),

     selectInput("possible variables",
                label="Possible variables:",
                multiple=T,
                selected='',
                choices=as.list(sort(names(ctylevel)))),

    radioButtons("demog",
                 "Output Demographics?",
                 choices=list("Yes"='yes',"No"='no'),selected='no')


    ),

  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(shinydashboard::box(width=NULL,solidHeader = T,status='primary',title='Clusters',
      dataTableOutput("clustermeans"))),

    fluidRow(shinydashboard::box(width=NULL,solidHeader = T, status='primary',title='Summary',
                 tableOutput("visit_summary"))),

    fluidRow(shinydashboard::box(width=NULL,solidHeader = T, status='primary',title='Download',
                   downloadButton('downloadData', 'Download County Level Data'))),

    fluidRow(
      plotOutput("map"))
    )
  )


