
library(plotly)
dashboardPage(
  dashboardHeader(title = ""),
  dashboardSidebar(


   sidebarMenu(id = "sidebarmenu",
 menuItem("Overview", tabName =  "main", icon = icon("th")),
        menuItem("Player Overview", tabName = "dashboard", icon = icon("th")),
        conditionalPanel("input.sidebarmenu === 'dashboard'",
        uiOutput("postionBox"),
        uiOutput("teamBox"),
uiOutput("playerBox"),
        uiOutput("teamBox_p2"),
uiOutput("playerBox_p2")),
        menuItem("Head to Head", tabName =  "rawdata", icon = icon("th"))
    )),
  dashboardBody(
    tabItems(
      tabItem("main", 
              fluidRow(h1("NLF Data Analysis"), imageOutput("splashImage")), 
              fluidRow(h1("Data Flow"), imageOutput("dataflowImage"))),
      tabItem("dashboard",
        fluidRow(
          valueBoxOutput("rate"),
          valueBoxOutput("count"),
          valueBoxOutput("users")
        ),
        plotlyOutput("dashboard_plot1")
      ),
      tabItem("Head to Head",
        numericInput("maxrows", "Rows to show", 25),
        verbatimTextOutput("rawtable"),
        downloadButton("downloadCsv", "Download as CSV")
      )
    )
  )
)

