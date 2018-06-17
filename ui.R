# ---- #
# ui.R #
# ---- #
# DESC: The ui.R script should be relatively short and straightforward. 
#       All that happens here is setting out where things go.
#       There are no calculations.
#       ui.R must create an object called ui, for example:

ui <- dashboardPage(
  
  # Title and Skin
  title = "Asian Economies",
  skin = "red",
  
  # Header
  header = dashboardHeader(
    title = "Dashboard: Asian Economies"
  ),
  
  # Sidebar
  sidebar = dashboardSidebar(
    
    sidebarMenu(
      id = "menu",
      
      # Guidance tab
      menuItem(
        text = "Guidance",
        icon = icon(name = "info-circle"),
        tabName = "info_guidance"
      ),
      
      # Country Report tab
      menuItem(
        text = "Country Report",
        icon = icon(name = "window-maximize"),
        tabName = "report_country"
      )
               
               
    ) #sideMenu
  ), #dashboardSidebar  
    
  # Body
  body = dashboardBody(
    tabItems(

    # Content: Guidance Tab ---------------------------------------------------
      tabItem(
        tabName = "info_guidance",
        
        box(
          width = 7, status = "warning", solidHeader = TRUE,
          
          # Welcome
          h2(icon("info"), "Welcome"), hr(),
          
          div(
            style = "font-size: 2vh;",
            "Welcome to an R Shiny dashboard of Asian Economies.",
            p("This dashboard collates economic information on a number of Asian countries.",
              strong("No responsibility will be take by the author if misuse of this information is made."))
          ),
          
          # Using the app
          h2(icon("users"), "Using the App"), hr(), 
          
          div(
            "Each of the tabs in the app are designed to do the following things:",
            tags$ul(
              style = "font-size: 2vh;",
              tags$li("Navigate across different tabs by clicking on the options in the left-hand black vertical box."),
              tags$li("The ", strong("title"), " tab generic information.")
            )
          ),
          
          h2(icon("question-circle-o"), "Further Information"), hr(),
          h4("Useful information about the Data Sources used, the 
             Construction and Security of the app are placed in the box on 
             the right hand side of this page. Please send any questions or feedback to ", 
             a(href = "mailto:a_vision@hotmail.co.uk"),
             " or on GitHub."), hr()
          
        ), #box
        
        box(
          width = 5, status = "warning", solidHeader = TRUE,
            
          # Data Sources
          h2(icon("database"), "Data Sources"), hr(),
            
          h4("This app uses data from: "), br(),
          h4(
            tags$ul(
              tags$li("Data Item 1"),
              tags$li("Data Item 2"),
              tags$li("Data Item 3")
            )
          ),
            
          hr(),
            
          # Construction
          h2(icon("cogs"), "Construction"), hr(),
            
          h4("This app has been constructed using: "), br(),
          h4(
            tags$ul(
              tags$li(a(href = "https://www.r-project.org/", "R"), "(for the data processing and calculation)"),
              tags$li(a(href = "https://shiny.rstudio.com/", "R Shiny", target = "_blank"), "(for the app design and interactivity)"),
              tags$li(a(href = "https://rstudio.github.io/shinydashboard/", "Shiny Dashboard", target = "_blank"), "(for the app layout and structure)")
            )
          ),
            
          hr()
        ) #box
      
      ), #tabItem
    

      # Country Report ----------------------------------------------------------
  
      tabItem(
        tabName = "report_country",
        selectInput(
          inputId = "name",
          label = "Please choose a country",
          choices = sort(data_consolidate$Name)
        ),
        
        fluidRow(
          # Country details
          box(
            
          ) #box
        ) #fluidRow
      ) #tabItem
      
    ) #tabItems
  
  ) #dashboardBody


) #dashboardPage