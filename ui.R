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
      
      # Subregion Report tab
      menuItem(
        text = "Subregion Report",
        icon = icon(name = "list-ul"),
        tabName = "report_subregion"
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
          width = 7, status = "danger", solidHeader = TRUE,
          
          # Welcome
          h2(icon("info"), "Welcome"), hr(),
          
          div(
            "Welcome to an R Shiny dashboard of Asian Economies.",
            p("This dashboard collates economic information on a number of Asian countries."),
            p(strong("No responsibility will be take by the author if misuse of this information is made."))
          ),
          
          # Using the app
          h2(icon("users"), "Using the App"), hr(), 
          
          div(
            "Each of the tabs in the app are designed to do the following things:",
            tags$ul(
              tags$li("Navigate across different tabs by clicking on the options in the left-hand black vertical box."),
              tags$li("The ", strong("title"), " tab generic information.")
            )
          ),
          
          h2(icon("question-circle-o"), "Further Information"), hr(),
          div(
            "Useful information about the Data Sources used, the 
            Construction and Security of the app are placed in the box on 
            the right hand side of this page. Please send any questions or feedback to ", 
            a(href = "mailto:a_vision@hotmail.co.uk", "my email"),
            " or to my ", a(href = "github.com/avisionh", "GitHub.")
          ), hr()
          
        ), #box
        
        box(
          width = 5, status = "danger", solidHeader = TRUE,
            
          # Data Sources
          h2(icon("database"), "Data Sources"), hr(),
            
          div(
            "This app uses data from: ", br(),
            tags$ul(
              tags$li("Data Item 1"),
              tags$li("Data Item 2"),
              tags$li("Data Item 3")
            )
          ), hr(),
            
          # Construction
          h2(icon("cogs"), "Construction"), hr(),
            
          div(
            "This app has been constructed using: ", br(),
            tags$ul(
              tags$li(a(href = "https://www.r-project.org/", "R"), "(for the data processing and calculation)"),
              tags$li(a(href = "https://shiny.rstudio.com/", "R Shiny", target = "_blank"), "(for the app design and interactivity)"),
              tags$li(a(href = "https://rstudio.github.io/shinydashboard/", "Shiny Dashboard", target = "_blank"), "(for the app layout and structure)")
            )
          ), hr()
          
        ) #box
      
      ), #tabItem
    
    # Report: Subregion -------------------------------------------------------
    
      tabItem(
        tabName = "report_subregion",
        selectInput(
          inputId = "subregion",
          label = "Please choose a subregion:",
          choices = sort(unique(data_plots_region$Subregion))
        ),
        
        fluidRow(
          # Text: Country in subregion
          box(
            title = "Countries in Subregion",  solidHeader = TRUE, status = "danger", width = 4,
            dataTableOutput(outputId = "table_subregion_countries")
          ) #box
        ), #fluidRow
        
        fluidRow(
          valueBoxOutput(outputId = "valuebox_region_avg_gdp", width = NULL),
          valueBoxOutput(outputId = "valuebox_region_debt", width = NULL),
          valueBoxOutput(outputId = "valuebox_region_trade_balance", width = NULL)
        )
      ),
    
      # Report: Country ----------------------------------------------------------
  
      tabItem(
        tabName = "report_country",
        selectInput(
          inputId = "name",
          label = "Please choose a country",
          choices = sort(data_consolidate$RegionalMember)
        ),
        
        fluidRow(

          # Table: Country Details --------------------------------------------------
          box(
            title = tags$b("Country Overview"), solidHeader = TRUE, status = "danger", width = 7, height = "25vh",
            dataTableOutput(outputId = "table_country_details", width = "100%")
          ), #box
          

          # Map: Country ------------------------------------------------------------
          box(
            title = tags$b("Map"), solidHeader = TRUE, status = "danger", width = 5, height = "25vh",
            leafletOutput(outputId = "map_country", height = "19vh")
          ) #box
          
        ), #fluidRow
        
        fluidRow(
          
          # Basic Stats
          box(
            status = "danger", width = 6,
            dataTableOutput(outputId = "table_basic_stats")
          ),
          
          column(
            width = 6,
            valueBoxOutput(outputId = "valuebox_current_gdp", width = NULL),
            valueBoxOutput(outputId = "valuebox_current_debt", width = NULL),
            valueBoxOutput(outputId = "valuebox_current_trade", width = NULL)
          ),
          
          # Plots
          tabBox(
            width = 6,
            height = "47vh",
            tabPanel(title = "GDP Percentage Change", height = "100%", plotOutput(outputId = "plot_gdpchange", height = "40vh")),
            tabPanel(title = "External Debt", height = "100%", plotOutput(outputId = "plot_debt", height = "40vh")),
            tabPanel(title = "Trade Balance", height = "100%", plotOutput(outputId = "plot_tradebalance", height = "40vh"))
          )
          
        ) #fluidRow
        
      ) #tabItem
    
    ) #tabItems
  
  ) #dashboardBody


) #dashboardPage