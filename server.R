# -------- #
# server.R #
# -------- #

# DESC: server.R code can be much longer and complicated than in ui.R.
#       Is where all the dynamic data manipulation happens, and plot creations.
#       e.g. Filtering based on user inputs and generating plots based on dynamically filtered data.
#       server.R must create a function called server, like below:

server <- function(input, output, session) {
 
# --- Country Report --- #

  # Reactive: Selected Country ----------------------------------------------
  # create reactive function to store the user's selected country to reuse later
  select_country <- reactive(
    x = {
    data_select_country <- data_consolidate %>% 
      filter(RegionalMember == input$name)
    
    return(data_select_country)
    }
  )
  

  # Scaffold Country Details ---------------------------------------------------------
  # 1. Fill scaffold_country_details dataframe
  country_details <- reactive (
    x = {
      scaffold_country_details[,2] <- c(
        select_country()$CountryCode,
        select_country()$RegionalMember,
        select_country()$Subregion
      )
      scaffold_country_details[,4] <- c(
        select_country()$Symbol,
        select_country()$Currency,
        NA
      )
      
      return(scaffold_country_details)
    }
  )
  
  # 2. Create output dataframe
  output$table_country_details <- renderDataTable(
    expr = {
      datatable(
        data = country_details(), rownames = FALSE, selection = "none",
        options = list(lengthChange = FALSE, searching = FALSE, info = FALSE,
                       paging = FALSE, ordering = FALSE,
                       columnDefs = list(className = "dt-left", targets = 1:4))) %>% 
        formatStyle(columns = c("Country Details", "Currency Details"), fontWeight = "bold")
    }
  )
  

  # InfoBox: Current GDP Change per year ------------------------------------
  # 1. Extract current GDP Changer per year
  value_current_gdp <- reactive(
    x = {
      current_gdp <- select_country() %>% 
        select(GDPRate201819)
      return(current_gdp)  
    }
  )
  
  # 2. Create output valueBox
  output$valuebox_current_gdp <- renderValueBox(
    expr = {
      tags$div(
        tipify(
          el = valueBox(
            value = paste0(value_current_gdp(), "%"),
            subtitle = "Forecast: GDP Change from 2018-19", icon = icon(name = "certificate"), color = "green"
          ),
          #note can't put apostrophes in title
          title = "This is a forecast. It shows the percentage change in the country GDP from 2018 to 2019.", 
          placement = "left", trigger = "hover")
      )
    }
  )
  
}