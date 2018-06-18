# -------- #
# server.R #
# -------- #

# DESC: server.R code can be much longer and complicated than in ui.R.
#       Is where all the dynamic data manipulation happens, and plot creations.
#       e.g. Filtering based on user inputs and generating plots based on dynamically filtered data.
#       server.R must create a function called server, like below:

server <- function(input, output, session) {
 
# --- Country Report --- #

  # Reactive: Selected Country on data_consolidate ----------------------------------------------
  # create reactive function to store the user's selected country to reuse later
  select_country <- reactive(
    x = {
    data_select_country <- data_consolidate %>% 
      filter(RegionalMember == input$name)
    
    return(data_select_country)
    }
  )
  

  # Reactive: Selected Country on data_gdp ----------------------------------
  # Create reactive function to store the user's selected country to reuse for plots
  select_country_plots <- reactive(
    x = {
      select_country_plots <- data_plots %>% 
        filter(RegionalMember == input$name)
      return(select_country_plots)
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
  # 1. Extract current GDP Change per year
  value_current_gdp <- reactive(
    x = {
      current_gdp <- select_country() %>% 
        select(GDPRate201819, colour_gdp, icon_gdp, tooltip_gdp)
      return(current_gdp)  
    }
  )
  
  # 2. Create output valueBox
  output$valuebox_current_gdp <- renderValueBox(
    expr = {
      tags$div(
        tipify(
          el = valueBox(
            value = paste0(value_current_gdp()$GDPRate201819, "%"),
            subtitle = "Forecasted 2018-19 GDP Rate", 
            icon = icon(name = value_current_gdp()$icon_gdp), 
            color = value_current_gdp()$colour_gdp
          ),
          #note can't put apostrophes in title
          title = value_current_gdp()$tooltip_gdp, 
          placement = "left", trigger = "hover"
        ) #tipify
      ) #div
    }
  ) #renderValueBox
  

  # InfoBox: Current Debt Outstanding ---------------------------------------
  # 1. Extract latest debt outstanding value
  value_debt_outstanding <- reactive(
    x = {
      current_debt <- select_country() %>% 
        select(OutstandingDebtUSDollar2017, colour_debt, icon_debt, tooltip_debt)
      return(current_debt)
    }
  )
  
  # 2. Create output valueBox
  output$valuebox_current_debt <- renderValueBox(
    expr = {
      tags$div(
        tipify(
          el = valueBox(
            value = paste0("$", value_debt_outstanding()$OutstandingDebtUSDollar2017, "m"),
            subtitle = "Latest Debt Outstanding", 
            icon = icon(name = value_debt_outstanding()$icon_debt), 
            color = value_debt_outstanding()$colour_debt
          ),
          title = value_debt_outstanding()$tooltip_debt,
          placement = "left", trigger = "hover"
        ) #tipify
      ) #div
    }
  ) #renderValueBox


  # Plot: GDP Change --------------------------------------------------------
  output$plot_gdpchange <- renderPlot(
    
    expr = {
      
      # filter dataframe for gdp
      gdp_change <- select_country_plots() %>% 
        filter(key == "GDPGrowthperYearPercent")
      
      custom_ggplot(
        data = gdp_change,
        axis_x = gdp_change$Year,
        axis_y = gdp_change$value,
        colours_column = gdp_change$colour,
        plot_title = "GDP change per year in percent",
        plot_subtitle = input$name,
        axis_y_title = "Percentage Change",
        axis_y_suffix = "%"
      )
    }
    
  )
  
  
    
}