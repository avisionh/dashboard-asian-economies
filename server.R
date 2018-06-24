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
  

  # Reactive: Selected Country on data_plots ----------------------------------
  # Create reactive function to store the user's selected country to reuse for plots
  select_country_plots <- reactive(
    x = {
      select_country_plots <- data_plots %>% 
        filter(RegionalMember == input$name)
      return(select_country_plots)
    }
  )
  

  # Reactive: Selected Country on data_basicstats ---------------------------
  select_country_stats <- reactive(
    x = {
      data_select_country_stats <- data_basicstats %>% 
        filter(RegionalEconomy == input$name)
      return(data_select_country_stats)
    }
  )

  
  # Reactive: Selected Subregion on data_plots_region -----------------------
  # Create reactive function to store the user's selected country to reuse for report
  select_subregion_plots <- reactive(
    x = {
      select_subregion_plots <- data_plots_region %>% 
        filter(Subregion == input$subregion)
      return(select_subregion_plots)
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
            subtitle = "Lastest GDP Rate Forecast", 
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
            value = paste0(dollar(value_debt_outstanding()$OutstandingDebtUSDollar2017), "m"),
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
  
  
  # InfoBox: Current Trade Balance ---------------------------------------
  # 1. Extract latest debt outstanding value
  value_trade_balance <- reactive(
    x = {
      current_trade <- select_country() %>% 
        select(TradeBalance2019, colour_trade, icon_trade, tooltip_trade)
      return(current_trade)
    }
  )
  
  # 2. Create output valueBox
  output$valuebox_current_trade <- renderValueBox(
    expr = {
      tags$div(
        tipify(
          el = valueBox(
            value = paste0(dollar(value_trade_balance()$TradeBalance2019), "m"),
            subtitle = "Latest Trade Balance Forecast", 
            icon = icon(name = value_trade_balance()$icon_trade), 
            color = value_trade_balance()$colour_trade
          ),
          title = value_trade_balance()$tooltip_trade,
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
        plot_title = "GDP Change per Year",
        plot_subtitle = input$name,
        axis_y_title = "GDP Percentage Change",
        axis_y_suffix = "%"
      )
    }
    
  )
  

  # Plot: External Debt Outstanding -----------------------------------------
  output$plot_debt <- renderPlot(
    
    expr = {
      
      # filter dataframe for external debt outstanding
      debt_outstanding <- select_country_plots() %>% 
        filter(key == "DebtOutstandingUSDollarMillion")
      
      custom_ggplot(
        data = debt_outstanding,
        axis_x = debt_outstanding$Year,
        axis_y = debt_outstanding$value,
        colours_column = debt_outstanding$colour,
        plot_title = "External Debt Outstanding",
        plot_subtitle = input$name,
        axis_y_title = "Value of outstanding external debt",
        axis_y_prefix = "US$", axis_y_suffix = "m"
      )
    }
    
  )
  

  # Plot: Trade Balance -----------------------------------------------------
  output$plot_tradebalance <- renderPlot(
    
    expr = {
      
      # filter dataframe for trade balance
      trade_balance <- select_country_plots() %>% 
        filter(key == "TradeBalanceInUSDollarMillion")
      
      custom_ggplot(
        data = trade_balance,
        axis_x = trade_balance$Year,
        axis_y = trade_balance$value,
        colours_column = trade_balance$colour,
        plot_title = "Trade Balance",
        plot_subtitle = input$name,
        axis_y_title = "Value of trade balance",
        axis_y_prefix = "US$", axis_y_suffix = "m"
      )
    }
  )
  

  # Table: Basic Statistics -------------------------------------------------
  output$table_basic_stats <- renderDataTable(
    expr = {
      datatable(
        
        data = select_country_stats() %>% 
          select(Statistic, Year, Value, UnitOfMeasurement, SustainableDevelopmentGoal) %>% 
          rename(
            `Unit of Measurement` = UnitOfMeasurement,
            `Sustainable Development Goal` = SustainableDevelopmentGoal
          ),
        
        # add strips to left and right of cells and set width
        class = "cell-border stripe",
        
        # add filter boxes to each column and turn off rownames
        filter = list(position = "top", clear = TRUE), rownames = FALSE,
        
        # set rows to 10, add download button, and order by Statistic
        extensions = "Buttons",
        
        options = list(
          pageLength = 10, scrollX = TRUE, order = list(1, "asc"),
          
          # fix row height
          lengthChange = FALSE, scrollY = "39vh",
          
          # only show table and pagination/page no.s at bottom
          info = FALSE,
          # add download buttons
          dom = 'Bfrtip',
          buttons = list(
            list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'), text = "Download"), 
            "print"
          ),
          
          # colour table header black
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"
          ),
          
          # show only first 20 characters of Name
          columnDefs = list(
            list(
              targets = 0, render = JS(
                "function(data, type, row, meta) {",
                "return type === 'display' && data.length > 20 ?",
                "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                "}"
              ) #JS
            ) #list
          ) #list
        ) #list
      ) %>%
      
      # make Statistic column bold and colour Value column
      formatStyle(columns = "Statistic", fontWeight = "bold") %>% 
      formatStyle(columns = "Value", fontWeight = "bold", color = "#cccc00")
    },
    
    # allow download of entire data_basicstats table
    server = FALSE
  )
  
   
# --- Subregion Report --- #   

  # Text: Subregion countries -----------------------------------------------
  # Create table of countries in subregion
  output$table_subregion_countries <- renderDataTable(
    expr = {
      datatable(
        data = data_consolidate %>% 
          filter(Subregion == input$subregion) %>%
          select(RegionalMember),
        rownames = FALSE,
        options = list(lengthChange = FALSE, scrollY = "30vh", searching = FALSE, info = FALSE, paging = FALSE, ordering = FALSE)
      ) #datatable
    }
  ) #renderDataTable
  

  # InfoBox: Subregion Average GDP Growth  ----------------------------------
  # 1. Extract average GDP Change
  value_region_avg_gdp <- reactive(
    x = {
      avg_gdp <- select_subregion_plots() %>%
        filter(key == "GDPGrowthperYearPercent")
      return(avg_gdp)  
    }
  )
  
  # 2. Create output valueBox
  output$valuebox_region_avg_gdp <- renderValueBox(
    expr = {
      tags$div(
        tipify(
          el = valueBox(
            value = paste0(value_region_avg_gdp()$mean_value, "%"),
            subtitle = "Average GDP Growth", 
            icon = icon(name = "piggy-bank", lib = "glyphicon"), 
            color = "maroon"
          ),
          title = "This is the average GDP growth rate of the region from 2013 to 2019 (where 2019 is a forecasted figure)",
          placement = "left", trigger = "hover"
        ) #tipify
      ) #div
    }
  ) #renderValueBox
  
  
  # InfoBox: Subregion Average Debt Outstanding  ----------------------------------
  # 1. Extract average GDP Change
  value_region_debt <- reactive(
    x = {
      avg_debt <- select_subregion_plots() %>%
        filter(key == "DebtOutstandingUSDollarMillion")
      return(avg_debt)  
    }
  )
  
  # 2. Create output valueBox
  output$valuebox_region_debt <- renderValueBox(
    expr = {
      tags$div(
        tipify(
          el = valueBox(
            value = paste0(dollar(value_region_debt()$mean_value), "m"),
            subtitle = "Average External Debt", 
            icon = icon(name = "hand-holding-usd"), 
            color = "orange"
          ),
          title = "This is the average external debt outstanding for the region from 2013 to 2017",
          placement = "left", trigger = "hover"
        ) #tipify
      ) #div
    }
  ) #renderValueBox
  
  
  # InfoBox: Subregion Average Trade Balance  ----------------------------------
  # 1. Extract average GDP Change
  value_region_trade_balance <- reactive(
    x = {
      avg_trade_balance <- select_subregion_plots() %>%
        filter(key == "TradeBalanceInUSDollarMillion")
      return(avg_trade_balance)  
    }
  )
  
  # 2. Create output valueBox
  output$valuebox_region_trade_balance <- renderValueBox(
    expr = {
      tags$div(
        tipify(
          el = valueBox(
            value = paste0(dollar(value_region_trade_balance()$mean_value), "m"),
            subtitle = "Average Trade Balance", 
            icon = icon(name = "handshake"), 
            color = "aqua"
          ),
          title = "This is the average trade balance for the region from 2013 to 2019 (where 2019 is a forecasted figure)",
          placement = "left", trigger = "hover"
        ) #tipify
      ) #div
    }
  ) #renderValueBox
  
  # Observer - Drill-through from Subregion to Country Report -------------------------------
  
  observeEvent(
    
    eventExpr = {input$table_subregion_countries_rows_selected},
    
    handlerExpr = {
      # user-selected variable
      
      # 1. User selects subregion in report_subregion which reduces data_consolidate rows, table_subregion_countries
      # 2. User selecting country here wants this country to change on report_country
      # 3. temp[input$table_subregion_countries_rows_selected, 1] in name_country returns row number
      #     hence need to filter data_consolidate, temp, before creating name_country
      #     to obtain user-selected country from report_subregion
      temp <- data_consolidate %>% filter(Subregion == input$subregion)
      name_country <- as.character(temp[input$table_subregion_countries_rows_selected, 1])
      
      updateTabItems(session, inputId = "menu", selected = "report_country")
      updateSelectInput(session, inputId = "name", selected = name_country)
      dataTableProxy(outputId = "table_subregion_countries") %>% 
        selectRows(selected = list())
    }
  )
  
}