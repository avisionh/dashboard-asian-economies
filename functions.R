# ----------- #
# functions.R #
# ----------- #
# DESC: The functions.R script houses functions for the app.


# Row Bind Transformation -------------------------------------------------
# Need to use lazy evaluation
transform_for_row_bind <- function(x, key_name, col_name) {
  x <- x %>%
    mutate(key = key_name) %>%
    rename(value = as.name(col_name)) %>%
    select(CountryCode, RegionalMember, Year, Subregion, key, value)
  return(x)
}

# GDP ValueBox Info -------------------------------------------------------

add_columns_gdp <- function(x, column){
  
  # create columns to refer to in valueBox() args in server.R
  x <- x %>% 
    mutate(
      colour_gdp = case_when(
        column < 0     ~ "red",
        column == 0    ~ "yellow",
        column > 0     ~ "green",
        TRUE           ~ "light-blue"
      ),
      icon_gdp = case_when(
        column < 0     ~ "exclamation-triangle",
        column == 0    ~ "exclamation-circle",
        column > 0     ~ "certificate",
        TRUE           ~ "question-circle"
      ),
      tooltip_gdp = case_when(
        column < 0     ~ "This country is forecasted to have negative economic growth in GDP from 2018-19.",
        column == 0    ~ "This country is forecasted to have no economic growth in GDP from 2018-19.",
        column > 0     ~ "This country is forecasted to have positive economic growth in GDP from 2018-19.",
        TRUE           ~ "This country does not have forecasts for economic growth in GDP from 2018-19."
      )
    )
  return(x)
}


# External Debt Outstanding Info ------------------------------------------

add_columns_debt <- function(x, column) {
  
  # create columns to refer to in valueBox() args in server.R
  x <- x %>% 
    mutate(
      colour_debt = case_when(
        column < 0     ~ "green",
        column == 0    ~ "yellow",
        column > 0     ~ "red",
        TRUE           ~ "light-blue"
      ),
      icon_debt = case_when(
        column < 0     ~ "exclamation-triangle",
        column == 0    ~ "exclamation-circle",
        column > 0     ~ "certificate",
        TRUE           ~ "question-circle"
      ),
      tooltip_debt = case_when(
        column < 0     ~ "This country has negative debt, meaning they are owed money by other countries from 2017.",
        column == 0    ~ "This country has no debt, meaning they do not owe money to other countries from 2017.",
        column > 0     ~ "This country has positive debt, meaning they owe money to other countries from 2017.",
        TRUE           ~ "This country has no information available for external outstanding debt for 2017."
      )
    )
  return(x)
      
}


# Custom ggplot Function -----------------------------------------------------
# DESC: Generic function to plot GDP Change, Trade Balance, Debt Outstanding
custom_ggplot <- function(data, axis_x, axis_y, colours_column, plot_title, plot_subtitle, axis_y_title) {
  ggplot(data = data, mapping = aes(x = axis_x, y = axis_y)) +
    geom_line(colour = "grey") +
    
    #add custom colouring on negative and positive values
    geom_point(mapping = aes(colour = colours_column), size = 3) +
    scale_colour_manual(values = c("#0072B2", "#D55E00")) +
    
    # add 0-horizontal line
    geom_hline(yintercept = 0, linetype = "dashed") +
    
    # include £ prefix
    theme_classic() + scale_y_continuous(labels = dollar_format(prefix = "\U00a3")) +
    
    # force unique academic years
    scale_x_continuous(breaks = unique(axis_x)) +
    labs(title = plot_title, subtitle = plot_subtitle) +
    xlab("Academic Year") + ylab(axis_y_title) +
    
    # general plot themes
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(face = "bold", hjust = 0.5),
          title = element_text(face = "bold"),
          legend.position = "none")
}
