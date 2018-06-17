# ----------- #
# functions.R #
# ----------- #
# DESC: The functions.R script houses functions for the app.


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
