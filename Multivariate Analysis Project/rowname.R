library(dplyr)

month_to_name <- function(month_number) {
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  return(months[month_number])
}

pop_drought = read.csv('PopulationDrought.csv')
pop_drought = pop_drought[,-1]

pop_drought$Month = month_to_name(pop_drought$Month)

pop_drought$month_year = paste(pop_drought$Month, pop_drought$Year)

pop_drought <- pop_drought %>%
  group_by(month_year) %>%
  mutate(week_month_year = paste0(row_number(), " ", month_year))

rownames = pop_drought$week_month_year

pop_drought = pop_drought[, -c(1,2,13,14)]

pop_drought = as.data.frame(pop_drought)

rownames(pop_drought) = rownames

