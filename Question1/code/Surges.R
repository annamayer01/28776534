identify_year_on_year_surges <- function(data, percent_change) {

#Popularity by name by year, national level and calculating total count of name in year
national_popularity <- data %>%
       group_by(Year, Name) %>%
       summarise(Total_Count = sum(Count, na.rm = TRUE)) %>%
        ungroup()

#percentage change in name counts for one year after the other
national_popularity<- national_popularity %>%
       arrange(Name, Year) %>%
       group_by(Name)   %>%
        mutate(Previous_Count = lag(Total_Count),
         Percent_Change = (Total_Count - Previous_Count) / Previous_Count * 100) %>%
        ungroup()

#Identifying those names with a very strong/significant sudden increases (e.g., > x%)
significant_surges<- national_popularity %>%
        filter(!is.na(Percent_Change) & Percent_Change > percent_change)

   significant_surges
}