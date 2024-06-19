visualize_surges <- function(data, names_to_highlight) {
# Filter for specific names
filtered_surges <- surges %>%
    filter(Name %in% names_to_highlight)

# Plot
g <-
ggplot(filtered_surges, aes(x = Year, y = Name, size= Percent_Change)) +
    geom_point(alpha = 0.6, color = "red") +
    scale_size_continuous(range = c(1, 10), name = "Percent Change") +
    labs(title = "Year-on-Year Surges in Baby Name Popularity",
         x= "Year",
         y = "Name") +
    theme_minimal()
g
}