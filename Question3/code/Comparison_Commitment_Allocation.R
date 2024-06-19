# Function to compare bilateral allocations and commitments for EU members
compare_eu_countries <- function(data, top_n) {

#only EU member countries
   aid_data_eu <- data %>%
    filter(`EU member` == 1)

    #difference between allocation and commitment for each country -> commitment gap
   # check which countries allocated more/less than they committed
    country_comparison <- aid_data_eu  %>%
        mutate(Difference = `Total bilateral allocations($ billion)` - `Total bilateral commitments($ billion)`) %>%
          select(Country, Difference)

#Extract the top 3 positive and top 3 negative differences
    top_pos <- country_comparison %>%
        top_n(top_n, wt = Difference)
    top_neg <- country_comparison  %>%
        top_n(-top_n, wt = Difference)

    #Combine  top positive and negative differences
    top_countries <- bind_rows(top_pos, top_neg)

    #Barplot with top 3 countries (highest differences) on both sides
   g <-
    ggplot(top_countries, aes(x= reorder(Country, Difference), y = Difference, fill = Difference > 0)) +
     geom_bar(stat = "identity", show.legend = FALSE) +
     coord_flip() +
        labs(title= "Top 3 Positive and Negative Differences between Allocations and Commitments",
             x = "Country",
             y = "Difference ($ billion)") +
        scale_fill_manual(values = c("red", "green")) +
        theme_minimal()
   g
}