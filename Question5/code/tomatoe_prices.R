# Average monthly tomatoe prices over time -comparison
plot_monthly_tomato_prices <- function(data1, data2) {

 #such that both data sets have the same names for ingredients
        retailer_data <- data1 %>%
        mutate(ingredient = category)

        # Filter for tomato prices, focusing on monthly data
    retailer_tomatoes <- retailer_data %>%
        filter(str_to_lower(ingredient) == "tomatoes") %>%
        mutate(month = floor_date(price_date, "month"))

    #same for other data set
    statssa_tomatoes <-data2 %>%
        filter(str_to_lower(ingredient) == "tomatoes") %>%
        mutate(month = floor_date(date,  "month"))

# calc average monthly prices for retailer data
    retailer_monthly_avg <-retailer_tomatoes   %>%
        group_by(month) %>%
        summarize(avg_price = mean(prices, na.rm = TRUE)) %>%
        mutate(source = "Retailer")
# same for statssa data
      statssa_monthly_avg <- statssa_tomatoes %>%
        group_by(month) %>%
        summarize(avg_value = mean(value, na.rm = TRUE))  %>%
        mutate(source = "Stats SA")

# combine the two adjusted datasets
combined_data<- retailer_monthly_avg %>%
        rename(avg_price = avg_price)  %>%
        bind_rows(statssa_monthly_avg   %>%
                      rename(avg_price = avg_value))

    # Plot
    g <-

    ggplot(combined_data, aes(x = month, y = avg_price, color = source)) +
        geom_line() +
        labs(title = "Average Monthly Tomato Prices",
             x = "Month",
             y = "Average Price",
             color = "Source") +
        theme_minimal()
    g
}