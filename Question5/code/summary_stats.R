# summary stats retailer data
plot_summary_stats <- function(data1, data2){

    # create summary stats
retailer_summary <- data1 %>%
    mutate(ingredient = category) %>%
    group_by(ingredient) %>%
    summarize(mean_price = mean(prices, na.rm = TRUE),
              sd_price = sd(prices, na.rm = TRUE),
              min_price = min(prices, na.rm = TRUE),
              max_price = max(prices, na.rm = TRUE),
              n = n())

#summary stats stats SA data
statssa_summary <- data2 %>%
    group_by(ingredient) %>%
    summarize(mean_value = mean(value, na.rm = TRUE),
              sd_value = sd(value, na.rm = TRUE),
              min_value = min(value, na.rm = TRUE),
              max_value = max(value, na.rm = TRUE),
              n = n())

# Combine summaries into one data frame for plotting
summary_combined <- retailer_summary  %>%
    rename(mean = mean_price, sd = sd_price, min = min_price, max = max_price) %>%
    mutate(source = "Retailer")  %>%
    bind_rows(statssa_summary %>%
                  rename(mean = mean_value, sd = sd_value, min = min_value, max = max_value)  %>%
                  mutate(source = "Stats SA"))

# Plot mean prices/values
g <-
ggplot(summary_combined, aes(x = ingredient, y = mean, fill = source)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Mean Prices (ZAR) of Braaibroodjie Ingredients",
         y = "Mean Price/Value", x = "Ingredient") +
    theme_minimal()
g
}