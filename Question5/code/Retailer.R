retailer_plot_fct <- function(data=retailer_filtered){

      # Rename 'category' to 'ingredient' in retailer data
        retailer_data <- data %>% rename(ingredient = category)

     # Define Braaibroodjie ingredients
        ingredients <- c("white bread", "cheddar", "margarine", "tomatoes", "onions", "salt", "chutney")


       # Again filter retailer data for Braaibroodjie ingredients since I only now changed the name to ingredient
        retailer_filtered <- retailer_data  %>%
            filter(str_to_lower(ingredient) %in% ingredients)   %>%
            mutate(ingredient = str_to_lower(ingredient))

#Calculate base price as the average price of the first month
        retailer_base_prices <- retailer_filtered %>%
            mutate(month = floor_date(price_date, "month")) %>%
            group_by(ingredient, month)   %>%
            summarize(avg_price = mean(prices, na.rm = TRUE)) %>%
            ungroup() %>%
            filter(month == min(month)) %>%
            select(ingredient, avg_price)  %>%
            rename(base_price = avg_price)

# calculating monthly averages and also add column for index, after ading base price back to the data
        retailer_monthly_avg <- retailer_filtered %>%
            mutate(month = floor_date(price_date, "month")) %>%
            group_by(ingredient, month) %>%
            summarize(avg_price = mean(prices, na.rm = TRUE)) %>%
            left_join(retailer_base_prices, by = "ingredient") %>% # base prices calculated back to data
            mutate(index = (avg_price / base_price) * 100)   %>%  # index calculation
            ungroup()

# calculating monthly average for index!
        retailer_braaibroodjie_index <- retailer_monthly_avg %>%
            group_by(month) %>%
            summarize(braaibroodjie_index = mean(index, na.rm = TRUE)) %>%
            mutate(source = "Retailer")

        # Plot the  index for retailer data
        g<-
        ggplot(retailer_braaibroodjie_index, aes(x = month, y = braaibroodjie_index, color = source)) +
            geom_line() +
            labs(title = "Braaibroodjie Index Over Time (Retailer)",
                 x = "Date",
                 y = "Index (Base: 100)",
                 color = "Source") +
            theme_minimal()
        g
}
