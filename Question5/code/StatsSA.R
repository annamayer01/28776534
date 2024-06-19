statssa_plot_fct <- function(data=statssa_filtered){

# base price as mean price of first month
    statssa_base_prices <- data %>%
        mutate(month = floor_date(date, "month")) %>%
        group_by(ingredient, month) %>%
        summarize(avg_value = mean(value, na.rm = TRUE)) %>%
        ungroup() %>%
        filter(month == min(month)) %>%
        select(ingredient, avg_value) %>%
        rename(base_value = avg_value)

    # calculcating monthly averages + base price back to data
    statssa_monthly_avg <- statssa_filtered %>%
        mutate(month = floor_date(date, "month")) %>%
        group_by(ingredient, month) %>%
        summarize(avg_value = mean(value, na.rm = TRUE)) %>%
        left_join(statssa_base_prices, by = "ingredient")  %>%
        mutate(index = (avg_value / base_value) * 100) %>% # index calculation
        ungroup()

# average index per month for stats SA data
    statssa_braaibroodjie_index <- statssa_monthly_avg %>%
        group_by(month) %>%
        summarize(braaibroodjie_index = mean(index, na.rm = TRUE))  %>%
        mutate(source = "Stats SA")

    # Plot average index over time
    g <-
    ggplot(statssa_braaibroodjie_index, aes(x = month, y = braaibroodjie_index, color = source))  +
        geom_line() +
        labs(title = "Braaibroodjie Index Over Time (Stats SA data)" ,
             x = "Date",
             y = "Index (Base= 100)" ,
             color = "Source") +
        theme_minimal()
    g
}
