scatter_plot_fct <- function(data, title) {

    #focusing on EU member states
    aid_data_eu<- data %>%
        filter(`EU member` == 1)

    # Plot the scatter plot of GDP vs Total bilateral allocations
    scatter_plot <-
    ggplot(aid_data_eu, aes(x = `GDP in 2021($ billion)`, y = `Total bilateral allocations($ billion)`)) +
        geom_point() +
        geom_smooth(method= "lm", formula = y ~ x, se = FALSE, color = "blue") + #add fitted line into scatter plot to see if there is a corr
        labs(title = title,
             x = "GDP in 2021 ($ billion)",
             y= "Total Bilateral Allocations ($ billion)") +
        theme_minimal()

    scatter_plot # return scatter plot
}