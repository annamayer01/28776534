compare_diff_GDP <- function(data, title) {

#only EU member states
aid_data_eu<- data  %>%
        filter(`EU member` == 1)

#difference between allocation and commitment for each country -> "commitment gap"
   country_comparison <- aid_data_eu  %>%
        mutate(Difference = `Total bilateral allocations($ billion)` - `Total bilateral commitments($ billion)`) %>%
        select(Country, Difference, `GDP in 2021($ billion)`)

#scatter plot of GDP vs Difference
    scatter_plot2 <-
        ggplot(country_comparison, aes(x = `GDP in 2021($ billion)`, y = Difference)) +
        geom_point() +
        geom_smooth(method= "lm", formula = y ~ x, se = FALSE, color = "blue") + # again fitted line to see potential corr
        labs(title = title,
             x = "GDP in 2021 ($ billion)",
             y = "Difference ($ billion)") +
        theme_minimal()

    scatter_plot2
}