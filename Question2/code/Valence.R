plot_valence_over_time<- function(data) {
    # Filter data for Coldplay and Metallica
    coldplay_data <- data %>% filter(artist == "Coldplay")
    metallica_data<- data %>% filter(artist == "Metallica")

# average valence by year for Coldplay
    coldplay_yearly <- coldplay_data %>%
        group_by(year) %>%
        summarise(mean_valence = mean(valence, na.rm = TRUE)) %>%
        ungroup()

    #same for Metallica
    metallica_yearly <- metallica_data %>%
        group_by(year)   %>%
        summarise(mean_valence = mean(valence, na.rm = TRUE)) %>%
        ungroup()

    #Plot
    g <-
    ggplot() +
        geom_line(data= coldplay_yearly, aes(x = year, y = mean_valence, color = "Coldplay"), size = 1) +
        geom_line(data = metallica_yearly, aes(x = year, y = mean_valence, color = "Metallica"), size = 1) +
        labs(title = "Average Valence of Coldplay vs Metallica Over Time",
             x = "Year",
             y = "Average Valence") +
        scale_color_manual(values = c("Coldplay" = "blue", "Metallica" = "red")) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_x_continuous(breaks = seq(min(c(coldplay_yearly$year, metallica_yearly$year), na.rm = TRUE),
                                        max(c(coldplay_yearly$year, metallica_yearly$year), na.rm = TRUE),
                                        by = 5),
                           labels = scales::number_format(accuracy = 1)) +
        guides(color = guide_legend(title = "Band"))
    g
}