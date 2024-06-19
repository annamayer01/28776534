plot_popularity_over_time <- function(data_coldplay, data_metallica) {

# calculate mean popularity by release year for Coldplay
coldplay_yearly <- data_coldplay %>%
 group_by(release_year)   %>%
    summarise(mean_popularity = mean(popularity, na.rm = TRUE)) %>%
    ungroup()
    # average  popularity by release year for Metallica
 metallica_yearly<- data_metallica %>%
     group_by(release_year) %>%
       summarise(mean_popularity = mean(popularity, na.rm = TRUE)) %>%
        ungroup()

# Plotting aeragee popularity of Metallic and Colplay  each over time (decided to do a line plot for comparison)
    g<-
    ggplot() +
       geom_line(data = coldplay_yearly, aes(x = release_year, y= mean_popularity, color = "Coldplay"), size = 1) +
       geom_line(data = metallica_yearly, aes(x = release_year, y = mean_popularity, color= "Metallica"), size = 1) +
   labs(title = "Average Popularity of Coldplay vs Metallica Over Time",
         subtitle = "Comparing Mean Popularity by Release Years of the Two Bands",
         x= "Year",
        y= "Average Popularity") +
        scale_color_manual(values = c("Coldplay" = "blue", "Metallica" = "red")) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_x_continuous(breaks = seq(min(c(coldplay_yearly$release_year, metallica_yearly$release_year), na.rm = TRUE),
                                 max(c(coldplay_yearly$release_year, metallica_yearly$release_year), na.rm = TRUE),
                                        by = 5),
                          labels = scales::number_format(accuracy = 1)) +
        guides(color = guide_legend(title = "Band"))
    g
}