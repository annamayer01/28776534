plot_top10_mentions <- function(data, artist1 = "Coldplay", artist2 = "Metallica") {

    artist1_top10 <- data %>%
        filter(artist == artist1 & rank <= 10) #filter how often Coldplay was in top 10

    artist2_top10 <- data %>%
        filter(artist == artist2 & rank <= 10) #  filter how often Metallica was  in top 10

#Next: Count the total top 10 mentions for each artist
    artist1_top10_count <- nrow(artist1_top10)
    artist2_top10_count <- nrow(artist2_top10)

#artist with most top 10 mentions, using rank var as well
top_artist<- data %>%
        filter(rank <= 10)  %>%
        group_by(artist) %>%
        summarise(top10_count = n())   %>%
        arrange(desc(top10_count)) %>% #arrange so I can pick artist with most top 10 rankings
        slice(1)

#new data frame putting results together for the bar plot
df <- data.frame(
        artist = c(artist1, artist2, top_artist$artist),
        top10_count = c(artist1_top10_count, artist2_top10_count, top_artist$top10_count))

# plot the bar chart
    g <-
    ggplot(df, aes(x = artist, y= top10_count, fill =artist)) +
        geom_bar(stat= "identity") +
        geom_text(aes(label = top10_count), vjust = -0.5) + # Add the total count of mentions in top 10on top of bar
        labs(title= "Top 10 Billboard Chart Mentions",
             subtitle = paste("Comparing the number of times", artist1, "and", artist2,
                              "appeared in the Top 10, along with the top artist overall"),
             x= "Artist",
             y= "Number of Top 10 Mentions") +
        theme_minimal()

    g
}