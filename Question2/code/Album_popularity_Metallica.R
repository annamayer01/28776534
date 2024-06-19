create_boxplot_Metallica <- function(data, title) {
   # creating a boxplot similar to the one in the task description for album popularity for Metallica here
    g <-
  ggplot(data, aes(x = album, y = popularity, fill=album)) +
        geom_boxplot(color = "blue", alpha = 0.7) +
        theme_minimal() +
        labs(
            title = title,
                    x = "Album",
            y = "Popularity"
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        coord_flip() +
        guides(fill = FALSE) #no legend / turn off
  g
}