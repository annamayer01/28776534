create_boxplot_Coldplay<- function(data, title) {
# creating a boxplot similar to the one in the task description for album popularity for Coldplay here
     g <-
    ggplot(data, aes(x= album_name, y = popularity, fill=album_name)) +
        geom_boxplot(color = "blue", alpha = 0.7) +
        theme_minimal() +
        labs(
            title = title,
                    x = "Album",
            y = "Popularity"
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        coord_flip() +
        guides(fill = FALSE) # no legend
     g

}