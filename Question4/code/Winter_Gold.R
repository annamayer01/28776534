gold_medals_winter <- function(data) {

# Filter Gold medals
 winter_gold <- data  %>% filter(Medal == "Gold")

#Calculate Gold medals per Caputa
 winter_population<- winter_gold %>%
        group_by(Country) %>%
        summarize(Gold_Medals = n(),
                  Population = mean(Population, na.rm = TRUE)) %>%
        mutate(Gold_Per_Capita = Gold_Medals / Population) #new column calculating Gold_medals per capita

 #Gold_Per_Capita -> sorting data
 winter_population <- winter_population %>% arrange(desc(Gold_Per_Capita))

 # Top 5 countries selection
 top_5_countries <- head(winter_population, 5)

#bar plot for top 5 countries
  g <-
    ggplot(top_5_countries, aes(x = reorder(Country, -Gold_Per_Capita), y = Gold_Per_Capita)) +
        geom_bar(stat= "identity", fill = "gold") +
        labs(title = "Gold Medals (Winter) per Capita by Country (Top 5)", x = "Country", y = "Gold Medals per Capita") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

    g
}