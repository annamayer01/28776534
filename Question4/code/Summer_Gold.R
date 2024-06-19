gold_medals_summer <- function(data) {

#Now filter summer data for Gold medals
 summer_gold <- data %>% filter(Medal == "Gold")

 #Gold medals per Capita
 summer_population <- summer_gold  %>%
        group_by(Country) %>%
         summarize(Gold_Medals = n(),
                  Population = mean(Population, na.rm = TRUE))  %>%
       mutate(Gold_Per_Capita = Gold_Medals / Population)

# Sort  by Gold_Per_Capita
 summer_population <- summer_population %>% arrange(desc(Gold_Per_Capita))

 # Top 5 countries
 top_5_countries <- head(summer_population, 5)

#Bar plot Top 5 countries
    g <-
        ggplot(top_5_countries, aes(x = reorder(Country, -Gold_Per_Capita), y= Gold_Per_Capita)) +
        geom_bar(stat = "identity", fill = "gold") +
         labs(title = "Gold Medals (Summer) per Capita by Country (Top 5)", x = "Country", y = "Gold Medals per Capita") +
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
    g
}