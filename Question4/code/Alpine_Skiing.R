Alpine_Skiing <- function(data){
    Slalom_subset <- data %>%
            filter(Event == "Slalom" & Discipline == "Alpine Skiing" & Sport == "Skiing") #filter for Alpine Slalom skiing

    # calculate total medals and sort by total medals by athlete
    athlete_medals<- Slalom_subset %>%
        group_by(Athlete) %>%
        summarize(Total_Medals = n())  %>%
        arrange(desc(Total_Medals))

#Top 10 Athletes by total medals
    top_athletes <- head(athlete_medals, 10)

# bar plot
    g <-
        ggplot(top_athletes, aes(x = reorder(Athlete, -Total_Medals), y = Total_Medals, fill=Athlete)) +
        geom_bar(stat = "identity") +
        labs(title = "Top 10 Athletes in Slalom (Alpine Skiing) by Total Medals", x = "Athlete", y = "Total Medals") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
        guides(fill = FALSE)
    g
}
