plot_medals_winter <- function(data) {

teamsports_vec <- c("2000M Tandem", "4X400M Relay", "600M Free Rifle, Team", "Coxless Pair (2-)", "Doubles",
                        "Team Pursuit (4000M)", "470 - Two Person Dinghy", "4X100M Freestyle Relay", "4X100M Relay",
                        "Handball", "Hockey", "Quadruple Sculls", "Quadruple Sculls Without Coxswain (4X)",
                        "Synchronized Diving 3M Springboard", "Team (Fita Olympic Round - 70M)", "Team Competition",
                        "Teams Fita Round", "Yngling - Keelboat", "Ice Hockey", "Five-Man", "Team, Horizontal Bar",
                        "Handball", "200M Team Swimming") # many more, however, beyond the scope of filtering them out.


    # Count total medals per Country and Year # again using grepl for accounting for more team sport events
total_medals<- data %>%
        group_by(Country, Year, Event)  %>%
        summarise(
            total_medals = ifelse(Event %in% teamsports_vec | grepl("Team", Event), 1, n_distinct(Medal))
        ) %>%
        ungroup() %>%
        group_by(Country, Year) %>%
        summarise(
            total_medals = sum(total_medals)
        ) %>%
        ungroup() %>%
        arrange(desc(total_medals)) # again sort by total medals to get top 5 in next step

    #find the top 5 countries by total medals
top_countries <- total_medals %>%
        group_by(Country) %>%
        summarise(Total_Medals = sum(total_medals)) %>%
        ungroup() %>%
        arrange(desc(Total_Medals)) %>%
        slice_head(n = 5) %>%
        pull(Country)

    #  Filter top 5 countries
total_medals_top5 <- total_medals %>%
        filter(Country %in% top_countries)

#Plot total medals by country over time
g <-
    ggplot(total_medals_top5, aes(x = Year, y = total_medals, color = Country, group = Country)) +
        geom_line(size = 1) + # Adjust the line size for better visibility
        geom_line(data = total_medals_top5 ) +
        labs(title = "Winter Olympics Medals Over Time",
             x= "Year",
             y = "Total Medals",
             color = "Country") +
        theme_minimal() +
        theme(legend.position = "bottom")
g
}