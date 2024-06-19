Medals_India<-function(data=Summer_GDP_emerging){

    #create a vector for the different medal types
   Medals <- c("Gold", "Silver", "Bronze")

     #create a teamsport vector
     teamsports_vec <- c("2000M Tandem", "4X400M Relay", "600M Free Rifle, Team", "Coxless Pair (2-)", "Doubles",
                        "Team Pursuit (4000M)", "470 - Two Person Dinghy", "4X100M Freestyle Relay", "4X100M Relay",
                        "Handball", "Hockey", "Quadruple Sculls", "Quadruple Sculls Without Coxswain (4X)",
                        "Synchronized Diving 3M Springboard", "Team (Fita Olympic Round - 70M)", "Team Competition",
                        "Teams Fita Round", "Yngling - Keelboat")


   # Count total medals per Country and Year
  total_medals<- data %>%
       group_by(Country, Year, Event) %>%
       summarise(
           total_medals = n_distinct(ifelse(Medal %in% Medals, Medal, NA)),
       ) %>%
       ungroup() %>%
       filter(!(Event %in% teamsports_vec & total_medals > 1)) %>% # if event is a teamsport only 1 for that year country event
       group_by(Country, Year) %>%
       summarise(
           total_medals = sum(total_medals)) %>% # calc total medals by country and year
       ungroup()

   #Line plot of total medals over time, making INdia's line thicker
   g <-
   ggplot(total_medals, aes(x = Year, y = total_medals, color = Country, group = Country)) +
       geom_line(size = ifelse(total_medals$Country == "IND", 1.5, 0.8)) +  # Making the India line thicker/ more visible
       labs(x= "Year", y = "Total Medals", color = "Country",
       title="Total Medals by (Emerging) Country over Time") +
       theme_minimal()
   g
}

