calculate_and_plot_correlations <- function(data) {

# First step:
    # Group by Year Gender and Name to get the total number of each name by year and gender
national_popularity<- data %>%
        group_by(Year, Gender, Name) %>%
        summarise(Total_Count = sum(Count, na.rm = TRUE)) %>%
        ungroup()

# Second: Now I added a function to extract the top 25 baby names by gender by year
    # Additionally, for the spearmnan corr, ranks are necesseary, so assigned them/ created new columns with ranks.
  get_top_25_ranks <- function(data, year, gender) {
        data %>%
            filter(Year == year, Gender == gender) %>%
            arrange(desc(Total_Count)) %>%
            head(25) %>%
            mutate(Rank = row_number()) %>%
            select(Name, Rank)
    }

# Thirdly: Created an empty data frame to store the results of the calculations below in there.
correlations <- data.frame(Year = integer(), Gender = character(), Correlation = numeric())


 # Fourth step: Calculated spearman r. correlations for each year with next 3 years and safe corr coeff in dataset

for (year in 1910:2011) {  #only years form 1910 to 2011 cause +3 years, otherwise didn't work.
    for (gender in c("M", "F")) {
            current_year_ranks <- get_top_25_ranks(national_popularity, year, gender)
            next_year_ranks <- get_top_25_ranks(national_popularity, year + 1, gender) # looping over next year
            next_2_year_ranks <- get_top_25_ranks(national_popularity, year + 2, gender) #looping 2nd year
            next_3_year_ranks <- get_top_25_ranks(national_popularity, year + 3, gender) # 3rd year

            combined_ranks_1 <- merge(current_year_ranks, next_year_ranks, by = "Name", all = FALSE)
            combined_ranks_2 <- merge(current_year_ranks, next_2_year_ranks, by = "Name", all = FALSE)
            combined_ranks_3 <- merge(current_year_ranks, next_3_year_ranks, by = "Name", all = FALSE)

              if (nrow(combined_ranks_1) > 0) {
                corr_1 <- cor(combined_ranks_1$Rank.x, combined_ranks_1$Rank.y, method = "spearman")
                correlations <- rbind(correlations, data.frame(Year = year, Gender = gender, Correlation = corr_1))
            }
              if (nrow(combined_ranks_2) > 0) {
                corr_2 <- cor(combined_ranks_2$Rank.x, combined_ranks_2$Rank.y, method = "spearman")
                correlations <- rbind(correlations, data.frame(Year = year, Gender = gender, Correlation = corr_2))
            }
              if (nrow(combined_ranks_3) > 0) {
                corr_3 <- cor(combined_ranks_3$Rank.x, combined_ranks_3$Rank.y, method = "spearman")
                correlations <- rbind(correlations, data.frame(Year = year, Gender = gender, Correlation = corr_3))
            }
        }
}
# Now we got 3 correlation coefficients per year per gender (as we loop over following three years)
# -> For easier interpretation in the graph, I decided to take the average per gender per year to see how the trend is
mean_correlations <- correlations %>%
    group_by(Year, Gender) %>%
    summarise(Mean_Correlation = mean(Correlation, na.rm = TRUE)) %>%
    ungroup()




# Finally do a line plot with the new data frame and the correlations over time to see the trends::
    g<-
        ggplot(mean_correlations, aes(x =Year, y = Mean_Correlation, color = Gender)) +
        geom_line(size = 1.5) +
        labs(title = "Top 25 Baby Names Over Time",
             subtitle = "Using Spearman Rank Correlation",
             x= "Year",
             y = "3 Year Mean Spearman Rank Correlation") +
        theme_minimal() +
        scale_color_manual(values = c("M" = "blue", "F" = "pink"))
    g
}