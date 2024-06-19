extract_top_n_names<- function(data, top_n) {
    top_n_names <- data %>%
        group_by(Year, Gender) %>%
        top_n(top_n, wt = Count) %>%
        arrange(Year, Gender, desc(Count))

    return(top_n_names)
}
