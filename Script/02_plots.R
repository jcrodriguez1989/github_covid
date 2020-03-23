library("dplyr")
library("ggplot2")
library("readr")

language <- "" # select if you want to query for a specific coding language
# language <- "R"
gh_data <- read_csv(paste0("Data/github_covid", language, ".csv"))

# I am not sure what 'code', 'commits' and 'topics' are returning by the
# GitHub API: https://developer.github.com/v3/search/
gh_data <- gh_data %>% filter(!event %in% c("code", "commits", "topics"))

# nicer for plot
colnames(gh_data) <- tools::toTitleCase(colnames(gh_data))
gh_data$Event <- tools::toTitleCase(gh_data$Event)
gh_data$Event <- sub("^Repositories$", "Repos", gh_data$Event)
gh_data$Event <- sub("^Pr$", "PR", gh_data$Event)

# let us mark which days are weekends (I know 04 and 05 Jan were)
gh_data$Weekend <- weekdays(gh_data$Date) %in%
  weekdays(c(as.Date("2020-01-04"), as.Date("2020-01-05")))

title_add <- ifelse(language == "", "", paste0(" for ", language))

(ggp <- ggplot(gh_data) +
  geom_point(aes(x = Date, y = Count, color = Event, shape = Weekend)) +
  geom_line(aes(x = Date, y = Count, color = Event)) +
  scale_x_date(breaks = "1 week") +
  scale_shape_manual(values = c(19, 1)) +
  facet_grid(rows = vars(Event), scales = "free_y") +
  ggtitle(paste0("Number of events per day on GitHub", title_add)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
)
ggsave(paste0("Output/github_covid", language, ".png"), ggp)

# split by week
gh_data <- gh_data %>%
  mutate(Week = strftime(Date, format = "%Y-W%V"))

(ggp <- ggplot(gh_data) +
  geom_bar(aes(x = Week, y = Count, fill = Event), stat = "sum") +
  facet_grid(rows = vars(Event), scales = "free_y") +
  ggtitle(paste0("Number of events per week on GitHub", title_add)) +
  theme(legend.position = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
)
ggsave(paste0("Output/github_covid_by_week", language, ".png"), ggp)
