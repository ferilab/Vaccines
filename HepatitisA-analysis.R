##                     Data analysis project using R                 ##
## Analysis of the effect of vaccination on the trend of Hepatitis A in the US 

library(tidyverse)
library(RColorBrewer)

# First, the wranglesd data for Hepatitis A is loaded.

load("rdas/HepatitisA_dat.rda")

# Graphical comparison  of the trend  of HepatitisA in 6 states in various geographical zones

the_case <- "Hepatitis A"
HepatitisA_dat %>% filter(state %in% c("Texas", "Florida", "California", "New York",
                      "Washington", "North Dakota") &!is.na(disease_rate)) %>%
  ggplot(aes(year, disease_rate, col = state)) +
  geom_line() +
  ylab("Hepatitis A cases per 10,000 population") + 
  geom_vline(xintercept=2005, col = "blue") +
  ggsave('fig/sample-states-trend.png')

# Plotting the trend of Hepatitis A for all states in one plot

HepatitisA_dat %>% ggplot(aes(year, state, fill = disease_rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept=2000, col = "blue") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position="bottom",
        text = element_text(size = 8)) +
  ggtitle(the_case, ' cases per 10,000 population') +
  ylab("") + xlab("") +
  ggsave("fig/all-states-trend.png")

# Now let's see how the states disease trends compares to the US average, and if the US trend proves the
# effectiveness of national vaccination plan.

avg <- us_contagious_diseases %>%
  filter(disease==the_case) %>% group_by(year) %>% 
  summarize(us_rate = sum(count, na.rm = TRUE) /
              sum(population, na.rm = TRUE) * 10000)

HepatitisA_dat %>%
  filter(!is.na(disease_rate)) %>%
  ggplot() +
  geom_line(aes(year, disease_rate, group = state), color = "grey50",
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1) +
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
  ggtitle("Cases per 10,000 by state") +
  xlab("") + ylab("") +
  geom_text(data = data.frame(x = 1977, y = 2),
            mapping = aes(x, y, label="US average"),
            color="black") +
  geom_vline(xintercept=2009, col = "blue") +
  ggsave("fig/us-average.png")

