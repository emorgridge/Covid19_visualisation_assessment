library(readxl)
library(lubridate)
library(dplyr)
library(viridisLite)
install.packages('zoo')
library(zoo)
covid_data <-read_csv('owid-covid-data.csv')
latest_covid <- read_csv('owid-covid-latest.csv')
view(latest_covid)
europe <- covid_data %>% filter(continent== 'Europe')
view(europe)

hmm <- read_csv('WHO-COVID-19-global-table-data.csv')
view(covid_data)
europe$mortaliy_rate <- europe$total_deaths/europe$total_cases
summary(europe$mortaliy_rate)
europe %>% filter(mortaliy_rate > 99)%>% view()
covid_data$date <- as.Date(covid_data$date)
covid_data$year <- year(covid_data$date)
covid_data$YearMonth <- format(covid_data$date, "%Y-%m")
covid_data$month <- month(covid_data$date)

data("economics")
view(economics)

monthly_data <- europe %>%
  group_by(year, month) %>%
  summarize(total_deaths_m = sum(total_deaths, na.rm = TRUE))

View(monthly_data)
sum(monthly_data$total_deaths_m)

europe_latest <- latest_covid%>% filter(continent== 'Europe')
view(europe_latest)

europe_latest$covid_mortality <- (europe_latest$total_deaths/ europe_latest$total_cases)*100

ggplot(europe, aes(x = date, y = new_deaths))
  labs(title = "Mortality Over the Years",
       x = "Year",
       y = "Total Deaths") +
  theme_minimal()

ggplot(data, aes(x = )) +
  geom_line(aes(y = mortality_rate, color = "Mortality Rate")) +
  geom_line(aes(y = gdp_change, color = "GDP Change")) +
  scale_y_continuous(name = "Mortality Rate", 
                     sec.axis = sec_axis(~ . / scale_factor, name="GDP Change")) +
  labs(title = "Mortality Rate and GDP Change Over Time")

#real work

continent_summary <- latest_covid %>%
  group_by(continent) %>%
  summarize(Total_Cases = sum(total_cases_per_million, na.rm = TRUE),
            Total_Deaths = sum(total_deaths_per_million, na.rm = TRUE),
            Total_Population = sum(population, na.rm = TRUE))

continent_summary <- continent_summary[!is.na(continent_summary$continent) , ]
view(continent_summary)

continent_summary<- continent_summary %>%
  mutate(Cases_Per_Million = Total_Cases / Total_Population * 1e6,
         Deaths_Per_Million = Total_Deaths / Total_Population * 1e6)

cs_long <- pivot_longer(continent_summary, 
                          cols = c("Cases_Per_Million", "Deaths_Per_Million"), 
                          names_to = "Measure", 
                          values_to = "Rate")
View(cs_long)

ggplot(cs_long, aes(fill=Measure, y=Rate, x=continent)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_y_log10()+
  scale_fill_viridis(discrete = TRUE, begin = 0.5, end = 1) +
  labs(title = "COVID-19 Impact per 1 Million People by Continent (Log Scale)",
       x = "\nContinent",
       y = "Log of Rate per 1 Million",
       fill = "Measure",
       caption= 'Source: Our World in Data') +
  theme_minimal()+
  theme(
      plot.title = element_text(size = 15, face = 'bold'),
      plot.caption = element_text(size= 10),
      axis.text.y = element_text(size = 10, face = 'bold'),
      axis.text.x = element_text(size = 10, face = 'bold'),
      axis.title.y = element_text(size = 10, face = 'bold'),
      axis.title.x = element_text(size = 12, face = 'bold'),
      panel.background = element_rect(fill = "white", colour = "black"))
   
ggplot(cs_long, aes(fill=Measure, y=Rate, x=continent)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_y_log10()+
  scale_fill_viridis(discrete = TRUE, begin = 0.5, end = 1) +
  labs(title = "COVID-19 Impact per 1 Million People by Continent (Log Scale)",
       x = "\nContinent",
       y = "Log of Rate per 1 Million",
       fill = "Measure",
       caption= 'Source: Our World in Data') +
  theme(
    plot.title = element_text(size = 15, face = 'bold'),
    plot.caption = element_text(size= 10),
    axis.text.y = element_text(size = 10, face = 'bold'),
    axis.text.x = element_text(size = 10, face = 'bold'),
    axis.title.y = element_text(size = 10, face = 'bold'),
    axis.title.x = element_text(size = 12, face = 'bold'),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    panel.background = element_blank())

extra_y_breaks <- c(10, 100, 1000, 10000, 100000)  # Adjust these values as needed

# Your plot code with the adjustment for extra y-axis ticks
covidbar_log<- ggplot(cs_long, aes(fill = Measure, y = Rate, x = continent)) + 
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_log10(breaks = extra_y_breaks, labels = trans_format("log10", math_format(10^.x))) +
  scale_fill_viridis(discrete = TRUE, begin = 0.5, end = 1) +
  labs(title = "COVID-19 Impact per 1 Million People by Continent (Log Scale)",
       x = "\nContinent",
       y = "Log of Rate per 1 Million",
       fill = "Measure",
       caption = 'Source: Our World in Data') +
  theme(
    plot.title = element_text(size = 13, face = 'bold'),
    plot.caption = element_text(size = 10),
    axis.text.y = element_text(size = 10, face = 'bold'),
    axis.text.x = element_text(size = 10, face = 'bold'),
    axis.title.y = element_text(size = 10, face = 'bold'),
    axis.title.x = element_text(size = 12, face = 'bold'),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    panel.background = element_blank(),
    axis.ticks.length.y = unit(0.2, "cm")  # Adjust the length of the ticks if needed
  )
ggsave("Covid19 Impact bar chart single Log.png", covidbar_log, width = 10, height = 8, dpi= 400) #save the plot


#scatter plot of smokers proportion vs mortality percentage
scatter_plot_s <- ggplot(europe_latest2, aes(smokers_percentage, covid_mortality))+
  geom_point(aes(colour = income, size = 2), alpha = 0.8) +
  guides(size = FALSE)+
  scale_colour_viridis_d()+
geom_smooth(method = 'loess',size= 0.1,colour= 'black', se= F)+
  labs(title= 'Proportion of Smokers Vs Covid Case Factor Risk\n
       ',
       x= 'Proportion of Smokers', y= 'Covid Case Factor Risk',
       subtitle= 'Europe',
       colour= 'Income Status', caption = 'Source: Our World in Data, World Bank Indicators')+
  theme(
    plot.title = element_text(size = 13, face = 'bold'),
    plot.caption = element_text(size = 8),
    axis.line.x = element_line(color = "black", linewidth = 0.3),
    axis.line.y = element_line(color = "black", linewidth = 0.3),
    axis.line.x.top = element_blank(),
    axis.line.y.right = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"))

ggsave("Association plot_s.png", scatter_plot_s, width = 10, height = 8, dpi= 500)

#cardiovascular deathrate
scatter_plot_c <- ggplot(europe_latest2, aes(cardiovasc_death_rate, covid_mortality))+
  geom_point(aes(colour = income, size = 2), alpha = 0.8) +
  guides(size = FALSE)+
  scale_colour_viridis_d()+
  geom_smooth(method = 'loess',size= 0.1,colour= 'black', se= F)+
  labs(title= 'Cardiovascular Death rate Vs Covid Case Factor Risk\n
       ',
       x= 'Cardiovascular Death rate', y= 'Covid Case Factor Risk',
       subtitle= 'Europe',
       colour= 'Income Status', caption = 'Source: Our World in Data, World Bank Indicators')+
  theme(
    plot.title = element_text(size = 13, face = 'bold'),
    plot.caption = element_text(size = 8),
    axis.line.x = element_line(color = "black", linewidth = 0.3),
    axis.line.y = element_line(color = "black", linewidth = 0.3),
    axis.line.x.top = element_blank(),
    axis.line.y.right = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"))

ggsave("Association plot_c.png", scatter_plot_c, width = 10, height = 8, dpi= 500)

#diabetes prevalence
scatter_plot_d<- ggplot(europe_latest2, aes(diabetes_prevalence, covid_mortality))+
  geom_point(aes(colour = income, size = 2), alpha = 0.8) +
  guides(size = FALSE)+
  scale_colour_viridis_d()+
  geom_smooth(method = 'loess',size= 0.1,colour= 'black', se= F)+
  labs(title= 'Diabetes prevalence score Vs Covid Case Factor Risk\n
       ',
       x= 'Diabetes prevalence score', y= 'Covid Case Factor Risk',
       subtitle= 'Europe',
       colour= 'Income Status', caption = 'Source: Our World in Data, World Bank Indicators') +
  theme(
    plot.title = element_text(size = 13, face = 'bold'),
    plot.caption = element_text(size = 8),
    axis.line.x = element_line(color = "black", linewidth = 0.3),
    axis.line.y = element_line(color = "black", linewidth = 0.3),
    axis.line.x.top = element_blank(),
    axis.line.y.right = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"))

ggsave("Association plot_d.png", scatter_plot_d,width = 10, height = 8, dpi= 500)

europe_latest$female_smokers_num <- (europe_latest$female_smokers/100)* europe_latest$population
fs
europe_latest$male_smokers_num <- (europe_latest$male_smokers/100)* europe_latest$population
europe_latest$total_smokers_num <-europe_latest$female_smokers_num+ europe_latest$male_smokers_num
europe_latest$smokers_percentage <- (europe_latest$total_smokers_num/europe_latest$population)*100
view(europe_latest)
europe_latest2 <- europe_latest %>%
  filter(!is.na(income))
view(covid_data2)
view(education)
education <- education %>%
  rename(location = country)

e_selected <- education  %>%
  arrange(iso_code, desc(year)) %>%
  group_by(iso_code) %>%
  slice(1) %>%
  ungroup()
e_selected <- e_selected%>% select(iso_code, income)
europe_latest<- europe_latest %>%
  left_join(e_selected, by = "iso_code")

#mapping covid cases onto europe
europe.countries <- c("Albania", "Andorra", "Austria", "Belarus", "Belgium", 
                  "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", 
                  "Czechia", "Denmark", "Estonia", "Faeroe Islands", "Finland", 
                  "France", "Germany", "Gibraltar", "Greece", "Guernsey", 
                  "Hungary", "Iceland", "Ireland", "Isle of Man", "Italy", 
                  "Jersey", "Kosovo", "Latvia", "Liechtenstein", "Lithuania", 
                  "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro", 
                  "Netherlands", "North Macedonia", "Norway", "Poland", 
                  "Portugal", "Romania", "Russia", "San Marino", "Serbia", 
                  "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", 
                  "Ukraine", "United Kingdom", "Vatican")

eu.countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", 
                                  "Czech Republic", "Denmark", "Estonia", "Finland", "France", 
                                  "Germany", "Greece", "Hungary", "Ireland", "Italy", 
                                  "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", 
                                  "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", 
                                  "Spain", "Sweden", "United Kingdom")

europe_latest$location <- case_when(
  europe_latest$location == "Czechia" ~ "Czech Republic",
  TRUE ~ europe_latest$location
)
eu.map <- map_data("world", region = europe.countries)
ggplot(eu.map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgrey", colour="black")+
  labs(title = "EU map", caption="maps package, R")+
  coord_map()

view(eu.map)

covid_eu_map <- left_join(eu.map,europe_latest,by = c("region"="location"))
view(covid_eu_map)
library(scales)

covid_eu_plot <- ggplot(covid_eu_map, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = total_cases_per_million)) +
  scale_fill_viridis_c(labels = label_number(big.mark = ","), direction= -1) +  # Formats numbers with commas
  theme_void()+
  labs(fill="Total Cases per\n1 million people",
       title="Map of Europe coloured by Total Covid-19 Cases",
       caption="Data source: Our World in Data")+
  theme(plot.title = element_text(size = 13, face = 'bold'),
        plot.caption = element_text(size = 8))

ggsave("Covid Eu Map.png", covid_eu_plot, width = 10, height = 8, dpi= 400)


#timeseries data
continent_cases <- covid_data %>%
  group_by(date, continent) %>%
  summarize(new_cases_per_million_continent = sum(new_cases_per_million, na.rm = TRUE))

ggplot(continent_cases, aes(x = date, y = new_cases_per_million_continent, color = continent)) +
  geom_line() +
  labs(title = "Daily Confirmed COVID-19 Cases per Million People per Continent",
       x = "Date",
       y = "Daily Cases per Million") +
  theme_minimal()
theme(legend.title = element_blank())

#or
monthly_data <- covid_data %>%
  group_by(continent, YearMonth) %>%
  summarize(monthly_cases_per_million = sum(new_cases_per_million, na.rm = TRUE), .groups = 'drop')

ggplot(monthly_data, aes(x = YearMonth, y = monthly_cases_per_million, group = continent, color = continent)) +
  geom_line()+
  labs(title = "Monthly New Confirmed COVID-19 Cases per Million People by Continent",
       x = "Year-Month",
       y = "Monthly Cases per Million") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

monthly_data <- monthly_data %>%
  group_by(continent) %>%
  mutate(smoothed_cases_per_million = rollmean(monthly_cases_per_million, 7, fill = NA, align = 'right')) %>%
  ungroup()

covid_data2 <- covid_data
covid_data2 <- covid_data2 %>%
  filter(!is.na(total_cases), !is.na(total_cases_per_million))
view(covid_data2)

covid_data2 <- covid_data2 %>%
  mutate(month_name = month.name[month])

monthly_cases_avg <- covid_data2 %>%
  group_by(continent, YearMonth) %>%
  summarize(monthly_avg_cases = mean(new_cases_per_million, na.rm = TRUE), .groups = 'drop')

View(monthly_cases_avg)
monthly_cases_avg <- monthly_cases_avg %>%
  filter(!is.na(continent))
monthly_cases_avg$YearMonth <- as.Date(paste0(monthly_cases_avg$YearMonth, "-01"))

y_breaks <- seq(from = min(monthly_cases_avg$monthly_avg_cases, na.rm = TRUE), 
                to = 16000, 
                by = 3000)
time_series_plt <- ggplot(monthly_cases_avg, aes(x = YearMonth, y = monthly_avg_cases, color = continent, group = continent)) +
  geom_line(size= 0.7) +
  scale_color_viridis_d()+
  scale_x_date(date_breaks = "5 months", date_labels = "%b %Y") +
  scale_y_continuous(breaks = y_breaks, labels = scales::label_number())+
  labs(title = "Monthly Average of New COVID-19 Cases per Million by Continent\n
       ",
       x = "\n
       Month-Year",y = "Monthly Average Cases per Million",
       colour= 'Continent', caption= 'Source: Our World in Data') +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90,size= 10, face= 'bold', hjust = 1),
        plot.title = element_text(size = 15, face = 'bold'),
        plot.caption = element_text(size= 10),
        axis.text.y = element_text(size = 8, face = 'italic'),
        axis.title.y = element_text(size = 11, face = 'bold'),
        axis.title.x = element_text(size = 11, face = 'bold'))

ggsave("Time Series.png", time_series_plt, width = 10, height = 8, dpi= 500)

library(patchwork)
combined_plot1 <- covidbar_log + time_series_plt 

combined_plot2 <- scatter_plot + covid_eu_map
  
combined_plot <- combined_plot +
  plot_annotation(
    title = "Composite Visualisation Depicting the Impact of COVID-19"
  ) &
  theme(
    plot.title = element_text(size = 20, face = "bold")
  )

# Display the plot
combined_plot2

# Save the plot to a file
ggsave("combined_plot2.png", combined_plot2, width = 10, height = 8, dpi = 300)

#EDA
df_means <- colMeans(latest_covid[, c("total_cases", "total_deaths", "total_cases_per_million", "total_deaths_per_million", "total_vaccinations")], na.rm = TRUE)

print(df_means)

cor.test(europe_latest$cardiovasc_death_rate, europe_latest$covid_mortality)
cor.test(europe_latest$covid_mortality, europe_latest$smokers_percentage)
cor.test(europe_latest$covid_mortality, europe_latest$diabetes_prevalence)
cor.test

view(covid)

library(patchwork)

composite_plot <- time_series_plt + covidbar_log+ covid_eu_plot+ scatter_plot_c+ scatter_plot_d + scatter_plot_s+
  plot_layout(ncol = 2, nrow = 3, byrow = TRUE) +
  plot_layout(guides = 'keep') & 
  theme(plot.margin = unit(rep(0, 4), "lines"))

library(gridExtra)
# Display the composite plot
composite_plot
grid_plt1 <- grid.arrange(time_series_plt, covidbar_log, covid_eu_plot,  scatter_plot_c, nrow = 2, ncol = 2)
grid_plt2 <- grid.arrange(scatter_plot_d, scatter_plot_s, nrow = 1, ncol = 2)
grid_plt3 <- grid.arrange(time_series_plt, covidbar_log, covid_eu_plot,  scatter_plot_c, scatter_plot_d, scatter_plot_s, nrow = 3, ncol = 2)
grid_plt4 <- grid.arrange(time_series_plt, covidbar_log, nrow = 1, ncol = 2)
grid_plt5 <- grid.arrange(covid_eu_plot,  scatter_plot_c, nrow = 1, ncol = 2)

ggsave("combined_plot1.png", grid_plt1, width = 10, height = 8, dpi = 300)
ggsave("combined_plot2.png", grid_plt2, width = 10, height = 8, dpi = 300)
ggsave("combined_plot3.png", grid_plt3, width = 10, height = 8, dpi = 300)
ggsave("combined_plot4.png", grid_plt4, width = 10, height = 8, dpi = 300)
ggsave("combined_plot5.png", grid_plt5, width = 10, height = 8, dpi = 300)