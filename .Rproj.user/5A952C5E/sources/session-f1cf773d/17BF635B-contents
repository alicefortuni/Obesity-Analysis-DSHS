# Import libraries
library(ggplot2) 
library(dplyr) 
library(sf)
library(rnaturalearth) 
library(rnaturalearthdata) 
library(lattice) 
library(stringr)
library(tidyr)

# Load dataset with header
data <- read.csv("data.csv", header = TRUE, sep = ",")

### Info about dataset ###

cat('Number of rows and columns: ', dim(data))

cat('First rows of dataset: \n')
head(data)

cat('Names of columns: \n')
colnames(data)

cat('Summary about columns: \n')
summary(data)

cat('Percentage of null values in each column: \n')
sapply(data, function(x) mean(is.na(x))) * 100 

cat('Unique values for each column: \n')
sapply(data, function(x) length(unique(x))) 

# Check the correctness of the Value field
n_inconsistent_rows <- nrow(data[(data$FactValueNumericLow > data$FactValueNumeric) | (data$FactValueNumericHigh < data$FactValueNumeric), ])
cat("Number of rows with inconsistent values: ", n_inconsistent_rows)


### Data preprocessing ###

# Delete irrelevant columns or columns with NA values
filtered_data <- data[, 
                      c(
                        "Period",
                        "FactValueNumeric", 
                        "ParentLocation", 
                        "Location", 
                        "Dim1",
                        "SpatialDimValueCode"
                      )]

# Rename columns
filtered_data <- filtered_data %>%
  rename(
    year = Period,
    value = FactValueNumeric,
    region = ParentLocation,
    country = Location,
    countryCode = SpatialDimValueCode,
    sex = Dim1
  )

head(filtered_data)

any(is.na(filtered_data)) #there's no more NA Values

cat('Unique values for each column: \n') #there's no more columns with a single value
sapply(filtered_data, function(x) length(unique(x))) 

summary(filtered_data)
attach(filtered_data)


### Exploratory Data Analysis ###

data_both<-filtered_data%>% filter(sex=="Both sexes")

# Value distribution for both sexes
ggplot(data_both, aes(x = value)) +
  geom_histogram( binwidth = 1, fill = "lightblue", color = "darkgray") +
  geom_density(aes(y = after_stat(count)), color = "red", linewidth = 0.4) +
  labs(title = "Distribution of obesity percentage with density curve",
       x = "Obesity percentage (%)",
       y = "Count") +
  theme_minimal()

cat('Summary about value distribution: \n') 
summary(data_both$value)



# Trend over year

table(year)
cat("Number of years: ", length(unique(year)))

##obesity mean for each year
data_avg_year  <- filtered_data %>% 
  group_by(year) %>%
  summarize(mean_value = mean(value, na.rm = TRUE))

ggplot(data_avg_year, aes(x = factor(year), y = mean_value, group = 1)) +
  geom_line(color = "seagreen", size = 0.5) +
  geom_point(color = "seagreen", size = 1) +
  labs(title = "Mean obesity percentage by Year",
       x = "Year",
       y = "Mean obesity percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_discrete(breaks = seq(min(data_avg_year$year), max(data_avg_year$year), by = 2)) 

ggplot(data_both%>%filter(year %% 2 == 0) , aes(x = factor(year), y = value)) +
  geom_boxplot( fill="seagreen3", position = position_dodge(0.8), width = 0.7) +
  labs(title = "Distribution of obesity percentage by Year (Even Years Only)",
       x = "Year",
       y = "Obesity percentage (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summary_stats <- data_both%>%
  group_by(year) %>%
  summarise(
    n = n(), 
    mean = mean(value, na.rm = TRUE),  
    sd = sd(value, na.rm = TRUE), 
    min = min(value, na.rm = TRUE),  
    max = max(value, na.rm = TRUE),  
    median = median(value, na.rm = TRUE), 
    IQR = IQR(value, na.rm = TRUE), 
    .groups = 'drop'
  )

cat('Summary about value from 1990 to 2022: \n')
print(summary_stats, n=33)


#2022 vs 1990
ggplot(data_both%>%filter(year%in% c(1990, 2022)), aes(x = value, fill = factor(year))) +
  geom_density(alpha = 0.6, size = 0.5) +
  facet_grid(year ~ .) + 
  labs(
    title = "Comparison of obesity distribution: 1990 vs 2022",
    x = "Obesity percentage (%)",
    y = "Density",
    fill = "Year",
    color = "Year"
  ) +
  scale_fill_manual(values = c("1990"="lightblue", "2022"="salmon2")) +
  theme_minimal()

summary_stats <- data_both %>%filter(year%in% c(1990, 2022))%>%
  group_by(year) %>%
  summarise(
    n = n(), 
    mean = mean(value, na.rm = TRUE),  
    sd = sd(value, na.rm = TRUE), 
    min = min(value, na.rm = TRUE),  
    max = max(value, na.rm = TRUE),  
    median = median(value, na.rm = TRUE), 
    IQR = IQR(value, na.rm = TRUE), 
    .groups = 'drop'
  )

cat('Summary about value in 1990 vs 2022: \n')
summary_stats


#Sex: Male and Female

table(sex)

#sex differences over years
data_avg_year_sex <- filtered_data %>% filter(sex == "Male" | sex == "Female") %>%
  group_by(year, sex, .groups = "drop") %>%
  summarize(mean_value = mean(value, na.rm = TRUE))

ggplot(data_avg_year_sex, aes(x = year, y = mean_value, color = sex)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(title = "Mean obesity percentage over years by Gender",
       x = "Year",
       y = "Mean obesity percentage (%)")  +
  scale_color_manual(values = c("Male" = "steelblue", "Female" = "indianred")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_continuous(breaks = seq(min(data_avg_year_sex$year), max(data_avg_year_sex$year), by = 2)) 

 
ggplot(filtered_data %>% filter((sex == "Male" | sex == "Female") & (year == 1990 | (year - 1990) %% 4 == 0)), aes(x = factor(year), y = value, fill = sex)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7) +
  labs(title = "Distribution of obesity percentage over years by Gender (Every 4 Years)",
       x = "Year",
       y = "Obesity percentage (%)",
       fill = "Sex") +
  scale_fill_manual(values = c("Male" = "steelblue2", "Female" = "pink1")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Male vs Female in 2022
data_2022<- filtered_data %>% filter(year==2022)

data_2022_male_female=data_2022%>% filter(sex == "Male" | sex == "Female")

ggplot(data_2022_male_female, aes(x = value, fill = sex)) +
  geom_density(alpha = 0.7) +
  facet_grid(sex ~ .) +
  labs(title = "Distribution of obesity percentage by Gender in 2022",
       x = "Obesity percentage (%)",
       y = "Density",
       fill = "Sex") +
  scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink")) +
  theme_minimal()

ggplot(data_2022_male_female, aes(x = sex, y = value, fill = sex)) +
  geom_violin(trim = FALSE) +
  labs(title = "Distribution of obesity percentage by Gender in 2022", 
       x = "Sex", 
       y = "Obesity Percentage (%)") +
  scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink")) +
  theme_minimal()


ggplot(data_2022_male_female, aes(x = value, y = sex, color = sex)) +
  geom_point(position = position_jitter(height = 0.3), alpha = 0.6) +
  labs(
    title = "Scatterplot of obesity percentage by Gender",
    y = "Sex", 
    x = "Obesity Percentage (%)") +
  scale_color_manual(values = c("Male" = "steelblue", "Female" = "indianred")) +
  theme_minimal()

summary_stats <- data_2022_male_female %>%
  group_by(sex) %>%
  summarise(
    n = n(), 
    mean = mean(value, na.rm = TRUE),  
    sd = sd(value, na.rm = TRUE), 
    min = min(value, na.rm = TRUE),  
    max = max(value, na.rm = TRUE),  
    median = median(value, na.rm = TRUE), 
    IQR = IQR(value, na.rm = TRUE), 
    .groups = 'drop'
  )

cat('Summary about value in 2022 for Male and Female: \n')
summary_stats


#Geografic AREA: Region and country

cat("Number of regions: ", length(unique(region))) 
table(region) 

#trend over years for differents regions

average_obesity_region<-filtered_data %>% 
  group_by(year, region, .groups = "drop") %>%
  summarize(mean_value = mean(value, na.rm = TRUE))

ggplot(average_obesity_region, aes(x = year, y = mean_value, color = region)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(title = " Mean obesity percentage over years by Region",
       x = "Year",
       y = "Mean obesity percentage (%)",
       color="Region"
  ) +
  theme_minimal() 

ggplot(average_obesity_region, aes(x = factor(year), y = region, fill = mean_value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red", na.value = "grey50") +
  labs(title = "Heatmap of obesity percentage by Region and Year",
       x = "Year",
       y = "Region",
       fill = "Obesity percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_discrete(breaks = seq(min(average_obesity_region$year), max(average_obesity_region$year), by = 4)) 


#annual average in each region with differentiation by gender
data_avg_year_region_sex  <- filtered_data %>% filter(sex == "Male" | sex == "Female") %>% 
  group_by(year, region, sex) %>%
  summarize(mean_value = mean(value, na.rm = TRUE))

ggplot(data_avg_year_region_sex, aes(x = year, y = mean_value, color = sex, group = sex)) +
  geom_line(size = 1) +
  facet_wrap(~ region, scales = "free_y") +
  labs(title = "Mean obesity percentage over years by Region and Sex",
       x = "Year",
       y = "Mean obesity percentage (%)",
       color = "Sex") +
  scale_color_manual(values = c("Male" = "steelblue", "Female" = "pink")) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplot(data_avg_year_region_sex, aes(x = year, y = region, fill = mean_value)) +
  geom_tile() +
  facet_wrap(~ sex) +
  scale_fill_gradient(low = "white", high = "red", na.value = "grey50") +
  labs(title = "Heatmap of obesity percentage by Region, Year and Sex",
       x = "Year",
       y = "Region",
       fill = "Obesity percentage (%)") +
  theme_minimal() 

#Region in 2022
data_2022_both<-data_2022%>% filter(sex  == "Both sexes")

ggplot(data_2022_both, aes(x = region, y = value, fill = region)) +
  geom_boxplot() +
  labs(title = "Distribution of obesity percentage by Region in 2022",
       x = "Region",
       y = "Obesity percentage (%)",
       fill = "Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

summary_stats <- data_2022_both %>%
  group_by(region) %>%
  summarise(
    n = n(), 
    mean = mean(value, na.rm = TRUE),  
    sd = sd(value, na.rm = TRUE), 
    min = min(value, na.rm = TRUE),  
    max = max(value, na.rm = TRUE),  
    median = median(value, na.rm = TRUE), 
    IQR = IQR(value, na.rm = TRUE), 
    .groups = 'drop'
  )

cat('Summary about value in 2022 for each Region: \n')
summary_stats

#Region by sex
ggplot(data_2022_male_female, aes(x = region, y = value, fill = sex)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7) +
  labs(title = "Distribution of obesity percentage by Region and Sex in 2022",
       x = "Region",
       y = "Obesity percentage (%)",
       fill = "Sex") +
  scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink")) +
  theme_minimal() +
  facet_grid( .~ region, scales = "free_x")+
  theme(
    strip.text.x = element_blank()) 


##country
table(country)
cat("Number of countries: ", length(unique(country))) 

n_country_per_region <- filtered_data %>%
  group_by(region) %>%
  summarise(n_country = n_distinct(country))%>%
  arrange(desc(n_country))

cat("Number of countries per Region: \n") 
n_country_per_region

regions <- unique(data_2022_both$region)


#plot for each region
for (region_name in regions) {
  region_data <- data_2022_both %>%
    filter(region == region_name) %>%
#    slice(seq(1, n(), by = 2)) %>%  # campiona ogni 2 paesi
    mutate(country = str_trunc(country, width = 10, side = "right", ellipsis = "")) 
  
  p <- ggplot(region_data, aes(x = reorder(country, value), y = value, fill = country)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = paste("Average obesity percentage by Country in", region_name, "(2022)"),
         x = "Country",
         y = "Obesity Percentage (%)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") 
  print(p)
}

#Percentage of overweight people in the world in 2022 by country
world <- ne_countries(scale = "medium", returnclass = "sf")
world_countries <- unique(world$iso_a3)
filtered_countries <- unique(data_2022_both$countryCode)

missing_in_world <- setdiff(filtered_countries, world_countries)

missing_names_df <- data_2022_both %>%
  filter(countryCode %in% missing_in_world) %>%
  select(countryCode, country)

missing_names_df

world_codes_df <- world %>%
  select(name, iso_a3) 

missing_names_with_codes <- missing_names_df %>%
  left_join(world_codes_df, by = c("country" = "name"))

missing_names_with_codes

world$iso_a3[world$name == "France"] <- "FRA"
world$iso_a3[world$name == "Norway"] <- "NOR"

world_countries <- unique(world$iso_a3)
filtered_countries <- unique(data_2022_both$countryCode)

missing_in_world <- setdiff(filtered_countries, world_countries)

world_data <- left_join(world, data_2022_both, by =  c( "iso_a3"="countryCode"))

ggplot(data = world_data) +
  geom_sf(aes(fill = value)) +
  scale_fill_gradientn(colors = c("#f1f1f1", "red", "red4"), na.value = "lightgrey", name = "Obesity percentage (%)") +
  theme_minimal() +
  labs(title = "Obesity percentage by Country in 2022") +
  theme(legend.position = "bottom")


### Statistical Tests ###

# -- Male vs Female in  2022 --#

data_2022_male_female%>%
  group_by(sex) %>%
  summarise(
    n = n(), 
    mean = mean(value),
    median = median(value),
    sd = sd(value)
  )

# Normality
qq_plots <- data_2022_male_female %>%
  ggplot(aes(sample = value)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ sex, scales = "free") +
  labs(title = paste("Q-Q Plot by Gender in 2022"),
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

qq_plots


shapiro_male <- shapiro.test(data_2022$value[data_2022$sex == "Male"])
shapiro_female <- shapiro.test(data_2022$value[data_2022$sex == "Female"])

cat("Shapiro-Wilk test results by gender in 2022: \n")
shapiro_male 
shapiro_female 


#Can't use t di Student -> Mann-Whitney test
mann_whitney_test <- wilcox.test(data_2022$value[data_2022$sex == "Male"], data_2022$value[data_2022$sex == "Female"], paired=FALSE)
cat("Mann-Whitney test results: \n")
mann_whitney_test #medians are not equal


# distribution
ggplot(data_2022_male_female, aes(x = sex, y = value, fill = sex)) +
  geom_boxplot() +
  labs(title = "Distribution of obesity percentage by Gender in 2022",
       x = "Sex",
       y = "Obesity percentage (%)") +
  theme_minimal()

# -- Regions in  2022 --#

#check normality and homoschedasticity to apply ANOVA
qq_plots <- data_2022_both %>%
  ggplot(aes(sample = value)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ region, scales = "free") +
  labs(title = paste("Q-Q Plot for Regions in 2022"),
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

qq_plots

data_2022_both$region <- as.factor(data_2022_both$region)

shapiro_test_results <- data_2022_both %>%
  group_by(region) %>%
  summarise(
    shapiro_p = shapiro.test(value)$p.value,
  )
cat("Shapiro-Wilk test results for regions in 2022: \n")
shapiro_test_results

#HOMOSCEDASTICITY
bartlett_test_result <- bartlett.test(value ~ region, data = data_2022_both)
cat("Bartlett's test result for homoscedasticity: \n")
bartlett_test_result 

'''
variance_by_region <- data_2022_both %>%
  group_by(region) %>%
  summarise(
    variance = var(value, na.rm = TRUE)
  )

variance_by_region
'''

#cannot apply ANOVA -> Kruskal Wallis test

kruskal_wallis_result <- kruskal.test(value ~ region, data = data_2022_both)
cat("Kruskal Wallis test result: \n")
kruskal_wallis_result

#Post-hoc analysis: which regions are different?

# Perform the Mann-Whitney test for each pair of regions
#bonferroni
pairwise_test_bonferroni <- pairwise.wilcox.test(data_2022_both$value, data_2022_both$region, p.adjust.method = "bonferroni")
print(pairwise_test_bonferroni)

#benjamin-hochberg 
pairwise_test_bh <- pairwise.wilcox.test(data_2022$value, data_2022$region, p.adjust.method = "BH")
print(pairwise_test_bh)


# -- Obesity over years--#

years<-2013:2022

data_both_avg <-data_both%>% filter(year %in% years)%>%
  group_by(region, year) %>%
  summarise(avg_value = mean(value), 
            .groups = 'drop')
print(data_both_avg, n=20)

ggplot(data_both_avg, aes(x = factor(year), y = avg_value)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Distribution of Mean Percentage of Obesity by Year",
       x = "Year",
       y = "Mean Percentage of Obesity (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

for (year in years) {
  cat("Summary for year:", year, "\n")
  print(summary(data_both_avg$avg_value[data_both_avg$year == year]))
  cat("\n")
}

data_both_avg$year <- as.factor(data_both_avg$year)
data_both_avg$region <- as.factor(data_both_avg$region)

wide_data <- data_both_avg %>%
  pivot_wider(names_from = year, values_from = avg_value)
wide_data

avg_matrix<- as.matrix(wide_data[,-1])  
rownames(avg_matrix) <- wide_data$region 

avg_matrix

apply(avg_matrix, MARGIN=2, FUN=mean) #la media negli anni cresce

#apply(avg_matrix, MARGIN=1, FUN=mean) # forte variabilità anche fra le regioni

mean(avg_matrix)

bwplot(avg_value ~ year | region,
       data = data_both_avg,
       scales = list(
         y = list(at = seq(0, 80, by = 10)),
         x = list(rot = 45)  
       ),
       xlab = "Year", 
       ylab = "Mean Obesity percentage (%)", 
       main = "Distribution of Average Percentage of Obesity by Year and Region"  
)

qq_plots <- data_both_avg %>%
  ggplot(aes(sample = avg_value)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ year, scales = "free") +
  labs(title = paste("Q-Q Plot for last 10 years"),
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()
qq_plots

shapiro_test_results <- data_both_avg %>%
  group_by(year) %>%
  summarise(
    shapiro_p = shapiro.test(avg_value)$p.value,
    .groups = 'drop'
  )

print("Shapiro-Wilk test results over the last 10 years:")
print(shapiro_test_results)

# HOMOSCEDASTICITY: Bartlett
bartlett_test_result <- bartlett.test(avg_value  ~  year, data=data_both_avg)
cat("Bartlett's test result for homoscedasticity: \n")
bartlett_test_result 


# SFERICITA': Greenhouse-Geisser
S <- var(avg_matrix) 
J <- dim(avg_matrix)[2]
numeratore <- J^2*mean(diag(S)-mean(S))^2
denominatore <- (J-1)*(sum(S^2)-2*J*sum(apply(S,1,mean)^2)+
                         +J^2*mean(S)^2)
epsilon <- numeratore/denominatore
epsilon

# can't use ANOVA -> Friedman
friedman_result <- friedman.test(avg_value ~ year | region, data = data_both_avg)
friedman_result


# ANOVA 
anova_result <- aov(avg_value ~ year + Error(region/year), data = data_both_avg)
summary(anova_result) 

#pairwise_test_bh <- pairwise.wilcox.test(data_both_avg$avg_value, data_both_avg$year, p.adjust.method = "BH", paired=T)
#print(pairwise_test_bh)
