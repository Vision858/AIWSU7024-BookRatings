#Task 1
# First, install the necessary libraries
install.packages("dplyr")
install.packages("ggplot2")

# Load the libraries into your current R session
library(dplyr)
library(ggplot2)

# Load datasets
rating_pga <- read.csv("E:/OneDrive/Desktop/7024 Programming in data Science Report/RatingPGA.csv")
book <- data.frame(
  ISBN = c('034545104X', '2080674722', '0425115801', '0449006522', '0060517794',
           '0553561618', '055356451X', '0747558167', '0451166892', '3404122879'),
  Book.Title = paste("Book", LETTERS[1:10]),
  Book.Author = rep("Author A", 10),
  Year.Of.Publication = c(1999, 2001, 1995, 2003, 1990, 1987, 1989, 2001, 1986, 1994),
  Publisher = c("Publisher 1", "Publisher 2", "Publisher 1", "Publisher 3", "Publisher 2",
                "Publisher 3", "Publisher 3", "Publisher 1", "Publisher 2", "Publisher 1")
)

# Merge book ratings with the mock book metadata to get publisher info
merged_data <- merge(rating_pga, book, by = "ISBN", all.x = TRUE)

# Group the data by publisher to find the average rating and total number of ratings
publisher_summary <- merged_data %>%
  group_by(Publisher) %>%
  summarise(Average_Rating = mean(Book.Rating), Total_Ratings = n()) %>%
  arrange(desc(Total_Ratings)) %>%
  head(10)

# Visualise the top 10 publishers based on the number of ratings using a bar chart
ggplot(publisher_summary, aes(x = reorder(Publisher, -Average_Rating), y = Average_Rating)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Average Book Ratings by Top 10 Publishers",
       x = "Publisher", y = "Average Rating") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

#Task 2

# Load datasets
users <- read.csv("E:/OneDrive/Desktop/7024 Programming in data Science Report/users.csv")
rating <- read.csv("E:/OneDrive/Desktop/7024 Programming in data Science Report/RatingPGA.csv")

# View column names to confirm match
colnames(users)
colnames(rating)

# Rename the 'User-ID' column to 'User' so we can join data correctly
colnames(users)[colnames(users) == "User.ID"] <- "User"   # If named 'User.ID'
colnames(users)[colnames(users) == "User-ID"] <- "User"   # If named 'User-ID'

# Combine the ratings and user demographic data
merged <- merge(rating, users, by = "User")

# Filter out users with missing age values since we need age for analysis
merged <- merged[!is.na(merged$Age), ]

# Group users into meaningful age brackets for comparison
merged$AgeGroup <- cut(merged$Age,
                       breaks = c(0, 18, 30, 45, 60, 100),
                       labels = c("0-18", "19-30", "31-45", "46-60", "61+"),
                       right = TRUE)

# Create a boxplot to show how book ratings vary across different age groups
boxplot(Book.Rating ~ AgeGroup, data = merged,
        main = "Book Ratings by Age Group",
        xlab = "Age Group", ylab = "Book Rating",
        col = "skyblue")

#Task 3

# Load all three datasets: users, RatingPGA, and RatingPGB
users <- read.csv("E:/OneDrive/Desktop/7024 Programming in data Science Report/users.csv")
pga <- read.csv("E:/OneDrive/Desktop/7024 Programming in data Science Report/RatingPGA.csv")
pgb <- read.csv("E:/OneDrive/Desktop/7024 Programming in data Science Report/RatingPGB.csv")

# Make sure the user ID column is named consistently for merging
colnames(users)[colnames(users) == "User.ID"] <- "User"

# Combine rating datasets with user information
merged_pga <- merge(pga, users, by = "User")
merged_pgb <- merge(pgb, users, by = "User")

# Extract the country name from the location string (last part after commas)
get_country <- function(loc) {
  if (grepl(",", loc)) {
    parts <- unlist(strsplit(as.character(loc), ","))
    return(tolower(trimws(tail(parts, 1))))
  } else {
    return("unknown")
  }
}
merged_pga$Country <- sapply(merged_pga$Location, get_country)
merged_pgb$Country <- sapply(merged_pgb$Location, get_country)

# Standardize country names and remove locations that are not actual countries
invalids <- c("unknown", "tennessee", "arizona", "california", "new york", "nebr", "indiana")

# Fix misspellings
merged_pga$Country[merged_pga$Country == "phillipines"] <- "philippines"
merged_pgb$Country[merged_pgb$Country == "phillipines"] <- "philippines"

# Remove invalid entries
merged_pga <- merged_pga[!merged_pga$Country %in% invalids, ]
merged_pgb <- merged_pgb[!merged_pgb$Country %in% invalids, ]

# Only keep countries that have at least 10 ratings to ensure meaningful averages
country_counts_pga <- table(merged_pga$Country)
merged_pga <- merged_pga[merged_pga$Country %in% names(country_counts_pga[country_counts_pga >= 10]), ]

country_counts_pgb <- table(merged_pgb$Country)
merged_pgb <- merged_pgb[merged_pgb$Country %in% names(country_counts_pgb[country_counts_pgb >= 10]), ]

# Find the average book rating for each remaining country
pga_avg <- aggregate(Book.Rating ~ Country, data = merged_pga, FUN = mean)
pgb_avg <- aggregate(Book.Rating ~ Country, data = merged_pgb, FUN = mean)

# Pick the top 5 countries with the highest average book ratings
top_pga <- head(pga_avg[order(-pga_avg$Book.Rating), ], 5)
top_pgb <- head(pgb_avg[order(-pgb_avg$Book.Rating), ], 5)

# Plot side-by-side bar charts for the top 5 countries in each dataset
par(mfrow = c(1, 2))

barplot(top_pga$Book.Rating,
        names.arg = top_pga$Country,
        col = "coral",
        main = "Top 5 Countries by Avg Rating (PGA)",
        ylab = "Average Rating",
        ylim = c(0, 10))

barplot(top_pgb$Book.Rating,
        names.arg = top_pgb$Country,
        col = "darkolivegreen3",
        main = "Top 5 Countries by Avg Rating (PGB)",
        ylab = "Average Rating",
        ylim = c(0, 10))


#Task 4 

# Load user data and both rating datasets
users <- read.csv("E:/OneDrive/Desktop/7024 Programming in data Science Report/users.csv")
pga <- read.csv("E:/OneDrive/Desktop/7024 Programming in data Science Report/RatingPGA.csv")
pgb <- read.csv("E:/OneDrive/Desktop/7024 Programming in data Science Report/RatingPGB.csv")

# Simulated Book Metadata (You may replace with actual file if available)
book <- data.frame(
  ISBN = c('034545104X', '2080674722', '0425115801', '0449006522', '0060517794',
           '0553561618', '055356451X', '0747558167', '0451166892', '3404122879'),
  Book.Title = paste("Book", LETTERS[1:10]),
  Book.Author = rep("Author A", 10),
  Year.Of.Publication = c(2001, 2003, 2005, 1999, 2010, 2008, 1989, 2002, 1986, 2001),
  Publisher = rep("Publisher X", 10)
)

# Standardize the user ID column name for consistency
colnames(users)[colnames(users) == "User.ID"] <- "User"

# Merge the datasets to get book info, user age, and rating in one place
merged_pga <- merge(pga, users, by = "User")
merged_pga <- merge(merged_pga, book, by = "ISBN")

merged_pgb <- merge(pgb, users, by = "User")
merged_pgb <- merge(merged_pgb, book, by = "ISBN")

# Focus only on books published after the year 2000
pga_after_2000 <- merged_pga[merged_pga$Year.Of.Publication > 2000 & !is.na(merged_pga$Age), ]
pgb_after_2000 <- merged_pgb[merged_pgb$Year.Of.Publication > 2000 & !is.na(merged_pgb$Age), ]

# Convert user ages into age group labels like 0–18, 19–30, etc.
cut_age <- function(age) {
  if (age <= 18) return("0-18")
  else if (age <= 30) return("19-30")
  else if (age <= 45) return("31-45")
  else if (age <= 60) return("46-60")
  else return("61+")
}

pga_after_2000$AgeGroup <- sapply(pga_after_2000$Age, cut_age)
pgb_after_2000$AgeGroup <- sapply(pgb_after_2000$Age, cut_age)

# Calculate average ratings given by users in each age group
avg_pga <- aggregate(Book.Rating ~ AgeGroup, data = pga_after_2000, mean)
avg_pgb <- aggregate(Book.Rating ~ AgeGroup, data = pgb_after_2000, mean)

# Show side-by-side bar plots comparing age group ratings for PGA and PGB
par(mfrow = c(1, 2))

barplot(avg_pga$Book.Rating,
        names.arg = avg_pga$AgeGroup,
        col = "slateblue1",
        main = "Avg Rating by Age Group (PGA, Year > 2000)",
        ylab = "Average Rating",
        ylim = c(0, 10))

barplot(avg_pgb$Book.Rating,
        names.arg = avg_pgb$AgeGroup,
        col = "tomato",
        main = "Avg Rating by Age Group (PGB, Year > 2000)",
        ylab = "Average Rating",
        ylim = c(0, 10))


