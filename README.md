# hackbio-stage-2-
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Import the data
data_url <- "https://raw.githubusercontent.com/HackBio-Internship/2025_project_collection/main/Python/Dataset/mcgc_METADATA.txt"
df <- read.table(data_url, header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# Preview the dataset
print(head(df))

# Convert to long format
df_long <- df %>%
  pivot_longer(cols = -Strain, names_to = "Condition", values_to = "OD600") %>%
  mutate(
    Mutant = ifelse(grepl("MUT", Condition), "+", "-"), # Identify knock-in (+) vs. knock-out (-)
    Time = rep(seq(0, 10, length.out = nrow(df)), each = ncol(df) - 1) # Assuming time series data
  )

# Convert OD600 to numeric (if needed)
df_long$OD600 <- as.numeric(gsub("[A-Za-z]", "", df_long$OD600)) # Extract numeric values
df_long <- na.omit(df_long)  # Remove any missing values

# Function to determine the time to reach carrying capacity
find_carrying_capacity_time <- function(strain_data) {
  if (nrow(strain_data) == 0) return(NA)
  max_od600 <- max(strain_data$OD600, na.rm = TRUE)
  time_to_capacity <- strain_data$Time[which.max(strain_data$OD600)]
  return(time_to_capacity)
}

# Compute carrying capacity time for each strain
capacity_times <- df_long %>%
  group_by(Strain, Mutant) %>%
  summarise(Carrying_Capacity_Time = find_carrying_capacity_time(cur_data()), .groups = "drop")

# Ensure carrying capacity times are correctly merged
df_long <- left_join(df_long, capacity_times, by = c("Strain", "Mutant"))

# 1️⃣ **Plot Growth Curves**
ggplot(df_long, aes(x = Time, y = OD600, color = Mutant)) +
  geom_line(aes(group = interaction(Strain, Condition)), alpha = 0.8) +
  facet_wrap(~ Strain) +
  labs(title = "Growth Curves of OD600 vs Time",
       x = "Time (hours)",
       y = "OD600",
       color = "Mutation Type") +
  theme_minimal()

# 2️⃣ **Scatter Plot: Time to Reach Carrying Capacity**
ggplot(capacity_times, aes(x = factor(Mutant), y = Carrying_Capacity_Time, color = factor(Mutant))) +
  geom_jitter(width = 0.2, size = 3, alpha = 0.7) +
  labs(title = "Scatter Plot: Time to Reach Carrying Capacity",
       x = "Mutation Type (- = Knock Out, + = Knock In)",
       y = "Time to Carrying Capacity (hours)") +
  theme_minimal()

# 3️⃣ **Box Plot: Carrying Capacity Comparison**
ggplot(capacity_times, aes(x = factor(Mutant), y = Carrying_Capacity_Time, fill = factor(Mutant))) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Comparison of Carrying Capacity Time",
       x = "Mutation Type",
       y = "Time to Carrying Capacity (hours)") +
  theme_minimal()

# 4️⃣ **Statistical Test (t-test)**
knock_out_times <- capacity_times$Carrying_Capacity_Time[capacity_times$Mutant == "-"]
knock_in_times <- capacity_times$Carrying_Capacity_Time[capacity_times$Mutant == "+"]

if (length(knock_out_times) > 1 & length(knock_in_times) > 1) {
  t_test_result <- tryCatch({
    t.test(knock_out_times, knock_in_times)
  }, error = function(e) {
    print("Error in t-test: Check data consistency.")
  })
  print(t_test_result)
} else {
  print("Not enough data for t-test")
}

# **Observations (Included in Code)**
# - The **growth curve plot** shows OD600 over time for different strains.
# - The **scatter plot** visualizes how long each strain took to reach carrying capacity.
# - The **box plot** compares the distributions of knock-out (-) and knock-in (+) strains.
# - The **t-test** determines if there is a statistically significant difference.
