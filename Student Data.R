library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)


course <- read_excel("C:/Users/HP/OneDrive/Documents/DATA 332/Course.xlsx")
registration <- read_excel("C:/Users/HP/OneDrive/Documents/DATA 332/Registration.xlsx")
student <- read_excel("C:/Users/HP/OneDrive/Documents/DATA 332/Student.xlsx")

joined_data <- student %>%
  left_join(registration, by = "Student ID") %>%
  left_join(course, by = "Instance ID")

joined_data <- joined_data %>%
  mutate(BirthYear = year(ymd(`Birth Date`)))

# Chart on the number of students per major (Title)
plot_students_per_major <- ggplot(joined_data, aes(x = Title)) +
  geom_bar(color = "pink", fill = "pink") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Number of Students per Major", x = "Major", y = "Count")
ggsave("students_per_major.png", plot_students_per_major, width = 10, height = 6, dpi = 300)

# Chart on the birth year of the student
plot_students_by_birth_year <- ggplot(joined_data, aes(x = as.factor(BirthYear))) +
  geom_bar(color= "black", fill= "purple") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Number of Students by Birth Year", x = "Birth Year", y = "Count in hundreds")
ggsave("students_by_birth_year.png", plot_students_by_birth_year, width = 10, height = 6, dpi = 300)

# Total cost per major, segmented by payment plan
cost_data <- joined_data %>%
  group_by(Title, `Payment Plan`) %>%
  summarise(TotalCost = sum(Cost, na.rm = TRUE)) %>%
  ungroup()

plot_total_cost_per_major <- ggplot(cost_data, aes(x = Title, y = TotalCost, fill = as.factor(`Payment Plan`))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Total Cost per Major by Payment Plan", x = "Major", y = "Total Cost")
ggsave("total_cost_per_major.png", plot_total_cost_per_major, width = 10, height = 6, dpi = 300)

# Total balance due by major, segmented by payment plan
balance_data <- joined_data %>%
  group_by(Title, `Payment Plan`) %>%
  summarise(TotalBalanceDue = sum(`Balance Due`, na.rm = TRUE)) %>%
  ungroup()


plot_total_balance_due_per_major <- ggplot(balance_data, aes(x = Title, y = TotalBalanceDue, fill = as.factor(`Payment Plan`))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Total Balance Due by Major by Payment Plan", x = "Major", y = "Total Balance Due")
ggsave("total_balance_due_per_major.png", plot_total_balance_due_per_major, width = 10, height = 6, dpi = 300)


