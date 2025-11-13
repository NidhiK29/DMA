library(ggplot2)
library(dplyr)
library(scales)
library(ggalluvial)
library(treemapify)

data <- read.csv("C:/Users/Nidhi/Downloads/cleaned_form_responses_simple.csv", stringsAsFactors = FALSE)
colnames(data)

#CGPA distribution
ggplot(data, aes(x = what_your_current_cgpa_range, fill = what_your_current_cgpa_range)) +
  geom_bar() +
  labs(
    title = "Distribution of Students by CGPA Range",
    x = "CGPA Range",
    y = "Number of Students"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# CGPA range vs frequency of visit
ggplot(data, aes(x = how_do_visit_library, fill = what_your_current_cgpa_range)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "CGPA Range vs Frequency of Library Visits",
    x = "Visit Frequency",
    y = "Percentage",
    fill = "CGPA Range"
  ) +
  theme_minimal()


#satisfaction levels by library type

ggplot(data, aes(x = which_library_you_most_often, fill = x_overall_how_satisfied_are_with_library_facilities_and_resources)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Satisfaction Levels by Library Type",
    x = "Library Type",
    y = "Percentage",
    fill = "Satisfaction Level"
  ) +
  theme_minimal()


#Treemap : branch vs purpose of library visit
data_summary <- data %>%
  count(branch_study, what_your_primary_purpose_visiting_library)

ggplot(data_summary, aes(
  area = n,
  fill = what_your_primary_purpose_visiting_library,
  label = branch_study
)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", size = 12) +
  labs(
    title = "Treemap: Branch vs Purpose of Library Visit",
    fill = "Purpose"
  ) +
  theme_minimal()


#major difficulty
library(ggplot2)
library(dplyr)
library(scales)

data %>%
  count(what_the_major_difficulty_faced_by_in_library) %>%
  mutate(percentage = n / sum(n)) %>%
  ggplot(aes(
    x = reorder(what_the_major_difficulty_faced_by_in_library, percentage),
    y = percentage,
    fill = percentage
  )) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Percentage of Students Facing Each Type of Difficulty in the Library",
    x = "Type of Difficulty",
    y = "Percentage of Students"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )


#satisfaction vs purpose of visit
library(reshape2)
ggplot(data, aes(
  x = what_your_primary_purpose_visiting_library,
  y = x_overall_how_satisfied_are_with_library_facilities_and_resources,
  fill = after_stat(count)
)) +
  geom_bin2d() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Heatmap: Purpose of Visit vs Satisfaction",
    x = "Purpose of Visit",
    y = "Satisfaction Level",
    fill = "Student Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# pie chart
library(ggplot2)
library(dplyr)

pie_data <- data %>%
  count(when_you_mostly_the_library) %>%
  mutate(percentage = n / sum(n) * 100)

ggplot(pie_data, aes(x = "", y = percentage, fill = when_you_mostly_the_library)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  labs(
    title = "When Do Students Mostly Use the Library?",
    fill = "Time of Visit"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )



