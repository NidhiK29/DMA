library(stringr)

data <- read.csv("C:/Users/Nidhi/Downloads/Untitled spreadsheet - Form Responses 1.csv", stringsAsFactors = FALSE)

# Clean and shorten column names
names(data) <- names(data) |>
  str_trim() |>
  str_to_lower() |>
  str_replace_all("[^a-z0-9]+", "_") |>
  str_replace_all("_+$", "") |>
  str_replace_all("_of_|_do_|_is_|_for_|_the_|_you_|_use_|_most_|_often_|_visit_", "_") |>
  str_replace_all("_+", "_")

# Remove columns containing 'timestamp'
data <- data[ , !grepl("timestamp", names(data), ignore.case = TRUE)]

# Remove rows where year column has timestamp/date
col_name <- grep("year", names(data), ignore.case = TRUE, value = TRUE)
if (length(col_name) > 0) {
  pattern <- "\\d{1,2}[/-]\\d{1,2}[/-]\\d{2,4}(?:\\s+\\d{1,2}:\\d{2}(:\\d{2})?)?"
  data <- subset(data, !grepl(pattern, data[[col_name]]))
}

# Remove empty or mostly blank rows
data <- data[rowSums(is.na(data) | data == "") < ncol(data) * 0.8, ]


# Standardize and clean the 'which_library_you_most_often' column
data$which_library_you_most_often <- trimws(tolower(data$which_library_you_most_often))
data$which_library_you_most_often[data$which_library_you_most_often %in% c("cse", "cse library")] <- "digital"
data$which_library_you_most_often <- tools::toTitleCase(data$which_library_you_most_often)

# Save cleaned file
write.csv(data, "C:/Users/Nidhi/Downloads/cleaned_form_responses_simple.csv", row.names = FALSE)

cat("Cleaned and renamed file saved as 'cleaned_form_responses_simple.csv'\n")


