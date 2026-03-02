## Introduction

Welcome to the "Data Wrangling" lesson, dear R users!

Today, we are diving into a dataset from the Economic Freedom of the World Report (2022) created by the Fraser Institute, a globally recognized benchmark that ranks countries based on how supportive their policies and institutions are toward economic freedom. The Fraser Institute, headquartered in Vancouver, Canada, is an independent research organization focused on analyzing how government actions affect citizens’ quality of life. Their work spans key areas like taxation, healthcare, education, economic freedom, and the environment.

Guided by the principle "if it matters, measure it," the Fraser Institute's goal is to inform public debate and shift the climate of opinion through data-driven insights.

All Fraser Institute research undergoes peer review and is overseen by an Editorial Advisory Board of leading scholars. You can access the dataset directly here: https://efotw.org/economic-freedom/dataset?geozone=world&min-year=2&max-year=0&filter=0&page=dataset 

This publicly available dataset is collected and updated annually, with the 2022 report offering a detailed view of 165 countries and territories. 

It assesses each on metrics such as government spending, taxation, judicial independence, trade openness, and monetary policy stability.
Each country’s economic profile is evaluated through 62 indicators, grouped into five core areas:

1. Size of Government
2. Legal System & Property Rights
3. Sound Money
4. Freedom to Trade Internationally
5. Regulation 

This information is aggregated in the economic freedom index, ranging from 0 to 10.

Today, we will focus on the 2022 report, which captures the state of economic freedom two years after the height of the COVID-19 pandemic. This dataset reflects how governments around the world recalibrated policies, spending, and freedoms in response to the global disruption.

# Why does this matter? 
Economic freedom is not just an abstract idea. It often correlates with economic and social indicators like income levels, entrepreneurial activity, and even longevity. 

# Why is this a useful tutorial for you? 
Learning to work with this dataset not only improves your R skills but also helps you ask meaningful questions about how our world has changed after COVID-19 from the global economy perspective.

## Loading packages and data

We first need to download and load relevant packages for data analysis. Packages contain all the essential functions that make it easier to manipulate, clean, analyze, and visualize data.

```{r, warning = FALSE, message = FALSE}
library(learnr)
library(tidyverse)
library(janitor)
library(dplyr)
library(ggplot2)
library(readr)

# Ensure exercise chunks are pre-evaluated when rendering so the HTML shows output
tutorial_options(exercise.eval = TRUE)

# Also set chunk defaults for clean HTML rendering
knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE,
  exercise.eval = TRUE
)
options(readr.show_col_types = FALSE)
options(vroom.show_progress = FALSE)
```

Since our dataset is in CSV format, we will use the read_csv() function to import it into our R notebook. We can also use the *glimpse()* or *View()* functions to examine the dataset. First, I convert safely to UTF-8, and read data from a temp file, then I use the read_csv() function.

```{r exercise-00, exercise=TRUE}
raw_lines <- readLines("tutorial_data_efi2022.csv", warn = FALSE)
raw_lines_utf8 <- iconv(raw_lines, from = "", to = "UTF-8", sub = "byte")

utf8_path <- tempfile(pattern = "tutorial_data_efi2022_utf8_", fileext = ".csv")
writeLines(raw_lines_utf8, utf8_path, useBytes = TRUE)

data <- read_csv(
  utf8_path,
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE,
  progress = FALSE
)

glimpse(data)
```

We are now ready to begin cleaning the dataset. 
We will get rid of all the columns that start with "data..." and "Area". This dataset was converted from XSLX format into CSV. Some redundant columns were left after reformatting.

## Data Cleaning
```{r exercise-01, exercise=TRUE}
data_cleaned <- data |>
  select(-any_of(grep("^(data|Area)", names(data), value = TRUE)))

head(data_cleaned)
```

We can also clean column names to snake_case for consistency.
```{r exercise-02, exercise=TRUE}
data_cleaned <- data_cleaned |> 
  clean_names()

head(data_cleaned)
```

## Data Wrangling
Let's try the first exercise: select the *countries*, *rank*, and *economic_freedom_summary_index* columns. 
We want to find the country with the highest economic freedom index in 2022.

```{r exercise-1, exercise=TRUE}
# Exercise 1:
data_ex1 <- data_cleaned |>
  select(countries, rank, economic_freedom_summary_index) |>
  arrange(desc(economic_freedom_summary_index))

data_ex1
```

Hong Kong SAR, China ranks first with a score of 8.58 on the Economic Freedom Index. This result is particularly interesting given that COVID-19 originated in Wuhan, China. It is possible that China and its neighboring countries experienced the initial shock of the pandemic earlier and therefore were able to adjust their economic policies sooner in response.

Now, let's try the second exercise: use the *mutate()* function to create a new column showing deviation from the median score for the economic freedom index. 

Since we have more than 50 columns, we will make the new column the sixth column in the dataset for easier access using *relocate()* function.
```{r exercise-2, exercise=TRUE}
# Exercise 2:
data_ex2 <- data_cleaned |>
  mutate(
    score_centered = economic_freedom_summary_index -
      median(economic_freedom_summary_index, na.rm = TRUE)
  ) |>
  arrange(score_centered) |>
  relocate(score_centered, .after = 5)

data_ex2
```

Notice that Venezuela RB, Zimbabwe, Sudan, the Syrian Arab Republic, Algeria, Myanmar, Argentina, the Islamic Republic of Iran, Libya, and the Republic of Yemen are the furthest from the median. This result may suggest that countries with unstable political climates tend to have lower economic freedom scores. The only notable exception in this group is Argentina. Its presence may be explained by the impact of severe inflation that reached 70% year-on-year in 2022 rather than political instability.

Next exercise will involve *group_by()* function: summarize min, max, and average economic freedom index by the variable *freedom to own foreign currency bank accounts*.
```{r exercise-3, exercise=TRUE}
# Exercise 3:
data_ex3 <- data_cleaned |>
  group_by(freedom_to_own_foreign_currency_bank_accounts) |>
  summarize(
    mean_score = mean(economic_freedom_summary_index, na.rm = TRUE),
    min_score  = min(economic_freedom_summary_index, na.rm = TRUE),
    max_score  = max(economic_freedom_summary_index, na.rm = TRUE),
    .groups = "drop"
  )

data_ex3
```

Variable *freedom to own foreign currency bank accounts* is designed to measure the ease with which other currencies can be used via domestic and foreign bank accounts. To receive the highest score (10 points), a country must maintain low and stable inflation and refrain from imposing regulations that restrict the use of alternative currencies. In contrast, a score of 0 points is assigned to countries experiencing high inflation alongside strict regulatory barriers to foreign currency banking. Countries facing moderate to high inflation and imposing some restrictions receive an intermediate score of 5 points.

The final results are expected: countries scoring 10 points tend to exhibit the highest minimum, maximum, and average values for the economic freedom index, while those with 0 points show the lowest values across the same metrics. We can conclude that countries scoring 10 points have stronger institutions, more open markets, and more stable policy environments, all of which contribute to greater economic freedom throughout the evaluated dimensions.

Now, let's take a look at the distribution of economic freedom scores. 
```{r exercise-0, exercise=TRUE}
# Plot:
ggplot(data_cleaned, aes(x = economic_freedom_summary_index)) +
  geom_histogram(binwidth = 0.3, fill = "purple", color = "white") +
  labs(
    title = "Distribution of Economic Freedom Scores (2022)",
    x = "Economic Freedom Index",
    y = "Number of Countries"
  ) +
  theme_minimal()
```
The histogram reveals a bimodal pattern, with peaks clustering around 6 and 7.5 points. The distribution appears negatively skewed, with a few notable outliers in the left tail — indicating countries with very low freedom scores. This graph suggests that while many countries cluster around moderate to high freedom levels, a smaller group remains substantially less free economically.

For the final exercise, identify the countries that are outliers with economic freedom scores below 5. Utilize the insights gained from Exercise 1, and ensure that your findings align with the results in Exercise 2.
```{r exercise-4, exercise=TRUE}
# Exercise 4:
data_ex4 <- data_cleaned |>
  filter(economic_freedom_summary_index < 5) |>
  select(countries, rank, economic_freedom_summary_index) |>
  arrange(desc(economic_freedom_summary_index))

data_ex4
```
## Conclusion

This is the end of the "Data Wrangling" lesson. Now you know how to clean, manipulate, and visualize economic data using $tidyverse$ tools. Through data wrangling techniques like filtering, transforming, and summarizing, we revealed a bimodal and negatively skewed distribution of economic freedom scores, with most countries clustering around moderate to high freedom levels, but a clear tail of lower-performing nations.

These findings carry important implications: while economic freedom is advancing in many parts of the world in the post-covid era, persistently low scores in some countries may reflect deep-rooted structural barriers to individual choice, income mobility, and institutional trust.

## Reference 

The Fraser Institute, Economic Freedom Report 2022.

Source: www.fraserinstitute.org/studies/economic-freedom-of-the-world-2022-annual-report

