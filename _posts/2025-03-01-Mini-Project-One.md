---
title: "Fur-tunate or Not? The Truth Behind Black Cat Adoption"
author: "Kaycie McColley"
date: "`r Sys.Date()`"
output: html_document
---

```{r message=FALSE, warning=FALSE, include=FALSE}
library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(janitor)

```

```{r include=FALSE}
cat_shelter_data <- read_csv("~/animal_shelter_data/animal-shelter-intakes-and-outcomes.csv") |>
  clean_names() %>%
  filter(animal_type == "CAT") %>%
  filter(primary_color %in% c("BLACK", "ORANGE", "WHITE", "BROWN", "GRAY", "TORTIE"))

head(cat_shelter_data)
```

# Introduction

The purpose of this project is to see if black cats are less likely to be adopted compared to other color cats. Black cats have a history of not being adopted because they are tied to a superstition causing bad luck. This report will analyze data from the Long Beach Animal Shelter and data on their cats from the years 2017 to 2025. I have also filtered the data to include only the cats categorized with primary colors black, orange, white, brown, gray, and tortie, because the dataset includes many different color variations. This data contains 8,055 entries and there is one cat per entry. I retrieved this data from the TidyTuesday website (<https://github.com/rfordatascience/tidytuesday/blob/main/data/2025/2025-03-04/readme.md>). The drive behind this project was because often times people will not adopt certain pets based on their breed or color. A popular example are pitbulls since they have a bad reputation for being aggressive dogs. This calls into question the everlasting debate: nurture vs nature. I was also interested in pursing this project because one day I plan on adopting a cat from a shelter (who knows, maybe a black cat!).

## Plot 1: Number of Cats For Each Type/Color

```{r}
ggplot(data = cat_shelter_data, aes(x = fct_infreq(primary_color), fill = primary_color)) + 
  geom_bar(color = "black") +  
  scale_fill_manual(values = c("BLACK" = "black", 
                               "ORANGE" = "orange", 
                               "WHITE" = "white", 
                               "BROWN" = "brown", 
                               "GRAY" = "gray", 
                               "TORTIE" = "purple")) + 
  labs(
    title = "Frequency of Cat Colors",
    subtitle = "A breakdown of the cats in the Long Beach Shelter based on their fur color",
    x = "Cat Color",
    y = "Frequency",
    fill = "Cat Color")
```

The purpose of this chart is to visually represent the frequency of each cat color (from the six selected fur color categories) at the Long Beach Shelter from 2017 to 2025. The x-axis shows the different fur colors, while the y-axis measures the frequency of cats, indicating how many cats of each color have been recorded. The color of the bars have been changes to also represent the different fur colors of the cats.

From the bar chart, we can see that over 4,000 black cats were in the shelter during the nine-year span of the data. This number more than doubles the second largest population, gray cats, at around 1,750 cats. White cats total approximately 1,200. Orange and tortie cats are close in number, with around 500 orange cats and about 490 tortie cats. Lastly, brown cats make up the smallest group, with roughly 100 cats over the nine years.

This chart supports the purpose of this study, as the sheer number of black cats greatly exceeds that of any other fur color category.

## Plot 2: Number of Cats and Their Outcome Results

```{r}
cat_shelter_data |>
  filter(outcome_type %in% c("ADOPTION", "RETURN TO OWNER", "TRANSFER", 	
"EUTHANASIA", "DIED")) |>
  ggplot(aes(x = fct_infreq(primary_color), fill = outcome_type)) +  # fct_infreq orders colors by frequency
  geom_bar() + 
  labs(
    title = "Outcome Results by Cat Fur Color",
    subtitle = "Comparison of fur color and outcome results.",
    x = "Fur Color",
    y = "Frequency",
    fill = "Outcome Type"
  )
```

To preface this chart, I should mention that the original data included many different outcome types. For clarity, the data was filtered to include only five specific outcomes: adoptions, euthanasia, returns to owners, deaths, and transfers. This was necessary to simplify the analysis, as there were 18 different outcome types, making it difficult to draw clear conclusions from the chart.

Unsurprisingly, due to the high frequency of black cats in shelters, the number of occurrences for each outcome type is greater for black cats compared to other fur colors. The distribution of cats by fur color in this chart mirrors the distribution seen in Plot 1. For all fur colors, the outcome types are relatively proportional, meaning the distribution across different outcomes is consistent.

Notably, the death outcome (excluding euthanasia) and the return-to-owner outcome are relatively small for all cat colors. However, more concerning is that for each fur color, the adoption and euthanasia rates appear to be roughly similar. This suggests that black cats, as well as other colors, may face similar challenges in terms of adoption rates.

## Plot 3: Adoption Rates for Cats Based on Fur Color

```{r}
adoption_rates <- cat_shelter_data |>
  filter(outcome_type == "ADOPTION") |>  # Only consider adopted cats
  group_by(primary_color) |>
  summarize(
    total_cats = n(),  # Count total adopted cats by color
    total_adopted = sum(outcome_type == "ADOPTION")  # Count of adoptions, which will be same as total_cats in this case
  ) |>
  mutate(adoption_rate = total_adopted / sum(total_adopted))
 
  ggplot(adoption_rates, aes(x = reorder(primary_color, adoption_rate, FUN = desc), y = adoption_rate, fill = primary_color)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("BLACK" = "black", "ORANGE" = "orange", "WHITE" = "white", "BROWN" = "brown", "GRAY" = "gray", "TORTIE" = "purple")) +
  labs(
    title = "Adoption Rates by Cat Color",
    subtitle = "Proportion of adoptions by each fur color compared to the total adoptions of this group.",
    x = "Cat Color",
    y = "Adoption Rate"
  )
```

This plot shows the proportional adoption rates of cats based on their fur color. Black cats have the highest adoption rate, at approximately 50%, among the six categories. Gray cats follow with the second highest adoption rate, at around 22%. White cats have an adoption rate of about 13.5%. Orange cats have an adoption rate of around 8%, while tortie cats have an adoption rate of approximately 5%. Finally, brown cats have the lowest adoption rate, at around 2%.

This distribution of adoption rates closely mirrors the distribution of cats by fur color in the Long Beach Animal Shelter, as seen in Plot 1.

## Conclusions

In conclusion, the data shows that black cats have the highest adoption rate among black, gray, orange, white, tortie, and brown cats at the Long Beach Animal Shelter from 2017 to 2025. This finding is consistent with what we see in Plot 1, where the number of black cats in the shelter significantly outweighs that of other cats with different fur colors.

One limitation of this study is that the number of black cats is not proportional to the other fur colors. If the dataset included an equal number of cats for each color, we could make more accurate comparisons of adoption rates. Therefore, further research is needed to address this imbalance. However, this report provides a solid foundation for comparing adoption rates of black cats against cats of different fur colors in future studies.
