---
title: "McColley DS Final"
author: "Kaycie McColley"
date: "`r Sys.Date()`"
output: html_document
---

# Does Your Height Make You A Better Swimmer?

## Introduction

I spent most of my life outside of school in a pool growing up. I was a highly competitive USA swimmer where I specialized in long distance swimming. I was always an average height for female swimmers in my age group and I remember always hearing that no matter how hard you train, the taller you are, the faster you will naturally be in the water.

This always struck me as unfair but I couldn't deny that I wasn't the fastest sprinter in the pool. It was always the taller swimmers.

As frustrating as this is, many Olympic swimmers are tall, so does this have some data to back up the claim? For this project, I want to use data to discover the true relationship between height and swimming speeds.

So this leads me to ask: Are taller swimmers truly faster swimmers?

## Discussion of the data

I did some digging into data sets and I found one on GitHub created by Irene De La Torre titled "swimming records".

This data set includes the variables:

Event - The official name of the swimming event (Categorical) Distance - The distance of the event measured in meters (Categorical) Sex - Groups swimmers by genders "Male" and "Female" (Categorical) Ranking - Swimmer rank based on time swam (Categorical) Time - The time in seconds the swimmer completed the event in (Quantitative) Milliseconds - The time in milliseconds the swimmer completed the event in (Quantitative) Relay - Was the race a relay (Categorical) When - When was the event swam (heat, final, or time trial) (Categorical) Name - First and Last name of the swimmer (Categorical) Nationality - Where was the swimmer from (Categorical) Date - Day of the week/ MM/DD/YYYY (Categorical)\
Meet - The official name of the competition (Categorical) Location - Where was the competition held (Categorical)

This data set however does not include swimmer heights which is a problem I will address later on.

While this data set provides wonderful insight into these swimmers, I needed to wrangle it a bit to be able to get after the question I want the answer to.

## Data Wrangling

In order the answer my question, I first needed to select the variables I am most interested in: Event, Distance, Time, and Name. I also filtered the "Event" variable to only include freestyle events that are not relays. This is because freestyle is a very basic stroke where most swimmers feel very confident.

```{r}

library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)

filename <- "https://raw.githubusercontent.com/irenedelatorre/swimming-records/master/data/20170306-swimming-times3.csv"
swim_data <- read.csv(filename)

View(swim_data)
```

Then I used the filter() function to separate my data into two groups to compare female versus male swimmers. I also make it so the times would present in a descending order.

```{r}
swim_subset <- swim_data |>
  select("Event", "Distance", "Sex", Time = "Time.1", "Name") |> #I selected my key variables
  filter(str_detect(Event, "freestyle") & !str_detect(Event, "relay")) #I only wanted freestyle events that were not relays

View(swim_subset)
```

```{r}
male_swimmers <- swim_subset|>
  filter(Sex == "Male")|> #seperating male and female swimmers
  arrange(Time)     #to make it so fastest times are listed first

female_swimmers <- swim_subset|>
  filter(Sex == "Female")|>
  arrange(Time)

View(male_swimmers)
View(female_swimmers)
```

The next big step was to separate short distance events (50m freestyle) and long distance events (1500m freestyle) for each gender. I also wanted to only have R report the top ten swimmers for each of these groups.

```{r}
male_swimmers_SD <- male_swimmers |>
  filter(Distance == "50") |>   #Selects 50m freestyle for male swimmers only
  distinct(Name, .keep_all = TRUE) |>
  arrange(Time) |>
  head(10)  #top 10 unique names (no repeats)

male_swimmers_LD <- male_swimmers |>
  filter(Distance == "1500") |> #Selects 1500m freestyle for male swimmers only
  distinct(Name, .keep_all = TRUE) |>
  arrange(Time) |>
  head(10)

# Select the top 10 unique female swimmers for 50m and 1500m distances
female_swimmers_SD <- female_swimmers |>
  filter(Distance == "50") |>
  distinct(Name, .keep_all = TRUE) |>
  arrange(Time) |>
  head(10)

female_swimmers_LD <- female_swimmers |>
  filter(Distance == "1500") |>
  distinct(Name, .keep_all = TRUE) |>
  arrange(Time) |>
  head(10)

# View the results
View(male_swimmers_SD)
View(male_swimmers_LD)
View(female_swimmers_SD)
View(female_swimmers_LD)
```

As I mentioned before, there was no height variable in this data set. I had to manually enter swimmer heights in inches through the separate (to separate first and last names into their own columns) and the mutate (to create a new "Height" column) functions.

```{r}
male_swimmers_SD$Name <- iconv(male_swimmers_SD$Name, from = "latin1", to = "UTF-8", sub = "byte")

male_swimmers_SD$Name <- gsub("é", "e", male_swimmers_SD$Name)
male_swimmers_SD$Name <- gsub("ç", "c", male_swimmers_SD$Name)

male_swimmers_SD_inches <- male_swimmers_SD |>
  separate(Name, into = c("FirstName", "LastName"), sep = " ", extra = "merge") |>
  mutate(Height = case_when(
    LastName == "Biondi" ~ 79,
    LastName == "Jager" ~ 75,
    LastName == "Sullivan" ~ 74,
    LastName == "Popov" ~ 78,
    LastName == "Bernard" ~ 77,
    LastName == "Cielo" ~ 76,       
    LastName == "Bousquet" ~ 74,   
    LastName == "Williams" ~72,
    LastName == "Leamy" ~ 73,
    LastName == "Halsall" ~ 75,
    TRUE ~ NA_real_  # Assign NA for others if no match
  ))
View(male_swimmers_SD_inches)
```

```{r}
male_swimmers_LD$Name <- iconv(male_swimmers_LD$Name, from = "latin1", to = "UTF-8", sub = "byte")

male_swimmers_LD$Name <- gsub("é", "e", male_swimmers_LD$Name)
male_swimmers_LD$Name <- gsub("ç", "c", male_swimmers_LD$Name)

male_swimmers_LD_inches <- male_swimmers_LD |>
  separate(Name, into = c("FirstName", "LastName"), sep = " ", extra = "merge") |>
  mutate(Height = case_when(
    LastName == "Yang" ~ 78,
    LastName == "Hackett" ~ 78,
    LastName == "Perkins" ~ 76,
    LastName == "Hoffmann" ~ 78,
    LastName == "Salnikov" ~ 71,
    LastName == "Goodell" ~ 68,       
    LastName == "Holland" ~ 60,   
    LastName == "Shaw" ~74,
    LastName == "Burton" ~ 69,
    LastName == "DeMont" ~ 71,
    TRUE ~ NA_real_  # Assign NA for others if no match
  ))
View(male_swimmers_LD_inches)
```

```{r}
female_swimmers_SD_inches <- female_swimmers_SD |>
  separate(Name, into = c("FirstName", "LastName"), sep = " ", extra = "merge") |>
  mutate(Height = case_when(
    LastName == "Steffen" ~ 71,
    LastName == "Trickett" ~ 66,
    LastName == "Veldhuis" ~ 72,
    LastName == "de Bruijn" ~ 70,
    LastName == "Jingyi" ~ 70,
    LastName == "Wenyi" ~ 70,       
    LastName == "Costache" ~ 68,   
    LastName == "Verstappen" ~71,
    LastName == "Torres" ~ 71,
    LastName == "Sterkel" ~ 71,
    TRUE ~ NA_real_  # Assign NA for others if no match
  ))
View(female_swimmers_SD_inches)
```

```{r}
female_swimmers_LD_inches <- female_swimmers_LD |>
  separate(Name, into = c("FirstName", "LastName"), sep = " ", extra = "merge") |>
  mutate(Height = case_when(
    LastName == "Ledecky" ~ 72,
    LastName == "Ziegler" ~ 72,
    LastName == "Evans" ~ 66,
    LastName == "Linehan" ~ 66,
    LastName == "Wickham" ~ 64,
    LastName == "Browne" ~ 63,       
    LastName == "Turrall" ~ 65,   
    LastName == "Harshbarger" ~64,
    LastName == "Gould" ~ 67,
    LastName == "Calhoun" ~ 69,
    TRUE ~ NA_real_  # Assign NA for others if no match
  ))
View(female_swimmers_LD_inches)
```

Now the data is properly wrangled and tidy to begin a graphical analysis!

## Data Visualizations

```{r}
ggplot(male_swimmers_SD_inches, aes(x = Height, y = Time)) +
  geom_point(color = "blue", size = 3) +
  labs(
    title = "Male Swim Time vs. Swimmer Height (50m Freestyle)",
    x = "Height (inches)",
    y = "Swim Time (seconds)"
  )
```

The scatter plot above creates a visual representation of male swimmers in short distance events comparing their height in inches and their event time measured in seconds. The scatter plot has a seemingly random distribution of data points for height and speed. The range is from 72 inches in height (6 feet tall) to 79 inches in height (6 feet 7 inches tall). The two fastest times belong to two swimmers, one at 76 inches tall (6 feet 4 inches) and one swimmer at 74 inches tall (6 feet and 2 inches tall).

```{r}
ggplot(male_swimmers_LD_inches, aes(x = Height, y = Time)) +
  geom_point(color = "blue", size = 3) +
  labs(
    title = "Male Swim Time vs. Swimmer Height (1500m Freestyle)",
    x = "Height (inches)",
    y = "Swim Time (seconds)"
  )
```

The above scatter plot shows the results from male swimmers in long distance events (1500m) comparing their height and speed in the water. This graph has a bit more of a correlation it seems between height and speed. The swimmers on this chart are mostly above 67 inches (5 feet 7 inches). There is a negative correlation where the taller the swimmer, the faster their swim time tends to be. This graph may provide an answer to this paper's central question. Male swimmers who are taller, tend to have faster swimming times in long distance freestyle events.

```{r}
ggplot(female_swimmers_SD_inches, aes(x = Height, y = Time)) +
  geom_point(color = "blue", size = 3) +
  labs(
    title = "Female Swim Time vs. Swimmer Height (50m Freestyle)",
    x = "Height (inches)",
    y = "Swim Time (seconds)"
  )
```

The scatter plot above creates a visual representation of female swimmers in short distance events comparing their height in inches and their event time measured in seconds. Interestingly, like the long distance male swimmer chart, this chart shows that there are more female swimmers who are taller also tend to have the faster swimming times in short distance events. While in this line up there are shorter swimmers who have achieved faster swimming times than their taller competitors, there are much more female swimmers above 69 inches tall (5 feet 9 inches).

```{r}
ggplot(female_swimmers_LD_inches, aes(x = Height, y = Time)) +
  geom_point(color = "blue", size = 3) +
  labs(
    title = "Female Swim Time vs. Swimmer Height (1500m Freestyle)",
    x = "Height (inches)",
    y = "Swim Time (seconds)"
  )
```

Last but certainly not least, the scatter plot above represents female swimmers in long distance events comparing their height and speed in the water. Interestingly, this plot seems to be the opposite of the short distance female swimmer plot above. There are much more female swimmers below the height of 69 inches tall (5 feet 9 inches) within the top ten times compared to female swimmers that are taller. This graph indicates however that the two tallest female swimmers in this study have the two fastest times. Unlike the long distance male swimmer plot, this plot provides evidence against the central question of this paper. Swimmer height does not necessarily indicate swimmer speed in long distance events.

## Preliminary Conclusions

If there was no correlation between swimmer speed and swimmer height, we would expect to see a random distribution of data points in the scatter plots generated. While this result is visible in the short distance male swimmers plot, it is not the case in the other three plots.

For males, height may not necessarily be a determining factor for swimming speed in shorter events. Taller males tend to perform faster in longer distance events than shorter male swimmers. In shorter distance events, taller females tend to have a majority of the faster times. In longer distance events, shorter females tend to have a majority of the faster times. The tallest females have the fastest times however.

In conclusion, height may be an important factor when it comes to swim speed, but it isn't an end all be all (especially across genders)!
