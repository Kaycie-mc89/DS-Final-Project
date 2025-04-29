---
title: "Names in Numbers: A Tutorial on how to use Data Wrangling to find Trends"
author: "Kaycie McColley"
date: "`r Sys.Date()`"
output: 
  html_document:
    toc: true  
---

```{r setup, include=FALSE}
library(tidyverse)
library(babynames)
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
```

## Introduction

Data wrangling is a very important skill when sharing and communicating data to others. It allows us to transform the data to allow us to make meaningful connections and observations. This project will focus on creating a data wrangling tutorial using the 'babynames' data set from the R package. This package comes from the U.S. Social Security Administration, and it contains information of baby name data over time. In this tutorial, I will explain how data wrangling works and use it as an example to uncover unique outcomes in the data set.

## Loading and Exploring the Data Set

```{r}
babynames
glimpse(babynames)
```

Its important to understand the data before you begin to edit it. This data set has five columns and 1,924,655 individual observations. The column year is a categorical variable describing the year the baby was born. Sex is a categorical variable that tells us the gender of the baby. Name is categorical and it tells us the name of the baby. N is a quantitative variable and it measures the amount of babies born with the same name. Prop is also a quantitative variable and it measures the proportion of babies with the same name.

## Data Wrangling

Let's say I want to analyze baby names only in the year 2004 - the year I was born!

### Step 1: Initial Filter

```{r}

babies_2004 <- babynames |>      #This is me saving what I want to filter into a new name from the dataset.  
  filter(year == 2004) |>         #This is me telling R I only want the names of babies born in the year 2004.       
  arrange(desc(n))             #This will arange the rows so the 'prop' is in a descending order.

print(babies_2004)
```

After this first step, we can see that 1,924,655 individual observations became 32,046 observations only from the year 2004!

### Step 2: Filtering Further

You can also filter further, you aren't limited to filter your data on only one variable! In this step, I am going to also filter sex to be F for female and the first letter of the name beginning with K.

```{r}
babies_2004_f_k <- babynames |>     
  filter(year == 2004, sex == "F", str_starts(name, "K")) |>         
  arrange(desc(n))

print(babies_2004_f_k)
```

The more filters you add, the more precise and smaller your data set will get (32,046 observations to 1,774)!

### Step 3: Renaming and Editing columns

In data wrangling, you can also rename and edit the columns of your data! I want to change the column 'n' to count because it is a better title for the variable. I also want to create a new column that will categorize each name by how popular it is based on the number of babies named per year. This will be called 'popularity'. I also want to add another column that quantifies the amount of letters in each name for a graph I want to create later. This new column will be called "letters"

```{r}
babies_2004_f_k <- babies_2004_f_k |>     
  filter(year == 2004) |>         
  rename(count = n)  # Renamed 'n' to 'count'

babies_2004_f_k_pop <- babies_2004_f_k |> 
  mutate(popularity = case_when(       #This will be how R knows how to label popularity levels in a new column from the 'mutate' function.
    count > 5000 ~ "Very Popular",
    count > 1000 ~ "Common",
    TRUE ~ "Rare"
  ))

babies_2004_f_k_pop <- babies_2004_f_k_pop |>
  arrange(desc(count)) |>  
  ungroup() |>  
  mutate(letters = nchar(name))  # Add name length column


print(babies_2004_f_k_pop)
```

Now our 'n' column is titled 'count', we have a new column that categorizes each name into popularity levels: Very Popular, Common, and Rare, and we have a new column that contains the letter amount in each name.

### Step 4: Putting It All Together

Now that our data is wrangled, we can use it to generate graphs and make observations about our data! Lets say I want to see if the letter length of K names of babies born in 2004 is related to how popular a name is.

```{r}
ggplot(babies_2004_f_k_pop, aes(x = letters, y = count)) + 
  geom_point(aes(color = popularity), size = 3, alpha = 0.7) + 
  labs(title = "Relationship Between Name Length and Popularity of 'K' Names in 2004", 
       x = "Name Length (Letters)", 
       y = "Number of Babies (Popularity)") +
  scale_color_manual(values = c("Very Popular" = "red", 
                                "Common" = "blue", 
                                "Rare" = "black"))+
  theme_minimal()
```

As we can see from this, I have generated a plot which allows us to easily view the letter length versus name popularity for females born in 2004 with their first name beginning with K. As we can see, the most popular names are from 5 letters to 9 letters in length. Across all letter lengths, we can see that rare names are present. Common names are present at 3 letters to around 8 letters. This is a distribution we would expect to see. The data appears to be uni-modal, with its peak at 5 letters.

## Conclusion

In this tutorial, I used data wrangling to edit and clean up the 'babynames' data set in R! This made it so I could very easily analyze the specific observations I was targeting in a very neat way. Through the steps of filtering, renaming columns, and visualizing the data, I was able to uncover interesting patterns and relationships in the baby names data. By the end of this tutorial, I hope you have a better understanding of how data wrangling can simplify the analysis process and make your data more manageable and insightful!

I also grew curious and was able to find my name among the data set which was really cool! I hope this tutorial can help demonstrate the effectiveness of data wrangling!
