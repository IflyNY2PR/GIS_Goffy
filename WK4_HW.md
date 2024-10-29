---
title: "WK4_HW"
author: "Goffy"
date: "2024-10-28"
output: html_document
---

##Library
```{r}

library(tidyverse)
library(countrycode)
library(sf)
library(here)
library(ggplot2)

```

##Read Data
```{r}

World_Countries <- st_read(here("Data","World_Countries_(Generalized)_9029012925078512962.geojson"))
Inequality_Data <- read_csv(here("Data", "HDR23-24_Composite_indices_complete_time_series.csv"))

```
##Clean Data
```{r}

GII <- Inequality_Data %>%
  filter(., !str_starts(iso3, "ZZ"))

```

##Creat New Columns
```{r}

GII_10_19 <- GII %>%
  mutate(diff_10_19 = gii_2019 - gii_2010)

GII_a <- GII_10_19 %>%
  select("iso3", "country", "diff_10_19")

```

##Transfer iso3 to iso2
```{r}

GII_b <- GII_a %>%
  mutate(iso2 = countrycode(iso3, origin = "iso3c", destination = "iso2c"))

GII_c <- GII_b %>%
  mutate(iso3=iso2) %>%
  select(-iso2)

GII_c <- GII_c %>%
  rename(ISO = iso3)

```

##Merge Data & Map
```{r}

Trend_GII <- World_Countries %>%
  left_join(GII_c, by = "ISO")

```

##Plot Data
```{r}

ggplot(Trend_GII) + 
  geom_sf(aes(fill = diff_10_19), color = "black") + 
  scale_fill_gradientn(
    colors = c("blue", "lightblue", "white", "pink", "red"),
    na.value = "gray", 
    name = "GII Difference") +
  theme_minimal() +
  ggtitle ("WorldGender Inequality Trend (2010-2019)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  labs(caption = "Source: Human Development Report (https://hdr.undp.org/data-center)")
  
```

##Try to Add Annotation
```{r}

library(grid)

ggplot(Trend_GII) + 
  geom_sf(aes(fill = diff_10_19), color = "black") + 
  scale_fill_gradientn(
    colors = c("blue", "lightblue", "white", "pink", "red"),
    na.value = "gray", 
    name = "GII Difference") +
  theme_minimal() +
  ggtitle("World Gender Inequality Trend (2010-2019)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.margin = unit(c(1, 1, 4, 1), "lines")) +  # Increase bottom margin
  labs(caption = "Source: Human Development Report (https://hdr.undp.org/data-center)") +
  coord_sf(clip = "off") +  # Allow annotations outside plot area
  annotation_custom(
    grob = rectGrob(gp = gpar(fill = "gray", col = "gray")),
    xmin = -180, xmax = -170, ymin = -110, ymax = -100
  ) +
  annotation_custom(
    grob = textGrob("No Data", gp = gpar(col = "black", fontsize = 10)),
    xmin = -150, xmax = -140, ymin = -110, ymax = -100)



```

