library(tidyverse)
install.packages('tidyverse')
library(tidyverse)
# The Importance of Forest Fire Data
forestfires <- read_csv("forestfires.csv")
```
# What columns are in the dataset?
colnames(forestfires)
```
We know that the columns correspond to the following information:
  
* **X**: X-axis spatial coordinate within the Montesinho park map: 1 to 9 
* **Y**: Y-axis spatial coordinate within the Montesinho park map: 2 to 9 
* **month**: Month of the year: 'jan' to 'dec' 
* **day**: Day of the week: 'mon' to 'sun' 
* **FFMC**: Fine Fuel Moisture Code index from the FWI system: 18.7 to 96.20 
* **DMC**: Duff Moisture Code index from the FWI system: 1.1 to 291.3 
* **DC**: Drought Code index from the FWI system: 7.9 to 860.6 
* **ISI**: Initial Spread Index from the FWI system: 0.0 to 56.10 
* **temp**: Temperature in Celsius degrees: 2.2 to 33.30 
* **RH**: Relative humidity in percentage: 15.0 to 100 
* **wind**: Wind speed in km/h: 0.40 to 9.40 
* **rain**: Outside rain in mm/m2 : 0.0 to 6.4 
* **area**: The burned area of the forest (in ha): 0.00 to 1090.84 


forestfires %>% pull(month) %>% unique
forestfires %>% pull(day) %>% unique

month_order <- c("jan", "feb", "mar",
                 "apr", "may", "jun",
                 "jul", "aug", "sep",
                 "oct", "nov", "dec")

dow_order <- c("sun", "mon", "tue", "wed", "thu", "fri", "sat")

forestfires <- forestfires %>% 
  mutate(
    month = factor(month, levels = month_order),
    day = factor(day, levels = dow_order)
  )

We need to create a summary tibble that counts the number of 
fires that appears in each month. Then, 
we will be able to use this tibble in a visualization. 
We can consider `month` and `day` to be different grouping 
variablse, so our code to produce the tibbles and plots will 
look similar.

## Month Level

```{r}
fires_by_month <- forestfires %>%
  group_by(month) %>%
  summarize(total_fires = n())

fires_by_month %>% 
  ggplot(aes(x = month, y = total_fires)) +
  geom_col() +
  labs(
    title = "Number of forest fires in data by month",
    y = "Fire count",
    x = "Month"
  )
```

fires_by_dow <- forestfires %>%
  group_by(day) %>%
  summarize(total_fires = n())

fires_by_dow %>% 
  ggplot(aes(x = day, y = total_fires)) +
  geom_col() +
  labs(
    title = "Number of forest fires in data by day of the week",
    y = "Fire count",
    x = "Day of the week"
  )

We see a massive spike in fires in August and September, as well as a smaller 
spike in March. Fires seem to be more frequent on the weekend.

# Plotting Other Variables Against Time 

```{r}
forest_fires_long <- forestfires %>% 
  pivot_longer(
    cols = c("FFMC", "DMC", "DC", 
             "ISI", "temp", "RH", 
             "wind", "rain"),
    names_to = "data_col",
    values_to = "value"
  )

forest_fires_long %>% 
  ggplot(aes(x = month, y = value)) +
  geom_boxplot() +
  facet_wrap(vars(data_col), scale = "free_y") +
  labs(
    title = "Variable changes over month",
    x = "Month",
    y = "Variable value"
  )
```
# Examining Forest Fire Severity

We are trying to see how each of the variables in the dataset relate to `area`. 
We can leverage the long format version of the data we created to use with `facet_wrap()`.

```{r}
forest_fires_long %>% 
  ggplot(aes(x = value, y = area)) +
  geom_point() +
  facet_wrap(vars(data_col), scales = "free_x") +
  labs(
    title = "Relationships between other variables and area burned",
    x = "Value of column",
    y = "Area burned (hectare)"
  )
```

# Outlier Problems

It seems that there are two rows where `area` that still hurt the scale of the visualization.
Let s make a similar visualization that excludes these observations so that we can better see how each variable relates to `area`.

```
forest_fires_long %>% 
  filter(area < 300) %>% 
  ggplot(aes(x = value, y = area)) +
  geom_point() +
  facet_wrap(vars(data_col), scales = "free_x") +
  labs(
    title = "Relationships between other variables and area burned (area < 300)",
    x = "Value of column",
    y = "Area burned (hectare)"
  )
```