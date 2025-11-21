**Use:**

The historicalBorders package allows users to visualize political boundaries for a selected continent, region, or country at a specific point in history. Users can specify a year, and the package will generate a static map displaying the borders of the chosen location. The map reflects borders that were stable for at least 10 years. This provides a historically accurate snapshot of geopolitical boundaries.

**Details:**

Users have dates to choose from spanning to back to 1800 and incremented by every 50 years. Users can examine different scales, including at the country, continent, and world levels. Users also have the option of inputting custom latitude and longitude values to map a desired region of the world.

The borders closest to the year inputted by the user will be returned (ex: if a user inputted 1847, it would return a map of borders in 1850). If the user inputs a year that is not within the range of years included in the package, an error will be returned.

**Installation**

Installing historicalBorders can be done through the use of

```{r}
install.packages("historicalBorders")
```

Alternately, one can install the package using:

```{r}
remotes::install_github("r-historicalBorders/historicalBorders")
```
