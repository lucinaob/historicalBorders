# Historic Borders

This package allows users to visualize political boundaries for a selected continent, region, or country at a specific point in history. Building upon existing ways of visualizing spatial data, we aimed to create a way for R users to map data over historic borders from the last 200 years. In this, we can visualize data in a way that makes sense historically — showing the borders that people throughout history would have understood and lived within. Maps generated with this package are derived from the [historic-basemaps](https://github.com/aourednik/historical-basemaps) repository.

# Details

Users have historic maps choose from spanning to back to 1800, with options of years viewable using `optionsYears()`. If a user ever tries to map a year that is not in the data, functions will map historic borders from the closest year available. Users can also map historic borders at different scales, including at the country, regional, continent, and global level. If a user wishes to map a more specific region of the world, they can customize a map using `mapCustom()` and inserting latitude and longitude pairs they wish to visualize within.

# **Installation**

One can install the package using:

```{r}
remotes::install_github("lucinaob/historicalBorders")
```
