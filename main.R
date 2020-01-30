library(rjson)
library(tidyverse)
library(ggthemes)

corona_json <- "https://services1.arcgis.com/0MSEUqKaxRlEPj5g/arcgis/rest/services/ncov_cases/FeatureServer/1/query?f=json&where=1%3D1&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&orderByFields=Confirmed%20desc&outSR=102100"

corona_raw <- fromJSON(readLines(corona_json))

corona_list <- sapply(X = corona_raw$features,
                      FUN = function (x) {
                        attr <- x$attributes
                        nulls <- sapply(attr, is.null)
                        attr[nulls] <- NA
                        attr
                      },
                      simplify = FALSE)


corona_data <- do.call(rbind, corona_list) %>% data.frame() %>% as_tibble()

corona_data <- apply(X = corona_data,
                     MARGIN = 2,
                     FUN = function (x) {
                       sapply(X = x,
                              FUN = function (y) y[[1]])
                     })
# Number of infections and deaths by country
corona_grouped <- corona_data %>%
  as_tibble() %>%
  mutate(country = trimws(Country_Region)) %>%
  group_by(country) %>%
  summarise(infections = sum(as.numeric(Confirmed), na.rm = TRUE),
            deaths = sum(as.numeric(Deaths), na.rm = TRUE))

# plot
corona_grouped %>%
  ggplot(aes(x = country, y = infections, size = deaths, group = country)) +
    geom_point(color = "#EF5D60") +
    xlab("Country") + ylab("Infections") +
    theme_tufte() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
