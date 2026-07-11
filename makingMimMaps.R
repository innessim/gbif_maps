# to make a distribution map for talk

#nasuta 8427564

## load libraries ####
library(rgbif)
library(ggplot2)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(dplyr)
library(ggthemes)
library(hexbin)

# Get occurence data ####
# Define North America countries vector
north_america_countries <- c("US", "CA", "MX") # choose desired geographic extent


## Get occurrences for *Mimulus moschatus* (taxonKey = 7346090) ####

# modify code below for desired taxa

# Fetch occurrences with coordinates limited to North America
occ_mosch_data <- occ_search(
  taxonKey = 7346090,             # Mimulus moschatus taxonKey
  limit = 10000,
  hasCoordinate = TRUE,
  country = north_america_countries,
)

occ_mosch_df <- bind_rows(lapply(occ_mosch_data, `[[`, "data"))


## Get occurrences for *Mimulus guttats* (taxonKey = 7346102) ####
# Fetch occurrences with coordinates limited to North America
occ_gutt_data <- occ_search(
  taxonKey = 7346102,             # Mimulus guttatus taxonKey
  limit = 10000,
  hasCoordinate = TRUE,
  country = north_america_countries,
)

occ_gutt_df <- bind_rows(lapply(occ_gutt_data, `[[`, "data"))


## Get occurrences for *Mimulus glabratus* (taxonKey = 8419357) ####
# Fetch occurrences with coordinates limited to North America
occ_glab_data <- occ_search(
  taxonKey = 8419357,             # Mimulus glabratus taxonKey
  limit = 10000,
  hasCoordinate = TRUE,
  country = north_america_countries,
)

occ_glab_df <- bind_rows(lapply(occ_glab_data, `[[`, "data"))


## Get occurrences for *Mimulus floribundus* (taxonKey = 7346092) ####
# Fetch occurrences with coordinates limited to North America
occ_flori_data <- occ_search(
  taxonKey = 7346092,             # Mimulus floribundus taxonKey
  limit = 10000,
  hasCoordinate = TRUE,
  country = north_america_countries,
)

occ_flori_df <- bind_rows(lapply(occ_flori_data, `[[`, "data"))


## Get occurrences for *Mimulus tilingii* (taxonKey = 7346099) ####
# Fetch occurrences with coordinates limited to North America
occ_tili_data <- occ_search(
  taxonKey = 7346099,             # Mimulus tilingii taxonKey
  limit = 10000,
  hasCoordinate = TRUE,
  country = north_america_countries,
)

occ_tili_df <- bind_rows(lapply(occ_tili_data, `[[`, "data"))


## Get occurrences for *Mimulus nasutus* (taxonKey = 8427564) ####
# Fetch occurrences with coordinates limited to North America
occ_nasu_data <- occ_search(
  taxonKey = 8427564,             # Mimulus nasutus taxonKey
  limit = 10000,
  hasCoordinate = TRUE,
  country = north_america_countries,
)

occ_nasu_df <- bind_rows(lapply(occ_nasu_data, `[[`, "data"))


### bring species dfs together and clean ####
mim_occ_df <- bind_rows(occ_mosch_df, occ_gutt_df, occ_glab_df, 
                        occ_flori_df, occ_tili_df, occ_nasu_df) %>% 
  filter(!is.na(decimalLatitude), !is.na(decimalLongitude)) %>%
  select(decimalLongitude, decimalLatitude)

### prepare map and plot ####
# Get countries in North America only
north_america_map <- ne_countries(
  scale = "medium",
  returnclass = "sf",
  continent = "North America"
)

# Plot all yellow monkeyflowers
ggplot() +
  geom_sf(data = north_america_map, fill = "#043b3f", color = "black", size = 0.1) +
  stat_bin_hex(
    data = mim_occ_df,
    aes(x = decimalLongitude, y = decimalLatitude, fill = after_stat(count)),
    binwidth = c(0.85, 0.6)  # smaller numbers = smaller hexes
  ) +
  scale_fill_gradientn(
    colours = c("yellow", "gold", "orange", "orangered", "red", "darkred"),
    trans = "sqrt",
    name = "Occurrences"
  ) +
  coord_sf(
    xlim = c(-170, -50),
    ylim = c(10, 80),
    expand = FALSE
  ) +
  guides(fill = "none") +
  theme_void(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "#7173ab", color = NA),
    plot.background = element_rect(fill = "#7173ab", color = NA),
    legend.background = element_rect(fill = "#7173ab", color = NA),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white")
  )



# Plot just musky monk
ggplot() +
  geom_sf(data = north_america_map, fill = "#043b3f", color = "black", size = 0.1) +
  stat_bin_hex(
    data = occ_mosch_df,
    aes(x = decimalLongitude, y = decimalLatitude, fill = after_stat(count)),
    binwidth = c(0.85, 0.6)  # smaller numbers = smaller hexes
  ) +
  scale_fill_gradientn(
    colours = c("yellow", "gold", "orange", "orangered", "red", "darkred"),
    trans = "sqrt",
    name = "Occurrences"
  ) +
  coord_sf(
    xlim = c(-170, -50),
    ylim = c(10, 80),
    expand = FALSE
  ) +
  guides(fill = "none") +
  theme_void(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "#7173ab", color = NA),
    plot.background = element_rect(fill = "#7173ab", color = NA),
    legend.background = element_rect(fill = "#7173ab", color = NA),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white")
  )





















