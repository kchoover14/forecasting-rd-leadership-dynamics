######################## LIBRARIES

library(vdemdata)    # V-Dem democracy dataset
library(wbstats)     # World Bank data via API
library(countrycode) # standardize country codes
library(dplyr)       # data wrangling
library(tidyr)       # data reshaping
library(ggplot2)     # static plots
library(plotly)      # interactive plots
library(ggeffects)   # predicted values from regression models
library(classInt)    # Jenks natural breaks classification
library(scales)      # axis and label formatting
library(htmlwidgets) # save interactive plots as HTML

######################## GET VDEM DATA

vdemData = vdem |>
  dplyr::select(
    vDemCtryId = country_id,
    country = country_name,
    region = e_regionpol_6C,
    year
  ) |>
  mutate(region = case_match(region,
    1 ~ "Eastern Europe",
    2 ~ "Latin America",
    3 ~ "Middle East",
    4 ~ "Africa",
    5 ~ "the West",
    6 ~ "Asia")
  )

# add ISO3 country codes for merge with World Bank
vdemData = vdemData |>
  mutate(iso3c = countrycode(
    sourcevar = vDemCtryId,
    origin = "vdem",
    destination = "wb")
  )

######################## GET WORLD BANK DATA

indicators = c(
  popTotal        = "SP.POP.TOTL",
  gdp             = "NY.GDP.MKTP.CD",
  oldAgeDependency = "SP.POP.DPND.OL",
  rdExpenditure   = "GB.XPD.RSDV.GD.ZS",
  researchersRD   = "SP.POP.SCIE.RD.P6",
  outputPubs      = "IP.JRN.ARTC.SC"
)

wbData = wb_data(indicators, mrv = 50) |>
  dplyr::select(!iso2c) |>
  rename(year = date)

######################## MERGE AND CLEAN

rd = left_join(vdemData, wbData, by = c("iso3c", "year")) |>
  rename(country = country.x) |>
  dplyr::select(!country.y) |>
  dplyr::select(!vDemCtryId) |>
  relocate(iso3c, .before = country)

# scale publications to per million people
rd$outputPubsScaled = (rd$outputPubs / rd$popTotal) * 1e6

######################## SUMMARIZE: 1996-2019

rdDateLimit = rd |>
  filter(year > 1995, year < 2020)

rdSummarize = rdDateLimit |>
  group_by(region, year) |>
  summarise(
    oldAgeDependencyMedian  = median(oldAgeDependency, na.rm = TRUE),
    rdExpenditureMedian     = median(rdExpenditure, na.rm = TRUE),
    researchersRDMedian     = median(researchersRD, na.rm = TRUE),
    outputPubsMedian        = median(outputPubs, na.rm = TRUE),
    outputPubsScaledMedian  = median(outputPubsScaled, na.rm = TRUE),
    gdpMedian               = median(gdp, na.rm = TRUE),
    popTotalMedian          = median(popTotal, na.rm = TRUE),
    .groups = "drop"
  )

######################## FIGURE 1: OLD AGE DEPENDENCY BY REGION (STATIC JPEG)

plotOldAgeDependency = ggplot(rdSummarize, aes(x = year, y = oldAgeDependencyMedian, color = region)) +
  geom_line() +
  labs(
    title = "",
    x = "Year",
    y = "Median Value for Age Dependency",
    caption = "Data Source: World Bank"
  ) +
  scale_color_viridis_d() +
  facet_wrap(~region, nrow = 3, scales = "free") +
  theme_minimal()

ggsave("plotoldAgeDependency.jpeg", plot = plotOldAgeDependency, device = "jpeg",
  width = 9, height = 6, units = "in", dpi = 300)

######################## FIGURE 2: R&D EXPENDITURE CHANGE BY AGE CATEGORY (INTERACTIVE)

rdFiltered = rd |>
  filter(year > 1995, year < 2020) |>
  filter(!is.na(oldAgeDependency), !is.na(rdExpenditure))

# calculate Jenks natural breaks
jenks_breaks = classIntervals(rdFiltered$oldAgeDependency, n = 3, style = "jenks")$brks

# ensure unique breaks
if (length(unique(jenks_breaks)) < length(jenks_breaks)) {
  jenks_breaks = unique(jenks_breaks)
}

break_labels = sprintf("%0.2f - %0.2f", head(jenks_breaks, -1), tail(jenks_breaks, -1))

# assign age categories
rdFiltered = rdFiltered |>
  mutate(ageCategory = cut(oldAgeDependency, breaks = jenks_breaks,
    labels = break_labels, include.lowest = TRUE))

# summarize by year and age category
rdSummarizeAge = rdFiltered |>
  group_by(year, ageCategory) |>
  summarise(
    oldAgeDependencyMedian = median(oldAgeDependency, na.rm = TRUE),
    rdExpenditureMedian    = median(rdExpenditure, na.rm = TRUE),
    .groups = "drop"
  )

# base year and three-year change labels
base_year = 1996 + ((3 - (1996 %% 3)) %% 3)

rdSummarizeAge = rdSummarizeAge |>
  arrange(ageCategory, year) |>
  group_by(ageCategory) |>
  mutate(changeInExpenditure = rdExpenditureMedian - lag(rdExpenditureMedian, n = 3)) |>
  ungroup() |>
  mutate(label = ifelse(
    year >= base_year & (year - base_year) %% 3 == 0 & !is.na(changeInExpenditure),
    sprintf("Delta %.2f", changeInExpenditure), "")
  )

plotAgeRD = ggplot(rdSummarizeAge, aes(x = year, y = rdExpenditureMedian,
    group = ageCategory, color = ageCategory)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Change in R&D Expenditure Every Three Years",
    x = "Year",
    y = "R&D Expenditure Median (USD per 1 Million)",
    color = "Old Age Dependency"
  ) +
  scale_x_continuous(breaks = seq(base_year, max(rdSummarizeAge$year), by = 3)) +
  scale_y_continuous(limits = c(
    min(rdSummarizeAge$rdExpenditureMedian) * 0.9,
    max(rdSummarizeAge$rdExpenditureMedian) * 1.1)
  ) +
  scale_color_viridis_d(end = 0.8) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.spacing.x = unit(1.0, "cm"),
    panel.spacing = unit(1, "cm")
  )

p1 = ggplotly(plotAgeRD)
saveWidget(p1, "RdExpenditureChange.html", selfcontained = TRUE)

######################## FIGURE 3: PREDICTED IMPACT ON PUBLICATIONS (INTERACTIVE)

fit = lm(outputPubsScaled ~ oldAgeDependency * region, data = rd)
preds = ggpredict(fit, terms = c("oldAgeDependency", "region"))

pubsWidget = ggplot(preds, aes(x = x, y = predicted)) +
  geom_line(color = "steelblue") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "slategray3", alpha = 0.2) +
  labs(
    x = "Aging Population Index",
    y = "Number of Publications per Million",
    title = "Predicted Impact of Aging Population on Publications",
    subtitle = "Analyzing Trends Over Regions"
  ) +
  facet_wrap(~group) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.background = element_rect(colour = "snow", fill = "steelblue"),
    strip.text.x = element_text(colour = "snow")
  )

p2 = ggplotly(pubsWidget)
saveWidget(p2, "PredPubs.html", selfcontained = TRUE)

######################## FIGURE 4: PREDICTED IMPACT ON RESEARCHERS IN R&D (INTERACTIVE)

fit2 = lm(researchersRD ~ oldAgeDependency * region, data = rd)
preds2 = ggpredict(fit2, terms = c("oldAgeDependency", "region"))

rdWidget = ggplot(preds2, aes(x = x, y = predicted)) +
  geom_line(color = "steelblue") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "slategray3", alpha = 0.2) +
  labs(
    x = "Aging Population Index",
    y = "Number of Researchers in R&D per Million",
    title = "Predicted Impact of Aging Population on Researchers in R&D",
    subtitle = "Analyzing Trends Over Regions"
  ) +
  facet_wrap(~group) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.background = element_rect(colour = "snow", fill = "steelblue"),
    strip.text.x = element_text(colour = "snow")
  )

p3 = ggplotly(rdWidget)
saveWidget(p3, "PredRd.html", selfcontained = TRUE)
