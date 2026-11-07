
# Install fredr if not already installed (run once in Console)
install.packages("fredr")

# Step 2: Get a free FRED API key
# Go to: https://fred.stlouisfed.org/docs/api/api_key.html
# Click "Request API Key", create a free account, and copy the key

# Set your API key (replace with your actual key)
library(fredr)
fredr_set_key("ceec410cf8cc56d48b14af7fbc5ed297")

# Pull the data
library(fpp3)

housing <- fredr(series_id = "HOUSTNSA") |>
  transmute(
    Month = yearmonth(date),
    starts = value          # thousands of units, NSA
  ) |>
  as_tsibble(index = Month) |>
  filter_index("1959 Jan" ~ .)   # full sample from Jan 1959

# Quick check
housing |> autoplot(starts) +
  labs(title = "U.S. Housing Starts (NSA)",
       y = "Thousands of Units", x = NULL)


# Dimensions of the tsibble 
glimpse(housing)          

# First and last observations ---
housing |> head(3)        # first 3 rows
housing |> tail(3)        # last 3 rows  

# Count observations and check missings 
housing |>
  as_tibble() |>
  summarise(
    n_obs       = n(),
    first_date  = min(Month),
    last_date   = max(Month),
    n_missing   = sum(is.na(starts)),
    min_starts  = min(starts, na.rm = TRUE),
    max_starts  = max(starts, na.rm = TRUE),
    mean_starts = round(mean(starts, na.rm = TRUE), 1)
  )

# Check for implicit gaps (missing months) ---
# A tsibble with gaps will report TRUE here
has_gaps(housing)
scan_gaps(housing)         # prints the exact missing months, if any

# Flag any zero or negative values (implausible) ---
housing |> filter(starts <= 0)

# COVID disruption: look at 2019-2021 in detail
housing |>
  filter(year(Month) %in% 2019:2021) |>
  print(n = 36)

# Key month to inspect is April 2020.
# % change from April 2019 to April 2020:
housing |>
  filter(month(Month) == 4, year(Month) %in% c(2019, 2020))

# Great Recession collapse: 2006-2012 
housing |>
  filter(year(Month) %in% 2006:2012)

# SECTION 7: Time-series plot for the proposal
library(ggplot2)

housing |>
  autoplot(starts, colour = "steelblue4", linewidth = 0.4) +
  labs(
    title    = "U.S. New Privately-Owned Housing Units Started (NSA)",
    subtitle = "Monthly, January 1959 – September 2026",
    y        = "Thousands of Units",
    x        = NULL,
    caption  = "Source: U.S. Census Bureau & HUD via FRED (series: HOUSTNSA)"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "grey40"),
        plot.caption  = element_text(colour = "grey50", size = 8))
















