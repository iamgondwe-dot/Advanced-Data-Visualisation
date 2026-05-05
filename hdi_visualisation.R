 install.packages(c(
  "tidyverse",          # Core: ggplot2, dplyr, tidyr, readr, etc.
   "readxl",             # Read Excel files
   "sf",                 # Simple features: spatial data handling
   "rnaturalearth",      # World map boundary data
   "rnaturalearthdata",  # Data package required by rnaturalearth
   "plotly",             # Interactive web figures
   "htmlwidgets",        # Save plotly figures as HTML files
   "RColorBrewer",       # Accessible colour palettes
  "scales",              # Axis formatting helpers
  "countrycode"
 ))

Y

# ---- SECTION 2: IMPORT AND CLEAN THE DATA -----------------------------------

# Set your working directory to the folder containing this script and the
# Excel file.  In RStudio: Session > Set Working Directory > To Source Location
# Or use setwd("path/to/your/folder")

# Read the Excel file.
# skip = 5  : ignore the first 5 rows (title, blanks, and column header rows).
# col_names = FALSE : we supply our own column names below.
hdi_raw <- read_excel(
  "HDR25_Statistical_Annex_HDI_Trends_Table.xlsx",
  sheet     = "Table 2. HDI trends",
  skip      = 5,
  col_names = FALSE
)

# The spreadsheet interleaves blank columns between each year column.
# Useful columns:
#   col 1  = HDI rank
#   col 2  = Country name
#   col 3  = 1990 HDI value
#   col 5  = 2000 HDI value   (col 4 is blank)
#   col 7  = 2010 HDI value   (col 6 is blank)
#   col 9  = 2015 HDI value   (col 8 is blank)
#   col 11 = 2020 HDI value   (col 10 is blank)
#   col 13 = 2021 HDI value   (col 12 is blank)
#   col 15 = 2022 HDI value   (col 14 is blank)
#   col 17 = 2023 HDI value   (col 16 is blank)

hdi_selected <- hdi_raw %>%
  select(1, 2, 3, 5, 7, 9, 11, 13, 15, 17) %>%
  rename(
    hdi_rank = 1,
    country  = 2,
    `1990`   = 3,
    `2000`   = 4,
    `2010`   = 5,
    `2015`   = 6,
    `2020`   = 7,
    `2021`   = 8,
    `2022`   = 9,
    `2023`   = 10
  )

# The spreadsheet contains group-header rows (e.g., "Very high human
# development") that are not actual country rows.  We detect them by
# checking whether they appear in col 2 while col 1 is NA, then use
# tidyr::fill() to forward-fill the group label into every country row
# that follows it.

group_labels <- c(
  "Very high human development",
  "High human development",
  "Medium human development",
  "Low human development",
  "Other countries or territories",
  "Human development groups",
  "Regions"
)

hdi_with_groups <- hdi_selected %>%
  mutate(
    group = if_else(country %in% group_labels, country, NA_character_)
  ) %>%
  # Forward-fill: each country inherits the group header above it
  fill(group, .direction = "down") %>%
  # Drop the header rows themselves
  filter(!country %in% group_labels) %>%
  # Drop rows with no HDI rank (footnote rows, blank rows, regional totals)
  filter(!is.na(hdi_rank))

# The UNDP dataset uses ".." to denote missing data.
# We convert those to proper R NA values, then coerce columns to numeric.
hdi_clean <- hdi_with_groups %>%
  mutate(
    across(
      .cols = c(`1990`, `2000`, `2010`, `2015`, `2020`, `2021`, `2022`, `2023`),
      .fns  = ~ na_if(as.character(.), "..")   # Step 1: ".." -> NA
    )
  ) %>%
  mutate(
    across(
      .cols = c(`1990`, `2000`, `2010`, `2015`, `2020`, `2021`, `2022`, `2023`),
      .fns  = as.numeric                        # Step 2: character -> number
    ),
    hdi_rank = as.integer(hdi_rank)
  )

# Quick sanity check – should show 193 rows (one per country)
cat("Countries in clean dataset:", nrow(hdi_clean), "\n")


# ---- SECTION 3: RESHAPE TO LONG FORMAT --------------------------------------

# ggplot2 works best with data in "long" (tidy) format:
# one row per country-year combination rather than one row per country.
hdi_long <- hdi_clean %>%
  pivot_longer(
    cols      = c(`1990`, `2000`, `2010`, `2015`, `2020`, `2021`, `2022`, `2023`),
    names_to  = "year",
    values_to = "hdi"
  ) %>%
  mutate(year = as.integer(year))


# =============================================================================
# FIGURE 1 – STATIC TIME SERIES
# HDI trends for seven countries spanning all four development tiers
# =============================================================================

# Country selection rationale:
#   Norway        – Very High: stable top performer (Scandinavia)
#   South Korea   – Very High: dramatic rise from ~0.74 in 1990
#   China         – High: fastest-growing large country
#   Brazil        – High: steady but slower improvement (Latin America)
#   India         – Medium: world's most populous, gradual improvement
#   Ghana         – Medium: among Africa's most consistent improvers
#   Niger         – Low: persistent development challenges (Sahel)
# Together these illustrate both cross-tier inequality and within-tier
# variation – a central theme in development studies.

selected_countries <- c(
  "Norway",
  "Korea (Republic of)",
  "China",
  "Brazil",
  "India",
  "Ghana",
  "Niger"
)

hdi_ts <- hdi_long %>%
  filter(country %in% selected_countries, !is.na(hdi)) %>%
  # Shorten the Korea label so it fits the legend neatly
  mutate(country = recode(country, "Korea (Republic of)" = "South Korea"))

# Colour-blind-safe palette (tested against deuteranopia and protanopia)
ts_colours <- c(
  "Norway"      = "#1B9E77",
  "South Korea" = "#D95F02",
  "China"       = "#7570B3",
  "Brazil"      = "#E7298A",
  "India"       = "#66A61E",
  "Ghana"       = "#E6AB02",
  "Niger"       = "#A6761D"
)

ts_plot <- ggplot(hdi_ts, aes(x = year, y = hdi, colour = country, group = country)) +

  # ---- Tier background bands (drawn first so lines sit on top) ----
  # Very High HDI band (>= 0.80)
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.800, ymax = 1.000,
           fill = "#d9f5e8", alpha = 0.35) +
  # High HDI band (0.70 – 0.80)
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.700, ymax = 0.800,
           fill = "#fff3cd", alpha = 0.35) +
  # Medium HDI band (0.55 – 0.70)
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.550, ymax = 0.700,
           fill = "#fde8d5", alpha = 0.35) +
  # Low HDI band (< 0.55)
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.250, ymax = 0.550,
           fill = "#fde2e2", alpha = 0.35) +

  # ---- Tier labels (right margin) ----
  annotate("text", x = 2024, y = 0.895, label = "Very High",
           colour = "#2d8657", size = 2.8, hjust = 0, fontface = "italic") +
  annotate("text", x = 2024, y = 0.745, label = "High",
           colour = "#b5850a", size = 2.8, hjust = 0, fontface = "italic") +
  annotate("text", x = 2024, y = 0.615, label = "Medium",
           colour = "#c0623a", size = 2.8, hjust = 0, fontface = "italic") +
  annotate("text", x = 2024, y = 0.370, label = "Low",
           colour = "#b03030", size = 2.8, hjust = 0, fontface = "italic") +

  # ---- Data lines and points ----
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.8, shape = 16) +

  # ---- Scales ----
  scale_colour_manual(values = ts_colours, name = NULL) +
  scale_x_continuous(
    breaks  = c(1990, 2000, 2010, 2015, 2020, 2023),
    expand  = expansion(mult = c(0.01, 0.12))   # Extra room on right for tier labels
  ) +
  scale_y_continuous(
    limits  = c(0.25, 1.00),
    breaks  = seq(0.3, 1.0, by = 0.1),
    labels  = label_number(accuracy = 0.1)
  ) +

  # ---- Labels ----
  labs(
    title    = "Human Development Index Trends, 1990–2023",
    subtitle = "Seven countries spanning all four HDI tiers and five world regions",
    x        = "Year",
    y        = "HDI Value",
    caption  = paste0(
      "Source: UNDP Human Development Report Office (2025). Statistical Annex Table 2.\n",
      "Shaded bands indicate UNDP HDI tier boundaries (Very High ≥ 0.80; High 0.70–0.80; ",
      "Medium 0.55–0.70; Low < 0.55)."
    )
  ) +

  # ---- Theme ----
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14, colour = "#1a1a1a"),
    plot.subtitle    = element_text(colour = "grey40", size = 10,
                                    margin = margin(b = 8)),
    plot.caption     = element_text(colour = "grey55", size = 7.5, hjust = 0,
                                    margin = margin(t = 8)),
    legend.position  = "right",
    legend.key.width = unit(1.5, "lines"),
    legend.text      = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey90"),
    axis.line        = element_line(colour = "grey70"),
    plot.margin      = margin(t = 12, r = 60, b = 12, l = 12)
  )

# Save at 300 dpi (print quality)
ggsave("figure1_time_series.png", ts_plot, width = 11, height = 6.5, dpi = 300)
cat("Figure 1 (time series) saved.\n")


# =============================================================================
# FIGURE 2 – CHOROPLETH MAP
# Global distribution of HDI values in 2023
# =============================================================================

# Load world boundaries from rnaturalearth.
# returnclass = "sf" gives us a spatial (simple features) data frame
# that ggplot2 can draw with geom_sf().
world <- ne_countries(scale = "medium", returnclass = "sf")

# Extract the 2023 HDI values we need to plot
hdi_2023 <- hdi_clean %>%
  select(country, hdi_2023 = `2023`, group)

# Country name harmonisation:
# UNDP and Natural Earth use different spellings for many countries.
# The lookup table below maps UNDP names -> Natural Earth 'name_long' field.
name_fixes <- tribble(
  ~undp_name,                               ~ne_name,
  "Korea (Republic of)",                    "South Korea",
  "Korea (Democratic People's Rep. of)",    "North Korea",
  "Congo (Democratic Republic of the)",     "Democratic Republic of the Congo",
  "Congo",                                  "Republic of Congo",
  "Tanzania (United Republic of)",          "United Republic of Tanzania",
  "Iran (Islamic Republic of)",             "Iran",
  "Moldova (Republic of)",                  "Moldova",
  "Bolivia (Plurinational State of)",       "Bolivia",
  "Venezuela (Bolivarian Republic of)",     "Venezuela",
  "Syrian Arab Republic",                   "Syria",
  "Viet Nam",                               "Vietnam",
  "Lao People's Democratic Republic",       "Laos",
  "Türkiye",                                "Turkey",
  "Czechia",                                "Czech Republic",
  "Eswatini (Kingdom of)",                  "Swaziland",
  "Cabo Verde",                             "Cape Verde",
  "Côte d'Ivoire",                          "Ivory Coast",
  "Brunei Darussalam",                      "Brunei",
  "Russian Federation",                     "Russia",
  "Micronesia (Federated States of)",       "Federated States of Micronesia",
  "Antigua and Barbuda",                    "Antigua and Barb.",
  "Trinidad and Tobago",                    "Trinidad and Tobago",
  "Saint Kitts and Nevis",                  "St. Kitts and Nevis",
  "Saint Vincent and the Grenadines",       "St. Vin. and Gren.",
  "Saint Lucia",                            "Saint Lucia",
  "Sao Tome and Principe",                  "São Tomé and Principe",
  "Palestine, State of",                    "Palestine"
)

# Apply name fixes and choose the join key
hdi_2023_fixed <- hdi_2023 %>%
  left_join(name_fixes, by = c("country" = "undp_name")) %>%
  mutate(join_name = if_else(!is.na(ne_name), ne_name, country))

# Join HDI data onto the world geometry
world_hdi <- world %>%
  left_join(hdi_2023_fixed, by = c("name_long" = "join_name"))

# Define five HDI bands for a stepped colour scale
# Breaks follow UNDP tier boundaries plus an intermediate cut
map_breaks <- c(0.40, 0.55, 0.70, 0.80, 0.90)
map_plot <- ggplot(world_hdi) +
  geom_sf(aes(fill = hdi_2023), colour = "white", linewidth = 0.15) +
  
  scale_fill_stepsn(
    colours  = c("#d73027", "#fc8d59", "#fee08b", "#91cf60", "#1a9850"),
    breaks   = c(0.40, 0.55, 0.70, 0.80, 0.90),
    limits   = c(0.30, 1.00),
    na.value = "grey82",
    name     = "HDI (2023)",
    guide    = guide_colorsteps(
      barwidth       = unit(0.55, "cm"),
      barheight      = unit(5.5, "cm"),
      title.position = "top",
      title.hjust    = 0.5
    )
  ) +
  
  coord_sf(crs = "+proj=robin") +
  
  labs(
    title    = "Global Human Development Index, 2023",
    subtitle = "Persistent spatial inequality: Sub-Saharan Africa and South Asia dominate the Low and Medium tiers",
    caption  = "Source: UNDP Human Development Report Office (2025). Statistical Annex Table 2.\nGrey = no data available. Robinson projection."
  ) +
  theme_void(base_size = 11) +
  theme(
    # ---- THIS IS THE KEY FIX: force a white background ----
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    
    plot.title    = element_text(face = "bold", size = 13, hjust = 0.5,
                                 margin = margin(t = 10, b = 4)),
    plot.subtitle = element_text(colour = "grey40", size = 9.5, hjust = 0.5,
                                 margin = margin(b = 8)),
    plot.caption  = element_text(colour = "grey55", size = 7.5, hjust = 0.5,
                                 margin = margin(t = 8, b = 6)),
    legend.position  = c(0.10, 0.38),
    legend.title     = element_text(size = 9, face = "bold"),
    legend.text      = element_text(size = 8, colour = "grey20"),
    plot.margin      = margin(t = 15, r = 10, b = 10, l = 10)
  )

ggsave("figure2_map.png", map_plot, width = 12, height = 6.5, dpi = 300,
       bg = "white")   # <-- bg = "white" is an extra safety net

# =============================================================================
# FIGURE 3 – INTERACTIVE PLOTLY FIGURE
# All 193 countries' HDI trajectories; lines togglable by HDI group
# =============================================================================

# Prepare data: keep only the four main UNDP country groups
hdi_interactive <- hdi_long %>%
  filter(
    !is.na(hdi),
    group %in% c(
      "Very high human development",
      "High human development",
      "Medium human development",
      "Low human development"
    )
  ) %>%
  mutate(
    group_short = recode(group,
      "Very high human development" = "Very High",
      "High human development"      = "High",
      "Medium human development"    = "Medium",
      "Low human development"       = "Low"
    ),
    group_short = factor(group_short,
                         levels = c("Very High", "High", "Medium", "Low"))
  )

# Colour key: same RdYlGn logic as the map, four discrete colours
group_cols <- c(
  "Very High" = "#1a9850",
  "High"      = "#91cf60",
  "Medium"    = "#fc8d59",
  "Low"       = "#d73027"
)

# ------------------------------------------------------------------
# Build the figure using a loop so we can assign each HDI group its
# own legend entry (clicking a legend item hides/shows all countries
# in that group simultaneously).
# ------------------------------------------------------------------

fig <- plot_ly()

for (grp in c("Very High", "High", "Medium", "Low")) {

  grp_data   <- hdi_interactive %>% filter(group_short == grp)
  grp_colour <- group_cols[[grp]]

  for (ctry in unique(grp_data$country)) {

    ctry_data <- grp_data %>% filter(country == ctry)

    # Show legend entry only for the first country in each group;
    # all others share the same legendgroup so they toggle together.
    show_legend <- (ctry == unique(grp_data$country)[1])

    fig <- fig %>%
      add_trace(
        data        = ctry_data,
        x           = ~year,
        y           = ~hdi,
        type        = "scatter",
        mode        = "lines+markers",
        name        = grp,                  # Legend label
        legendgroup = grp,                  # Groups traces for toggle
        showlegend  = show_legend,
        line        = list(color = grp_colour, width = 1.2),
        marker      = list(color = grp_colour, size = 4),
        text        = ~paste0(
          "<b>", country, "</b><br>",
          "Year: ", year, "<br>",
          "HDI: ", round(hdi, 3)
        ),
        hoverinfo   = "text"
      )
  }
}

fig <- fig %>%
  layout(
    title = list(
      text = paste0(
        "<b>Human Development Index Trends by Country, 1990\u20132023</b><br>",
        "<sup>Hover over a line to identify the country. ",
        "Click a legend group to hide/show it.</sup>"
      ),
      x    = 0.5,
      xanchor = "center",
      font = list(size = 15)
    ),
    xaxis = list(
      title    = "Year",
      tickvals = list(1990, 2000, 2010, 2015, 2020, 2023),
      showgrid = TRUE,
      gridcolor = "#eeeeee",
      zeroline = FALSE
    ),
    yaxis = list(
      title    = "HDI Value",
      range    = list(0.18, 1.02),
      showgrid = TRUE,
      gridcolor = "#eeeeee",
      zeroline = FALSE
    ),
    legend = list(
      title       = list(text = "<b>HDI Group</b>"),
      x           = 0.01,
      y           = 0.99,
      bgcolor     = "rgba(255,255,255,0.85)",
      bordercolor = "#cccccc",
      borderwidth = 1
    ),
    hovermode     = "closest",
    plot_bgcolor  = "#fafafa",
    paper_bgcolor = "#ffffff",
    font          = list(family = "Arial, sans-serif", size = 12),
    annotations   = list(
      list(
        text      = paste0(
          "Source: UNDP Human Development Report Office (2025). ",
          "Statistical Annex Table 2."
        ),
        x = 0, y = -0.11,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        font      = list(size = 9.5, color = "#888888")
      )
    ),
    margin = list(t = 100, b = 80, l = 60, r = 30)
  )

# Save as a self-contained HTML file that can be opened in any browser.
# Upload this file to GitHub and paste the link into your Word document.
saveWidget(fig, "figure3_interactive.html", selfcontained = TRUE)
cat("Figure 3 (interactive) saved as figure3_interactive.html.\n")

cat("\n--------------------------------------------------\n")
cat("All three figures created successfully!\n")
cat("Files in your working directory:\n")
cat("  figure1_time_series.png\n")
cat("  figure2_map.png\n")
cat("  figure3_interactive.html  <- upload this to GitHub\n")
cat("--------------------------------------------------\n")
