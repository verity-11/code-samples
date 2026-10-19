# ==============================================================================
# Israeli and Russian currency compositions of international debt securities
# Source: BIS International Debt Securities Statistics
# Author: Verity Warn
# Last edit: 15 Jan 2025 (rescale Israel plot axis)
# ==============================================================================

# To run: change working directory as appropriate
setwd("/Users/veritywarn/Library/CloudStorage/ProtonDrive-verity.warn@protonmail.com-folder/School/3. EPOG/Paris/Variations and transformations of capitalism")

# 1. Load packages -------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(zoo)
library(scales)

# 2. Read BIS data through API -------------------------------------------------

urls <- c("https://stats.bis.org/api/v2/data/dataflow/BIS/WS_DEBT_SEC2_PUB/1.0/Q.IL.3P.1.1.C.A.F.EU1+TO1+USD.K.A.A.A.A.I?format=csv", 
          "https://stats.bis.org/api/v2/data/dataflow/BIS/WS_DEBT_SEC2_PUB/1.0/Q.RU.3P.1.1.C.A.F.EU1+TO1+USD.K.A.A.A.A.I?format=csv")

data = list()

for (i in 1:length(urls)) {
  data[[i]] <- read.csv(urls[i])
}

data_il <- data[[1]]
data_ru <- data[[2]]


# 3. Select & clean components need for plot -----------------------------------
data_il_core <- data_il %>%
  dplyr::select(ISSUE_CUR,TIME_PERIOD,OBS_VALUE) 
data_ru_core <- data_ru %>%
  dplyr::select(ISSUE_CUR,TIME_PERIOD,OBS_VALUE) 


# 4. Reshape wide and compute currency shares ----------------------------------

data_il_wide <- data_il_core %>%
  pivot_wider(
    names_from = ISSUE_CUR,
    values_from = OBS_VALUE) %>%
  rename(
    usd   = USD,
    eur   = EU1,
    total = TO1,
    date  = TIME_PERIOD
  ) %>%
  mutate(
    usd_share = usd / total,
    eur_share = eur / total,
    date = zoo::as.yearqtr(date, format = "%Y-Q%q")
  ) %>%
  filter(date >= "2000 Q1") # EUR data starts 1999 Q1, this just makes plot nicer


data_ru_wide <- data_ru_core %>%
  pivot_wider(
    names_from = ISSUE_CUR,
    values_from = OBS_VALUE) %>%
  rename(
    usd   = USD,
    eur   = EU1,
    total = TO1,
    date  = TIME_PERIOD
  ) %>%
  mutate(
    usd_share = usd / total,
    eur_share = eur / total,
    date = zoo::as.yearqtr(date, format = "%Y-Q%q")
  ) %>%
  filter(date >= "2000 Q1") # consistent with Israel plot


# 5. Labels for key events -----------------------------------------------------
events_il <- tibble::tibble(
  date = as.yearqtr(c(
    "2000 Q3",
    "2005 Q3",
    "2008 Q4",
    "2014 Q3",
    "2021 Q2",
    "2023 Q3"
  )),
  label = c(
    "Second Intifada",
    "BDS founded",
    "2008 Gaza war",
    "2014 Gaza war",
    "Unity Intifada",
    "Genocide"
  )
)

events_ru <- tibble::tibble(
  date = as.yearqtr(c(
    "2014 Q1",
    "2022 Q1"
  )),
  label = c(
    "Annexation of Crimea",
    "Invasion of Ukraine"
  )
)


# 6. Plot stylized fact --------------------------------------------------------

# Colours
usd_col <- "#020254"
eur_col <- "#2221a7"  
usd_lvl_col <- "#F2B705"   
eur_lvl_col <- "#FBC932" 

# Start and end q1 (for labels)
first_q1 <- zoo::as.yearqtr("2000-Q1", format = "%Y-Q%q")
last_q1 <- zoo::as.yearqtr("2025-Q1", format = "%Y-Q%q")


# Combo plot (Israel) ----

# Scale factor: map max USD level to 100%
max_total <- max(data_ru_wide$total, na.rm = TRUE) # Set this max to the one for russia plot so they match

ggplot(data_il_wide, aes(x = date)) +
  
  # USD area (background)
  geom_area(
    aes(y = usd_share, fill = "USD"),
    color = usd_col,
    linewidth = 0.9
  ) +
  
  # EUR area (foreground)
  geom_area(
    data = data_il_wide,
    aes(y = eur_share, fill = "EUR"),
    color = eur_col,
    linewidth = 0.9
  ) +
  
  # USD level (rescaled)
  geom_line(
    aes(y = usd / max_total, color = "USD (level)"),
    linewidth = 0.8,
    linetype = "dotdash"
  ) +
  
  # EUR level (rescaled)
  geom_line(
    aes(y = eur / max_total, color = "EUR (level)"),
    linewidth = 0.8,
    linetype = "dotdash"
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.2),
    labels = percent_format(accuracy = 1),
    expand = c(0, 0),
    sec.axis = sec_axis(
      ~ . * max_total / 1000,
      name = "Outstanding amount (USD bn)"
    )
  ) +

  coord_cartesian(ylim = c(0, 1), clip = "off") +

  scale_color_manual(
    breaks = c(
      "USD (level)",
      "EUR (level)"
    ),
    values = c(
      "USD (level)"  = usd_lvl_col,
      "EUR (level)"  = eur_lvl_col
    ),
    name = "Absolute value"
  ) + 
  scale_fill_manual(
    breaks = c(
      "USD",
      "EUR"
    ),
    values = c(
      "USD" = usd_col,
      "EUR" = eur_col
    ),
    name = "Percentage"
  ) +
  
  labs(
    title = "Currency Denominations of Israel's International Securities",
    subtitle = "Areas show issue currency shares of total outstanding debt securities; dashed lines show levels (right axis)",
    x = NULL,
    y = "Share of total outstanding"
  ) +
  
  scale_x_yearqtr(
    breaks = seq(from = first_q1, to = last_q1, by = 2),
    format = "%Y Q%q",
    expand = expansion(mult = c(0, 0.))
  ) +
  
  ## Theme
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(t = 25, r = 15, b = 10, l = 10),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#E5E5E5"),
    
    # Plot titles
    plot.title = element_text(
      face = "bold",
      size = 12,
      hjust = 0
    ),
    plot.subtitle = element_text(
      size = 10,
      color = "grey30"
    ),
    
    
    # Axis titles
    axis.title.x = element_text(
      size = 10,
      color = "grey30",
      margin = margin(t = 8)
    ),
    axis.title.y = element_text(
      size = 10,
      color = "grey30",
      margin = margin(r = 8)
    ),
    
    # Axis tick labels
    axis.text.x = element_text(
      size = 9,
      color = "grey30",
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    axis.text.y = element_text(
      size = 9,
      color = "grey30"
    ),
    
    # Legend
    legend.title = element_text(
      size = 10
    ),
    legend.text = element_text(
      size = 9
    )
    
    
  ) +

# Add events & labels
geom_vline(
  data = events_il,
  aes(xintercept = date),
  color = "white",
  linewidth = 0.4,
  linetype = "dotted"
) +

geom_text(
  data = events_il,
  aes(
    x = date,
    y = 0.2,
    label = label
  ),
  angle = 45,
  vjust = 0,
  hjust = 0,
  size = 3.5,
  color = "white"
) 
  
ggsave(
  "israel_plot.pdf",
  plot = last_plot(),
  width = 9,
  height = 4.5,   
  units = "in"
)


# Combo plot (Russia) ----

# Scale factor: map max USD level to 100% ### need to fix this so scale consistent with Israel plot
max_total <- max(data_ru_wide$total, na.rm = TRUE)

ggplot(data_ru_wide, aes(x = date)) +
  
  # USD area (background)
  geom_area(
    aes(y = usd_share, fill = "USD"),
    color = usd_col,
    linewidth = 0.9
  ) +
  
  # EUR area (foreground)
  geom_area(
    data = data_ru_wide,
    aes(y = eur_share, fill = "EUR"),
    color = eur_col,
    linewidth = 0.9
  ) +
  
  # USD level (rescaled)
  geom_line(
    aes(y = usd / max_total, color = "USD (level)"),
    linewidth = 0.8,
    linetype = "dotdash"
  ) +
  
  # EUR level (rescaled)
  geom_line(
    aes(y = eur / max_total, color = "EUR (level)"),
    linewidth = 0.8,
    linetype = "dotdash"
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.2),
    labels = percent_format(accuracy = 1),
    expand = c(0, 0),
    sec.axis = sec_axis(
      ~ . * max_total / 1000,
      name = "Outstanding amount (USD bn)"
    )
  ) +
  
  coord_cartesian(ylim = c(0, 1), clip = "off") +
  
  scale_color_manual(
    breaks = c(
      "USD (level)",
      "EUR (level)"
    ),
    values = c(
      "USD (level)"  = usd_lvl_col,
      "EUR (level)"  = eur_lvl_col
    ),
    name = "Absolute value"
  ) + 
  scale_fill_manual(
    breaks = c(
      "USD",
      "EUR"
    ),
    values = c(
      "USD" = usd_col,
      "EUR" = eur_col
    ),
    name = "Percentage"
  ) +
  
  labs(
    title = "Currency Denominations of Russia's International Securities",
    subtitle = "Areas show issue currency shares of total outstanding debt securities; dashed lines show levels (right axis)",
    x = NULL,
    y = "Share of total outstanding"
  ) +
  
  scale_x_yearqtr(
    breaks = seq(from = first_q1, to = last_q1, by = 2),
    format = "%Y Q%q",
    expand = expansion(mult = c(0, 0.))
  ) +
  
  
  ## Theeeeeme
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(t = 25, r = 15, b = 10, l = 10),
    
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#E5E5E5"),
    
    
    # Plot titles
    plot.title = element_text(
      face = "bold",
      size = 12,
      hjust = 0
    ),
    plot.subtitle = element_text(
      size = 9,
      color = "grey30"
    ),
    
    
    # Axis titles
    axis.title.x = element_text(
      size = 10,
      color = "grey30",
      margin = margin(t = 8)
    ),
    axis.title.y = element_text(
      size = 10,
      color = "grey30",
      margin = margin(r = 8)
    ),
    
    # Axis tick labels
    axis.text.x = element_text(
      size = 9,
      color = "grey30",
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    axis.text.y = element_text(
      size = 9,
      color = "grey30"
    ),
    
    # Legend
    legend.title = element_text(
      size = 10
    ),
    legend.text = element_text(
      size = 9
    )
    

  ) +

# add events & labels
geom_vline(
  data = events_ru,
  aes(xintercept = date),
  color = "white",
  linewidth = 0.4,
  linetype = "dotted"
) +
  
  geom_text(
    data = events_ru,
    aes(
      x = date,
      y = 0.1,
      label = label
    ),
    angle = 45,
    vjust = 0,
    hjust = 0,
    size = 3.5,
    color = "white"
  ) 

ggsave(
  "russia_plot.pdf",
  plot = last_plot(),
  width = 9,
  height = 4.5, 
  units = "in"
)


