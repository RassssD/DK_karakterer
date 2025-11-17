library(tidyverse)
library(ggplot2)
library(ggtext)
library(extrafont)
loadfonts(device = "win")
windowsFonts('Roboto Mono'=windowsFont("Roboto Mono"))

# https://da.wikipedia.org/wiki/Karakter_(bed%C3%B8mmelse)

df <- read_csv("C:/Users/rasmu/Desktop/karakterer.csv", locale = locale(encoding = "UTF-8")) |>
  filter(Start > 1788)

df <- df %>%
  mutate(
    symbol = as.character(Symbol),
    numeral = as.numeric(Numeral),
    label_da = as.character(Label_da),
    label_en = as.character(Label_en),
    numeral_str = as.character(numeral),
    End=as.numeric(End)
  ) %>%
  select(Start, End, Scale, symbol, numeral, numeral_str, label_da, label_en) |>
  mutate(Period = paste0(Start, "-", End)) |>
  mutate(numeral_str = if_else(Start == 2026 & numeral==12, "12*", numeral_str)) |>
  mutate(Period = if_else(Start == 2026, "Forslag", Period)) |>
  mutate(Period = if_else(Start == 2006, "2006-Nutid", Period))

# Data already loaded and cleaned as plot_df
plot_df <- df %>%
  group_by(End) %>%
  arrange(numeral, .by_group = TRUE) %>%
  mutate(grade_order = row_number()) %>%
  ungroup()

# Manually add pre-1805 grades
pre1805 <- tibble(
  Start = 1788,
  End = 1805,
  Scale = "Latin",
  symbol = c("Laudabilis præ ceteris", "Laudabilis", "Haud illaudabilis", "Non contemnendus", "0"),
  numeral = c(12.5, 10, 5 , 1, -1.3),  # arbitrary numeric mapping for plotting
  numeral_str = c("Laudabilis \npræ \nceteris", "Laudabilis", "Haud \nillaudabilis", "Non \ncontemnendus", "0"),
  Period = "1788-1805"
)

plot_df <- bind_rows(plot_df, pre1805) %>%
  group_by(End) %>%
  arrange(numeral, .by_group = TRUE) %>%
  mutate(grade_order = row_number()) %>%
  mutate(grade_order = if_else(Start == 1788, 1, grade_order)) |>
  ungroup()

# Identify min and max per scale
labels_df <- plot_df %>%
  group_by(End) %>%
  filter((numeral == max(numeral) | numeral == min(numeral)) | Start == 1788) %>%
  mutate(vjust_label = if_else(numeral == max(numeral), -0.5, 1.5),
         hjust_label = if_else(numeral == max(numeral), 0.8, -0.05)) |> 
  mutate(vjust_label = if_else(Start == 1788, 0, vjust_label),
         hjust_label = if_else(Start == 1788, 0.5, hjust_label)) |>
  ungroup()

# Compute number of grades per Period
facet_labels <- plot_df %>%
  group_by(Period) %>%
  summarise(n_steps = n()) %>%
  mutate(n_steps = if_else(Period == "Forslag", n_steps+1, n_steps)) |>
  mutate(facet_title = paste0("**", Period, "**", "<br>", n_steps, " trin"))

# Join back to plot_df
plot_df <- plot_df %>%
  left_join(facet_labels %>% select(Period, facet_title), by = "Period")
# Add facet_title to labels_df
labels_df <- labels_df %>%
  left_join(facet_labels %>% select(Period, facet_title), by = "Period")


min_passing_df <- plot_df %>%
  group_by(facet_title) %>%
  summarise(min_passing = case_when(
    Period == "1788-1805" ~ min(numeral[symbol %in% c("Laudabilis præ ceteris", "Laudabilis", "Haud illaudabilis", "Non contemnendus")]) - 1,
    Period %in% c("1805-1845", "1845-1871") ~ 5,
    Period == "1871-1903" ~ 3.67,
    Period == "1903-1911" ~ 4,
    Period == "1911-1919" ~ 2,
    Period == "1919-1943" ~ 5,
    Period == "1943-1963" ~ 0,
    Period == "1963-2006" ~ 6,
    Period == "2006-Nutid" ~ 2,
    Period == "Forslag" ~2,
    TRUE ~ NA_real_
  )) %>%
  slice(1)  # ensure only one row per facet_title

p = ggplot(plot_df) +
  geom_step(data=plot_df %>% filter(Period != "1788-1805"), aes(x = grade_order, y = numeral, group = as.character(Period), colour = as.character(Period)),
            direction = "hv", size = 1.2) +
  geom_hline(yintercept = 0, linetype = "solid", colour = "gray50", size=1) +
  geom_hline(data = min_passing_df, aes(yintercept = min_passing),
             linetype = "dashed", colour = "black", size = 1) +
  geom_text(data = labels_df |> filter(Start != 1788),
            aes(x = grade_order, y = numeral, label = numeral_str, colour = as.character(Period), vjust = vjust_label, hjust = hjust_label),
            , size = 7) +
  geom_text(data = labels_df |> filter(Start == 1788),
            aes(x = grade_order, y = numeral, label = numeral_str, colour = as.character(Period), vjust = vjust_label, hjust = hjust_label),
            , size = 6) +
  facet_wrap(~facet_title, scales = "free_x", nrow = 1) +
  scale_x_continuous(breaks = plot_df$grade_order) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),   # remove x tick labels
    axis.ticks.x = element_blank(),   # remove x tick marks
    panel.spacing = unit(1.5, "lines"),
    axis.title.y=element_text(size=20),
    axis.text.y=element_text(size=15),
    strip.text.x = element_markdown(size = 15),
    title=element_text(size=15),
    plot.caption=element_text(size=15),
    text=element_text(base_family=windowsFonts()$`Roboto Mono`)
    
  ) +
  labs(
    title = "Fra Laudabilis til 12*: Den danske karakterskala har altid været mærkelig",
    x = "",
    y = "",
    caption = "--- Stiplede linje viser minimum bestået"
  )
p

plot_preview <- function(plot_obj, width_px = 1200, height_px = 600) {
  # Convert pixels to inches (R's windows() uses inches)
  # Assuming 96 DPI as default
  width_in <- width_px / 96
  height_in <- height_px / 96
  
  # Open a new window
  windows(width = width_in, height = height_in)
  
  # Print the plot
  print(plot_obj)
}
#plot_preview(p, width_px = 2400, height_px = 1000)

