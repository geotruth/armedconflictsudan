# libraries ---------------------------------------------------------------

library(ggtext)
library(tidyverse)
library(tibbletime)
library(extrafont)
library(patchwork)

library(ggplot2)
library(glue)
library(scales)
library(ggtext)

# Load fonts (assuming this function call is correct)
loadfonts(device = "pdf")

# data --------------------------------------------------------------------

source("getacleddata.R")


# dates for map-------------------------------------------------------------------

# Define the time range for plotting
enddate <- max(events$event_date, na.rm = TRUE)
startdate <- enddate -lubridate::days(7) 

# datestimeseries -------------------------------------------------------------------


# Define the time range for plotting
start.date <- as.Date("2023-04-01")
recentrange <- c(start.date , enddate + days(1))


# Ensure month_year is ordered correctly
events %>%
  filter(event_date >= as.Date("2023-04-01")) %>%
  tidyr::complete(
    event_date = seq.Date(from = min(event_date), to = max(event_date), by = "day"),
    admin1Name = unique(events$admin1Name),
    fill = list(fatalities = 0)
  ) %>% 
  mutate(
    actor = case_when(
      grepl(pattern = "Military Forces of Sudan|Government of Sudan", x = actor) ~ "Sudanese Armed Forces (SAF)",
      grepl(pattern = "Rapid Support Forces", x = actor) ~ "Rapid Support Forces (RSF)",
      grepl(pattern = "Militia|Group", x = actor) ~ "Another Group or Militia",
      TRUE ~ "Another Group or Militia"
    )
  ) %>%
  mutate(month_year = format(event_date, "%b %Y")) %>% 
  group_by(month_year, admin1Name, actor) %>% 
  summarise(fatalities = sum(fatalities, na.rm = TRUE), .groups = 'keep') %>% 
  group_by(admin1Name, .drop = FALSE) %>% 
  mutate(percent = fatalities * 100 / sum(fatalities)) %>% 
  ungroup() %>%
  mutate(month_year = factor(month_year, levels = unique(month_year[order(as.Date(paste0(month_year, "01"), "%b %Y %d"))]))
         ) -> actor_fatalities


# map plots ---------------------------------------------------------------

# 1. large map of sudan with events ------------------------------------------


events %>% 
  # st_drop_geometry() %>%
  mutate(event_date = as.Date(event_date)) %>% 
  dplyr::filter(event_date >= startdate & event_date <= enddate) %>% 
  mutate(actor1 = case_when(
    actor1 == "Military Forces of Sudan (2019-) Military Intelligence Service" ~ "Sudanese Armed Forces (SAF)",
    actor1 == "Military Forces of Sudan (2019-)" ~ "Sudanese Armed Forces (SAF)",
    actor1 == "Rapid Support Forces" ~ "Rapid Support Forces (RSF)",
    TRUE ~ actor1
  )) %>% 
  mutate(
  actorshort = case_when(
    actor1 %in% "Sudanese Armed Forces (SAF)" ~"SAF", 
    actor1 %in% "Rapid Support Forces (RSF)" ~ "RSF", 
    TRUE ~ "Other"
  )) %>% 
  group_by(admin1Name, actorshort) %>%
  summarise(
    counts = n(),
    .groups = 'keep' 
  ) %>%
  mutate(percent = counts * 100 / sum(counts)) %>% ungroup %>% 
  group_by(admin1Name, .drop=F) %>% 
  mutate(
    totalcounts = sum(counts),
    percent = counts * 100 / totalcounts,
  ) %>%
  ungroup()  %>% 
  mutate(   
    totalpercent=totalcounts*100/sum(totalcounts))->eventsbytypeactorgeo

# > label for totals by admin region ----------------------------------------

custom_palette <- c("#FFE6E0", "#FCCAC0", "#F8A9A1", "#F78C81", "#F8766D")


eventsbytypeactorgeo %>%
  st_drop_geometry() %>%
  pivot_wider(names_from = actorshort, 
              values_from = c(counts, percent),
              names_sep = "") %>%
  ungroup() %>% 
  merge(  sudan_boundaries %>%  
            mutate(centroid = st_centroid(geometry),
                   long = st_coordinates(centroid)[, 1],
                   lat=st_coordinates(centroid)[, 2]+3
                                           ) %>% 
            dplyr::select(long, lat, admin1Name) %>% distinct(admin1Name, .keep_all = T) %>% 
            st_drop_geometry() ,
          all.y = T) %>%   
  bind_rows(
    tibble(
      admin1Name = "RandomAdmin1")) %>% 
  
  
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>% 
  mutate(
    
    total_label = glue::glue("<b>{admin1Name}</b> <br> SAF: ({countsSAF}) {round(percentSAF, 0)}% | RSF: ({countsRSF}) {round(percentRSF, 0)}%"),         
    total_label = case_when(admin1Name == "RandomAdmin1" ~ glue::glue("<b>Admin Area Name</b><br>  Actor Name: (# of events per actor in area) % of events attributed to actor per region
                                                                      <br><br> -SAF: Military Forces of Sudan (2019-)
                                                                      <br> -RSF: Rapid Support Forces
                                                                      <br><br> Calculations include any type of armed conflict event that is within the adminstrative regions of Sudan.
                                                                      <br> Data Source: <b>ACLED</b>"), TRUE~ total_label),
    countsSAF   = case_when(admin1Name == "RandomAdmin1" ~ 0, TRUE ~ countsSAF),
    countsRSF   = case_when(admin1Name == "RandomAdmin1" ~ 0, TRUE ~ countsRSF),
    percentSAF  = case_when(admin1Name == "RandomAdmin1" ~ 0, TRUE ~ percentSAF),
    percentRSF  = case_when(admin1Name == "RandomAdmin1" ~ 0, TRUE ~ percentRSF),
    totalcounts = case_when(admin1Name == "RandomAdmin1" ~ 1, TRUE ~ totalcounts),
    long        = case_when(admin1Name == "RandomAdmin1" ~ 28.41388, TRUE ~ long),
    lat         = case_when(admin1Name == "RandomAdmin1" ~ 9.78632, TRUE ~ lat )) %>% 
  mutate(
    total_label = dplyr::case_when(totalcounts == 0 ~ paste(admin1Name), TRUE ~ (total_label)),
    nudge_x = case_when(
      admin1Name == "South Darfur" ~ 0,       
      admin1Name == "West Darfur" ~ 3,       
      admin1Name == "Central Darfur" ~ 1,       
      # admin1Name == "RandomAdmin1" ~ 7,      
      TRUE ~ 0),
    nudge_y = case_when(
      admin1Name == "South Darfur" ~ 1,
      admin1Name == "River Nile" ~ 1,
      admin1Name == "Red Sea" ~ .5,
      admin1Name == "North Darfur" ~ 3,
      admin1Name == "Northern" ~ .5,
      # admin1Name == "RandomAdmin1" ~ -3,
      TRUE ~ 0
    ),
    segment_x = long,  
    segment_y = lat,   
    end_x = long + nudge_x, 
    end_y = lat + nudge_y
  ) %>% 
  filter(!(admin1Name %in% "RandomAdmin1"))->sudanregion_label




# 2. selected regions maps ---------------------------------------------------






plotmaps <- function(x) {

  ggplot()+
    geom_sf(data = sudan_boundaries, alpha = .15, fill = "grey88") +
    theme_bw() +
    theme(text = element_text(size = 13, family = "RobotoCondensed-Regular")) +
    geom_richtext(
      data = sudanregion_label,
      aes(
        x = ifelse(admin1Name %in% x[["admin1Name"]], no = long, yes = long+nudge_x), 
        y = ifelse(admin1Name %in% x[["admin1Name"]], no = lat, yes = lat+nudge_y), 
        label = admin1Name),
      size = 10,
      na.rm = TRUE,
      fill = ifelse(sudanregion_label$admin1Name %in% x[["admin1Name"]], no = NA, yes = "#333333"),  # Conditional fill
      colour = ifelse(!(sudanregion_label$admin1Name %in% x[["admin1Name"]]), no = "white", yes = NA),  # Conditional fill
      # alpha = .85,
      family = "RobotoCondensed-Regular",
      # Removes background fill around text
      label.color = NA # Removes border around text
    ) + 
    geom_sf(data = x %>% filter(admin1Name %in% x[["admin1Name"]]),
            mapping = aes( alpha=.85, linewidth= .0016), fill = "grey30")+  
    theme(
      strip.background = element_blank(),
      panel.spacing = unit(3, "pt"),
      plot.title = element_text(size = 18, lineheight = 1.1, face = "plain"),
      plot.subtitle = element_text(size = 22),
      plot.caption = element_text(size = 9),
      legend.title =  element_blank(),
      axis.line = element_line(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      axis.ticks.y = element_blank(), 
      axis.line.y = element_blank(),
      plot.background = element_rect(fill = NA, color = NA),
      legend.position = "none",
      panel.background = element_rect(fill = NA, color = NA)) 
  
  
  
}








sp::merge(
  x = sudan_boundaries %>% 
    select(events %>% select_if(!names(.) %in% (names(events))) %>% names, "admin1Name"),
  y = events %>% st_drop_geometry()  ,
  by = "admin1Name", all.x = TRUE) %>% 
  group_by(admin1Name, .drop=F) %>% 
  mutate(fatalitiestotal = sum(fatalities),
         eventstotal= sum(n()),
         namegroup = admin1Name) %>% 
  ungroup %>% 
  filter(!admin1Name=="Abyei PCA Area") %>%
  select(admin1Name,namegroup, fatalitiestotal, eventstotal ) %>% 
  unique %>% 
  arrange(admin1Name) %>%
  mutate(admin1Name = factor(admin1Name, levels = sort(unique(admin1Name)))) %>% 
  group_by(namegroup, .drop=F) %>%
  named_group_split(namegroup) %>% 
  purrr::map(.x= .,
             .f= safely(plotmaps)) %>% 
  map("result")->sudanplots


# times series plots ------------------------------------------------------


plottimeseries <- function(x) {
  ggplot(data = x, aes(x = month_year, y = percent, fill = actor)) +
    
    # Use geom_col for clean, stacked bar visualization
    geom_col(position = "stack", alpha = 0.9, width = 0.4,just = 0) +
    
    # Add text labels for percentages inside the bars
    geom_text(aes(label = ifelse(percent > 1, paste0(round(percent, 0), "%"), "")),  # Only label bars with >5% for clarity
              position = position_stack(vjust = 0.5),  # Center the text within the stacked bars
              size = 6,  # Adjust the text size
              color = "black") +  # Use white text for contrast
    
    # Standardize the y-axis to display percentages (0-100%)
    scale_y_continuous(position = "right",
      limits = c(0, 100),
      breaks = seq(0, 100, by = 25),
      labels = function(x) paste0(x, "%"),
      expand = c(0, 0)
    ) +
    
    # Customize colors in a sleek, minimal palette
    scale_fill_manual(
      name = NULL,  # Remove title for a cleaner look
      values = c("Sudanese Armed Forces (SAF)" = "#FCCAC0", "Rapid Support Forces (RSF)" = "#F8766D", "Another Group or Militia" = "darkgrey"),
      breaks = c("Sudanese Armed Forces (SAF)", "Rapid Support Forces (RSF)", "Another Group or Militia"),
      labels = c("Sudanese Armed Forces (SAF)", "Rapid Support Forces (RSF)", "Another Group or Militia")
    ) +
    
    # Add labels and captions
    labs(
      # title = "% of Fatalities by Actor per State | Sudan",
      x = NULL,  # Remove x-axis label for simplicity
      y = NULL,  # Remove y-axis label
      caption = glue::glue("Data source: ACLED | Generated by geo:truth team at {format(Sys.time(), '%Y-%m-%d')} | Reach us: admin@geotruth.org")
    ) +
    
    # Minimalist theme inspired by World Economic Forum style
    guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
    theme_minimal() +
    theme(
      plot.title = element_markdown(family = "Roboto Condensed", size = 40, face = "plain", hjust = 0.5, margin = margin(b = 15)),
      plot.subtitle = element_text(color = "darkgrey", size = 40),
      text = element_text(size = 35, family = "Roboto Condensed"),
      axis.line.x = element_line(),
      # axis.title.y = element_blank(),
      axis.title.x.bottom = element_blank(),
      plot.caption = element_text(family = "Roboto Condensed", size = 10, color = "darkgrey", hjust = 1),
      legend.text = element_text(family = "Roboto Condensed", size = 35),
      axis.text.x = element_blank(),
      axis.ticks.x = element_line(color = "black", size = 0.5),
      legend.position = "top",
      axis.text.y = element_blank(),
      legend.key.height = unit(2.4, "lines"),
      strip.background.x = element_rect(fill = "grey95", color = "grey95"),  # Grey color for facet strips
      panel.spacing = unit(.1, "lines")  # Add space between facet panels
      
    ) +
    
    facet_wrap(~month_year,dir = 'h', scales = "free_x", nrow = 1, ncol = 20, strip.position = "top") +
    ggtitle(glue::glue("% of Fatalities by Actor per State | <span style='color:#F8766D;'><b>Sudan</span></b>."))
  
    # ggtitle = glue::glue("% of Fatalities by Actor per State <span style='color:#F8766D;'>Sudan</span>")
      
}



    



# Plotting
  sp::merge(
    x = sudan_boundaries %>% st_drop_geometry()%>% select( "admin1Name"),
    y = actor_fatalities  ,
    by = "admin1Name", all.y = TRUE) %>% 
  filter(!admin1Name=="Abyei PCA Area") %>%
  mutate(admin1Name = factor(admin1Name, levels = sort(unique(admin1Name)))) %>% 
  filter(!is.na(admin1Name)) %>% 
    # filter(admin1Name %in% sudan_boundaries$admin1Name) %>%
  mutate(namegroup = admin1Name) %>% 
  group_by(namegroup, .drop=F) %>%
  named_group_split(namegroup) %>% 
  purrr::map(.x= .,
             .f= safely(plottimeseries)
  )->timeseriesplots
timeseriesplots[1]


# putting map and timeseries together -------------------------------------
map2(sudanplots , timeseriesplots,
     \(x,y) (x|y) + plot_layout(widths = c(2, 9))) %>% 
  pluck(names) %>% 
  as.list()->regionnames

map2(
  sudanplots ,
  timeseriesplots ,
  \(x, y) (x | y) + plot_layout(widths = c(2, 9))
) %>% 
  map2(
    regionnames,
    ~ggsave(
      filename = paste0(getwd(), "/plots/", .y, "_combinedplots.pdf"),
      plot = .x,
      width = 49,  # Adjust width
      height = 10, # Adjust height
      device = cairo_pdf
    )
  )




