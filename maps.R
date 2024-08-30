options(scipen = 999) #so numbers dont display as scientfic notation

# Load required libraries
library(tidyverse);
library(lubridate) %>% suppressMessages() %>% suppressWarnings();library(ggrepel); library(httr) %>% suppressMessages() %>% suppressWarnings();
library(sf); library(rworldmap); library(readxl);library(ggthemes)


readxl::read_xlsx('filepaths.xlsx') %>% 
  dplyr::filter(name %in% 'working') %>% pull(2) ->working

readxl::read_xlsx(glue::glue('{working}/keys/tokens.xlsx') ) %>% 
  filter(Program %in% 'acled') %>%
  pull(key)->acledtoken

readxl::read_xlsx(glue::glue('{working}/keys/tokens.xlsx') ) %>% 
  filter(Program %in% 'email') %>%
  pull(key)->acledemail



rworldmap::getMap() %>% 
  sf::st_as_sf() %>% 
  dplyr::filter(continent %in% "Africa")->africabound

sf::st_crs(africabound)->crs

rworldmap::getMap() %>% 
  sf::st_as_sf() %>% 
  dplyr::filter(NAME %in% "Sudan") %>% 
  sf::st_transform(crs)->sudanbound


country = 'Sudan'
startdate = format(lubridate::as_date(x = "1500-01-01"), "%Y-%m-%d")
enddate = format(lubridate::today() , "%Y-%m-%d")

# glue::glue(
#   'https://api.acleddata.com/acled/read.csv?key={acledtoken}&email={acledemail}&country={country}&event_date={startdate}|{enddate}&event_date_where=BETWEEN&limit=1000000
# '
# ) %>%
#   #head %>%
#   httr::GET(.) %>%
#   content(., as =  "parsed")  %>%
#   mutate(
#     event_type = as.factor(event_type),
#     event_date = as.Date(event_date),
#     location = as.factor(location)
#   ) -> acled_data
# acled_data %>% saveRDS('acled_data.RDS')
read_rds("acled_data.RDS")->acled_data
acled_data %>%
  filter(event_date >= "2019-01-01") %>%
  mutate(
    actor = case_when(
      actor1 %in% c("Military Forces of Sudan (2019-) Military Intelligence Service" ,"Military Forces of Sudan (2019-)")~ "Military Forces of Sudan",TRUE ~ actor1)
  )  %>%
  filter(actor %in%
           c("Military Forces of Sudan", "Rapid Support Forces")) %>% 
  mutate(    actor = as.factor(actor))-> acled


CRSsp <- "+proj=longlat +datum=WGS84"
useCRS <- "EPSG:4326"



# as points ---------------------------------------------------------------



acled %>%   
  mutate(long = longitude,                 
          lat = latitude ) %>% 
  st_as_sf(coords = c("long", "lat")) %>%
  st_set_crs(CRSsp) %>%
  st_transform(useCRS) ->eventpoints



# Load administrative boundaries for Sudan
sf::read_sf("/Users/rashaelnimeiry/OneDrive - Johns Hopkins University/Arc GIS R Files/Malaria Sudan/sdn_admbndna_adm1_imwg_11302015/sdn_admbndna_adm1_imwg_11302015.shp") %>%
  mutate(admin1 = admin1Name) %>% 
  st_transform(st_crs(CRSsp)) -> sudan_boundaries 

sudan_boundaries %>% 
  st_as_sf( as(., "Spatial")) %>% 
  st_transform(st_crs(useCRS))->boundarypolygon


  
st_join(x= eventpoints, y = boundarypolygon, join= st_within)  %>% 
  st_transform(st_crs(CRSsp)) %>% 
  mutate(admin1Name = as_factor(admin1Name)
         )->events #(!!)#remove duplicated columns
  





# Prepare ACLED data
acled %>%
  filter(disorder_type %in% "Political violence") %>%
  select(event_date, fatalities, admin1, actor, latitude, longitude) %>%
  arrange(event_date) %>%
  group_by(event_date, admin1, actor) %>%
  summarise(fatalities = sum(fatalities, na.rm = TRUE), .groups = 'keep') %>%
  mutate(
    month_year = format(event_date, "%b %Y"),  # New column for month and year
    month = month(event_date)) %>%
  filter(event_date > as.Date("2023-04-01")) %>%
  ungroup() %>% 
  complete(admin1 = unique(sudan_boundaries$admin1), fill = list(fatalities = 0))->actor_fatalities






named_group_split <- function(.tbl, ...) {
  grouped <- group_by(.tbl, ...)
  names <- rlang::inject(paste(!!!group_keys(grouped), sep = " / "))
  
  grouped %>% 
    group_split() %>% 
    rlang::set_names(names)
}



plotmaps <- function(x) {
  
  ggplot()+
    geom_sf(data = sudan_boundaries,
            mapping = aes(
                           color = "grey78", alpha=.15 ))+ 
    geom_sf_label(
      data = sudan_boundaries %>% filter(admin1Name %in% x[["admin1Name"]]),
      mapping = aes(label =admin1Name),nudge_x = 1, vjust = .9,fill = NA,size = 11.5, 
      nudge_y = 4,label.size = 0,colour = "black")+
    
    geom_sf(data = x,
              mapping = aes(fill= NULL, color="#d2a932", alpha=.25, linewidth= .0015))+  
    # rcartocolor::scale_fill_carto_c(palette = "Peach")+
    geom_point(data =events %>% filter(admin1Name %in% x[["admin1Name"]]),
               mapping = aes(x = longitude, y=latitude, color ="#d73027"
               ))+
    scale_color_manual(values = c("grey28", "#d73027", "grey78","#d2a932" ))+
    # theme_wsj(
    #   base_size = 12,
    #   color = "brown",
    #   base_family = "sans",
    #   title_family = "mono"
    # ) +
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
      # panel.background = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      axis.ticks.y = element_blank(), 
      axis.line.y = element_blank(),
      # axis.line.y = element_blank()
      # strip.text = element_blank(),
      # panel.spacing = unit(-8, "lines"),
      # plot.margin = margin(-40, 50, 10, 50),
      plot.background = element_rect(fill = NA, color = NA),
      # legend.text =  element_blank(),
      # legend.key =  element_blank(),
      # legend.background = element_blank(),
      legend.position = "none",
      panel.background = element_rect(fill = NA, color = NA)) 
    # sf::st_combine(.)

  }



sp::merge(
  x = sudan_boundaries,
                    y = events %>% st_drop_geometry(),
                    by = "admin1Name", all.x = TRUE) %>% 
  group_by(admin1Name, .drop=F) %>% 
  mutate(fatalitiestotal = sum(fatalities),
         eventstotal= sum(n()),
         namegroup = admin1Name) %>% 
    ungroup %>% 
  select(admin1Name,namegroup, fatalitiestotal, eventstotal ) %>% 
  unique %>% 
  head(5) %>% 
  group_by(namegroup, .drop=F) %>%
  named_group_split(namegroup) %>% str
  purrr::map(.x= .,
    .f= safely(plotmaps))#%>% 
  map("result")#
  
  

# data by admin region ----------------------------------------------------

  # library(cowplot)
  library(ggtext)
  library(tibbletime)
  library(extrafont)
  library(patchwork)
  
  # Load fonts (assuming this function call is correct)
  loadfonts(device = "pdf")
  
  # Define the rolling sum function for a 7-day window
  rolling_sum <- tibbletime::rollify(.f = sum, window = 7)
  
  # Calculate the data for the heatmap
  acled %>%
    filter(disorder_type %in% "Political violence") %>%
    dplyr::select(event_date, fatalities, admin1, actor, notes) %>% 
    arrange(event_date) %>%
    group_by(event_date, admin1, actor) %>%
    summarise(fatalities = sum(fatalities, na.rm = TRUE), .groups = 'drop') %>%
    complete(event_date = seq(min(event_date), max(event_date), by = "day"), admin1, actor) %>% 
    filter(event_date > as.Date("2023-04-01")) %>% 
    mutate(
      month_year = format(event_date, "%b %Y"),  # New column for month and year
      month = month(event_date)) %>% 
    mutate(month_year = factor(month_year, levels = unique(month_year)))-> actor_fatalities  
 
  
  # Define the time range for plotting
  end.date <- max(actor_fatalities$event_date, na.rm = TRUE)
  start.date <- as.Date("2023-04-01")
  recentrange <- c(start.date , end.date + days(1))
  

  
  # Plotting
  actor_fatalities %>% ungroup %>% 
    mutate(namegroup = admin1 ) %>% 
  group_by(namegroup, .drop=F) %>%
  named_group_split(namegroup) %>% 
    head (2)  %>%  
    purrr::map(.x= .,
               .f= safely(plottimeseries))#%>% 
                 

plottimeseries <- function(x) {
  
                 ggplot(data =x, aes(x = event_date, y = admin1, color = actor, size = fatalities)) +
    geom_point(data = x, alpha = 0.8) +
    scale_size_continuous(range = c(1, 10), name = "Number of Fatalities") +
    scale_color_manual(values = c("#a6611a", "#dfc27d", "#80cdc1", "#018571", "#5e3c99"), 
                       name = "Fatalities Enacted by") +  # Change legend title
    theme_minimal() +
    geom_vline(xintercept = end.date, show.legend = FALSE, lty = 2, lwd = 0.5, alpha = 0.8, color = "darkgrey") +
    labs(
      # title = NULL,
      subtitle = glue::glue("{x %>% select(namegroup) %>% unique}: Number of Fatalities by Actor and State over Time"),
      x = "Date",
      y = "Location",
      caption = glue::glue("Generated by geo:truth team at {Sys.time()} \n Data source: ACLED")) +
    scale_x_date(date_breaks = "weeks", date_labels = "%d", expand = c(0.01, 0.01)) +
    scale_y_discrete(position = "right") +
    guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
    facet_wrap(~month_year, scales = "free_x", nrow = 1, ncol = 15, strip.position = "top") +
    theme(
      plot.title = element_text(color = "#d73027", size = 80, family = "Roboto Condensed", face = "bold.italic"),
      plot.subtitle = element_text(color = "darkgrey", size = 30),
      text = element_text(size = 12, family = "Roboto Condensed"),
      axis.title.y = element_blank(),
      axis.title.x.bottom = element_blank(),
      plot.caption = element_text(size = 10, color = "darkgrey", hjust = 1),
      legend.text = element_text(size = 15),  # Increase legend text size
      axis.text.x = element_text(angle = 0, size = 10, vjust = 0.5, hjust = 1),
      axis.ticks.x = element_line(color = "black", size = 0.5),  # Darker ticks on x-axis
      legend.position = "bottom",
      legend.key.height = unit(2, "lines"),  # Adjust legend key height
      # strip.text.x = element_text(size = 12, color = "black")
    )#->plot_fatalities_over_time 
}
  
  # Calculate total fatalities by actor
  actor_fatalities_summary <- acled %>%
    filter(disorder_type %in% "Political violence" & event_date >= as.Date("2023-04-01")) %>%
    group_by(actor) %>%
    summarise(total_fatalities = sum(fatalities, na.rm = TRUE)) %>%
    mutate(percent_fatalities = (total_fatalities / sum(total_fatalities)) * 100)
  
  # Plotting the lollipop chart
  ggplot(actor_fatalities_summary, aes(x = percent_fatalities, y = reorder(actor, percent_fatalities), color = actor)) +
    geom_segment(aes(x = 0, xend = percent_fatalities, y = actor, yend = actor), size = 4.3, alpha = 0.65, linewidth = 1.7) +  # Adjust height parameter
    geom_point(size = 20,alpha = .65) +
    geom_text(aes(label = actor, color= actor), nudge_x = -4, nudge_y = 0.5, family = "Roboto Condensed", size = 8) +
    geom_text(aes(label = paste0(round(percent_fatalities), "%")), nudge_x = -1.8, hjust = 0, color = "black", fontface="bold", family = "Roboto Condensed", size = 8) +
    scale_color_manual(values = c("#a6611a", "#dfc27d", "#80cdc1", "#018571", "#5e3c99")) +
    scale_y_discrete(position = "right",expand = expansion(mult = 1)) + #space between line segments
    scale_x_continuous( limits = c(0, 100)) +
    # scale_y_discrete(position = "right") +
    labs(title = NULL,
         subtitle = "Percentage of Total Fatalities by Actor in Sudan Conflict Since April 2023") +
    theme_minimal() +
    
    theme(
      plot.title = element_text(color = "#d73027", size = 80, family = "Roboto Condensed", face = "bold.italic"),
      plot.subtitle = element_text(color = "darkgrey", size = 30),
      text = element_text(size = 12, family = "Roboto Condensed"),
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x  = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid   = element_blank(),
      panel.spacing.y = unit(1, units = "in"), 
      plot.margin = unit(c(t=0, r=0, b=0, l=-.5), "in"),  # Add left margin to shift the plot content to the left
    ) +ggtitle("Telling a War Story | Sudan.")-> plot_percent_fatalities
  
  
  # Combine plots
  plot_percent_fatalities + plot_fatalities_over_time+
    plot_layout(widths = c(5,10),
                ncol = 1, 
                heights = c(4, 25)# Adjust heights to make the lollipop chart bigger
    )  ->combined_plot 
  
  # Save the combined plot
  ggsave("plots/combined_plot.png", combined_plot, width = 21, height = 27, units = "in", limitsize = FALSE)
  

  



# scratch -----------------------------------------------------------------

ggplot(data = mtcars, aes(x = hp, y = mpg, color = factor(cyl))) +
  geom_point() +
  scale_color_manual(values = c("#FF0000", "#00FF00", "#0000FF"))

ggplot(mtcars)+
  geom_bar(mapping = aes(x = cyl))+
  theme_wsj(
    base_size = 12,
    color = "brown",
    base_family = "sans",
    title_family = "mono"
  )


purrr::map(.x = listofadmins, .f = ~ggplot()+
             geom_sf(data = listofadmins$data[[".x"]],
                     mapping = aes(fill=fillcolour)))

listofadmins$data[[1]]
ggplot()+
  geom_sf(data = data$data[[1]],
          mapping = aes(fill=fillcolour))
#
#list of admins
sp::merge(x = sudan_boundaries,
          y = actor_fatalities,
          by = "admin1", all.x = TRUE) %>% 
  select(admin1Name, admin1RefN) %>% 
  group_by(admin1RefN, .drop = F) %>% 
  nest(.names_sep = "admin") %>% 
  pull("admin1RefN")->listofadmins

"grey88" ->fillcolour