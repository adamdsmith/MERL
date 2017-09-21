pacman::p_load(dplyr, dbplyr, ggplot2, ggmap, ggforce, viridis, lubridate)

### Extract relevant detection data and join with active receivers
db <- list.files("./Data/", pattern = "*.motus$", full.names = TRUE)

merl <- src_sqlite(db) %>% tbl("alltags") %>%
  select(tagProj, tagProjID, fullID, id, motusTagID, ts, sig, runLen, 
         freqsd, tagModel, spEN, depLat, depLon, recv, site, 
         lat, lon, ant, antType, antBearing) %>%
  collect() %>%
  filter(runLen > 2, freqsd < 0.1) %>%
  mutate(ts = as.POSIXct(ts, origin = "1970-01-01", tz = "GMT")) %>%
  filter(!is.na(lat),
         !is.na(site))
attr(merl$ts, "tzone") <- "America/New_York"

### Get deployment data
merl_dep <- read.csv("./Data/MotusTagsTemplate-45.csv",
                     stringsAsFactors = FALSE) %>%
  mutate(depDate = ymd_hm(
    paste(paste(utcYearStart, utcMonthStart, utcDayStart, sep = "-"), 
          paste(utcHourStart, utcMinuteStart, sep = ":")))) %>%
  select(motusTagID = tagID, depDate)
merl <- left_join(merl, merl_dep, by = "motusTagID")

merl_plot <- merl %>% 
  select(site, lat, lon, ts, spEN, id, depDate, depLat, depLon) %>% 
  group_by(id, spEN, depDate) %>%
  unique() %>% group_by(site, add = TRUE) %>%
  summarise(#date_sort = min(ts),
    since_rel = difftime(min(ts), min(depDate), units = "days"),
    dates = ifelse(identical(min(ts), max(ts)),
                   format(min(ts), format = "%d %b"), 
                   ifelse(identical(month(min(ts)), month(max(ts))),
                          paste(format(min(ts), format = "%d"), 
                                format(max(ts), format = "%d %b"), sep = "-"),
                          paste(format(min(ts), format = "%d %b"), 
                                format(max(ts), format = "%d %b"), sep = "-"))),
    lat = mean(lat), lon = mean(lon)) %>%
  as.data.frame()

merl_plot <- merl_plot %>% 
  arrange(id, since_rel) %>%
  group_by(id, spEN) %>%
  mutate(label = paste(year(depDate), paste("MERL", id), sep = ": "),
         lat_prev = Hmisc::Lag(lat),
         lon_prev = Hmisc::Lag(lon))

merl_bb <- with(merl_plot, make_bbox(c(min(lon), max(lon)),
                                     c(min(lat), max(lat))))
merl_map <- get_googlemap(center = c(mean(merl_bb[c(1,3)]),
                                     mean(merl_bb[c(2,4)])),
                          zoom = 3, maptype = "terrain", 
                          color = "bw")

# Calculate the number of pages with 5 panels per page
n_pages <- n_distinct(merl_plot$id)

pdf_out <- "C:/Users/adsmith/Desktop/BI_Merlin.pdf"
pdf(file = pdf_out, 
    height = 10.5, width = 8, paper = "letter")

for (i in seq_len(n_pages)) {
  
  p <- ggmap(merl_map, maprange = FALSE, extent = "device",
             base_layer = ggplot(aes(x = lon, y = lat), data = merl_plot)) + 
    geom_point(aes(color = since_rel)) +
    geom_segment(aes(x = lon_prev, xend = lon, 
                     y = lat_prev, yend = lat), color = "black",
                 arrow = arrow(type="closed", length = unit(0.1, "cm")), size = 2) +
    geom_segment(aes(x = lon_prev, xend = lon, 
                     y = lat_prev, yend = lat, color = since_rel),
                 arrow = arrow(type="closed", length = unit(0.1, "cm")), size = 1.5) +
    xlab("Longitude") + ylab("Latitude") +
    scale_color_viridis("Days since release:") +
    facet_wrap_paginate(~label, ncol = 1, nrow = 1, page = i) +
    theme_classic() +
    theme(legend.position = "top",
          legend.direction = "horizontal")
  print(p)
}

dev.off()

```
