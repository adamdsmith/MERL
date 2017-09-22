pacman::p_load(dplyr, dbplyr, leaflet, leaflet.extras, 
               htmlwidgets, htmltools, viridis, lubridate)

### Extract relevant detection data and join with active receivers
db <- list.files("./Data/", pattern = "*.motus$", full.names = TRUE)

merl <- src_sqlite(db) %>% tbl("alltags") %>%
  select(tagProj, tagProjID, id, motusTagID, ts, sig, runLen, 
         freqsd, tagModel, spEN, depLat, depLon, recv, site, 
         lat, lon, ant, antType, antBearing) %>%
  collect() %>%
  filter(runLen > 2, freqsd < 0.1) %>%
  mutate(ts = as.POSIXct(ts, origin = "1970-01-01", tz = "GMT"),
         lat = case_when(
           site == "PR3"  ~ 30.4545,
           site == "PR2"  ~ 30.4298,
           site == "CHDE" ~ 38.7703,
           site == "SACH" ~ 41.4787,
           site == "BISE" ~ 41.1532,
           TRUE ~ lat),
         lon = case_when(
           site == "PR3"  ~ -88.5857,
           site == "PR2"  ~ -88.5944,
           site == "CHDE" ~ -75.0852,
           site == "SACH" ~ -71.2438,
           site == "BISE" ~ -71.5527,
           TRUE ~ lon)) %>%
  filter(!is.na(lat),
         !is.na(site))
attr(merl$ts, "tzone") <- "America/New_York"

### Get deployment data
merl_dep <- read.csv("./Data/deployments.csv",
                     stringsAsFactors = FALSE) %>%
  mutate(id = as.character(id),
         depDate = ymd_hm(paste(depDate, depTime)),
         xmtr_mb = round(xmtr_wt/wt * 100, 1)) %>%
  select(id,  sex, age, depDate, att_type, xmtr_mb)
merl <- left_join(merl, merl_dep, by = "id")

merl_det_hist <- merl %>%
  mutate(ts_h = round_date(ts, "hours")) %>%
  group_by(id, recv, site, ts_h) %>%
  arrange(-sig) %>%
  filter(row_number() == 1) %>%
  mutate(since_rel = as.numeric(difftime(min(ts_h), min(depDate), units = "days"))) %>%
  arrange(lat) %>%
  ungroup() %>%
  mutate(site = factor(site, unique(site)),
         label = paste(paste("MERL", id), sex, age, tagModel, sep = ", ")) %>%
  arrange(id, ts_h, lat)

elapsedPal <- colorNumeric(palette = viridis(24), domain = merl_det_hist$since_rel)

# Set up separate overlays/colors by tag id
tags <- unique(merl_det_hist$id)

p <- leaflet() %>%
  # Base map group
  addTiles("http://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}",
           group = "Terrain") %>%
  addTiles("http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
           group = "Aerial")

for (tag in tags) {
  d <- merl_det_hist[merl_det_hist$id == tag, ]
  p <- p %>% addCircleMarkers(data = d, lng = ~lon, lat = ~lat,
                              fillColor = ~elapsedPal(since_rel),
                              fillOpacity = 1, radius = 10,
                              color = "black", weight = 1, opacity = 1,
                              popup = ~paste(paste("MERL:", id),
                                             paste(age, sex),
                                             paste("Tag model:", tagModel),
                                             paste("Site:", site),
                                             paste("First detection:", ts_h),
                                             sep = "<br/>"),
                              group = as.character(tag)) %>%
    hideGroup(tag)
}

p <- p %>% addLayersControl(baseGroups = c("Terrain", "Aerial"),
                       overlayGroups = tags,
                       options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("topleft", pal = elapsedPal, values = merl_det_hist$since_rel,
            title = "Elapsed days",
            opacity = 1) %>%
  moveZoomControl()
  
htmlwidgets::saveWidget(p, file = "MERL_detection_summary.html")
