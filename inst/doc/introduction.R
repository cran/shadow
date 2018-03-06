## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  dev = "svg",
  fig.align = "center",
  collapse = TRUE, 
  comment = "#>"
  )
set.seed(1014)
# options(dplyr.print_max = 10)

## ----trigdrawing, fig.cap="Shade height calculation", out.width="400px", echo=FALSE----
knitr::include_graphics("shade_height.svg")

## ---- message=FALSE------------------------------------------------------
library(shadow)
library(raster)
library(rgeos)

## ----buildings, fig.cap="Sample buildings and heights"-------------------
plot(rishon)
text(gCentroid(rishon, byid = TRUE), rishon$BLDG_HT)

## ------------------------------------------------------------------------
location = gCentroid(rishon)
time = as.POSIXct("2004-12-24 13:30:00", tz = "Asia/Jerusalem")
location_geo = spTransform(location, "+proj=longlat +datum=WGS84")
solar_pos = maptools::solarpos(location_geo, time)
solar_pos

## ------------------------------------------------------------------------
h = shadowHeight(
  location = location, 
  obstacles = rishon, 
  obstacles_height_field = "BLDG_HT", 
  solar_pos = solar_pos
  )
h

## ----ray, fig.cap="Shade height at a single location", message=FALSE-----
sun = shadow:::.sunLocation(
  location = location, 
  sun_az = solar_pos[1, 1], 
  sun_elev = solar_pos[1, 2]
  )
sun_ray = ray(from = location, to = sun)
build_outline = as(rishon, "SpatialLinesDataFrame")
inter = gIntersection(build_outline, sun_ray)
plot(rishon)
text(gCentroid(rishon, byid = TRUE), rishon$BLDG_HT)
plot(location, add = TRUE)
text(location, round(h, 2), pos = 3)
plot(sun_ray, add = TRUE, col = "yellow")
plot(inter, add = TRUE, col = "red")

## ------------------------------------------------------------------------
ext = as(extent(rishon) + 50, "SpatialPolygons")
r = raster(ext, res = 2)
proj4string(r) = proj4string(rishon)

## ------------------------------------------------------------------------
r = shadowHeight(
  location = r, 
  obstacles = rishon, 
  obstacles_height_field = "BLDG_HT", 
  solar_pos = solar_pos
  )

## ----heightresult, dev="png", fig.cap="Shade height (m) grid", fig.height=5.5----
plot(r, col = grey(seq(0.9, 0.2, -0.01)))
contour(r, add = TRUE)
plot(rishon, add = TRUE, border = "red")
text(gCentroid(rishon, byid = TRUE), rishon$BLDG_HT)

## ----park, fig.cap="Park location"---------------------------------------
park_location = raster::shift(location, y = 20, x = -8)
park = rgeos::gBuffer(park_location, width = 12)
plot(rishon)
text(gCentroid(rishon, byid = TRUE), rishon$BLDG_HT)
plot(park, col = "lightgreen", add = TRUE)

## ---- echo=FALSE---------------------------------------------------------
time2 = as.POSIXct("2004-06-24 09:30:00", tz = "Asia/Jerusalem")
solar_pos2 = maptools::solarpos(location_geo, time2)
footprint = shadowFootprint(
  obstacles = rishon, 
  obstacles_height_field = "BLDG_HT", 
  solar_pos = solar_pos2
  )
park_shade = gIntersection(park, footprint)
shade_prop = gArea(park_shade) / gArea(park)

## ------------------------------------------------------------------------
time2 = as.POSIXct("2004-06-24 09:30:00", tz = "Asia/Jerusalem")
solar_pos2 = maptools::solarpos(location_geo, time2)
solar_pos2
footprint = shadowFootprint(
  obstacles = rishon, 
  obstacles_height_field = "BLDG_HT", 
  solar_pos = solar_pos2
  )
park_shade = gIntersection(park, footprint)
shade_prop = gArea(park_shade) / gArea(park)
shade_prop

## ----footprint, fig.cap="Shade footprint at 2004-06-24 09:30:00"---------
plot(footprint,  col = adjustcolor("lightgrey", alpha.f = 0.5))
plot(rishon, col = "darkgrey", add = TRUE)
plot(park, col = "lightgreen", add = TRUE)
plot(park_shade, col = adjustcolor("darkgreen", alpha.f = 0.5), add = TRUE)
text(gCentroid(park_shade), round(shade_prop, 2))

## ------------------------------------------------------------------------
time_seq = seq(
  from = as.POSIXct("2004-06-24 03:30:00", tz = "Asia/Jerusalem"),
  to = as.POSIXct("2004-06-24 22:30:00", tz = "Asia/Jerusalem"),
  by = "1 hour"
)
solar_pos_seq = maptools::solarpos(location_geo, time_seq)
solar_pos_seq

## ------------------------------------------------------------------------
shade_props = rep(NA, nrow(solar_pos_seq))
for(i in 1:nrow(solar_pos_seq)) {
  if(solar_pos_seq[i, 2] < 0)
    shade_props[i] = 1 else {
      footprint = 
        shadowFootprint(
          obstacles = rishon, 
          obstacles_height_field = "BLDG_HT", 
          solar_pos = solar_pos_seq[i, , drop = FALSE]
          )
    park_shade = gIntersection(park, footprint)
    if(is.null(park_shade))
      shade_props[i] = 0
    else
      shade_props[i] = gArea(park_shade) / gArea(park)
    }
}

## ----timeseries, fig.cap="Shaded park proportion on 2004-06-24"----------
plot(
  time_seq, 
  shade_props, 
  xlab = "Time", 
  ylab = "Shade proportion", 
  type = "b"
  )
text(
  x = time_seq[7], y = shade_props[7], 
  label = round(shade_props[7], 2), 
  pos = 4, col = "red"
  )

