## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  fig.align = "center",
  collapse = TRUE, 
  comment = "#>"
  )
set.seed(1014)
# options(dplyr.print_max = 10)

## ----trigdrawing, fig.cap="Shade height calculation", out.width = "400px", echo=FALSE----
knitr::include_graphics("shade_height.png")

## ---- message=FALSE------------------------------------------------------
library(shadow)
library(raster)
library(rgeos)

## ----buildings, fig.cap="Sample buildings and heights"-------------------
plot(build)
text(gCentroid(build, byid = TRUE), build$BLDG_HT)

## ------------------------------------------------------------------------
location = gCentroid(build)
time = as.POSIXct("2004-12-24 13:30:00", tz = "Asia/Jerusalem")
location_geo = spTransform(location, "+proj=longlat +datum=WGS84")
solar_pos = maptools::solarpos(location_geo, time)
solar_pos

## ------------------------------------------------------------------------
h = shadeHeight(location, build, "BLDG_HT", solar_pos)
h

## ----ray, fig.cap="Shade height at a single location", message=FALSE-----
sun = shadow:::.sunLocation(
  location = location, 
  sun_az = solar_pos[1, 1], 
  sun_elev = solar_pos[1, 2]
  )
sun_ray = ray(from = location, to = sun)
build_outline = as(build, "SpatialLinesDataFrame")
inter = gIntersection(build_outline, sun_ray)
plot(build)
text(gCentroid(build, byid = TRUE), build$BLDG_HT)
plot(location, add = TRUE)
text(
  location, 
  round(shadeHeight(location, build, "BLDG_HT", solar_pos), 2), 
  pos = 3
  )
plot(sun_ray, add = TRUE, col = "yellow")
plot(inter, add = TRUE, col = "red")

## ------------------------------------------------------------------------
ext = as(extent(build) + 50, "SpatialPolygons")
r = raster(ext, res = 2)
proj4string(r) = proj4string(build)
grid = rasterToPoints(r, spatial = TRUE)
grid = SpatialPointsDataFrame(
  grid,
  data.frame(grid_id = 1:length(grid))
  )

## ----heightgrid, fig.cap="Input grid for creating a shade heights surface"----
plot(grid, pch = ".")
plot(build, add = TRUE)

## ---- eval=FALSE---------------------------------------------------------
#  library(parallel)
#  shade_heights = mclapply(
#    split(grid, grid$grid_id),
#    shadeHeight,
#    build, "BLDG_HT", solar_pos,
#    mc.cores = 3
#    )
#  grid$shade_height = simplify2array(shade_heights)

## ---- echo=FALSE---------------------------------------------------------
grid$shade_height = NA
for(i in 1:length(grid)) {
  grid$shade_height[i] = 
    shadeHeight(grid[i, ], build, "BLDG_HT", solar_pos, messages = FALSE)
}

## ----heightresult, fig.cap="Shade height (m) grid", fig.height=5.5-------
shade = as(grid, "SpatialPixelsDataFrame")
shade = raster(shade, layer = "shade_height")
plot(shade, col = grey(seq(0.9, 0.2, -0.01)))
plot(shade, col = grey(seq(0.9, 0.2, -0.01)))
contour(shade, add = TRUE)
plot(build, add = TRUE, border = "red")
text(gCentroid(build, byid = TRUE), build$BLDG_HT)

## ----park, fig.cap="Park location"---------------------------------------
park_location = raster::shift(location, y = 20, x = -8)
park = rgeos::gBuffer(park_location, width = 12)
plot(build)
text(gCentroid(build, byid = TRUE), build$BLDG_HT)
plot(park, col = "lightgreen", add = TRUE)

## ---- echo=FALSE---------------------------------------------------------
time2 = as.POSIXct("2004-06-24 09:30:00", tz = "Asia/Jerusalem")
solar_pos2 = maptools::solarpos(location_geo, time2)
footprint = shadeFootprint(build, "BLDG_HT", solar_pos2)
park_shade = gIntersection(park, footprint)
shade_prop = gArea(park_shade) / gArea(park)

## ------------------------------------------------------------------------
time2 = as.POSIXct("2004-06-24 09:30:00", tz = "Asia/Jerusalem")
solar_pos2 = maptools::solarpos(location_geo, time2)
solar_pos2
footprint = shadeFootprint(build, "BLDG_HT", solar_pos2)
park_shade = gIntersection(park, footprint)
shade_prop = gArea(park_shade) / gArea(park)
shade_prop

## ----footprint, fig.cap="Shade footprint at 2004-06-24 09:30:00"---------
plot(footprint,  col = adjustcolor("lightgrey", alpha.f = 0.5))
plot(build, col = "darkgrey", add = TRUE)
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
        shadeFootprint(
          build, 
          "BLDG_HT", 
          solar_pos_seq[i, , drop = FALSE]
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

