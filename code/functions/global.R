################################################################################
# Global functions used in collar-viewer shiny app
# McCrea Cobb <mccrea_cobb@fws.gov>
################################################################################



#----
## Aesthetics

# Color palatte used in the application
color_pal <- c("#3366CC", "#DC3912", "#FF9900", "#109618", "#990099", "#0099C6",
               "#DD4477", "#66AA00", "#B82E2E", "#316395", "#994499", "#22AA99",
               "#AAAA11", "#6633CC", "#E67300", "#8B0707", "#651067", "#329262",
               "#5574A6", "#3B3EAC")



#----
## Mapping functions


#' Leaflet map of GPS collar data
#'
#' @description Map collar data. One point per day for lines. First and last
#' points.
#'
#' @author McCrea Cobb <mccrea_cobb@fws.gov
#'
#' @param gps_collar A dataframe of GPS collar fixes containing lat, lon, and animal_id, site and fix_time fields
#'
#' @return A leaflet map of fix locations, subsetted to one daily fix.
#' @export
#'
#' @examples collar_map(dat)
#'
collar_map <- function(gps_collar) {
  df <- gps_collar %>%
    filter(!(is.na(lon) | is.na(lat))) %>%
    arrange(animal_id, fix_time) %>%
    group_by(animal_id, lubridate::as_date(fix_time)) %>%
    slice(1) %>%  # takes the first fix of the day for each collar
    ungroup()

  ids <- unique(df$animal_id)
  pal <- rep_len(color_pal, length(ids))

  map <- leaflet() %>%
    addProviderTiles("Esri.WorldTopoMap", options = providerTileOptions(attribution = NA)) %>%
    addMeasure(primaryLengthUnit="kilometers", secondaryLengthUnit="kilometers") %>%
    addScaleBar(position="bottomleft") %>%
    setView(lng = -150, lat = 65, zoom = 4)


  for (i in seq_along(ids)) {
    d <- df %>% filter(animal_id == ids[i])
    dp <- d[c(1, nrow(d)), ]
    map <- addPolylines(map, lng = d$lon, lat = d$lat,
                        weight = 1,
                        color = pal[i],
                        opacity = .4)
    map <- addCircleMarkers(map, lng = dp$lon, lat = dp$lat,
                            stroke = FALSE,
                            radius = 4,
                            color = pal[i],
                            fillOpacity = 1,
                            popup = paste(sep = "<br>",
                                          paste("<b>Lynx ID:<b>", ids[i]),
                                          paste("<b>Study site:<b>", d$site),
                                          paste("<b>Last fix:<b> ", d$fix_time)))
  }
  return(map)
}



#----
## Functions used to create dat.move() in the Home Range tab


#' Add projected coordinates to a dataframe
#'
#' @description Add x and y projected coordinates to the dataframe containing latitude and longitude coordinates
#'
#' @param df A dataframe containing unprojected lat and long coordinates
#' @param xy Combined vectors containing lat and long variables.
#' @param CRSin The coordinate reference system id of \code{df}.
#' @param CRSout The desired output coordinate reference system id for the projected x and y values.
#'
#' @return
#' @export
#'
#' @examples xy_conv(df, xy = c('lon', 'lat'), CRSin = '+proj=longlat',
#' CRSout = "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
#'
xy_conv <- function(df, xy = c('lon', 'lat'), CRSin = '+proj=longlat',
                   CRSout = "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") { # Alaska Albers

  df <- df[complete.cases(df[, xy]), ]
  conv <- sp::SpatialPoints(coordinates(cbind('x' = df[, xy[1]],
                                              'y' = df[, xy[2]])),
                            proj4string = CRS(CRSin))
  conv <- sp::spTransform(conv, CRS(CRSout))
  conv <- data.frame(conv)
  colnames(conv) <- c('x', 'y')
  df <- cbind(df, conv)

  return(df)
}



#' Calculate movement distance
#'
#' @description Calculate movement distances. Used by \code{move_speed()}.
#'
#' @param x A vector of projected x coordinates.
#' @param y A vector of projected y coordinates.
#'
#' @return A vector of movement distances. The units are dependent upon the CRS of \code{x} and \code{y}. For most applications, it will be meters.
#' @export
#'
#' @examples move_dist(x, y)
#'
move_dist <- function(x, y) {
  dist <- c(0, sqrt((x[-1] - x[-length(x)])^2 +
                      (y[-1] - y[-length(y)])^2))
  return(dist)  # same unit as input (meters)
}



#' Calculate net squared displacement
#'
#' @description Calculate net squared displacement. Used by \code{move_speed()}.
#'
#' @param x A vector of projected x coordinates.
#' @param y A vector of projected y coordinates.
#'
#' @return A vector of net square displacement values. The units are dependent upon the CRS of \code{x} and \code{y}. For most applications, it will be meters.
#' @export
#'
#' @examples move_nst(x, y)
#'
move_nsd <- function(x, y) {
  r2n <- (x - x[1])^2 + (y - y[1])^2
  r2n <- (r2n - min(r2n))/(max(r2n) - min(r2n))
  return(r2n)
}



#' Calculate time between consecutive GPS fixes
#'
#' @description Calculate the time between consecutive GPS fixes. Used by \code{move_speed()}.
#'
#' @param time A vector of POSIXct times.
#'
#' @return The time in seconds between consecutive time values in a vector of
#' fixtimes.
#' @export
#'
#' @examples move_dt(fix_time)
#'
move_dt <- function(time) {
  dt <- c(0, unclass(time[-1]) - unclass(time[-length(time)]))
  return(dt/3600) # seconds
}



#' Calculate speed of consecutive GPS fixes
#'
#' @description Calculate speed of consecutive GPS fixes.
#'
#' @param dist A vector of movement distances returned from \code{move_dist()}.
#' @param time A vector of movement times (seconds) returned from \code{move_dt()}.
#'
#' @return The movement speed between consecutive GPS collar fixes, generally in meters/second.
#' @export
#'
#' @examples move_speed(Distance, dTime)
#'
move_speed <- function(dist, time) {
  speed <- (dist/1000)/time
  speed[is.nan(speed)] <- 0
  return(speed)
}



#' Create leaflet map of subsetted GPS collars
#'
#' @description Create a leaflet map object of GPS collar fixes that includes lines connecting consecutive points. Used on the Home Range tab.
#'
#' @param map A leaflet map object
#' @param df A dataframe of GPS collar fixes that include lat, lon, animal_id, and fix_time variables.
#'
#' @return A leaflet map object with points and lines
#' @export
#'
#' @examples map_pts(map, df)
#'
map_pts <- function(map, df) {
  ids <- unique(df$animal_id)
  pal <- rep_len(color_pal, length(ids))
  layers <- list()

  for(i in seq_along(ids)) {
    dat <- df %>% filter(animal_id == ids[i])
    map <- addPolylines(map, lng = dat$lon, lat = dat$lat,
                        group = as.character(ids[i]),
                        color = pal[i], weight = 1)
    map <- addCircleMarkers(map, lng = dat$lon, lat = dat$lat,
                            group = as.character(ids[i]), color = pal[i],
                            radius = 3, stroke = FALSE, fillOpacity = .3,
                            popup = paste(sep = "<br>",
                                          paste("<b>Lynx ID:</b> ", ids[i]),
                                          paste("<b>Date:</b> ", dat$fix_time)))
    layers <- c(layers, as.character(ids[i]))
  }
  map <- addLayersControl(map, overlayGroups = layers)
  return(map)
}



#' Create leaflet map of home range polygons
#'
#' @description Add geoJSON polygons to a leaflet map. Used on the Home Range tab.
#'
#' @param map A leaflet map object
#' @param geojson A geoJSON polygon containing home range contours
#'
#' @return A leaflet map object with home range contours
#' @export
#'
#' @examples map_polygons(map, hr)
#'
map_polygons <- function(map, geojson) {
  pal <- rep_len(color_pal, length(geojson))
  for (i in seq_along(geojson)) {
    map <- addGeoJSON(map, geojson[[i]], color = pal[i],
                      weight = 1, group = names(geojson)[i])
  }
  return(map)
}




#----
## Brownian Bridge home range functions

#' Create a trajectory object
#'
#' @description Create an \code{ltraj} trajectory object for Brownian Bridge home range estimates
#'
#' @param dat A dataframe containing projected x and y coordinates and associated POSIXct times
#'
#' @return A \code{adehabitatHR::ltraj()} trajectory object
#' @export
#'
#' @examples to_ltraj(dat)
#'
to_ltraj <- function(dat) {
  dat <- as.data.frame(dat)
  dat$fix_time <- as.POSIXct(dat$fix_time, format = '%Y-%m-%d %H:%M:%S')
  dat <- dat[complete.cases(dat[, c("x", "y", "fix_time")]), ]

  traj <-  adehabitatLT::as.ltraj(dat[, c("x", "y")], date = dat$fix_time, id = dat$animal_id)
  return(traj)
}



#' Estimate Brownian Bridge home range
#'
#' @description Estimate Brownian Bridge home range utlization distribution
#'
#' @param traj A \code{adehabitatHR::ltraj()} trajectory object
#'
#' @return An \code{estUDm} object with an estimate of a Brownian Bridge home range utilization distribution
#'
#' @export
#'
#' @examples estimate_bbmm(traj)
#'
# **This needs updating because the sigmas need to vary by individual home range instead of using a single value across all animals.**
estimate_bbmm <- function(traj) {
  sig1 <- adehabitatHR::liker(traj, sig2 = 40, rangesig1 = c(0, 10), plotit = FALSE)
  bb <- adehabitatHR::kernelbb(traj, sig1[[1]]$sig1, 40, grid = 100)
  return(bb)
}

# estimate_bbmm <- function(traj) {
#   sig1 <- adehabitatHR::liker(traj, sig2 = 40, rangesig1 = c(0, 10), plotit = FALSE)
#   bb <- lapply(1:length(traj), function(x) {
#     adehabitatHR::kernelbb(traj[x], sig1[[x]]$sig1, 40, grid = 100)
#   })
#   names(bb) <- lapply(1:length(traj), function(x) {
#     attr(traj[[x]], "animal_id")
#   })
#   return(bb)
# }

#' Fix for Brownian Bridge home ranges
#'
#' @description Fix for Brownian Bridge home ranges
#'
#' @param bb An \code{estUDm} object with an estimate of a Brownian Bridge home range utilization distribution
#'
#' @return An \code{estUDm} object or list of \code{estUDm} objects with an estimate of a Brownian Bridge home range utilization distribution
#' @export
#'
#' @examples bb_fix(bb)
#'
bb_fix <- function(bb) {  # A fix for Brownian bridge calculations
  if (class(bb) == 'estUDm') {
    v <- bb
  } else {
    v <- list(bb)
  }
  return(v)
}



#----
## For maps of kernel and Brownian Bridge home ranges

#' Get home range contours
#'
#' @description
#'
#' @param ud A \code{estUD} object returned from \code{adehabitatHR::kernelUD()} or code\{adehabitatHR::kernelbb()}
#' @param pct Vector of numeric values specifying the desired percent contours of the resulting home range polygon
#'
#' @return A spatial polygon dataframe of kernel or Brownian Bridge utilization density contours
#'
#' @examples get_contours(kd, c(50, 95))
#'
get_contours <- function(ud, pct) {
  ids <- as.character(pct)
  x <- getvolumeUD(ud)
  xyma <- coordinates(x)
  xyl <- list(x = unique(xyma[, 1]), y = unique(xyma[, 2]))
  z <- as.image.SpatialGridDataFrame(x[, 1])$z

  cl <- lapply(pct, function(x) {
    contourLines(x = xyl$x, y = xyl$y, z, nlevels = 1, levels = x)
  })

  plys <- lapply(seq_along(cl), function(i) {
    Polygons(lapply(seq_along(cl[[i]]), function(j) {
      m <- cl[[i]][[j]]
      ply <- cbind(m$x, m$y)
      ply <- rbind(ply, ply[1, ])
      Polygon(ply)
    }), ID = ids[i])
  })

  plys <- lapply(plys, function(x) checkPolygonsHoles(x))
  splys <- SpatialPolygons(plys)
  dff <- data.frame(animal_id = ids)
  row.names(dff) <- ids
  spdf <- SpatialPolygonsDataFrame(splys, dff)
  return(spdf)
}
