library(dplyr)

trajectories_A3 <- function() {
  data <- A3@data
  coords <- A3@sp@coords
  p4 <- A3@sp@proj4string@projargs
  #time <- c(A3@time)
  time_attrs <- attributes(A3@time)
  time <- c(time_attrs$index)
  tz <- if (nchar(time_attrs$tz) == 0) "UTC" else time_attrs$tz
  time <- ISOdatetime(1970, 1, 1, 0, 0, 0, tz = tz) + time
  v <- bind_cols(data_frame(x = coords[,1], y = coords[,2], t = time, .br0=1, .ob0=1), data)
  b <- v %>% distinct(.br0) %>% select(.br0, .ob0)
  o <- b %>% distinct(.ob0) %>% mutate(p4 = p4)
  list(v = v, b = b, o = o)
}

library(gris)
pl(trajectories_A3(), type = "l")


albatross_adehabitatLT <- function() {
  ## unpick this later
  ## The coordinates are given in meters (UTM - zone 42).
  p4 <- "+proj=utm +south +zone=42 +ellps=WGS84"
  x <- suppressWarnings(trip::ltraj2trip(albatross))
  v <- as_data_frame(as.data.frame(x)) %>% mutate(.br0 = as.integer(factor(burst)), .ob0 = as.integer(factor(id)),y = y + 1e7)
  b <- v %>% distinct(.br0) %>% select(.br0, .ob0)
  list(v = v, b = v, o = b %>% distinct(.ob0) %>% mutate(p4 = p4))
}

pl(albatross_adehabitatLT()$v, type = "l")
library(rgdal)
p <- project(as.matrix(albatross_adehabitatLT()$v %>% select(x, y)), albatross_adehabitatLT()$o$p4[1], inv = TRUE)
pt <- matrix(rev(c(-46.416667, 51.983333)), ncol = 2)
pp <- project(pt, "+proj=utm +south +zone=42 +ellps=WGS84 ")
abline(h = pp[2], v = pp[1])
w <- bld(wrld_simpl)$v
pl(w %>% filter(abs(x - pt[1]) < 50, abs(y - pt[2]) < 50))
points(p)
## need add = TRUE here
pl(w, add = TRUE)
