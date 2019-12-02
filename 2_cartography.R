# ----------------
# N. LAMBERT, 2019
# ----------------

# devtools::install_github("tylermorganwall/rayshader")
# https://www.rayshader.com/

# devtools::install_github("statnmap/maps2ray")
# https://github.com/statnmap/maps2ray

# https://wcmbishop.github.io/rayshader-demo/
# https://www.davidsolito.com/post/a-rayshader-base-tutortial-bonus-hawaii/

# Packages

library(raster)
library(rayshader)
library(cartography)
library(sf)
library(elevatr)
library(leaflet)
library(rosm)
library(png)

# import

com <- st_read("data/communes.shp")
r <- raster("data/popstewart600.tif")


# Interpolating raster to get finer resolution
r <- disaggregate(r, fact=10, method='bilinear')

plot(r)

# local peaks

f <- function(X) max(X, na.rm = TRUE)
ww <- matrix(1, nrow=41, ncol=41) ## Weight matrix for cells in moving window
localmax <- focal(r, fun=f, w=ww, pad=TRUE, padValue=0)
r2 <- r==localmax
r2[localmax<2000] <- 0
maxXY <- xyFromCell(r2, Which(r2==1, cells=TRUE))


plot(r)
points(maxXY[,1], maxXY[,2], col = "red", pch = 20)


# t <- getTiles(com,type = "osm", zoom = 13, crop = TRUE)
# t <- resample(t$layer.1,r)
# t <- raster_to_matrix(t)
# writePNG(t,'data/texture2.png')

# Texture

elevation.texture.map <- readPNG("data/texture.png")
# elevation.texture.map <- resample(elevation.texture.map, s, method='bilinear')

# PLOT 2D

# plot(st_geometry(com))
# plot(r, add=T)
# plot(st_geometry(com), add=T, lwd=0.4)
# plot(t)

# PLOT 3D

popmat = raster_to_matrix(r)

zscale <- 800

# Rayshade raster
ambmat <- ambient_shade(popmat, zscale = zscale)
raymat <- ray_shade(popmat, zscale = zscale)

ray_image <- popmat %>%
  sphere_shade(texture = "imhof4") %>%
  add_shadow(raymat, max_darken = 0.5) %>%
  add_overlay(elevation.texture.map, alphalayer = 0.99) %>% 
  add_shadow(ambmat, max_darken = 0.5) 

# 
  
ray_image %>% 
  plot_3d(
    popmat,
    zscale = zscale, windowsize = c(1500, 1500),
    water = TRUE, waterdepth = 0, wateralpha = 0.9, watercolor = "lightblue",
    theta = 50, phi = 5,
    zoom = 0.5, 
    fov = 0)
render_label(montereybay,x=100,y=100, z=10000,zscale=50,text = "BLA BLA BLA",relativez=FALSE)
Sys.sleep(0.4)
# render_highquality(lightdirection = 0, lightaltitude  = 30, clamp_value = 10,samples=200, clear=TRUE)
#render_snapshot(clear=TRUE)

