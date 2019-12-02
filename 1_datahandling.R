# ----------------
# N. LAMBERT, 2019
# DATA IMPORT (INSEE GRID 200M)
# ----------------

# packages

library(sf)
library(SpatialPosition)
library(raster)

# Carroyages 200m INSEE

grid200m <- st_read("input/car_m.mif")
st_crs(grid200m) <- "+proj=lcc +lat_1=45.90287723937 +lat_2=47.69712276063 +lat_0=46.8 +lon_0=2.337229104484 +x_0=600000 +y_0=2200000 +ellps=clrk80 +towgs84=-168,-60,320,0,0,0,0 +units=m +no_defs"

# Communes IGN

com <- st_read("input/COMMUNE.shp")
prj <- st_crs(com)
com <- st_transform(com,crs=st_crs(grid200m))

# Study area

center <- st_centroid(st_union(com[com$CODE_DEPT == 75,]))
buff <- st_buffer(center, 15000)
buff <- st_as_sfc(st_bbox(buff), crs = st_crs(com))
communes <- st_intersection(x = com, st_geometry(buff))
grid200m <- st_intersection(x = grid200m, st_geometry(st_buffer(buff, 1000)))
dots <- st_centroid(grid200m)


plot(st_geometry(dots), pch=20, col="red", cex=0.1)
plot(st_geometry(communes), add=T)

# pop

pop <- foreign::read.dbf("input/car_m.dbf", as.is = FALSE)
dots <- merge (x = dots, y = pop, by.x = "id",  by.y = "id", all.x = TRUE)
dots <- dots[,c("id","ind_c","geometry")]

# export

# dots <- st_transform(dots, prj)
st_write(dots,"data/pop200m.shp")

# communes <- st_transform(communes, prj)
st_write(communes,"data/communes.shp")

# Stewart computation -----------------------------------------------------

com <- st_read("data/communes.shp")
pop <- st_read("data/pop200m.shp")

mygrid <- CreateGrid(w = pop, resolution = 350)
mymat <- CreateDistMatrix(knownpts = pop, unknownpts = mygrid)
mystewart <- stewart(knownpts = pop, unknownpts = mygrid,
                     matdist = mymat, varname = "ind_c",
                     typefct = "exponential", span = 2000,
                     beta = 3, mask = com, returnclass = "sf")

# Raster

r <- rasterStewart(mystewart, mask = com)
writeRaster(r, 'data/popstewart2000.tif')
