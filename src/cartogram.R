library(maptools)
library(cartogram)
library(rgdal)
data(wrld_simpl)
# Remove uninhabited regions
wld <- spTransform(wrld_simpl[wrld_simpl$REGION & wrld_simpl$POP2005 > 0,],
                   CRS("+init=epsg:3395"))
# Create cartogram
wld_carto <- cartogram_cont(wld, "POP2005", 3)
# Plot
par(mfcol=c(1,2))
plot(wld, main="original")
plot(wld_carto, main="distorted (sp)")
# Same with sf objects
library(sf)
wld_sf = st_as_sf(wld)
wld_sf_carto <- cartogram_cont(wld_sf, "POP2005", 3)
# Plot
par(mfcol=c(1,3))
plot(wld, main="original")
plot(wld_carto, main="distorted (sp)")
plot(st_geometry(wld_sf_carto), main="distorted (population)")

