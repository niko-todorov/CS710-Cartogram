library(maptools)
library(cartogram)
library(rgdal)
data(wrld_simpl)
# Remove uninhabited regions
wld <- spTransform(wrld_simpl[wrld_simpl$REGION & wrld_simpl$POP2005 > 0,],
                   CRS("+init=epsg:3395"))
# Create cartogram
wld_nc <- cartogram_ncont(afr, "POP2005")
# Plot
plot(wld)
plot(wlc_nc, add = TRUE, col = 'red')
# Same with sf objects
library(sf)
wld_sf = st_as_sf(wld)
wld_sf_nc <- cartogram_ncont(wld_sf, "POP2005")
plot(st_geometry(wld_sf))
plot(st_geometry(wld_sf_nc), add = TRUE, col = 'red')
