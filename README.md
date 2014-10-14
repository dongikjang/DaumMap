DaumMap
=======
R functions for using Daum Map tiles


### Daum Map

```coffee
require(rgdal)
require(png)
require(RgoogleMaps)
require(RColorBrewer)
tloc <- read.csv("TCountingLocInSeoul.csv", header=TRUE, stringsAsFactors = FALSE)
lon <- tloc$X5
lat <- tloc$X6
dmap <- getDaumMap(lon, lat, zoom=NA)
cols <- brewer.pal(9, "Set1")
plot(dmap)
daumloc <- WGS842Daum(tloc[, c("X5", "X6")])
points(daumloc,  pch=19, col=cols[tloc$X2])
```

!(screenshots/tloc.png)