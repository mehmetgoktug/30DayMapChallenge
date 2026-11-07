# Load packages
library("terra")
library("geodata")

# Settings-constants
CRS <- "EPSG:8857" # equal earth
PATH <- "."

# World ADM
w <- world(path=PATH)
w <- project(w, CRS)

# Get WorldClim
e <- project(ext(-180, 180, -90, 90), "EPSG:4326", CRS) # extent
tAvg <- worldclim_global(var="tavg", res=10, path=PATH)
tAvg <- mean(tAvg)
tAvg <- project(tAvg, CRS)
tAvg <- crop(tAvg, w, mask=TRUE)
tAvg <- extend(tAvg, e)

# Create graticule
g <- graticule(lon=30, lat=30, crs=CRS)

# Plot
cols <- hcl.colors(110, "Blue-Red 3")[1:86]
rng <- c(-55, 31)

png("day7.png", width=10, height=5.5, units="in", res=300)
par(family="CMU Serif")
plot(tAvg, col=cols, range=rng, axes=FALSE,
     main="Average Temperature (1970–2000)", plg=list(title="°C", tic="out"),
     mar=c(1.8, 1.5, 1.6, 3.2))
plot(g, col="grey12", lwd=0.4, background="lightblue", lab.lat = 2:6, 
     retro=TRUE, add=TRUE)
plot(tAvg, col=cols, range=rng, legend=FALSE, add=TRUE)
plot(w, lwd=0.5, add=TRUE)
add_mtext("#30DayMapChallenge · Day 7: 10 minute map", side=1, line=3, adj=0,
          cex=0.7)
add_mtext("Data: WorldClim · Plot: Mehmet Göktuğ Öztürk", side=1, line=3,
          adj=1, cex=0.7)
dev.off()
