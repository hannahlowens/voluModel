# Trying fix plotLayers with ggplot; really not working

redVal <- 1
blueVal <- 0
stepSize <- 1/nlyr(rast)

plotStart <- ggplot() +
  geom_spatraster(data = rast[[1]], maxcell = maxpixels) +
  scale_fill_stepsn(colours = c(rgb(0,0,0,0),
                                rgb(redVal,0,blueVal,stepSize)),
                    na.value = "transparent", guide = "none") +
  coord_sf(xlim=ext(rast)[1:2], ylim=ext(rast)[3:4], expand = 0) +
  labs(title = title) +
  theme_minimal()

for(i in 2:nlyr(rast)){
  redVal <- redVal - stepSize
  blueVal <- blueVal + stepSize
  print(paste0("red value: ", redVal, " blue value: ", blueVal))
  plotStart <- plotStart +
    geom_spatraster(data = rast[[i]], maxcell = maxpixels,
                    aes(fill = c(rgb(0,0,0,0),
                             rgb(redVal,0,blueVal, stepSize)), na.value = "transparent"))
}

if(!any(is.na(land))){
  plotStart <- plotStart +
    geom_spatvector(data = land, fill = landCol, color = landCol)
}

plot(plotStart)

  geom_spatvector(data = land, fill = landCol, color = landCol) +
  coord_sf(xlim=ext(rast)[1:2], ylim=ext(rast)[3:4], expand = 0) +
  labs(title = title) +
  theme_minimal()

plotStart <- sp::spplot(rast[[1]],
                        col.regions = c(rgb(0,0,0,0),
                                        rgb(redVal,0,blueVal,stepSize)),
                        cuts = 1, colorkey = FALSE, col="transparent",
                        main = title,
                        par.settings = list(mai = c(0,0,0,0)),
                        maxpixels = maxpixels, sp.layout=list(as(land, "Spatial"), fill=landCol, first=TRUE))
