chillmapr <- function(region = NULL, resolution, theme = "minimal", savepng = FALSE, pngname = "map.png", pngdpi = 1000, pngdim = c(4,4)){
  require(ggplot2)
  require(raster)
  require(elevatr)
  require(terra)
  require(ggnewscale)
  require(sf)
  require(ggspatial)
  require(ggrastr)
  
  if(is.null(region)){
    stop("
    \n No input shapefile provided! 
         \n You can easily make one using the R package 'rnaturalearth'
         \n For example: 
         \n Colombia <- rnaturalearth::ne_states(country = 'Colombia') ")
  }
  
  #Retrieve elevation data from within this shapefile:
  regionelev <- elevatr::get_elev_raster(locations = region, z = resolution, clip = "locations")
  regionelev <- rast(regionelev)
  regionelev.df <- raster::as.data.frame(regionelev, xy = TRUE) %>% na.omit()
  colnames(regionelev.df) <- c("Longitude", "Latitude", "Elevation")
  
  #Shaded relief layer:
  slope <- terrain(regionelev, "slope", unit = "radians")
  aspect <- terrain(regionelev, "aspect", unit = "radians")
  hillshade <- shade(slope, aspect, angle = 45, direction = 120)
  hillshade.df <- as.data.frame(hillshade, xy = TRUE) %>% na.omit()
  colnames(hillshade.df) <- c("Longitude", "Latitude", "hillshade")
  
  #make the ggplot
  
  if(theme == "minimal"){
    plot <- ggplot()+
      geom_tile(data = hillshade.df, aes(x=Longitude, y = Latitude, fill = hillshade), show.legend = FALSE)+
      scale_fill_distiller(palette = "Greys", direction = 1)+
      new_scale_fill()+
      geom_tile(data = regionelev.df, aes(x=Longitude, y = Latitude, fill = Elevation), alpha = 0.5, show.legend = FALSE)+
      scale_fill_distiller(palette = "Greys", direction = 1)+
      geom_sf(data = region, fill = NA, lwd = 0.4, color = 'gray20')+
      new_scale_fill()+
      theme_minimal()+
      theme(legend.position = "none")+
      xlab("Longitude")+
      ylab("Latitude")+
      labs(x=NULL, y = NULL)
  }
  if(theme == "void"){
    plot <- ggplot()+
      geom_tile(data = hillshade.df, aes(x=Longitude, y = Latitude, fill = hillshade), show.legend = FALSE)+
      scale_fill_distiller(palette = "Greys", direction = 1)+
      new_scale_fill()+
      geom_tile(data = regionelev.df, aes(x=Longitude, y = Latitude, fill = Elevation), alpha = 0.5, show.legend = FALSE)+
      scale_fill_distiller(palette = "Greys", direction = 1)+
      geom_sf(data = region, fill = NA, lwd = 0.4, color = 'gray20')+
      new_scale_fill()+
      theme_void()+
      theme(legend.position = "none")+
      xlab("Longitude")+
      ylab("Latitude")+
      labs(x=NULL, y = NULL)
  }
  
  if(savepng == TRUE){ggsave(pngname, plot = plot, width = pngdim[1], height = pngdim[2], units = "in", dpi = pngdpi, bg = "transparent")}
  return(plot)

}




