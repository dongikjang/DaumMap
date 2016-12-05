###################################################
toTileDaum <- function(lon, lat, zoom=NA, maproj = c("WGS84", "Daum")[2]){
    require(rgdal)
    maxZoomLevels <- 14
    if(!is.na(zoom)){
      if(zoom > maxZoomLevels) stop(paste("zoom level is greater than", maxZoomLevels, "\n"))
    }
    if(length(lon) > 2) lon <- range(lon, na.rm=TRUE)
    if(length(lat) > 2) lat <- range(lat, na.rm=TRUE)
    
    if(length(lon) == 1) lon <- lon + c(-.1, .1)
    if(length(lat) == 1) lat <- lat + c(-.1, .1)
      
    xy <- data.frame(x=lon, y=lat)
    coordinates(xy) <- c("x", "y")
    proj4string(xy) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 
                             +units=m +no_defs")
    xytr <- spTransform(xy, CRS("+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000  +y_0=500000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
    xy <- coordinates(xytr)
    

    maxExtent <- list(left = -30000, bottom = -60000, right = 494288, top = 988576)
    resolutions <- c(2048, 1024, 512, 256, 128, 64, 32, 16, 8, 4, 2, 1, 0.5, 0.25)
    
    if(is.na(zoom)){
      candires <- max(apply(xy, 2, diff)/ (4*256))
      zoom <- max(which(resolutions > candires)) 
    }
    
    #xtile <- round((xy[,1] - maxExtent$left)/ (resolutions[zoom+1] * 256))
    #ytile <- round((xy[,2] - maxExtent$bottom)/ (resolutions[zoom+1] * 256))
    xtile <- sort((xy[,1] - maxExtent$left)/ (resolutions[zoom+1] * 256))
    ytile <- sort((xy[,2] - maxExtent$bottom)/ (resolutions[zoom+1] * 256))
    xtile <- c(floor(xtile[1]), ceiling(xtile[2]))
    ytile <- c(floor(ytile[1]), ceiling(ytile[2]))
    
    xytile <- expand.grid(xtile[1]:xtile[2], ytile[1]:ytile[2])
    seqxtile <- xytile[ , 1]
    seqytile <- xytile[ , 2]
    xy <- data.frame(x = seqxtile * (resolutions[zoom+1] * 256) + maxExtent$left,
                       y = seqytile * (resolutions[zoom+1] * 256) + maxExtent$bottom)
    if(maproj[1] == "WGS84"){               
        coordinates(xy) <- c("x", "y")
        proj4string(xy) <- CRS("+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000  +y_0=500000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
        xyor <- spTransform(xy, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 
                                     +units=m +no_defs"))
        xy <- coordinates(xyor)
    }
  
    if(xtile[2] > xtile[1]){ 
        xtile[2] <- xtile[2] - 1
    }
    if(ytile[2] > ytile[1]){ 
        ytile[2] <- ytile[2] - 1
    }
    zoom <- length(resolutions) - zoom
  return(list(xtileind = xtile, ytileind = ytile, bbox = xy, maproj = maproj, zoom=zoom))
}


getDaumMap <- function(lon, lat, zoom=NA, maproj = c("WGS84", "Daum")[2], GRAYSCALE=FALSE, 
                       mapstyle=c("Street", "Hybrid", "Physical", "Satellite")){
    require(png)
    require(RgoogleMaps)
    lon <- sort(lon)
    lat <- sort(lat)
      
    tileind <- toTileDaum(lon, lat, zoom=zoom, maproj=maproj)
  
    zoom <- tileind$zoom
    xtileind <- sort(tileind$xtileind[1]:tileind$xtileind[2])
    ytileind <- sort(tileind$ytileind[1]:tileind$ytileind[2])
    z <- zoom #+ 1
  
    tmp1 <- NULL
    nx <- 0
    mapadd <- switch(mapstyle[1],
                      Hybrid = "http://map1.daumcdn.net/map_hybrid/17no/L",
                      Physical = "http://map1.daumcdn.net/map_shaded_relief/2.00/L",
                      Satellite = "http://map1.daumcdn.net/map_skyview/L",
                      Street = "http://map1.daumcdn.net/map_2d/17no/L")
    for(x in xtileind){
        tmp2 <- NULL
        ny <- 0
        for(y in ytileind){
          if(mapstyle[1] == "Satellite"){
            addr <- paste(mapadd, z, "/", y, "/", x, ".jpg?v=140830", sep="")
            download.file(addr, "test.jpg", quiet = TRUE, mode="wb")
            if(GRAYSCALE){
              #require(jpeg)
              test <- readJPEG("test.png", native = FALSE)
              test <- RGB2GRAY(test)
              writeJPEG(test, "test.jpg")
              test <- readJPEG("test.jpg", native = FALSE)
              tmp2 <- rbind(test, tmp2)
            } else{
              test <- readJPEG("test.jpg")
              ny <- ny + 256
              tmp2 <- array(c(rbind(test[,,1], tmp2[,,1]),
                              rbind(test[,,2], tmp2[,,2]),
                              rbind(test[,,3], tmp2[,,3])), dim = c(ny, 256, 3))
            }
          } else {
            addr <- paste(mapadd, z, "/", y, "/", x, ".png", sep="")
            download.file(addr, "test.png", quiet = TRUE, mode="wb")
            if(GRAYSCALE){
              test <- readPNG("test.png", native = FALSE)
              test <- RGB2GRAY(test)
              writePNG(test, "test.png")
              test <- readPNG("test.png", native = FALSE)
              tmp2 <- rbind(test, tmp2)
            } else{
              test <- readPNG("test.png")
              ny <- ny + 256
              tmp2 <- array(c(rbind(test[,,1], tmp2[,,1]),
                              rbind(test[,,2], tmp2[,,2]),
                              rbind(test[,,3], tmp2[,,3])), dim = c(ny, 256, 3))
            }
          }  
            
        }
        if(GRAYSCALE){
          tmp1 <- cbind(tmp1, tmp2)  
        } else{
          nx <- nx + 256
          tmp1 <- array(c(cbind(tmp1[ , , 1], tmp2[ , , 1]),
                          cbind(tmp1[ , , 2], tmp2[ , , 2]),
                          cbind(tmp1[ , , 3], tmp2[ , , 3])), dim = c(ny, nx, 3))
        }
        
    
    }
    if(maproj[1] == "WGS84"){
      proj4 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84
                   +units=m +no_defs")
    } else{
      proj4 <- CRS("+proj=tmerc +lat_0=38 +lon_0=127 +k=1 
                    +x_0=200000  +y_0=500000 +ellps=GRS80 
                    +towgs84=0,0,0,0,0,0,0 +units=m +no_defss")
    }
  
    outobj <- list(pngmap = tmp1, bbox=tileind$bbox, tileind=tileind, zoom=zoom, 
                   maproj=maproj, proj4=proj4, mapstyle=mapstyle[1])
    class(outobj) <- "daummap"
    return(outobj)
}

print.daummap <- function(obj){
    cat(paste("Importing Daum map tiles (", obj$mapstyle, ")\n", sep=""))
    cat(paste(" Zoom Level:", obj$zoom, "\n")) 
    cat(paste(" Tile :", diff(obj$tileind$xtileind)+1, "*", diff(obj$tileind$ytileind)+1), "\n") 
    cat(paste(" Tile index: longitude (", obj$tileind$xtileind[1], "-", 
                                          obj$tileind$xtileind[2],
                         "), latitude (", obj$tileind$ytileind[1], "-", 
                                          obj$tileind$ytileind[2], ")", sep=""),
        "\n")
    
    if(obj$maproj == "WGS84"){
        projval <- CRS("+proj=losnglat +ellps=WGS84 +datum=WGS84 
                        +units=m +no_defs")
    } else{
        projval <- CRS("+proj=tmerc +lat_0=38 +lon_0=127 +k=1 
                       +x_0=200000  +y_0=500000 +ellps=GRS80 
                       +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
    }
    print(projval)
    cat(paste(" Map projection:", obj$maproj), "\n")
    cat(paste(" Bounding box\n")) 
    nbbox <- nrow(obj$bbox)
    print(cbind(long = range(obj$bbox[, 1]), lati = range(obj$bbox[, 2])))
}

plot.daummap <- function(x, interpolate=TRUE, angle=0, xlab = "Longitude", ylab = "Latitude",...){
    xrng <- range(x$bbox[, 1])
    yrng <- range(x$bbox[, 2])
    if(x$maproj == "WGS84"){
        arr <- x$bbox[c(1, diff(x$tileind$xtileind) +2), ]
        v1 <- arr[2,] - arr[1,]
        v2 <- v1
        v2[2] <- 0
        ang <- sign(v1[2] - v2[2]) * acos(sum(v1*v2)/sqrt(sum(v1^2) * sum(v2^2)))
        angdgree <- ang*180/pi
        rotmat <- matrix(c(cos(-ang), sin(-ang), -sin(-ang), cos(-ang)), 2, 2)
        rotbbox <- x$bbox[c(1, nrow(x$bbox)), ]
        orgbbox <- rbind(rotbbox[1,], c(rotmat %*% apply(rotbbox, 2, diff) + rotbbox[1, ]))
        xrng <- orgbbox[,1]
        yrng <- orgbbox[,2]
        
        #axes <- TRUE
        #plot(xrng, yrng, type = "n", xlab = "Longitude", ylab = "Latitude", 
        #     xaxs='i', yaxs='i', asp=1, axes=TRUE)
        #rasterImage(as.raster(x$pngmap), xrng[1], yrng[1], xrng[2], yrng[2],
        #            interpolate = interpolate, angle=angdgree)
        
        #plot(x$bbox, pch=19)
        #for(i in 1:5) lines(x$bbox[c(1,6) + 6*(i-1), ])
        #for(i in 1:6) lines(x$bbox[seq(1, nrow(x$bbox), by=24) + (i-1), ])
        #tmp <- x$bbox   
        #tmp[,1] <- tmp[,  1] - tmp[1,1]
        #tmp[,2] <- tmp[, 2] - tmp[1,2]
        #tmp2 <- t(rotmat %*% t(tmp))
        #tmp2[, 1] <- tmp2[,1] + x$bbox[1,1]
        #tmp2[, 2] <- tmp2[,2] + x$bbox[1,2]
        #points(tmp2,  col=2, pch=19)
        #for(i in 1:5) lines(tmp2[c(1,6) + 6*(i-1), ], col=2)
        #for(i in 1:6) lines(tmp2[seq(1, nrow(tmp2), by=24) + (i-1), ], col=2)
            

        rasterimg <- as.raster(x$pngmap)
        m <- dim(x$pngmap)[1]
        n <- dim(x$pngmap)[2]
        n1 <- m * tan(ang)
        ind <- floor((1:ceiling(n1))/tan(ang))
        ind <- ind[ind < m]
        if(length(ind) > 1){
          rasterimg[, 1] <- "transparent"
        }
        for(i in 1:length(ind)){
          rasterimg[1:(m-ind[i]), i+1] <- "transparent"
        }
        
        ind <- floor((-ceiling(n1):0 + n1)/tan(ang))
        ind <- ind[ind > 0 & ind < m]
        ind <- rev(ind)
        if(length(ind) > 1){
          rasterimg[, n] <- "transparent"
        }
        for(i in 1:length(ind)){
          rasterimg[(m-ind[i]):m, n-i] <- "transparent"
        }
        
        plot(xrng, yrng, type = "n", xlab = xlab, ylab = yalb, 
             xaxs='i', yaxs='i', asp=1, axes=FALSE)
        rasterImage(rasterimg, xrng[1], yrng[1], xrng[2], yrng[2],
                    interpolate = interpolate, angle=angdgree, ...)    
        axis(1)
        axis(2)
    } else {
        axes <- FALSE
        plot(xrng, yrng, type = "n", xlab = xlab, ylab = ylab, 
             xaxs='i', yaxs='i', asp=1, axes=FALSE)
        
        rasterImage(x$pngmap, xrng[1], yrng[1], xrng[2], yrng[2],
                    interpolate = interpolate, ...)
    } 
    box()

}


getDaumMapimg <- function(lon, lat, zoom=NA, GRAYSCALE=FALSE){
  
  require(png)
  require(RgoogleMaps)
  lon <- sort(lon)
  lat <- sort(lat)

  tileind <- toTileDaum(lon, lat, zoom=zoom, maproj="Naver")
  
  zoom <- tileind$zoom
  xtileind <- sort(tileind$xtileind[1]:tileind$xtileind[2])
  ytileind <- sort(tileind$ytileind[1]:tileind$ytileind[2])
  z <- zoom #+ 1
  
  nx <- length(xtileind)
  ny <- length(ytileind)
  
  
  xrng <- range(tileind$bbox[, 1])
  yrng <- range(tileind$bbox[, 2])  
  plot(xrng, yrng, type = "n", xlab = "Longitude", ylab = "Latitude", 
       xaxs='i', yaxs='i', asp=1, axes=FALSE)
  

  yiter <- 0
  for(y in ytileind){
    yiter <- yiter + 1
    xiter <- 0
    for(x in xtileind){
      xiter <- xiter + 1
      addr <- paste("http://i1.maps.daum-img.net/map/image/G03/i/2200keery/L", z, "/", y, "/", x, ".png", sep="")
      download.file(addr, "~/test.png", quiet = TRUE)
      
      
      if(GRAYSCALE){
        test <- readPNG("~/test.png", native = FALSE)
        test <- RGB2GRAY(test)
        writePNG(test, "~/test.png")
        test <- readPNG("~/test.png", native = FALSE)
      } else {
        test <- readPNG("~/test.png")
      }
      
      rasterImage(test, tileind$bbox$x[xiter + (nx + 1)*(yiter - 1)], 
                  tileind$bbox$y[yiter + (nx + 1)*(yiter - 1)], 
                  tileind$bbox$x[xiter + (nx + 1)*(yiter - 1) + 1], 
                  tileind$bbox$y[yiter + (nx + 1)*yiter],
                  interpolate=TRUE)  
    }
  }
}


WGS842Daum <- function(lanepath, raw=FALSE){
  require(rgdal)
  lanepath <- as.data.frame(lanepath)
  colnames(lanepath) <- c("x", "y")
  coordinates(lanepath) <- c("x", "y")
  proj4string(lanepath) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  lanepath <- spTransform(lanepath, CRS("+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=GRS80 +units=m +no_defs"))
  xy <- coordinates(lanepath)
  if(raw){
    xy[, 1] <- xy[, 1] * 2.5
    xy[, 2] <- xy[, 2] * 2.5
  }
  return(xy)
}

Daum2WGS84 <- function(xy){
    xy <- data.frame(x = as.numeric(xy[,1])/2.5,
                     y = as.numeric(xy[,2])/2.5)
    
    coordinates(xy) <- c("x", "y")
    proj4string(xy) <- CRS("+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=GRS80 +units=m +no_defs")
    xywgs84 <- spTransform(xy, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
    coordinates(xywgs84)
}
