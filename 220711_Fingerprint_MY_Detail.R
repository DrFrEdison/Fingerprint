# detail 1 ####
detaillist <- list()
detaillist$detail[[1]] <- c(18000, 25000)
detaillist$detail[[2]] <- c(19000, 19750)
detaillist$detail[[3]] <- c(19200, 19500)

setwd( bev$wd)
dir.create( "Mannheim_MY")
setwd("./Mannheim_MY")
dir.create( "detail1")
setwd("./detail1")

bev$para$txt$spc$loc.line
i = 2

bev$pca$fp$spc[[ 2  ]]
png(paste0(date(),"_Mannheim_MY_Detail1_Fingerprint_SPC.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
plot( as.numeric( bev$pca$fp$spc[[ 2  ]]$calres$scores[ , 1] )
      , axes = F
      , xlab = ""
      , ylab = "PC 1"
      , main = "Mannheim MY Fingerprint PCA - PC1"
      , ylim = c(-1, 1)
      , xlim = c(detaillist$detail[[1]][1], detaillist$detail[[1]][2])
      , pch = 20)
xaxisdate( bev$raw$spc$Mannheim_MY$datetime, type = "n", las = 2)
dev.off()

png(paste0(date(),"_Mannheim_MY_Detail1_Fingerprint_PCA_zoom_y_x.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
plot(bev$pca$fp$spc[[ 2  ]]$calres$scores[ , 1], axes = F
     , xlab = ""
     , ylab = "PC 1"
     , main = "Mannheim MY Fingerprint PCA - PC1"
     , ylim = c(-.7, .1)
     , xlim = c(detaillist$detail[[1]][1], detaillist$detail[[1]][2])
     , pch = 20)
xaxisdate( bev$raw$spc$Mannheim_MY$datetime, type = "n", las = 2)
dev.off()

png(paste0(date(),"_Mannheim_MY_Detail1_Fingerprint_PCA_zoom_y_x2.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
plot(as.POSIXct(bev$raw$spc$Mannheim_MY$datetime[ - dat.row.sum[[ i ]] ][c(detaillist$detail[[2]][1] : detaillist$detail[[2]][2])])
     , bev$pca$fp$spc[[ 2  ]]$calres$scores[ c(detaillist$detail[[2]][1] : detaillist$detail[[2]][2])  , 1], axes = T
     , xlab = ""
     , ylab = "PC 1"
     , main = "Mannheim MY Fingerprint PCA - PC1"
     , ylim = c(-.7, .1)
     , pch = 20)
abline( v = as.POSIXct(bev$raw$ref$Mannheim_MY$datetime, tz = "UTC" ), lty = 3)
dev.off()

png(paste0(date(),"_Mannheim_MY_Detail1_Fingerprint_PCA_zoom_y_x3.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
plot(as.POSIXct(bev$raw$spc$Mannheim_MY$datetime[ - dat.row.sum[[ i ]] ][c(detaillist$detail[[3]][1] : detaillist$detail[[3]][2])])
     , bev$pca$fp$spc[[ 2  ]]$calres$scores[ c(detaillist$detail[[3]][1] : detaillist$detail[[3]][2])  , 1], axes = T
     , xlab = ""
     , ylab = "PC 1"
     , main = "Mannheim MY Fingerprint PCA - PC1"
     , ylim = c(-1, .1)
     , pch = 20)
abline( v = as.POSIXct(bev$raw$ref$Mannheim_MY$datetime, tz = "UTC" ), lty = 3)
legend("topright", "Referenz/Dunkelwert", lty = 3)
dev.off()

bev$raw$ref$Mannheim_MY$datetime
ii = 3
fp.detail.1 <- list()
fp.detail.1$ref <- as.POSIXct(bev$raw$ref$Mannheim_MY$datetime, tz = "UTC" )[ 
  which( as.POSIXct(bev$raw$ref$Mannheim_MY$datetime, tz = "UTC" ) > range(as.POSIXct(bev$raw$spc$Mannheim_MY$datetime[ - dat.row.sum[[ i ]] ][c(detaillist$detail[[ii]][1] : detaillist$detail[[ii]][2])]))[1] &
           as.POSIXct(bev$raw$ref$Mannheim_MY$datetime, tz = "UTC" ) < range(as.POSIXct(bev$raw$spc$Mannheim_MY$datetime[ - dat.row.sum[[ i ]] ][c(detaillist$detail[[ii]][1] : detaillist$detail[[ii]][2])]))[2])
]
fp.detail.1$drk <- as.POSIXct(bev$raw$drk$Mannheim_MY$datetime, tz = "UTC" )[ 
  which( as.POSIXct(bev$raw$drk$Mannheim_MY$datetime, tz = "UTC" ) > range(as.POSIXct(bev$raw$spc$Mannheim_MY$datetime[ - dat.row.sum[[ i ]] ][c(detaillist$detail[[ii]][1] : detaillist$detail[[ii]][2])]))[1] &
           as.POSIXct(bev$raw$drk$Mannheim_MY$datetime, tz = "UTC" ) < range(as.POSIXct(bev$raw$spc$Mannheim_MY$datetime[ - dat.row.sum[[ i ]] ][c(detaillist$detail[[ii]][1] : detaillist$detail[[ii]][2])]))[2])
]
fp.detail.1$spc <- as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC" )[ 
  which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC" ) > range(as.POSIXct(bev$raw$spc$Mannheim_MY$datetime[ - dat.row.sum[[ i ]] ][c(detaillist$detail[[ii]][1] : detaillist$detail[[ii]][2])]))[1] &
           as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC" ) < range(as.POSIXct(bev$raw$spc$Mannheim_MY$datetime[ - dat.row.sum[[ i ]] ][c(detaillist$detail[[ii]][1] : detaillist$detail[[ii]][2])]))[2])
]

png(paste0(date(),"_Mannheim_MY_Detail1_Dunkelwertspektren.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
matplot( bev$para$wl
         , t( bev$raw$drk$Mannheim_MY[ which( as.POSIXct(bev$raw$drk$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.1$drk ) , bev$ppp$drk$Mannheim_MY$numcol, with = F ])
         , lty = 1, type = "l"
         , xlab = lambda, ylab = "Counts", main = "Dunkelwertspektren in Mannheim MY"
         , col = rainbow(6))
legend("topright", as.character(fp.detail.1$drk), lty = 1, col = rainbow(6))
dev.off()

png(paste0(date(),"_Mannheim_MY_Detail1_Referenzspektren.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
matplot( bev$para$wl
         , t( bev$raw$ref$Mannheim_MY[ which( as.POSIXct(bev$raw$ref$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.1$ref ) , bev$ppp$ref$Mannheim_MY$numcol, with = F ])
         , lty = 1, type = "l"
         , xlab = lambda, ylab = "Counts", main = "Dunkelwertspektren in Mannheim MY"
         , col = rainbow(6))
legend("topright", as.character(fp.detail.1$ref), lty = 1, col = rainbow(6))
dev.off()

require(colorRamps)
fp.detail.1$colp$spc <-  blue2red(length( fp.detail.1$spc))
png(paste0(date(),"_Mannheim_MY_Detail1_Produktionsspektren.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
par( mar = c(5,4,4,7))
matplot( bev$para$wl
         , t( bev$raw$spc$Mannheim_MY[ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.1$spc ) , bev$ppp$spc$Mannheim_MY$numcol, with = F ])
         , lty = 1, type = "l"
         , xlab = lambda, ylab = "Counts", main = "Produktionsspektren in Mannheim MY"
         , col = fp.detail.1$colp$spc
         , xlim = c(210, 350)
         , ylim = c(.1, .8))

legend_image <- as.raster(matrix(fp.detail.1$colp$spc, ncol=1))

x1 <- par("usr")[2] #- diff(par("usr")[c(1,2)]) * .3
x2 <- x1 + diff(par("usr")[c(1,2)]) * .05
y1 <- par("usr")[4] - diff(par("usr")[c(3,4)]) * .1
y2 <- .3

rasterImage(legend_image, x1 ,y1, x2, y2, xpd = T)
segments(x2
         , y3 <- seq(y1, y2, len = 5)
         , x3 <- x2 + diff(par("usr")[c(1,2)]) * .025
         , y3, xpd = T)
rect(x1,y1,x2,y2, xpd = T)
text(x3, y3 #+ diff(par("usr")[c(3,4)]) * .05
     , rev(paste(round((cumsum(c(0, diff(fp.detail.1$spc))) / 60 / 60)[seq(1, length(cumsum(c(0, diff(fp.detail.1$spc))) / 60 / 60), len = 5)] , 1), "h"))
     , adj = 0, xpd = T)
dev.off()

fp.detail.1$data <- lg3.status(data = bev$raw$spc$Mannheim_MY )

png(paste0(date(),"_Mannheim_MY_Detail1_Status.png"),xxx<-4800 / 1,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
par(mfrow = c(3,4), mar = c(3,3,2,1))
for(iii in 1 : ncol(fp.detail.1$data$data)){
  
  if(iii == ncol(fp.detail.1$data$data)) next
  
  plot( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC")[ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.1$spc ) ]
        , as.numeric(unlist(fp.detail.1$data$data[ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.1$spc ) , iii, with = F]))
        , main = fp.detail.1$data$names[iii], xlab = "", ylab = fp.detail.1$data$names[iii])
  abline( v = as.POSIXct(bev$raw$ref$Mannheim_MY$datetime[ - dat.row.sum[[ i ]]], tz = "UTC" ), lty = 3)
  
  par(new = T)
  
  plot( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC")[ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.1$spc ) ]
        , as.numeric(unlist(bev$pca$fp$spc[[ 2  ]]$calres$scores[ , 1][ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.1$spc )]))
        , col = "blue", pch = 20, cex = .3, xlab = "", ylab = "", axes = F)
}

trs <- transfer_csv(bev$raw$spc$Mannheim_MY[ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.1$spc ) , ])

GS2 <- use_model_on_device("CCEP", "Coca_Cola", "3", "GS2", trs)
Coffein <- use_model_on_device("CCEP", "Coca_Cola", "3", "Coffein", trs)

plot(as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC")[ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.1$spc ) ]
     , GS2, main = "GS2", xlab = "", ylab = "GS2 in %")
abline( v = as.POSIXct(bev$raw$ref$Mannheim_MY$datetime[ - dat.row.sum[[ i ]]], tz = "UTC" ), lty = 3)

par(new = T)

plot( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC")[ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.1$spc ) ]
      , as.numeric(unlist(bev$pca$fp$spc[[ 2  ]]$calres$scores[ , 1][ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.1$spc )]))
      , col = "blue", pch = 20, cex = .3, xlab = "", ylab = "", axes = F)

plot(as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC")[ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.1$spc ) ]
     , Coffein, main = "Coffein", xlab = "", ylab = "Coffein in %")
abline( v = as.POSIXct(bev$raw$ref$Mannheim_MY$datetime[ - dat.row.sum[[ i ]]], tz = "UTC" ), lty = 3)

par(new = T)

plot( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC")[ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.1$spc ) ]
      , as.numeric(unlist(bev$pca$fp$spc[[ 2  ]]$calres$scores[ , 1][ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.1$spc )]))
      , col = "blue", pch = 20, cex = .3, xlab = "", ylab = "", axes = F)

dev.off()  



# detail 2 ####
detaillist <- list()
detaillist$detail[[4]] <- c(23000, 25000)
detaillist$detail[[5]] <- c(23200, 24200)
detaillist$detail[[6]] <- c(23200, 23800)

setwd( bev$wd)
dir.create( "Mannheim_MY")
setwd("./Mannheim_MY")
dir.create( "Detail2")
setwd("./Detail2")

bev$para$txt$spc$loc.line
i = 2

bev$pca$fp$spc[[ 2  ]]
png(paste0(date(),"_Mannheim_MY_Detail2_Fingerprint_SPC.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
plot( as.numeric( bev$pca$fp$spc[[ 2  ]]$calres$scores[ , 1] )
      , axes = F
      , xlab = ""
      , ylab = "PC 1"
      , main = "Mannheim MY Fingerprint PCA - PC1"
      , ylim = c(-1, 1)
      , xlim = c(detaillist$detail[[4]][1], detaillist$detail[[4]][2])
      , pch = 20)
xaxisdate( bev$raw$spc$Mannheim_MY$datetime, type = "n", las = 2)
dev.off()

png(paste0(date(),"_Mannheim_MY_Detail2_Fingerprint_PCA_zoom_y_x.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
plot(bev$pca$fp$spc[[ 2  ]]$calres$scores[ , 1], axes = F
     , xlab = ""
     , ylab = "PC 1"
     , main = "Mannheim MY Fingerprint PCA - PC1"
     , ylim = c(-.7, .1)
     , xlim = c(detaillist$detail[[4]][1], detaillist$detail[[4]][2])
     , pch = 20)
xaxisdate( bev$raw$spc$Mannheim_MY$datetime, type = "n", las = 2)
dev.off()

png(paste0(date(),"_Mannheim_MY_Detail2_Fingerprint_PCA_zoom_y_x2.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
plot(as.POSIXct(bev$raw$spc$Mannheim_MY$datetime[ - dat.row.sum[[ i ]] ][c(detaillist$detail[[5]][1] : detaillist$detail[[5]][2])])
     , bev$pca$fp$spc[[ 2  ]]$calres$scores[ c(detaillist$detail[[5]][1] : detaillist$detail[[5]][2])  , 1], axes = T
     , xlab = ""
     , ylab = "PC 1"
     , main = "Mannheim MY Fingerprint PCA - PC1"
     , ylim = c(-.8, -.4)
     , pch = 20)
abline( v = as.POSIXct(bev$raw$ref$Mannheim_MY$datetime, tz = "UTC" ), lty = 3)
dev.off()

png(paste0(date(),"_Mannheim_MY_Detail2_Fingerprint_PCA_zoom_y_x3.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
plot(as.POSIXct(bev$raw$spc$Mannheim_MY$datetime[ - dat.row.sum[[ i ]] ][c(detaillist$detail[[6]][1] : detaillist$detail[[6]][2])])
     , bev$pca$fp$spc[[ 2  ]]$calres$scores[ c(detaillist$detail[[6]][1] : detaillist$detail[[6]][2])  , 1], axes = T
     , xlab = ""
     , ylab = "PC 1"
     , main = "Mannheim MY Fingerprint PCA - PC1"
     , ylim = c(-.8, -.4)
     , pch = 20)
abline( v = as.POSIXct(bev$raw$ref$Mannheim_MY$datetime, tz = "UTC" ), lty = 3)
legend("topright", "Referenz/Dunkelwert", lty = 3)
dev.off()

bev$raw$ref$Mannheim_MY$datetime
ii = 6
fp.detail.2 <- list()
fp.detail.2$ref <- as.POSIXct(bev$raw$ref$Mannheim_MY$datetime, tz = "UTC" )[ 
  which( as.POSIXct(bev$raw$ref$Mannheim_MY$datetime, tz = "UTC" ) > range(as.POSIXct(bev$raw$spc$Mannheim_MY$datetime[ - dat.row.sum[[ i ]] ][c(detaillist$detail[[ii]][1] : detaillist$detail[[ii]][2])]))[1] &
           as.POSIXct(bev$raw$ref$Mannheim_MY$datetime, tz = "UTC" ) < range(as.POSIXct(bev$raw$spc$Mannheim_MY$datetime[ - dat.row.sum[[ i ]] ][c(detaillist$detail[[ii]][1] : detaillist$detail[[ii]][2])]))[2])
]
fp.detail.2$drk <- as.POSIXct(bev$raw$drk$Mannheim_MY$datetime, tz = "UTC" )[ 
  which( as.POSIXct(bev$raw$drk$Mannheim_MY$datetime, tz = "UTC" ) > range(as.POSIXct(bev$raw$spc$Mannheim_MY$datetime[ - dat.row.sum[[ i ]] ][c(detaillist$detail[[ii]][1] : detaillist$detail[[ii]][2])]))[1] &
           as.POSIXct(bev$raw$drk$Mannheim_MY$datetime, tz = "UTC" ) < range(as.POSIXct(bev$raw$spc$Mannheim_MY$datetime[ - dat.row.sum[[ i ]] ][c(detaillist$detail[[ii]][1] : detaillist$detail[[ii]][2])]))[2])
]
fp.detail.2$spc <- as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC" )[ 
  which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC" ) > range(as.POSIXct(bev$raw$spc$Mannheim_MY$datetime[ - dat.row.sum[[ i ]] ][c(detaillist$detail[[ii]][1] : detaillist$detail[[ii]][2])]))[1] &
           as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC" ) < range(as.POSIXct(bev$raw$spc$Mannheim_MY$datetime[ - dat.row.sum[[ i ]] ][c(detaillist$detail[[ii]][1] : detaillist$detail[[ii]][2])]))[2])
]

png(paste0(date(),"_Mannheim_MY_Detail2_Dunkelwertspektren.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
matplot( bev$para$wl
         , t( bev$raw$drk$Mannheim_MY[ which( as.POSIXct(bev$raw$drk$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.2$drk ) , bev$ppp$drk$Mannheim_MY$numcol, with = F ])
         , lty = 1, type = "l"
         , xlab = lambda, ylab = "Counts", main = "Dunkelwertspektren in Mannheim MY"
         , col = rainbow(6))
legend("topright", as.character(fp.detail.2$drk), lty = 1, col = rainbow(6))
dev.off()

png(paste0(date(),"_Mannheim_MY_Detail2_Referenzspektren.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
matplot( bev$para$wl
         , t( bev$raw$ref$Mannheim_MY[ which( as.POSIXct(bev$raw$ref$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.2$ref ) , bev$ppp$ref$Mannheim_MY$numcol, with = F ])
         , lty = 1, type = "l"
         , xlab = lambda, ylab = "Counts", main = "Dunkelwertspektren in Mannheim MY"
         , col = rainbow(6))
legend("topright", as.character(fp.detail.2$ref), lty = 1, col = rainbow(6))
dev.off()

require(colorRamps)
fp.detail.2$colp$spc <-  blue2red(length( fp.detail.2$spc))
png(paste0(date(),"_Mannheim_MY_Detail2_Produktionsspektren.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
par( mar = c(5,4,4,7))
matplot( bev$para$wl
         , t( bev$raw$spc$Mannheim_MY[ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.2$spc ) , bev$ppp$spc$Mannheim_MY$numcol, with = F ])
         , lty = 1, type = "l"
         , xlab = lambda, ylab = "Counts", main = "Produktionsspektren in Mannheim MY"
         , col = fp.detail.2$colp$spc
         , xlim = c(210, 350)
         , ylim = c(.1, .8))

legend_image <- as.raster(matrix(fp.detail.2$colp$spc, ncol=1))

x1 <- par("usr")[2] #- diff(par("usr")[c(1,2)]) * .3
x2 <- x1 + diff(par("usr")[c(1,2)]) * .05
y1 <- par("usr")[4] - diff(par("usr")[c(3,4)]) * .1
y2 <- .3

rasterImage(legend_image, x1 ,y1, x2, y2, xpd = T)
segments(x2
         , y3 <- seq(y1, y2, len = 5)
         , x3 <- x2 + diff(par("usr")[c(1,2)]) * .025
         , y3, xpd = T)
rect(x1,y1,x2,y2, xpd = T)
text(x3, y3 #+ diff(par("usr")[c(3,4)]) * .05
     , rev(paste(round((cumsum(c(0, diff(fp.detail.2$spc))) / 60 / 60)[seq(1, length(cumsum(c(0, diff(fp.detail.2$spc))) / 60 / 60), len = 5)] , 1), "h"))
     , adj = 0, xpd = T)
dev.off()

fp.detail.2$data <- lg3.status(data = bev$raw$spc$Mannheim_MY )

png(paste0(date(),"_Mannheim_MY_Detail2_Status.png"),xxx<-4800 / 1,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
par(mfrow = c(3,4), mar = c(3,3,2,1))
for(iii in 1 : ncol(fp.detail.2$data$data)){
  
  if(iii == ncol(fp.detail.2$data$data)) next
  
  plot( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC")[ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.2$spc ) ]
        , as.numeric(unlist(fp.detail.2$data$data[ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.2$spc ) , iii, with = F]))
        , main = fp.detail.2$data$names[iii], xlab = "", ylab = fp.detail.2$data$names[iii])
  abline( v = as.POSIXct(bev$raw$ref$Mannheim_MY$datetime[ - dat.row.sum[[ i ]]], tz = "UTC" ), lty = 3)
  
  par(new = T)
  
  plot( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC")[ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.2$spc ) ]
        , as.numeric(unlist(bev$pca$fp$spc[[ 2  ]]$calres$scores[ , 1][ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.2$spc )]))
        , col = "blue", pch = 20, cex = .3, xlab = "", ylab = "", axes = F)
}

trs <- transfer_csv(bev$raw$spc$Mannheim_MY[ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.2$spc ) , ])

GS2 <- use_model_on_device("CCEP", "Coca_Cola", "3", "GS2", trs)
Coffein <- use_model_on_device("CCEP", "Coca_Cola", "3", "Coffein", trs)

plot(as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC")[ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.2$spc ) ]
     , GS2, main = "GS2", xlab = "", ylab = "GS2 in %")
abline( v = as.POSIXct(bev$raw$ref$Mannheim_MY$datetime[ - dat.row.sum[[ i ]]], tz = "UTC" ), lty = 3)

par(new = T)

plot( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC")[ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.2$spc ) ]
      , as.numeric(unlist(bev$pca$fp$spc[[ 2  ]]$calres$scores[ , 1][ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.2$spc )]))
      , col = "blue", pch = 20, cex = .3, xlab = "", ylab = "", axes = F)

plot(as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC")[ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.2$spc ) ]
     , Coffein, main = "Coffein", xlab = "", ylab = "Coffein in %")
abline( v = as.POSIXct(bev$raw$ref$Mannheim_MY$datetime[ - dat.row.sum[[ i ]]], tz = "UTC" ), lty = 3)

par(new = T)

plot( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC")[ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.2$spc ) ]
      , as.numeric(unlist(bev$pca$fp$spc[[ 2  ]]$calres$scores[ , 1][ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.2$spc )]))
      , col = "blue", pch = 20, cex = .3, xlab = "", ylab = "", axes = F)

dev.off()  

# detail 3 ####
detaillist <- list()
detaillist$detail[[7]] <- c(28200, 29000)
detaillist$detail[[8]] <- c(28300, 28800)
detaillist$detail[[9]] <- c(28300, 28800)

setwd( bev$wd)
dir.create( "Mannheim_MY")
setwd("./Mannheim_MY")
dir.create( "Detail3")
setwd("./Detail3")

bev$para$txt$spc$loc.line
i = 2

png(paste0(date(),"_Mannheim_MY_Detail3_Fingerprint_SPC.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
plot( as.numeric( bev$pca$fp$spc[[ 2  ]]$calres$scores[ , 1] )
      , axes = F
      , xlab = ""
      , ylab = "PC 1"
      , main = "Mannheim MY Fingerprint PCA - PC1"
      , ylim = c(-1, 1)
      , xlim = c(detaillist$detail[[7]][1], detaillist$detail[[7]][2])
      , pch = 20)
xaxisdate( bev$raw$spc$Mannheim_MY$datetime, type = "n", las = 2)
dev.off()

png(paste0(date(),"_Mannheim_MY_Detail3_Fingerprint_PCA_zoom_y_x.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
plot(bev$pca$fp$spc[[ 2  ]]$calres$scores[ , 1], axes = F
     , xlab = ""
     , ylab = "PC 1"
     , main = "Mannheim MY Fingerprint PCA - PC1"
     , ylim = c(-.7, .1)
     , xlim = c(detaillist$detail[[7]][1], detaillist$detail[[7]][2])
     , pch = 20)
xaxisdate( bev$raw$spc$Mannheim_MY$datetime, type = "n", las = 2)
dev.off()

png(paste0(date(),"_Mannheim_MY_Detail3_Fingerprint_PCA_zoom_y_x2.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
plot(as.POSIXct(bev$raw$spc$Mannheim_MY$datetime[ - dat.row.sum[[ i ]] ][c(detaillist$detail[[8]][1] : detaillist$detail[[8]][2])])
     , bev$pca$fp$spc[[ 2  ]]$calres$scores[ c(detaillist$detail[[8]][1] : detaillist$detail[[8]][2])  , 1], axes = T
     , xlab = ""
     , ylab = "PC 1"
     , main = "Mannheim MY Fingerprint PCA - PC1"
     , ylim = c(-.7, .1)
     , pch = 20)
abline( v = as.POSIXct(bev$raw$ref$Mannheim_MY$datetime, tz = "UTC" ), lty = 3)
dev.off()

png(paste0(date(),"_Mannheim_MY_Detail3_Fingerprint_PCA_zoom_y_x3.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
plot(as.POSIXct(bev$raw$spc$Mannheim_MY$datetime[ - dat.row.sum[[ i ]] ][c(detaillist$detail[[9]][1] : detaillist$detail[[9]][2])])
     , bev$pca$fp$spc[[ 2  ]]$calres$scores[ c(detaillist$detail[[9]][1] : detaillist$detail[[9]][2])  , 1], axes = T
     , xlab = ""
     , ylab = "PC 1"
     , main = "Mannheim MY Fingerprint PCA - PC1"
     , ylim = c(-.7, .1)
     , pch = 20)
abline( v = as.POSIXct(bev$raw$ref$Mannheim_MY$datetime, tz = "UTC" ), lty = 3)
legend("topright", "Referenz/Dunkelwert", lty = 3)
dev.off()

bev$raw$ref$Mannheim_MY$datetime
ii = 9
fp.detail.3 <- list()
fp.detail.3$ref <- as.POSIXct(bev$raw$ref$Mannheim_MY$datetime, tz = "UTC" )[ 
  which( as.POSIXct(bev$raw$ref$Mannheim_MY$datetime, tz = "UTC" ) > range(as.POSIXct(bev$raw$spc$Mannheim_MY$datetime[ - dat.row.sum[[ i ]] ][c(detaillist$detail[[ii]][1] : detaillist$detail[[ii]][2])]))[1] &
           as.POSIXct(bev$raw$ref$Mannheim_MY$datetime, tz = "UTC" ) < range(as.POSIXct(bev$raw$spc$Mannheim_MY$datetime[ - dat.row.sum[[ i ]] ][c(detaillist$detail[[ii]][1] : detaillist$detail[[ii]][2])]))[2])
]
fp.detail.3$drk <- as.POSIXct(bev$raw$drk$Mannheim_MY$datetime, tz = "UTC" )[ 
  which( as.POSIXct(bev$raw$drk$Mannheim_MY$datetime, tz = "UTC" ) > range(as.POSIXct(bev$raw$spc$Mannheim_MY$datetime[ - dat.row.sum[[ i ]] ][c(detaillist$detail[[ii]][1] : detaillist$detail[[ii]][2])]))[1] &
           as.POSIXct(bev$raw$drk$Mannheim_MY$datetime, tz = "UTC" ) < range(as.POSIXct(bev$raw$spc$Mannheim_MY$datetime[ - dat.row.sum[[ i ]] ][c(detaillist$detail[[ii]][1] : detaillist$detail[[ii]][2])]))[2])
]
fp.detail.3$spc <- as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC" )[ 
  which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC" ) > range(as.POSIXct(bev$raw$spc$Mannheim_MY$datetime[ - dat.row.sum[[ i ]] ][c(detaillist$detail[[ii]][1] : detaillist$detail[[ii]][2])]))[1] &
           as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC" ) < range(as.POSIXct(bev$raw$spc$Mannheim_MY$datetime[ - dat.row.sum[[ i ]] ][c(detaillist$detail[[ii]][1] : detaillist$detail[[ii]][2])]))[2])
]

png(paste0(date(),"_Mannheim_MY_Detail3_Dunkelwertspektren.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
matplot( bev$para$wl
         , t( bev$raw$drk$Mannheim_MY[ which( as.POSIXct(bev$raw$drk$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.3$drk ) , bev$ppp$drk$Mannheim_MY$numcol, with = F ])
         , lty = 1, type = "l"
         , xlab = lambda, ylab = "Counts", main = "Dunkelwertspektren in Mannheim MY"
         , col = rainbow(6))
legend("topright", as.character(fp.detail.3$drk), lty = 1, col = rainbow(6))
dev.off()

png(paste0(date(),"_Mannheim_MY_Detail3_Referenzspektren.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
matplot( bev$para$wl
         , t( bev$raw$ref$Mannheim_MY[ which( as.POSIXct(bev$raw$ref$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.3$ref ) , bev$ppp$ref$Mannheim_MY$numcol, with = F ])
         , lty = 1, type = "l"
         , xlab = lambda, ylab = "Counts", main = "Dunkelwertspektren in Mannheim MY"
         , col = rainbow(6))
legend("topright", as.character(fp.detail.3$ref), lty = 1, col = rainbow(6))
dev.off()

require(colorRamps)
fp.detail.3$colp$spc <-  blue2red(length( fp.detail.3$spc))
png(paste0(date(),"_Mannheim_MY_Detail3_Produktionsspektren.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
par( mar = c(5,4,4,7))
matplot( bev$para$wl
         , t( bev$raw$spc$Mannheim_MY[ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.3$spc ) , bev$ppp$spc$Mannheim_MY$numcol, with = F ])
         , lty = 1, type = "l"
         , xlab = lambda, ylab = "Counts", main = "Produktionsspektren in Mannheim MY"
         , col = fp.detail.3$colp$spc
         , xlim = c(210, 350)
         , ylim = c(.1, .8))

legend_image <- as.raster(matrix(fp.detail.3$colp$spc, ncol=1))

x1 <- par("usr")[2] #- diff(par("usr")[c(1,2)]) * .3
x2 <- x1 + diff(par("usr")[c(1,2)]) * .05
y1 <- par("usr")[4] - diff(par("usr")[c(3,4)]) * .1
y2 <- .3

rasterImage(legend_image, x1 ,y1, x2, y2, xpd = T)
segments(x2
         , y3 <- seq(y1, y2, len = 5)
         , x3 <- x2 + diff(par("usr")[c(1,2)]) * .025
         , y3, xpd = T)
rect(x1,y1,x2,y2, xpd = T)
text(x3, y3 #+ diff(par("usr")[c(3,4)]) * .05
     , rev(paste(round((cumsum(c(0, diff(fp.detail.3$spc))) / 60 / 60)[seq(1, length(cumsum(c(0, diff(fp.detail.3$spc))) / 60 / 60), len = 5)] , 1), "h"))
     , adj = 0, xpd = T)
dev.off()

fp.detail.3$data <- lg3.status(data = bev$raw$spc$Mannheim_MY )

png(paste0(date(),"_Mannheim_MY_Detail3_Status.png"),xxx<-4800 / 1,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
par(mfrow = c(3,4), mar = c(3,3,2,1))
for(iii in 1 : ncol(fp.detail.3$data$data)){
  
  if(iii == ncol(fp.detail.3$data$data)) next
  
  plot( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC")[ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.3$spc ) ]
        , as.numeric(unlist(fp.detail.3$data$data[ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.3$spc ) , iii, with = F]))
        , main = fp.detail.3$data$names[iii], xlab = "", ylab = fp.detail.3$data$names[iii])
  abline( v = as.POSIXct(bev$raw$ref$Mannheim_MY$datetime[ - dat.row.sum[[ i ]]], tz = "UTC" ), lty = 3)
  
  par(new = T)
  
  plot( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC")[ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.3$spc ) ]
        , as.numeric(unlist(bev$pca$fp$spc[[ 2  ]]$calres$scores[ , 1][ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.3$spc )]))
        , col = "blue", pch = 20, cex = .3, xlab = "", ylab = "", axes = F)
}

trs <- transfer_csv(bev$raw$spc$Mannheim_MY[ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.3$spc ) , ])

GS2 <- use_model_on_device("CCEP", "Coca_Cola", "3", "GS2", trs)
Coffein <- use_model_on_device("CCEP", "Coca_Cola", "3", "Coffein", trs)

plot(as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC")[ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.3$spc ) ]
     , GS2, main = "GS2", xlab = "", ylab = "GS2 in %")
abline( v = as.POSIXct(bev$raw$ref$Mannheim_MY$datetime[ - dat.row.sum[[ i ]]], tz = "UTC" ), lty = 3)

par(new = T)

plot( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC")[ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.3$spc ) ]
      , as.numeric(unlist(bev$pca$fp$spc[[ 2  ]]$calres$scores[ , 1][ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.3$spc )]))
      , col = "blue", pch = 20, cex = .3, xlab = "", ylab = "", axes = F)

plot(as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC")[ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.3$spc ) ]
     , Coffein, main = "Coffein", xlab = "", ylab = "Coffein in %")
abline( v = as.POSIXct(bev$raw$ref$Mannheim_MY$datetime[ - dat.row.sum[[ i ]]], tz = "UTC" ), lty = 3)

par(new = T)

plot( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC")[ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.3$spc ) ]
      , as.numeric(unlist(bev$pca$fp$spc[[ 2  ]]$calres$scores[ , 1][ which( as.POSIXct(bev$raw$spc$Mannheim_MY$datetime, tz = "UTC") %in% fp.detail.3$spc )]))
      , col = "blue", pch = 20, cex = .3, xlab = "", ylab = "", axes = F)

dev.off()  
