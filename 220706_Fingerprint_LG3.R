#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#
###                         ###
###  ######       ########  ###
###  ##    ##     ########  ###
###  ##     ##       ##     ###
###  ##     ##       ##     ###
###  ##     ##       ##     ###
###  ##     ##       ##     ###
###  ##    ##        ##     ###
###  ######          ##     ###
###                         ###
#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#
###                         ###
### By Markus Kurtz         ###
### For Dausch Technologies ###
### 2022                    ###
###                         ###
#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#
###
###
### Script is a template to analyse spectra from LG systems, data exported from Backup
###
###
### required functions and packages are loaded from source_read.R and source_pls.R

# load all required functions and packages ####
library(r4dt)

# set working directory ####
setwd(wd$csvtemp)

# list and read files ####
bev <- list()
bev$wd <- getwd()
bev$para$customer = "CCEP"
bev$para$beverage <- "Coca_Cola"

setwd(paste0(bev$wd.model <- paste0(wd$fe[[ grep(bev$para$customer, names(wd$fe)) ]]$Mastermodelle, bev$para$beverage)))
setwd( bev$wd <- wd$fe$fp )
bev$wd.git <- paste(wd$git, bev$para$customer, bev$para$beverage, sep = "/")

bev$para$date <- date()
bev$para$wl1 <- c(190)
bev$para$wl2 <- c(598)
bev$para$wl <- seq(bev$para$wl1, bev$para$wl2, 1)

# Unit ####
bev$para$substance <- c("Coffein", "GS2")
bev$para$unit <- c( bquote("%"),  bquote("%"))
bev$para$ylab <- c(bquote("Coffein in %"), bquote("GS2 in %"))

# Rezept und SOLL-Werte ####
setwd( paste0( bev$wd.model, "/", "/Rezept") )
bev$rez <- read.xlsx(grep(".xlsx", dir( paste0( bev$wd.model, "/", "/Rezept")), value = T)[ length(grep(".xlsx", dir( paste0( bev$wd.model, "/", "/Rezept")), value = F))])
bev$rez[ grep("Messparameter", bev$rez[ , 3]): nrow(bev$rez) , ]
bev$para$SOLL <- c(100, 100)
bev$para$eingriff <- data.frame( Coffein = c(100 - 1.1/57*100, 100 + 1.1/57*100)
                                 , GS = c(100 - 2/100*100, 100 + 2/10*100))

bev$para$sperr <- data.frame( Coffein = c(100 - 1.7/57*100, 100 + 1.7/57*100)
                              , GS = c(100 - 4/100*100, 100 + 4/10*100))

# List files ####
setwd(bev$wd)
setwd("./spc")
bev$para$files$ref <- grep("ref", dir(pattern = "ref.csv$")[grep(bev$para$beverage, dir(pattern = "ref.csv$"))], value = T) # Background spc
bev$para$files$drk <- grep("rk", dir(pattern = "rk.csv$")[grep(bev$para$beverage, dir(pattern = "rk.csv$"))], value = T) # Dark spc
bev$para$files$spc <- grep("spc", dir(pattern = "spc.csv$")[grep(bev$para$beverage, dir(pattern = "spc.csv$"))], value = T) # Production spc

# subdat <- c(1,2)
# bev$para$files$ref <- bev$para$files$ref[ subdat]
# bev$para$files$drk <- bev$para$files$drk[ subdat]
# bev$para$files$spc <- bev$para$files$spc[ subdat]
# get file info ####
bev$para$txt <- lapply(bev$para$files, txt.file)

# read files ####
bev$raw$ref <- lapply(bev$para$files$ref, function(x) fread(x, dec = ",", sep = ";")) # Background spc
bev$raw$drk <- lapply(bev$para$files$drk, function(x) fread(x, dec = ",", sep = ";")) # Dark spc
bev$raw$spc <- lapply(bev$para$files$spc, function(x) fread(x, dec = ",", sep = ";")) # Production spc

bev$raw$spc <- lapply(bev$raw$spc, function( x ) x[ seq( 1, nrow(x), 4), ])

# set names ####
names(bev$raw$ref) <- bev$para$txt$ref$loc.line
names(bev$raw$drk) <- bev$para$txt$drk$loc.line
names(bev$raw$spc) <- bev$para$txt$spc$loc.line

# read wavelength columns ####
bev$ppp <- lapply(bev$raw, function(x) lapply(x, transfer_csv.num.col))

setwd(bev$wd)
setwd("./plot")

# Integrationszeit ####
png(paste0("Integrationszeiten_LG3.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
par( mfrow = c(2,2), mar = c(3, 4, 3, 1))
for(i in 1 : length(bev$raw$spc)){
  
  
  
  plot( bev$raw$ref[[ i ]]$datetime, bev$raw$ref[[ i ]]$integrationTime
        , axes = T, xlab = "", ylab = "Integrationszeit"
        , main = paste("Integrationszeit", bev$para$txt$ref$location[ i ], bev$para$txt$ref$line[ i ])
        , ylim = c(0, 250))
  
}

dev.off()

# Median und FP Referenzen ####
bev$median$ref <- mapply( function( spc, numcol) median_spc( spc, numcol)
                          , spc = bev$raw$ref
                          , numcol = bev$ppp$ref
                          , SIMPLIFY = F)
bev$fp$ref <- mapply( function( spc_ref, spc, numcol) fingerprint( spc_ref, spc, numcol)
                      , spc_ref = bev$median$ref    
                      , spc = bev$raw$ref
                      , numcol = bev$ppp$ref
                      , SIMPLIFY = F)

# Median und FP SPC ####
bev$median$spc <- mapply( function( spc, numcol) median_spc( spc, numcol)
                          , spc = bev$raw$spc
                          , numcol = bev$ppp$spc
                          , SIMPLIFY = F)

bev$median.daily.all <- mapply( function( spc, numcol) median_daily_spc( spc = spc
                                                                         , date = spc$datetime
                                                                         , tz = "UTC"
                                                                         , numcol = numcol)
                                , spc = bev$raw$spc
                                , numcol = bev$ppp$spc
                                , SIMPLIFY = F)

bev$fp$spc <- mapply( function( spc_ref, spc, numcol) fingerprint( spc_ref, spc, numcol)
                      , spc_ref = bev$median$spc
                      , spc = bev$raw$spc
                      , numcol = bev$ppp$spc
                      , SIMPLIFY = F)

# Plot Medianreferenzen ####
setwd(bev$wd)
setwd("./plot")

require("wesanderson")
bev$par$colfunc <- colorRampPalette( c( wes_palettes$Zissou1[1:3] ) )
for(i in 1 : length(bev$raw$ref)) bev$par$fp$ref$colp[[ i ]] <- bev$par$colfunc( nrow( bev$fp$ref[[ i ]] ) )

bev$par$colp.location <- c("blue", "red", "darkgreen", "orange") # [ subdat ]

png(paste0("Medianreferenzen_LG3.png")
    ,xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")

par(mfrow = c(1,1))
matplot(bev$para$wl
        , do.call(cbind, bev$median$ref)
        , type = "l", lty = 1
        , col = bev$par$colp.location
        , xlab = lambda, ylab = "Counts"
        , main = "Medianreferenzen beim LG3, alle Standorte zwischen der letzten und vorletzten Wartung")
legend( "topright", bev$para$txt$ref$loc.line
        , lty = 1, col = bev$par$colp.location)

dev.off()

# Plot Medianreferenzen ####
for(i in 1 : length(bev$raw$spc)){
png(paste0("Fingerprint_Referenzen_", bev$para$txt$spc$loc.line[i], ".png")
    ,xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")

par(mfrow = c(1,1))
matplot(bev$para$wl
        , t( bev$fp$ref[[ i ]])
        , type = "l", lty = 1
        , col = bev$par$fp$ref$colp[[ i ]]
        , xlab = lambda, ylab = "Counts"
        , main = paste("Fingerprint der Referenzen in", bev$para$txt$spc$loc.line[i])
        , ylim = c(-.25, .25))

dev.off()
}

# Fingerprint pca clean up for NA and INF ####
dat.row.sum <- list()
dat.row.inf <- list()
for(i in 1:length(bev$fp$spc)){
  
  dat.row.sum[[ i ]] <- apply(bev$fp$spc[[ i ]][ , bev$para$wl %in% 220:450], 1, sum)
  dat.row.sum[[ i ]] <- which( is.na(dat.row.sum[[ i ]]) )
  
  dat.row.inf[[ i ]] <- apply(bev$fp$spc[[ i ]][ , bev$para$wl %in% 220:450], 1, function(x) any( is.infinite( x)))
  dat.row.inf[[ i ]] <- which( dat.row.inf[[ i ]] )
  
  dat.row.sum[[ i ]] <- unique( sort( c(dat.row.sum[[ i ]], dat.row.inf[[ i ]]) ) )
  
  bev$pca$fp$spc[[ i ]] <- mdatools::pca( bev$fp$spc[[ i ]][ - dat.row.sum[[ i ]], bev$para$wl %in% 220:450] , ncomp = 2)
  
}

# Plot Fingerprintspektren ####
for(i in 1 : length(bev$raw$spc)){
  png(paste0("Fingerprint_Spektren_Cola_", bev$para$txt$spc$loc.line[i], ".png")
      ,xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
  
  par(mfrow = c(1,1))
  matplot(bev$para$wl
          , t( bev$fp$spc[[ i ]])[ , seq(1, nrow( bev$fp$spc[[ i ]] ), 5)]
          , type = "l", lty = 1
          , col = bev$par$fp$ref$colp[[ i ]]
          , xlab = lambda, ylab = "Counts"
          , main = paste("Fingerprint der Cola-Spektren in", bev$para$txt$spc$loc.line[i])
          , ylim = c(-.25, .25))
  
  dev.off()
}

# Plot Fingerprintspektren PCA PC1 ####
png(paste0("Fingerprint_Spektren_Cola_PCA_PC1.png")
    ,xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")
par( mfrow = c(2,2), mar = c(3, 4, 3, 1))
for(i in 1 : length(bev$raw$spc)){
  
  plot( bev$pca$fp$spc[[ i ]]$calres$scores[ , 1]
        , axes = T, xlab = "", ylab = "PC1"
        , main = paste("PC1", bev$para$txt$ref$location[ i ], bev$para$txt$ref$line[ i ])
        , ylim = c(- 1, 1))
  
}

dev.off()



