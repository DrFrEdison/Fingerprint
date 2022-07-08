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

# Read all ref ####
setwd( paste0(wd$servicebackup$CCEP$Mannheim$MY, "/ref") )
bev$raw$refall$Mannheim_MY <- lapply( dir( pattern = "_ref.csv"), function( x ) fread( x, sep = ";", dec = ","))

setwd( paste0(wd$servicebackup$CCEP$Moenchengladbach$G9, "/ref") )
bev$raw$refall$Moenchengladbach_G9 <- lapply( dir( pattern = "_ref.csv"), function( x ) fread( x, sep = ";", dec = ","))

setwd( paste0(wd$servicebackup$CCEP$Dorsten$DC, "/ref") )
bev$raw$refall$Dorsten_DC <- lapply( dir( pattern = "_ref.csv"), function( x ) fread( x, sep = ";", dec = ","))

setwd( paste0(wd$servicebackup$CCEP$Dorsten$DS, "/ref") )
bev$raw$refall$Dorsten_DS <- lapply( dir( pattern = "_ref.csv"), function( x ) fread( x, sep = ";", dec = ","))

bev$para$txt$ref$loc.line <- c("Mannheim_MY", "Moenchengladbach_G9", "Dorsten_DC", "Dorsten_DS")
# Rbind & Numeric columns
bev$raw$refall <- lapply(bev$raw$refall, function( x ) rbindlist(x, fill = T))
bev$ppp <- lapply(bev$raw, function(x) lapply(x, transfer_csv.num.col))

# Daily Median ####
bev$median.daily.all <- mapply( function( spc, numcol) median_daily_spc( spc = spc
                                                                         , date = spc$datetime
                                                                         , tz = "UTC"
                                                                         , numcol = numcol)
                                , spc = bev$raw$refall
                                , numcol = bev$ppp$refall
                                , SIMPLIFY = F)

lapply( bev$median.daily.all, length)
# Plot Integrationszeit ####
setwd(bev$wd)
setwd("./plot")

require("wesanderson")
bev$par$colfunc <- colorRampPalette( c( wes_palettes$Zissou1 ) )
for(i in 1:4) bev$par$colp[[ i ]] <- bev$par$colfunc( length( bev$median.daily.all[[ i ]] ) )

# Integrationszeit ####
bev$par$wl = 200
png(paste0("Tagesmedianreferenz_AU", bev$par$wl, ".png")
    ,xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")

par( mfrow = c(2,2), mar = c(3, 4, 3, 1))
for(i in 1:4){
  plot( as.POSIXct(as.character( gsub("X", "", names(bev$median.daily.all[[ i ]]))), tz = "UTC", format = "%y%m%d")
        , unlist( lapply(bev$median.daily.all[[ i ]], function(x) x[ which( bev$para$wl %in% bev$par$wl ) ]) ) 
        , axes = T, xlab = "", ylab = paste("Counts bei", bev$par$wl, "nm")
        , main = paste( "AU bei", bev$par$wl, "nm in", bev$para$txt$ref$loc.line[ i ])
        , ylim = c(10000, 50000)
        , col = bev$par$colp[[ i ]])
}
dev.off()

bev$par$wl = 350
png(paste0("Tagesmedianreferenz_AU", bev$par$wl, ".png")
    ,xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")

par( mfrow = c(2,2), mar = c(3, 4, 3, 1))
for(i in 1:4){
  plot( as.POSIXct(as.character( gsub("X", "", names(bev$median.daily.all[[ i ]]))), tz = "UTC", format = "%y%m%d")
        , unlist( lapply(bev$median.daily.all[[ i ]], function(x) x[ which( bev$para$wl %in% bev$par$wl ) ]) ) 
        , axes = T, xlab = "", ylab = paste("Counts bei", bev$par$wl, "nm")
        , main = paste( "AU bei", bev$par$wl, "nm in", bev$para$txt$ref$loc.line[ i ])
        , ylim = c(0, 30000)
        , col = bev$par$colp[[ i ]])
}
dev.off()

png(paste0("Integrationszeit_seit_t0.png")
    ,xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")

par( mfrow = c(2,2), mar = c(3, 4, 3, 1))
for(i in 1:4){
  plot( bev$raw$refall[[ i ]]$datetime
        , bev$raw$refall[[ i ]]$integrationTime
        , axes = T, xlab = "", ylab = paste("Integrationszeit in ms")
        , main = paste( "Integrationszeit in", bev$para$txt$ref$loc.line[ i ])
        , ylim = c(0, 300)
        , col = bev$par$colp[[ i ]])
}
dev.off()

# Spektren
png(paste0("Tagesmedianspektren_seit_t0.png")
    ,xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")

par( mfrow = c(2,2), mar = c(3, 4, 3, 1))
for(i in 1:4){
  matplot(bev$para$wl
        , do.call(cbind, bev$median.daily.all[[ i ]])
        , type = "l", lty = 1
        , col = bev$par$colp[[ i ]]
        , xlab = lambda, ylab = "Counts"
        , main = paste("Tägliche Medianreferenzen in", bev$para$txt$ref$loc.line[ i ])
        , ylim = c(0, 60000))
# legend( "topright", bev$para$txt$ref$loc.line
#         , lty = 1, col = bev$par$colp.location)
}

dev.off()
