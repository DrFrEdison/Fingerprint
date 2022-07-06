
bev$raw$ref$Dorsten_DC

setwd( paste0(wd$servicebackup$CCEP$Mannheim$MY, "/ref") )
bev$raw$refall$Mannheim_MY <- lapply( dir( pattern = "_ref.csv"), function( x ) fread( x, sep = ";", dec = ","))

setwd( paste0(wd$servicebackup$CCEP$Moenchengladbach$G9, "/ref") )
bev$raw$refall$Moenchengladbach_G9 <- lapply( dir( pattern = "_ref.csv"), function( x ) fread( x, sep = ";", dec = ","))

setwd( paste0(wd$servicebackup$CCEP$Dorsten$DC, "/ref") )
bev$raw$refall$Dorsten_DC <- lapply( dir( pattern = "_ref.csv"), function( x ) fread( x, sep = ";", dec = ","))

setwd( paste0(wd$servicebackup$CCEP$Dorsten$DS, "/ref") )
bev$raw$refall$Dorsten_DS <- lapply( dir( pattern = "_ref.csv"), function( x ) fread( x, sep = ";", dec = ","))

bev$raw$refall <- lapply(bev$raw$refall, function( x ) rbindlist(x, fill = T))

bev$ppp <- lapply(bev$raw, function(x) lapply(x, transfer_csv.num.col))

bev$median.daily.all <- mapply( function( spc, numcol) median_daily_spc( spc = spc
                                                                         , date = spc$datetime
                                                                         , tz = "UTC"
                                                                         , numcol = numcol)
                                , spc = bev$raw$refall
                                , numcol = bev$ppp$refall
                                , SIMPLIFY = F)
setwd(bev$wd)
setwd("./plot")

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
        , ylim = c(10000, 50000))
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
        , ylim = c(0, 30000))
}
dev.off()
