netmensuration.figures = function( p, DS="", outdir = file.path( project.datadirectory("aegis"), "analysis" ) ) {

  if (! file.exists( outdir ) ) dir.create( outdir, recursive =TRUE )

  gsinf = groundfish_survey_db( DS="sweptarea" )
  gg = which( gsinf$geardesc == "Western IIA trawl" & gsinf$settype %in% c(1,2,5) )
  gsinf = gsinf[ gg, ]

  if (DS %in% c("wing.v.door", "all") ) {
    fn = file.path( outdir, "wing.v.door.pdf" )
    cols = c("slateblue", "red", "orange")
    pdf( file=fn )
      ii = which ( is.finite( gsinf$wing.sd) & is.finite (gsinf$door.sd ) )
      plot(door.mean~wing.mean, gsinf[ii,], type="n", xlab=" Wing spread (m)", ylab="Door spread (m)", xlim=c(8,20), ylim=c(6,81) )
      iold = intersect(ii, which( gsinf$geardesc == "Western IIA trawl" & (gsinf$yr < 2011 | gsinf$yr==2012)  ) )
      points ( door.mean ~ wing.mean, gsinf[iold, ], col=cols[1], pch=20, cex=0.7 )
      imed = intersect(ii, which( gsinf$geardesc == "Western IIA trawl" & gsinf$yr %in% c( 2011 ) ) )
      points ( door.mean ~ wing.mean, gsinf[imed, ], col=cols[2], pch=20, cex=0.7 )
      inew = intersect(ii, which( gsinf$geardesc == "Western IIA trawl" & gsinf$yr >= 2013 ) )
      points ( door.mean ~ wing.mean, gsinf[inew, ], col=cols[3], pch=20, cex=0.9 )
      legend( "bottomright",  legend=c("2004-2010, 2012", "2011", "2013-2015"),  text.col=cols, pch=20, col=cols )
    dev.off()
    print( fn )
  }

  if (DS %in% c("sa.comparison", "all") ) {
    fn = file.path( outdir, "sa.comparison.pdf" )
    cols = c("slateblue", "red", "orange")
    pdf( file=fn )
     ii = which ( is.finite( gsinf$wing.sd)  )  ## where good estimates exist
     plot( wing.sa ~ jitter(sakm2), data=gsinf[ii,], col=cols[1], cex=0.5, pch=20, ylab="Net configuration analysis surface area (km^2)", xlab="GSINF start-end surface area (km^2)" )
      abline( a=0, b=1, col="orange" )
    dev.off()
    print( fn )
  }


  if (DS %in% c("sa.comparison.all", "all") ) {
    fn = file.path( outdir, "sa.comparison.all.pdf" )
    cols = c("slateblue", "red", "orange")
    pdf( file=fn )
     plot( wing.sa ~ jitter(sakm2), data=gsinf[,], col=cols[1], cex=0.5, pch=20, ylab="Net configuration analysis and modelled surface area (km^2)", xlab="GSINF start-end surface area (km^2)" )
      abline( a=0, b=1, col="orange" )
    dev.off()
    print( fn )
  }



  if (DS %in% c("wing.v.door.byyear", "all") ) {
    fn = file.path( outdir, "wing.v.door.byyear.pdf" )
    cols = c("slateblue", "red", "orange")
    pdf( file=fn )
      layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE), respect = TRUE)
      ii = which ( is.finite( gsinf$wing.sd)  )  ## where good estimates exist
      boxplot( door.mean ~ yr, gsinf[ii,], ylab="Door spread (m)" )
      boxplot( wing.mean ~ yr, gsinf[ii,], ylab="Wing spread (m)", xlab="Year" )
    dev.off()
    print( fn )
  }


  if (DS %in% c("toweddistance", "all") ) {
    fn = file.path( outdir, "toweddistance.pdf" )
    cols = c("slateblue", "orange")
    pdf( file=fn )
      plot( jitter(dist_km) ~ dist_pos , gsinf, xlim=c(0, 8.5) ,  ylim=c(0, 8.5), col=cols[1], pch=20, cex=0.5, ylab="Tow distance (km)", xlab="Tow distance logged in GSINF (km)" )

      points( bc.dist ~ dist_pos, gsinf, col=cols[2], pch=20, cex=0.5 ) # bottom contact
      abline( a=0, b=1, col="orange" )

      legend( "bottomright",  legend=c("Logged positions", "Bottom contact analysis"),  text.col=cols, pch=20, col=cols )
      text( 4, 8, "Western II-A, \n Set types 1, 2, 5 \n (stratified random, regular survey, comparative fishing)" )

    dev.off()
    print( fn )
  }


  # -------

  if (DS %in% c("noSets", "all") ) {
    fn = file.path( outdir, "numberOfSets.pdf" )
    cols = c("slateblue", "orange")
    gsinf = groundfish_survey_db( DS="sweptarea" ) # use all data not just western 2a
    m = tapply( rep(1,nrow(gsinf)), gsinf$yr, sum)
    bc2 = gsinf[ is.finite(gsinf$bottom_duration) , ]
    n = tapply( rep(1,nrow(bc2)), bc2$yr, sum)
    pdf( file=fn )
      plot( m ~ as.numeric(names(m)), type="b", pch=20, col=cols[1], ylim=c(0, max(m)), ylab="", xlab="Year"  )
      lines( n ~ as.numeric(names(n)), type="b", pch=20, col=cols[2] )
      legend( "topleft", legend=c("No. of sets completed in Groundfish survey", "No. of sets in Groundfish survey with usable bottom contact data"),  text.col=cols, pch=20, col=cols)
    dev.off()
    print( fn )
  }



  # -------

  if (DS %in% c("", "all") ) {
    fn = file.path( outdir, "xxx.pdf" )
    cols = c("slateblue", "red", "orange")
    #pdf( file=fn )


    #dev.off()
    #print( fn )
  }



 }


