carstm_plots = function( res, outputdir, fn_root, sppoly, additional_features, background, map_centre, map_zoom ) {

  # ---

  # NOTE: these can use fit instead of res, but res is faster to load; res are already in use space

  # H marginal effects -- make sure to load res for N first above
  if ( grepl("probability", fn_root) ) {

    carstm_plotxy( res, vn=c( "res", "random", "inla.group(z, method = \"quantile\", n = 11)" ), 
      # transf=inverse.logit,  
      type="b", col="slategray", pch=19, lty=1, lwd=2.5, cex=1.0, 
      ylim=c(0, 1) ,
      outfilename=file.path( outputdir, "effects", "habitat_depth.png"),
      xlab="Depth (m)", ylab="Probability", h=0.5  )

    carstm_plotxy( res, vn=c( "res", "random", "inla.group(t, method = \"quantile\", n = 11)" ), 
      # transf=inverse.logit,   
      type="b", col="slategray", pch=19, lty=1, lwd=2.5, cex=1.0, 
      ylim=c(0, 1) ,
      outfilename=file.path( outputdir, "effects", "habitat_temperature.png"),
      xlab="Bottom temperature (degrees Celsius)", ylab="Probability", h=0.5  )

    carstm_plotxy( res, vn=c( "res", "random", "inla.group(log.substrate.grainsize, method = \"quantile\", n = 11)" ), 
      # transf=inverse.logit, 
      ylim=c(0, 1), 
      type="b", col="slategray", pch=19, lty=1, lwd=2.5, cex=1.0, 
      outfilename=file.path( outputdir, "effects", "habitat_substrate.png"),
      xlab="ln(grain size; mm)", ylab="Probability", h=0.5  )

    gears = c("Western IIA", "Yankee #36", "US 4seam 3beam",  "Engle", "Campelen 1800", "Nephrops" )
    carstm_plotxy( res, vn=c( "res", "random", "gear" ), subtype="errorbar", errorbar_labels=gears,
      type="p",
      # transf=inverse.logit, 
      ylim=c(0, 1), 
      col="slategray", pch=19, lty=1, lwd=2.5, cex=1.0,  
      offs=1,
      outfilename=file.path( outputdir, "effects", "habitat_gear.png"),
      xlab="Gear type", ylab="Probability", h=0.5 )


    carstm_plotxy( res, vn=c( "res", "random", "time" ), 
      # transf=inverse.logit,  # only if from fit and not res
      type="b", col="slategrey", pch=19, lty=1, lwd=2.5, cex=1.0, 
      ylim=c(0, 1), 
      outfilename=file.path( outputdir, "effects", "habitat_year.png"),
      xlab="Year", ylab="Probability", h=0.5, v=1992   )

    carstm_plotxy( res, vn=c( "res", "random", "cyclic" ), 
      # transf=inverse.logit, 
      type="b", col="slategray", pch=19, lty=1, lwd=2.5, cex=1.0, 
      ylim=c(0, 1),
      outfilename=file.path( outputdir, "effects", "habitat_season.png"),
      xlab="Season", ylab="Probability", h=0.5  )

    carstm_plotxy( res, vn=c( "res", "random", "inla.group(pca1, method = \"quantile\", n = 11)" ), 
      # transf=inverse.logit, 
      type="b", col="slategray", pch=19, lty=1, lwd=2.5, cex=1.0, 
      ylim=c(0, 1),
      outfilename=file.path( outputdir, "effects", "habitat_pca1.png"),
      xlab="PCA1", ylab="Probability", h=0.5  )

    carstm_plotxy( res, vn=c( "res", "random", "inla.group(pca2, method = \"quantile\", n = 11)" ), 
      # transf=inverse.logit, 
      type="b", col="slategray", pch=19, lty=1, lwd=2.5, cex=1.0, 
      ylim=c(0, 1),
      outfilename=file.path( outputdir, "effects", "habitat_pca2.png"),
      xlab="PCA2", ylab="Probability", h=0.5  )

  }



  # N marginal effects -- make sure to load res for N first above
  if ( grepl("number", fn_root) ) {

    carstm_plotxy( res, vn=c( "res", "random", "inla.group(z, method = \"quantile\", n = 11)" ), 
      # transf=exp,  # ylim=c(0, 0.8) ,
      type="b", col="slategray", pch=19, lty=1, lwd=2.5, 
      outfilename=file.path( outputdir, "effects", "number_depth.png"),
      xlab="Depth (m)", ylab="Effect size", h=1.0  )

    carstm_plotxy( res, vn=c( "res", "random", "inla.group(t, method = \"quantile\", n = 11)" ), 
      # transf=exp,   
      #ylim=c(0.2, 0.8) ,
      type="b", col="slategray", pch=19, lty=1, lwd=2.5, 
      outfilename=file.path( outputdir, "effects", "number_temperature.png"),
      xlab="Bottom temperature (degrees Celsius)", ylab="Effect size", h=1.0  )

    carstm_plotxy( res, vn=c( "res", "random", "inla.group(log.substrate.grainsize, method = \"quantile\", n = 11)" ), 
      # transf=exp, # ylim=c(0.35, 0.65), 
      outfilename=file.path( outputdir, "effects", "number_substrate.png"),
      type="b", col="slategray", pch=19, lty=1, lwd=2.5,
      xlab="ln(grain size; mm)", ylab="Effect size", h=1.0  )

    gears = c("Western IIA", "Yankee #36", "US 4seam 3beam",  "Engle", "Campelen 1800", "Nephrops" )
    carstm_plotxy( res, vn=c( "res", "random", "gear" ), subtype="errorbar", errorbar_labels=gears,
      type="p",
      # transf=exp, 
      outfilename=file.path( outputdir, "effects", "number_gear.png"),
      ylim=c(0., 25), 
      col="slategray", pch=19, lty=1, lwd=2.5, adj=0,    offs=10,
      xlab="Gear type", ylab="Effect size", h=1.0  )


    carstm_plotxy( res, vn=c( "res", "random", "time" ), 
      ## transf=exp,
      ylim=c(0,4), 
      outfilename=file.path( outputdir, "effects", "number_year.png"),
      col="slategray", pch=19, lty=1, lwd=2.5, 
      type="b",  xlab="Year", ylab="Effect size", h=1.0, v=1992   )

    carstm_plotxy( res, vn=c( "res", "random", "cyclic" ), 
      # transf=exp, 
      # ylim=c(0.35, 0.65),
      type="b", col="slategray", pch=19, lty=1, lwd=2.5, 
      outfilename=file.path( outputdir, "effects", "number_season.png"),
      xlab="Season", ylab="Effect size", h=1.0  )

    carstm_plotxy( res, vn=c( "res", "random", "inla.group(pca1, method = \"quantile\", n = 11)" ), 
      type="b", col="slategray", pch=19, lty=1, lwd=2.5, cex=1.0, 
      ylim=c(0.4, 1.75),
      outfilename=file.path( outputdir, "effects", "number_pca1.png"),
      xlab="PCA1", ylab="Effect size", h=1  )

    carstm_plotxy( res, vn=c( "res", "random", "inla.group(pca2, method = \"quantile\", n = 11)" ), 
        type="b", col="slategray", pch=19, lty=1, lwd=2.5, cex=1.0, 
      ylim=c(0.4, 1.75 ),
      outfilename=file.path( outputdir, "effects", "number_pca2.png"),
      xlab="PCA2", ylab="Effect size", h=1  )
 
  }


  if ( grepl("weight", fn_root) ) {

    # W marginal effects -- make sure to load res for W first above


    carstm_plotxy( res, vn=c( "res", "random", "inla.group(z, method = \"quantile\", n = 11)" ), 
      ylim=c(0.6, 2.75) ,
      type="b", col="slategray", pch=19, lty=1, lwd=2.5, 
      outfilename=file.path( outputdir, "effects", "weight_depth.png"),
      xlab="Depth (m)", ylab="Effect size", h=1.0  )

    carstm_plotxy( res, vn=c( "res", "random", "inla.group(t, method = \"quantile\", n = 11)" ), 
      ylim=c(0.8, 1.2) ,
      type="b", col="slategray", pch=19, lty=1, lwd=2.5, 
      outfilename=file.path( outputdir, "effects", "weight_temperature.png"),
      xlab="Bottom temperature (degrees Celsius)", ylab="Effect size", h=1.0  )

    carstm_plotxy( res, vn=c( "res", "random", "inla.group(log.substrate.grainsize, method = \"quantile\", n = 11)" ), 
      ylim=c(0.8, 1.2), 
      type="b", col="slategray", pch=19, lty=1, lwd=2.5,
      outfilename=file.path( outputdir, "effects", "weight_substrate.png"),
      xlab="ln(grain size; mm)", ylab="Effect size", h=1.0  )

    gears = c("Western IIA", "Yankee #36", "US 4seam 3beam",  "Engle", "Campelen 1800", "Nephrops" )
    carstm_plotxy( res, vn=c( "res", "random", "gear" ), subtype="errorbar", errorbar_labels=gears,
      type="p",
      ylim=c(0.3, 1.75), 
      col="slategray", pch=19, lty=1, lwd=2.5, adj=NULL,
      outfilename=file.path( outputdir, "effects", "weight_gear.png"),
      xlab="Gear type", ylab="Effect size", h=1.0  )


    carstm_plotxy( res, vn=c( "res", "random", "time" ), 
      ylim=c(0, 7), 
      col="slategray", pch=19, lty=1, lwd=2.5,
      outfilename=file.path( outputdir, "effects", "weight_year.png"),
      type="b",  xlab="Year", ylab="Effect size", h=1.0, v=1992   )

    carstm_plotxy( res, vn=c( "res", "random", "cyclic" ), 
      ylim=c(0.85, 1.2),
      type="b", col="slategray", pch=19, lty=1, lwd=2.5, 
      outfilename=file.path( outputdir, "effects", "weight_season.png"),
      xlab="Season", ylab="Effect size", h=1.0  )

    carstm_plotxy( res, vn=c( "res", "random", "inla.group(pca1, method = \"quantile\", n = 11)" ), 
      type="b", col="slategray", pch=19, lty=1, lwd=2.5, cex=1.0, 
      ylim=c(0.4, 1.75),
      outfilename=file.path( outputdir, "effects", "weight_pca1.png"),
      xlab="PCA1", ylab="Effect size", h=1.0  )

    carstm_plotxy( res, vn=c( "res", "random", "inla.group(pca2, method = \"quantile\", n = 11)" ), 
        type="b", col="slategray", pch=19, lty=1, lwd=2.5, cex=1.0, 
      ylim=c(0.4, 1.75 ),
      outfilename=file.path( outputdir, "effects", "weight_pca2.png"),
      xlab="PCA2", ylab="Effect size", h=1.0  )

  }


 
  dev.new(width=10, height=8, pointsize=16)

  tf = identity

  if ( grepl("probability", fn_root) ) tf = identity
  if ( grepl("numerical", fn_root) )   tf = log10
  if ( grepl("biomass", fn_root) )     tf = log10
  if ( grepl("weight", fn_root) )      tf = log10


  # generic plots
  vn=c( "random", "space", "combined" )
  tmatch = ""
  fn = file.path( outputdir, "predictions", paste(fn_root, paste0(vn, collapse="_"), "png", sep=".") )
  carstm_map(  res=res, vn=vn, tmatch=tmatch, 
      sppoly = sppoly, 
      palette="-RdYlBu",
      plot_elements=c(  "compass", "scale_bar", "legend" ),
      additional_features=additional_features,
      outfilename=fn,
      # title = paste( title, "spatial effect") ,
      background = background,
      transformation= tf, 
      tmap_zoom= c(map_centre, map_zoom)  
  )


  vn="predictions" 
  brks = pretty( quantile( tf( res[[vn]]), probs=c(0.05, 0.95), na.rm=TRUE )  )
  for (y in res$time ){
    time_match = as.character(y) 
    fn = file.path( outputdir, "predictions", paste(fn_root, paste0(vn, collapse="_"), time_match, "png", sep=".") )
    carstm_map(  res=res, vn=vn, tmatch=time_match,
      sppoly = sppoly, 
      breaks=brks,
      palette="-RdYlBu",
      plot_elements=c(  "compass", "scale_bar", "legend" ),
      additional_features=additional_features,
      title= paste( time_match) , #paste(fn_root, time_match, sep="_"),  
      outfilename=fn,
      background = background,
      transformation = tf, 
      scale=1.5,
      map_mode="view",
      tmap_zoom= c(map_centre, map_zoom)
    )
  }


}
