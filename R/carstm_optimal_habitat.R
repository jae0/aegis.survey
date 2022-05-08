
carstm_optimal_habitat = function( 
    domain, 
    res, 
    xvar = "inla.group(t, method = \"quantile\", n = 11)",  
    yvar = "inla.group(z, method = \"quantile\", n = 11)",
    proj_planar="+proj=utm +ellps=WGS84 +zone=20 +units=km", 
    season = 5,
    depths=NULL, 
    temperatures=NULL,
    probs=c(0.5, 1.0) 
  ) {

  require(ggplot2)

  crs_plot = st_crs( domain )
  out = list()


  # use highest resolution depths and aggregate temps
  # "aggregated" == depths aggregated (averaged) to 0.5 km X 0.5 km basis
  # "complete" = 1x1 km

  Z = aegis.bathymetry::bathymetry_db( 
    p = aegis.bathymetry::bathymetry_parameters( spatial_domain="SSE", project_class="stmv" ), 
    DS ="complete" 
  )  
  Z = planar2lonlat(Z, proj.type=proj_planar ) 

  crs_lonlat = st_crs(projection_proj4string("lonlat_wgs84"))
  inside = st_points_in_polygons(
    pts = st_as_sf( Z[, c("lon", "lat")], coords=c("lon","lat"), crs=crs_lonlat ),
    polys = st_transform( st_union(domain), crs_lonlat )
  )

  Z = Z[which(is.finite(inside)), ]
  inside = NULL; gc()

  y_spline_function = carstm_spline( res, vn=c("random", yvar), statvar="mean" ) 
  y_spline_function_lb = carstm_spline( res, vn=c("random", yvar), statvar="quant0.025" ) 
  y_spline_function_ub = carstm_spline( res, vn=c("random", yvar), statvar="quant0.975" ) 


  if (!is.null(depths)) {
    Z$z[ Z$z > depths[2] ] =  NA
    Z$z[ Z$z < depths[1] ] =  NA
  }

  depths = range(Z$z, na.rm=TRUE )
  dr = seq( depths[1], depths[2], by=5 ) 

  isobaths = c( 10, 100, 200, 300, 400, 800 )
  isobs = aegis.bathymetry::isobath_db( depths=isobaths, project_to=crs_plot )
  isobs = st_intersection(isobs, domain)

  o = ggplot() +
      geom_sf( data=isobs, aes(alpha=0.1), colour="lightgray" ) +
      geom_raster(data = Z, aes(x=plon, y=plat, fill=z, alpha=1.0) ) +
      scale_fill_gradientn(name = "Depth", colors =color.code( "seis", dr), na.value=NA ) +
      guides(fill = guide_colorbar(
        title.theme = element_text(size = 20),
        label.theme = element_text(size = 18) ) ) +
      scale_alpha(range = c(0.9, 0.95), guide = "none") +
      theme(
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), 
        legend.position=c( 0.1, 0.8 ),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank() )
        ggtitle("Cod depth") +
        coord_fixed()
  
  
  ntot = nrow(Z)
  
  print( paste( "Surface area for depths: ", length(which(is.finite(Z$z))), "of", ntot ) )

  out[["depth"]] = isobs
  out[["depth_plot"]] = o
  isobs = NULL
  isobaths = NULL
  o = NULL
  gc()


  x_spline_function = carstm_spline( res, vn=c("random", xvar), statvar="mean" ) 
  x_spline_function_lb = carstm_spline( res, vn=c("random", xvar), statvar="quant0.025" ) 
  x_spline_function_ub = carstm_spline( res, vn=c("random", xvar), statvar="quant0.975" ) 

    
  # add temperature:
  require(aegis.temperature)
  year.assessment = 2021
  pT = temperature_parameters( project_class="carstm", yrs=1970:res$year.assessment, carstm_model_label="1970_present"  ) 

  sppolyT = areal_units( p=pT )  # same

  res = NULL; gc()

  # to load currently saved results
  resT = carstm_model( p=pT, DS="carstm_modelled_summary", sppoly=sppolyT )
  resT = resT$predictions_posterior_simulations[,,season,]

  
  if (!is.null(temperatures)) {
    # filter our very low pr 
    resT[ resT > temperatures[2] ] =  NA
    resT[ resT < temperatures[1] ] =  NA
  }


  raster_resolution_km = 1 # res of Z (pB$pres)

  require(raster)
  require(fasterize)
  require(data.table)

  raster_template = raster(extent(sppoly)) # +1 to increase the area
  res(raster_template) = raster_resolution_km  # in meters
  crs(raster_template) = projection(sppoly) # transfer the coordinate system to the raster

  TP = st_transform( sppolyT, crs_plot)  # same
  TP = st_make_valid( TP )
  TP = st_cast(TP, "MULTIPOLYGON")
  TP$rn = 1:nrow(TP)
  iTP = fasterize( TP, raster_template, field="rn" )
  TP = NULL
  gc()

  bb = raster::bbox(raster_template)
    
  ic = array_map( "xy->1", Z[, c("plon", "plat")], dims=dim(resT)[1:2], res=c(1,1), origin=bb[,"min"] )
  

  Zprob = Z$z*NA
  j = which(is.finite( Z$z))
  Zprob[j] = y_spline_function(Z$z[j]) 
  i = which(Zprob < 0)
  if (length(i) > 0 ) Zprob[ i ] = 0  
  k = which(Zprob > 1)
  if (length(k) > 0 ) Zprob[ k ] = 1  

browser()
  h_zt = data.frame(yr=pT$yrs  ) 
  Tprob = resT[] * NA 
  j = which(is.finite( resT))
  Tprob[j] = x_spline_function(resT[j]) 
  k = which( Tprob < 0 )
  if (length(k) > 0 ) Tprob[k] = 0
  l = which( Tprob > 1 )
  if (length(l) > 0 ) Tprob[l] = 1
  m = which(!is.finite(Tprob) )
  if (length(m) > 0 ) Tprob[m] = NA

  # Tprob is too large .. break into annual steps
  h_zt$habitat = NA
  h_zt$habitat_sa = NA
  

  for ( iy in 1:nrow(h_zt)) {
    TPR = Tprob[ iTP[ic],iy,]*Zprob
    h_zt$habitat[iy] = mean( rowMeans(TPR, na.rm=TRUE), na.rm=TRUE )
    i = which( (TPR > probs[1]) & (TPR < probs[2]) )
    TPR[] = NA
    if (length(i) > 0)  TPR[ i ] = 1
    h_zt$habitat_sa[iy] = mean( rowSums(TPR, na.rm=TRUE), na.rm=TRUE )  # count area with condition
  }



  Zprob = Z$z*NA
  j = which(is.finite( Z$z))
  Zprob[j] = y_spline_function_lb(Z$z[j]) 
  i = which(Zprob < 0)
  if (length(i) > 0 ) Zprob[ i ] = 0  
  k = which(Zprob > 1)
  if (length(k) > 0 ) Zprob[ k ] = 1  

  Tprob = resT[] * NA 
  j = which(is.finite( resT))
  Tprob[j] = x_spline_function_lb(resT[j]) 
  k = which( Tprob < 0 )
  if (length(k) > 0 ) Tprob[k] = 0
  l = which( Tprob > 1 )
  if (length(l) > 0 ) Tprob[l] = 1
  m = which(!is.finite(Tprob) )
  if (length(m) > 0 ) Tprob[m] = NA
  # Tprob is too large .. break into annual steps

  h_zt$habitat_lb = NA
  h_zt$habitat_lb_sa = NA

  for ( iy in 1:nrow(h_zt)) {
    TPR = Tprob[ iTP[ic],iy,]*Zprob
    h_zt$habitat_lb[iy] = mean( rowMeans(TPR, na.rm=TRUE), na.rm=TRUE )
    i = which( (TPR > probs[1]) & (TPR < probs[2]) )
    TPR[] = NA
    if (length(i) > 0)  TPR[ i ] = 1
    h_zt$habitat_lb_sa[iy] = mean( rowSums(TPR, na.rm=TRUE), na.rm=TRUE )  # count area with condition
  }




  Zprob = Z$z*NA
  j = which(is.finite( Z$z))
  Zprob[j] = y_spline_function_ub(Z$z[j]) 
  i = which(Zprob < 0)
  if (length(i) > 0 ) Zprob[ i ] = 0  
  k = which(Zprob > 1)
  if (length(k) > 0 ) Zprob[ k ] = 1  

  Tprob = resT[] * NA 
  j = which(is.finite( resT))
  Tprob[j] = x_spline_function_ub(resT[j]) 
  k = which( Tprob < 0 )
  if (length(k) > 0 ) Tprob[k] = 0
  l = which( Tprob > 1 )
  if (length(l) > 0 ) Tprob[l] = 1
  m = which(!is.finite(Tprob) )
  if (length(m) > 0 ) Tprob[m] = NA

  # Tprob is too large .. break into annual steps

  h_zt$habitat_ub = NA
  h_zt$habitat_ub_sa = NA
  for ( iy in 1:nrow(h_zt)) {
    TPR = Tprob[ iTP[ic],iy,]*Zprob
    h_zt$habitat_ub[iy] = mean( rowMeans(TPR, na.rm=TRUE), na.rm=TRUE )
    i = which( (TPR > probs[1]) & (TPR < probs[2]) )
    TPR[] = NA
    if (length(i) > 0)  TPR[ i ] = 1
    h_zt$habitat_ub_sa[iy] = mean( rowSums(TPR, na.rm=TRUE), na.rm=TRUE )  # count area with condition
  }
  
  dev.new(pointsize=24, width=10, height=8 ) 
  plot(habitat ~ yr, h_zt, type="b", col="slategray", xlab="Year", ylab="Habitat (depth, temperature)", pch=20, cex=1.5, ylim=range( h_zt[, c("habitat_ub", "habitat_lb")]))
  lines( habitat_lb ~ yr, data=h_zt, lty="dashed", col="gray" )
  lines( habitat_ub  ~ yr, data=h_zt, lty="dashed", col="gray" )

  out[["temperature_depth"]] = h_zt
  out[["temperature"]] = Tprob
  out[["depth"]] = Zprob


  return(out)

}