
carstm_optimal_habitat = function( 
    domain, 
    res, 
    xvar = "inla.group(t, method = \"quantile\", n = 11)",  
    yvar = "inla.group(z, method = \"quantile\", n = 11)",
    season = 5,
    depths=NULL, 
    temperatures=NULL,
    probs=c(0, 1.0) 
  ) {

  require(ggplot2)

  domain = st_as_sf(domain)
  crs_domain = st_crs( domain )
  out = list()


  x_spline_function = carstm_spline( res, vn=c("random", xvar), statvar="mean" ) 
  x_spline_function_lb = carstm_spline( res, vn=c("random", xvar), statvar="quant0.025" ) 
  x_spline_function_ub = carstm_spline( res, vn=c("random", xvar), statvar="quant0.975" ) 

  y_spline_function = carstm_spline( res, vn=c("random", yvar), statvar="mean" ) 
  y_spline_function_lb = carstm_spline( res, vn=c("random", yvar), statvar="quant0.025" ) 
  y_spline_function_ub = carstm_spline( res, vn=c("random", yvar), statvar="quant0.975" ) 

    
  # use highest resolution depths and aggregate temps
  # "aggregated" == depths aggregated (averaged) to 0.5 km X 0.5 km basis
  # "complete" = 1x1 km

  Z = aegis.bathymetry::bathymetry_db( 
    p = aegis.bathymetry::bathymetry_parameters( spatial_domain="SSE", project_class="stmv" ), 
    DS ="complete" 
  )  

  if (!is.null(depths)) {
    Z = Z[ which( Z$z > depths[1] ) , ]
    Z = Z[ which( Z$z < depths[2] ) , ]
  }

  proj_stmv = st_crs("+proj=utm +ellps=WGS84 +zone=20 +units=km")
  inside = st_points_in_polygons(
    pts = st_transform( st_as_sf( Z[, c("plon", "plat")], coords=c("plon","plat"), crs=proj_stmv ), crs=crs_domain),
    polys =  st_union(domain)
  )

  Z = Z[which(is.finite(inside)), ]
  inside = NULL; gc()
 
  Z$Zprob = y_spline_function(Z$z ) 
  Z$Zprob_lb = y_spline_function_lb(Z$z ) 
  Z$Zprob_ub = y_spline_function_ub(Z$z ) 
  
  # any outside of domain of spline is not supported by data .. assume NA to prevent extrapolation
  i = which(Z$Zprob < 0)
  if (length(i) > 0 ) Z$Zprob[ i ] = 0
  k = which(Z$Zprob > 1)
  if (length(k) > 0 ) Z$Zprob[ k ] = 1  

  i = which(Z$Zprob_lb < 0)
  if (length(i) > 0 ) Z$Zprob_lb[ i ] = 0  
  k = which(Z$Zprob_lb > 1)
  if (length(k) > 0 ) Z$Zprob_lb[ k ] = 1  

  i = which(Z$Zprob_ub < 0)
  if (length(i) > 0 ) Z$Zprob_ub[ i ] = 0 
  k = which(Z$Zprob_ub > 1)
  if (length(k) > 0 ) Z$Zprob_ub[ k ] = 1 

  Z = Z[, c("z", "Zprob", "Zprob_lb", "Zprob_ub", "plon","plat") ]
  Z = Z[ which(is.finite(rowSums(Z) )) , ]

  isobaths = c( 0, 100, 200, 300, 400, 800 )
  isobs = aegis.bathymetry::isobath_db( depths=isobaths, project_to=crs_domain )
  isobs = st_intersection(isobs, domain)

 
  o = ggplot() +
      geom_sf( data=isobs, aes(alpha=0.1), colour="lightgray" ) +
      geom_raster(data = Z, aes(x=plon, y=plat, fill=Zprob, alpha=1.0) ) +
      scale_fill_gradientn(name = "Probability (depth)", colors =color.code( "seis", seq( 0, 1, by=0.1 )), na.value=NA ) +
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
  
  dev.new(width=14, height=8, pointsize=20)
  print(o) 
  out[["depth_plot"]] = o
  isobs = NULL
  isobaths = NULL
  o = NULL
  gc()


  # add temperature:
  require(aegis.temperature)
  year.assessment = 2021
  pT = temperature_parameters( project_class="carstm", yrs=1970:res$year.assessment, carstm_model_label="1970_present"  ) 

  sppolyT = areal_units( p=pT )  

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

  raster_template = raster(extent(domain)) # +1 to increase the area
  res(raster_template) = raster_resolution_km  # in meters
  crs(raster_template) = projection(domain) # transfer the coordinate system to the raster

  TP = st_transform( sppolyT, crs_domain)  # same
  TP = st_make_valid( TP )
  TP = st_cast(TP, "MULTIPOLYGON")
  TP$rn = 1:nrow(TP)
  iTP = fasterize( TP, raster_template, field="rn" )
  TP = NULL
  gc()

  bb = raster::bbox(raster_template)
  
  ic = array_map( "xy->1", Z[, c("plon", "plat")], dims=dim(resT)[1:2], res=c(1,1), origin=bb[,"min"] )
  

  h_zt = data.frame(yr=pT$yrs  ) 

  Tprob = resT[] * NA 
  j = which(is.finite( resT))
  Tprob[j] = x_spline_function(resT[j]) 

  # any outside of domain of spline is not supported by data .. assume NA to prevent extrapolation
  k = which( Tprob < 0 )
  if (length(k) > 0 ) Tprob[k] = 0
  l = which( Tprob > 1 )
  if (length(l) > 0 ) Tprob[l] = 1
  m = which(!is.finite(Tprob) )  # in case of infinites
  if (length(m) > 0 ) Tprob[m] = NA

  # Tprob is too large .. break into annual steps
  h_zt$habitat = NA
  h_zt$habitat_sa = NA


  for ( iy in 1:nrow(h_zt)) {
    TPR = Tprob[ iTP[ic],iy,]*Z$Zprob
    h_zt$habitat[iy] = mean( colMeans(TPR, na.rm=TRUE), na.rm=TRUE )
    i = which( (TPR > probs[1]) & (TPR < probs[2]) )
    TPR[] = NA
    if (length(i) > 0)  TPR[ i ] = 1
    h_zt$habitat_sa[iy] = mean( colSums(TPR, na.rm=TRUE), na.rm=TRUE )  # count area with condition
  }
 
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
    TPR = Tprob[ iTP[ic],iy,] * Z$Zprob_lb
    h_zt$habitat_lb[iy] = mean( colMeans(TPR, na.rm=TRUE), na.rm=TRUE )
    i = which( (TPR > probs[1]) & (TPR < probs[2]) )
    TPR[] = NA
    if (length(i) > 0)  TPR[ i ] = 1
    h_zt$habitat_lb_sa[iy] = mean( colSums(TPR, na.rm=TRUE), na.rm=TRUE )  # count area with condition
  }
 

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
    TPR = Tprob[ iTP[ic],iy,]*Z$Zprob_ub
    h_zt$habitat_ub[iy] = mean( colMeans(TPR, na.rm=TRUE), na.rm=TRUE )
    i = which( (TPR > probs[1]) & (TPR < probs[2]) )
    TPR[] = NA
    if (length(i) > 0)  TPR[ i ] = 1
    h_zt$habitat_ub_sa[iy] = mean( colSums(TPR, na.rm=TRUE), na.rm=TRUE )  # count area with condition
  }
  
  dev.new(pointsize=24, width=10, height=8 ) 
  plot(habitat ~ yr, h_zt, type="b", col="slategray", xlab="Year", ylab="Habitat (depth, temperature)", pch=20, cex=1.5, ylim=range( h_zt[, c("habitat_ub", "habitat_lb")]))
  lines( habitat_lb ~ yr, data=h_zt, lty="dashed", col="gray" )
  lines( habitat_ub  ~ yr, data=h_zt, lty="dashed", col="gray" )

  out[["temperature_depth"]] = h_zt
  out[["total_surface"]] = ntot

  return(out)

}