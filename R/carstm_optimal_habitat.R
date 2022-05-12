
carstm_optimal_habitat = function( 
    domain, 
    res, 
    year.assessment = 2021,
    xvar = "inla.group(t, method = \"quantile\", n = 11)",  
    yvar = "inla.group(z, method = \"quantile\", n = 11)",
    depths=NULL, 
    plot_map=FALSE,
    fun_choice=NULL,
    probability_limit=NULL
  ) {


  proj_stmv =  "+proj=utm +ellps=WGS84 +zone=20 +units=km"  # crs of depth from stmv
  crs_domain = st_crs( proj_stmv )

  domain = st_transform( st_union( st_as_sf(domain) ), crs_domain  )

  out = list()

  x_spline_function = carstm_spline( res, vn=c("random", xvar), statvar="mean" ) 
  x_spline_function_lb = carstm_spline( res, vn=c("random", xvar), statvar="quant0.025" ) 
  x_spline_function_ub = carstm_spline( res, vn=c("random", xvar), statvar="quant0.975" ) 

  y_spline_function = carstm_spline( res, vn=c("random", yvar), statvar="mean" ) 
  y_spline_function_lb = carstm_spline( res, vn=c("random", yvar), statvar="quant0.025" ) 
  y_spline_function_ub = carstm_spline( res, vn=c("random", yvar), statvar="quant0.975" ) 

  res = NULL

  # use highest resolution depths and aggregate temps
  # "aggregated" == depths aggregated (averaged) to 0.5 km X 0.5 km basis
  # "complete" = 1x1 km
  pZ =  aegis.bathymetry::bathymetry_parameters( spatial_domain="SSE", project_class="stmv" )

  Z = aegis.bathymetry::bathymetry_db( p=pZ, DS ="complete" )   

  if (!is.null(depths)) {
    Z = Z[ which( Z$z > depths[1] ) , ]
    Z = Z[ which( Z$z < depths[2] ) , ]
  }

  inside = st_points_in_polygons(
    pts = st_transform( st_as_sf( Z[, c("plon", "plat")], coords=c("plon","plat"), crs=proj_stmv ), crs=crs_domain),
    polys =  domain
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

  out[["depths"]] = Z
  
  ntot = nrow(Z)
  
  print( paste( "Surface area for depths: ", length(which(is.finite(Z$z))), "of", ntot ) )
  


  # add temperature:
  require(aegis.temperature)
  
  pT = temperature_parameters( project_class="carstm", yrs=1970:year.assessment, carstm_model_label="1970_present"  ) 

  h_zt = data.frame(yr=pT$yrs  ) 

  sppolyT = st_transform( areal_units( p=pT ), crs=crs_domain )  
 

  raster_resolution_km = 1 # res of Z (pB$pres)

  require(raster)
  require(fasterize)
  require(data.table)

  raster_template = raster(extent(st_as_sf(domain) )) # +1 to increase the area
  res(raster_template) = raster_resolution_km  # in meters
  crs(raster_template) = projection(domain) # transfer the coordinate system to the raster

  TP = st_transform( sppolyT, crs_domain)  # same
  TP = st_make_valid( TP )
  TP = st_buffer( TP, 0)
  TP = st_cast(TP, "MULTIPOLYGON")
  TP$rn = 1:nrow(TP)
  iTP = fasterize( TP, raster_template, field="rn" )
  TP = NULL
  gc()

  bb = raster::bbox(raster_template)

  # to load currently saved results
  resT = carstm_model( p=pT, DS="carstm_modelled_summary", sppoly=sppolyT )$predictions_posterior_simulations
  dim_resT = dim(resT)
  ic = array_map( "xy->1", Z[, c("plon", "plat")], dims=dim(raster_template)[1:2], res=c(raster_resolution_km,raster_resolution_km), origin=bb[,"min"] ) # map stmv to raster
  iitpc = iTP[ as.integer(ic) ]

  iTP = ic = NULL 
  sppolyT = NULL
  domain = NULL
  gc()
  
  # fast by ignoring na operations
  mean_fast = function(x) .Internal(mean(x))
  sum_fast = function(x) .Internal(sum(x))
  fun_choice = function( x ) {
    g = which(is.finite(x))
    if ( length(g)==0) return(NA)
    y = x[g]
    return( y[which.min(y)]  )
  }

  if (!is.null(probability_limit)) {
    tpr_limit = probability_limit
  } else {
    tpr_limit = 0.25
  }   

  ny = dim_resT[2]
  nsims = dim_resT[4]

  summ = summh = summ_lb = summh_lb = summ_ub = summh_ub =array( NA, dim=c(ny, nsims) )

  message("This will take a while ... ")

  fn_res = tempfile()
  saveRDS(resT, file=fn_res, compress=FALSE )

  for (ss in 1:nsims) {
    
    print( paste( "Sim: ", ss, "of", nsims) )
    
    if (is.null(resT)) resT = readRDS(fn_res) 

    resY = resT[,,,ss]
    resT = NULL; gc()
    
    fk = which( is.finite(resY) )

    Tprob = array(NA, dim=dim(resY) )
    Tprob[fk] = x_spline_function( resY[fk]  ) 

    k = which( Tprob < 0 )
    if (length(k) > 0 ) Tprob[k] = 0
   
    k = which( Tprob > 1 )
    if (length(k) > 0 ) Tprob[k] = 1

    TPR = Tprob[ iitpc,, ] * Z$Zprob  # sp, yr, seas, sim
    # na's created due to alignment problems with rasterization .. ignore rather than setting to zero
   
    # mean across space then sims
    summ[,ss] =  colMeans( apply(TPR, c(1,2), mean, na.rm=TRUE  ), na.rm=TRUE )   

    TPR = ifelse( TPR > tpr_limit, 1, 0 )
    # mean across seasons,  sum across space  
    summh[,ss] = colSums( apply(TPR, c(1,2), mean, na.rm=TRUE  ), na.rm=TRUE )  


    Tprob = array(NA, dim=dim(resY) )
    Tprob[fk] = x_spline_function_lb( resY[fk]  ) 

    k = which( Tprob < 0 )
    if (length(k) > 0 ) Tprob[k] = 0
   
    k = which( Tprob > 1 )
    if (length(k) > 0 ) Tprob[k] = 1
    k = NULL

    TPR = Tprob[ iitpc,, ] * Z$Zprob_lb  # sp, yr, seas, sim
    # na's created due to alignment problems with rasterization .. ignore rather than setting to zero

    # mean across space then sims
    summ_lb[,ss] =  colMeans( apply(TPR, c(1,2), mean, na.rm=TRUE  ), na.rm=TRUE )   

    TPR = ifelse( TPR > tpr_limit, 1, 0 )
    # mean across seasons,  sum across space  
    summh_lb[,ss] = colSums( apply(TPR, c(1,2), mean, na.rm=TRUE  ), na.rm=TRUE )  


    Tprob = array(NA, dim=dim(resY) )
    Tprob[fk] = x_spline_function_ub( resY[fk]  ) 

    k = which( Tprob < 0 )
    if (length(k) > 0 ) Tprob[k] = 0
    k = NULL
   
    k = which( Tprob > 1 )
    if (length(k) > 0 ) Tprob[k] = 1
    k = NULL
 
    Tprob = array(NA, dim=dim(resY) )
    TPR = Tprob[ iitpc,, ] * Z$Zprob_ub  # sp, yr, seas, sim  # na's created due to alignment problems with rasterization .. ignore rather than setting to zero

    # mean across space then sims
    summ_ub[,ss] =  colMeans( apply(TPR, c(1,2), mean, na.rm=TRUE  ), na.rm=TRUE )   

    TPR = ifelse( TPR > tpr_limit, 1, 0 )
    # mean across seasons,  sum across space  
    summh_ub[,ss] = colSums( apply(TPR, c(1,2), mean, na.rm=TRUE  ), na.rm=TRUE )  

    TPR = NULL

    h_zt$habitat  = rowMeans(summ, na.rm=TRUE)   # mean across space then sims
    h_zt$habitat_sa = rowMeans(summh, na.rm=TRUE)

    h_zt$habitat_lb  = rowMeans(summ_lb, na.rm=TRUE)   # mean across space then sims
    h_zt$habitat_sa_lb = rowMeans(summh_lb, na.rm=TRUE)

    h_zt$habitat_ub  = rowMeans(summ_ub, na.rm=TRUE)   # mean across space then sims
    h_zt$habitat_sa_ub = rowMeans(summh_ub, na.rm=TRUE)

    saveRDS( h_zt, file=file.path( work_root, "temp_depth_habitat.RDS") )  # temp save
  
  }
 

  h_zt$habitat  = rowMeans(summ, na.rm=TRUE)   # mean across space then sims
  h_zt$habitat_sa = rowMeans(summh, na.rm=TRUE)

  h_zt$habitat_lb  = rowMeans(summ_lb, na.rm=TRUE)   # mean across space then sims
  h_zt$habitat_sa_lb = rowMeans(summh_lb, na.rm=TRUE)

  h_zt$habitat_ub  = rowMeans(summ_ub, na.rm=TRUE)   # mean across space then sims
  h_zt$habitat_sa_ub = rowMeans(summh_ub, na.rm=TRUE)

 
  dev.new(pointsize=24, width=10, height=8 ) 
  plot(habitat ~ yr, h_zt, type="b", col="slategray", xlab="Year", ylab="Habitat (depth, temperature)", pch=20, cex=1.5, ylim=range( h_zt[, c("habitat_ub", "habitat_lb")]))
  lines( habitat_lb ~ yr, data=h_zt, lty="dashed", col="gray" )
  lines( habitat_ub  ~ yr, data=h_zt, lty="dashed", col="gray" )
 

  out[["temperature_depth"]] = h_zt
  out[["total_surface"]] = ntot

  return(out)

}
