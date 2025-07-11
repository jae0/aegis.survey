
carstm_optimal_habitat = function( 
    domain, 
    res, 
    year.assessment = 2021,
    xvar = "inla.group(t, method = \"quantile\", n = 11)",  
    yvar = "inla.group(z, method = \"quantile\", n = 11)",
    depths=NULL, 
    plot_map=FALSE,
    nsims=100, 
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
  
  pT = temperature_parameters( project_class="carstm", yrs=1970:year.assessment, carstm_model_label="default"  ) 

  h_zt = data.frame(yr=pT$yrs  ) 

  sppolyT = st_transform( areal_units( p=pT ), crs=crs_domain )  
 

  raster_resolution_km = 1 # res of Z (pB$pres)
 
  require(stars) 
 
  TP = st_transform( sppolyT["AUID"], crs_domain)  # same
  TP = st_make_valid( TP )
  TP = st_buffer( TP, 0)
  TP$rn = 1:nrow(TP)
  TP = st_cast(TP, "MULTIPOLYGON")

  iTP = st_rasterize( TP["rn"], dx=raster_resolution_km, dy=raster_resolution_km )

  zz = st_as_sf(Z[,c("plon", "plat") ], coords=c("plon", "plat") )
  st_crs(zz) = crs_domain
  ic = st_extract(iTP, zz )$rn

  TP = NULL
  iTP = zz = NULL 
  sppolyT = NULL
  domain = NULL
  gc()

 
  # to load currently saved results
  resT = carstm_model( p=pT, DS="carstm_samples", sppoly=sppolyT )[["predictions"]]
  sims_to_keep = sample.int( dim(resT)[4], nsims )
  resT = resT[,,,sims_to_keep]  # reduce size to keep RAM from saturating

  dim_resT = dim(resT)
   
  if (!is.null(probability_limit)) {
    tpr_limit = probability_limit
  } else {
    tpr_limit = 0.25
  }

  ny = dim_resT[2]
  ns = dim_resT[4]

  summ = summh = summ_lb = summh_lb = summ_ub = summh_ub =array( NA, dim=c(ny, ns) )

  message("This will take a while ... ")

  fn_monitor = file.path( work_root, "temp_depth_habitat.rdz")
  message( "load the following to monitor : u = aegis::read_write_fast(", fn_monitor, ")" ) 
  

  for ( ss in 1:ns ) {
    
    print( paste( "Sim: ", ss, "of", ns ) )
     
    resY = resT[,,,ss] 
    
    fk = which( is.finite(resY) )

    Tprob = array(NA, dim=dim(resY) )
    Tprob[fk] = x_spline_function( resY[fk]  ) 

    k = which( Tprob < 0 )
    if (length(k) > 0 ) Tprob[k] = 0
   
    k = which( Tprob > 1 )
    if (length(k) > 0 ) Tprob[k] = 1

    TPR = Tprob[ ic,, ] * Z$Zprob  # sp, yr, seas, sim
    # na's created due to alignment problems with rasterization .. ignore rather than setting to zero
   
    # mean across space then sims
    summ[,ss] =  colMeans( apply(TPR, c(1,2), mean, na.rm=TRUE ), na.rm=TRUE )   

    TPR = ifelse( TPR > tpr_limit, 1, 0 )
    # mean across seasons,  sum across space  
    summh[,ss] = colSums( apply(TPR, c(1,2), mean, na.rm=TRUE ), na.rm=TRUE )  


    Tprob = array(NA, dim=dim(resY) )
    Tprob[fk] = x_spline_function_lb( resY[fk]  ) 

    k = which( Tprob < 0 )
    if (length(k) > 0 ) Tprob[k] = 0
   
    k = which( Tprob > 1 )
    if (length(k) > 0 ) Tprob[k] = 1 

    TPR = Tprob[ ic,, ] * Z$Zprob  # sp, yr, seas, sim
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
   
    k = which( Tprob > 1 )
    if (length(k) > 0 ) Tprob[k] = 1 
  
    TPR = Tprob[ ic,, ] * Z$Zprob  # sp, yr, seas, sim  # na's created due to alignment problems with rasterization .. ignore rather than setting to zero

    # mean/min across space then sims
    summ_ub[,ss] =  colMeans( apply(TPR, c(1,2), mean, na.rm=TRUE  ), na.rm=TRUE )   

    TPR = ifelse( TPR > tpr_limit, 1, 0 )
    # mean/min across seasons,  sum across space  
    summh_ub[,ss] = colSums( apply(TPR, c(1,2), mean, na.rm=TRUE  ), na.rm=TRUE )  

    TPR = NULL

    h_zt$habitat  = rowMeans(summ, na.rm=TRUE)   # mean across space then sims
    h_zt$habitat_sa = rowMeans(summh, na.rm=TRUE)

    h_zt$habitat_lb  = rowMeans(summ_lb, na.rm=TRUE)   # mean across space then sims
    h_zt$habitat_sa_lb = rowMeans(summh_lb, na.rm=TRUE)

    h_zt$habitat_ub  = rowMeans(summ_ub, na.rm=TRUE)   # mean across space then sims
    h_zt$habitat_sa_ub = rowMeans(summh_ub, na.rm=TRUE)

    read_write_fast( data=h_zt, fn=fn_monitor )  # temp save
  
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
