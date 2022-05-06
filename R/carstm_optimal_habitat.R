
carstm_optimal_habitat = function( 
    domain, 
    res, 
    xvar = "inla.group(t, method = \"quantile\", n = 11)",  
    yvar = "inla.group(z, method = \"quantile\", n = 11)",
    proj_planar="+proj=utm +ellps=WGS84 +zone=20 +units=km", 
    depths=NULL, 
    temperatures=NULL 
  
  ) {

  require(ggplot2)

  crs_plot = st_crs( domain )
  out = list()


  if (!is.null(depths)) {
    # use highest resolution depths and aggregate temps
    # "aggregated" == depths aggregated (averaged) to 0.5 km X 0.5 km basis
    # "complete" = 1x1 km

    Z = aegis.bathymetry::bathymetry_db( 
      p = aegis.bathymetry::bathymetry_parameters( spatial_domain="SSE", project_class="stmv" ), 
      DS ="complete" 
    )  
    Z = planar2lonlat(Z, proj.type=proj_planar ) 
    Z$depth = Z$z 

    crs_lonlat = st_crs(projection_proj4string("lonlat_wgs84"))
    inside = st_points_in_polygons(
      pts = st_as_sf( Z[, c("lon", "lat")], coords=c("lon","lat"), crs=crs_lonlat ),
      polys = st_transform( st_union(domain), crs_lonlat )
    )

    Z = Z[which(is.finite(inside)), ]
    Z$depth[ Z$depth > depths[2] ] =  NA
    Z$depth[ Z$depth < depths[1] ] =  NA

    dr = seq( depths[1], depths[2], by=5 ) 
 
    isobaths = c( 10, 100, 200, 300, 400, 800 )
    isobs = aegis.bathymetry::isobath_db( depths=isobaths, project_to=crs_plot )
    isobs = st_intersection(isobs, domain)
  
    o = ggplot() +
        geom_sf( data=isobs, aes(alpha=0.1), colour="lightgray" ) +
        geom_raster(data = Z, aes(x=plon, y=plat, fill=depth, alpha=1.0) ) +
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
    (o)  
    print( paste( "Surface area for depths: ", length(which(is.finite(Z$depth))) ) ) 
    out["depth"] = isobs
  }

  if (!is.null(temperatures)) {

    x_spline_function = carstm_spline( res, vn=c("random", xvar), statvar="mean" ) 
    x_spline_function_lb = carstm_spline( res, vn=c("random", xvar), statvar="quant0.025" ) 
    x_spline_function_ub = carstm_spline( res, vn=c("random", xvar), statvar="quant0.975" ) 

    
    y_spline_function = carstm_spline( res, vn=c("random", yvar), statvar="mean" ) 
    y_spline_function_lb = carstm_spline( res, vn=c("random", yvar), statvar="quant0.025" ) 
    y_spline_function_ub = carstm_spline( res, vn=c("random", yvar), statvar="quant0.975" ) 
  
    # add temperature:
    require(aegis.temperature)
    year.assessment = 2021
    pT = temperature_parameters( project_class="carstm", yrs=1970:year.assessment, carstm_model_label="1970_present"  ) 
  
    sppolyT = areal_units( p=pT )  # same
  
    resT = carstm_model( p=pT, DS="carstm_modelled_summary", sppoly=sppolyT  ) # to load currently saved results
    resT = resT$predictions[,,,"mean"]

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
    
    bb = raster::bbox(raster_template)
    
    # iz = which( is.finite( Z$depth ) )
    ic = array_map( "xy->1", Z[, c("plon", "plat")], dims=dim(resT)[1:2], res=c(1,1), origin=bb[,"min"] )
    ZTP = resT[ iTP[ic],,] # select matching row numbers of TP and resT
    Tprob = ZTP[]*NA
    j = which(is.finite( ZTP))
    Tprob[j] = x_spline_function(ZTP[j]) 
    k = which( ZTP > 15 )
    Tprob[k] = NA
    Tprob[ Tprob < 0 ] = NA
  

    Zprob = Z$z*NA
    j = which(is.finite( Z$z))
    Zprob[j] = y_spline_function(Z$z[j]) 
    k = which( Z$z < 10 )  # drop extrapolation 
    Zprob[k] = NA
    k = which( Z$z > 500 ) # drop extrapolation
    Zprob[k] = NA
    Zprob[ Zprob < 0 ] = NA  # beyond prediction range

    tp = apply(Tprob*Zprob, c(2,3), mean, na.rm=TRUE )

    h_zt = data.frame(yr=p$yrs, habitat=rowMeans(tp) ) 

    dev.new(pointsize=24, width=10, height=8 ) 
    plot(habitat ~ yr, h_zt, type="b", col="slategray", xlab="Year", ylab="Habitat (depth|temperature)", pch=20, cex=1.5 )

    out["temperature_depth"] = h_zt
    out["temperature"] = Tprob
    out["depth"] = Zprob

  }

  return(out)

}