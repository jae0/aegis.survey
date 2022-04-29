set_surface_area_to_NA = function( sppoly, auid_to_drop ) {
    
  # this is to match Michelle's extraction for "Summer RV" for final aggregation and plotting 
  auid_to_drop = strata_definitions( c("Gulf", "Georges_Bank", "Spring", "Deep_Water") ) 
  gfpoly = maritimes_groundfish_strata()

  gfpoly = gfpoly[ -which( gfpoly$AUID %in% auid_to_drop ), ]
  gfpoly = st_transform( gfpoly, st_crs("+proj=utm +ellps=WGS84 +zone=20 +units=km"))
  domain = st_union( st_buffer(gfpoly, 1 ))  # to remover slivers
  domain = st_make_valid(domain)

  sppoly = st_transform( sppoly, st_crs("+proj=utm +ellps=WGS84 +zone=20 +units=km") )
  ooo = st_intersection(  sppoly, domain )
  ooo$surfacearea = st_area( ooo )
  attributes(ooo$surfacearea) = NULL
  sppoly$au_sa_km2 = NA
  j = match( ooo$AUID, sppoly$AUID )      
  sppoly$au_sa_km2[j] = ooo$surfacearea  # force new SA estimates
  plot(sppoly["au_sa_km2"] )
  return(  sppoly ) 
}
