
discretizations = function( x=NULL, p=NULL) {
  
  z = c(0, 10, 20, 40, 80, 100, 150, 200, 250, 300, 350, 400, 500, 1000, 2000, 5000, 8000 )
  dz = c(0.01, 0.1,  1, 2, 4, 6, 8, 10, 12 )
  ddz = c(0.01, 0.1, 0.2, 0.4, 0.8, 1, 2, 4  )
  substrate.grainsize = c( 0, 1, 2, 4, 8, 12, 16, 20, 32 )
  pca1 = seq( -1, 1, by=0.1  )
  pca2 = seq( -1, 1, by=0.1  )
  t = seq( -4, 25, by=1  )
  tsd = seq( 0, 25, by=1  )
  tmin = seq( -4, 25, by=1  )
  tmax = seq( -4, 25, by=1  )
  degreedays = c(10, 100, 200, 400, 800, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 5000)
  dyear = seq( 0, 1, by=0.1  )

  disc = list(    
       z=z,
        dz=dz,
        ddz=ddz,
        substrate.grainsize=substrate.grainsize,
        pca1=pca1,
        pca2=pca2,
        t=t,
        tsd=tsd,
        tmin=tmin,
        tmax=tmax,
        degreedays=degreedays,
        dyear=dyear
  )


  if (!is.null(x)) {
    if (is.null(p)) {
      return( disc[[x]] )
    } else{
      return( parameters_add_without_overwriting( p, disc[[x]] ) )
    }
  } else {
    if (is.null(p)) {
      return( disc )
    } else{
      return( parameters_add_without_overwriting( p,  
        z=z,
        dz=dz,
        ddz=ddz,
        substrate.grainsize=substrate.grainsize,
        pca1=pca1,
        pca2=pca2,
        t=t,
        tsd=tsd,
        tmin=tmin,
        tmax=tmax,
        degreedays=degreedays,
        dyear=dyear
      ) )
    }
  }
    return(disc)
} 
