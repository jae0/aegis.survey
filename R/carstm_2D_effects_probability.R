
carstm_2D_effects_probability = function( res, 
  xvar = "inla.group(t, method = \"quantile\", n = 11)",  
  yvar = "inla.group(z, method = \"quantile\", n = 11)",
  xlab = "temperature",
  ylab = "depth",
  zlab = "probability",
  xgrid = seq(-1, 12, by=0.5),
  ygrid = seq(0,500, by=50),
  xslice= 4,
  yslice= -100,
  nx = 100,
  ny = 100,
  theta = 235,
  phi = 15,
  y_flip = TRUE
  ) {

  
  x_spline_function = carstm_spline( res, vn=c("random", xvar), statvar="mean" ) 
  x_spline_function_lb = carstm_spline( res, vn=c("random", xvar), statvar="quant0.025" ) 
  x_spline_function_ub = carstm_spline( res, vn=c("random", xvar), statvar="quant0.975" ) 

  
  y_spline_function = carstm_spline( res, vn=c("random", yvar), statvar="mean" ) 
  y_spline_function_lb = carstm_spline( res, vn=c("random", yvar), statvar="quant0.025" ) 
  y_spline_function_ub = carstm_spline( res, vn=c("random", yvar), statvar="quant0.975" ) 
 

  PB = expand.grid( x=xgrid, y=ygrid)
  PB$px = x_spline_function(PB$x) 
  PB$px_lb = x_spline_function_lb(PB$x) 
  PB$px_ub = x_spline_function_ub(PB$x) 

  PB$py = y_spline_function(PB$y)
  PB$py_lb = y_spline_function_lb(PB$y)
  PB$py_ub = y_spline_function_ub(PB$y)

  if (y_flip) PB$y = - PB$y

  for (i in c("px", "py", "px_lb", "py_lb", "px_ub", "py_ub") ) {
    o = which( PB[,i] < 0) 
    if (length(o) > 0) PB[o,i] = 0
    o = which( PB[,i] > 1) 
    if (length(o) > 0) PB[o,i] = 1
  }

  PB$pz = PB$px * PB$py
  PB$pz_lb = PB$px_lb * PB$py_lb
  PB$pz_ub = PB$px_ub * PB$py_ub

  dev.new( width=12, height=12, pointsize=18)
  par(mai=c(1, 1, 0.6, 0.6)) 
  layout( matrix(1:4, ncol=2, byrow=TRUE ))

  plot( y ~ py, PB[which(PB$x==xslice),], type="b", pch=19, xlab=zlab, ylab=ylab, xlim=c(0,1))
  lines( y ~ py_lb, PB[which(PB$x==xslice),], lty="dashed")
  lines( y ~ py_ub, PB[which(PB$x==xslice),], lty="dashed")

  require(MBA)
  Z = mba.surf(PB[, c("x", "y", "pz")], no.X=nx, no.Y=ny, extend=TRUE) $xyz.est

  image(Z, col = rev(gray.colors(30, gamma=1.75)), xlab=xlab, ylab=ylab )
  contour(Z, add = TRUE, drawlabels = TRUE, lty="dotted")

  library(plot3D)
  # reduce res to see texture in 3D
  nx = 30
  ny = 30
  Z = mba.surf(PB[, c("x", "y", "pz")], no.X=nx, no.Y=ny, extend=TRUE) $xyz.est
  xx = t(t(rep(1, ny))) %*% Z$x
  yy = t( t(t(rep(1, nx))) %*% Z$y )
  surf3D( x=xx, y=yy, z=Z$z, colkey = TRUE, xlab=xlab, ylab=ylab, zlab =zlab,
        box = TRUE, bty = "b", phi = phi, theta = theta, contour=TRUE, ticktype = "detailed") 

  plot( px ~ x, PB[which(PB$y==yslice),], type="b", pch=19, xlab=xlab, ylab=zlab, ylim=c(0,1))
  lines( px_lb ~ x, PB[which(PB$y==yslice),], lty="dashed" )
  lines( px_ub ~ x, PB[which(PB$y==yslice),], lty="dashed" )

  out = list(
    PB=PB, 
    x_spline_function=x_spline_function,
    x_spline_function_lb=x_spline_function_lb,
    x_spline_function_ub=x_spline_function_ub,
    y_spline_function=y_spline_function,
    y_spline_function_lb=y_spline_function_lb,
    y_spline_function_ub=y_spline_function_ub
  )
  return(out)

}