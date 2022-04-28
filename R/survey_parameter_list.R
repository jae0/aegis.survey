
survey_parameter_list = function( p, model_label, type="abundance" ) {

  p$label = model_label 
  p$type = type

   
    if (model_label == "S_fac.T_fac" ) {

      if (p$type =="abundance") {
        p$label ="Atlantic cod summer standardtow totno"
        p$carstm_model_label = model_label
        p$variabletomodel = "totno"  
        p$family = "poisson" 
        p$formula = formula( 
          totno ~ 1 + offset( data_offset )
            + as.factor(space)
            + as.factor(time)
        )
      }

      if (p$type =="meansize") {
        p$label ="Atlantic cod summer standardtow meansize"
        p$carstm_model_label = model_label
        p$variabletomodel = "meansize"  
        p$family = "gaussian" 
        p$formula = formula ( 
          meansize ~ 1  
            + as.factor(space)
            + as.factor(time)
        )
      }

      if (p$type =="habitat") {
        p$label ="Atlantic cod summer standardtow habitat"
        p$carstm_model_label = model_label
        p$variabletomodel = "pa"  
        p$family = "binomial" 
        p$formula = formula( 
            pa ~ 1  
              + as.factor(space)
              + as.factor(time)
        )
      }

    }

    if (model_label == "SxT" ) {
      
      if (p$type =="abundance") {
        p$label ="Atlantic cod summer standardtow totno"
        p$carstm_model_label = model_label
        p$variabletomodel = "totno"  
        p$family = "poisson" 
        p$formula = formula( 
            totno ~ 1 + offset( data_offset )
              + as.factor(space) * as.factor(time)
        )
      }

      if (p$type =="meansize") {
        p$label ="Atlantic cod summer standardtow meansize"
        p$carstm_model_label = model_label
        p$variabletomodel = "meansize"  
        p$family = "gaussian" 
        p$formula = formula ( 
            meansize ~ 1  
              + as.factor(space) * as.factor(time)
        )
      }
    }
  
    if (model_label == "SiT.ST_iid" ) {
      
      if (p$type =="abundance") {
        p$label ="Atlantic cod summer standardtow totno"
        p$carstm_model_label = model_label
        p$variabletomodel = "totno"  
        p$family = "poisson" 
        p$formula = formula( 
            totno ~ 1 + offset( data_offset )
              + as.factor(space) : as.factor(time)
              + f( space_time, model="iid",  group=time_space, hyper=H$iid )  # required to stabilize missing combos
  #            + f( space_time, model="iid",  group=time_space, hyper=H$iid, control.group=list(model="ar1", hyper=H$ar1_group))  # required to stabilize missing combos
        )
      }

      if (p$type =="meansize") {
        p$label ="Atlantic cod summer standardtow meansize"
        p$carstm_model_label = model_label
        p$variabletomodel = "meansize"  
        p$family = "gaussian" 
        p$formula = formula ( 
            meansize ~ 1 
              + as.factor(space) : as.factor(time)
              + f( space_time, model="iid",  group=time_space, hyper=H$iid )  # required to stabilize missing combos
  #            + f( space_time, model="iid",  group=time_space, hyper=H$iid, control.group=list(model="ar1", hyper=H$ar1_group))  # required to stabilize missing combos
        )
      }
    }
  

    if (model_label == "SxT.ST_iid" ) {
      
      if (p$type =="abundance") {
        p$label ="Atlantic cod summer standardtow totno"
        p$carstm_model_label = model_label
        p$variabletomodel = "totno"  
        p$family = "poisson" 
        p$formula = formula( 
            totno ~ 1 + offset( data_offset )
              + as.factor(space) * as.factor(time)
              + f( space_time, model="iid",  group=time_space, hyper=H$iid, control.group=list(model="iid", hyper=H$iid))  # required to stabilize missing combos
        )
      }

      if (p$type =="meansize") {
        p$label ="Atlantic cod summer standardtow meansize"
        p$carstm_model_label = model_label
        p$variabletomodel = "meansize"  
        p$family = "gaussian" 
        p$formula = formula ( 
            meansize ~ 1 
              + as.factor(space) * as.factor(time)
              + f( space_time, model="iid",  group=time_space, hyper=H$iid, control.group=list(model="iid", hyper=H$iid))  # required         )
        )
      } 
    }
  
    if (model_label == "S_iid.T_iid" ) {
      
      if (p$type =="abundance") {
        p$label ="Atlantic cod summer standardtow totno"
        p$carstm_model_label = model_label
        p$variabletomodel = "totno"  
        p$family = "poisson" 
        p$formula = formula( 
            totno ~ 1 + offset( data_offset )
              + f(space, model="iid", hyper=H$iid)
              + f(time,  model="iid", hyper=H$iid )
        )
      }

      if (p$type =="meansize") {
        p$label ="Atlantic cod summer standardtow meansize"
        p$carstm_model_label = model_label
        p$variabletomodel = "meansize"  
        p$family = "gaussian" 
        p$formula = formula ( 
            meansize ~ 1  
              + f(space, model="iid", hyper=H$iid)
              + f(time,  model="iid", hyper=H$iid )
        )
      } 


      if (p$type =="habitat") {
        p$label ="Atlantic cod summer standardtow habitat"
        p$carstm_model_label = model_label
        p$variabletomodel = "pa"  
        p$family = "binomial" 
        p$formula = formula( 
            pa ~ 1  
              + f(space, model="iid", hyper=H$iid)
              + f(time,  model="iid", hyper=H$iid )
        )
      }
    }


    if (model_label == "S_iid.T_iid.ST_iid" ) {
 
      if (p$type =="abundance") {
        p$label ="Atlantic cod summer standardtow totno"
        p$carstm_model_label = model_label
        p$variabletomodel = "totno"  
        p$family = "poisson" 
        p$formula = formula( 
            totno ~ 1 + offset( data_offset )
            + f(space, model="iid", hyper=H$iid)
            + f(time,  model="iid", hyper=H$iid )
            + f(space_time, model="iid", group=time_space, hyper=H$iid)
        )
      }

      if (p$type =="meansize") {
        p$label ="Atlantic cod summer standardtow meansize"
        p$carstm_model_label = model_label
        p$variabletomodel = "meansize"  
        p$family = "gaussian" 
        p$formula = formula ( 
            meansize ~ 1  
            + f(space, model="iid", hyper=H$iid)
            + f(time,  model="iid", hyper=H$iid )
            + f(space_time, model="iid", group=time_space, hyper=H$iid)
        )
      }

      if (p$type =="habitat") {
        p$label ="Atlantic cod summer standardtow habitat"
        p$carstm_model_label = model_label
        p$variabletomodel = "pa"  
        p$family = "binomial" 
        p$formula = formula( 
            pa ~ 1 
            + f(space, model="iid", hyper=H$iid)
            + f(time,  model="iid", hyper=H$iid )
            + f(space_time, model="iid", group=time_space, hyper=H$iid)
        )
      }


    }


    if (model_label == "S_bym2.T_fac.ST_bym2.env.eco" ) {
 
      if (p$type =="abundance") {
        p$label ="Atlantic cod summer standardtow totno"
        p$carstm_model_label = model_label
        p$variabletomodel = "totno"  
        p$family = "poisson" 
        p$formula = formula( 
            totno ~ 1 + offset( data_offset )
              + as.factor(time)  
  #            + f( time, model="ar1",  hyper=H$ar1 ) 
              + f( cyclic, model="rw2", scale.model=TRUE, hyper=H$rw2, cyclic =TRUE, values=cyclic_values   ) 
              + f( inla.group( t, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( inla.group( z, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( inla.group( log.substrate.grainsize, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( inla.group( pca1, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( inla.group( pca2, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE,  hyper=H$bym2 ) 
              + f( space_time, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, group=time_space,  hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group)) 
        )
      }

      if (p$type =="meansize") {
        p$label ="Atlantic cod summer standardtow meansize"
        p$carstm_model_label = model_label
        p$variabletomodel = "meansize"  
        p$family = "lognormal" 
        p$formula = formula ( 
            meansize ~ 1  
              + as.factor(time)   
  #            + f( time, model="ar1",  hyper=H$ar1 ) 
              + f( cyclic, model="rw2", scale.model=TRUE, hyper=H$rw2, cyclic =TRUE, values=cyclic_values   ) 
              + f( inla.group( t, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( inla.group( z, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( inla.group( substrate.grainsize, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( inla.group( pca1, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( inla.group( pca2, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE,  hyper=H$bym2 ) 
              + f( space_time, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, group=time_space,  hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group)) 
        )
      }
    
       if (p$type =="habitat") {
        p$label ="Atlantic cod summer standardtow habitat"
        p$carstm_model_label = model_label
        p$variabletomodel = "pa"  
        p$family = "binomial" 
        p$formula = formula( 
            pa ~ 1 
              + as.factor(time)   
  #            + f( time, model="ar1",  hyper=H$ar1 ) 
              + f( gear, model="iid",  hyper=H$iid ) 
              + f( cyclic, model="rw2", scale.model=TRUE, hyper=H$rw2, cyclic =TRUE, values=cyclic_values   ) 
              + f( inla.group( t, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( inla.group( z, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( inla.group( substrate.grainsize, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( inla.group( pca1, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( inla.group( pca2, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE,  hyper=H$bym2 ) 
              + f( space_time, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, group=time_space,  hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group)) 
        )
      }    
    
    }


    if (model_label == "S_bym2.T_ar1.ST_bym2.env.eco" ) {
 
      if (p$type =="abundance") {
        p$label ="Atlantic cod summer standardtow totno"
        p$carstm_model_label = model_label
        p$variabletomodel = "totno"  
        p$family = "poisson" 
        p$formula = formula( 
          totno ~ 1 + offset( data_offset )
              + f( gear, model="iid",  hyper=H$iid ) 
              + f( time, model="ar1",  hyper=H$ar1 ) 
              + f( cyclic, model="rw2", scale.model=TRUE, hyper=H$rw2, cyclic =TRUE, values=cyclic_values   ) 
              + f( inla.group( t, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( inla.group( z, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( inla.group( log.substrate.grainsize, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE,  hyper=H$bym2 ) 
              + f( space_time, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, group=time_space,  hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group)) 
        )
      }

      if (p$type =="meansize") {
        p$label ="Atlantic cod summer standardtow meansize"
        p$carstm_model_label = model_label
        p$variabletomodel = "meansize"  
        p$family = "lognormal" 
        p$formula = formula ( 
            meansize ~ 1  
              + f( gear, model="iid",  hyper=H$iid ) 
              + f( time, model="ar1",  hyper=H$ar1 ) 
              + f( cyclic, model="rw2", scale.model=TRUE, hyper=H$rw2, cyclic =TRUE, values=cyclic_values   ) 
              + f( inla.group( t, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( inla.group( z, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( inla.group( log.substrate.grainsize, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE,  hyper=H$bym2 ) 
              + f( space_time, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, group=time_space,  hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group)) 
        )
      }
    
    
      if (p$type =="habitat") {
        p$label ="Atlantic cod summer standardtow habitat"
        p$carstm_model_label = model_label
        p$variabletomodel = "pa"  
        p$family = "binomial" 
        p$formula = formula( 
            pa ~ 1 
              + f( gear, model="iid",  hyper=H$iid ) 
              + f( time, model="ar1",  hyper=H$ar1 ) 
              + f( cyclic, model="rw2", scale.model=TRUE, hyper=H$rw2, cyclic =TRUE, values=cyclic_values   ) 
              + f( inla.group( t, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( inla.group( z, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( inla.group( log.substrate.grainsize, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE,  hyper=H$bym2 ) 
              + f( space_time, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, group=time_space,  hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group)) 
        )
      }      
    
    } 
    

    if (model_label == "full_model" ) {
 
      if (p$type =="abundance") {
        p$label ="Atlantic cod summer standardtow totno"
        p$carstm_model_label = model_label
        p$variabletomodel = "totno"  
        p$family = "poisson" 
        p$formula = formula( 
          totno ~ 1 + offset( data_offset )
              + f( gear, model="iid",  hyper=H$iid ) 
              + f( time, model="ar1",  hyper=H$ar1 ) 
              + f( cyclic, model="rw2", scale.model=TRUE, hyper=H$rw2, cyclic =TRUE, values=cyclic_values   ) 
              + f( inla.group( t, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( inla.group( z, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( inla.group( log.substrate.grainsize, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( inla.group( pca1, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( inla.group( pca2, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE,  hyper=H$bym2 ) 
              + f( space_time, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, group=time_space,  hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group)) 
        )
      }

      if (p$type =="meansize") {
        p$label ="Atlantic cod summer standardtow meansize"
        p$carstm_model_label = model_label
        p$variabletomodel = "meansize"  
        p$family = "lognormal" 
        p$formula = formula ( 
            meansize ~ 1  
              + f( gear, model="iid",  hyper=H$iid ) 
              + f( time, model="ar1",  hyper=H$ar1 ) 
              + f( cyclic, model="rw2", scale.model=TRUE, hyper=H$rw2, cyclic =TRUE, values=cyclic_values   ) 
              + f( inla.group( t, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( inla.group( z, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( inla.group( log.substrate.grainsize, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( inla.group( pca1, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( inla.group( pca2, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE,  hyper=H$bym2 ) 
              + f( space_time, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, group=time_space,  hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group)) 
        )
      }    

       
      if (p$type =="habitat") {
        p$label ="Atlantic cod summer standardtow habitat"
        p$carstm_model_label = model_label
        p$variabletomodel = "pa"  
        p$family = "binomial" 
        p$formula = formula( 
            pa ~ 1 
              + f( gear, model="iid",  hyper=H$iid ) 
              + f( time, model="ar1",  hyper=H$ar1 ) 
              + f( cyclic, model="rw2", scale.model=TRUE, hyper=H$rw2, cyclic =TRUE, values=cyclic_values   ) 
              + f( inla.group( t, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( inla.group( z, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( inla.group( log.substrate.grainsize, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( inla.group( pca1, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( inla.group( pca2, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2) 
              + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE,  hyper=H$bym2 ) 
              + f( space_time, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, group=time_space,  hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group)) 
        )
      }      

    }  
    
    return(p)
}