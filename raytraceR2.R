library(R6)
library(assertthat)
library(ggplot2)

unit_vector = function(v) {
  return ( v / sqrt(v %*% v))
}

get_squared_length = function(v) {
  return(v %*% v)
}

random_in_unit_sphere = function(n) {
  x = 2 * matrix(runif(3*n), ncol=3, nrow=n) - 1
  return (x[rowSums(x * x) < 1.0, ])
}

rius = random_in_unit_sphere(5000)

Bitmap = R6Class("Bitmap",
  public = list(
    width = NULL,
    height = NULL,
    pixels = NULL,
    initialize = function(width=200, height=100) {
      self$width=width
      self$height=height
      self$cls()
    },
    cls = function() {
      self$pixels=matrix(0, nrow=self$width*self$height, ncol=3, 
        dimnames=list(NULL, c("R","G","B")))
    },
    setPixel = function(x, y, color) {
      assert_that(x <= self$width)
      assert_that(y <= self$height)
      self$pixels[(y-1)*self$width+x, ] = color
    },
    plot = function() {
      z=as.data.frame(self$pixels)
      z$y = rep(1:self$height, each=self$width)
      z$x = rep(1:self$width, times=self$height)
      
      p = ggplot(data=z, aes(x=x, y=y, fill=rgb(R,G,B))) +
        scale_fill_identity() +
        geom_tile(show.legend=F) +
        labs(x=NULL,y=NULL) +
        coord_fixed() +
        theme_minimal()
      
      print(p)
    }
  )
)

Ray = R6Class("Ray",
  public = list(
    origin = NULL,
    direction = NULL,
    initialize = function(origin = NA, direction = NA) {
      self$origin = origin
      self$direction = direction
    },
    getLength = function() { return( sqrt(self$direction %*% self$direction)) },
    point_at_parameter = function(t) {
      return (self$origin + (self$direction * t))
    }
  )
)

Camera = R6Class("Camera",
  public = list(
    lower_left_corner = NULL,
    horizontal = NULL,
    vertical = NULL,
    origin = NULL,
    initialize = function(origin=c(0,0,0), lower_left_corner=c(-2,-1,-1), horizontal=c(4,0,0), vertical=c(0,2,0)) {
      self$origin = origin
      self$horizontal = horizontal
      self$vertical = vertical
      self$lower_left_corner = lower_left_corner
    },
    getEyeRay = function(u,v) {
      assert_that(u <= 1 & v <= 1)
      return(Ray$new(
        origin=self$origin,
        direction = self$lower_left_corner + (u*self$horizontal) + (v*self$vertical)
      ))
    }
  )
)

Sphere = R6Class("Sphere",
  public = list(
    center = NULL,
    radius = NULL,
    radius2 = NULL,
    id = NULL,
    initialize = function(center = c(0,0,0), radius=1.0) {
      self$center = center
      self$radius = radius
      self$radius2 = radius * radius
    },
    
    intersects_ray = function(origin, direction, t_min, t_max) {
      oc = origin - self$center
      a = direction %*% direction # x^2 + y^2 + z^2
      b = oc %*% direction
      c = (oc %*% oc) - self$radius2
      discriminant = b*b - a*c
      if (discriminant > 0) {
        temp = (-b - sqrt(discriminant)) / a
        if (temp < t_max & temp > t_min) {
          p = origin + (direction * temp)
          return(list(
            hit=T,
            t = temp,
            p = p,
            normal = (p - self$center) / self$radius
          ))
        }
      }
      return(list(hit=F))
    },
    
    intersects_rays = function(origins, directions, tmaxs) {
      oc = sweep(origins, 2, self$center, '-') # oc = origin-self$center
      a = rowSums(directions * directions) # direction %*% direction # x^2 + y^2 + z^2
      b = rowSums(oc * directions) #oc %*% direction
      c = rowSums(oc * oc) - self$radius2 # (oc %*% oc) - self$radius * self$radius
      discriminants = b*b - a*c
      
      hits = which(discriminants > 0)
      dist = sqrt(discriminants[hits])
      dist = (-b[hits] - dist) / a[hits]
      
      pass = which(dist > 0.001 & dist < tmaxs[hits])
      
      dist = dist[pass]
      hits = hits[pass]

      if (length(hits)>0) {
        p = origins[hits,] + (directions[hits,] * dist)
        if(length(p)==3) p=t(p) # only single hit point, convert vector to single row matrix
        n  = sweep(p,2,self$center,'-') / self$radius
        return(list(hits = hits, dist=dist, p=p, n=n))
      }
      else
        return(list(hits=c(),dist=c(),p=c(),n=c()))
    }
  )
)

World = R6Class("World",
  public = list(
    entities = c(),
    counter=0,
    initialize = function() {},
    
    addEntity = function(entity) {
      entity$id = length(self$entities) + 1
      self$entities = c(self$entities, entity)
      return(invisible(self))
    },
    
    collide = function(origin, direction, t_min, t_max) {
      hit_anything = F
      closest_so_far = t_max
      result = list(hit=F)
      
      for (i in 1:length(self$entities)) {
        collide_result = self$entities[[i]]$intersects_ray(origin, direction, t_min, closest_so_far)
        if (collide_result$hit == T) {
          hit_anything = T
          closest_so_far = collide_result$t
          result = collide_result
        }
      }
      
      return(result)
    },
    
    color = function(direction, origin) {
      self$counter = self$counter + 1
      if (self$counter %% 10000 == 0) cat(self$counter,"\n")
      
      wc = self$collide(origin, direction, 0.001, 1000000000)
      
      if (wc$hit==T) {
        # produce a target within a unit sphere sitting tangent to the hitpoint
        # target = wc$p + wc$normal + rius[(self$counter %% nrow(rius)) + 1, ]
        target = wc$p + wc$normal + rius[runif(1,min=1,max=nrow(rius)), ]
        
        # recursively bounce a ray from the hit point out through the target
        return(0.5 * self$color(direction = target-wc$p, origin = wc$p))
      }
      else {
        unit.direction = direction / sqrt(direction %*% direction) 
        t = 0.5 * (unit.direction[2] + 1.0)
        return( (1.0-t) * c(1,1,1) + (t*c(0.5,0.7,1.0)) )
      }
    },
    
    getClosestCollision = function(origins, directions) {
      
      tmax = rep(1000000, nrow(directions))
      ids = rep(-1, nrow(directions))
      p = matrix(0, nrow=nrow(directions), ncol=3)
      n = matrix(0, nrow=nrow(directions), ncol=3)

      for (i in 1:length(self$entities)) {
        hits = self$entities[[i]]$intersects_rays(origins, directions, tmax)
        tmax[hits$hits] = hits$dist
        ids[hits$hits] = i
        p[hits$hits, ] = hits$p
        n[hits$hits, ] = hits$n
      }
      
      return(list(ids=ids, dist=tmax, p=p, n=n))
    },
    
    color2 = function(origins, directions, level=1) {
      
      if (length(directions)==3) directions = t(directions)
      if (length(origins==3)) origins=matrix(origins,nrow=nrow(directions), ncol=3, byrow=T)
      res = self$getClosestCollision(origins, directions)
      hits = which(res$ids != -1)
      
      colors = matrix(0, nrow=nrow(directions), ncol=3)
      
      # for hit points generate a bounce target jiggled off the normal
      if (length(hits)>0) {
        targets = res$p[hits,] + res$n[hits,] + rius[runif(length(hits),min=1,max=nrow(rius)), ]
        colors[hits, ] = 0.5 * (res$n[hits,] + c(1,1,1))
        # colors[hits, ] = 0.5 * self$color2(origins = res$p[hits,], directions=targets, level=level+1)
        # browser(condition=any(is.na(colors)))
        
        nohits = (1:nrow(directions))[-hits]
      }
      else
        nohits = 1:nrow(directions)
      
      # for not hit points calculate color from sky dome
      if (length(nohits) > 0) {
        nohitdirs = directions[nohits,]
        if (length(nohitdirs)==3) nohitdirs = t(nohitdirs)
        unit.dirs = nohitdirs / sqrt(rowSums(nohitdirs*nohitdirs))
        t = 0.5 * (unit.dirs[,2] + 1.0)
        x = 1.0-t
        colors[nohits, ] = cbind(x+t*0.5, x+t*0.7, x+t)
      }
      
      return(colors)
    }
  )
)

tst = function() {
  camera = Camera$new(
    origin = c(0,0,0),
    lower_left_corner = c(-2,-1,-1),
    horizontal = c(4,0,0),
    vertical=c(0,2,0))
  
  bmp = Bitmap$new(width=200, height=100)
  
  world = World$new()
  world$addEntity(Sphere$new(center=c(0,0,-1), radius=0.5))
  # world$addEntity(Sphere$new(center=c(0,-100.5,-1), radius=100))
  
  # matrix of u,v pairs
  v = (1:bmp$height) / bmp$height
  u = (1:bmp$width) / bmp$width
  uv = expand.grid(u=u,v=v)
  
  cols = t(apply(uv, 1, function(uvp) {
    # produce 100 rays through u,v with jiggle

    eyerays = matrix(camera$lower_left_corner, nrow=50, ncol=3, byrow=T) +
      (uvp[1] - (runif(50)/bmp$width)) %*% t(camera$horizontal) +
      (uvp[2] - (runif(50)/bmp$height)) %*% t(camera$vertical)
    
    return(colMeans(world$color2(camera$origin, eyerays)))
  }))

  bmp$pixels = cols
  colnames(bmp$pixels) = c("R","G","B")
  
  bmp$plot()
  
}