library(R6)
library(assertthat)
library(ggplot2)

unit_vector = function(v) {
  return ( v / sqrt(v %*% v))
}

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

Sphere = R6Class("Sphere",
  public = list(
    center = NULL,
    radius = NULL,
    initialize = function(center = c(0,0,0), radius=1.0) {
      self$center = center
      self$radius = radius
    },
    intersects_ray = function(ray, t_min, t_max) {
      oc = ray$origin - self$center
      a = ray$direction %*% ray$direction # x^2 + y^2 + z^2
      b = oc %*% ray$direction
      c = (oc %*% oc) - self$radius * self$radius
      discriminant = b*b - a*c
      if (discriminant > 0) {
        temp = (-b - sqrt(discriminant)) / a
        if (temp < t_max & temp > t_min) {
          p = ray$point_at_parameter(temp)
          return(list(
            hit=T,
            t = temp,
            p = p,
            normal = (p - self$center) / self$radius
          ))
        }
        temp = (-b + sqrt(discriminant)) / a
        if (temp < t_max & temp > t_min) {
          p = ray$point_at_parameter(temp)
          return(list(
            hit=T,
            t=temp,
            p=p,
            normal = (p - self$center) / self$radius
          ))
        }
      }
      return(list(hit=F))
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
    },
    getBackgroundColor = function(ray) {
      unit.direction = ray$direction / ray$getLength()
      t = 0.5 * (unit.direction[2] + 1.0)
      return( (1.0-t) * c(1,1,1) + (t*c(0.5,0.7,1.0)) )
    }
  )
)

World = R6Class("World",
  public = list(
    entities = c(),
    initialize = function() {},
    addEntity = function(entity) {
      self$entities = c(self$entities, entity)
      return(invisible(self))
    },
    collide = function(ray, t_min, t_max) {
      hit_anything = F
      closest_so_far = t_max
      result = list(hit=F)
      for (i in 1:length(self$entities)) {
        collide_result = self$entities[[i]]$intersects_ray(ray, t_min, closest_so_far)
        if (collide_result$hit == T) {
          hit_anything = T
          closest_so_far = collide_result$t
          result = collide_result
        }
      }
      return(result)
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
  world$addEntity(Sphere$new(center=c(0,-100.5,-1), radius=100))

  for (j in 1:bmp$height) {
    if (j %% 10 == 0) {
      cat("Row ", j, "\n")
      bmp$plot()
    }
    for (i in 1:bmp$width) {
      col = c(0,0,0)
      for (s in 1:10) {
        u = (i - runif(1,max=0.9)) / bmp$width
        v = (j - runif(1,max=0.9)) / bmp$height
        ray = camera$getEyeRay(u, v)
        wc = world$collide(ray, 0.0, 1000000000)
        if (wc$hit == T) {
          col = col + (0.5*(wc$normal+1))
        }
        else
          col = col + camera$getBackgroundColor(ray)
      }
      bmp$setPixel(i, j, col/10)
    }
  }
  
  bmp$plot()
}