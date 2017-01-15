library(R6)
library(assertthat)
library(ggplot2)

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
    getLength = function() { return( sqrt(self$direction %*% self$direction)) }
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
    intersects_ray = function(ray) {
      oc = ray$origin - self$center
      a = ray$direction %*% ray$direction # x^2 + y^2 + z^2
      b= 2.0 * (oc %*% ray$direction)
      c = (oc %*% oc) - self$radius * self$radius
      discriminant = b * b - 4 * a* c
      return(discriminant>0)
    }
  ))

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

tst = function() {
  camera = Camera$new(
    origin = c(0,0,0),
    lower_left_corner = c(-2,-1,-1),
    horizontal = c(4,0,0),
    vertical=c(0,2,0))
  
  bmp = Bitmap$new(width=200, height=100)
  sphere = Sphere$new(center=c(0,0,-1), radius=0.5)

  for (j in 1:bmp$height) {
    for (i in 1:bmp$width) {
      u = i/bmp$width
      v = j/bmp$height
      ray = camera$getEyeRay(u, v)
      if (sphere$intersects_ray(ray)) {
        bmp$setPixel(i,j,c(1,0,0))
      }
      else
        bmp$setPixel(i,j, camera$getBackgroundColor(ray))
    }
  }
  
  bmp$plot()
}