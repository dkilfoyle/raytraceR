library(magrittr)
library(ggplot2)

createBMP = function(width=200, height=100) {
  return(list(
    width=width,
    height=height,
    pixels=matrix(0, nrow=width*height, ncol=3, dimnames=list(NULL, c("R","G","B")))
  ))
}

bmp.setPixel = function(bmp, x, y, color) {
  bmp$pixels[(y-1)*bmp$width+x,] = color
  return(bmp)
}

bmp.plot = function(bmp) {
  z=as.data.frame(bmp$pixels)
  z$y = rep(1:bmp$height, each=bmp$width)
  z$x = rep(1:bmp$width, times=bmp$height)
  
    p = ggplot(data=z, aes(x=x, y=y, fill=rgb(R,G,B))) +
      scale_fill_identity() +
      geom_tile(show.legend=F) +
      labs(x=NULL,y=NULL) +
      coord_fixed() +
      theme_minimal()

    print(p)
}

createRay = function(origin = c(0,0,0), direction = c(1,0,0)) {
  return(list(
    origin=origin,
    direction=direction
  ))
}

ray.length = function(ray) {
  return(sqrt(ray$direction %*% ray$direction))
}

hit.sphere = function(center, radius, ray) {
  oc = ray$origin - center
  a = ray$direction %*% ray$direction # x^2 + y^2 + z^2
  b= 2.0 * (oc %*% ray$direction)
  c = (oc %*% oc) - radius * radius
  discriminant = b * b - 4 * a* c
  return(discriminant>0)
}

getBackgroundColor = function(ray) {
  unit.direction = ray$direction / ray.length(ray)
  t = 0.5 * (unit.direction[2] + 1.0)
  return( (1.0-t) * c(1,1,1) + (t*c(0.5,0.7,1.0)) )
}

tst = function() {
  lower.left.corner = c(-2,-1,-1)
  horizontal=c(4,0,0)
  vertical=c(0,2,0)
  origin=c(0,0,0)
  bmp = createBMP()
  for (j in 1:bmp$height) {
    for (i in 1:bmp$width) {
      u = i/bmp$width
      v = j/bmp$height
      ray = createRay(origin=origin, direction=(lower.left.corner + (u*horizontal) + (v*vertical)))
      if (hit.sphere(center=c(0,0,-1), radius=0.5, ray)) {
        bmp$pixels[(j-1)*bmp$width+i,]=c(1,0,0)
      }
      else
        bmp$pixels[(j-1)*bmp$width+i,]=getBackgroundColor(ray)
    }
  }
  bmp.plot(bmp)
}