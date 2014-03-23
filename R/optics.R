#' Point class
#' 
#' Internal class for ray-tracing 2D optics
#' 
#' @param x x coordinate
#' @param y y coordinate
#' @return Object of Point class
#' @family optics
#' @keywords internal
Point <- function(x, y) {
  stopifnot(is.numeric(x) & is.finite(x))
  stopifnot(is.numeric(y) & is.finite(x))
  obj <- list(X = x,
              Y = y)
  class(obj) <- 'Point'
  return(obj)
}

#' Line class
#' 
#' Internal class for ray-tracing 2D optics
#' 
#' @param A Starting point (of Point class)
#' @param B Ending point (of Point class)
#' @return Object of Line class
#' @family optics
#' @keywords internal
Line <- function(a, b) {
  stopifnot(class(a) == 'Point')
  stopifnot(class(b) == 'Point')
  if (a$X == b$X & a$Y == b$Y) {
    stop("Degenerate line")
  }
  obj <- list(A = a,
              B = b)
  class(obj) <- 'Line'
  return(obj)
}

#' Return the length of a line
#' 
#' Internal function for ray-tracing 2D optics
#' 
#' @return Length (scalar)
#' @family optics
#' @keywords internal
length.Line <- function(line) {
  stopifnot(class(line) == 'Line')
  dx <- line$A$X - line$B$X
  dy <- line$A$Y - line$B$Y
  return(sqrt(dx*dx + dy*dy))
}

#' Scale a line by adjusting the end point
#' 
#' Internal function for ray-tracing 2D optics
#' 
#' @param s Scale
#' @return Object of Line class
#' @family optics
#' @keywords internal
scale <- function(line, s) {
  stopifnot(class(line) == 'Line')
  stopifnot(is.numeric(s) & is.finite(s))
  dx <- line$B$X - line$A$X
  dy <- line$B$Y - line$A$Y
  return(Line(line$A,
              Point(line$A$X + dx * s,
                    line$A$Y + dy * s)))
}

#' Center a line by translating so the start point is the origin
#' 
#' Internal function for ray-tracing 2D optics
#' 
#' @return Object of Line class
#' @family optics
#' @keywords internal
center <- function(line) {
  stopifnot(class(line) == 'Line')
  dx <- line$B$X - line$A$X
  dy <- line$B$Y - line$A$Y
  return(Line(Point(0,0),
              Point(dx, dy)))
}

#' Normalize a line by scaling it to be unit length
#' 
#' Internal function for ray-tracing 2D optics
#' 
#' @return Object of Line class
#' @family optics
#' @keywords internal
normalize <- function(line) {
  stopifnot(class(line) == 'Line')
  return(scale(line, 1.0 / length(line)))
}

#' Rotate a line about the starting point
#' 
#' Internal function for ray-tracing 2D optics
#' 
#' @param angle Angle to rotate (in radians)
#' @return Object of Line class
#' @family optics
#' @keywords internal
rotate <- function(line, angle) {
  stopifnot(class(line) == 'Line')
  dx <- line$B$X - line$A$X
  dy <- line$B$Y - line$A$Y
  cs <- cos(angle)
  sn <- sin(angle)
  return(Line(line$A,
              Point(line$A$X + dx * cs - dy * sn,
                    line$A$Y + dx * sn + dy * cs)))
}

#' Return an orthogonal line
#' 
#' Internal function for ray-tracing 2D optics
#' 
#' @return Object of Line class
#' @family optics
#' @keywords internal
orthogonal <- function(line) {
  stopifnot(class(line) == 'Line')
  return(rotate(line, pi/2))
}

#' Compute the angle between two lines
#' 
#' Internal function for ray-tracing 2D optics
#' 
#' @param A First line
#' @param B Second line
#' @return Angle in radians (scalar)
#' @family optics
#' @keywords internal
angle <- function(A, B) {
  stopifnot(class(A) == 'Line')
  stopifnot(class(B) == 'Line')
  
  Ax <- A$A$X - A$B$X
  Ay <- A$A$Y - A$B$Y
  Al <- length(A)
  Bx <- B$B$X - B$A$X
  By <- B$B$Y - B$A$Y
  Bl <- length(B)
  
  cross <- Ax * By - Ay * Bx
  angle <- asin(cross / Al / Bl)
  return(angle)
}

#' Circle class
#' 
#' Internal class for ray-tracing 2D optics
#' 
#' @param center Center of the circle (Point)
#' @param radius Radius of the circle (scalar)
#' @return Object of Circle class
#' @family optics
#' @keywords internal
Circle <- function(center, radius) {
  stopifnot(class(center) == 'Point')
  stopifnot(is.numeric(radius) & is.finite(radius) & radius > 0)
  obj <- list(O = center,
              R = radius)
  class(obj) <- 'Circle'
  return(obj)
}

#' Plot a Point class on an existing plot
#' 
#' Internal function for ray-tracing 2D optics
#' 
#' @param ... Other options to pass to the base graphics points function
#' @family optics
#' @keywords internal
plot.Point <- function(pt, ...) {
  points(x = pt$X, y = pt$Y, ...)
}

#' Plot a Line class on an existing plot
#' 
#' Internal function for ray-tracing 2D optics
#' 
#' @param ... Other options to pass to the base graphics segments function
#' @family optics
#' @keywords internal
plot.Line <- function(ln, ...) {
  segments(x0 = ln$A$X, y0 = ln$A$Y,
           x1 = ln$B$X, y1 = ln$B$Y,
           ...)
}

#' Plot a Circle class on an existing plot
#' 
#' Internal function for ray-tracing 2D optics
#' 
#' @param points Number of points with which to approximate a circle
#' @param around Optional Point or Line to limit the extent of the drawn circle
#' @param ... Other options to pass to the base graphics lines function
#' @family optics
#' @keywords internal
plot.Circle <- function(c, points = 100, around = NULL, ...) {
  i <- seq(0, 2*pi, length = points)
  x <- c$O$X + sin(i) * c$R
  y <- c$O$Y + cos(i) * c$R
  if (! is.null(around) & class(around) == 'Point') {
    i <- abs(x - around$X) < 2 & abs(y - around$Y) < 10
    x <- x[i]
    y <- y[i]
  }
  if (! is.null(around) & class(around) == 'Line') {
    i <- (abs(x - around$A$X) < 2 & abs(y - around$A$Y) < 10) |
         (abs(x - around$B$X) < 2 & abs(y - around$B$Y) < 10) |
         (x > around$A$X & x < around$B$X & y > around$A$Y & y < around$B$Y)
    x <- x[i]
    y <- y[i]
  }
  lines(x = x, y = y, ...)
}

#' Intersect and Line and Circle
#' 
#' Internal function for ray-tracing 2D optics
#' 
#' @param line Line to intersect (treated as an infinite line)
#' @param circle Circle to intersect
#' @return A list of 0, 1, or 2 Points of intersection between the line and circle
#' @family optics
#' @keywords internal
IntersectLC <- function(line, circle) {
  stopifnot(class(line) == 'Line')
  stopifnot(class(circle) == 'Circle')
  
  xA <- line$A$X - circle$O$X
  xB <- line$B$X - circle$O$X
  yA <- line$A$Y - circle$O$Y
  yB <- line$B$Y - circle$O$Y
  r <- circle$R
  
  dx <- xB - xA
  dy <- yB - yA
  dr <- sqrt(dx*dx + dy*dy)
  D <- xA * yB - xB * yA
  
  delta <- r * r * dr * dr - D * D
  
  if (delta < 0) {
    return(list())
  }
    
  sgn <- function(x) { if (x < 0) { return(-1) } else {return(1)}}
  
  xA <- (D * dy - sgn(dy) * dx * sqrt(delta)) / (dr * dr) + circle$O$X
  xB <- (D * dy + sgn(dy) * dx * sqrt(delta)) / (dr * dr) + circle$O$X
  
  yA <- (-D * dx - abs(dy) * sqrt(delta)) / (dr * dr) + circle$O$Y
  yB <- (-D * dx + abs(dy) * sqrt(delta)) / (dr * dr) + circle$O$Y
  
  if (delta == 0) {
    return(list(Point(xA, yA)))
  } else {
    A <- Point(xA, yA)
    B <- Point(xB, yB)
    if (xA < xB) {
      return(list(A, B))
    } else if (xA > xB) {
      return(list(B, A))
    } else {
      # xA == xB
      if (yA < yB) {
        return(list(A, B))
      } else {
        return(list(B, A))
      }
    }
  }
}

#' Refract a line given a normal direction and two index of refractions
#' 
#' Internal function for ray-tracing 2D optics.
#' 
#' The line argument provides the direction for the incident ray which is 
#' assumed to intersect with the normal at the normal's starting point. This is 
#' not checked and only the direction of the incident line matters (not its 
#' location or scale). The resulting refracted ray also starts at the normal's 
#' starting point.
#' 
#' @param line Infinite line to refract
#' @param normal Normal of the refracting surface (the surface is located at the
#'   normal's starting point)
#' @param n1 Index of refraction for the incident ray
#' @param n2 Index of refraction for the refracted ray
#' @param plot Whether to plot the incident ray, surface normal, and refracted 
#'   ray
#' @return The refracted ray with the starting point being the normal's starting
#'   point (class Line)
#' @family optics
#' @keywords internal
RefractLL <- function(line, normal, n1 = 1.0, n2 = 1.0, plot = TRUE, ...) {
  stopifnot(class(line) == 'Line')
  stopifnot(class(normal) == 'Line')
  stopifnot(is.numeric(n1) & is.finite(n1))
  stopifnot(is.numeric(n2) & is.finite(n2))

  angle1 <- angle(normal, line)
  angle2 <- asin(sin(angle1) * n1 / n2)
  if (! is.finite(angle2)) {
    warning("NaN produced")
    return(NA)
  }
  cat('Incident angle: ', angle1 * 180/pi, '\n')
  cat('Refracted angle: ', angle2 * 180/pi, '\n')
  
  normal <- normalize(center(normal))
  ray <- scale(normal, -1)
  ray <- rotate(ray, angle2)
  
  obj <- Line(line$B,
              Point(line$B$X + ray$B$X,
                    line$B$Y + ray$B$Y))
  if (plot) {
    plot(Line(line$B,
              Point(line$B$X + normalize(center(line))$B$X,
                    line$B$Y + normalize(center(line))$B$Y)), lty=2, ...)
    plot(Line(Point(line$B$X + normal$B$X/2, line$B$Y + normal$B$Y/2),
              Point(line$B$X - normal$B$X/2, line$B$Y - normal$B$Y/2)),
         lty=3, ...)
    plot(obj, ...)
  }
  return(obj)
}

#' Refract a line into a circle
#' 
#' Internal function for ray-tracing 2D optics.
#' 
#' Refracts a line into a circle by first computing the intersection of the line
#' and the circle and then computing the surface normal of the circle at that 
#' intersection point. The parameters first and inside choose whether the first
#' (left/top) or second (right/bottom) intersection of the line with the circle
#' is the chosen refraction surface. The inside parameter determines whether the
#' refraction is taken as a ray intersecting in-to or out-of the circle.
#' 
#' @param line Infinite line to refract
#' @param normal Normal of the refracting surface (the surface is located at the
#'   normal's starting point)
#' @param n1 Index of refraction for the incident ray
#' @param n2 Index of refraction for the refracted ray
#' @param first Whether to refract at the first intersection of the line and 
#'   circle or the second.
#' @param inside Whether the incident line is leaving the circle (starting 
#'   inside) and the refracting ray is outside of the circle
#' @param plot Whether to plot the incident ray, surface normal, and refracted 
#'   ray
#' @return The refracted ray with the starting point being the normal's starting
#'   point (class Line)
#' @family optics
#' @keywords internal
RefractLC <- function(line, circle, n1 = 1.0, n2 = 1.0, first = TRUE, 
                      inside = !first, plot = TRUE, ...) {
  stopifnot(class(line) == 'Line')
  stopifnot(class(circle) == 'Circle')
  stopifnot(is.numeric(n1) & is.finite(n1))
  stopifnot(is.numeric(n2) & is.finite(n2))
  pts <- IntersectLC(line, circle)
  if (length(pts) == 0) {
    return(NA)
  }
  B <- pts[[1]]
  scale = 1
  if (! first & length(pts) > 1) {
    B <- pts[[2]]
  }
  if (inside) {
    scale = -1
  }
  if (plot) {
    plot(Line(line$A, B), ...)
    plot(circle, around = B, ...)
  }
  return(RefractLL(Line(line$A, B),
                   scale(Line(circle$O, B), scale),
                   n1, n2, plot, ...))
}

test <- function() {
  n1 <- 1.5
  n2 <- 2
  n3 <- 3
  x <- 8
  y <- 7
  
  l1 <- Circle(Point(0,0),8)
  l2 <- Circle(Point(0,0),6)
  l3 <- Circle(Point(0,0),4)
  
  plot(0, xlim=c(-10,10), ylim=c(-10,10), type='n')
  #plot(0, xlim=c(0,5), ylim=c(-6,2), type='n')
  for (y in seq(0,y,by=0.5)) {
    a <- Line(Point(-10,y),Point(-5,y))
    a1 <- RefractLC(a, l1, n1 = 1.0, n2 = n1)
    if (class(a1) != 'Line') { a1 <- a }
    a2 <- RefractLC(a1, l2, n1 = n1, n2 = n2, col='red')
    if (class(a2) != 'Line') { a2 <- a1 }
    a3 <- RefractLC(a2, l3, n1 = n2, n2 = n3, col='green')
    if (class(a3) != 'Line') { a3 <- a2 }

    b3 <- RefractLC(a3, l3, n1 = n3, n2 = n2, inside = TRUE, first = (a3$B$X < a3$A$X), col='green')
    if (class(b3) != 'Line') { b3 <- a3 }
    b2 <- RefractLC(b3, l2, n1 = n2, n2 = n1, inside = TRUE, first = (b3$B$X < b3$A$X), col='blue')
    if (class(b2) != 'Line') { b2 <- b3 }
    b1 <- RefractLC(b2, l1, n1 = n1, n2 = 1.0, inside = TRUE, first = (b2$B$X < b2$A$X))
    if (class(b1) != 'Line') { b1 <- b2 }
    
    plot(scale(b1, 20), col='blue')
  }
}

eye <- function(y = 1, by = 0.5, angle = 0,
                AL = 24, 
                cornea1 = 7.7, tcornea = 0.5, ncornea = 1.376, cornea2 = 6.8,
                naqueous = 1.336, ACD = 3.6, 
                cortex1 = 10.0, tcortexA = 0.546, ncortex = 1.386,
                nucleus1 = 7.911, tnucleus = 2.419, nnucleus = 1.406, nucleus2 = -5.76,
                tcortexB = 0.635, cortex2 = -6,
                nvitreous = 1.336, retina = -12) {
  
  Cornea1 <- Circle(Point(x = cornea1, y = 0), radius = cornea1)
  Cornea2 <- Circle(Point(x = tcornea + cornea2, y = 0), radius = cornea2)
  Cortex1 <- Circle(Point(x = ACD + cortex1, y = 0), radius = cortex1)
  Nucleus1 <- Circle(Point(x = ACD + tcortexA + nucleus1, y = 0), radius = nucleus1)
  Nucleus2 <- Circle(Point(x = ACD + tcortexA + tnucleus + nucleus2, y = 0), radius = abs(nucleus2))
  Cortex2 <- Circle(Point(x = ACD + tcortexA + tnucleus + tcortexB + cortex2, y = 0), radius = abs(cortex2))
  Retina <- Circle(Point(x = AL + retina, y = 0), radius = abs(retina))
  
  par(mar = c(0,0,0,0))
  plot(0, xlim=c(-1,AL), ylim=c(-AL/4,AL/4), type='n', asp = 1,
       xaxt = 'n', xlab = '', yaxt = 'n', ylab = '')
  plot(Cornea1, around = Line(Point(tcornea/2,-10),Point(tcornea/2,10)))
  plot(Cornea2, around = Line(Point(tcornea/2,-10),Point(tcornea/2,10)))
  plot(Cortex1, around = Line(Point(ACD+tcortexA/2,-10),Point(ACD+tcortexA/2,10)))
  plot(Nucleus1, around = Line(Point(ACD+tcortexA/2,-10),Point(ACD+tcortexA/2,10)))
  plot(Nucleus2, around = Line(Point(ACD+tcortexA+tnucleus+tcortexB/2,-10),Point(ACD+tcortexA+tnucleus+tcortexB/2,10)))
  plot(Cortex2, around = Line(Point(ACD+tcortexA+tnucleus+tcortexB/2,-10),Point(ACD+tcortexA+tnucleus+tcortexB/2,10)))
  plot(Retina, around = Line(Point(AL-2,-10),Point(AL-2,10)))
  for (y in seq(-y, y, by = by)) {
    plot <- FALSE

    ray <- rotate(Line(Point(-1,y), Point(0,y)), angle * pi/180)
    rayCorneaA <- RefractLC(ray, Cornea1, n1 = 1.0, n2 = ncornea, plot = plot)
    rayCorneaB <- RefractLC(rayCorneaA, Cornea2, n1 = ncornea, n2 = naqueous, plot = plot)
    rayLensA <- RefractLC(rayCorneaB, Cortex1, n1 = naqueous, n2 = ncortex, plot = plot)
    rayLensB <- RefractLC(rayLensA, Nucleus1, n1 = ncortex, n2 = nnucleus, plot = plot)
    rayLensC <- RefractLC(rayLensB, Nucleus2, n1 = nnucleus, n2 = ncortex, inside = TRUE, first = FALSE, plot = plot)
    rayLensD <- RefractLC(rayLensC, Cortex2, n1 = ncortex, n2 = nvitreous, inside = TRUE, first = FALSE, plot = plot)
    rayRetina <- RefractLC(rayLensD, Retina, n1 = nvitreous, n2 = 100, inside=TRUE, first = FALSE, plot = plot)
    
    plot(Line(ray$A, rayCorneaA$A))
    plot(Line(rayCorneaA$A, rayCorneaB$A), col = 'blue')
    plot(Line(rayCorneaB$A, rayLensA$A))
    plot(Line(rayLensA$A, rayLensB$A), col = 'red')
    plot(Line(rayLensB$A, rayLensC$A), col = 'green')
    plot(Line(rayLensC$A, rayLensD$A), col = 'red')
    plot(Line(rayLensD$A, rayRetina$A))
    
    #plot(scale(rayLensD, AL))
  }  
}
