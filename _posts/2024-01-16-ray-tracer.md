---
title: "How ray tracing works"
date: 16 January 2024
layout: post
categories: 
- Emacs
tags: 
- emacs 
- math
---

**[This org file](https://github.com/dalanicolai/dalanicolai.github.io/blob/main/_posts/org/ray-tracer.org) contains a translation from the ulisp article at <http://www.ulisp.com/show?2NWA> to elisp**

**Additionally this file uses ppm-gen-simple.el from <https://github.com/dalanicolai/dala-emacs-lisp> for drawing**

**After using `org-babel-execute-buffer` on this org file, a link to the resulting image is inserted in this buffer, below the `(tracer)` call**

**For fun, you could tangle this buffer, byte/native compile the resulting lisp file and run `(tracer)` again to experience the increase in speed**

Ray tracing creates a two-dimensional image of a simulated world consisting of
one or more three-dimensional objects. It works by considering an image plane,
represented by a vertical rectangle in the following diagram:

![world](/assets/world.gif)

For each pixel in the image plane, defined by its x and y coordinates, we trace
the path of a ray from an imaginary eye, through the pixel, to the objects in
the world, here represented by two spheres. The first object hit by the ray
determines the colour of the pixel.

In these routines the ray is defined by a starting point pt, usually the eye
position, and a unit vector pr specifying the direction of the ray. When the ray
hits an object, the hit point is defined by a distance d, which is the distance
along the ray from the initial point pt to the hit point. The coordinates of the
hit point h can easily be calculated from d, pt, and pr:

h = pt + d x pr

When the ray hits an object in the scene the colour of the object determines the
colour of the pixel. A direct hit, normal to the object's surface, produces a
bright colour. A glancing hit produces a darker colour, based on its angle to
the surface. If the ray doesn't hit any objects the pixel's colour will be
determined by the background.

The objects in the scene are rendered as if they are illuminated by a light at
the eye position. This world also includes a second light not shown in the above
diagram; it's a point light source vertically above the scene, so that the
objects will cast shadows.


## Utilities

{% highlight emacs-lisp %}
;; This el file contains a translation from the ulisp article at http://www.ulisp.com/show?2NWA to elsip
;; Additionally this file uses ppm-gen.el from https://www.emacswiki.org/emacs/PpmGen for plotting

;; (require 'ppm-gen)
(require 'cl-lib)
{% endhighlight %}

To keep the program as concise as possible points, vectors, and colours are each
represented by a list of three elements. The following routines can be used to
construct an appropriate vector from its components:

{% highlight emacs-lisp %}
(defun colour (r g b) (list r g b))
(defun pnt (x y z) (list x y z))
(defun vect (x y z) (list x y z))
{% endhighlight %}

Having three separate routines makes the programs more readable by making it
clear what type of value we're creating.

Representing vectors in this way allows us to implement vector addition, add,
and subtraction, subtract, using mapping functions:

{% highlight emacs-lisp %}
(defun add (v w) (cl-mapcar #'+ v w))
(defun subtract (v w) (cl-mapcar #'- v w))
{% endhighlight %}

Likewise, the dot or scalar product of two vectors, dot, is defined as:

{% highlight emacs-lisp %}
(defun mul (c v) (mapcar (lambda (x) (* c x)) v))
(defun dot (v w) (apply #'+ (cl-mapcar #'* v w)))
{% endhighlight %}

and the magnitude of a vector is given by mag, where:

{% highlight emacs-lisp %}
(defun square (x) (* x x))
(defun magnitude (v) (sqrt (apply #'+ (mapcar #'square v))))
{% endhighlight %}

The unit vector is:

{% highlight emacs-lisp %}
(defun unit-vector (v)
  (let ((d (magnitude v)))
    (mapcar (lambda (j) (/ j d)) v)))
{% endhighlight %}

and the distance between two points is:

{% highlight emacs-lisp %}
(defun distance (p1 p2)
  (magnitude (cl-mapcar #'- p1 p2)))
{% endhighlight %}

Using mapping functions avoids the need to extract the x, y, and z components of
each point or vector, or the r, g, and b components of colours.

Defining the world

The easiest objects to ray trace are spheres, because it's relatively easy to
calculate where the ray intersects with a sphere. I've also included an infinite
plane object, that gives the other objects in the world something to stand on.

The world, eye, and light are stored in global variables:

{% highlight emacs-lisp %}
(defvar *world* nil)
(defvar *eye* nil)
(defvar *light* nil)
{% endhighlight %}

**world** stores a list of the objects in the world. **eye** and **light** give the
coordinates of the eye position and point light source.

Objects

I've implemented a very simple object system to deal with the two types of
object in the world, spheres and planes. This will make it easy to extend the
ray tracer to cope with other objects.

Each object consists of a list starting with the object name, and followed by
the object's parameters.

Sphere

A sphere consists of:

('sphere centre radius colour)

where centre and colour are each lists of three items. For example, this is a
red sphere at (200, -150, -800) with a radius of 50:

('sphere (point 200 -150 -800) 50 (colour 1 0 0))

The following functions provide convenient ways of accessing the sphere
parameters:

{% highlight emacs-lisp %}
(defun sphere-center (s) (cl-second s))
(defun sphere-radius (s) (cl-third s))
(defun sphere-colour (s) (cl-fourth s))
{% endhighlight %}

Plane

A plane consists of:

('plane point normal colour)

where point is any point on the plane, and normal is a unit vector giving the
direction normal to the plane. Each parameter is a list of three items. For
example, this is a white plane:

('plane (point 0 -200 0) (vect 0 -1 0) (colour 2 2 2))

Here are the functions to access the plane parameters:

{% highlight emacs-lisp %}
(defun plane-point (s)  (cl-second s))
(defun plane-normal (s) (cl-third s))
(defun plane-colour (s) (cl-fourth s))
{% endhighlight %}

Adding an object to the world

The function make adds an object definition to the world:

{% highlight emacs-lisp %}
(defun make (&rest list)
  (push list *world*))
{% endhighlight %}

Here are the function calls to define the world:

{% highlight emacs-lisp %}
(setq magnification 1)

(setq *world* nil)
(setq *eye* (mul magnification (pnt 0.0 0.0 200.0)))
(setq *light* (mul magnification (pnt -5000 10000 -10000)))
(make 'plane (mul magnification (pnt 0 -200 0)) (vect 0 -1 0) (colour 2 2 2))
(make 'sphere (mul magnification (pnt -250 0 -1000)) (* magnification 200) (colour 0 1 .5))
(make 'sphere (mul magnification (pnt 50 0 -1200)) (* magnification 200) (colour 1 .5 0))
(make 'sphere (mul magnification (pnt 400 0 -1400)) (* magnification 200) (colour 0 .5 1))
(make 'sphere (mul magnification (pnt -50 -150 -600)) (* magnification 50) (colour 0 0 1))
(make 'sphere (mul magnification (pnt 200 -150 -800)) (* magnification 50) (colour 1 0 0))
(make 'sphere (mul magnification (pnt -50 220 -600)) (* magnification 50) (colour 1 1 0))
{% endhighlight %}

The eye is on the z axis, 200 units from the origin. The objects in the world
are all on the other side of the image plane, so they have negative z
coordinates. The light is vertically above the objects, and on their left.

Object methods

Three object method functions are defined for the objects: object-colour,
object-normal, and object-hit:

The object-colour method gets the colour of the object:

{% highlight emacs-lisp %}
(defun object-colour (s)
  (cl-case (car s)
    (sphere (sphere-colour s))
    (plane (plane-colour s))))
{% endhighlight %}

The object-normal method gives the normal to the object s at the point pt:

{% highlight emacs-lisp %}
  (defun object-normal (s pt)
    (cl-case (car s)
      (sphere (sphere-normal s pt))
      (plane (plane-normal s))))

(defun sphere-normal (s pt)
  (unit-vector (subtract (sphere-center s) pt)))
{% endhighlight %}

The object-hit method calculates where on the object's the ray defined by pt and
pr hits, or it returns nil if it misses:

{% highlight emacs-lisp %}
(defun object-hit (s pt pr)
  (cl-case (car s)
    (sphere (sphere-hit s pt pr))
    (plane (plane-hit s pt pr))))

(defun sphere-hit (s pt pr)
  (let* ((c (sphere-center s))
         (oc (cl-mapcar #'- pt c)))
    (minroot
     (apply #'+ (mapcar #'square pr))
     (* 2 (dot oc pr))
     (- (dot oc oc) (square (sphere-radius s))))))

(defun plane-hit (s pt pr)
  (let ((denom (dot (plane-normal s) pr)))
    (unless (zerop denom)
      (let ((n (/ (dot (subtract (plane-point s) pt) (plane-normal s)) denom)))
        (when (>= n 0) n)))))
{% endhighlight %}

Finding the hit point on a sphere involves solving the equation resulting from
the intersection of the ray and the sphere. This gives a quadratic equation of
the form:

ax2 + bx + c

which can have 0, 1, or 2 solutions. No solutions corresponds to the ray missing
the sphere; one solution corresponds to it grazing the surface at one point; two
solutions corresponds to it entering the sphere on one side and exiting on the
other, in which case we're only interested in the minimum root. The sphere-hit
routine calls minroot to calculate this:

{% highlight emacs-lisp %}
(defun minroot (a b c)
  (if (zerop a)
      (/ (- c) b)
    (let ((disc (- (square b) (* 4 a c))))
      (unless (cl-minusp disc)
        (min (/ (+ (- b) (sqrt disc)) (* 2 a))
             (/ (- (- b) (sqrt disc)) (* 2 a)))))))
{% endhighlight %}

Generating the ray-traced image

The ray-traced image has a resolution of 160 x 128 pixels. To generate this we
call tracer:

{% highlight emacs-lisp %}
(setq *xres* (* magnification 320))
(setq *yres* (* magnification 256))
(setq img (ppm-create *xres* *yres*))

(defun tracer ()
  (dotimes (x *xres*)
    (dotimes (y *yres*)
      ;; (print (apply #'color-rgb-to-hex (colour-at (- x 80) (- 64 y)))))))
      (ppm-plot img x y (colour-at (- x 80) (- 64 y)))))
    (ppm-display img))
{% endhighlight %}

This calls plotpoint to plot the pixel on the display device. For each pixel it
calls colour-at to get the colour of the pixel:

{% highlight emacs-lisp %}
(defun colour-at (x y)
  (let ((c (send-ray
            *eye*
            (unit-vector
             (subtract (list x y 0) *eye*)))))
    (or c (background x y))))
{% endhighlight %}

This calls send-ray to send a ray from the eye in the direction defined by the
unit vector from the eye to the pixel. It returns the colour returned by
send-ray, or the background colour if send-ray doesn't hit anything and returns
nil.

Background

The background colour is a solid light blue, representing the sky. It's
generated by this function:

{% highlight emacs-lisp %}
(defun background (x y) (colour 0.5 0.7 1))
{% endhighlight %}

Sending a ray

The function send-ray sends a ray and returns the colour where the ray hits the
first object, or nil if it doesn't hit anything. Here's a simple version of
send-ray that ignores the light:

{% highlight emacs-lisp %}
(defun send-ray (pt pr)
  (let* ((f (first-hit pt pr))
         (s (cl-first f))
         (hit (cl-second f)))
    (when s (mul (lambert s hit pr) (object-colour s)))))
{% endhighlight %}

It then returns the object's colour multiplied by the Lambert factor. Lambert's
law says that the intensity of light reflected by a point on a surface is
proportional to the dot product of the unit normal vector at that point and the
unit vector from the point to the light source:

{% highlight emacs-lisp %}
(defun lambert (s hit pr)
  (max 0 (dot pr (object-normal s hit))))
{% endhighlight %}

If the light is shining perpendicular to the surface the dot product will be 1,
the maximum value, and the surface will be bright. If the light is hitting the
surface at an angle of 90° the dot product will be zero, and the surface will be
dark.

The routine send-ray calls first-hit, which goes through each of the objects in
the world finding the object with the closest hit point. It returns a list of
two items: the closest object hit, and the coordinates of the hit point:

{% highlight emacs-lisp %}
(defun first-hit (pt pr)
  (let (surface hit dist)
    (dolist (s *world*)
      (let ((n (object-hit s pt pr)))
        (when n
          (let* ((h (add pt (mul n pr)))
                 (d (distance h pt)))
            (when (or (null dist) (< d dist))
              (setq surface s)
              (setq hit h)
              (setq dist d))))))
    (list surface hit)))
{% endhighlight %}

Casting shadows

To give shadows in the scene we can extend send-ray to take account of the point
light source:

{% highlight emacs-lisp %}
(defun send-ray (pt pr)
  (let* ((f (first-hit pt pr))
         (s (cl-first f))
         (hit (cl-second f)))
    (when s
      (let* ((c (mul (lambert s hit pr) (object-colour s)))
             (f2 (first-hit *light* (unit-vector (subtract hit *light*))))
             (h2 (cl-second f2)))
        (cond
         ((< (distance hit h2) 1) c)
         (t (mul .75 c)))))))
{% endhighlight %}

{% highlight emacs-lisp %}
(tracer)
{% endhighlight %}

The result is shown below (unfortunately, github pages, with remote
theme, does not correctly show ppm's so that the below image is a
png)

![scene](/assets/scene.png)

When the ray hits the surface of an object we trace the path of a second ray
from the light to the hit point. If it hits a point close to the original hit
point we leave the colour unchanged. Otherwise we reduce the brightness by a
factor of 0.75 to represent the fact that another object is casting a shadow
from the light.

Further suggestions

Here are some suggestions for extending this program:

Anti-aliasing

You can improve the quality of the rendered image by using anti-aliasing; for
each point in the final image ray-trace four points separated by half a pixel in
each direction, and then average them together. This smooths the jagged edges of
the objects at the expense of taking proportionally longer to render.

Adding other primitive objects

To add support for other primitive objects, such as cylinders, cones, toruses,
polygons, or discs, you need to define object-colour, object-normal, and
object-hit methods for the objects. For details of the mathematics see the
References below.

Adding other surfaces

The ray tracer could be extended by supporting other object surfaces, such as
reflective metal.

References

This ray tracer is developed from an example in Paul Graham's book "ANSI Common
Lisp" [2]. A useful explanation of ray tracing is "Ray Tracing in One Weekend"
by Peter Shirley [3]. For information about adding other primitive objects to
the ray tracer, such as a cylinder, cone, torus, polygon, or disc, see [4].

Update

1st August 2019: Running on Common Lisp

This program will also run on any standard Common Lisp implementation with one
modification; you need to prefix the function arguments to apply and mapcar with
the function macro expression, #'. This isn't necessary in uLisp because
function names and variables share the same namespace. For example:

(defun dot (v w) (apply #'+ (mapcar #'\* v w)))

You will also need to replace the definition of plotpoint with a routine to plot
to the computer's graphics display rather than an external TFT display.

21st February 2020: Added information about running the program on an Adafruit CLUE.

---

1 ^ Adafruit CLUE - nRF52480 Express on Adafruit.
2 ^ Graham, Paul "ANSI Common Lisp"  Prentice-Hall, New Jersey, 1996, pp. 151-158.
3 ^ Ray Tracing in One Weekend on Real-Time Rendering.
4 ^ Ray tracing primitives on University of Cambridge Computer Laboratory website.
