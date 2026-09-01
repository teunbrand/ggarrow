# Arrow ornament functions

There are two types of arrow ornament functions: functions for arrow
heads, and functions for arrow fins. The heads and fins can be used
interchangeably, but the name makes it clearer what is suitable.

## Usage

``` r
arrow_head_wings(offset = 20, inset = 30)

arrow_fins_feather(indent = 0.3, outdent = indent, height = 0.5)

arrow_head_line(angle = 30, lineend = "butt")

arrow_fins_line(angle = 30, lineend = "butt")

arrow_cup(lineend = "round", angle = NULL)

arrow_head_minimal(angle = 45)

arrow_fins_minimal(angle = 45)

arrow_head_halfwing(offset = 20, inset = 30, direction = 1)

arrow_head_halfline(angle = 30, lineend = "butt", direction = 1)
```

## Arguments

- offset, angle:

  A `numeric(1)` giving an angle in degrees for the angle between the
  line and tip.

- inset:

  A `numeric(1)` giving an angle in degrees for the angle inside the tip
  of the arrowhead.

- indent, outdent:

  A `numeric(1)` giving the fraction of the feather feather length to
  offset the notch and the end respectively.

- height:

  A `numeric(1)` ratio between the length of the feathers and the height
  of the feathers.

- lineend:

  A `character(1)`, one of `"butt"`, `"square"`, `"round"` or
  `"parallel"`. For `arrow_cup()`, only `"butt"` and `"round"` are
  allowed.

- direction:

  A `numeric(1)` whose sign determines the side of the shaft that the
  arrowhead is placed on.

## Value

A `<matrix[n, 2]>` with `x` and `y` columns describing a polygon. It has
a `notch_angle` attribute that is used fusing the fins/head to the shaft
of the arrow. They can be given to an arrow plotting function.

## Details

The convention for these functions is that the arrow shaft is fused to
the ornament at the (0,0) point and the ornaments ends at the (1,0)
point.

## Functions

- `arrow_head_wings()`: Places two triangles at either side of the line.
  Let ABC be a triangle, where A is at the end of the line, B is on the
  line and C is the arrow wingtip. Then `offset` is the angle at corner
  A and `inset` is the angle at corner C.

- `arrow_fins_feather()`: Places trapezoids at either side of the line.
  Let ABCD be a quadrilateral shape, where A is at the end of the line,
  B is on the line, and CD is parallel to AB, but offset from the line.
  Then, `indent` is the distance along the line between A and D and
  `outdent` is the distance along the line between B and C.

- `arrow_head_line()`: A line as an arrow head.

- `arrow_fins_line()`: A line as an arrow fin.

- `arrow_cup()`: A curved line some fixed distance away from the point
  to be resected, resembling a 'cup' shape.

- `arrow_head_minimal()`: This is a 'fake' arrow head who in practice
  doesn't draw anything, but sets the `notch_angle` attribute such that
  the arrow shaft is whittled into a triangular point.

- `arrow_fins_minimal()`: This is a 'fake' arrow head who in practise
  doesn't draw anything, but sets the `notch_angle` attribute such that
  a triangle is taken out of the arrow shaft.

- `arrow_head_halfwing()`: This a 'half' version of
  `arrow_head_wings()`.

- `arrow_head_halfline()`: This a 'half' version of `arrow_head_line()`.

## Examples

``` r
# Plotting winged head
plot(c(-0.5, 1), c(-0.6, 0.6), type = "n")
polygon(arrow_head_wings(), col = "gray")


# Plotting feather fins
plot(c(0, 1), c(-0.25, 0.25), type = "n")
polygon(arrow_fins_feather(), col = "gray")
```
