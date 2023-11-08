% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_arrow.R
\name{geom_arrow}
\alias{geom_arrow}
\title{Arrows}
\usage{
geom_arrow(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  arrow_head = arrow_head_wings(),
  arrow_fins = NULL,
  arrow_mid = NULL,
  length = 4,
  length_head = NULL,
  length_fins = NULL,
  length_mid = NULL,
  justify = 0,
  force_arrow = FALSE,
  mid_place = 0.5,
  resect = 0,
  resect_head = NULL,
  resect_fins = NULL,
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}}. If specified and
\code{inherit.aes = TRUE} (the default), it is combined with the default mapping
at the top level of the plot. You must supply \code{mapping} if there is no plot
mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{stat}{The statistical transformation to use on the data for this
layer, either as a \code{ggproto} \code{Geom} subclass or as a string naming the
stat stripped of the \code{stat_} prefix (e.g. \code{"count"} rather than
\code{"stat_count"})}

\item{position}{Position adjustment, either as a string naming the adjustment
(e.g. \code{"jitter"} to use \code{position_jitter}), or the result of a call to a
position adjustment function. Use the latter if you need to change the
settings of the adjustment.}

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}. These are
often aesthetics, used to set an aesthetic to a fixed value, like
\code{colour = "red"} or \code{size = 3}. They may also be parameters
to the paired geom/stat.}

\item{arrow_head, arrow_fins, arrow_mid}{A function call to one of the
\link[=arrow_ornaments]{arrow ornament} functions that can determine the shape
of the arrow head, fins or middle (interior) arrows.}

\item{length, length_head, length_fins, length_mid}{Determines the size of
the arrow ornaments. \code{length} sets the default length, whereas
\code{length_head}, \code{length_fins} and \code{length_mid} set the lengths of the
arrow head, arrow fins or middle arrows respectively. Can be one of the
following:
\itemize{
\item A \verb{<numeric>} to set the ornament size relative to the \verb{linewidth\{_\\*\}}
settings.
\item A \code{\link[grid:unit]{<unit>}} to control the ornament size in an absolute
manner. Behaviour of relative units such as \code{"npc"} or \code{"null"} is
undefined.
}}

\item{justify}{A \code{numeric(1)} between [0-1] to control where the
arrows should be drawn relative to the path's endpoints. A value of \code{0}
sets the arrow's tips at the path's end, whereas a value of \code{1} sets the
arrow's base at the path's end.}

\item{force_arrow}{A \code{logical(1)} which, if \code{TRUE} an arrow will be drawn
even when the length of the arrow is shorter than the arrow heads and fins.
If \code{FALSE}, will drop such arrows.}

\item{mid_place}{Sets the location of middle (interior) arrows, when
applicable. Can be one of the following:
\describe{
\item{A \code{numeric} vector}{with values between [0-1] to set middle
arrows at relative positions along the arc-length of a path.}
\item{A \code{<}\code{\link[grid:unit]{unit}}\code{>}}{to fill a path with arrows with
the provided unit as distance between one arrow to the next.}
}}

\item{resect, resect_head, resect_fins}{A \code{numeric(1)} denoting millimetres or
\code{<}\code{\link[grid:unit]{unit}}\code{>} to shorten the arrow. \code{resect_head} shortens
the arrow from the arrow head side, whereas \code{resect_fins} shortens the
arrow from the fins side. Both inherit from \code{resect}.}

\item{lineend}{Line end style (round, butt, square).}

\item{linejoin}{Line join style (round, mitre, bevel).}

\item{linemitre}{Line mitre limit (number greater than 1).}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}
}
\value{
A \verb{<Layer>} ggproto object that can be added to a plot.
}
\description{
This arrow geom can be used to draw lines adorned with arrow heads or fins.
It is useful as an annotation layer to point to or away from other things
on the plot. An arrow typically consists of three parts: the arrowhead, the
shaft and fins. This geom places arrow heads at the end of a line and fins
at the beginning of a line.
}
\section{Aesthetics}{

\code{geom_arrow()} understands the following aesthetics (required aesthetics are in bold):
\itemize{
\item \strong{\code{x}}
\item \strong{\code{y}}
\item \code{alpha}
\item \code{arrow_fins}
\item \code{arrow_head}
\item \code{arrow_mid}
\item \code{colour}
\item \code{group}
\item \code{linewidth}
\item \code{stroke_colour}
\item \code{stroke_width}
}
Learn more about setting these aesthetics in \code{vignette("ggplot2-specs")}.
}

\examples{
# Setting up a plot
p <- ggplot(whirlpool(), aes(x, y, colour = group)) +
  coord_equal()

# A standard arrow
p + geom_arrow()

# Arrows can have varying linewidths
p + geom_arrow(aes(linewidth = arc))

# You can use `length_head` to decouple arrow-head size from linewidth
p + geom_arrow(aes(linewidth = arc), length_head = unit(10, "mm"))

# The arrow head shape can be controlled with the `arrow_head` argument
p + geom_arrow(arrow_head = arrow_head_line(), length_head = unit(10, "mm"))

# This works similarly for the arrow fins
p + geom_arrow(
  arrow_fins = arrow_fins_feather(),
  length_fins = unit(7, "mm")
)
}
\seealso{
Other arrow geoms: 
\code{\link{annotate_arrow}()},
\code{\link{geom_arrow_chain}()},
\code{\link{geom_arrow_curve}()},
\code{\link{geom_arrow_segment}()}
}
\concept{arrow geoms}