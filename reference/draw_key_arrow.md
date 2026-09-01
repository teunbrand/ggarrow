# Legend key glyph for arrows

Like any [legend key
glyphs](https://ggplot2.tidyverse.org/reference/draw_key.html), this key
can be used to display arrows in a legend.

## Usage

``` r
draw_key_arrow(data, params, size)
```

## Arguments

- data:

  A single row data frame containing the scaled aesthetics to display in
  this key

- params:

  A list of additional parameters supplied to the geom.

- size:

  Width and height of key in mm.

## Value

An `<arrow_path>` grob

## Examples

``` r
ggplot(mpg, aes(displ, colour = factor(cyl))) +
  geom_density(key_glyph = draw_key_arrow)
```
