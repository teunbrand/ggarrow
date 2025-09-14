on_load({
  if (exists("element_geom", asNamespace("ggplot2"))) {
    element_arrow <- S7::new_class(
      name = "element_arrow",
      parent = element_line,
      properties = S7_element_arrow_properties,
      constructor = S7_element_arrow_constructor
    )
    S7::method(merge_element, list(element_arrow, S7::class_any)) <-
      S7_merge_method
    S7::method(element_grob, element_arrow) <-
      element_grob.element_arrow
  }
})

on_load({
  if (exists("element_geom", asNamespace("ggplot2"))) {
    new_defaults <- aes(
      colour        = from_theme(fill %||% ink),
      linewidth     = from_theme(linewidth * 2),
      linetype      = from_theme(linetype),
      stroke_colour = from_theme(colour %||% NA),
      stroke_width  = from_theme(borderwidth / 2)
    )
    update_geom_defaults(GeomArrow, new_defaults)
    update_geom_defaults(GeomArrowChain, aes(
      !!!new_defaults,
      size          = from_theme(pointsize)
    ))
    update_geom_defaults(GeomArrowCurve, new_defaults)
    update_geom_defaults(GeomArrowSegment, new_defaults)
  }
})

.onLoad <- function(lib, pkg) {
  run_on_load()
}
