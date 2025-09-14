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

.onLoad <- function(lib, pkg) {
  run_on_load()
}
