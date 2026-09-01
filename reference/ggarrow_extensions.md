# ggarrow extensions to ggplot2

ggarrow relieas on the extension mechanism of ggplot2 through
[ggproto](https://ggplot2.tidyverse.org/reference/ggproto.html) class
objects, that allow for cross-package inheritance of geoms. These
objects can be ignored by users for the purpose of making plots, since
interacting with these objects is preferred through various `geom_*()`
functions.
