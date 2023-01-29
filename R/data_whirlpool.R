#' Whirlpool data
#'
#' This function returns some made-up data to test arrow functionality with.
#'
#' @param n The number of streams in the whirlpool.
#' @param detail The number of points per stream.
#'
#' @return A `data.frame`
#' @export
#'
#' @examples
#' whirlpool()
whirlpool <- function(n = 5, detail = 100) {
  t <- seq(1, 0, length.out = detail) * pi

  seg <- 0.25 + seq_along(t) / length(t)

  offsets <- seq(0, 1, length.out = n + 1)[-(n + 1)] * 2 * pi

  t <- outer(t, offsets, FUN = `+`)

  out <- data_frame0(
    x = sin(as.vector(t)) * rep(seg, n),
    y = cos(as.vector(t)) * rep(seg, n),
    group = factor(rep(seq_len(n), each = detail))
  )
  id <- new_rle(out$group)
  out$arc <- arc_length(out$x, out$y, rle_start(id), length = field(id, "length"))
  out
}
