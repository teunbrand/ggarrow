# Run length encoding functions -------------------------------------------

# These are a bunch of functions related to run length encoded vectors. We use
# this to keep track of polygon grouping mostly. At the base is the
# <vctrs_group_rle> class. In contrast to base::rle, it doesn't store values,
# which we don't need for keeping track of groups.

# Constructor ---_---------------------------------------------------------

new_rle <- function(x = NULL, lengths = NULL, alt = NULL) {
  if (!is.null(lengths)) {
    new_rcrd(
      list(
        group  = seq_along(lengths),
        length = as.integer(lengths)
      ),
      n     = length(lengths),
      class = "vctrs_group_rle"
    )
  } else if (!is.null(x)) {
    vec_group_rle(x)
  } else {
    new_rcrd(
      list(
        group  = 1L,
        length = alt
      ),
      n     = 1L,
      class = "vctrs_group_rle"
    )
  }
}

# Basics ------------------------------------------------------------------

rle_end <- function(rle) {
  cumsum(field(rle, "length"))
}

rle_start <- function(rle) {
  c(1L, rle_end(rle)[-length(rle)] + 1L)
}

rle_inv <- function(rle) {
  rep.int(field(rle, "group"), field(rle, "length"))
}

rle_length <- function(rle) {
  sum(field(rle, "length"))
}

rle_valid <- function(rle) {
  !(is.na(field(rle, "group")) | field(rle, "length") < 1L)
}

# Specialised -------------------------------------------------------------

rle_rebase <- function(rle) {
  field(rle, "group") <- seq_along(field(rle, "group"))
  rle
}

rle_subset <- function(rle, i) {
  ends <- rle_end(rle)
  n <- ends[length(ends)]
  i <- vec_as_location(i, n)
  ival <- findInterval(i, ends, left.open = TRUE) + 1L
  field(rle, "length")  <- tabulate(ival, length(rle))
  rle
}

along_rle <- function(x, rle) {
  n <- rle_length(rle)
  if (length(x) == n) {
    return(x)
  }
  if (length(x) == length(rle)) {
    x <- rep.int(x, field(rle, "length"))
  } else {
    rep(x, length.out = n)
  }
}

# Splitting ---------------------------------------------------------------

rle_idx <- function(rle, rev = FALSE) {
  if (rev) {
    Map(`:`, rle_end(rle), rle_start(rle))
  } else {
    Map(`:`, rle_start(rle), rle_end(rle))
  }
}

rle_chop <- function(x, rle) {
  vec_chop(x, rle_idx(rle))
}

rle_unchop <- function(list, rle) {
  list_unchop(list, indices = rle_idx(rle))
}

split_rle <- function(x, rle) {
  lapply(rle_idx(rle), function(i) x[i])
}


