place_arrow <- function(
  arrow = NULL, x, y, id, size, angle, type = "head", force = FALSE
) {
  if (is.null(arrow)) {
    return(arrow)
  }
  # Recycle size
  size  <- rep_len(size, length(id))
  valid <- rle_valid(id)
  if (is.list(arrow)) {
    valid <- valid & lengths(arrow) > 0.0
  }

  # If there are invalid lines, try again without these
  if (!all(valid) && !force) {
    if (!any(valid)) {
      return(NULL)
    }
    keep  <- rep(valid, field(id, "length"))
    if (is.list(arrow)) {
      arrow <- arrow[valid]
    }
    retry <- place_arrow(
      arrow = arrow, x = x[keep], y = y[keep],
      id = id[valid], size = size[valid], angle = angle[valid],
      type = type, force = force
    )
    ans <- vector("list", length(id))
    ans[valid] <- retry
    return(ans)
  }

  # Get ends of paths
  if (type == "head") {
    i <- rle_end(id)[seq_along(angle)]
  } else {
    i <- rle_start(id)[seq_along(angle)]
  }

  arrow <- rotate_scale(arrow, angle)
  arrow <- scale_translate(arrow, x[i], y[i], size)
  polygonise(arrow)
}

resolve_ornament <- function(
  ornament, length, id, width, resect, type = "head"
) {

  if (is.null(ornament)) {
    ans <- list(ornament = NULL, length = rep(0.0, length(id)),
                resect = rep(0.0, length(id)), angle = NA, scale = NA)
    return(ans)
  }
  if (is.list(ornament)) {
    ornament <- Map(
      resolve_ornament,
      ornament = ornament,
      length = split(length, seq_along(length)),
      id = id,
      width = rle_chop(width, id),
      resect = resect,
      MoreArgs = list(type = type)
    )
    ornament <- list(
      ornament = lapply(ornament, .subset2, "ornament"),
      length   = vapply(ornament, .subset2, numeric(1L), "length"),
      scale    = vapply(ornament, .subset2, numeric(1L), "scale"),
      resect   = vapply(ornament, .subset2, numeric(1L), "resect"),
      angle    = vapply(ornament, .subset2, numeric(1L), "angle")
    )
    ornament$ornament <- lapply(ornament$ornament, function(x) {
      if (is.list(x) && length(x) == 1L) {
        return(.subset2(x, 1L))
      }
      x
    })
    return(ornament)
  }

  if (type == "head") {
    i <- rle_end(id)
  } else {
    i <- rle_start(id)
  }
  if (is.function(ornament)) {
    ornament <- Map(
      ornament,
      length = as_mm(length), width = width[i], resect = resect
    )
    attrnames <- names(attributes(ornament[[1L]]))
    if ("resect" %in% attrnames) {
      resect <- vapply(ornament, attr, numeric(1L), "resect")
    } else {
      resect <- rep(0.0, length(ornament))
    }
    if ("notch_angle" %in% attrnames) {
      angle <- vapply(ornament, attr, numeric(1L), "notch_angle")
    } else {
      angle <- rep(.halfpi, length(ornament))
    }
    scale  <- rep(1.0, length(id))
  } else {
    length <- pmax(as_mm(length), width[i] / diff(range(ornament[, "y"])))
    length <- (attr(ornament, "length") %||% max(ornament[, "x"])) * length
    resect <- (attr(ornament, "resect") %||% 1.0) * length
    angle  <- attr(ornament, "notch_angle") %||% .halfpi
    scale  <- length
  }
  list(ornament = ornament, length = resect, scale = scale,
       resect = resect, angle = angle)
}

resolve_inner <- function(
  ornament, length,
  x, y, id, width,
  placement
) {
  if (is.null(ornament)) {
    return(ornament)
  }
  if (is.list(ornament)) {
    ornament <- Map(
      resolve_inner,
      ornament = ornament,
      length = split(rep_len(length, length(ornament)), seq_along(ornament)),
      x = rle_chop(x, id),
      y = rle_chop(y, id),
      id = id,
      width = rle_chop(width, id),
      MoreArgs = list(placement = placement)
    )
    i <- lengths(ornament) > 0.0
    ornament[i] <- unlist(ornament[i], recursive = FALSE)
    return(ornament)
  }

  start     <- rle_start(id)
  end       <- rle_end(id)
  id_length <- field(id, "length")

  arc_length <- arc_length(x, y, start, id_length)
  n_places <- length(placement)

  if (!is.unit(placement)) {
    placement <- placement[order(abs(placement))]
    sign <- sign(placement)
    placement <- abs(placement)

    norm <- arc_length / arc_length[rep.int(end, id_length)]
    n_arrow <- rep(n_places, length(id))
    place_idx <- lapply(
      rle_chop(norm, id), findInterval,
      x = placement, all.inside = TRUE
    )
    place_lens <- rep.int(seq_along(id), lengths(place_idx))
    place_idx <- unlist0(place_idx) + start[place_lens] - 1L

    # Interpolate
    placement <- rep(placement, length(id))
    d     <- interpol_dist(placement, place_idx, norm)
    width <- linear_interpol(width, place_idx, d)
    arc   <- linear_interpol(arc_length, place_idx, d)

    if (is.function(ornament)) {
      ornament <- Map(ornament, length = as_mm(length), width = width)
      length   <- vapply(ornament, attr, numeric(1L), "resect")
      ornament <- lapply(ornament, function(o) {
        o[, "x"] <- o[, "x"] - 0.5 * (attr(o[, "x"], "resect") %||% 1.0)
        o
      })
      scale <- 1.0
    } else {
      if (!is.unit(length)) {
        length <- length * width
      }
      length <- pmax(as_mm(length), width / diff(range(ornament[, "y"])))
      ornament[, "x"] <- ornament[, "x"] - 0.5
      scale <- length
    }

    pos <- outer(length, c(-0.5, 0.0, 0.5))
    pos[] <- pos * rep(width, 3L)
    pos[] <- pos + rep(arc, 3L)

    index <- rep(seq_along(id), each = n_places)
    index <- split(seq_len(nrow(pos)), index)
    split_arc <- rle_chop(arc_length, id)
    index <- lapply(seq_along(id), function(i) {
      idx <- index[[i]]
      x <- findInterval(pos[index[[i]], ], split_arc[[i]], all.inside = TRUE)
      `dim<-`(x, c(length(idx), 3L)) + start[i] - 1L
    })
  } else {
    sign <- sign(as.numeric(placement))
    placement <- abs(placement)

    temp_width <- (width[start] + width[end]) / 2.0
    if (!is.unit(length)) {
      length <- length * temp_width
    }
    if (is.function(ornament)) {
      ornament <- Map(ornament, length = length, width = temp_width)
      length   <- vapply(ornament, attr, numeric(1L), "resect")
      ornament <- lapply(ornament, function(o) {
        o[, "x"] <- o[, "x"] - 0.5 * (attr(o[, "x"], "resect") %||% 1.0)
        o
      })
      scale <- 1.0
    } else {
      thickness <- diff(range(ornament[, "y"]))
      length <- pmax(as_mm(length), temp_width / thickness)
      scale <- length
    }

    placement  <- as_mm(placement)
    sum_length <- length + placement
    arc_end    <- arc_length[end]
    n_arrow    <- arc_end %/% sum_length
    remain     <- arc_end - sum_length * (n_arrow - 1.0) + length

    split_arc <- rle_chop(arc_length, id)
    pos <- lapply(seq_along(id), function(i) {
      pos <- sum_length[i] * 0L:(n_arrow[i] - 1L) + 0.5 * remain[i]
      unname(cbind(pos - 0.5 * length[i], pos, pos + 0.5 * length[i]))
    })

    index <- lapply(seq_along(id), function(i) {
      idx <- findInterval(pos[[i]], split_arc[[i]], all.inside = TRUE)
      `dim<-`(idx + start[i] - 1L, dim(pos[[1L]]))
    })
    pos <- do.call(rbind, pos)
  }

  index <- do.call(rbind, index)
  d     <- interpol_dist(pos, index, arc_length)
  new_x <- linear_interpol(x, index, d)
  new_y <- linear_interpol(y, index, d)

  dx <- new_x[, 3L] - new_x[, 1L]
  dy <- new_y[, 3L] - new_y[, 1L]
  angle <- atan2(dy, dx)
  if (!is.null(sign)) {
    i <- sign == -1.0
    angle[i] <- norm_angle(angle[i] + pi)
  }

  arrow <- rotate_scale(ornament, angle)
  arrow <- scale_translate(arrow, new_x[, 2L], new_y[, 2L], scale)

  arrow <- polygonise(arrow)
  split(arrow, rep(seq_along(id), n_arrow))
}
