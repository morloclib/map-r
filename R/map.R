# WARNING: The Map implementation in R is limited. R doesn't have a general
# map type. The named `list` type works properly only for string keys.

# --- Pack/Unpack ---

morloc_unpack_map <- function(x){
  list(names(x), unname(x))
}

morloc_pack_map <- function(xs){
  names(xs[[2]]) <- xs[[1]]
  as.list(xs[[2]])
}

# --- Construction ---

morloc_emptyMap <- function() {
  structure(list(), names = character(0))
}

morloc_singleton <- function(k, v) {
  result <- list(v)
  names(result) <- k
  result
}

morloc_from_list <- function(xs) {
  result <- list()
  for (pair in xs) {
    result[[pair[[1]]]] <- pair[[2]]
  }
  result
}

morloc_to_list <- function(m) {
  lapply(names(m), function(k) list(k, m[[k]]))
}

# --- Query ---

morloc_lookup <- function(key, m) {
  m[[key]]
}

morloc_member <- function(key, m) {
  key %in% names(m)
}

morloc_size <- function(m) {
  length(m)
}

# --- Modify ---

morloc_insert <- function(key, val, m) {
  result <- m
  result[[key]] <- val
  result
}

morloc_delete <- function(key, m) {
  result <- m
  result[[key]] <- NULL
  result
}

# --- Bulk access ---

morloc_keys <- function(m) {
  as.list(names(m))
}

morloc_vals <- function(m) {
  unname(as.list(m))
}

# --- Combine ---

morloc_union <- function(a, b) {
  result <- a
  for (k in names(b)) {
    result[[k]] <- b[[k]]
  }
  result
}

morloc_unionWith <- function(f, a, b) {
  result <- a
  for (k in names(b)) {
    if (k %in% names(result)) {
      result[[k]] <- f(result[[k]], b[[k]])
    } else {
      result[[k]] <- b[[k]]
    }
  }
  result
}

morloc_intersectionWith <- function(f, a, b) {
  result <- list()
  for (k in names(a)) {
    if (k %in% names(b)) {
      result[[k]] <- f(a[[k]], b[[k]])
    }
  }
  result
}

morloc_difference <- function(a, b) {
  result <- list()
  for (k in names(a)) {
    if (!(k %in% names(b))) {
      result[[k]] <- a[[k]]
    }
  }
  result
}

# --- Transform ---

morloc_map_val <- function(f, m) {
  result <- list()
  for (k in names(m)) {
    result[[k]] <- f(m[[k]])
  }
  result
}

morloc_map_key <- function(f, m) {
  result <- list()
  for (k in names(m)) {
    result[[f(k)]] <- m[[k]]
  }
  result
}

morloc_mapWithKey <- function(f, m) {
  result <- list()
  for (k in names(m)) {
    result[[k]] <- f(k, m[[k]])
  }
  result
}

morloc_filter_map <- function(f, m) {
  result <- list()
  for (k in names(m)) {
    if (f(k, m[[k]])) {
      result[[k]] <- m[[k]]
    }
  }
  result
}

morloc_foldWithKey <- function(f, init, m) {
  acc <- init
  for (k in names(m)) {
    acc <- f(acc, k, m[[k]])
  }
  acc
}
