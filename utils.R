path <- function(x) {
  if (grepl("docs", getwd(), fixed = TRUE)) {
    return(file.path("..", x))
  } else {
    return(x)
  }
}

# PaulScore Calculation
query_score <- function(positions, F) {
  if (length(positions) == 1 || all(is.na(positions))) {
    # no clicks were made
    return(0)
  } else {
    positions <- positions[!is.na(positions)] # when operating on 'events' dataset, SERP events won't have positions
    return(sum(F^positions))
  }
}

# Bootstrapping
bootstrap_mean <- function(x, m, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  n <- length(x)
  return(replicate(m, mean(x[sample.int(n, n, replace = TRUE)])))
}
