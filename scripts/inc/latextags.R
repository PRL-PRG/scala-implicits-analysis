## Overview

overview <- function(...) {
  bind_rows(...)
}

r <- function(name, value, ...) {
  stopifnot(is.character(name))
  stopifnot(length(name) == 1)
  
  UseMethod("r", value)  
}

r.numeric <- function(name, value, ...) {
  tag(str_c(name, " rnd"), fmt(oom(value), ...))
  
  NextMethod("r", value)
}

r.default <- function(name, value, ...) {
  stopifnot(length(value) == 1)
  
  tibble(name=name, value=tag(name, fmt(value, ...)))
}

r.data.frame <- function(name, value, ...) {
  r(name, nrow(value))
}


## Tags

#' Create a new tags container
#'
#' It creates a new tags container that will be associated with the given
#' `filename` and the tags will be prefixed with `prefix` string.
#'
#' @param filename the name of the file where the tags will be stored.
#' @param prefix the string to prepend to all tag names.
#' @param default if `TRUE` make this the default tags container.
#'
#' @export
create_tags <- function(filename, prefix="", default=TRUE) {
  tags <- new.env(parent=emptyenv())
  tags$filename <- filename
  tags$prefix <- prefix
  clear_tags(tags)
  
  if (default) {
    set_default_tags(tags)
  }
  
  writeLines("", filename)
  
  class(tags) <- "latextags"
  
  tags
}

clear_tags <- function(tags=get_default_tags()) {
  tags$values <- tibble::tibble(name=character(), value=character(), latex=character())
}

tags_filename <- function(tags) {
  tags$filename
}

tags_prefix <- function(tags) {
  tags$prefix
}

tags <- function(tags) {
  tags$values
}

tags_latex <- function(tags) {
  tags(tags)$latex
}

#' Create a new tag
#'
#' Creates a new tag named `name` with value `value` in the `tags` tags
#' container.
#'
#' @param name the tag name.
#' @param value the tag value.
#' @param tags the tag container, using the default container by default.
#'
#' @export
tag <- function(name, value, tags=get_default_tags()) {
  if (is.null(tags)) {
    return(value)
  }
  
  stopifnot(is.character(name))
  stopifnot(length(name)==1)
  
  value <- as.character(value)
  latex <- generate_latex_command(name, value, prefix=tags_prefix(tags))
  
  existing <- which(tags$values$name == name)
  
  if (length(existing) > 0) {
    tags$values[existing, "value"] <- value
    tags$values[existing, "latex"] <- latex
  } else {
    tags$values <- tibble::add_row(
      tags$values,
      name,
      value,
      latex
    )
  }
  
  save_tag_file(tags)
  
  invisible(value)
}

fmt_tag <- function(name, value, ...) {
  tag(name, fmt(value), ...)
}

latex_command_name <- function(s) {
  s <- stringr::str_split_fixed(s, "\\(", 2)[, 1]
  s <- stringr::str_to_title(s)
  stringr::str_replace_all(s, "\\s", "")
}

# TODO: test on latex_escape
latex_escape <- function(s) {
  s %>%
    stringr::str_replace_all("%", "\\%") %>%
    stringr::str_replace_all("_", "\\_") %>%
    stringr::str_replace_all("-", "")
}

generate_latex_command <- function(name, value, prefix = "") {
  stopifnot(length(names) == length(value))
  
  if (nchar(prefix)) {
    prefix <- stringr::str_c(prefix, " ")
  }
  name <- stringr::str_c(prefix, name)
  name <- latex_command_name(name)
  
  stringr::str_c(
    "\\newcommand{\\", latex_escape(name), "}{", latex_escape(value), "\\xspace}"
  )
}

save_tag_file <- function(tags) {
  out <- stringr::str_c(tags(tags)$latex, collapse="\n")
  writeLines(out, tags_filename(tags))
}

get_default_tags <- function() {
  getOption("latextags_default")
}

set_default_tags <- function(tags) {
  options(latextags_default=tags)
}

#' @export
as.list.latextags <- function(x, ...) {
  list(
    filename=tags_filename(x),
    prefix=tags_prefix(x),
    tags=tags(x)
  )
}

#' @export
format.latextags <- function(x, ...) {
  format(as.list(x), ...)
}

#' @export
print.latextags <- function(x, ...) {
  print(as.list(x), ...)
}

## Formatting

#' Formats given value.
#'
#' The `x` value is formatted according to its class with an additional
#' `prefix` and `suffix` prepended and appended respectively.
#'
#' @param x the value to be formatted
#' @param prefix the string to be prepended
#' @param suffix the string to be appended
#' @param ... parameters to be passed to the actual formatter. The number
#'     formatter accepts `floor` and `ceiling` to indicate that given value
#'     should be rounded down or up to a full integer before formatting or
#'     `digits` to indicate the number of digits the number should be rounded
#'     to.
#'
#' @export
fmt <- function(x, prefix="", suffix="", ...) {
  if (is.null(x)) {
    NULL
  } else {
    v <- as.character(.fmt(x, ...))
    
    stringr::str_c(prefix, v, suffix)
  }
}

.fmt <- function(x, ...) {
  UseMethod(".fmt", x)
}

.fmt.default <- function(x) {
  x
}

.fmt.integer <- function(x) {
  format(x, big.mark=",", trim=TRUE)
}

# TODO: make this parameters part of some global options so they can be set globally
# TODO: test c(0.0123, 1.123, -0.0123, -1.123)
.fmt.double <- function(x, digits=1, floor=FALSE, ceiling=FALSE) {
  if (floor) x <- floor(x)
  if (ceiling) x <- ceiling(x)
  
  x <- sapply(x, function(y) {
    y <- abs(y)
    
    if (is.na(y)) y
    else if (y < 1) signif(y, digits)
    else if (y > 1) round(y, digits)
    else y
  })

  prettyNum(x, big.mark=",", scientific=F)
}

.fmt.num_with_suffix <- function(x, floor=FALSE, ceiling=FALSE, digits=1) {
  suffix <- attr(x, "suffix")
  fmt(as.double(x), suffix=suffix, floor=floor, ceiling=ceiling, digits=digits)
}

#' Converts given value to a percent representation.
#'
#' It multiples the value by 100 and adds the `%` suffix attribute.
#'
#' @param x the value to convert
#'
#' @export
percent <- function(x) {
  x <- x*100
  class(x) <- "num_with_suffix"
  attr(x, "suffix") <- "%"
  x
}

# TODO: test for x < 0 and x > base^length(suffixes)
scale_with_suffix <- function(x, base, suffixes) {
  stopifnot(all(base > 0))
  stopifnot(length(suffixes) > 0)
  stopifnot(is.character(suffixes))
 
  scale <- if (length(base) == 1) {
    base^(1:length(suffixes))
  } else {
    stopifnot(length(base) == length(x))
    base
  }
  scale <- scale[1:length(suffixes)-1]
  
  magnitute <- floor(log(abs(x), base))
  magnitute[magnitute < 0] <- 0
  magnitute[magnitute > length(scale)] <- length(scale)
  
  magnitute <- as.integer(magnitute)
  suffix <- sapply(magnitute, function(x) suffixes[x + 1])
  
  d <- base^magnitute
  v <- x / d
  class(v) <- "num_with_suffix"
  attr(v, "suffix") <- suffix
  v
}

#' Scale the value to its closest order of magnitude
#'
#' It uses the base of 1000.
#'
#' @param x the value to scale
#' @param suffixes are the suffixes to be add. Each corresponds to the exponent
#'     of the base.
#'
#' @export
oom <- function(x, suffixes=c("", "K", "M", "G", "T")) {
  scale_with_suffix(x, base=1000, suffixes=suffixes)
}

#' Scale the value to its closest order of magnitude in the size of bytes.
#'
#' It uses the base of 1024.
#'
#' @param x the value to scale
#' @param units are the suffixes to be add. Each corresponds to the exponent
#'     of the base.
#'
#' @export
size <- function(x, units=c("B", "kB", "MB", "GB", "TB")) {
  scale_with_suffix(x, base=1024, suffixes=units)
}