library(dplyr)
library(stringr)

format_size <- function(x) {
  if (is.na(x)) return(NA)
  
  units <- c("B", "kB", "MB", "GB", "TB", "PB")
  
  fmt <- function(x, i=1) {
    xx <- x / 1024
    if (abs(xx) > 1 && i < length(units)) {
      fmt(xx, i+1)
    } else {
      sprintf("%.2f %s", x, units[i])
    }
  }
  
  sapply(x, fmt, USE.NAMES=FALSE)
}

fmt <- function(x, prefix="", suffix="", floor=FALSE, ceiling=FALSE, digits=1) {
  v <- switch(
    typeof(x), 
    integer=format(x, big.mark=",", small.mark="."),
    double={
      if (floor) x <- floor(x)
      if (ceiling) x <- ceiling(x)
      format(round(x, digits), big.mark=",")
    },
    character=x
  )
  str_c(prefix, v, suffix)
}

percent <- function(x, ...) my_format(x, s="%", ...)

round_down_nice <- function(x, nice=c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * (nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]-1]])
}

my_stat_fmt <- function(x) {
  s <- sd(x, na.rm = T)
  m <- median(x, na.rm = T)
  str_c('(s=', fmt(s, floor=s > 1e3), ' m=', fmt(m, floor=m > 1e3), ')')
}

add_num <- function(key, v) {
  stat <- function(fun) if (length(v) > 1) fun(v, na.rm=TRUE) else NA
  
  tibble(
    key, 
    sum=sum(v, na.rm = TRUE),
    n=length(v),
    mean=stat(mean), 
    median=stat(median), 
    sd=stat(sd),
    min=stat(min),
    max=stat(max)
  ) %>%
    mutate_at(vars(-key), fmt)
}

make_stats <- function(...) {
  all <- list(...)
  bind_rows(all)
}

is_outlier_min <- function(x) quantile(x, 0.25) - 1.5 * IQR(x)
is_outlier_max <- function(x) quantile(x, 0.75) + 1.5 * IQR(x)
is_outlier <- function(x) {
  (x < is_outlier_min(x)) | (x > is_outlier_max(x))
}