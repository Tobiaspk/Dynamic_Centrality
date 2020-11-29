cat_ <- function(...) cat(paste0(..., "\n"))
collapse_ <- function(..., collapse = ", ") paste0(..., collapse = collapse)
fastdate <- function(x) as.Date(fasttime::fastPOSIXct(x))
map_id <- function(x, levels) as.integer(factor(x, levels = levels))
grep_get <- function(pattern, x) x[grep(pattern = tolower(pattern), x = tolower(x))]
