# Operators to check if entries of a vector x are inside the limits specified by a tuple y.
# <TODO> Document!
# <TODO> add Date methods!
in_closed <- function (x, y, ...) UseMethod("in_closed")
in_closed.numeric <- function (x, y, ...) {
  if ((length(y) != 2) | !is.numeric(y)) stop()
  (x >= y[1]) & (x <= y[2])
}
in_closed.POSIXct <- function (x, y, ...) {
  if ((length(y) != 2)) stop()
  if (inherits(try(sapply(y, as.POSIXct), silent = TRUE), "try-error")) stop()
  if (!inherits(y, "POSIXct")) y <- as.POSIXct(y, ...)
  (x >= y[1]) & (x <= y[2])
}
in_closed.Date <- function (x, y, ...) {
  if ((length(y) != 2)) stop()
  if (inherits(try(sapply(y, as.Date), silent = TRUE), "try-error")) stop()
  if (!inherits(y, "Date")) y <- as.Date(y, ...)
  (x >= y[1]) & (x <= y[2])
}
"%[]%" <- in_closed


in_open <- function (x, y, ...) UseMethod("in_open")
in_open.numeric <- function (x, y, ...) {
  if ((length(y) != 2) | !is.numeric(y)) stop()
  (x > y[1]) & (x < y[2])
}
in_open.POSIXct <- function (x, y, ...) {
  if ((length(y) != 2)) stop()
  if (inherits(try(sapply(y, as.POSIXct), silent = TRUE), "try-error")) stop()
  if (!inherits(y, "POSIXct")) y <- as.POSIXct(y, ...)
  (x > y[1]) & (x < y[2])
}
in_open.Date <- function (x, y, ...) {
  if ((length(y) != 2)) stop()
  if (inherits(try(sapply(y, as.Date), silent = TRUE), "try-error")) stop()
  if (!inherits(y, "Date")) y <- as.Date(y, ...)
  (x > y[1]) & (x < y[2])
}
"%()%" <- in_open


in_halfopen_left <- function (x, y, ...) UseMethod("in_halfopen_left")
in_halfopen_left.numeric <- function (x, y, ...) {
  if ((length(y) != 2) | !is.numeric(y)) stop()
  (x > y[1]) & (x <= y[2])
}
in_halfopen_left.POSIXct <- function (x, y, ...) {
  if ((length(y) != 2)) stop()
  if (inherits(try(sapply(y, as.POSIXct), silent = TRUE), "try-error")) stop()
  if (!inherits(y, "POSIXct")) y <- as.POSIXct(y, ...)
  (x > y[1]) & (x <= y[2])
}
in_halfopen_left.Date <- function (x, y, ...) {
  if ((length(y) != 2)) stop()
  if (inherits(try(sapply(y, as.Date), silent = TRUE), "try-error")) stop()
  if (!inherits(y, "Date")) y <- as.Date(y, ...)
  (x > y[1]) & (x <= y[2])
}
"%(]%" <- in_halfopen_left


in_halfopen_right <- function (x, y, ...) UseMethod("in_halfopen_right")
in_halfopen_right.numeric <- function (x, y, ...) {
  if ((length(y) != 2) | !is.numeric(y)) stop()
  (x >= y[1]) & (x < y[2])
}
in_halfopen_right.POSIXct <- function (x, y, ...) {
  if ((length(y) != 2)) stop()
  if (inherits(try(sapply(y, as.POSIXct), silent = TRUE), "try-error")) stop()
  if (!inherits(y, "POSIXct")) y <- as.POSIXct(y, ...)
  (x >= y[1]) & (x < y[2])
}
in_halfopen_right.Date <- function (x, y, ...) {
  if ((length(y) != 2)) stop()
  if (inherits(try(sapply(y, as.Date), silent = TRUE), "try-error")) stop()
  if (!inherits(y, "Date")) y <- as.Date(y, ...)
  (x >= y[1]) & (x < y[2])
}
"%[)%" <- in_halfopen_right

# Examples
# 4 %[]% c(4, 6)
# 3 %[]% c(4, 6)
# 4 %(]% c(4, 6)
# 6 %(]% c(4, 6)
# 4 %[)% c(4, 6)
# 6 %[)% c(4, 6)
# 6 %()% c(4, 6)
# 5 %()% c(4, 6)
