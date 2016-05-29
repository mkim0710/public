readRDS2object <- function (object_name) {
  print(sprintf("%s.RDS", deparse(substitute(object_name)) ) )
  tmp <- readRDS( sprintf("%s.RDS", deparse(substitute(object_name)) ) )
  assign(deparse(substitute(object_name)), tmp, inherits = TRUE)
}