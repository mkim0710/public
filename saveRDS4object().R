saveRDS4object <- function (object_name) {
  saveRDS(object_name, file = sprintf("%s.RDS", deparse(substitute(object_name)) ) )
}
