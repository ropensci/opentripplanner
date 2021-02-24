#
#
#
# slow_func <- function(x, p){
#   #x <- x ** 2
#   p(sprintf("x=%g", x))
#   Sys.sleep(1)
#   return(x)
# }
#
#
# batch_function <- function(vals = rep(1:10, 2)){
#   vals <- vals[order(vals, decreasing = TRUE)]
#   progressr::handlers(global = TRUE)
#   progressr::handlers("progress")
#   p <- progressr::progressor(along = vals)
#
#   future::plan("future::multisession", workers = 4)
#   r <- future.apply::future_lapply(vals,
#                                    slow_func,
#                                    p = p,
#                                    future.scheduling = TRUE,
#                                    future.chunk.size = NULL)
#   future::plan("sequential")
#   r <- unlist(r)
#   return(r)
# }
