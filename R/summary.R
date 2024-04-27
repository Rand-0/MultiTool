#'@export
summary.Mltvector <- function(object, ...)
{
  vec_name = attr(object, "label")
  vec_type = object$type
  cat("   ", vec_name, "\n")
  cat("Type   :", vec_type, "\n")
  if(vec_type == 'numeric')
  {
    vec_basic = object$basic
    vec_out = object$outliers
    vec_quant = object$quantiles
    cat("Min.   :", round(vec_basic$min, 3), "\n")
    cat("1st Qu.:", round(vec_quant[2], 3), "\n")
    cat("Median :", round(vec_basic$median, 3), "\n")
    cat("Mean   :", round(vec_basic$mean, 3), "\n")
    cat("3rd Qu.:", round(vec_quant[4], 3), "\n")
    cat("Max.   :", round(vec_basic$max, 3), "\n")
    cat("Var.   :", round(vec_basic$var, 3), "\n")
    cat("NA's   :", vec_basic$missings, "\n")
    cat("Out.   :", length(vec_out[[1]]), "\n")
  } else if(vec_type == "factor")
  {
    summary(as.factor(object$raw))
  }
}
