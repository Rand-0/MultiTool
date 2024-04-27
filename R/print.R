#'@export
print.MltMetaData <- function(object)
{
  print.default(object)
}

#'@export
print.Mltvector <- function(object)
{
  vec_name = attr(object, "label")
  vec_type = object$type
  cat("   ", vec_name, "\n")
  cat("Type   :", vec_type, "\n\n")
  if(vec_type == 'numeric')
  {
    vec_basic = object$basic
    vec_adv = object$advanced
    vec_out = object$outliers
    vec_quant = object$quantiles
    vec_dis = object$distributions

    cat("Basic:\n")
    cat("Mean   :", round(vec_basic$mean, 3), "\n")
    cat("Sd.    :", round(vec_basic$var, 3), "\n")
    cat("Var.   :", round(vec_basic$var, 3), "\n")
    cat("NA's   :", vec_basic$missings, "\n\n")

    cat("Advanced:\n")
    cat("Kurt.  :", round(vec_adv$kurtosis, 3), "\n")
    cat("Skew.  :", round(vec_adv$skewness, 3), "\n\n")

    cat("Distributions:\n")
    for(i in names(vec_dis))
    {
      cat(vec_dis[[i]][['method']], "\n")
      cat("statistic =", vec_dis[[i]][['statistic']], ", p-value =", vec_dis[[i]][['p.value']], "\n\n")
    }

    cat("Quantiles:\n")
    print(vec_quant)

    cat("\nOutliers:\n")
    for(i in names(vec_out))
    {
      cat(i, ":", length(vec_out[[i]]), "\n")
    }
  }
}
