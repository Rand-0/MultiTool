#' @export
plot.Mltvector <- function(object, ...)
{
  #It's a wrapper that calls plot based on type of given vector
  #For numeric variables we do a box plot and histogram for non-outliers
  #For factors we call normal plot.factor except with level for NA
  #For dates and characters we plot some how many missings, corrupted, etc
  params = list(...)
  vec_type = object$type
  vec_name = deparse1(substitute(object))
  if(vec_type == "numeric")
  {
    #we check wich missings to exclude in histogram
    out_meth = "IQR"
    if("outlier.method" %in% names(params)) {out_meth = params[["outlier.method"]]}

    vec_out = unlist(object$outliers[out_meth])

    vec_box = na.omit(object$raw)
    vec_hist = vec_box[!(vec_box %in% vec_out)]

    par(mfrow=c(2,1))

    boxplot(vec_box,
            main = paste("Box plot of", vec_name),
            xlab = "Values",
            ylab = vec_name,
            col = "lightblue",
            border = "black",
            horizontal = TRUE)

    hist(vec_hist,
         main = paste("Histogram of", vec_name),
         xlab = "Values",
         col = "lightblue",
         border = "black")
  } else if(vec_type == "factor")
  {

    vec_factor = as.character(object$raw)
    vec_factor[is.na(vec_factor)] = "NA"

    par(mfrow=c(1,1))

    plot(as.factor(vec_factor),
         main = paste("Value counts of", vec_name),
         col = "lightblue",
         ylab = "Frequency",
         xlab = "Category")

  } else if(vec_type == "date")
  {

    vec_val = length(object$raw) - object$corrupted - object$missings
    vec_inv = object$corrupted
    vec_miss = object$missings

    par(mfrow=c(1,1))

    barplot(c(vec_val, vec_inv, vec_miss),
            main = paste("Summary of", vec_name),
            col = "lightblue",
            ylab = "Frequency",
            names.arg = c("Valid", "Invalid", "NA"))

  } else if(vec_type == "character")
  {

    vec_uni = object$unique
    vec_dup = length(object$duplicates)
    vec_miss = object$missings

    par(mfrow=c(1,1))

    barplot(c(vec_uni, vec_dup, vec_miss),
            main = paste("Summary of", vec_name),
            col = "lightblue",
            ylab = "Frequency",
            names.arg = c("Unique", "Duplicates", "NA"))

  } else
  {
    mess = paste("Type", vec_type, "it's not supported!")
    stop(mess)
  }


}
