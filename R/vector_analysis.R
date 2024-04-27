#' @export
vector_analysis <- function(vec)
{
  #We check if it's atomic
  if (!is.atomic(vec)) {stop("Provided object must be an atomic vector!")}
  if(length(vec) < 100) {warning("Auto-detection might not work correctly for short vectors!")}

  UseMethod("vector_analysis")
}

#' @export
vector_analysis.mltnumeric <- function(vec)
{
  #To do: improve factors detection
  #Dependants: moments
  #Parameters: Outlier_method = c("IQR", "Mod_Zscore")
  #For numeric variables

  vec_n = length(vec)

  #Basic (mean, median, min, max, sd, var, quantiles, missings):
  vec_miss = sum(is.na(vec))
  vec_miss_p = vec_miss/vec_n*100
  vec_nm = na.omit(vec)

  vec_mean = mean(vec_nm)
  vec_med = median(vec_nm)
  vec_min = min(vec_nm)
  vec_max = max(vec_nm)
  vec_sd = sd(vec_nm)
  vec_var = vec_sd^2
  vec_quant = quantile(vec_nm)

  #Outliers
  vec_out_z = c()
  vec_out_iqr = c()

  if(mad(vec_nm) != 0)
  {
    #Modified Z-score method
    vec_mad = mad(vec_nm)
    for(i in vec_nm) {if((0.6745*(i - vec_med)/vec_mad) > 3.5) {vec_out_z = c(vec_out_z, i)}}
  }

  #IQR method
  vec_iqr = IQR(vec_nm)
  vec_bounds = c(vec_quant[2] - 1.5 * vec_iqr, vec_quant[4] + 1.5 * vec_iqr)
  for(i in vec_nm) {if(i < vec_bounds[1] | i > vec_bounds[2]) {vec_out_iqr = c(vec_out_iqr, i)}}


  #Advanced (kurtosis, skewness)
  vec_kurt = moments::kurtosis(vec_nm)
  vec_skew = moments::skewness(vec_nm)


  #Distributions
  #Normal
  vec_norm = isNormalDist(vec, vec_kurt, vec_skew)


  basic = list(mean = vec_mean, median = vec_med,
               min = vec_min, max = vec_max, missings = vec_miss,
               sd = vec_sd, var = vec_var)

  advanced = list(kurtosis = vec_kurt, skewness = vec_skew)

  distributions = list(normal = vec_norm)

  outliers = list(Mod_Zscore = vec_out_z, IQR = vec_out_iqr)

  result = list(type = "numeric", raw = vec, basic = basic, advanced = advanced,
                distributions = distributions, quantiles = vec_quant, outliers = outliers)

  #assigning class for methods for print, plot etc
  class(result) = "Mltvector"
  attr(result, "label") = attr(vec, "label")

  result
}

#' @export
vector_analysis.mltfactor <- function(vec)
{
  #For factor variables
  vec_label = attr(vec, "label")
  vec = as.factor(vec)

  #Basic (missings, levels)
  vec_n = length(vec)
  vec_miss = sum(is.na(vec))
  vec_miss_p = vec_miss/vec_n*100
  vec_nm = na.omit(vec)
  vec_lev = levels(vec_nm)

  vec_table = table(vec_nm)

  basic = list(missings = vec_miss, levels = vec_lev)

  result = list(type = "factor", raw = unclass(vec), basic = basic, table = vec_table)

  #assigning class for methods for print, plot etc
  class(result) = "Mltvector"
  attr(result, "label") = vec_label

  result
}

#' @export
vector_analysis.mltcharacter <- function(vec)
{
  vec_nm = na.omit(vec)
  vec_miss = sum(is.na(vec))

  #For only strings
  vec_uni = length(unique(vec_nm))
  vec_dup = unique(vec_nm[duplicated(vec_nm)])

  basic = list(missings = vec_miss, unique = vec_uni, duplicates = vec_dup)

  result = list(type = "character", raw = vec, basic = basic)

  #assigning class for methods for print, plot etc
  class(result) = "Mltvector"
  attr(result, "label") = attr(vec, "label")

  result
}

#' @export
vector_analysis.mltdate <- function(vec)
{
  vec_n = length(vec)
  vec_miss = sum(is.na(vec))
  vec_miss_p = vec_miss/vec_n*100
  vec_nm = na.omit(vec)

  if(typeof(vec) == "character")
  {
    vec_dates = sapply(vec_nm, DateFormat)
    vec_notdates = vec_nm[which(is.na(vec_dates))]
  }
  vec_min = min(vec_dates)
  vec_max = max(vec_dates)
  vec_cor = length(vec_notdates)

  basic = list(missings = vec_miss, min = as.Date(vec_min), max = as.Date(vec_max))

  trans = list(Date = as.Date(vec_dates), Other = vec_notdates)

  result = list(type = "date", raw = vec, basic = basic, corrupted = vec_cor,
                transformed = trans)

  #assigning class for methods for print, plot etc
  class(result) = "Mltvector"
  attr(result, "label") = attr(vec, "label")

  result
}


