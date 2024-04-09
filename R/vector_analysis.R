#' @export
vector_analysis <- function(vec)
{
  #We check if it's atomic
  if (!is.atomic(vec)) {stop("Provided object must be an atomic vector!")}
  if(length(vec) < 100) {warning("Auto-detection might not work correctly for short vectors!")}

  UseMethod("vector_analysis")
}

#' @export
vector_analysis.numeric <- function(vec)
{
  #Dependants: moments
  #Parameters: Outlier_method = c("IQR", "Mod_Zscore")
  #For numeric variables

  #First we check if it's not a factor variable with weird encoding
  if(length(unique(vec)) <= 10) { return(vector_analysis.factor(as.factor(vec))) }

  #Basic (mean, median, min, max, sd, var, quantiles, missings):
  vec_n = length(vec)
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

  outliers = list(IQR = vec_out_iqr, Mod_Zscore = vec_out_z)

  result = list(type = "numeric", raw = vec, basic = basic, advanced = advanced,
                distributions = distributions, quantiles = vec_quant, outliers = outliers)

  #assigning class for methods for print, plot etc
  class(result) = "Mltvector"

  result
}

#' @export
vector_analysis.integer <- function(vec)
{
  #We need to check if it's a numeric variable or with only natural numbers
  #or it's a factor variable
  if(length(unique(vec)) <= 10)
    { vector_analysis.factor(as.factor(vec)) } else {vector_analysis.numeric(vec)}

}

#' @export
vector_analysis.factor <- function(vec)
{
  #For factor variables

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

  result
}

#' @export
vector_analysis.character <- function(vec)
{
  #We need to check if it's a factor variable, a date or just plain text
  #We check if >50% of non-missing data is formated as date
  vec_n = length(vec)
  vec_miss = sum(is.na(vec))
  vec_miss_p = vec_miss/vec_n*100
  vec_nm = na.omit(vec)

  #To avoid long computational time (12 months * 3 types of sep)
  if(length(unique(sapply(vec_nm, nchar))) < 36)
  {
    vec_dates = sapply(vec_nm, DateFormat)

    if(length(na.omit(vec_dates))/(vec_n-vec_miss) > 0.5) {return(vector_analysis.dates(vec))}
  }

  #It might be a factor then (since it's not numeric we can consider max 10% levels)
  vec_ulev = length(unique(vec_nm))

  if(vec_ulev < vec_n/10) {return(vector_analysis.factor(as.factor(vec)))}

  #For now it's most likely just plain text
  vec_uni = length(unique(vec_nm))
  vec_dup = unique(vec_nm[duplicated(vec_nm)])

  result = list(type = "character", raw = vec, missings = vec_miss, unique = vec_uni,
                duplicates = vec_dup)

  #assigning class for methods for print, plot etc
  class(result) = "Mltvector"

  result
}

#' @export
vector_analysis.dates <- function(vec)
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

  basic = list(min = as.Date(vec_min), max = as.Date(vec_max))

  trans = list(Date = as.Date(vec_dates), Other = vec_notdates)

  result = list(type = "date", raw = vec, basic = basic,
                missings = vec_miss, corrupted = vec_cor,
                transformed = trans)

  #assigning class for methods for print, plot etc
  class(result) = "Mltvector"

  result
}


