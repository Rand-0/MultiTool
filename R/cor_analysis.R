analyze_cor_nts_internal <- function(mlt_df, p.value)
{
  #TODO: use mapply()
  #Interface for analyzing correlation in given MltDataFrame
  #We calculate correlation for only numeric and factor variables (with more than 2 levels)
  #All checks are in internal functions (we just na.omit at the end)
  var_factors = na.omit(unname(sapply(mlt_df,
                                      function(var) if(var$type == "factor")
                                       {attr(var, "label")} else {NA})))

  var_numeric = na.omit(unname(sapply(mlt_df,
                                      function(var) if(var$type == "numeric")
                                       {attr(var, "label")} else {NA})))

  cor_factors = list()
  cor_numeric = list()

  for(i in var_factors)
  {
    cor_i = list()
    for(j in var_factors[-which(var_factors == i)])
    {
      res = analyze_cor_basic(mlt_df[[i]],
                              mlt_df[[j]],
                              p.value)

      if(length(res) == 1) {next}

      cor_i = c(cor_i, setNames(list(res), j))
    }

    if(length(cor_i) == 0) {next}

    cor_factors[[i]] = cor_i

  }

  for(i in var_numeric)
  {
    cor_i = list()
    for(j in var_numeric[-which(var_numeric == i)])
    {
      res = analyze_cor_basic(mlt_df[[i]],
                              mlt_df[[j]],
                              p.value)

      if(length(res) == 1) {next}

      cor_i = c(cor_i, setNames(list(res), j))
    }

    if(length(cor_i) == 0) {next}

    cor_numeric[[i]] = cor_i

  }
  result = list(numeric = cor_numeric, factor = cor_factors)

  result
}


analyze_cor_basic <- function(var1, var2, p.value)
{
  #TODO: type check for factors
  #Most basic correlation between 2 variables
  #It's not intended for ts
  #Correlation is calculated without missings
  #It's a interface for specific functions
  #Returns name of method, p-value and correlation coefficient

  tvar1 = var1$type
  tvar2 = var2$type

  raw_var1 = var1$raw
  raw_var2 = var2$raw

  #First we check if there were provided only numeric/factor variables
  if(!all(c(tvar1, tvar2) %in% c("numeric", "factor")))
  {stop("Correlation cannot be calculated between non-numeric/factor variables!")}

  if(all(c(tvar1, tvar2) == "factor"))
  {
    raw_var1 = as.factor(raw_var1)
    raw_var2 = as.factor(raw_var2)
  }

  #Then we check if provided variables match their real types
  if(!(isRealType(raw_var1, var1$type) & isRealType(raw_var2, var2$type)))
  {stop("Provided variable type does not match it's real type!")}

  if(all(c(tvar1, tvar2) == "numeric"))
  {
    result = analyze_cor_basic_num(var1, var2, p.value)

  } else if(all(c(tvar1, tvar2) == "factor"))
  {
    result = analyze_cor_basic_factor(var1, var2)
  } else
  {
    stop("Correlation cannot be calculated for different types of variables!")
  }

  result
}

analyze_cor_basic_num <- function(var1, var2, p.val)
{
  raw_var1 = var1$raw
  raw_var2 = var2$raw
  #Dependants: pcaPP
  #Parameters: p.val = 0.05
  #Correlation between 2 numeric variables
  #Firstly we need to check if they are normally distributed
  norm1 = isNormalDist(raw_var1, var1$advanced$kurtosis, var1$advanced$skewness)
  norm2 = isNormalDist(raw_var2, var2$advanced$kurtosis, var2$advanced$skewness)

  #We check if there is a sufficient no of observations
  if(any(is.character(c(norm1, norm2)))) {return(NA)}

  #Then we check for outliers - we use list no 1. since it should be better than those later
  #As default, it's modified Z-score
  vec1_out = var1$outliers[[1]]
  vec2_out = var2$outliers[[1]]

  #We chose an appriopriate test
  if(all(c(norm1$p.value, norm2$p.value) >= p.val) & any(length(c(vec1_out, vec2_out) == 0)))
  {
    #If variables are normally distributed and there are no outliers
    #We use pearson coefficient
    #Pearson is very fast, no need for external library
    pearson_test = cor.test(raw_var1, raw_var2, method = "pearson")
    test_pval = pearson_test$p.value
    test_method = pearson_test$method
    test_cor = unname(pearson_test$estimate)

  } else if(!any(is.na(c(raw_var1, raw_var2))))
  {
    #If there are no missing values, we can use fast kendall tau
    test_pval = NA
    test_method = "Kendall's rank correlation tau"
    test_cor = pcaPP::cor.fk(raw_var1, raw_var2)

  } else
  {
    #If there are missings we cannot use fast estimations of kendall tau
    #We use basic one and just wait
    kendall_test = cor.test(raw_var1, raw_var2, method = "kendall")
    test_pval = kendall_test$p.value
    test_method = kendall_test$method
    test_cor = unname(kendall_test$estimate)

  }

  return(list(method = test_method, p.value = test_pval, coefficient = test_cor))

}

analyze_cor_basic_factor <- function(var1, var2)
{
  #Dependants: rcompanion
  #Correlation between 2 factor variables (with >2 levels)
  if(any(c(length(var1$basic$levels), length(var2$basic$levels)) <= 2))
  {return(NA)}

  var1_trans = as.factor(var1$raw)
  var2_trans = as.factor(var2$raw)

  test_pval = NA
  test_method = "Cramer's V"
  test_cor = unname(rcompanion::cramerV(var1_trans, var2_trans))

  return(list(method = test_method, p.value = test_pval, coefficient = test_cor))
}
