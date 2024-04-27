analyze_cor_nts_internal <- function(mlt_df, p.value)
{
  #Dependants: purrr
  #Interface for analyzing correlation in given MltDataFrame
  #We calculate correlation for only numeric and factor variables (with more than 2 levels)
  #All checks are in internal functions (we just na.omit at the end)
  var_factors = na.omit(unname(sapply(mlt_df,
                                      function(var) if(var$type == "factor")
                                       {attr(var, "label")} else {NA})))

  var_numeric = na.omit(unname(sapply(mlt_df,
                                      function(var) if(var$type == "numeric")
                                       {attr(var, "label")} else {NA})))

  var_factors_comb = t(combn(var_factors, 2))
  var_numeric_comb = t(combn(var_numeric, 2))

  interfaceCor <- function(nvar1, nvar2, df, p.value)
  {
    analyze_cor_basic(df[[nvar1]], df[[nvar2]], p.value)
  }

  cor_factors = purrr::map2(var_factors_comb[,1],
                            var_factors_comb[,2],
                            interfaceCor,
                            mlt_df,
                            p.value)

  cor_numeric = purrr::map2(var_numeric_comb[,1],
                            var_numeric_comb[,2],
                            interfaceCor,
                            mlt_df,
                            p.value)


  res_factors = list()
  res_numeric = list()

  for(i in var_factors)
  {
    for(j in 1:length(cor_factors))
    {
      res = cor_factors[[j]][[i]]
      if(!is.null(res))
      {
        if((!is.na(res)))
        {
          res_factors[[i]] = append(res_factors[[i]], res)
        }
      }
    }
  }

  for(i in var_numeric)
  {
    for(j in 1:length(cor_numeric))
    {
      res = cor_numeric[[j]][[i]]
      if(!is.null(res))
      {
        if(!is.na(res))
        {
          res_numeric[[i]] = append(res_numeric[[i]], res)
        }
      }
    }
  }

  result = list(numeric = res_numeric, factor = res_factors)

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
    res = analyze_cor_basic_num(var1, var2, p.value)

  } else if(all(c(tvar1, tvar2) == "factor"))
  {
    res = analyze_cor_basic_factor(var1, var2)
  } else
  {
    stop("Correlation cannot be calculated for different types of variables!")
  }

  var1_name = attr(var1, "label")
  var2_name = attr(var2, "label")

  result = list()

  result[[var1_name]] = list()
  result[[var1_name]][[var2_name]] = res

  result[[var2_name]] = list()
  result[[var2_name]][[var1_name]] = res

  result
}

analyze_cor_basic_num <- function(var1, var2, p.val)
{
  #Dependants: pcaPP
  #Parameters: p.val = 0.05
  #Correlation between 2 numeric variables
  #Firstly we need to check if they are normally distributed
  raw_var1 = var1$raw
  raw_var2 = var2$raw

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

generate_cor_matrix <- function(cor_list, var_names, precision)
{
  #Returns correlation matrix
  #Internal function of getMlt.corMatrix
  #If var_names = NULL, all variables are being used

  if(is.null(var_names))
  {
    var_names = names(cor_list)
  }

  generateRow <- function(var, cors, var_names)
  {
    res = unlist(sapply(cors, function(x) {x[[var]][['coefficient']]}))
    res = round(res[names(res) %in% var_names], precision)
    base = NA
    names(base) = var
    base_index = which(names(cors) == var) - 1
    return(append(res, base, after = base_index))
  }

  result = sapply(var_names, generateRow, cor_list, var_names)

  result

}

