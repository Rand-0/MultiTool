#'@export
analyze_cor_basic <- function(var1, var2)
{
  #Most basic correlation between 2 variables
  #It's not intended for ts and panel variables
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

  #Then we check if provided variables match their real types
  if(!(isRealType(raw_var1, tvar1) & isRealType(raw_var2, tvar2)))
  {stop("Provided variable type does not match it's real type!")}

  if(all(c(tvar1, tvar2) == "numeric"))
  {
    analyze_cor_basic_num(var1, var2)
  }
}

analyze_cor_basic_num <- function(var1, var2)
{
  #To Do: Fast correlation calculations
  #Parameters: p.val = 0.05
  p.val = 0.05
  #Correlation between 2 numeric variables
  #Firstly we need to check if they are normally distributed
  norm1 = isNormalDist(var1)
  norm2 = isNormalDist(var2)

  #We check if there is a sufficient no of observations
  if(any(is.character(c(norm1, norm2)))) {return(NA)}

  #Then we check for outliers - we use list no 1. since it should be better than those later
  #As default, it's modified Z-score
  vec1_out = var1$outliers[1]
  vec2_out = var2$outliers[1]

  #We chose an appriopriate test
  if(all(c(norm1$p.value, norm2$p.value) >= p.val))
  {
    if(any(length(c(vec1_out, vec2_out) == 0)))
    {
      #If variables are normally distributed and there are no outliers
      #We use pearson coefficient

    }
  }


}
