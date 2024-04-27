#' @export
getMlt <- function(mltDF, type, ...)
{
  #General interface for extracting various things from MltDataFrame
  if(class(mltDF) != "MltDataFrame")
  {stop("Provided object is not a MltDataFrame!")}

  params = list(...)

  #By doing this we can use methods
  class(type) = type

  getMlt_internal(type, mltDF, params)
}


getMlt_internal <- function(type, mltDF, params)
{
  UseMethod("getMlt_internal")
}

#' @export
getMlt_internal.transformed <- function(type, mltDF, params)
{
  #TODO: better ways to remove NA & var.rename
  #Returns normal df with transformed variables
  #options:
  #dummies - factors name to create dummies or TRUE to do all
  #add.index - adds numeric index to each row
  #na.replace - with what to replace NA
  #var.rename - rename variables based on pattern

  #extracting parameters
  if("dummies" %in% names(params))
    {var_dum = params[["dummies"]]} else {var_dum = NULL}

  if("add.index" %in% names(params))
    {add.index = params[["add.index"]]} else {add.index = FALSE}

  if("na.replace" %in% names(params))
    {na.replace = params[["na.replace"]]} else {na.replace = NULL}

  #Check which factors to replace
  if(!is.character(var_dum) & !is.null(var_dum))
    {all_dummies = ifelse(var_dum, TRUE, FALSE)} else {all_dummies = FALSE}

  vars = mltDF$variables

  df_n = mltDF$observations

  var_names = c()

  #Adding ID - we remove it after if add.index = FALSE
  id_name = ifelse("ID" %in% names(vars), "ID_DF", "ID")
  result = data.frame(ID = 1:df_n)
  names(result) = id_name

  #main loop
  for(i in names(vars))
  {
    var_i = vars[[i]][["raw"]]
    if(!is.null(na.replace))
    {
      var_i = sapply(var_i, function(x) {if(is.na(x)) {na.replace} else {x}})
    }

    if((all_dummies | (i %in% var_dum)) & vars[[i]][["type"]] == "factor")
    {
      vars[[i]][["raw"]] = var_i
      var_df = createDummies(vars[[i]])
      var_names = c(var_names, names(var_df))
    } else
    {
      attr(var_i, "label") = NULL

      if(vars[[i]][["type"]] == "factor")
      {
        var_df = as.factor(var_i)
      } else
      {
        var_df = var_i
      }

      var_names = c(var_names, i)
    }

    result = cbind(result, var_df)
  }

  if(add.index)
  {
    var_names = c(id_name, var_names)

    colnames(result) = var_names

  } else
  {
    result = result[,-1]
    colnames(result) = var_names
  }

  result
}


#' @export
getMlt_internal.corMatrix <- function(type, mltDF, params)
{
  #TODO: params checks
  #TODO: add smart division of corMatrix to simplify results
  #TODO: Providing >1 type of variables
  #Returns correlation matrix for either given variables or type
  #options:
  #var.type - for which type
  #variables - for which specific variables (of the same type)
  #precision - round results? (default 3)

  #First we check if correlation has been calculated
  if(length(mltDF$correlation) == 1)
  {if(is.na(mltDF$correlation))
    {stop("Correlation matrix is not available for this object. While analyzing df, run 'calc.cor = TRUE'!")}}

  #extracting parameters
  if("var.type" %in% names(params))
  {var_type = params[["var.type"]]} else {var_type = NULL}

  if("variables" %in% names(params))
  {vars = params[["variables"]]} else {vars = NULL}

  if("precision" %in% names(params))
  {precision = params[["precision"]]} else {precision = 3}

  if(is.null(var_type) & is.null(vars))
   {stop("Either var.type or variables parameter should be specified for correlation matrix!")}

  if(!is.null(var_type))
  {
    #When type is specified, it's easier since we dont need to check if variables are provided correctly
    if(!is.null(vars))
    {warning("Since var.type is specified, variables parameter will be suppresed.")}

    if(!(var_type %in% names(mltDF$correlation)))
    {stop("Provided variable type does not exist in given object!")}

    vars = NULL
    var_cor = mltDF$correlation[[var_type]]
  } else
  {
    #We remove duplicates
    vars = unique(vars)

    #First we check if all variables are the same type and exists
    vars_sup = vars[!(vars %in% names(mltDF$variables))]
    vars = vars[vars %in% names(mltDF$variables)]

    if(length(vars) <= 1)
    {stop("Correlation matrix cannot be generated with less than 1 variable specified!")}

    #Then we check which type is most common
    type_check = sapply(vars, function(x) {mltDF$variables[[x]][["type"]]})
    type_check_n = sapply(unique(type_check), function(x) {length(which(type_check == x))})
    var_type = names(which.max(type_check_n))

    vars_sup = append(vars_sup, names(type_check[type_check != var_type]))
    vars = names(type_check[type_check == var_type])

    if(length(vars_sup) > 0)
    {
      mess = "Not all provided variables exists nor are the same type!\nSuppressed the following variables: "
      for(i in vars_sup)
      {
        mess = paste0(mess, i, " ")
      }
      warning(mess)
    }

    var_cor = mltDF$correlation[[var_type]]

  }

  result = generate_cor_matrix(var_cor, vars, precision)

  result
}


