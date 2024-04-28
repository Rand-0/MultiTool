#'@export
modifyMlt <- function(mltDF, variables, actions, ...)
{
  #Interface function for mltdf modifications
  #type of actions - normalize, standarize, winsorize, na.replace, type.change
  #Actions are done in given order

  if(class(mltDF) != "MltDataFrame")
  {stop("Provided object is not a MltDataFrame!")}

  params = list(...)

  vars = unique(variables)
  vars = vars[vars %in% names(mltDF$variables)]

  if(!all(actions %in% c("normalize", "standarize", "winsorize", "na.replace")))
  {stop("Incorrect action types supplied! Check ?modifyMlt for available options.")}

  #We subset variables to transform
  vars_to_transform = mltDF$variables[vars]

  #we iterate over actions
  for(act in actions)
  {
    func = paste("modifyMlt", act, sep = "_")
    vars_to_transform = get(func)(vars_to_transform, params)
  }

  vars_to_transform
}

modifyMlt_normalize <- function(vars, params)
{
  result = list()

  for(i in names(vars))
  {
    if(vars[[i]][["type"]] != "numeric")
    {stop("Normalization is valid only for numer variables!")}

    var_i = vars[[i]]

    var_i_trans = (var_i$raw - var_i$basic$min)/(var_i$basic$max - var_i$basic$min)

    result[[i]] = vector_analysis.mltnumeric(var_i_trans)
  }

  result
}

modifyMlt_type.change <- function(vars, params)
{
  #TODO: Finish this
  new_types = params[c("factor", "numeric", "date", "character", "other")]

  new_types = Filter(function(x) length(x) > 0, new_types)

  for(i in names(vars))
  {
    for(j in names(new_types))
    {

    }
  }
}
