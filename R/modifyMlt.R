#'@export
modifyMlt <- function(mltDF, variables, actions, ...)
{
  #Interface function for mltdf modifications
  #type of actions - normalize, standarize, winsorize, na.replace
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
