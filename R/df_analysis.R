#' @export
preanalyze_df <- function(df, index.limit = 3)
{
  #Dependants: tibble
  #General interface for df preprocessing
  #Should call all internal functions
  #Typical process:
  #index -> types
  #it returns object
  if(!is.data.frame(df)) { stop("Provided object is not a dataframe!") }
  if(tibble::is_tibble(df)) { df = as.data.frame(df) }

  index = analyze_df_index(df, index.limit)

  types = detect_type(df)

  result = list(index = index, column_types = types)

  class(result) = "MltMetaData"

  result
}

#' @export
analyze_df <- function(df, type = "C-S", index = NULL, calc.cor = FALSE,
                       p.value = 0.05, ...)
{
  #Dependants: dplyr
  #Parameters: type = c("C-S", "TS", "Panel"), index = c(time_index, index1, index2,...)
  #If object "MltMetaData" is supplied, we skip preanalysis

  #Message no 1 - start
  cat("Gathering metadata... ")

  params = list(...)
  if("meta.data" %in% names(params)) {meta_data = params[["meta.data"]]} else
  { meta_data = preanalyze_df(df) }

  #We give a benefit of the doubt to the user
  if(any(names(params) %in% c("factor", "numeric", "date", "character", "other")))
  { meta_data = correctMetaData(meta_data, params[c("factor", "numeric", "date", "character", "other")]) }

  if(!is.null(index))
  {
    if(!(index %in% unlist(meta_data, recursive = FALSE)))
    {stop("Provided index is not unique!")}

    #0 means auto index
    if(index == 0)
    {index = unlist(meta_data, recursive = FALSE)[[1]]}
  }

  if(is.null(index))
  {
    #For non-panel data lack of index is fine
    if(is.null(index))
    {
      if(type != "Panel")
      {
        index = NA
      } else
      {
        stop("Panel data requires valid index!")
      }
    }
  }

  #Makes transformations easier
  df_n = nrow(df)

  #Message no 1 - end
  cat("Done\n")

  var_types = c()

  #Message no 2 - start
  cat("Analyzing variables... ")

  for(i in names(meta_data$column_types))
  {
    var_types = append(var_types, rep(paste0("mlt", i),
                                      length(unlist(meta_data$column_types[[i]],
                                                    recursive = FALSE))))
  }

  names(var_types) = unlist(meta_data$column_types)

  if(!is.na(index))
    {df_noindex = df[,-which(names(df) %in% index)]} else {df_noindex = df}

  var_analysis = analyze_df_internal(df_noindex, var_types)

  #Message no 2 - end
  cat("Done\n")

  if(calc.cor)
  {
    #Message no 3 - start
    cat("Calculating correlations... ")

    cor_analysis = analyze_df_cor(var_analysis, type, p.value)

    #Message no 3 - end
    cat("Done\n")

  } else {cor_analysis = NA}

  type = dplyr::case_when(type == "C-S" ~ "Cross-sectional data",
                          type == "Panel" ~ "Panel data",
                          type == "TS" ~ "Time series data")

  result = list(type = type, observations = df_n, index = index, variables = var_analysis,
                correlation = cor_analysis)

  class(result) = "MltDataFrame"

  cat("Analysis completed.\n")

  result
}

analyze_df_internal <- function(df, types)
{
  #We call this function for each non-index variables
  results = list()

  for (i in colnames(df))
  {
    vec = df[,i]
    class(vec) = types[i]
    attr(vec, "label") = i
    results[[i]] = vector_analysis(vec)
  }

  results
}

#' @export
analyze_df_index <- function(df, index.limit = 3)
{
  #Dependants: dplyr
  #TO DO: ADD DATES
  #First function to call in analyze_df if indexes are not specified
  #We consider only integer/character/factor variables w/e missings and negative values
  #It returns potential variables to use as index (non-limited amount)
  #If there is no candidates for index, it returns NA and we treat it as c-s data
  columns = colnames(df)
  df_n = nrow(df)

  can_index <- function(col)
  {
    if(!any(is.na(df[,col])))
    {
      if(typeof(df[,col]) %in% c("integer", "character", "factor"))
      {col} else if (all(grepl("^\\d+$", as.character(df[,col])))) {col} else {NA}
    } else {NA}
  }

  candidates = na.omit(sapply(columns, can_index))

  #We have to convert doubles to integer
  for(i in candidates)
  {
    if(is.numeric(df[,i])) {df[,i] = as.integer(df[,i])}
  }

  #We search for columns combination that are unique across df
  #Since checking combinations of multiple variables might be time-consuming
  #We limit it to 3 (by default)
  indexes = list()

  #At first we consider single columns
  indexes[["index_1"]] = na.omit(sapply(candidates,
                                        function(i) {if(length(unique(df[,i])) == df_n)
                                                     {i} else {NA}}))

  #If we found something that is an index, we should consider it anymore
  candidates = candidates[!(candidates %in% indexes[["index_1"]])]


  if(index.limit <= 1 || index.limit >= ncol(df)) {return(indexes)}

  #We remove empty lists from indexes
  contains_only_na <- function(sublist) {all(is.na(sublist))}

  #Then we consider all variables combinations (no longer than index.limit)
  for(i in 2:index.limit)
  {
    if(length(candidates) < i) {break}

    candidates_i = t(combn(candidates, i))

    ind_i = apply(candidates_i, 1,
                  function(cmb) {if(nrow(dplyr::distinct(df[,cmb])) == df_n)
                                 {cmb} else {NA}})

    ind_name = paste0("index_", i)

    indexes[[ind_name]] = Filter(Negate(contains_only_na), ind_i)
  }

  #We return NA if there is no possible combinations (it's a cross-sectional dataset)
  if(all(lengths(indexes) == 0)) {NA} else {indexes}
}

analyze_df_cor <- function(df, method, p.value)
{
  #We analyze correlation between variables
  #It should call different methods based on type (ts, panel, c-s)

  if(method %in% c("C-S", "Panel"))
  {
    return(analyze_cor_nts_internal(df, p.value))
  }
}
