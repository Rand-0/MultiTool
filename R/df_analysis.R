#' @export
analyze_df_internal <- function(df)
{
  #We call this function for id/time variables
  results = list()

  for (i in colnames(df))
  {
    results[[i]] = vector_analysis(df[,i])
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

analyze_df_cor <- function(df)
{
  #We analyze correlation between variables
  #It should call different methods based on type (ts, panel, c-s)
}
