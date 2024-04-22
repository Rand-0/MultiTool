DateFormat <- function(string)
{
  trials = as.Date(string, optional = T,
                  tryFormats = c("%d-%m-%Y", "%d/%m/%Y", "%d.%m.%Y", "%d %m %Y",
                                 "%d-%m-%y", "%d/%m/%y", "%d.%m.%y", "%d %m %y",
                                 "%d-%b-%Y", "%d/%b/%Y", "%d.%b.%Y", "%d %b %Y", "%d%b%Y",
                                 "%d-%b-%y", "%d/%b/%y", "%d.%b.%y", "%d %b %y", "%d%b%y",
                                 "%d-%B-%Y", "%d/%B/%Y", "%d.%B.%Y", "%d %B %Y", "%d%B%Y",
                                 "%d-%B-%y", "%d/%B/%y", "%d.%B.%y", "%d %B %y", "%d%B%y",
                                 "%m-%d-%Y", "%m/%d/%Y", "%m.%d.%Y", "%m %d %Y",
                                 "%m-%d-%y", "%m/%d/%y", "%m.%d.%y", "%m %d %y",
                                 "%b-%d-%Y", "%b/%d/%Y", "%b.%d.%Y", "%b %d %Y", "%b%d %Y",
                                 "%b-%d-%y", "%b/%d/%y", "%b.%d.%y", "%b %d %y", "%b%d %y",
                                 "%B-%d-%Y", "%B/%d/%Y", "%B.%d.%Y", "%B %d %Y", "%B%d %Y",
                                 "%B-%d-%y", "%B/%d/%y", "%B.%d.%y", "%B %d %y", "%B%d %y",
                                 "%Y-%d-%m", "%Y/%d/%m", "%Y.%d.%m", "%Y %d %m",
                                 "%y-%d-%m", "%y/%d/%m", "%y.%d.%m", "%y %d %m",
                                 "%Y-%d-%b", "%Y/%d/%b", "%Y.%d.%b", "%Y %d %b", "%Y %d%b",
                                 "%y-%d-%b", "%y/%d/%b", "%y.%d.%b", "%y %d %b", "%y %d%b",
                                 "%Y-%d-%B", "%Y/%d/%B", "%Y.%d.%B", "%Y %d %B", "%Y %d%B",
                                 "%y-%d-%B", "%y/%d/%B", "%y.%d.%B", "%y %d %B", "%y %d%B",
                                 "%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d", "%Y %m %d",
                                 "%y-%m-%d", "%y/%m/%d", "%y.%m.%d", "%y %m %d",
                                 "%Y-%b-%d", "%Y/%b/%d", "%Y.%b.%d", "%Y %b %d", "%Y%b%d",
                                 "%y-%b-%d", "%y/%b/%d", "%y.%b.%d", "%y %b %d", "%y%b%d",
                                 "%Y-%B-%d", "%Y/%B/%d", "%Y.%B.%d", "%Y %B %d", "%Y%B%d",
                                 "%y-%B-%d", "%y/%B/%d", "%y.%B.%d", "%y %B %d", "%y%B%d"))
  trials
}

isRealType <- function(vec, type)
{
  check_type = paste0("is.", type)
  if(get(check_type)(vec)) {TRUE} else {FALSE}
}

isNormalDist <- function(vec_raw, vec_kurt = NULL, vec_skew = NULL)
{
  #Test for normality of distrubtion
  #If provided variable with <10 obsevation, we return message
  #For <2000 observation we use Shapiro-Wilk, for rest - Jarque-Bera
  vec_nm = na.omit(vec_raw)
  vec_n = length(vec_nm)

  if(vec_n < 10)
  {
    return("Not available for variables with less than 10 observations!")
  }
  else if(vec_n<= 2000)
  {
    test = shapiro.test(vec_nm)
    method = test$method
    statistic = test$statistic[[1]]
    p_val = test$p.value

  } else
  {
    method = "Jarque-Bera normality test"
    statistic = (((vec_n-1)/6) * (vec_skew^2 + (0.25*(vec_kurt-3)^2)))
    p_val = 2 * dchisq(statistic, 2)
  }
  list(method = method, statistic = statistic, p.value = p_val)
}

areValuesInOrder <- function(vec)
{
  #We check if differences between unique values in given vector are stable
  #It indicates it's most likely a factor variable
  vec_uni = unique(vec)

  #Function is invalid for variables with less than 5 unique values!
  if(length(unique(vec)) < 5) {return(TRUE)}
  #Or when it exceeds 30% length
  if(length(vec_uni) / length(vec) > 0.3) {return(FALSE)}

  #We trim min and max value since it's common for factors to have weird number indicating missings etc
  vec_uni_trim = vec_uni[-c(which.min(vec_uni), which.max(vec_uni))]

  #Sorting vector to calculate differences
  vec_sort = sort(vec_uni_trim)
  vec_diff = diff(vec_sort)

  #TODO: Is this really a valid way to do it?
  if(length(unique(vec_diff)) <= 2) {TRUE} else {FALSE}
}

correctMetaData <- function(meta_data, params)
{
  #we remove types with no variables to change
  params = Filter(function(x) length(x) > 0, params)

  remove_value_from_sublists <- function(main_list, value_to_remove)
  {
    lapply(main_list, function(sublist) {lapply(sublist, function(vec) {vec[vec != value_to_remove]})})
  }

  for(i in names(params))
  {
    for(j in params[[i]])
    {
      if(any(sapply(meta_data$column_types, function(slist) j %in% unlist(slist))))
      {
        meta_data$column_types = remove_value_from_sublists(meta_data$column_types, j)
        meta_data$column_types[[i]][[1]] = append(meta_data$column_types[[i]][[1]], j)
      }
    }
  }
  meta_data
}

createDummies <- function(vec)
{
  if(vec$type != "factor") {stop("Dummies cannot be created for non-factor variables!")}

  vec_factor = as.factor(vec$raw)

  vec_cols = vec$basic$levels
  vec_name = attr(vec, "label")

  vec_new = sapply(vec_cols, function(x) {ifelse(vec_factor == x, 1, 0)})

  result = data.frame(vec_new)
  colnames(result) = sapply(vec_cols, function(x) {paste0(vec_name, "_", x)})

  result
}

