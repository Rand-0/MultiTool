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
