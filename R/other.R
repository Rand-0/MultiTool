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
