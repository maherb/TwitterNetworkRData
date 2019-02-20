period5Names <- c("BIPARTISAN + ANONYMOUS", 
                  "BLM 1", 
                  "BLM 2", 
                  "BLACK ENTERTAINERS",
                  "CONSERVATIVES",
                  "MAINSTREAM NEWS 1", 
                  "MAINSTREAM NEWS 2",
                  "SPANISH/ENG NEWS",
                  "TWITTER HUMOR",
                  "YBT")

# @param period: the period index, as an integer
# @param group: the group index, as an integer

getGroupName <- function(period, group) {
  if (period == 5) {
    return(period5Names[group])
  }
  return(NULL)
  
}