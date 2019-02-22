period5Groups <- c('5_blm1' ,'5_bipart','5_blm2','5_blkent','5_consv','5_mnews1','5_mnews2','5_snews','5_thumor','5_ybt')

period5Names <- c("BLM 1", 
                  "BIPARTISAN + ANONYMOUS", 
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