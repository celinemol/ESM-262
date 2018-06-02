#' Describe diversity based on a list of species 
#' 
#' Compute a species diversity index
#' @param fish_price a table that has prices for different fish
#' @param fish_catches a table that has the number caught for each fish species for each location 
#' @return list with the following items
#' \describe{
#' \item{mostfrequent}{ Most frequently caught fish in each location}
#' \item{totalrevenue_bylocation}{Total revenue for each location}
#' \item{totalrevenue}{Total fisheries revenue sum}
#' }
#' @examples
#' computediversity(c("butterfly","butterfly","mosquito","butterfly","ladybug","ladybug")))
#' @references
#' http://www.tiem.utk.edu/~gross/bioed/bealsmodules/simpsonDI.html

#* if user requests it graph of revenue by location and total revenue (as text)

## --- Function begins here

fisheries = function(fish_price, fish_catches, showplot = FALSE) {
  
  # data prep
  fish_catches = as.data.frame(fish_catches)
  nloc = ncol(fish_catches)
  
  # * most frequently caught fish in each location
  mostfrequent = list()
  for(i in 1:nloc){
    catch = sprintf("In %s we mostly caught %s", 
            colnames(fish_catches)[i], 
            rownames(fish_catches)[which.max(fish_catches[,i])])
    mostfrequent[i] = catch
  }
  
  # * total revenue for each location
  fish_catches$fish = rownames(fish_catches)
  new_catches = gather(fish_catches, key="location", value="size", -fish)
  new_catches = left_join(new_catches, fish_price, by = "fish")
  
  totalrevloc = new_catches %>% 
    group_by(location) %>% 
    mutate(revenue = price*size) %>% 
    summarize(revenue_totals = sum(revenue))
  
  totalrevenue_bylocation = list()
  for(i in 1:nloc){
    revenuebyloc = sprintf("In %s the total revenue was %s",
                           colnames(fish_catches)[i],
                           totalrevloc$revenue_totals[i])
    totalrevenue_bylocation[i] = revenuebyloc
  }
  
  # * total fisheries revenue sum
  totalrev = sum(totalrevloc$revenue_totals)
  totalrevenue = sprintf("The total fisheries revenue sum is %s", totalrev)
  
  # * if user requests it graph of revenue by location and total revenue (as text)
  if (showplot) {
    p = ggplot(totalrevloc, aes(location, revenue_totals, fill=location)) +
      geom_col() +
      ylab("Revenue ($)") +
      ggtitle(totalrevenue) +
      theme_bw()
  }
  else p=NULL
  
  return(list(mostfrequent, totalrevenue_bylocation, totalrevenue, plt=p))
  
}



