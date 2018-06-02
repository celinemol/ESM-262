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

## Generate some example data for your function.
# NOTE: This is going to go into a separate file later... just here for convenience

# Fish prices
fish = as.factor(c("Cod", "Salmon", "Steelhead", "Tuna", "Shark", "Sole"))
price = c(34.89, 26.66, 15.41, 13.84, 58.66, 63.96)
fish_price = as.data.frame(cbind(fish, price))

# Fish catches
location1 = sample(fish, size=10, prob = c(0.2, 0.2, 0.1, 0.1, 0.3, 0.1), replace=T)
# lets create a test case that should have low diversity, by repeating the same thing
location2 = sample(fish, size=10, prob = c(0.4, 0.1, 0.2, 0.1, 0.2, 0.0), replace=T)
location3 = sample(fish, size=10, prob = c(0.1, 0.2, 0.3, 0.0, 0.0, 0.4), replace=T)
fish_catches = as.data.frame(cbind(location1, location2, location3))

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
            names(which.max(summary(as.factor(fish_catches[,i])))))
    mostfrequent[i] = catch
  }
  
  # * total revenue for each location
  location_names = colnames(fish_catches)
  colnames(fish_catches) = rep("fish", ncol(fish_catches))
  #wholetable = left_join(fish_catches, fish_price, type = "full", 
                    #by = "fish")
  
  # * total fisheries revenue sum
  
  
  # * if user requests it graph of revenue by location and total revenue (as text)
  if (showplot) 
    p=ggplot(sm,aes(x=fish,y=frequency, fill=fish))+geom_col()
  else
    p=NULL
  
  return(list(mostfrequent=mostfrequent, plt=p))
  
}



