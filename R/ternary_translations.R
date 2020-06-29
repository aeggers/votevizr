## ternary translations 

simplex.x <- function(x){
  if(sum(x) == 0){return(.5)}
  (x[2] + 0.5*x[3])/sum(x)
}
simplex.y <- function(x){
  if(sum(x) == 0){return(sqrt(.75)*(1/3))}  
  (sqrt(0.75)*x[3])/sum(x)
} 

# these do not normalize. I don't know if I need both. 
simplex.x2 <- function(x){
  x[2] + 0.5*x[3]
}
simplex.y2 <- function(x){
  sqrt(0.75)*x[3]
} 

simplex.xy <- function(coords){
  c(simplex.x(coords), simplex.y(coords))
}