p_star <- function(v1){
   sapply(v1, function(x){
   if(x > 0.08)return("")
   if(x <= 0.08 &
      x > 0.05) return(".")
   if(x <= 0.05 &
      x > 0.01) return("*")
   if(x <= 0.01 &
      x > 0.001) return("**")
   if(x < 0.001) return("***")
   })
}
