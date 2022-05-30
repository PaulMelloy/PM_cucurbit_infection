
pred_plot <- function(mod, test_dat){


# create a data.frame with predicted values and reference them to the respective temperature
mod_pred <-
   as.data.table(cbind(test_dat,
                       round(predict(mod,
                                     test_dat,
                                     type = "p"),
                             3)))

# format to long
mod_pred <-
   melt(
      mod_pred,
      id.vars = c("Tm","vpd","exp"),
      measure.vars = c("0", "1", "2", "3"),
      variable.name = "branches",
      value.name = "prob"
   )

mod_pred[,.(prob = mean(prob)), by= .(vpd,Tm,branches)] |>
   ggplot(aes(x = vpd, y = prob, colour = branches))+
   facet_wrap(facets = vars(Tm))+
   geom_line(size = 1)+
   theme_minimal()
}
