ks.test_auto <- function(x) {
  #Determine parameters of distribution
  m <- mean(x)
  s <- sd(x)
  #Test
  ans <- ks.test(x, pnorm, m, s)
  c(ans[[1]], ans[[2]])
}

qqplot.data <- function (vec) # argument: vector of numbers
{
  # following four lines from base R's qqline()
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  c(slope = unname(slope), int = unname(int))
}

# autoplot.lm <- function(model, ..., which=c(1:3, 5), mfrow=c(1,1)){
#   # Modified after http://librestats.com/2012/06/11/autoplot-graphical-methods-with-ggplot2/
#   # This program is released under the GNU GPL >=2 license.
#   require(grid)
#   df <- fortify(model)
#   df <- cbind(df, rows=1:nrow(df))
#   
#   # residuals vs fitted
#   g1 <- ggplot(df, aes(.fitted, .resid)) +
#     geom_point()  +
#     geom_smooth(se=FALSE) +
#     geom_hline(linetype=2, size=.2, yintercept = 0) +
#     scale_x_continuous("Fitted Values") +
#     scale_y_continuous("Residual") +
#     labs(title="Residuals vs Fitted")
#   
#   # normal qq
#   y <- quantile(df$.resid[!is.na(df$.resid)], c(0.25, 0.75))
#   x <- qnorm(c(0.25, 0.75))
#   slope <- diff(y)/diff(x)
#   int <- y[1L] - slope * x[1L]
#   g2 <- ggplot(df, aes(sample=.resid)) +
#     stat_qq() +
#     geom_abline(slope=slope, intercept=int) +
#     scale_x_continuous("Theoretical Quantiles") +
#     scale_y_continuous("Standardized Residuals") +
#     labs(title="Normal Q-Q")
#   
#   # scale-location
#   g3 <- ggplot(df, aes(.fitted, sqrt(abs(.stdresid)))) +
#     geom_point() +
#     geom_smooth(se=FALSE) +
#     scale_x_continuous("Fitted Values") +
#     scale_y_continuous((expression(sqrt("|Standardized residuals|")))) +
#     labs(title="Scale-Location")
#   
#   # cook's distance
#   g4 <-  ggplot(df, aes(rows, .cooksd, ymin=0, ymax=.cooksd)) +
#     geom_point() + geom_linerange() +
#     scale_x_continuous("Observation Number") +
#     scale_y_continuous("Cook's distance") +
#     labs(title="Cook's Distance")
#   
#   # residuals vs leverage
#   r.hat <- range(df$.hat, na.rm = TRUE)
#   isConstLev <- all(r.hat == 0) ||
#     diff(r.hat) < 1e-10 * mean(df$.hat, na.rm = TRUE)
#   if(isConstLev){
#     fs <- sapply(df, is.factor)
#     fs <- names(df)[fs]
#     df$nf <- stringr::str_wrap(interaction(df[, fs], sep = ":"), width = 10)
# #     sep <- if(any(nchar(levels(df$nf)) > 10)) ":\n" else ":"
# #     df$nf <- interaction(df[, fs], sep = sep)
#     g5 <- ggplot(df, aes(nf , .stdresid)) +
#       geom_point() +
#       geom_smooth(se=FALSE) +
#       geom_hline(linetype=2, size=.2, yintercept = 0) +
#       scale_x_discrete("Factor Level Combination") +
#       scale_y_continuous("Standardized Residuals") +
#       labs(title="Contanst Leverage:\nResiduals vs Factor Levels") +
#       theme(axis.text.x = element_text(angle = 45, hjust =1))
#   }else{
#     g5 <- ggplot(df, aes(.hat, .stdresid)) +
#       geom_point(aes(size = .cooksd)) +
#       geom_smooth(se=FALSE) +
#       geom_hline(linetype=2, size=.2, yintercept = 0) +
#       scale_x_continuous("Leverage") +
#       scale_y_continuous("Standardized Residuals") +
#       labs(title="Residuals vs Leverage")
#   }
#   # cooksd vs leverage
#   
#   g6 <- ggplot(df, aes(.hat, .cooksd)) +
#     geom_point() +
#     geom_smooth(se=FALSE) +
#     geom_abline(slope=seq(0,3,0.5), color="gray", linetype="dashed") +
#     scale_x_continuous("Leverage") +
#     scale_y_continuous("Cook's distance") +
#     labs(title="Cook's dist vs Leverage")
#   
#   plots <- list(g1, g2, g3, g4, g5, g6)
#   
#   # making the plots
#   grid.newpage()
#   
#   if (prod(mfrow)>1) {
#     mypos <- expand.grid(1:mfrow[1], 1:mfrow[2])
#     mypos <- mypos[with(mypos, order(Var1)), ]
#     pushViewport(viewport(layout = grid.layout(mfrow[1], mfrow[2])))
#     formatter <- function(.){}
#   } else {
#     mypos <- data.frame(matrix(1, length(which), 2))
#     pushViewport(viewport(layout = grid.layout(1, 1)))
#     formatter <- function(.) {
#       .dontcare <- readline("Hit <Return> to see next plot: ")
#       grid.newpage()
#     }
#   }
#   
#   j <- 1
#   for (i in which){
#     formatter()
#     print(plots[[i]], vp=viewport(layout.pos.row=mypos[j,][1], layout.pos.col=mypos[j,][2]))
#     j <- j+1
#   }
# }

repeat.before = function(x) {   # repeats the last non NA value. Keeps leading NA
  ind = which(!is.na(x))      # get positions of nonmissing values
  if(is.na(x[1]))             # if it begins with a missing, add the 
    ind = c(1,ind)        # first position to the indices
  rep(x[ind], times = diff(   # repeat the values at these indices
    c(ind, length(x) + 1) )) # diffing the indices + length yields how often 
}

median.test.default <- function(x, na.rm = FALSE){
  great_median <- median(unlist(x), na.rm = na.rm)
  x_bool <- as.data.frame(sapply(x, function(x) x > great_median))
  cont_table <- table(melt(x_bool, measure.vars = 1:ncol(x_bool)))
  chisq.test(cont_table)
}  