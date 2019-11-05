library(shiny)
library(ggplot2)
library(ggpmisc)
library(gganimate)

num_simulations <- 25
num_observations <- 10L
num_intercept <- 0
num_slope <- 0.1
num_sd <- 6

x <- rep(seq(from = 0, to = 50, length.out = num_observations), num_simulations)
y <- num_intercept + x * num_slope +
  c(rnorm(n = num_observations * num_simulations, mean = 0, sd = num_sd))
my.data <- data.frame(x, y, replicate = factor(rep(1:num_simulations, times = rep(num_observations, num_simulations))))

p <-
  ggplot(my.data, aes(x, y, colour = replicate)) +
  geom_abline(intercept = as.numeric(num_intercept),
              slope = as.numeric(num_slope),
              linetype = "dashed", color = "blue", size = 1) +
  geom_point() +
  geom_rug() +
  geom_smooth(method = "lm", formula = y ~ x) +
  scale_x_continuous(name = "x, fixed") +
  scale_y_continuous(name = "y, with error") +
  scale_color_discrete(guide = FALSE) +
  expand_limits(y = c(-30, 30)) +
  transition_states(replicate,
                    transition_length = 2,
                    state_length = 2) +
  ggtitle(paste("a = ", round(num_intercept, 1), ", b = ",
                round(num_slope, 1), "; n = ", as.integer(num_observations), "; Data set {closest_state}",
                sep = ""),
          subtitle = 'Frame {frame} of {nframes}') +
  theme_bw(20)

p

anim_save("figures/regression_anim_b01_100.gif")

# if (input$disp_equation4) {
#   p + stat_poly_eq(aes(label =  paste(stat(eq.label), stat(rr.label), sep = "~~~~")),
#                    formula = y ~ x, parse = TRUE, size = 7)
# } else {
#   p + stat_fit_tb(label.x = "left", size = 5)
# }

library(lme4)

num_simulations <- 1000L
num_observations <- 10L
num_intercept <- 0
num_slope <- 0.2
num_sd <- 6

x <- rep(seq(from = 0, to = 50, length.out = num_observations), num_simulations)
y <- num_intercept + x * num_slope +
  c(rnorm(n = num_observations * num_simulations, mean = 0, sd = num_sd))
my.data <- data.frame(x, y, replicate = factor(rep(1:num_simulations, times = rep(num_observations, num_simulations))))

mf_6.ls <- lmList(y ~ x | replicate, data = my.data)

sum(coef(summary(mf_6.ls))[ , 4, "x"] < 0.05) / num_simulations

sum(coef(summary(mf_6.ls))[ , 4, "(Intercept)"] < 0.05) / num_simulations

ggplot(data.frame(a = coef(summary(mf_6.ls))[ , 1, "(Intercept)"]), aes(x = a)) +
  stat_bin() +
  geom_vline(xintercept = num_intercept, colour = "yellow")

ggplot(data.frame(b = coef(summary(mf_6.ls))[ , 1, "x"]), aes(x = b)) +
  stat_bin() +
  geom_vline(xintercept = num_slope, colour = "yellow")

