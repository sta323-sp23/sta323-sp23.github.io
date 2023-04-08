library(tidyverse)
library(hexSticker)
library(wesanderson)
library(latex2exp)

pal = wes_palette("FantasticFox1")
colors <- c("f" = "black", "g0" = pal[5], "g1" = pal[4])


# surrogate
g = function(x, xn) {
  cos(xn) - (sin(xn) * (x - xn)) +
    (.5 * ((x - xn)^2))
}

nextPoint = function(xn) {
  xn + sin(xn)
}

x0 = 1
x1 = nextPoint(x0)
x2 = nextPoint(x1)


# plot
sta323_plot = ggplot() +
  xlim(0, 2*pi) +
  ylim(-1, 1) +
  geom_function(fun = cos, aes(color = "f"), lwd = 1) +
  geom_function(fun = g, args = list(xn = x0), aes(color = "g0"),
                linetype = 'dashed', lwd = 1) + 
  theme_minimal() +
  # annotate(geom = "point", x = 1, y = cos(1), col = 'steelblue') +
  # annotate(geom = "text", x = 1, y = cos(1)-.1, label = TeX("$x_0$")) +
  # annotate(geom = "point", x = x1, y = g(x1, x0), col = 'steelblue') +
  # annotate(geom = "text", x = x1, y = g(x1, x0)-.1, label = TeX("$x_1$")) +
  geom_function(fun = g, args = list(xn = x1), aes(color = "g1"),
                linetype = 'dashed', lwd = 1) +
  # annotate(geom = "point", x = x1, y = cos(x1), col = 'steelblue') +
  # annotate(geom = "text", x = x1, y = cos(x1)-.1, label = TeX("$x_1$")) +
  # annotate(geom = "point", x = x2, y = g(x2, x1), col = 'steelblue') +
  # annotate(geom = "text", x = x2, y = g(x2, x1)-.1, label = TeX("$x_2$")) +
  scale_color_manual(name="function",
                     values=c(f = "black", g0= pal[5], g1 = pal[4]), labels = c(TeX("$f(x)$"), TeX("$g(x|x_0)$"), TeX("$g(x|x_1)$"))) +
  labs(x = "", y = "") +
  theme(legend.position="none",
        axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), # remove y axis labels
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), # remove grid
                       panel.grid.minor = element_blank()) #+
  # annotate(geom = "text", x = 3.14, y = 0, 
  #          label = TeX("$L(\\theta)$"), size = 4)

mu0 = 0
sd0 = 1

shadeFrom = 1.75
shadeLeft = shadeFrom - 0.0125

max0 = dnorm(0, mean = mu0, sd = sd0)
hmax0 = 0.5*max0
x_fwhm = sqrt(-2*log(hmax0*sqrt(2*pi)))
slope0 = 0.11

dnorm_limit <- function(x) {
  y <- dnorm(x)
  y[x < shadeLeft  |  x > 3] <- NA
  return(y)
}


shift_d_norm = function(x, mean, sd) {
  d = dnorm(mean, sd)
  shift = (2.1 * slope0)
  return(shift + d)
}


textColor = "#2774AE" 
borderColor = "#2774AE" # mustard: "#b58900" # solarized-light-color: "#fdf6e3"
fillColor = "#FFFFFF"#"#002b36"
titleColor  = textColor



sta101plot1 = ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = mu0, sd = sd0), color = textColor) +
  geom_segment(x = shadeFrom, y = 0, xend = shadeFrom, yend = dnorm(shadeFrom, mu0, sd0), color = textColor) +
  stat_function(fun = dnorm_limit, geom = "area", fill = textColor, alpha = 1) +
  # geom_segment(x = x_fwhm, y = hmax0, xend = -1*x_fwhm, yend = hmax0) +
  labs(x = "", y = "") +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL) +
  theme_minimal() +
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank()
  )

sticker(sta323_plot, package="STA 323", p_size=9, s_x=0.92, s_y=.65, s_width=2.5, s_height=1,
        h_fill=fillColor, h_color = "#2774AE", p_color  = titleColor, filename="~/Desktop/test.png")
