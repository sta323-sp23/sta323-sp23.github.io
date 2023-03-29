library(tidyverse)

N = 5000
x = runif(N, min = -1, max = 1)
y = runif(N, min = -1, max = 1)

points = data.frame(x, y)  %>%
  mutate(inside = ifelse(x^2 + y^2 < 1, 1, 0))

inside = points$inside

estimate = vector(length = N)
for(n in 1:N) {
  estimate[n] = mean(inside[1:n]) * 4
}

df = data.frame(x = seq(N), 
                y = estimate)

#### SECOND ROUND ####
x = runif(N, min = -5, max = 5)
y = runif(N, min = -5, max = 5)

points = data.frame(x, y)  %>%
  mutate(inside = ifelse(x^2 + y^2 < 1, 1, 0))

inside = points$inside

estimate = vector(length = N)
for(n in 1:N) {
  estimate[n] = mean(inside[1:n]) * 100
}

df2 = data.frame(x = seq(N), 
                y = estimate)

df %>%
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  geom_line(data = df2, aes(x = x, y = y, color = "Big box"))

