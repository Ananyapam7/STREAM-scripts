library(readr)
# Read the CSV file
data <- read.csv("/home/ananyapam/Projects/STREAM/R_scripts/inter_beex.csv")
bee_x <- data$x
data <- read.csv("/home/ananyapam/Projects/STREAM/R_scripts/inter_beey.csv")
bee_y <- data$x
data <- read.csv("/home/ananyapam/Projects/STREAM/R_scripts/inter_time.csv")
time <- data$x
data <- read.csv("/home/ananyapam/Projects/STREAM/R_scripts/inter_touchx.csv")
touch_x <- data$x
data <- read.csv("/home/ananyapam/Projects/STREAM/R_scripts/inter_touchy.csv")
touch_y <- data$x

plot(bee_x, bee_y)
plot(touch_x, touch_y)
plot(bee_y)
plot(bee_y[52:(250)])

Index <- 1:length(bee_y)
Index <- Index[52:(250)]

start_values <- list(a=300, b=1/50, c=pi, d=200)
model <- nls(bee_y[52:(250)] ~ a*sin(b*Index + c) + d, data = data, start = start_values)
summary(model)

a_fitted <- 166.1
b_fitted <- 0.01587
c_fitted <- -1.677
e_fitted <- 19.81

Y_new <-  a_fitted * sin(b_fitted * Index + c_fitted) + d_fitted * Index + e_fitted

plot(Y_new)
plot(bee_y[52:(250)])
