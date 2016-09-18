library(ggplot2)

y = c(2.53692882435, 2.5399940131, 2.54583814375, 2.5401304081, 2.5407035992, 2.54424540395, 
      2.52995588495, 2.53401203737, 2.54027205904, 2.54025181587, 2.53979663427)
x = c(200, 210, 220, 230, 240, 250, 260, 270, 280, 290, 300)

dat = data.frame(x = x, y = y)

p = ggplot(data = dat, aes(x = x, y = y)) + geom_line() + ylab("CV AMS") + xlab("Number of Rounds") + 
  ggtitle("CV AMS vs. nRounds")
p


thresh = c(0.10, 0.11, 0.12, 0.13, 0.14, 0.15, 0.16, 0.17, 0.18, 0.19, 0.20,
           0.21, 0.22, 0.23, 0.24, 0.25)

ams = c(2.37267164454, 2.41993129771, 2.42527749117, 2.4322679368, 2.46847227975,
        2.48003286222, 2.47304423986, 2.47552597318, 2.4610218834, 2.4520400876, 
        2.43748904027, 2.40315906479, 2.37772843471, 2.34891457475, 2.2952776835, 
        2.27824008434)

dat2 = data.frame(x = thresh, y = ams)

threshplot = ggplot(data = dat2, aes(x = x, y = y)) + geom_line() + 
  ylab("CV AMS") + xlab("Threshold") + ggtitle("CV AMS vs. Threshold")
threshplot

summary(stacked_preds_0)

ranks = 1:550000
summary(ranks)
