
# Q1. Approximate Median for Grouped Data
age <- c(5, 15, 20, 50, 80, 110)
frequency <- c(200, 450, 300, 1500, 700, 44)

N <- sum(frequency)
cf <- cumsum(frequency)
median_class <- which(cf >= N / 2)[1]

lower <- c(1, 5, 15, 20, 50, 80)[median_class]
f <- frequency[median_class]
cf_prev <- if (median_class == 1) 0 else cf[median_class - 1]
class_width <- age[median_class] - lower

median_value <- lower + ((N / 2 - cf_prev) / f) * class_width
print(median_value)


# Q2. Mean, Median, Mode, Midrange and Quartiles
age <- c(13, 15, 16, 16, 19, 20, 20, 21, 22, 22,
         25, 25, 25, 25, 30, 33, 33, 35, 35, 35,
         35, 36, 40, 45, 46, 52, 70)

print(mean(age))
print(median(age))

mode_age <- as.numeric(names(table(age))[table(age) == max(table(age))])
print(mode_age)

print((min(age) + max(age)) / 2)
print(quantile(age, 0.25))
print(quantile(age, 0.75))


# Q3. Normalization
x <- c(200, 300, 400, 600, 1000)

min_max_norm <- (x - min(x)) / (max(x) - min(x))
print(min_max_norm)

z_score_norm <- (x - mean(x)) / sd(x)
print(z_score_norm)


# Q4. Smoothing by Binning
data <- c(11, 13, 13, 15, 15, 16, 19, 20, 20, 20,
          21, 21, 22, 23, 24, 30, 40, 45, 45, 45,
          71, 72, 73, 75)

# Sort data and divide into 4 equal-frequency bins
data <- sort(data)
bins <- split(data, rep(1:4, each = 6))

# Smoothing by bin mean
mean_smooth <- lapply(bins, function(x) rep(mean(x), length(x)))
print(unlist(mean_smooth))

# Smoothing by bin median
median_smooth <- lapply(bins, function(x) rep(median(x), length(x)))
print(unlist(median_smooth))

# Smoothing by bin boundaries
boundary_smooth <- lapply(bins, function(x) {
  low <- min(x)
  high <- max(x)
  sapply(x, function(v) {
    if (abs(v - low) <= abs(v - high)) low else high
  })
})
print(unlist(boundary_smooth))


# Q5. Age and Body Fat Statistics and Plots
age <- c(23, 23, 27, 27, 39, 41, 47, 49, 50,
         52, 54, 54, 56, 57, 58, 58, 60, 61)

fat <- c(9.5, 26.5, 7.8, 17.8, 31.4, 25.9, 27.4,
         27.2, 31.2, 34.6, 42.5, 28.8, 33.4, 30.2,
         34.1, 32.9, 41.2, 35.7)

print(mean(age))
print(median(age))
print(sd(age))

print(mean(fat))
print(median(fat))
print(sd(fat))

boxplot(age, fat, names = c("Age", "Body Fat"),
        main = "Boxplots of Age and Body Fat")

scatter.smooth(age, fat,
               xlab = "Age", ylab = "Body Fat",
               main = "Age vs Body Fat")

qqplot(age, fat,
       xlab = "Age", ylab = "Body Fat",
       main = "Q-Q Plot: Age vs Body Fat")


# Q6. Normalization of Age Value 35
v <- c(23, 23, 27, 27, 39, 41, 47, 49, 50,
       52, 54, 54, 56, 57, 58, 58, 60, 61)

# (i) Min-max normalization to range [0, 1]
min_max <- (35 - min(v)) / (max(v) - min(v))
print(min_max)

# (ii) Z-score normalization
z_score <- (35 - mean(v)) / 12.94
print(z_score)

# (iii) Decimal scaling
decimal_scaling <- 35 / (10 ^ (floor(log10(abs(35))) + 1))
print(decimal_scaling)


# Q7. Mean, Median and Mode of Pencils
pencils <- c(9, 25, 23, 12, 11, 6, 7, 8, 9, 10)

print(mean(pencils))
print(median(pencils))

mode_pencils <- as.numeric(
  names(table(pencils))[table(pencils) == max(table(pencils))]
)
print(mode_pencils)


# Q8. Scatter Plot of Mobile Phones Sold and Money
x <- c(4, 1, 5, 7, 10, 2, 50, 25, 90, 36)
y <- c(12, 5, 13, 19, 31, 7, 153, 72, 275, 110)

scatter.smooth(x, y,
               xlab = "Mobile Phones Sold",
               ylab = "Money",
               main = "Phones Sold vs Money")


# Q9. Equal-Frequency and Equal-Width Partitioning
marks <- c(55, 60, 71, 63, 55, 65, 50, 55, 58,
           59, 61, 63, 65, 67, 71, 72, 75)

# (a) Equal-frequency partitioning
sorted_marks <- sort(marks)
equal_frequency_bins <- split(
  sorted_marks,
  cut(seq_along(sorted_marks), breaks = 3, labels = FALSE)
)
print(equal_frequency_bins)

hist(marks, breaks = quantile(marks, probs = seq(0, 1, length.out = 4)),
     include.lowest = TRUE,
     main = "Equal-Frequency Partitioning",
     xlab = "Marks", col = "lightblue")

# (b) Equal-width partitioning
equal_width_bins <- cut(marks, breaks = 3, include.lowest = TRUE)
print(split(marks, equal_width_bins))

hist(marks, breaks = 3,
     main = "Equal-Width Partitioning",
     xlab = "Marks", col = "lightgreen")


# Q10. Interquartile Range and Standard Deviation
v <- c(78.3, 81.8, 82, 74.2, 83.4,
       84.5, 82.9, 77.5, 80.9, 70.6)

print(IQR(v))
print(sd(v))


# Q11. First and Third Quartiles
age <- c(13, 15, 16, 16, 19, 20, 20, 21, 22, 22,
         25, 25, 25, 25, 30, 33, 33, 35, 35, 35,
         35, 36, 40, 45, 46, 52, 70)

print(quantile(age, 0.25))
print(quantile(age, 0.75))
