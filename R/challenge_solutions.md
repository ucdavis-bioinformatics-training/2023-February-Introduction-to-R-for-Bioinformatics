---
title: "Challenge Solutions"
author: "UC Davis Bioinformatics Core"
date: "3/16/2021"
output:
  html_document:
    keep_md: TRUE
---



There is more than one way to solve each of these challenges, and not all solutions for each challenge are provided.

### mtcars


```r
# using only functions already seen
mtcars_avg <- colSums(mtcars) / nrow(mtcars)
mtcars2 <- rbind(mtcars, mtcars_avg)
rownames(mtcars2) <- c(rownames(mtcars), "avg")
mtcars2$hp.gt.100 <- mtcars2$hp > 100
mtcars2

# simplified
mtcars3 <- rbind(mtcars, "avg" = colMeans(mtcars))
mtcars3$hp.gt.100 <- mtcars3$hp > 100
mtcars3
```

### iris


```r
table(iris[iris$Sepal.Length > 6, "Species"])

# a data frame with two columns: species and sepal + petal
data.frame(Species = iris$Species,
           Length.Sum = iris$Sepal.Length + iris$Petal.Length)
```

### min max normalization

```r
# make small version of data2 to test formula
data3 <- data2[c(1:3),c(1:3)]
data3
minMaxNormalize <- function(x) {(x - min(x)) / (max(x) - min(x))}
t(apply(data3, 1, minMaxNormalize))
```

### log2-fold change


```r
# add one to all values to avoid taking log of 0 and division by zero
data4 <- t(apply(data3, 1, minMaxNormalize)) + 1
data4
log2FoldChange <- function(a,b) {log2(a / b)}
# for all genes between any two samples
mapply(log2FoldChange, data4[,"C61"], data4[,"C62"])
# for all genes between one sample and all other samples
apply(data4[,-1], 2, function(b) {log2FoldChange(data4[,1], b)})

# or more generally
lfcSample <- function(X, c) {
  lfc = log2FoldChange(X[,c], X[,-c])
  colnames(lfc) = paste("lfc", colnames(X)[c], colnames(lfc), sep = ".")
  lfc}

lfcSample(data4, 1)
# all genes, all sample comparisons
## with a for loop
for (i in 1:ncol(data4)) {
  print(lfcSample(data4, i))
}
## with an apply function
lapply(seq(dim(data4)[2]), function(c) {lfcSample(data4, c)})
```



### Visualization Challenge

```r
# simple scatter plot

plot(gapminder_1982$gdpPercap, gapminder_1982$lifeExp, log="x", xlab="GDP per capita", ylab="Life Expectancy")
```

```r
# scatter plot with color and legend

mycol <- c(Asia = "tomato", Europe = "chocolate4", Africa = "dodgerblue2", 
  Americas = "darkgoldenrod1", Oceania = "green4"
)
plot(gapminder_1982$gdpPercap, gapminder_1982$lifeExp, log="x", col=mycol[gapminder_1982$continent], xlab="GDP per capita", ylab="Life Expectancy")

legend("bottomright", fill=mycol, legend=names(mycol), cex=0.7)
```

```r
# scatter plot with color, scaled point size, and legend

mycol <- c(Asia = "tomato", Europe = "chocolate4", Africa = "dodgerblue2", 
  Americas = "darkgoldenrod1", Oceania = "green4"
)

mycex <- function(x, r){
  x= sqrt(x)
  x_scaled = (x - min(x))/(max(x) - min(x))
  r[1] + x_scaled * (r[2] - r[1])
}

plot(gapminder_1982$gdpPercap, gapminder_1982$lifeExp, log="x", col=mycol[gapminder_1982$continent], cex=mycex(gapminder_1982$pop, c(0.5,10)), xlab="GDP per capita", ylab="Life Expectancy")
legend("bottomright", fill=mycol, legend=names(mycol), cex=0.7)

``` 


