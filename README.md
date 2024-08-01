---
title: "Aggregation methods for correlation"
format: html
editor: visual
execute:
  warning: false
  message: false
---

### The following document shows how aggregating a variable by averaging or summing its' components effect the correlation with another variable.

```{r install-faux}, echo=FALSE}

require(faux)
msg <- function(avg=cor_avg,sum=cor_sum){
  cat(
" By Avg:", avg,"\n",
"By Sum:", sum,"\n
 They are the same."
)
}
```

First let's generate some fake data. Using `faux` we can generate correlated variables.

Since I am using random correlations anyway, in this specific example, It doesn't really matter what is considered as X variable, and what is the Y variable.

```{r generate-data}
set.seed(1)
r <- sample(seq(-1,1,0.001), 6, replace = TRUE)

set.seed(2)
X <- rnorm_multi(n = 1000,varnames = paste0('x',1:3),r = r[1:3])
set.seed(3)
Y <- rnorm_multi(n = 1000,varnames = paste0('y',1:3),r = r[4:6])
```

```{r show, echo=FALSE}
cbind(X,Y)[1:3,]

```

```{r agg-average-summ}

avg_X <- rowMeans(X)
sum_X <- rowSums(X)

avg_Y <- rowMeans(Y)
sum_Y <- rowSums(Y)

```

```{r correlation}
cor_avg <- cor(avg_X,avg_Y)

cor_sum <- cor(sum_X,sum_Y)

msg()
```

## Mathematical solution

Let's define the average and sum of a variable

$\bar{X} = \frac{X_1 + X_2 + ... + X_n}{n}$

$S = X_1 + X_2 + ... + X_n = n\bar{X}$

It's clear now that the Sum is a linear transformation of the average

$S = \bar{X} \cdot n$

And since Pearson correlation is not effected by linear transformation, both methods yield the same correlation.

Here is a reminder of one of the many ways to define the pearson correlation:

$$
r_{XY} = \frac{\sum_{i=1}^N (X_i - \bar{X})(Y_i - \bar{Y})}{N \cdot s_X \cdot s_Y}
$$

There is a standartization to Z-Scores embedded in the formula, thus linear transformation does not effect the correlation!
