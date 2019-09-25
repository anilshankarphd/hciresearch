## my useful functions
sem = function(X) sqrt(var(X, na.rm=TRUE) / (length(X) - sum(is.na(X))))

r.crit = function (df, alpha = 0.05)
{
  t1 = qt(p = alpha, df = df, lower.tail = FALSE)
  r1 = sqrt(t1^2/(t1^2 + df))
  t2 = qt(p = alpha/2, df = df, lower.tail = FALSE)
  r2 = sqrt(t2^2/(t2^2 + df))
  output = c(df, alpha, r1, r2)
  names(output) = c("df", "alpha", "1-tail", "2-tail")
  output = round(output, 4)
  output
}

Cohens.d = function(group1, group2)
{
  group1 = group1[!is.na(group1)]          # strip NAs from group 1
  group2 = group2[!is.na(group2)]          # strip NAs from group 2
  SSE = var(group1)*(length(group1)-1)+var(group2)*(length(group2)-1)
  dfE = length(group1)+length(group2)-2
  MSE = SSE/dfE
  pooled.s = sqrt(MSE)
  diff = abs(mean(group1)-mean(group2))
  t = diff/(pooled.s*sqrt(1/length(group1)+1/length(group2)))
  c(diff=diff,t=t,Cohens.d=abs(mean(group1)-mean(group2))/pooled.s)
}

# # e.g
# data("chickwts")
# casein = chickwts$weight[chickwts$feed=="casein"]
# horsebean = chickwts$weight[chickwts$feed=="horsebean"]
# Cohens.d(horsebean, casein)
