Assignment (Due to April. 30th)
========================================================
200903877 Ȳ �� ��
-------------------------

## Analysis of returns in monthly data

### 2004�� 1������ 2014�� 4�������� APPLE��� HP���� ���� ���ͷ�..

### �������ͷ� �����ʹ� ���Ժ����� ����ص�
### ������ ���ٰ� �˷��� ����. (By Central Limit Theorem)

### �����ڷῡ���� ���ͷ��� Rt=(S(t)/S(t-1))-1 �� ����Ѵ�.
### Taylor�� ���� �ٻ�ġ�� ��Ȯ���� ����..

### ���� ���ͷ��� ���� ��������

```r
par(mfrow = c(1, 2))

apple <- read.csv("C:/Users/student/Desktop/Sung-yoon.R/Final Exam/apple_month.csv", 
    sep = ",", header = T)
x <- apple$Close
n <- length(x)
apple.return <- x[2:n]/x[1:(n - 1)] - 1  # Rt=(St/S(t-1))-1
plot(density(apple.return))
x <- seq(-0.4, 0.4, by = 0.001)
lines(x, dnorm(x, mean(apple.return), sd(apple.return)), col = 2)

hp <- read.csv("C:/Users/student/Desktop/Sung-yoon.R/Final Exam/hp_month.csv", 
    sep = ",", header = T)
x <- hp$Close
n <- length(x)
hp.return <- x[2:n]/x[1:(n - 1)] - 1  # Rt=(St/S(t-1))-1
plot(density(hp.return))
x <- seq(-0.6, 0.4, by = 0.001)
lines(x, dnorm(x, mean(hp.return), sd(hp.return)), col = 2)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 



```r
mean(apple.return)  # Expected return(������) 
```

```
## [1] 0.03327
```

```r
sd(apple.return)  # Volatility 
```

```
## [1] 0.1127
```



```r
mean(hp.return)  # Expected return(������)
```

```
## [1] 0.01811
```

```r
sd(hp.return)  # Volatility
```

```
## [1] 0.1161
```


#### Interpretation
�Ϻ��ڷῡ���� ���ͷ��� ���Ժ����ʹ� �Ÿ��� �־���.
������ �����ڷῡ�� ���� ���ͷ��� ���� ���� �� �׷������� �� �� �ֵ��� ���Ժ����� ������� �´´ٴ� ���� Ȯ���� �� �ִ�.
�߰������� Apple���� �������ͷ��� �������� �� 3%, �������� �� 11%�̸�, HP���� �������ͷ��� �������� �� 2%, �������� �� 12%���� �� �� �ִ�.

### apple�� hp ������ �������

```r
par(mfrow = c(1, 1))
plot(apple.return, hp.return)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-41.png) 

```r
qqnorm(lm(hp.return ~ apple.return)$res)
qqline(lm(hp.return ~ apple.return)$res)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-42.png) 

```r
shapiro.test(lm(hp.return ~ apple.return)$res)
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  lm(hp.return ~ apple.return)$res
## W = 0.9319, p-value = 9.971e-06
```

```r
cor(apple.return, hp.return, method = "spearman")
```

```
## [1] 0.2244
```

```r
cor.test(apple.return, hp.return, method = "spearman")
```

```
## 
## 	Spearman's rank correlation rho
## 
## data:  apple.return and hp.return
## S = 240524, p-value = 0.01272
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##    rho 
## 0.2244
```


#### Interpretation
Apple��� Hp���� �������ͷ����� ������踦 �м��غ��Ҵ�. �̸� ���� ���� �������� �׸� ��� �������� ������ ���� ������谡 ��� ���� �ִ� ������ ��������. �׸��� ����м��� �ϱ� �� �Ϲ����� Pearson�� ����� ����� �� �ִ��� �Ǵ��ϱ� ���� ȸ�ͺм��� �� �� �߻��ϴ� ������ ���Լ��� ����ô�. Q-Q Plot������ �� ������ ������ ���������� ������ �߻��ϸ� ������������� ����Ȯ���� 0.05���� �ſ� �۰� ����Ǵ� ������ ���� �Ϲ����� ȸ�ͺм��� ������ �������� ���ϴ� ������ �Ǵ��ϰ� ������ �̿��ϴ� �������� Spearman�� ����� ����Ͽ���. �� ��� �������� ���� 0.2244264 �̾�����, ������� ����Ȯ���� 0.05���� �����Ƿ� ������谡 �ִٰ� ���� �� �ְڴ�.

## Portfolio effect

### If X1, X2, X3, X4, X5, X6 ~ i.i.d N( mu , (sigma)^2 ) , then,
### Var(X1+X2+X3+X4+X5+X6) 
### = Var(X1)+Var(X2)+Var(X3)+Var(X4)+Var(X5)+Var(X6)
### = 6*(sigma)^2
### -> sd(X1+X2+X3+X4+X5+X6) = sqrt(6)*(sigma)
### ���� ���� ���ͷ��� Volatility�� ���� ���� �����ٿ� ����Ͽ� �����Ѵ�.
### (��, ���� ���ͷ��� ������ ��� ���� ���� �����̶�� �������� ��..)

### ��Ʈ������ �̷��� �̿��ϸ� risk�� �ּ�ȭ�� �� �ִ�.

### ���� ���ͷ� : X (APPLE) ~ N( (3%) , (11%)^2 ) , Y (HP) ~ N( (2%) , (12%)^2 )
### ���࿡ ������ apple�� 1/3, hp�� 2/3 ������ �����Ѵٰ� ����. �׷���,
### (1/3)*X + (2/3)*Y ~ 
### N( (1/3)*(3%) + (2/3)*(2%) ,
### (1/9)*(11%)^2 + (4/9)*(12%)^2 + 2*(1/3)*(2/3)*Cov(X,Y) )

```r
cov(apple.return, hp.return)
```

```
## [1] 0.00274
```

### = N( (2.3%) , (8.8%)^2 )

### R1 ~ N( mu1 , (sigma1)^2 ) , R2 ~ N( mu2 , (sigma2)^2 )
### ��Ʈ������ ���ͷ� : Rp = w*R1 + (1-w)*R2 ( 0 <= w <= 1 )
### Rp ~ N( w*(mu1) + (1-w)*(mu2) , 
### ( w^2 )*( (sigma1)^2 ) + ( (1-w)^2 )*( (sigma2)^2 ) + 2*w*(1-w)*(rho)*(sigma1)*(sigma2) )

### �׷��� ������ rho�� ���� -1�� 1������ ���̹Ƿ�

### mu(p) = w*(mu1) + (1-w)*(mu2)
### sigma(p) = ( w^2 ) * ( (sigma1)^2 ) + ( (1-w)^2 ) * ( (sigma2)^2 ) + 2*w*(1-w)*(rho)*(sigma1)*(sigma2)
### <= ( w^2 ) * ( (sigma1)^2 ) + ( (1-w)^2 ) * ( (sigma2)^2 ) + 2*w*(1-w)*(sigma1)*(sigma2)
### = { w*(sigma1) + (1-w)*(sigma2) }^2 
### ���� �ֽ� risk�� ���� �������ٴ� ���� => ��Ʈ������ ȿ��
### sigma(p)�� w�� ���Ͽ� �̺��ؼ� ���� �ּҷ� �ϴ� w�� ã�� �ȴ�.
### �� ����� w* = ( (sigma2)^2 - (rho)*(sigma1)*(sigma2) )
### / ( (sigma1)^2 + (sigma2)^2 - 2*(rho)*(sigma1)*(sigma2) )
### => ( w* , 1-w* ) : minimun variance portfolio

### Portfolio effect

```r
min.var.portfolio <- function(r1, r2) {
    mu.1 <- mean(r1)
    mu.2 <- mean(r2)
    sigma.1 <- sd(r1)
    sigma.2 <- sd(r2)
    rho <- cor(r1, r2)
    w <- (sigma.2^2 - rho * sigma.1 * sigma.2)/(sigma.1^2 + sigma.2^2 - 2 * 
        rho * sigma.1 * sigma.2)
    mu.p <- w * mu.1 + (1 - w) * mu.2
    sigma.p <- sqrt(w^2 * sigma.1^2 + (1 - w)^2 * sigma.2^2 + 2 * w * (1 - w) * 
        rho * sigma.1 * sigma.2)
    return(list(w = w, mu.p = mu.p, sigma.p = sigma.p, mu.1 = mu.1, mu.2 = mu.2, 
        sigma.1 = sigma.1, sigma.2 = sigma.2, rho = rho))
}

min.var.portfolio(apple.return, hp.return)
```

```
## $w
## [1] 0.5191
## 
## $mu.p
## [1] 0.02598
## 
## $sigma.p
## [1] 0.08894
## 
## $mu.1
## [1] 0.03327
## 
## $mu.2
## [1] 0.01811
## 
## $sigma.1
## [1] 0.1127
## 
## $sigma.2
## [1] 0.1161
## 
## $rho
## [1] 0.2093
```


#### Interpretation
���� �Լ����� ����� ����� ������ Apple���� �ֽĿ� �� 52%�� �����ϴ� ���� ��Ʈ�������� �л��� �ּ�ȭ��Ű�� ������� Ȯ���� �� �ִ�. ��Ʈ������ ȿ���� ���Ͽ� ������ �ֽ��� ���ͷ��� ���������� ��Ʈ�������� �л��� �� ������ �߰������� Ȯ���� �� �ִ�. ������ ��Ʈ�������� �л길 �ּ�ȭ��Ű�� ���� �̻����� ���� �ƴϴ�. �����ڵ� �߿��� ��Ʈ�������� �����͵� ����ϴ� ��쵵 �����Ƿ� �̿� ���� ������ �ؾ��� �ʿ伺�� �ִ�.
