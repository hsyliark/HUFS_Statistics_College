Assignment (Due to March. 26th. 2014)
========================================================
200903877 Ȳ �� ��
-------------------------
5���� ����� 2010�� 1�� 1�Ϻ��� 2014�� 3�� 24�ϱ����� �ֽĿ� ���� ����.
---------------------------------------------------------------------------

### 1. ������ �ҷ����̱� �� ���ͷ��� ���� �׷��� �׸���

#### 1. Samsung eletronics 

```r
dat.1 <- read.csv("C:/Users/user/Desktop/Sung-yoon.R/Mid-term Exam/Samsung eletronics/samsung_daily.csv", 
    sep = ",", header = T)
samsung <- dat.1$Close
par(mfrow = c(1, 2))
plot(samsung, type = "l", main = "Samsung eletronics", xlab = "t", ylab = "Samsung")
samsung.return <- diff(log(samsung))
plot(samsung.return, type = "l", main = "Samsung returns", xlab = "t", ylab = "returns")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

#### Interpretation
�Ｚ���� �ֽĿ� ���� �ð迭 �׷������� �� �� �ֵ��� 
�߹ݺο� �ֽ��� �ް��ϰ� ��������� �� �� �ְ�
������ �ð迭���� �� �� �ִ�.
�׸��� �ֽ��� ���ͷ��� ���� �׷����� ���Ͽ�
�Ϲ����� �ֽ��� Ư¡�̶�� �� �� �ִ�
���ͷ��� �������� ��ȭ���� �� �� ��ȭ�� ����
���ӱⰣ�� ��ü������ ��� ��Ÿ���� ������ �� �� �ִ�.
#### 2. KT corporation

```r
dat.2 <- read.csv("C:/Users/user/Desktop/Sung-yoon.R/Mid-term Exam/KT Corporation/kt_daily.csv", 
    sep = ",", header = T)
kt <- dat.2$Close
par(mfrow = c(1, 2))
plot(kt, type = "l", main = "KT corporation", xlab = "t", ylab = "KT")
kt.return <- diff(log(kt))
plot(kt.return, type = "l", main = "KT returns", xlab = "t", ylab = "returns")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

#### Interpretation
KT corporation �� �ֽĿ� ���� �ð迭 �׷������� �� �� �ֵ��� 
�ֱ� 5�⵿�� �ֽ��� �϶��ϴ� �߼��� ���̰� ������
������ �ð迭���� �� �� �ִ�.
�׸��� �߰��� �ֽ��� ū ������ �������� �κ��� �ִٴ� ���� 
Ư¡�̴�.
���� �ֽ��� ���ͷ��� ���� �׷����� ���Ͽ�
�Ϲ����� �ֽ��� Ư¡�̶�� �� �� �ִ�
���ͷ��� �������� ��ȭ���� �� �� ��ȭ�� ����
���ӱⰣ�� ��ü������ ��� ��Ÿ���� ������ �� �� �ִ�.
#### 3. LG Display Co. Ltd 

```r
dat.3 <- read.csv("C:/Users/user/Desktop/Sung-yoon.R/Mid-term Exam/LG Display Co., Ltd/lg_daily.csv", 
    sep = ",", header = T)
lg <- dat.3$Close
par(mfrow = c(1, 2))
plot(lg, type = "l", main = "LG Display Co. Ltd", xlab = "t", ylab = "LG")
lg.return <- diff(log(lg))
plot(lg.return, type = "l", main = "LG returns", xlab = "t", ylab = "returns")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

#### Interpretation
LG Display �ֽĿ� ���� �ð迭 �׷������� �� �� �ֵ��� 
�߹ݺ� �������� �ְ��� ũ�� �϶��Ͽ��� 
���������� �ֱ� 5�� ���� �ְ��� �϶��ϴ�
�߼��� �������� �� �� �ִ�.
�׸��� �ֽ��� ���ͷ��� ���� �׷����� ����
���ͷ��� �������� ���� ���ӱⰣ��
���������� ��� ��Ÿ���� �ִ� ������ �Ǵܵȴ�.
#### 4. BMW 

```r
dat.4 <- read.csv("C:/Users/user/Desktop/Sung-yoon.R/Mid-term Exam/BMW/BMW_daily.csv", 
    sep = ",", header = T)
bmw <- dat.4$Close
par(mfrow = c(1, 2))
plot(bmw, type = "l", main = "BMW", xlab = "t", ylab = "BMW")
bmw.return <- diff(log(bmw))
plot(bmw.return, type = "l", main = "BMW returns", xlab = "t", ylab = "returns")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

#### Interpretation
BMW �ֽĿ� ���� �ð迭 �׷������� �� �� �ֵ��� 
���ݺ� �������� �ְ��� ũ�� ����Ͽ����� �� �� �ִ�. 
�׸��� �ֽ��� ���ͷ��� ���� �׷����� ����
���ͷ��� �������� ���� ���ӱⰣ��
���������� ��� ��Ÿ���� �ִ� ������ �Ǵܵȴ�.
#### 5. Apple Inc 

```r
dat.5 <- read.csv("C:/Users/user/Desktop/Sung-yoon.R/Mid-term Exam/Apple Inc/apple_daily.csv", 
    sep = ",", header = T)
apple <- dat.5$Close
par(mfrow = c(1, 2))
plot(apple, type = "l", main = "Apple Inc", xlab = "t", ylab = "Apple")
apple.return <- diff(log(apple))
plot(apple.return, type = "l", main = "Apple returns", xlab = "t", ylab = "returns")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

#### Interpretation
Apple Inc �ֽĿ� ���� �ð迭 �׷������� �� �� �ֵ��� 
���������� �ְ��� ����ϴ� �߼��� �������� �� �� ������
�߹ݺο� �Ĺݺ� ���� �߰��� ū ������ ����ߴٴ� �͵� 
Ȯ���� �� �ִ�.
�׸��� �ֽ��� ���ͷ��� ���� �׷����� ����
�Ϲ����� ���ͷ��� ���� ����������
���ͷ��� �������� ���� ���ӱⰣ��
���������� ��� ��Ÿ���� �ִ� ������ �Ǵܵȴ�.

### 2. ���ͷ��� ���� ���, ǥ������, �ֵ�, �׸��� ÷��

#### 1. Samsung eletronics 

```r
z <- scale(samsung.return)
skew.samsung <- mean(z^3)
kurt.samsung <- mean(z^4)
print(c(mean(samsung.return), sd(samsung.return), skew.samsung, kurt.samsung))
```

```
## [1] 0.0004178 0.0176938 0.0788364 4.0592690
```

#### Interpretation
��� : 0.0004178181, ǥ������ : 0.0176937566,
�ֵ� : 0.0788363519 > 0 --> ����� ������ ���� ���������� ������ �����Ǵ� ������ ����,
÷�� : 4.0592690227 > 3 --> �Ϲ����� ���Ժ����� �׷������� �� ������ ������ ������ ������ ���̶�� ������ �� ����.
#### 2. KT corporation

```r
z <- scale(kt.return)
skew.kt <- mean(z^3)
kurt.kt <- mean(z^4)
print(c(mean(kt.return), sd(kt.return), skew.kt, kurt.kt))
```

```
## [1] -0.0002274  0.0165461 -0.1646026  5.5892649
```

#### Interpretation
��� : -0.000227397, ǥ������ : 0.016546067,
�ֵ� : -0.164602632 < 0 --> ������ ������ ���� �������� ������ �����Ǵ� ������ ����,
÷�� : 5.589264940 > 3 --> �Ϲ����� ���Ժ����� �׷������� �� ������ ������ ������ ������ ���̶�� ������ �� ����.
#### 3. LG Display Co. Ltd 

```r
z <- scale(lg.return)
skew.lg <- mean(z^3)
kurt.lg <- mean(z^4)
print(c(mean(lg.return), sd(lg.return), skew.lg, kurt.lg))
```

```
## [1] -0.000383  0.026411 -0.001076  5.560847
```

#### Interpretation
��� : -0.0003830322, ǥ������ : 0.0264105771,
�ֵ� : -0.0010760994 , 0 --> ������ ������ ���� �������� ������ �����Ǵ� ������ ����,
÷�� : 5.5608465389 > 3 --> �Ϲ����� ���Ժ����� ÷���� 3���� ũ�� ������ ������ ����� ������ �����ϰ� �ִ� ������ ���δ�.
#### 4. BMW 

```r
z <- scale(bmw.return)
skew.bmw <- mean(z^3)
kurt.bmw <- mean(z^4)
print(c(mean(bmw.return), sd(bmw.return), skew.bmw, kurt.bmw))
```

```
## [1]  0.0009239  0.0193432 -0.1424242  4.8996305
```

#### Interpretation
��� : 0.0009282477, ǥ������ : 0.0193515181,
�ֵ� : -0.1430340687 < 0 --> ������ ������ ���� �������� ������ �����Ǵ� ������ ����,
÷�� : 4.8958161469 > 3 --> �Ϲ����� ���Ժ����� ÷���� 3���� ũ�� ������ ������ ����� ������ �����ϰ� �ִ� ������ ���δ�.
#### 5. Apple Inc

```r
z <- scale(apple.return)
skew.apple <- mean(z^3)
kurt.apple <- mean(z^4)
print(c(mean(apple.return), sd(apple.return), skew.apple, kurt.apple))
```

```
## [1]  0.0008606  0.0174217 -0.3852795  7.9795077
```

#### Interpretation
��� : 0.0008606177, ǥ������ : 0.0174216501,
�ֵ� : -0.3852795005 , 0 --> ������ ������ ���� �������� ������ �����Ǵ� ������ ����,
÷�� : 7.9795077037 > 3 --> �Ϲ����� ���Ժ����� ÷���� 3���� ũ�� ������ ������ ����� ������ �����ϰ� �ִ� ������ ���δ�.

### 3. ���Լ��� ���� ����

#### 1. Samsung eletronics

```r
par(mfrow = c(1, 1))
hist(samsung.return, prob = T, nclass = 50)
x <- seq(from = -0.05, to = 0.05, by = 1e-04)
lines(x, dnorm(x, mean = mean(samsung.return), sd(samsung.return)), col = 2)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

#### Interpretation
������׷��� ���°� ���Ժ����� ������ �� �� �����ϰ� �����ʿ��� ������ �����ǰ� ������ �� �β����� �� �� �ִ�. �Ϲ������� ���ʿ��� ������ �����Ǵ� ��찡 ���ٴ� ��ǰ� ������ �� Ư���� case��� �� �� �ְڴ�.
#### 2. KT corporation

```r
par(mfrow = c(1, 1))
hist(kt.return, prob = T, nclass = 50)
x <- seq(from = -0.05, to = 0.05, by = 1e-04)
lines(x, dnorm(x, mean = mean(kt.return), sd(kt.return)), col = 2)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

#### Interpretation
������׷��� ���°� ���Ժ����� ������ �� �� �����ϰ� ���ʿ��� ������ �����ǰ� ������ �� �β����� �� �� �ִ�. �̴� ���ͷ��� ������ ���Ժ����ʹ� �Ÿ��� �ִٴ� ���� ���Ѵ�.
#### 3. LG Display Co. Ltd 

```r
par(mfrow = c(1, 1))
hist(lg.return, prob = T, nclass = 50)
x <- seq(from = -0.1, to = 0.15, by = 1e-04)
lines(x, dnorm(x, mean = mean(lg.return), sd(lg.return)), col = 2)
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 

#### Interpretation
������׷��� ���°� ���Ժ����� ������ �� �����ϰ� ���ʿ��� ������ �����ǰ� ������ �� �β����� �� �� �ִ�. �� ����� ���� ���Լ��� �����ϴ� �Ϳ� �־�� �����ؾ� �� ������ ���δ�.
#### 4. BMW 

```r
par(mfrow = c(1, 1))
hist(bmw.return, prob = T, nclass = 50)
x <- seq(from = -0.1, to = 0.05, by = 1e-04)
lines(x, dnorm(x, mean = mean(bmw.return), sd(bmw.return)), col = 2)
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 

#### Interpretation
������׷��� ���°� ���Ժ����� ������ �� �����ϰ� ���ʿ��� ������ �����ǰ� ������ �� �β����� �� �� �ִ�. ���̰� �ִ� ������ ���� ���Լ��� �����ϴ� �Ϳ� �־�� �����ؾ� �� ������ ���δ�.
#### 5. Apple Inc 

```r
par(mfrow = c(1, 1))
hist(apple.return, prob = T, nclass = 50)
x <- seq(from = -0.1, to = 0.05, by = 1e-04)
lines(x, dnorm(x, mean = mean(apple.return), sd(apple.return)), col = 2)
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15.png) 

#### Interpretation
������׷��� ���°� ���Ժ����� ������ �� �����ϰ� ���ʿ��� ������ �����ǰ� ������ �� �β����� �� �� �ִ�. ���̰� �ִ� ������ ���� ���Լ��� �����ϴ� �Ϳ� �־�� ������ ���� ���̶� ��������.

### 4. ���Լ� ����(Shapiro-Wilk normality test)

#### 1. Samsung eletronics

```r
par(mfrow = c(1, 1))
qqnorm(samsung.return)
qqline(samsung.return)
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16.png) 

```r
shapiro.test(samsung.return)
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  samsung.return
## W = 0.9893, p-value = 3.95e-07
```

#### Interpretation
Q-Q plot�� ���ؼ� �� �� �ֵ��� �׷����� ������ ���������� �κ��� �糡���� �߻��ϰ� ������ �� �� �ְ�, ������� ����Ȯ���� 0.05���� ���� ������ ���� �����Ͱ� ���Ժ����� �����ٴ� H0�� �Ⱒ�Ѵ�. ��, ���ͷ��� ���Լ��� ��� �ִٰ� ���� �� ����.
#### 2. KT corporation

```r
par(mfrow = c(1, 1))
qqnorm(kt.return)
qqline(kt.return)
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17.png) 

```r
shapiro.test(kt.return)
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  kt.return
## W = 0.9665, p-value = 6.377e-15
```

#### Interpretation
Q-Q plot�� ���ؼ� �� �� �ֵ��� �׷����� ������ ���������� �κ��� �糡���� �߻��ϰ� ������ �� �� �ְ�, ������� ����Ȯ���� 0.05���� ���� ������ ���� �����Ͱ� ���Ժ����� �����ٴ� H0�� �Ⱒ�Ѵ�. ��, ���ͷ��� ���Լ��� ��� �ִٰ� ���� �� ����.
#### 3. LG Display Co. Ltd 

```r
par(mfrow = c(1, 1))
qqnorm(lg.return)
qqline(lg.return)
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18.png) 

```r
shapiro.test(lg.return)
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  lg.return
## W = 0.9735, p-value = 5.379e-13
```

#### Interpretation
Q-Q plot�� ���ؼ� �� �� �ֵ��� �׷����� ������ ���������� �κ��� �糡���� �߻��ϰ� ������ �� �� �ְ�, ������� ����Ȯ���� 0.05���� �ſ� ���� ������ ���� �����Ͱ� ���Ժ����� �����ٴ� H0�� �Ⱒ�Ѵ�. ��, ���ͷ��� ���Լ��� ��� �ִٰ� ���� �� ����.
#### 4. BMW 

```r
par(mfrow = c(1, 1))
qqnorm(bmw.return)
qqline(bmw.return)
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19.png) 

```r
shapiro.test(bmw.return)
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  bmw.return
## W = 0.9771, p-value = 4.016e-12
```

#### Interpretation
Q-Q plot�� ���ؼ� �� �� �ֵ��� �׷����� ������ ���������� �κ��� �糡���� �߻��ϰ� ������ �� �� �ְ�, ������� ����Ȯ���� 0.05���� �ſ� ���� ������ ���� �����Ͱ� ���Ժ����� �����ٴ� H0�� �Ⱒ�Ѵ�. ��, ���ͷ��� ���Լ��� ��� �ִٰ� ���� �� ����.
#### 5. Apple Inc 

```r
par(mfrow = c(1, 1))
qqnorm(apple.return)
qqline(apple.return)
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20.png) 

```r
shapiro.test(apple.return)
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  apple.return
## W = 0.9576, p-value < 2.2e-16
```

#### Interpretation
Q-Q plot�� ���ؼ� �� �� �ֵ��� �׷����� ������ ���������� �κ��� �糡���� �߻��ϰ� ������ �� �� �ְ�, ������� ����Ȯ���� 0.05���� �ſ� ���� ������ ���� �����Ͱ� ���Ժ����� �����ٴ� H0�� �Ⱒ�Ѵ�. ��, ���ͷ��� ���Լ��� ��� �ִٰ� ���� �� ����.

### 5. �ڱ����Լ� (Autocorrelation function)

#### 1. Samsung eletronics

```r
par(mfrow = c(2, 1))
acf(samsung.return)
acf(abs(samsung.return))
```

![plot of chunk unnamed-chunk-21](figure/unnamed-chunk-21.png) 

#### Interpretation
���ͷ��� ���� ACF�� ���� ��κ� �ŷڱ����� ����� �ʰ� �����Ƿ� �������� ���ͷ��� ������ ���ͷ��� ��ġ�� ������ ���� ������ �� �� �ִ�. ������ ���ͷ��� ���밪�� ���� ACF�� ���� ����� ������ �ŷڱ����� �հ� ������ ��찡 ���� ������ �� �� �ִ�. �׷��Ƿ� ũ���� ������ �������� ���ͷ��� ��ȭ�� ������ ���ͷ��� ��ȭ�� ��� ���� ������� ����ϰ� ������ �� �� �ִ�.
#### 2. KT corporation

```r
par(mfrow = c(2, 1))
acf(kt.return)
acf(abs(kt.return))
```

![plot of chunk unnamed-chunk-22](figure/unnamed-chunk-22.png) 

#### Interpretation
���ͷ��� ���� ACF�� ���� ��κ� �ŷڱ����� ����� �ʰ� �����Ƿ� �������� ���ͷ��� ������ ���ͷ��� ��ġ�� ������ ���� ������ �� �� �ִ�. ���ͷ��� ���밪�� ���� ACF ���� �Ϻ� ������ �����ϸ� ������ ����� �����ִ� ������ ���� �������� ���ͷ��� ��ȭ ���� ������ ���ͷ��� ��ȭ�� ���ٸ� ������ ���� ���ϰ� �ִ� ������ �ؼ��� �� �ְڴ�. ������ �� ������ �м������ ���� ������ �޶��� �� ������ ���Ǹ� �ؾ� �ϰڴ�.
#### 3. LG Display Co. Ltd 

```r
par(mfrow = c(2, 1))
acf(lg.return)
acf(abs(lg.return))
```

![plot of chunk unnamed-chunk-23](figure/unnamed-chunk-23.png) 

#### Interpretation
���ͷ��� ���� ACF�� ���� ��κ� �ŷڱ����� ����� �ʰ� �����Ƿ� �������� ���ͷ��� ������ ���ͷ��� ��ġ�� ������� ũ�ٰ� �� ���� ���ڴ�. �׷��� ���ͷ��� ���밪�� ���� ACF�� ���� �ŷڱ����� �հ� ������ ��찡 ���� �����ϰ� �ִ�. ��, �������� ���ͷ��� ��ȭ�� ������ ���ͷ��� ��ȭ�� ���� ������� ����ϰ� ������ �� �� �ִ�. �׷��Ƿ� ���ͷ� ��ü�� �м��ϴ� �ͺ��� ���ͷ��� ��ȭ�� ���ؼ� �����غ��� ���� �� �ٶ����� ������ ��������.
#### 4. BMW 

```r
par(mfrow = c(2, 1))
acf(bmw.return)
acf(abs(bmw.return))
```

![plot of chunk unnamed-chunk-24](figure/unnamed-chunk-24.png) 

#### Interpretation
���ͷ��� ���� ACF�� ���� ��κ� �ŷڱ����� ����� �ʰ� �����Ƿ� �������� ���ͷ��� ������ ���ͷ��� ��ġ�� ������� ũ�ٰ� �� ���� ���ڴ�. �׷��� ���ͷ��� ���밪�� ���� ACF������ �������� ���ͷ��� ��ȭ�� ������ ���ͷ��� ��ȭ�� ������ ��ġ�� ������ ��Ÿ���� �ִ�. �׷��Ƿ� ���ͷ� ��ü�� �м��ϴ� �ͺ��� ���ͷ��� ��ȭ�� ���ؼ� �����غ��� ���� �� �ٶ����� ������ ��������.
#### 5. Apple Inc 

```r
par(mfrow = c(2, 1))
acf(apple.return)
acf(abs(apple.return))
```

![plot of chunk unnamed-chunk-25](figure/unnamed-chunk-25.png) 

#### Interpretation
���ͷ��� ���� ACF�� ���� ��κ� �ŷڱ����� ����� �ʰ� �����Ƿ� �������� ���ͷ��� ������ ���ͷ��� ��ġ�� ������� ũ�ٰ� �� ���� ���ڴ�. ���ͷ��� ���밪�� ���� ACF ���� �Ϻ� ������ �����ϸ� ������ ����� �����ִ� ������ ���� �������� ���ͷ��� ��ȭ ���� ������ ���ͷ��� ��ȭ�� ���ٸ� ������ ���� ���ϰ� �ִ� ������ �ؼ��� �� �ְڴ�. ������ �� ������ �м������ ���� ������ �޶��� �� ������ ���Ǹ� �ؾ� �ϰڴ�.
