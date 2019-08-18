Final Assignment 2 (�����ڷ�м� �� �ǽ�)
========================================================
200903877 Ȳ �� ��
-------------------------
# 2000����� 2009������� �̱��� 5���� �������� ���� ȿ������ ���� �м�
## ������ 1 : ������ �Ź� ������ (code : 316213 (row 104))
## ������ 2 : ������ �Ź� ������ (code : 316214 (row 105))
## ������ 3 : ����� ���� ������ (code : 316991 (row 107))
## ������ 4 : ��Ź�� ��� ������ (code : 333312 (row 320))
## ������ 5 : �������� ��� ������ (code : 333315 (row 323))

### ������ �ҷ����̱� & ��Ű�� ��ġ

```r
nber2000 <- read.csv("C:/Users/user/Desktop/Sung-yoon.R/Final Exam/Final Assignment 2,3/Final Assignment 2/nber2000.csv", 
    sep = ",", header = T)
nber2001 <- read.csv("C:/Users/user/Desktop/Sung-yoon.R/Final Exam/Final Assignment 2,3/Final Assignment 2/nber2001.csv", 
    sep = ",", header = T)
nber2002 <- read.csv("C:/Users/user/Desktop/Sung-yoon.R/Final Exam/Final Assignment 2,3/Final Assignment 2/nber2002.csv", 
    sep = ",", header = T)
nber2003 <- read.csv("C:/Users/user/Desktop/Sung-yoon.R/Final Exam/Final Assignment 2,3/Final Assignment 2/nber2003.csv", 
    sep = ",", header = T)
nber2004 <- read.csv("C:/Users/user/Desktop/Sung-yoon.R/Final Exam/Final Assignment 2,3/Final Assignment 2/nber2004.csv", 
    sep = ",", header = T)
nber2005 <- read.csv("C:/Users/user/Desktop/Sung-yoon.R/Final Exam/Final Assignment 2,3/Final Assignment 2/nber2005.csv", 
    sep = ",", header = T)
nber2006 <- read.csv("C:/Users/user/Desktop/Sung-yoon.R/Final Exam/Final Assignment 2,3/Final Assignment 2/nber2006.csv", 
    sep = ",", header = T)
nber2007 <- read.csv("C:/Users/user/Desktop/Sung-yoon.R/Final Exam/Final Assignment 2,3/Final Assignment 2/nber2007.csv", 
    sep = ",", header = T)
nber2008 <- read.csv("C:/Users/user/Desktop/Sung-yoon.R/Final Exam/Final Assignment 2,3/Final Assignment 2/nber2008.csv", 
    sep = ",", header = T)
nber2009 <- read.csv("C:/Users/user/Desktop/Sung-yoon.R/Final Exam/Final Assignment 2,3/Final Assignment 2/nber2009.csv", 
    sep = ",", header = T)
install.packages("Benchmarking")
```

```
## Error: trying to use CRAN without setting a mirror
```

```r
library(Benchmarking)
```

```
## Loading required package: lpSolveAPI
## Loading required package: ucminf
```


### �������� ȿ���� �м��� ���� ����� input, output ����
#### output : vadd(Total value added in $1m : �� �ΰ���ġ(column 10))
#### input : emp(Total employment in 1000s : �� ������� ��(column 3)), invest(Total capital expenditure in $1m : �� �ں����� ���(column 11)), cap(Total real capital stock in $1m : ȸ���� �� �ں���(column 14))

## DEA (Data Envelopment Analysis)
### Estimate of frontier function
### frontier function (y=g(x)) -> Y=g(x)-u (u >= 0, inefficiency factor)

### ����̳� ������ǰ�� �� ���� ���� �� ���� ��� ������ ��� ���������� �߿�. 
#### (example) �����/�Ӵ�� = 30 (�Ӵ�ῡ 30�踸ŭ ������ �����ִ�.)
#### --> ����/���� : ���� ȿ���� (productivity or efficiency)
#### ����(������, ��������, �Ҹ�����, �湮���� ..) ,
#### ����(�Ӵ��, ������, ������, ���� ..)
#### �Ӵ�� ��� ������� �ִ밪�� ����..

### DEA ���� ���� ���̴� ���
#### 1. CRS (Constant Returns-to-scale) : convex cone (convexity, free disposability�� ����)
input ��� output�� ���� ȿ�������� ���� ������ ��쿡 �ش��ϴ� ���� ������ �����ϴ� ������ frontier function�� ���������� ����Ѵ�. ���� �ܼ��� ����̴�.
#### 2. VRS (Variable Returns-to-scale) : convex hull (convexity, free disposability�� ����))
�����Ϳ��� ��������� ȿ�����̶�� �������� ��쿡 �ش��ϴ� ������ ���� ������ �����Ͽ� ���������ϰ� ���� �����ϴ� frontier function�� �������� �����.
#### 3. FDH (Free Disposal Hull) : �̻����� ������ ��� (free disposability�� ����)
�־��� �����ͷκ��� ���������� ������ ������ �������� ��� ��Ƴ��� ������ ���Ͽ� ��������� �Լ��� ���� frontier function�� ���������� ����Ѵ�.

### DEA �м������� 2���� ����
#### 1. free disposability : frontier function g(x) �� ���������Լ��̴�.
#### 2. convexity : frontier function g(x) �� ���� ������ �Լ��̴�. �� ������ �����Ϳ� �̻����� �����ϴ� ��쿡�� ������ �� �ִ�.

### �м��ϴ� ���
#### case 1 : ORIENTATION="in"
������ output�� ������ ���� �ּ� input�� ���� ������ ���� input�� ��� ���������� ��Ÿ���� efficiency�� ���� ������ ���Ѵ�. �� ���� ���� ���� input�� ���� �����ϰ� �����ٴ� �ǹ��̹Ƿ� ���꼺�� ���� ���� ������ �Ǵ��ϸ� �ȴ�. �׷��Ƿ�  efficiency�� ���� 1�� ��찡 ���� ȿ������ ���̶�� �� �� �ִ�.
#### case 2 : ORIENTATION="out"
������ input�� ������ ������ �� �ִ� �ִ� output�� ���� ������ ����� output�� �� �������� ��Ÿ���� efficiency�� ���� ������ ���Ѵ�. �� ���� Ŭ ���� output�� ����� ���� �������� ���ߴٴ� �ǹ��̹Ƿ� ���꼺�� ���� ���� ������ �Ǵ��ϸ� �ȴ�. �׷��Ƿ�  efficiency�� ���� 1�� ��찡 ���� ȿ������ ���̶�� �� �� �ִ�.

### ���� �����ϰ� ���忡�� �ٹ��ϴ� �������� ���ڿ� ����Ǵ� ���� ������ ���õ� �����͸� ������ �м��غ����� �ϰڴ�. 
### Make data

```r
par(mfrow = c(1, 1))
X <- c(5, 10, 15, 20, 25, 30, 25, 15)  # Number of staffs (input)
Y <- c(100, 250, 300, 280, 330, 380, 290, 250)  # Number of cups (output)
par(mfrow = c(1, 1))
plot(X, Y)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

### Using CRS

```r
par(mfrow = c(1, 1))
res.in <- dea(X, Y, RTS = "crs", ORIENTATION = "in")
res.out <- dea(X, Y, RTS = "crs", ORIENTATION = "out")
cbind(X, Y, res.in$eff, res.out$eff)  # ���� ������ ���� ������ �޶���.
```

```
##       X   Y             
## [1,]  5 100 0.8000 1.250
## [2,] 10 250 1.0000 1.000
## [3,] 15 300 0.8000 1.250
## [4,] 20 280 0.5600 1.786
## [5,] 25 330 0.5280 1.894
## [6,] 30 380 0.5067 1.974
## [7,] 25 290 0.4640 2.155
## [8,] 15 250 0.6667 1.500
```

```r
dea.plot.frontier(X, Y, RTS = "crs")  # �׸� �׸���
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

### Using VRS

```r
par(mfrow = c(1, 1))
res.in <- dea(X, Y, RTS = "vrs", ORIENTATION = "in")
res.out <- dea(X, Y, RTS = "vrs", ORIENTATION = "out")
cbind(X, Y, res.in$eff, res.out$eff)  # ���� ������ ���� ������ �޶���.
```

```
##       X   Y             
## [1,]  5 100 1.0000 1.000
## [2,] 10 250 1.0000 1.000
## [3,] 15 300 1.0000 1.000
## [4,] 20 280 0.6500 1.167
## [5,] 25 330 0.8250 1.071
## [6,] 30 380 1.0000 1.000
## [7,] 25 290 0.5600 1.218
## [8,] 15 250 0.6667 1.200
```

```r
dea.plot.frontier(X, Y, RTS = "vrs")  # �׸� �׸���
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

### Using FDH

```r
par(mfrow = c(1, 1))
res.in <- dea(X, Y, RTS = "fdh", ORIENTATION = "in")
res.out <- dea(X, Y, RTS = "fdh", ORIENTATION = "out")
cbind(X, Y, res.in$eff, res.out$eff)  # ���� ������ ���� ������ �޶���.
```

```
##       X   Y             
## [1,]  5 100 1.0000 1.000
## [2,] 10 250 1.0000 1.000
## [3,] 15 300 1.0000 1.000
## [4,] 20 280 0.7500 1.071
## [5,] 25 330 1.0000 1.000
## [6,] 30 380 1.0000 1.000
## [7,] 25 290 0.6000 1.138
## [8,] 15 250 0.6667 1.200
```

```r
dea.plot.frontier(X, Y, RTS = "fdh")  # �׸� �׸���
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

#### Interpretation
X�� ������ ���̰� Y�� ����Ǵ� ���� �����̴�. 3���� ����� ��� ���� ��ȿ������ ��찡 (X,Y)=(25,290) �̶�� ����� �ְ� �ִ�. ������ ȿ������ ���� ����� ��� �ٸ���. CRS�� ��� ���� ȿ������ ��찡 (10,250) �� �Ѱ��� ���� �Ϳ� ���� VRS�� (5,100), (10,250), (15,300), (30,380) �� 4����, FDH�� (5,100), (10,250), (15,300), (25,330), (30,380) �� 5�����̴�. �� �����, �׸��� ������ input���� output������ ���� ����� �ٸ��� ���� �� �����Ƿ� �м��ϰ��� �ϴ� �����Ϳ� �����ϴ� ����� � ������ ���� �����غ��� �͵� ���� ���̴�. 

# Input criterion
## �м��� �Կ� �־� ������� input�� output�� ���踦 ��Ÿ���� plot�� ���� ������ ���̴�.

## 2000��
### Graph of 'output vs input'

```r
par(mfrow = c(2, 3))
X <- nber2000[, c(3, 11, 14)]  # input
Y <- nber2000[, c(10)]  # output
plot(X[, 1], Y, main = "emp vs vadd")
plot(X[, 2], Y, main = "invest vs vadd")
plot(X[, 3], Y, main = "cap vs vadd")
# log transformation
X.2000 <- log(nber2000[, c(3, 11, 14)])  # input
Y.2000 <- log(nber2000[, c(10)])  # output
plot(X.2000[, 1], Y.2000, main = "emp vs vadd")
plot(X.2000[, 2], Y.2000, main = "invest vs vadd")
plot(X.2000[, 3], Y.2000, main = "cap vs vadd")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

#### Interpretation
�׸��� ���� �����Ϳ� �̻����� �������� �� �� �ִ�. ���� input�� output�� ���� log ��ȯ�� �ǽ������� �� ��� �̻����� ������� Ȯ���Ͽ���. ��ȯ ���Ŀ� ����� �׸��� ���� VRS ������� ����ϱ�� �����Ͽ���.

### Using VRS

```r
dea.res.2000 <- dea(X.2000, Y.2000, RTS = "vrs", ORIENTATION = "in")
res.2000 <- cbind(dea.res.2000$eff[104], dea.res.2000$eff[105], dea.res.2000$eff[107], 
    dea.res.2000$eff[320], dea.res.2000$eff[323])
rownames(res.2000) <- c("Year2000")
colnames(res.2000) <- c("316213", "316214", "316991", "333312", "333315")
res.2000
```

```
##          316213 316214 316991 333312 333315
## Year2000 0.7983 0.9377 0.8871 0.7615 0.7589
```


## 2001��
### Graph of 'output vs input'

```r
par(mfrow = c(2, 3))
X <- nber2001[, c(3, 11, 14)]  # input
Y <- nber2001[, c(10)]  # output
plot(X[, 1], Y, main = "emp vs vadd")
plot(X[, 2], Y, main = "invest vs vadd")
plot(X[, 3], Y, main = "cap vs vadd")
# log transformation
X.2001 <- log(nber2001[, c(3, 11, 14)])  # input
Y.2001 <- log(nber2001[, c(10)])  # output
plot(X.2001[, 1], Y.2001, main = "emp vs vadd")
plot(X.2001[, 2], Y.2001, main = "invest vs vadd")
plot(X.2001[, 3], Y.2001, main = "cap vs vadd")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

#### Interpretation
�׸��� ���� �����Ϳ� �̻����� �������� �� �� �ִ�. ���� input�� output�� ���� log ��ȯ�� �ǽ������� �� ��� �̻����� ������� Ȯ���Ͽ���. ��ȯ ���Ŀ� ����� �׸��� ���� VRS ������� ����ϱ�� �����Ͽ���.

### Using VRS

```r
dea.res.2001 <- dea(X.2001, Y.2001, RTS = "vrs", ORIENTATION = "in")
res.2001 <- cbind(dea.res.2001$eff[104], dea.res.2001$eff[105], dea.res.2001$eff[107], 
    dea.res.2001$eff[320], dea.res.2001$eff[323])
rownames(res.2001) <- c("Year2001")
colnames(res.2001) <- c("316213", "316214", "316991", "333312", "333315")
res.2001
```

```
##          316213 316214 316991 333312 333315
## Year2001 0.8713      1 0.9737 0.7858  0.732
```


## 2002��
### Graph of 'output vs input'

```r
par(mfrow = c(2, 3))
X <- nber2002[, c(3, 11, 14)]  # input
Y <- nber2002[, c(10)]  # output
plot(X[, 1], Y, main = "emp vs vadd")
plot(X[, 2], Y, main = "invest vs vadd")
plot(X[, 3], Y, main = "cap vs vadd")
# log transformation
X.2002 <- log(nber2002[, c(3, 11, 14)])  # input
Y.2002 <- log(nber2002[, c(10)])  # output
plot(X.2002[, 1], Y.2002, main = "emp vs vadd")
plot(X.2002[, 2], Y.2002, main = "invest vs vadd")
plot(X.2002[, 3], Y.2002, main = "cap vs vadd")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

#### Interpretation
�׸��� ���� �����Ϳ� �̻����� �������� �� �� �ִ�. ���� input�� output�� ���� log ��ȯ�� �ǽ������� �� ��� �̻����� ������� Ȯ���Ͽ���. ��ȯ ���Ŀ� ����� �׸��� ���� VRS ������� ����ϱ�� �����Ͽ���.

### Using VRS

```r
dea.res.2002 <- dea(X.2002, Y.2002, RTS = "vrs", ORIENTATION = "in")
res.2002 <- cbind(dea.res.2002$eff[104], dea.res.2002$eff[105], dea.res.2002$eff[107], 
    dea.res.2002$eff[320], dea.res.2002$eff[323])
rownames(res.2002) <- c("Year2002")
colnames(res.2002) <- c("316213", "316214", "316991", "333312", "333315")
res.2002
```

```
##          316213 316214 316991 333312 333315
## Year2002 0.8163 0.7875      1 0.7928 0.6651
```


## 2003��
### Graph of 'output vs input'

```r
par(mfrow = c(2, 3))
X <- nber2003[, c(3, 11, 14)]  # input
Y <- nber2003[, c(10)]  # output
plot(X[, 1], Y, main = "emp vs vadd")
plot(X[, 2], Y, main = "invest vs vadd")
plot(X[, 3], Y, main = "cap vs vadd")
# log transformation
X.2003 <- log(nber2003[, c(3, 11, 14)])  # input
Y.2003 <- log(nber2003[, c(10)])  # output
plot(X.2003[, 1], Y.2003, main = "emp vs vadd")
plot(X.2003[, 2], Y.2003, main = "invest vs vadd")
plot(X.2003[, 3], Y.2003, main = "cap vs vadd")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

#### Interpretation
�׸��� ���� �����Ϳ� �̻����� �������� �� �� �ִ�. ���� input�� output�� ���� log ��ȯ�� �ǽ������� �� ��� �̻����� ������� Ȯ���Ͽ���. ��ȯ ���Ŀ� ����� �׸��� ���� VRS ������� ����ϱ�� �����Ͽ���.

### Using VRS

```r
dea.res.2003 <- dea(X.2003, Y.2003, RTS = "vrs", ORIENTATION = "in")
res.2003 <- cbind(dea.res.2003$eff[104], dea.res.2003$eff[105], dea.res.2003$eff[107], 
    dea.res.2003$eff[320], dea.res.2003$eff[323])
rownames(res.2003) <- c("Year2003")
colnames(res.2003) <- c("316213", "316214", "316991", "333312", "333315")
res.2003
```

```
##          316213 316214 316991 333312 333315
## Year2003 0.8223 0.7859 0.8458 0.7969 0.7875
```


## 2004��
### Graph of 'output vs input'

```r
par(mfrow = c(2, 3))
X <- nber2004[, c(3, 11, 14)]  # input
Y <- nber2004[, c(10)]  # output
plot(X[, 1], Y, main = "emp vs vadd")
plot(X[, 2], Y, main = "invest vs vadd")
plot(X[, 3], Y, main = "cap vs vadd")
# log transformation
X.2004 <- log(nber2004[, c(3, 11, 14)])  # input
Y.2004 <- log(nber2004[, c(10)])  # output
plot(X.2004[, 1], Y.2004, main = "emp vs vadd")
plot(X.2004[, 2], Y.2004, main = "invest vs vadd")
plot(X.2004[, 3], Y.2004, main = "cap vs vadd")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 

#### Interpretation
�׸��� ���� �����Ϳ� �̻����� �������� �� �� �ִ�. ���� input�� output�� ���� log ��ȯ�� �ǽ������� �� ��� �̻����� ������� Ȯ���Ͽ���. ��ȯ ���Ŀ� ����� �׸��� ���� VRS ������� ����ϱ�� �����Ͽ���.

### Using VRS

```r
dea.res.2004 <- dea(X.2004, Y.2004, RTS = "vrs", ORIENTATION = "in")
res.2004 <- cbind(dea.res.2004$eff[104], dea.res.2004$eff[105], dea.res.2004$eff[107], 
    dea.res.2004$eff[320], dea.res.2004$eff[323])
rownames(res.2004) <- c("Year2004")
colnames(res.2004) <- c("316213", "316214", "316991", "333312", "333315")
res.2004
```

```
##          316213 316214 316991 333312 333315
## Year2004 0.7997  0.752 0.8613   0.81 0.6898
```


## 2005��
### Graph of 'output vs input'

```r
par(mfrow = c(2, 3))
X <- nber2005[, c(3, 11, 14)]  # input
Y <- nber2005[, c(10)]  # output
plot(X[, 1], Y, main = "emp vs vadd")
plot(X[, 2], Y, main = "invest vs vadd")
plot(X[, 3], Y, main = "cap vs vadd")
# log transformation
X.2005 <- log(nber2005[, c(3, 11, 14)])  # input
Y.2005 <- log(nber2005[, c(10)])  # output
plot(X.2005[, 1], Y.2005, main = "emp vs vadd")
plot(X.2005[, 2], Y.2005, main = "invest vs vadd")
plot(X.2005[, 3], Y.2005, main = "cap vs vadd")
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16.png) 

#### Interpretation
�׸��� ���� �����Ϳ� �̻����� �������� �� �� �ִ�. ���� input�� output�� ���� log ��ȯ�� �ǽ������� �� ��� �̻����� ������� Ȯ���Ͽ���. ��ȯ ���Ŀ� ����� �׸��� ���� VRS ������� ����ϱ�� �����Ͽ���.

### Using VRS

```r
dea.res.2005 <- dea(X.2005, Y.2005, RTS = "vrs", ORIENTATION = "in")
res.2005 <- cbind(dea.res.2005$eff[104], dea.res.2005$eff[105], dea.res.2005$eff[107], 
    dea.res.2005$eff[320], dea.res.2005$eff[323])
rownames(res.2005) <- c("Year2005")
colnames(res.2005) <- c("316213", "316214", "316991", "333312", "333315")
res.2005
```

```
##          316213 316214 316991 333312 333315
## Year2005 0.8307 0.9282 0.8665 0.8588 0.7881
```


## 2006��
### Graph of 'output vs input'

```r
par(mfrow = c(2, 3))
X <- nber2006[, c(3, 11, 14)]  # input
Y <- nber2006[, c(10)]  # output
plot(X[, 1], Y, main = "emp vs vadd")
plot(X[, 2], Y, main = "invest vs vadd")
plot(X[, 3], Y, main = "cap vs vadd")
# log transformation
X.2006 <- log(nber2006[, c(3, 11, 14)])  # input
Y.2006 <- log(nber2006[, c(10)])  # output
plot(X.2006[, 1], Y.2006, main = "emp vs vadd")
plot(X.2006[, 2], Y.2006, main = "invest vs vadd")
plot(X.2006[, 3], Y.2006, main = "cap vs vadd")
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18.png) 

#### Interpretation
�׸��� ���� �����Ϳ� �̻����� �������� �� �� �ִ�. ���� input�� output�� ���� log ��ȯ�� �ǽ������� �� ��� �̻����� ������� Ȯ���Ͽ���. ��ȯ ���Ŀ� ����� �׸��� ���� VRS ������� ����ϱ�� �����Ͽ���.

### Using VRS

```r
dea.res.2006 <- dea(X.2006, Y.2006, RTS = "vrs", ORIENTATION = "in")
res.2006 <- cbind(dea.res.2006$eff[104], dea.res.2006$eff[105], dea.res.2006$eff[107], 
    dea.res.2006$eff[320], dea.res.2006$eff[323])
rownames(res.2006) <- c("Year2006")
colnames(res.2006) <- c("316213", "316214", "316991", "333312", "333315")
res.2006
```

```
##          316213 316214 316991 333312 333315
## Year2006 0.7923 0.8632 0.8362 0.8599 0.7361
```


## 2007��
### Graph of 'output vs input'

```r
par(mfrow = c(2, 3))
X <- nber2007[, c(3, 11, 14)]  # input
Y <- nber2007[, c(10)]  # output
plot(X[, 1], Y, main = "emp vs vadd")
plot(X[, 2], Y, main = "invest vs vadd")
plot(X[, 3], Y, main = "cap vs vadd")
# log transformation
X.2007 <- log(nber2007[, c(3, 11, 14)])  # input
Y.2007 <- log(nber2007[, c(10)])  # output
plot(X.2007[, 1], Y.2007, main = "emp vs vadd")
plot(X.2007[, 2], Y.2007, main = "invest vs vadd")
plot(X.2007[, 3], Y.2007, main = "cap vs vadd")
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20.png) 

#### Interpretation
�׸��� ���� �����Ϳ� �̻����� �������� �� �� �ִ�. ���� input�� output�� ���� log ��ȯ�� �ǽ������� �� ��� �̻����� ������� Ȯ���Ͽ���. ��ȯ ���Ŀ� ����� �׸��� ���� VRS ������� ����ϱ�� �����Ͽ���.

### Using VRS

```r
dea.res.2007 <- dea(X.2007, Y.2007, RTS = "vrs", ORIENTATION = "in")
res.2007 <- cbind(dea.res.2007$eff[104], dea.res.2007$eff[105], dea.res.2007$eff[107], 
    dea.res.2007$eff[320], dea.res.2007$eff[323])
rownames(res.2007) <- c("Year2007")
colnames(res.2007) <- c("316213", "316214", "316991", "333312", "333315")
res.2007
```

```
##          316213 316214 316991 333312 333315
## Year2007 0.7488  0.787 0.8107 0.8212 0.7447
```


## 2008��
### Graph of 'output vs input'

```r
par(mfrow = c(2, 3))
X <- nber2008[, c(3, 11, 14)]  # input
Y <- nber2008[, c(10)]  # output
plot(X[, 1], Y, main = "emp vs vadd")
plot(X[, 2], Y, main = "invest vs vadd")
plot(X[, 3], Y, main = "cap vs vadd")
# log transformation
X.2008 <- log(nber2008[, c(3, 11, 14)])  # input
Y.2008 <- log(nber2008[, c(10)])  # output
plot(X.2008[, 1], Y.2008, main = "emp vs vadd")
plot(X.2008[, 2], Y.2008, main = "invest vs vadd")
plot(X.2008[, 3], Y.2008, main = "cap vs vadd")
```

![plot of chunk unnamed-chunk-22](figure/unnamed-chunk-22.png) 

#### Interpretation
�׸��� ���� �����Ϳ� �̻����� �������� �� �� �ִ�. ���� input�� output�� ���� log ��ȯ�� �ǽ������� �� ��� �̻����� ������� Ȯ���Ͽ���. ��ȯ ���Ŀ� ����� �׸��� ���� VRS ������� ����ϱ�� �����Ͽ���.

### Using VRS

```r
dea.res.2008 <- dea(X.2008, Y.2008, RTS = "vrs", ORIENTATION = "in")
res.2008 <- cbind(dea.res.2008$eff[104], dea.res.2008$eff[105], dea.res.2008$eff[107], 
    dea.res.2008$eff[320], dea.res.2008$eff[323])
rownames(res.2008) <- c("Year2008")
colnames(res.2008) <- c("316213", "316214", "316991", "333312", "333315")
res.2008
```

```
##          316213 316214 316991 333312 333315
## Year2008 0.8547 0.9832 0.8379 0.8478 0.8106
```


## 2009��
### Graph of 'output vs input'

```r
par(mfrow = c(2, 3))
X <- nber2009[, c(3, 11, 14)]  # input
Y <- nber2009[, c(10)]  # output
plot(X[, 1], Y, main = "emp vs vadd")
plot(X[, 2], Y, main = "invest vs vadd")
plot(X[, 3], Y, main = "cap vs vadd")
# log transformation
X.2009 <- log(nber2009[, c(3, 11, 14)])  # input
Y.2009 <- log(nber2009[, c(10)])  # output
plot(X.2009[, 1], Y.2009, main = "emp vs vadd")
plot(X.2009[, 2], Y.2009, main = "invest vs vadd")
plot(X.2009[, 3], Y.2009, main = "cap vs vadd")
```

![plot of chunk unnamed-chunk-24](figure/unnamed-chunk-24.png) 

#### Interpretation
�׸��� ���� �����Ϳ� �̻����� �������� �� �� �ִ�. ���� input�� output�� ���� log ��ȯ�� �ǽ������� �� ��� �̻����� ������� Ȯ���Ͽ���. ��ȯ ���Ŀ� ����� �׸��� ���� VRS ������� ����ϱ�� �����Ͽ���.

### Using VRS

```r
dea.res.2009 <- dea(X.2009, Y.2009, RTS = "vrs", ORIENTATION = "in")
res.2009 <- cbind(dea.res.2009$eff[104], dea.res.2009$eff[105], dea.res.2009$eff[107], 
    dea.res.2009$eff[320], dea.res.2009$eff[323])
rownames(res.2009) <- c("Year2009")
colnames(res.2009) <- c("316213", "316214", "316991", "333312", "333315")
res.2009
```

```
##          316213 316214 316991 333312 333315
## Year2009 0.8227 0.8539 0.8232 0.8671 0.7371
```


## Final Result of efficiency

```r
par(mfrow = c(1, 1))
res.fin <- rbind(res.2000, res.2001, res.2002, res.2003, res.2004, res.2005, 
    res.2006, res.2007, res.2008, res.2009)
print(res.fin)
```

```
##          316213 316214 316991 333312 333315
## Year2000 0.7983 0.9377 0.8871 0.7615 0.7589
## Year2001 0.8713 1.0000 0.9737 0.7858 0.7320
## Year2002 0.8163 0.7875 1.0000 0.7928 0.6651
## Year2003 0.8223 0.7859 0.8458 0.7969 0.7875
## Year2004 0.7997 0.7520 0.8613 0.8100 0.6898
## Year2005 0.8307 0.9282 0.8665 0.8588 0.7881
## Year2006 0.7923 0.8632 0.8362 0.8599 0.7361
## Year2007 0.7488 0.7870 0.8107 0.8212 0.7447
## Year2008 0.8547 0.9832 0.8379 0.8478 0.8106
## Year2009 0.8227 0.8539 0.8232 0.8671 0.7371
```

```r
plot(res.fin[, 1], type = "l", main = "Men's footwear manufacturing")
```

![plot of chunk unnamed-chunk-26](figure/unnamed-chunk-261.png) 

```r
plot(res.fin[, 2], type = "l", main = "Women's footwear manufacturing")
```

![plot of chunk unnamed-chunk-26](figure/unnamed-chunk-262.png) 

```r
plot(res.fin[, 3], type = "l", main = "Luggage manufacturing")
```

![plot of chunk unnamed-chunk-26](figure/unnamed-chunk-263.png) 

```r
plot(res.fin[, 4], type = "l", main = "Commercial laundry, drycleaning, and pressing machine manufacturing")
```

![plot of chunk unnamed-chunk-26](figure/unnamed-chunk-264.png) 

```r
plot(res.fin[, 5], type = "l", main = "Photographic and photocopying equipment manufacturing")
```

![plot of chunk unnamed-chunk-26](figure/unnamed-chunk-265.png) 

#### Interpretation
2000����� 2009������� 5���� �������� ���� input ������ ȿ������ ���������� ��Ÿ���ô�. ȿ������ ���� ���� ���� ���Ե� input�� �����ϴٴ� �ǹ��̹Ƿ� ȿ�������� ���� ������ �Ǵ��ϸ� �ȴ�.
�׷����� ������ ������, �׸��� ������ �Ź� �������� 10�⵿�� �߼��� ������ ������ ���δ�. �׷��� ����� ���� �������� ���� efficiency�� ���� �����ϴ� �߼��� �������� �ݴ�� ��Ź�� ��� �������� ���� �����ϴ� �߼��� ������. ���� �������� ��� �������� ���� �ѷ������� ������ ��ü������ ���� ���� ���Գ��� efficiency�� ���� �����ϴ� �߼��� �������� Ȯ���� �� �ִ�. 

# Output criterion
## ���� output�� �������� �ϴ� �м��� �����غ����� �ϰڴ�. ������� input ���ؿ����� �����ϰ� VRS�� ����ϵ��� �ϰڴ�.

### Output ���ؿ����� efficiency�� ���� Ŭ ���� ������ ����� ���ߴٴ� �ǹ��̹Ƿ� ȿ������ ���� ���� ������ �Ǵ��ϸ� �ȴ�.

## 2000��
### Using VRS

```r
dea.res.2000.out <- dea(X.2000, Y.2000, RTS = "vrs", ORIENTATION = "out")
res.2000.out <- cbind(dea.res.2000.out$eff[104], dea.res.2000.out$eff[105], 
    dea.res.2000.out$eff[107], dea.res.2000.out$eff[320], dea.res.2000.out$eff[323])
rownames(res.2000.out) <- c("Year2000")
colnames(res.2000.out) <- c("316213", "316214", "316991", "333312", "333315")
res.2000.out
```

```
##          316213 316214 316991 333312 333315
## Year2000  1.146  1.012  1.091  1.259  1.191
```


## 2001��
### Using VRS

```r
dea.res.2001.out <- dea(X.2001, Y.2001, RTS = "vrs", ORIENTATION = "out")
res.2001.out <- cbind(dea.res.2001.out$eff[104], dea.res.2001.out$eff[105], 
    dea.res.2001.out$eff[107], dea.res.2001.out$eff[320], dea.res.2001.out$eff[323])
rownames(res.2001.out) <- c("Year2001")
colnames(res.2001.out) <- c("316213", "316214", "316991", "333312", "333315")
res.2001.out
```

```
##          316213 316214 316991 333312 333315
## Year2001  1.107      1  1.018  1.237  1.253
```


## 2002��
### Using VRS

```r
dea.res.2002.out <- dea(X.2002, Y.2002, RTS = "vrs", ORIENTATION = "out")
res.2002.out <- cbind(dea.res.2002.out$eff[104], dea.res.2002.out$eff[105], 
    dea.res.2002.out$eff[107], dea.res.2002.out$eff[320], dea.res.2002.out$eff[323])
rownames(res.2002.out) <- c("Year2002")
colnames(res.2002.out) <- c("316213", "316214", "316991", "333312", "333315")
res.2002.out
```

```
##          316213 316214 316991 333312 333315
## Year2002  1.168  1.088      1  1.207  1.324
```


## 2003��
### Using VRS

```r
dea.res.2003.out <- dea(X.2003, Y.2003, RTS = "vrs", ORIENTATION = "out")
res.2003.out <- cbind(dea.res.2003.out$eff[104], dea.res.2003.out$eff[105], 
    dea.res.2003.out$eff[107], dea.res.2003.out$eff[320], dea.res.2003.out$eff[323])
rownames(res.2003.out) <- c("Year2003")
colnames(res.2003.out) <- c("316213", "316214", "316991", "333312", "333315")
res.2003.out
```

```
##          316213 316214 316991 333312 333315
## Year2003  1.181  1.091  1.091  1.224  1.112
```


## 2004��
### Using VRS

```r
dea.res.2004.out <- dea(X.2004, Y.2004, RTS = "vrs", ORIENTATION = "out")
res.2004.out <- cbind(dea.res.2004.out$eff[104], dea.res.2004.out$eff[105], 
    dea.res.2004.out$eff[107], dea.res.2004.out$eff[320], dea.res.2004.out$eff[323])
rownames(res.2004.out) <- c("Year2004")
colnames(res.2004.out) <- c("316213", "316214", "316991", "333312", "333315")
res.2004.out
```

```
##          316213 316214 316991 333312 333315
## Year2004  1.203  1.165  1.112  1.177  1.183
```


## 2005��
### Using VRS

```r
dea.res.2005.out <- dea(X.2005, Y.2005, RTS = "vrs", ORIENTATION = "out")
res.2005.out <- cbind(dea.res.2005.out$eff[104], dea.res.2005.out$eff[105], 
    dea.res.2005.out$eff[107], dea.res.2005.out$eff[320], dea.res.2005.out$eff[323])
rownames(res.2005.out) <- c("Year2005")
colnames(res.2005.out) <- c("316213", "316214", "316991", "333312", "333315")
res.2005.out
```

```
##          316213 316214 316991 333312 333315
## Year2005  1.139  1.052  1.103  1.099  1.117
```


## 2006��
### Using VRS

```r
dea.res.2006.out <- dea(X.2006, Y.2006, RTS = "vrs", ORIENTATION = "out")
res.2006.out <- cbind(dea.res.2006.out$eff[104], dea.res.2006.out$eff[105], 
    dea.res.2006.out$eff[107], dea.res.2006.out$eff[320], dea.res.2006.out$eff[323])
rownames(res.2006.out) <- c("Year2006")
colnames(res.2006.out) <- c("316213", "316214", "316991", "333312", "333315")
res.2006.out
```

```
##          316213 316214 316991 333312 333315
## Year2006  1.179  1.068  1.145  1.113  1.184
```


## 2007��
### Using VRS

```r
dea.res.2007.out <- dea(X.2007, Y.2007, RTS = "vrs", ORIENTATION = "out")
res.2007.out <- cbind(dea.res.2007.out$eff[104], dea.res.2007.out$eff[105], 
    dea.res.2007.out$eff[107], dea.res.2007.out$eff[320], dea.res.2007.out$eff[323])
rownames(res.2007.out) <- c("Year2007")
colnames(res.2007.out) <- c("316213", "316214", "316991", "333312", "333315")
res.2007.out
```

```
##          316213 316214 316991 333312 333315
## Year2007   1.22  1.213  1.162  1.146  1.187
```


## 2008��
### Using VRS

```r
dea.res.2008.out <- dea(X.2008, Y.2008, RTS = "vrs", ORIENTATION = "out")
res.2008.out <- cbind(dea.res.2008.out$eff[104], dea.res.2008.out$eff[105], 
    dea.res.2008.out$eff[107], dea.res.2008.out$eff[320], dea.res.2008.out$eff[323])
rownames(res.2008.out) <- c("Year2008")
colnames(res.2008.out) <- c("316213", "316214", "316991", "333312", "333315")
res.2008.out
```

```
##          316213 316214 316991 333312 333315
## Year2008   1.11  1.011  1.125  1.111  1.096
```


## 2009��
### Using VRS

```r
dea.res.2009.out <- dea(X.2009, Y.2009, RTS = "vrs", ORIENTATION = "out")
res.2009.out <- cbind(dea.res.2009.out$eff[104], dea.res.2009.out$eff[105], 
    dea.res.2009.out$eff[107], dea.res.2009.out$eff[320], dea.res.2009.out$eff[323])
rownames(res.2009.out) <- c("Year2009")
colnames(res.2009.out) <- c("316213", "316214", "316991", "333312", "333315")
res.2009.out
```

```
##          316213 316214 316991 333312 333315
## Year2009  1.167  1.084  1.202  1.129  1.195
```


## Final Result of efficiency

```r
par(mfrow = c(1, 1))
res.fin.out <- rbind(res.2000.out, res.2001.out, res.2002.out, res.2003.out, 
    res.2004.out, res.2005.out, res.2006.out, res.2007.out, res.2008.out, res.2009.out)
print(res.fin.out)
```

```
##          316213 316214 316991 333312 333315
## Year2000  1.146  1.012  1.091  1.259  1.191
## Year2001  1.107  1.000  1.018  1.237  1.253
## Year2002  1.168  1.088  1.000  1.207  1.324
## Year2003  1.181  1.091  1.091  1.224  1.112
## Year2004  1.203  1.165  1.112  1.177  1.183
## Year2005  1.139  1.052  1.103  1.099  1.117
## Year2006  1.179  1.068  1.145  1.113  1.184
## Year2007  1.220  1.213  1.162  1.146  1.187
## Year2008  1.110  1.011  1.125  1.111  1.096
## Year2009  1.167  1.084  1.202  1.129  1.195
```

```r
plot(res.fin.out[, 1], type = "l", main = "Men's footwear manufacturing")
```

![plot of chunk unnamed-chunk-37](figure/unnamed-chunk-371.png) 

```r
plot(res.fin.out[, 2], type = "l", main = "Women's footwear manufacturing")
```

![plot of chunk unnamed-chunk-37](figure/unnamed-chunk-372.png) 

```r
plot(res.fin.out[, 3], type = "l", main = "Luggage manufacturing")
```

![plot of chunk unnamed-chunk-37](figure/unnamed-chunk-373.png) 

```r
plot(res.fin.out[, 4], type = "l", main = "Commercial laundry, drycleaning, and pressing machine manufacturing")
```

![plot of chunk unnamed-chunk-37](figure/unnamed-chunk-374.png) 

```r
plot(res.fin.out[, 5], type = "l", main = "Photographic and photocopying equipment manufacturing")
```

![plot of chunk unnamed-chunk-37](figure/unnamed-chunk-375.png) 

#### Interpretation
�׷����� ���� output ���ؿ� ���� ����� input�� ���� ����� �̾߱⸦ �ϰ� ������ �� �� �ִ�.
������, �׸��� ������ �Ź� �������� 10�⵿�� �߼��� ������ ������ ��������, ����� ���� �������� ���� efficiency�� ���� �����ϴ� �߼��� �������� �ݴ�� ��Ź�� ��� �������� ���� �����ϴ� �߼��� ������. �׸��� �������� ��� �������� ���� �ѷ������� ������ ��ü������ ���� ���� ���Գ��� efficiency�� ���� �����ϴ� �߼��� �������� Ȯ���� �� �ִ�. 
�׷��Ƿ� ����� ���� �������� ���ؼ��� ȿ������ �������� ������ �Ը��Ͽ� �ǹ��� �ݿ���� ��ġ�� �������� �� ���̸� ������, ������ �Ź� �������� ���ؼ��� �� ȿ�������� ������ �̷���� �� �ֵ��� ������ �ؾ� �� ���̴�. 


