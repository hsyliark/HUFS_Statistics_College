Final Assignment 3 (�����ڷ�м� �� �ǽ�)
========================================================
200903877 Ȳ �� ��
-------------------------

# 2009���� �̱��� 473���� ����� ȿ������ ���� �м� �ǽ�.
## ����� ����� : DEA(Data Envelopment Analysis)���� ���� ���̴� 3���� �����(CRS, VRS, FDH)
## ������ ��ó : The National Bureau of Economic Research (http://www.nber.org/data/nberces5809.html)

### ������ �ҷ����̱� & ��Ű�� ��ġ

```r
nber2009 <- read.csv("C:/Users/user/Desktop/Sung-yoon.R/Final Exam/Final Assignment 2,3/Final Assignment 3/nber2009.csv", 
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
#### input : emp(Total employment in 1000s : �� ������� ��(column 3)), pay(Total payroll in $1m : ��εǴ� �� �޷�(column 4)), invest(Total capital expenditure in $1m : �� �ں����� ���(column 11)), cap(Total real capital stock in $1m : ȸ���� �� �ں���(column 14))

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

## Analysis of NBER in 2009

### ���� �� �����ҿ� ���õ� ������ ����

```r
X <- nber2009[, c(3, 4, 11, 14)]  # input
Y <- nber2009[, c(10)]  # output
```


#### �տ��� �Ұ��� ������ ������� input�� ������ output������ ������ ����  ȿ������ ���� ������ �� ���̴�.

### Drawing scatter plot

```r
par(mfrow = c(2, 2))
plot(X[, 1], Y, main = "emp vs vadd")
plot(X[, 2], Y, main = "pay vs vadd")
plot(X[, 3], Y, main = "invest vs vadd")
plot(X[, 4], Y, main = "cap vs vadd")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

#### Interpretation
�������� ���� �����Ͱ� ���ִ� ���� �ƴ϶� ����� �ִ� �̻������� ���� �����Ѵٴ� ���� �� �� �ִ�. ���� �� �����Ϳ� log ��ȯ�� �ǽ��Ͽ� �м��� �����ϰ� �ϵ��� �ϰڴ�.

### log transformation

```r
Xt <- log(nber2009[, c(3, 4, 11, 14)])  # input
Yt <- log(nber2009[, c(10)])  # output
par(mfrow = c(2, 2))
plot(Xt[, 1], Yt, main = "emp vs vadd")
plot(Xt[, 2], Yt, main = "pay vs vadd")
plot(Xt[, 3], Yt, main = "invest vs vadd")
plot(Xt[, 4], Yt, main = "cap vs vadd")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

#### Interpretation
log ��ȯ�� �ǽ��� ���� �������� �ٽ� �׷��ô�. �� ��� ����� �ִ� �����͵��� ������� ���̰� �� �� �־����� ������ �� ��� �� �����Ϳ��� VRS ������� ���� �������� ������ ������ ����. ���� CRS, VRS, �׸��� FDH �� 3���� ����е��� �����Ͽ� ����� ȿ������ �м��غ����� �ϰڴ�.

### Using CRS (input)

```r
res.crs.in <- dea(Xt, Yt, RTS = "crs", ORIENTATION = "in")
res.crs.in.1 <- cbind(Xt, Yt, res.crs.in$eff)
min(res.crs.in$eff)
```

```
## [1] 0.7065
```

```r
nber2009[which(res.crs.in$eff == min(res.crs.in$eff)), 1]
```

```
## [1] 334613
```

```r
res.crs.in.1[which(res.crs.in$eff == min(res.crs.in$eff)), ]
```

```
##       emp   pay invest   cap    Yt res.crs.in$eff
## 384 1.792 6.082  4.631 8.187 6.607         0.7065
```

```r
length(nber2009[which(res.crs.in$eff == max(res.crs.in$eff)), 1])
```

```
## [1] 9
```


### Using CRS (output)

```r
res.crs.out <- dea(Xt, Yt, RTS = "crs", ORIENTATION = "out")
res.crs.out.1 <- cbind(Xt, Yt, res.crs.out$eff)
max(res.crs.out$eff)
```

```
## [1] 1.415
```

```r
nber2009[which(res.crs.out$eff == max(res.crs.out$eff)), 1]
```

```
## [1] 334613
```

```r
res.crs.out.1[which(res.crs.out$eff == max(res.crs.out$eff)), ]
```

```
##       emp   pay invest   cap    Yt res.crs.out$eff
## 384 1.792 6.082  4.631 8.187 6.607           1.415
```

```r
length(nber2009[which(res.crs.out$eff == min(res.crs.out$eff)), 1])
```

```
## [1] 9
```

```r
as.numeric(nber2009[which(res.crs.in$eff == max(res.crs.in$eff)), 1] == nber2009[which(res.crs.out$eff == 
    min(res.crs.out$eff)), 1])
```

```
## [1] 1 1 1 1 1 1 1 1 1
```

#### Interpretation
CRS ������� ���� �ΰ��� ������ ���� �м��� ����� ���Ҵ�. ���� ȿ�������� ���� ����� ���ݳ����� ���õ� ������(Magnetic and Optical Recording Media Manufacturing : code 334613)�̾���. �� ����� ���Ͽ� ����� efficiency�� ���� ���� ������ ������ input�� ���� ������ �� 71% ������ ���� �� ������ output�� ���� ���ݺ��� �� 42% ���� �� �ø� �� �ִٴ� ����̴�. �׸��� Benchmarking �Ҹ��� ������ ����� �� 9������� ����� �ְ� �ִ�. ������ ��� ���� �ΰ��� ������ ����� ��Ȯ�ϰ� ��ġ�ߴ�. ������ �� ������� ���� �ܼ��ϱ� ������ �����Ϳ� ���� ���� ���ɼ��� ����. ���� VRS ������� �����غ���� ����.

### Using VRS (input)

```r
res.vrs.in <- dea(Xt, Yt, RTS = "vrs", ORIENTATION = "in")
res.vrs.in.1 <- cbind(Xt, Yt, res.vrs.in$eff)
min(res.vrs.in$eff)
```

```
## [1] 0.7121
```

```r
nber2009[which(res.vrs.in$eff == min(res.vrs.in$eff)), 1]
```

```
## [1] 334613
```

```r
res.vrs.in.1[which(res.vrs.in$eff == min(res.vrs.in$eff)), ]
```

```
##       emp   pay invest   cap    Yt res.vrs.in$eff
## 384 1.792 6.082  4.631 8.187 6.607         0.7121
```

```r
length(nber2009[which(res.vrs.in$eff == max(res.vrs.in$eff)), 1])
```

```
## [1] 18
```


### Using VRS (output)

```r
res.vrs.out <- dea(Xt, Yt, RTS = "vrs", ORIENTATION = "out")
res.vrs.out.1 <- cbind(Xt, Yt, res.vrs.out$eff)
max(res.vrs.out$eff)
```

```
## [1] 1.403
```

```r
nber2009[which(res.vrs.out$eff == max(res.vrs.out$eff)), 1]
```

```
## [1] 334613
```

```r
res.vrs.out.1[which(res.vrs.out$eff == max(res.vrs.out$eff)), ]
```

```
##       emp   pay invest   cap    Yt res.vrs.out$eff
## 384 1.792 6.082  4.631 8.187 6.607           1.403
```

```r
length(nber2009[which(res.vrs.out$eff == min(res.vrs.out$eff)), 1])
```

```
## [1] 18
```

```r
as.numeric(nber2009[which(res.vrs.in$eff == max(res.vrs.in$eff)), 1] == nber2009[which(res.vrs.out$eff == 
    min(res.vrs.out$eff)), 1])
```

```
##  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
```

#### Interpretation
VRS ������� ��쵵 CRS�� ���������� �ΰ��� ������ ���� �м��� ����� �������� ���� ȿ�������� ���� ����� ���ݳ����� ���õ� ������(Magnetic and Optical Recording Media Manufacturing : code 334613)�̾���. �� ����� ���Ͽ� ����� efficiency�� ���� ���� ������ ������ input�� ���� ������ �� 71% ������ ���� �� ������ output�� ���� ���ݺ��� �� 40% ���� �� �ø� �� �ִٴ� ����̴�. �׸��� CRS�ʹ� �ٸ��� VRS������ Benchmarking �Ҹ��� ������ ����� �� 18������� ����� �ְ� �ִ�. ������ ��� ���� �ΰ��� ������ ����� ��Ȯ�ϰ� ��ġ�ߴ�. ���������� FDH ������� �����Ͽ� �м��غ���� ����.

### Using FDH (input)

```r
res.fdh.in <- dea(Xt, Yt, RTS = "fdh", ORIENTATION = "in")
res.fdh.in.1 <- cbind(Xt, Yt, res.fdh.in$eff)
min(res.fdh.in$eff)
```

```
## [1] 0.7929
```

```r
nber2009[which(res.fdh.in$eff == min(res.fdh.in$eff)), 1]
```

```
## [1] 325411
```

```r
res.fdh.in.1[which(res.fdh.in$eff == min(res.fdh.in$eff)), ]
```

```
##       emp   pay invest   cap    Yt res.fdh.in$eff
## 181 3.281 7.573  6.529 9.149 8.835         0.7929
```

```r
length(nber2009[which(res.fdh.in$eff == max(res.fdh.in$eff)), 1])
```

```
## [1] 1
```

```r
nber2009[which(res.fdh.in$eff == max(res.fdh.in$eff)), 1]
```

```
## [1] 323110
```

#### Interpretation
CRS�� VRS�ʹ� �ٸ��� FDH ����п����� �ΰ��� ������ ���� ����� �ٸ��� ��Ÿ����. input�� �������� �� �� ���� ��ȿ������ ����� �Ƿ�� �Ĺ��� ���õ� ������(Medicinal and Botanical Manufacturing : code 325411)�̾���. �� �������� ���Ͽ� ����� efficiency�� ���� ���� ������ ������ input�� ���� ������ �� 79% ������ ���� �� �ִٴ� ����̴�. �׸��� Ư���ϰ� Benchmarking �� ���� ������ ����� �����μ⹰�� ���õ� ���(Commercial Lithographic Printing : code 323110) �ϳ� ���̶�� ����� �ְ� �ִ�.  

### Using FDH (output)

```r
res.fdh.out <- dea(Xt, Yt, RTS = "fdh", ORIENTATION = "out")
res.fdh.out.1 <- cbind(Xt, Yt, res.fdh.out$eff)
max(res.fdh.out$eff)
```

```
## [1] 1.308
```

```r
nber2009[which(res.fdh.out$eff == max(res.fdh.out$eff)), 1]
```

```
## [1] 335991
```

```r
res.fdh.out.1[which(res.fdh.out$eff == max(res.fdh.out$eff)), ]
```

```
##       emp   pay invest   cap    Yt res.fdh.out$eff
## 405 2.054 5.902  5.193 7.401 6.774           1.308
```

```r
length(nber2009[which(res.fdh.out$eff == min(res.fdh.out$eff)), 1])
```

```
## [1] 127
```

#### Interpretation
FDH ����п� ���� output �������� �� �� ���� ��ȿ������ ����� ź�ҿ� ��������� ���õ� ������(Carbon and Graphite Product Manufacturing : code 335991)�̾���. �� �������� ���Ͽ� ����� efficiency�� ���� ���� output�� ���� ���ݺ��� �� 31% ���� �� �ø� �� �ִٴ� ����̴�. �׸��� Benchmarking �� ���� ������ ����� 127������� ����� �ְ� �ִ�.

### Final result
�տ����� �м�������� �� �� �ֵ��� ����п� ���� �м������ ���̰� �� �� �ִ�. �׷��Ƿ� �м��� �ǽ��ϱ� ���� ���� �������� Ư���� ����� ���캸�� ���� ������ ������� ��� ������ ����غ��� �ڼ��� �ʿ��ϴٰ� �����Ѵ�.
