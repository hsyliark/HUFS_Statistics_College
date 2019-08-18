Data Mining Final Project
========================================================
Statmania
-------------------------
200903877 Ȳ �� ��
-------------------------
201103594 �� �� ��
-------------------------

�⸻������ �����Ͽ� �м������ ���� Visualization �ǽ�
-------------------------------------------------------

```r
library(UsingR)
```

```
## Loading required package: MASS
```

```r
library(knitr)
library(lattice)
library(ggplot2)
```

```
## 
## Attaching package: 'ggplot2'
## 
## The following object is masked from 'package:UsingR':
## 
##     movies
```

```r
library(ggmap)
```

```
## 
## Attaching package: 'ggmap'
## 
## The following object is masked from 'package:UsingR':
## 
##     crime
```

```r
final <- read.csv("D:/�����ڷ�/����/(3)Junior/3-2/�����͸��̴׽ǽ�(1����)/Final project/Data for analysis/Data for analysis(final).csv", 
    sep = ",", header = T)
final$year <- as.factor(final$year)
attach(final)
```


### ������ ���� �ʵ��б� ���з��� ������ ������ ����
#### ���з� vs �ҳ�, �ҳ� ������ ��

```r
ggplot(final, aes(household, percent)) + geom_point(width = 1, aes(colour = factor(year))) + 
    facet_grid(. ~ year)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

#### ���з� vs ���ʻ�Ȱ�������� ��

```r
ggplot(final, aes(basic, percent)) + geom_point(aes(colour = factor(year), size = percent), 
    alpha = I(0.7))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

#### ���з� vs ���������� ȯ���� ��

```r
ggplot(final, aes(infect, percent, colour = factor(year))) + geom_line() + geom_point() + 
    facet_grid(. ~ year)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

#### ���з� vs �������¹߻���

```r
qplot(percent, data = final, geom = "histogram", fill = violence, position = "stack") + 
    facet_grid(. ~ year)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

```
## Warning: position_stack requires constant width: output may be incorrect
## Warning: position_stack requires constant width: output may be incorrect
## Warning: position_stack requires constant width: output may be incorrect
## Warning: position_stack requires constant width: output may be incorrect
## Warning: position_stack requires constant width: output may be incorrect
## Warning: position_stack requires constant width: output may be incorrect
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

#### ���з� vs ������

```r
ggplot(final, aes(hospital, percent)) + geom_point(aes(colour = factor(year), 
    size = percent), position = "jitter") + geom_smooth(aes(colour = factor(year)), 
    method = rlm)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

#### ���з� vs �����оƵ���

```r
library(ggthemes)
ggplot(final, aes(noenter, percent, colour = factor(year), size = factor(year))) + 
    geom_point() + theme_economist_white() + scale_colour_economist(stata = TRUE)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

#### Simple interpretation
��ü������ ���� �� �ҳ�, �ҳ� �����, �������¹߻���, �׸��� �����оƵ����� ������ ��, ���ʻ�Ȱ�������� ��, ���������� ȯ�ڼ�, �׸��� �������� ������ �� �ʵ��б� ���з��� �����ϴ� ������ ����. ����Ư���� ����û���� ���з��� ���� �� �����оƵ����� ����߰� ���ʻ�Ȱ�����ڿ� ���ؼ��� ������������ ������ ���õ� ������ �ϱ� ������ �̷��� ����� ���� ������ ������.

### ������ ���� �ʵ��б� ���з�
#### ���з� vs ����(��ġ��)

```r
qmap("seoul", zoom = 11) + geom_point(aes(x = LNG, y = LAT, colour = percent, 
    size = percent), data = final)
```

```
## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=seoul&zoom=11&size=%20640x640&scale=%202&maptype=terrain&sensor=false
## Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=seoul&sensor=false
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

#### Simple interpretation
������ ���� �ʵ��б� ���з��� ������ �����Ѵٰ� �� �� ����.

### ������ ���� �ʵ��б� ���з�
#### ���з� vs ����(2006~2011)

```r
qmap("seoul", zoom = 11) + geom_point(aes(x = LNG, y = LAT, colour = year, size = percent), 
    data = final) + facet_wrap(~year)
```

```
## Using archived map...
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

#### Simple interpretation
������ ���� �ʵ��б� ���з��� ���̰� �ִٴ� ����� �Ѵ��� �ľ��� ����.

### ������ ������ ������踦 �����ִ� �����ĵ�
#### Scatter plot matrix

```r
splom(final[, c(1, 7, 10, 13, 14, 17, 20)], main = "Scatter plot Matrix of 7 variables")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

#### Simple interpretation
���������� ���з�(percent)�� ������ ������ �������� ���̿��� ������ ���谡 ���� ���� ������ ���δ�. �׷����� �ұ��ϰ� ������ ���谡 �ִ� ������ ���̴� 2���� �������� ã�ƺ���� �Ѵٸ� ������(hospital)�� ���������� ȯ�ڼ�(infect)�� �����ϰ� ���� ������ �ʹ�. ȯ���� ���� �����ϸ� �������� �����ϴ� ������ ���δ�.
