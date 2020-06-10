# Лабораторна робота No 5

```R
read_files <- function(directory, id=1:332) {
  files <- list.files(directory, full.names=T)[id]
  
  data_list <- lapply(files, read.csv)
  
  return (data_list)
}

pmean <- function(directory, pollutant, id=1:332) {
  data_list <- read_files(directory, id)
  data <- do.call(rbind, data_list)

  result <- mean(data[,pollutant], na.rm = TRUE)
  return (result)
}

complete <- function(directory, id) {
  data_list <- read_files(directory, id)
  nobs <- sapply(data_list, function(x) { sum(complete.cases(x)) })

  result <- data.frame(id, nobs)
  return (result)
}

corr <- function(directory, threshold = 0) {
  data_list <- read_files(directory)
  data_list <- lapply(data_list, function(x) { x[complete.cases(x),] })
  
  filtered_list <- Filter(function(x) { nrow(x) > threshold }, data_list)
  
  if (length(filtered_list) == 0) {
    return (c())
  }

  cors <- sapply(filtered_list, function(x) { cor(x[,'sulfate'], x[,'nitrate']) })
  return (cors)
}
```

1. Написати функцію pmean, яка обчислює середнє значення (mean) забруднення сульфатами або нітратами серед заданого переліка моніторів. Ця функція приймає три аргументи: «directory», «pollutant»,
«id». Directory – папка, в якій розміщені дані, pollutant – вид забруднення, id – перелік моніторів. Аргумент id має значення за замовчуванням 1:332. Функція повинна ігнорувати NA значення.

```R
pmean('specdata', 'sulfate', 1:10)
```

```R
[1] 4.064128
```

```R
pmean('specdata', 'sulfate', 55)
```

```R
[1] 3.587319
```

```R
pmean('specdata', 'nitrate')
```

```R
[1] 1.702932
```
2. Написати функцію complete, яка виводить кількість повних спостережень (the number of completely observed cases) для кожного файлу. Функція приймає два аргументи: «Directory» та «id» та повертає data frame, в якому перший стовпчик – ім’я файлу, а другий – кількість повних спостережень.

```R
complete("specdata", 1)
```

```R
  id nobs
1  1  117
```

```R
complete("specdata", c(2, 4, 8, 10, 12))
```

```R
  id nobs
1  2 1041
2  4  474
3  8  192
4 10  148
5 12   96
```

```R
complete("specdata", 50:60)
```

```R
   id nobs
1  50  459
2  51  193
3  52  812
4  53  342
5  54  219
6  55  372
7  56  642
8  57  452
9  58  391
10 59  445
11 60  448
```

3. Написати функцію corr, яка приймає два аргументи: directory (папка, де знаходяться файли спостережень) та threshold (порогове значення, за замовчуванням дорівнює 0) та обчислює кореляцію між сульфатами та нітратами для моніторів, кількість повних спостережень для яких більше порогового значення. Функція повинна повернути вектор значень кореляцій. Якщо ні один монітор не перевищує порогового значення, функція повинна повернути numeric вектор довжиною 0. Для обчислення кореляції між сульфатами та нітратами використовуйте вбудовану функцію «cor» з параметрами за замовчуванням.

```R
cr <- corr('specdata', 150)
head(cr) 
summary(cr)
```

```R
[1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-0.21057 -0.04999  0.09463  0.12525  0.26844  0.76313 
```

```R
cr <- corr('specdata', 400)
head(cr) 
summary(cr)
```

```R
[1] -0.01895754 -0.04389737 -0.06815956 -0.07588814  0.76312884 -0.15782860
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-0.17623 -0.03109  0.10021  0.13969  0.26849  0.76313 
```

```R
cr <- corr('specdata', 5000)
head(cr) 
summary(cr)
length(cr)
```

```R
NULL
Length  Class   Mode 
     0   NULL   NULL 
[1] 0
```