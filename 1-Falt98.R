sink(file = 'Results-1-Flat98.txt')
num = seq(1, 41)
num
a = c(22.04, 21.99, 22.08, 21.92, 22.10, 21.92, 21.98, 22.21, 21.99, 22.11, 21.94, 22.09, 22.02, 22.05, 22.00, 16.09, 15.94, 15.89, 16.14, 16.16, 15.93, 16.09, 15.95, 15.95, 16.09, 15.89, 16.06, 15.94, 9.98, 10.10, 9.93, 10.10, 9.68, 9.88, 10.02, 10.08, 10.09, 10.12, 10.05, 9.96, 10.31)
a

v1 = rep('1/3 от срока', 15)
v2 = rep('2/3 от срока', 13)
v3 = rep('конец срока', 13)

b = c(v1, v2, v3)
b
b_factor = factor(b)
b_factor

my_frame = data.frame(Number = num, Value = a, Expiration = b_factor)
#содержимое таблицы
my_frame
#размерность
dim(my_frame)
#структура
str(my_frame)
#названия признаков
names(my_frame)
#4 первых строки таблицы
head(my_frame, n = 4L)
my_frame[my_frame$Value > 16.0 & my_frame$Value < 20.0, ] 
my_frame[c(1, 3, 6, 9, 10), ]
my_frame[my_frame$Value == min(my_frame$Value), ]

new = data.frame(Number = 42, Value = 21.77, Expiration = 'конец срока')
my_frame = rbind(my_frame, new)

m = mean(a)
m
s = sd(a)
s

ab = c(my_frame$Value)
New = dnorm(ab, mean = m, sd = s)

cbind(my_frame, New)
my_frame

sink()



flat = read.xlsx(xlsxFile = 'Flat98.xlsx')
#вывод в файл 5 первых строк таблицы
write.table(head(flat, n = 5), file = "Results-1-Flat98.txt", sep = "\t", row.names = FALSE, append = TRUE, quote = FALSE)



#создадим пользовательскую функцию f, которая возвращает номера 4х элементов 
#произвольного вектора, значения которых наиболее близки к среднему 
#значению элементов вектора
f = function(ab) {
  # Вычисляем среднее значение элементов вектора
  mean_var = mean(ab)
  # Рассчитываем абсолютные отклонения каждого элемента от среднего
  deviations = abs(ab - mean_var)
  # Сортируем отклонения и выбираем индексы четырех минимальных отклонений
  closest_indices = order(deviations)[1:4]
  #возвращаем полученное значение - индексы искомых строк
  return(closest_indices)
}

#создадим ветор из данных стоблца livsp
a_livsp = c(flat$livsp)
#применим пользовательскую функцию к 
result = f(a_livsp)
#выводим в файл
print(flat[result, ])
write.table(flat[result, ], file = "Results-1-Flat98.txt", sep = "\t", row.names = FALSE, append = TRUE, quote = FALSE)

a_price = c(flat$price)
filtered_flat <- flat[a_price > 50, ]

result = f(c(filtered_flat$totsp))
print(filtered_flat[result, ])
write.table(filtered_flat[result, ], file = "Results-1-Flat98.txt", sep = "\t", row.names = FALSE, append = TRUE, quote = FALSE)

