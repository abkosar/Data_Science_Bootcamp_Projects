raw_train_sample = sample_n(train_original, 1000000)

plot(raw_train_sample$Semana,raw_train_sample$Demanda_uni_equil,
     xlab = "Week", ylab = "Adjusted Demand", main = "Weekly Adjusted Demand")

N <- cor(raw_train_sample)
corrplot(N)




############PRODUCT ID vs. DEMAND FOR EACH WEEK############
week_3 = raw_train_sample[raw_train_sample$Semana == 3]
week_4 = raw_train_sample[raw_train_sample$Semana == 4]
week_5 = raw_train_sample[raw_train_sample$Semana == 5]
week_6 = raw_train_sample[raw_train_sample$Semana == 6]
week_7 = raw_train_sample[raw_train_sample$Semana == 7]
week_8 = raw_train_sample[raw_train_sample$Semana == 8]
week_9 = raw_train_sample[raw_train_sample$Semana == 9]

par(mfrow=c(3,3))

plot(week_3$Producto_ID, week_3$Demanda_uni_equil, 
     xlab = "Product_ID", ylab = "Adjusted Demand",
     main = "Week 3")

plot(week_4$Producto_ID, week_4$Demanda_uni_equil, 
     xlab = "Product_ID", ylab = "Adjusted Demand",
     main = "Week 4")

plot(week_5$Producto_ID, week_5$Demanda_uni_equil, 
     xlab = "Product_ID", ylab = "Adjusted Demand",
     main = "Week 5")

plot(week_6$Producto_ID, week_6$Demanda_uni_equil, 
     xlab = "Product_ID", ylab = "Adjusted Demand",
     main = "Week 6")

plot(week_7$Producto_ID, week_7$Demanda_uni_equil, 
     xlab = "Product_ID", ylab = "Adjusted Demand",
     main = "Week 7")

plot(week_8$Producto_ID, week_8$Demanda_uni_equil, 
     xlab = "Product_ID", ylab = "Adjusted Demand",
     main = "Week 8")

plot(week_9$Producto_ID, week_9$Demanda_uni_equil, 
     xlab = "Product_ID", ylab = "Adjusted Demand",
     main = "Week 9")


############PRODUCT ID vs. SALES FOR EACH WEEK############

par(mfrow=c(3,3))

plot(week_3$Producto_ID, week_3$Venta_hoy, 
     xlab = "Product_ID", ylab = "Sales Week 3",
     main = "Week 3")

plot(week_4$Producto_ID, week_4$Venta_hoy, 
     xlab = "Product_ID", ylab = "Sales Week 4",
     main = "Week 4")

plot(week_5$Producto_ID, week_5$Venta_hoy, 
     xlab = "Product_ID", ylab = "Sales Week 5",
     main = "Week 5")

plot(week_6$Producto_ID, week_6$Venta_hoy, 
     xlab = "Product_ID", ylab = "Sales Week 6",
     main = "Week 6")

plot(week_7$Producto_ID, week_7$Venta_hoy, 
     xlab = "Product_ID", ylab = "Sales Week 7",
     main = "Week 7")

plot(week_8$Producto_ID, week_8$Venta_hoy, 
     xlab = "Product_ID", ylab = "Sales Week 8",
     main = "Week 8")

plot(week_9$Producto_ID, week_9$Venta_hoy, 
     xlab = "Product_ID", ylab = "Sales Week 9",
     main = "Week 9")



























