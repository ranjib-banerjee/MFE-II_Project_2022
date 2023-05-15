ugc<-read_excel("D:\\Maths SEM 2 report\\Enrollment\\ug courses.xlsx")
ugc
ugc_long <- tidyr::pivot_longer(ugc, -courses, names_to = "Year", values_to = "Value")
ggplot(ugc_long, aes(x = Year, y = Value, fill = courses)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Value", title = "UGC Dataset") +
  theme_minimal()

pgc<-read_excel("D:\\Maths SEM 2 report\\Enrollment\\pg courses.xlsx")
pgc
pgc_long <- tidyr::pivot_longer(ugc, -courses, names_to = "Year", values_to = "Value")
ggplot(ugc_long, aes(x = Year, y = Value, fill = courses)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Value", title = "PGC Dataset") +
  theme_minimal()
emp<-read_excel("D:\\Maths SEM 2 report\\Employability\\Employablity.xlsx")
emp
emp_long <- tidyr::pivot_longer(emp, -DOMAIN, names_to = "Year", values_to = "Value")
ggplot(emp_long, aes(x = Year, y = Value, fill = DOMAIN)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Value", title = "EMPLOYABILITY") +
  theme_minimal()
pgc <- read_excel("D:\\Maths SEM 2 report\\Enrollment\\Ug courses.xlsx")
pgc_long <- tidyr::pivot_longer(pgc, -courses, names_to = "Year", values_to = "Value")
ggplot(pgc_long, aes(x = Year, y = Value, group = courses, color = courses)) +
  geom_line() +
  labs(x = "Year", y = "Value", title = "UGC Dataset") +
  theme_minimal()
pgc <- read_excel("D:\\Maths SEM 2 report\\Enrollment\\pg courses.xlsx")
pgc_long <- tidyr::pivot_longer(pgc, -courses, names_to = "Year", values_to = "Value")
ggplot(pgc_long, aes(x = Year, y = Value, group = courses, color = courses)) +
  geom_line() +
  labs(x = "Year", y = "Value", title = "PGC Dataset") +
  theme_minimal()
unemployability<-read_excel("D:\\Maths SEM 2 report\\Employability\\Employablity.xlsx")
data_long <- tidyr::pivot_longer(unemployability, -DOMAIN, names_to = "Year", values_to = "Value")
data_long$Year <- as.numeric(data_long$Year)
ggplot(data_long, aes(x = Year, y = Value, color = DOMAIN)) +
  geom_line() +
  labs(x = "Year", y = "Unemployability", title = "Unemployability by Domain") +
  theme_minimal()



