library(tidyverse)
library(ggthemes)

paste0(here(), "/data") %>% setwd()
load("public_info.RData")
paste0(here(), "/output") %>% setwd()

total_payments = sum(public_info$Salary) / 1000000
total_employess = nrow(public_info)

university_analysis_a <- public_info %>%
  group_by(CMP) %>%
  summarize(payments_millions = sum(Salary)/1000000,
            n_employees = table(CMP),
            av_salary_K = (sum(Salary)/1000)/n_employees,
            min_salary_K = min(Salary)/1000,
            median_salary_K = median(Salary)/1000,
            max_salary_K = max(Salary)/1000,
            highest_paid_individual = max(Salary),
            highest_paid_position = max(Salary),
            salary_range = max_salary_K - min_salary_K,
            payments_ratio = (payments_millions/(total_payments) * 100),
            employee_ratio = 100 * n_employees/total_employess) %>%
  arrange(CMP)

university_analysis_b <- public_info %>%
  group_by(CMP) %>%
  filter(Salary == max(Salary)) %>%
  summarize(highest_paid_individual = Name[1],
            highest_paid_position = Title[1]) %>%
  arrange(CMP)

university_analysis <- university_analysis_a
university_analysis$highest_paid_individual <- university_analysis_b$highest_paid_individual
university_analysis$highest_paid_position <- university_analysis_b$highest_paid_position
university_analysis <- university_analysis %>%
  arrange(desc(payments_millions))

rm(university_analysis_a, university_analysis_b)

department_analysis_a <- public_info %>%
  group_by(CMP, Bargaining_Unit) %>%
  summarize(payments_10K = sum(Salary)/10000,
            employees = table(Bargaining_Unit),
            av_salary_K = (sum(Salary)/1000)/employees,
            min_salary_K = min(Salary)/1000,
            median_salary_K = median(Salary)/1000,
            max_salary_K = max(Salary)/1000,
            highest_paid_individual = max(Salary),
            highest_paid_position = max(Salary),
            salary_range = max_salary_K - min_salary_K) %>%
  arrange(CMP, Bargaining_Unit)

department_analysis_b <- public_info %>%
  group_by(CMP, Bargaining_Unit) %>%
  filter(Salary == max(Salary)) %>%
  summarize(highest_paid_individual = Name[1],
            highest_paid_position = Title[1]) %>%
  arrange(CMP, Bargaining_Unit)

department_analysis <- department_analysis_a
department_analysis$highest_paid_individual <- department_analysis_b$highest_paid_individual
department_analysis$highest_paid_position <- department_analysis_b$highest_paid_position
department_analysis <- department_analysis %>%
  arrange(CMP, desc(av_salary_K))

rm(department_analysis_a, department_analysis_b)  

college_analysis_a <- public_info %>%
  group_by(CMP, Department) %>%
  summarize(employees = table(Department),
            av_salary_K = (sum(Salary)/1000)/employees,
            min_salary_K = min(Salary)/1000,
            median_salary_K = median(Salary)/1000,
            max_salary_K = max(Salary)/1000,
            highest_paid_individual = max(Salary),
            highest_paid_position = max(Salary),
            salary_range = max_salary_K - min_salary_K) %>%
  arrange(CMP, Department)

college_analysis_b <- public_info %>%
  group_by(CMP, Department) %>%
  filter(Salary == max(Salary)) %>%
  summarize(highest_paid_individual = Name[1],
            highest_paid_position = Title[1]) %>%
  arrange(CMP, Department)

college_analysis <- college_analysis_a
college_analysis$highest_paid_individual <- college_analysis_b$highest_paid_individual
college_analysis$highest_paid_position <- college_analysis_b$highest_paid_position
college_analysis <- college_analysis %>%
  arrange(CMP, desc(av_salary_K))

rm(college_analysis_a, college_analysis_b)

plot1a <- ggplot(university_analysis, aes(reorder(CMP, -payments_millions), payments_millions)) +
  geom_bar(aes(fill = CMP, color = CMP), stat = "identity") + xlab("University") + 
  ylab("Payments (millions)") +
  scale_color_ptol() +
  scale_fill_ptol() +
  theme_linedraw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

plot1b <- ggplot(university_analysis, aes(reorder(CMP, -n_employees), n_employees)) +
  geom_bar(aes(fill = CMP, color = CMP), stat = "identity") + xlab("University") + 
  ylab("# of employees") +
  scale_color_ptol() +
  scale_fill_ptol() +
  theme_linedraw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("university_payments.png", plot = plot1a, width = 4, height = 5)
ggsave("university_employees.png", plot = plot1b, width = 4, height = 5)

plot2 <- ggplot(department_analysis, 
                aes(reorder(Bargaining_Unit, -payments_10K), 
                    payments_10K, 
                    fill = Bargaining_Unit,
                    color = Bargaining_Unit)) + 
  geom_bar(stat = "identity") + 
  xlab("Department") + 
  ylab("Payments (tens of thousands)") + facet_wrap(~CMP) +
  scale_color_ptol() +
  scale_fill_ptol() +
  theme_linedraw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("department_payments.png", plot = plot2, width = 10, height = 7)

plot3 <- ggplot(department_analysis, 
                aes(reorder(Bargaining_Unit, -employees), 
                    employees, 
                    fill = Bargaining_Unit,
                    color = Bargaining_Unit)) + 
  geom_bar(stat = "identity") + 
  scale_color_ptol() +
  scale_fill_ptol() +
  theme_linedraw() +
  xlab("Department") + 
  ylab("# of employees") + 
  facet_wrap(~CMP) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("employees.png", plot = plot3, width = 10, height = 7)

plot4 <- ggplot(department_analysis, 
                aes(reorder(Bargaining_Unit, -av_salary_K), 
                    av_salary_K, 
                    fill = Bargaining_Unit,
                    color = Bargaining_Unit)) + 
  geom_bar(stat = "identity") + 
  scale_color_ptol() +
  scale_fill_ptol() +
  theme_linedraw() +
  xlab("Department") + 
  ylab("average salary (in thousands)") + 
  facet_wrap(~CMP) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("average salaries.png", plot = plot4, path = main.dir, width = 10, height = 7)

rm(plot1a, plot1b, plot2, plot3, plot4)
