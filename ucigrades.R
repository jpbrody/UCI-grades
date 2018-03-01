# excel file of grades requested by Freedom of Information (public records)
# by Austin Walters http://austingwalters.com/foia-requesting-100-universities/

library(readxl)    
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}

library(dplyr)
library(ggplot2)

undergrad <- read_excel_allsheets("uci-undergrad.xls.xlsx")

# grades<-rbind_all(undergrad)
grades<-bind_rows(undergrad)
grades_gpa <- mutate(grades,GPA = (4*GradeACount+3*GradeBCount+2*GradeCCount+GradeDCount)/(GradeACount+GradeBCount+GradeCCount+GradeDCount+GradeFCount))
grades_gpa <- mutate(grades_gpa, Total = GradeACount+GradeBCount+GradeCCount+GradeDCount+GradeFCount)

#GPA by Dept, upper division courses
depts <- grades_gpa %>% filter(ClassType=='LEC',Total>40,`Course number`>99) %>% group_by(DepartmentNameByCourseCode) %>% summarize(median(GPA))

grades_db <- src_sqlite("grades_db.splite3", create= T)
grades_sqlite <- copy_to(grades_db,grades_gpa,temporary=FALSE, indexes = list(c("Instructor1Name","DepartmentNameByCourseCode")))

#some plots
qplot(depts$`median(GPA)`,geom="histogram")
#ggplot(data=depts)+aes

