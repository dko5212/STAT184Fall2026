# Name: Dhiren Oswal

# Problem 1

student_id <- c("S01", "S02", "S03", "S04", "S05", "S06")
section    <- c("A", "B", "A", "B", "A", "B")
quiz1      <- c(82, 91, 76, 88, 95, 69)
quiz2      <- c(85, 89, 80, 92, 94, 74)
passed     <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)

# Part A

section <- factor(section, levels = c("A", "B"))
students <- data.frame(student_id, section, quiz1, quiz2, passed)
score_matrix <- matrix(
  c(quiz1, quiz2),
  nrow = 6, ncol = 2,
  dimnames = list(student_id, c("quiz1", "quiz2"))
)
course_record <- list(
  course  = "R Programming",
  scores  = students,
  cutoffs = c(pass = 70, excellent = 90)
)

typeof(students);      class(students);      length(students);      dim(students)
typeof(score_matrix);  class(score_matrix);  length(score_matrix);  dim(score_matrix)
typeof(section);       class(section);       length(section)
typeof(course_record); class(course_record); length(course_record)
str(students)
str(score_matrix)
str(section)
str(course_record)

# A vector is one type in 1D; a matrix is a vector with dimensions; a factor is an
# integer vector with labeled levels; a list can mix types/lengths; a data frame is a
# list of equal-length columns that can each hold a different type.

# Part B

score_matrix["S04", "quiz2"]
score_matrix[1:2, , drop = FALSE]
course_record["course"]
course_record[["course"]]
course_record$course

# `[` returns a sub-list (same type as container); `[[` and `$` extract the element
# itself; `$` only works with a literal name, `[[` also works with a variable/index.

# Part C
students$average <- rowMeans(students[, c("quiz1", "quiz2")])
students$excellent <- students$average >= 90
section_A_high <- students[students$section == "A" & students$average >= 80, ]
section_A_high[, c("student_id", "section", "average")]
avg_named <- setNames(students$average, students$student_id)
avg_named

# rowMeans, >=, and & all operate on whole vectors at once, so no loop is needed.


# Problem 2

csv_text <- "sample_id,site,temp_c,ph,status
M01,North,18.2,7.1,ok
M02,South,20.5,,ok
M03,North,NA,6.8,review
M04,East,22.1,7.4,ok
M05,South,19.7,7.0,review
M06,East,23.0,NA,ok
M07,North,17.8,6.9,ok
M08,South,21.2,7.2,ok"

# Part A
measurements <- read.csv(text = csv_text, na.strings= c("", "NA"))
head(measurements)
str(measurements)
dim(measurements)
names(measurements)
colSums(is.na(measurements))
measurements_complete <- measurements[complete.cases(measurements), ]
measurements_complete
setdiff(measurements$sample_id, measurements_complete$sample_id)

# NA comparisons return NA, not TRUE/FALSE, since R can't know if two unknowns are equal.
# Use is.na(x) instead.

# Part B
measurements$site   <- factor(measurements$site)
measurements$status <-factor(measurements$status)
levels(measurements$site)
levels(measurements$status)
measurements$temp_f <- measurements$temp_c * 9 / 5 + 32
measurements$ph_below_7 <- measurements$ph < 7
subset_ok <- measurements[
  complete.cases(measurements) &
    measurements$site %in% c("North", "South") &
    measurements$status == "ok",
]
subset_ok[, c("sample_id", "site", "temp_c", "temp_f", "ph")]
mean(measurements$temp_c, na.rm = TRUE)
mean(measurements$temp_c[measurements$site == "South"], na.rm = TRUE)

# Part C
A <- matrix(1:4, nrow = 2)
B <- matrix(5:8, nrow = 2)
A * B
A %*% B
dim(A * B)
dim(A %*% B)

# A * B multiplies same-position elements; A %*% B takes row-by-column dot products.


# Problem 3

student_id <-paste0("P", sprintf("%02d", 1:8))
scores <- c(95, 82, NA, 67, 74, 88, 59, 91)

# Part A
grade_one <- function(score, a_min = 90, b_min = 80, c_min = 70, d_min = 60) {
  if (is.na(score)) {
    return(NA_character_)
  } else if (score >= a_min) {
    "A"
  } else if (score >=b_min) {
    "B"
  } else if (score >= c_min) {
    "C"
  } else if (score >= d_min) {
    "D"
  } else {
    "F"
  }
}

grade_one(NA)
grade_one(90)
grade_one(80)
grade_one(85)
grade_one(74)

# Part B
grades <- rep(NA_character_, length(scores))
for (i in seq_along(scores)) {
  grades[i] <- grade_one(scores[i])
}
names(grades) <- student_id
grades

# i is the current loop index, from 1 to length(scores).

# Part C-1
summarize_scores <- function(x, na.rm = TRUE, digits = 1) {
  c(
    total   = length(x),
    missing = sum(is.na(x)),
    mean    = round(mean(x, na.rm= na.rm), digits),
    sd      = round(sd(x, na.rm = na.rm), digits),
    min     = round(min(x, na.rm = na.rm), digits),
    max     = round(max(x, na.rm = na.rm), digits)
  )
}

summarize_scores(scores)
summarize_scores(x =scores, na.rm = TRUE, digits = 2)

# Part C-2
plot_scores <- function(x, ...) {
  plot(seq_along(x), x, ...)
}

plot_scores(scores, type = "b", pch = 19, xlab = "Position", ylab = "Score",
            main = "Student Scores")

# This plots scores against positions 1 through 8 using points and connecting lines.
# The line breaks around position 3 because that score is NA.