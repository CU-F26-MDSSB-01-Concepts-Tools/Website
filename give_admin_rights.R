library(tidyverse)
library(Microsoft365R) # For sending emails via outlook
library(readxl)
library(glue)
library(ghclass)

# Compile xlsx
orgname <- "CU-F25-MDSSB-01-Concepts-Tools"
jlorenz_outlook <- Microsoft365R::get_business_outlook()
jlorenz_od <- Microsoft365R::get_business_onedrive()

jlorenz_od$download_file(
  "Microsoft Teams Chat Files/student_feedback.xlsx",
  overwrite = TRUE
)
sheetHW <- readxl::read_xlsx("student_feedback.xlsx", sheet = "Homework Repos")

repos <- sheetHW |>
  select(RepoName, GitHub) |>
  filter(!is.na(RepoName)) |>
  filter(!is.na(GitHub))

repo_user_permission(
  glue("{orgname}/Project_NYCFlights_{repos$RepoName}"),
  repos$GitHub,
  "admin"
)
repo_user_permission(
  glue("{orgname}/Project_COVID19_{repos$RepoName}"),
  repos$GitHub,
  "admin"
)
repo_user_permission(
  glue("{orgname}/Project_FuelWatch_{repos$RepoName}"),
  repos$GitHub,
  "admin"
)
