library(ghclass) # For managing the GitHub organization
library(tidyverse)
library(Microsoft365R) # For sending emails via outlook
library(readxl)
library(glue)
library(googlesheets4)

# Variables
orgname <- "CU-F25-MDSSB-01-Concepts-Tools"
jlorenz_outlook <- Microsoft365R::get_business_outlook()
jlorenz_od <- Microsoft365R::get_business_onedrive()
jlorenz_od$download_file(
  "Microsoft Teams Chat Files/student_feedback.xlsx",
  overwrite = TRUE
)
students <- read_xlsx("student_feedback.xlsx", sheet = "Final Project")
team <- students |>
  filter(!is.na(Team)) |>
  mutate(
    Name = paste(FirstName, LastName),
    RepoName = glue("FinalProject_{Topic}")
  )
teams <- team |>
  summarize(
    Names = paste(Name, collapse = ", "),
    Emails = paste(Email, collapse = ", "),
    RepoName = RepoName[1],
    InitalQuestion = `Initial Questions`[1],
    Feedback = `Question Feedback`[1],
    .by = Team
  )
team_email <- team |> left_join(teams, by = "Team")
repo_set_template("janlorenz/FinalProject_Template")

# # For late student:
# team <- tibble(
#   Name = "Tamjeed Ur Rahman Syed",
#   Email = "tsyed@constructor.university",
#   Topic = "COVID19_9",
#   GitHub = "Tsyed0604"
# )
# teams <- team |>
#   summarize(
#     Names = paste(Name, collapse = ", "),
#     Emails = paste(Email, collapse = ", "),
#     RepoName = RepoName[1],
#     InitalQuestion = `Initial Questions`[1],
#     Feedback = `Question Feedback`[1],
#     .by = Team
#   )

# THIS creates errors probably because github needs some time to process previous commands
# org_create_assignment(
#   org = orgname,
#   user = team$GitHub,
#   repo = team$RepoName,
#   team = team$RepoName,
#   source_repo = "janlorenz/FinalProject_Template",
#   private = TRUE,
#   add_badges = FALSE
# )
# HELPERS FOR CLEANING UP
#org_teams(org = orgname))
#team_delete(org = orgname, team = tolower(teams$RepoName))
#repo_delete(glue("{orgname}/{teams$RepoName}"), prompt = TRUE)

# Better go step by step
repo_create(
  org = orgname,
  repo = teams$RepoName,
  template = "janlorenz/FinalProject_Template",
  private = TRUE,
  auto_init = FALSE
)
team_create(
  org = orgname,
  team = teams$RepoName
)
team_invite(
  org = orgname,
  team = team$RepoName,
  user = team$GitHub
)
repo_add_team(
  repo = glue("{orgname}/{teams$RepoName}"),
  team = teams$RepoName,
  permission = c("push"),
  team_type = c("slug")
)

# Emails
for (i in 1:nrow(team_email)) {
  email <- glue(
    "Dear {team_email$Names[i]}, 

The repository {team_email$RepoName[i]} has been created for your Team. Find it here:
https://github.com/{orgname}/{team_email$RepoName[i]}
  
You are all assigned as collaborators with pull and push rights.
One learning goal to do collaborative work with git and GitHub. I will talk about it in the next lecture.
  
Below you find your initial questions and my feedback. Please read it carefully and feel free to ask for clarification.
  
Jan Lorenz
  
  
Initial Questions:
{team_email$InitalQuestion[i]}

  
Feedback:
{team_email$Feedback[i]}"
  )
  jlorenz_outlook$create_email(
    to = team_email$Email[i],
    subject = glue("Data Science Tools Final Project Repository"),
    body = email
  )$send()
}
