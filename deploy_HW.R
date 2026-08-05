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
  "GitHub username for Data Science Concepts_Tools.xlsx",
  overwrite = TRUE
)
students <- read_xlsx(
  "GitHub username for Data Science Concepts_Tools.xlsx"
) |>
  filter(year(`Start time`) == 2025) |>
  mutate(
    FirstName = word(Name, 1),
    GitHub = `Your GitHub username`,
    Status = "Student"
  )
instructors <- read_xlsx("instructors.xlsx")


for (proj in c("NYCFlights", "COVID19", "DataScienceProfiles", "FuelWatch")) {
  gh <- bind_rows(students, instructors) |>
    mutate(HW_Project = paste0("Project_", proj, "_", FirstName))
  gh_missing <- gh |>
    filter(
      HW_Project %in%
        (setdiff(
          glue("{orgname}/{gh$HW_Project}"),
          org_repo_search(orgname, paste0("Project_", proj, "_"))
        ) |>
          str_remove(glue("{orgname}/")))
    )
  if (nrow(gh_missing) > 0) {
    org_create_assignment(
      org = orgname,
      user = gh_missing$GitHub, #"org_members(orgname),
      repo = gh_missing$HW_Project,
      source_repo = paste0("janlorenz/Project_", proj, "_Template"),
      private = TRUE
    )

    # Write an email with jlorenz_outlook
    for (i in 1:nrow(gh_missing)) {
      # Email students to get started
      additional_note <- ifelse(
        proj == "DataScienceProfiles",
        "
File Links:
Data Science Profiles.xlsx: https://constructoruniversity-my.sharepoint.com/:x:/r/personal/jlorenz_constructor_university/Documents/Data%20Science%20Profile.xlsx?d=wa654ba79067c4ccb9783dfc480d48309&csf=1&web=1&e=ZMnSI7
Data Science Profiles 2021-2024.csv: https://constructoruniversity-my.sharepoint.com/:x:/r/personal/jlorenz_constructor_university/Documents/Microsoft%20Teams%20Chat%20Files/Data%20Science%20Profiles%202021-2024.csv?d=wc3e02ed67b4c4867ac499094a05d68dd&csf=1&web=1&e=OroHkS

If you are not in the Survey but also want to work with your personal data: Try filling it here https://forms.office.com/e/dHvdFusjqV and then download it again.
    ",
        ""
      )
      email <- glue(
        "Dear {gh_missing$FirstName[i]}, 

The Homework repository {proj} has been created for you. Find it here:
https://github.com/{orgname}/Project_{proj}_{gh_missing$FirstName[i]}

Start working on it and learn data science! Reach out for help if you get stuck with the.
Tip: Build a workgroup with your classmates and work together on it for an hour or so. 
{additional_note}
Jan Lorenz"
      )
      jlorenz_outlook$create_email(
        to = gh_missing$Email[i],
        subject = glue("Data Science Concepts/Tools Homework Project {proj}"),
        body = email
      )$send()
    }
  }
}
