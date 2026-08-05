library(tidyverse)
library(Microsoft365R) # For sending emails via outlook
library(readxl)
library(glue)

# Compile xlsx
orgname <- "CU-F25-MDSSB-01-Concepts-Tools"
jlorenz_outlook <- Microsoft365R::get_business_outlook()
jlorenz_od <- Microsoft365R::get_business_onedrive()

jlorenz_od$download_file(
  "Microsoft Teams Chat Files/student_feedback.xlsx",
  overwrite = TRUE
)
studs <- read_xlsx("student_feedback.xlsx") |>
  mutate(
    email_body = if_else(
      is.na(GitHub),
      glue(
        "Dear {FirstName} {LastName}, 

You are registered in Data Science Concepts and Data Science Tools. We found that you are not member of the course's GitHub-Organization. Therefore, you could not start to work on the Homework Projects. Homework Projects are an essential part of the intended learning path and you should start working on them as soon as possible. 

Important: It is a requirement for passing in the Data Science Tools module to have half of them solved! So, you need to work on some to pass the Tools module. See https://{orgname}.github.io/Website/#tools-module

All information for getting into the GitHub-Organization is at the public frontpage of the Organization: 
https://github.com/{orgname}

Once you fill the form, please notify me in Teams or by replying to this email. I need to do some steps, in particular creating your repositories. I do not monitor the form entries daily anymore, because most students are already onboard.

Jan Lorenz"
      ),
      glue(
        "Dear {FirstName} {LastName}, 

My teaching assistants and myself have started to look at your repositories of your Homework Projects. 

I also finalized the list of the 5 Homework Projects (3 to come), and I specified the requirements for passing the Data Science Tools module on the course website: https://{orgname}.github.io/Website/#tools-module

We assessed each contributions with one of these 4 labels as 
“Just Passed” when half of the tasks are solved meaningfully,
“Passed” when almost all tasks are solved (some shortcomings are acceptable),
“Bonus” when the solutions are great, or
“Fail” when none of this has been achieved.

Your work currently is at this stage:

NYCFlights: {NYCFlights_Status_1}
COVID19: {COVID19_Status_1}

This is not the final assessment of the requirements! These will happen at the END of the semester. So, you can improve your work. 

In future rounds of reviews of your Homework Projects we want to provide also constructive feedback on how to improve your work. We want to start preparing Feedback for 
NYCFlights on Mon, Oct 27, and for 
COVID19 on Mon, Nov 3.  

So, it makes sense push your latest work to our your repository by then to receive constructive feedback!
We encourage you to work in groups on a weekly basis. You all need to solve the Homework Projects on your own repositories, but working together is highly encouraged. Soon, we will start with the Project check-in for the Final Project, where you should also work in groups. 

When you struggle to get into working on Homework Projects, please reach out to me or the Teaching Assistants.  Tell us where you get stuck. We are happy to help you get started! We were thinking of a live session about starting to work on NYCFlights guided by a Teaching Assistant. Let us know if this would be helpful for you.

Jan Lorenz"
      )
    ),
    email_subject = if_else(
      is.na(GitHub),
      "IMPORTANT: Data Science Tools - GitHub Onboarding for Homework Projects",
      "Data Science Tools Homework Project Status"
    )
  )

for (i in 1:nrow(studs)) {
  # Email students to get started
  jlorenz_outlook$create_email(
    to = studs$Email[i],
    subject = studs$email_subject[i],
    body = studs$email_body[i]
  )$send()
}
