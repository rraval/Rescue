import Rescue.Latex

resume = do
  preamble "John Doe"
           "1234 Fifth Street, Province, Country"
           "(123) 456-7890"
           "example@example.com"
  skills
  education
  postscript

skills = section "Skills" $ do
           item "I like to do stuff"
           item "I'm also really good at other stuff"

education = section "Education" $ do
              bigitem "University of Someplace"
                      (date "Someplace, Country (From Date -- To Date)")
                      (newline "Candidate for a degree\nIn some program at some faculty or other")
              subsection "Relevant Courses and Assignments" $ do
                      bigitem "Course Title"
                              (date "Location (From Date -- To Date)")
                              "Description of what tasks were completed and why they are relevant."
                      bigitem "Some other Course"
                              (date "At the end of the world (0 AD -- 0 AD)")
                              "Insert filler material."

main = putStrLn $ show resume
