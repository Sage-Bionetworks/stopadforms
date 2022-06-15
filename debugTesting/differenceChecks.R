setdiff(submissions$submission, reviews$submission)


setdiff(submissions$submission %>% trimws(), reviews$submission)

setdiff(reviews$submission, submissions$submission)


submissionDistinct <- submissions %>%
  distinct(step, form_data_id, submission)

nrow(submissionDistinct)
nrow(submissions)

reviewsDistinct <- reviews %>% 
  distinct(submission, step, form_data_id)

nrow(reviewsDistinct)
nrow(reviews)