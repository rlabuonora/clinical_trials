library(shinytest2)

test_that("{shinytest2} recording: clinical_trials", {
  app <- AppDriver$new(name = "clinical_trials", height = 840, width = 1364)
  app$set_inputs(phase = c("PHASE1", "PHASE2"))
  app$set_inputs(status = c("COMPLETED", "RECRUITING"))
  app$click("api_request")
  app$expect_values()
})
