(module awful-survey

 ;; Exported stuff
 (awful-survey
  survey
  survey-title
  survey-end
  survey-data-dir
  save-survey-answers
  multiple-choices
  single-choice
  single-choice/dropdown
  text-box
  text-box/multiline)

(import scheme)
(cond-expand
 (chicken-5
  (import (chicken base)) ;; for include
  ))
(include "scm/awful-survey.scm"))
