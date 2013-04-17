(module awful-survey

 ;; Exported stuff
 (awful-survey
  survey
  survey-title
  survey-end
  multiple-choices
  single-choice
  single-choice/dropdown
  text-box
  text-box/multiline)

(import chicken scheme)
(include "scm/awful-survey.scm"))
