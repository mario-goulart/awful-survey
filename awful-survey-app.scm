(use awful awful-survey)

(awful-survey "/" "surveys/example.scm"
              awful-settings: (lambda (continue)
                                (parameterize ((page-css "/css/chicken.css"))
                                  (continue))))
