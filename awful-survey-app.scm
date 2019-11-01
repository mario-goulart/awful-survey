(import scheme)
(cond-expand
 (chicken-4
  (use awful awful-survey))
 (chicken-5
  (import awful awful-survey))
 (else
  (error "Unsupported CHICKEN version.")))

(awful-survey "/" "surveys/example.scm"
              awful-settings: (lambda (continue)
                                (parameterize ((page-css "/css/chicken.css"))
                                  (continue))))
