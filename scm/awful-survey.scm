(use awful spiffy simple-sha1)
(use data-structures srfi-1 srfi-13 extras posix files)

(define-record survey-widget
  id
  multiple-choice?
  dropdown?
  options)

;; User configurable parameters
(define survey (make-parameter '()))

(define survey-title (make-parameter ""))

(define survey-end
  (make-parameter
   '(p "Thanks for participating in the survey.")))

;; User widgets
(define *survey-widgets* '())

(define (add-survey-widget . args)
  (let ((survey-widget (apply make-survey-widget args)))
    (set! *survey-widgets*
          (cons (cons (car args) survey-widget)
                *survey-widgets*))
    survey-widget))

(define (multiple-choices id options)
  (register-options! id options)
  (render-options (add-survey-widget id #t #f options)))

(define (single-choice id options)
  (register-options! id options)
  (render-options (add-survey-widget id #f #f options)))

(define (single-choice/dropdown id options)
  (register-options! id options)
  (render-options (add-survey-widget id #f #t options)))

(define (text-box id)
  (render-text-box (add-survey-widget id #f #f #f)))

(define (text-box/multiline id)
  ;; hack: use multiple-choice? to indicate multiline
  (render-text-box (add-survey-widget id #f #f #f)))


;; Internal stuff
(define *options*
  ;; ((hash name text) ...)
  '())

(define (register-option! name option)
  (let ((hash (string->sha1sum (sprintf "i~a-~a" name option))))
    (when (alist-ref hash *options* equal?)
      (error 'register-option
             (sprintf "Hash ~a for ~a ~a exists."
                      hash name option)))
    (set! *options* (cons (list hash name option)
                          *options*))))

(define (register-options! name options)
  (for-each (lambda (option)
              (register-option! name option))
            options))

(define (option-text hash)
  (and-let* ((name/text (alist-ref hash *options* equal?)))
    (cadr name/text)))

(define (option-hash name text)
  (and-let* ((option (find (lambda (item)
                             (and (eq? name (cadr item))
                                  (equal? text (caddr item))))
                           *options*)))
    (car option)))

(define (combo-box name options)
  `(select (@ (name ,name) (id ,name))
           ,(map (lambda (option)
                   `(option (@ (value ,(option-hash name option)))
                            ,option))
                 (cons "" options))))

(define (render-options survey-widget)
  (let ((id (survey-widget-id survey-widget))
        (options (survey-widget-options survey-widget)))
    (if (survey-widget-dropdown? survey-widget)
        (combo-box id options)
        (map (lambda (option)
               `((input (@ (type ,(if (survey-widget-multiple-choice? survey-widget)
                                      "checkbox"
                                      "radio"))
                           (name ,id)
                           (id ,id)
                           (value ,(option-hash id option)))
                        ,option)
                 (br)))
             options))))

(define (render-text-box survey-widget)
  (let ((id (survey-widget-id survey-widget)))
    (if (survey-widget-multiple-choice? survey-widget)
        `(textarea (@ (name ,id) (id ,id)))
        `(input (@ (type "text") (name ,id) (id ,id))))))

(define (render-survey)
   `(div (@ (id "content"))
         (h1 ,(survey-title))
         (form (@ (method "post")
                  (action ,(make-pathname (app-root-path) "submit")))
               ,(survey)
               (p (input (@ (type "submit")))))))

(define (render-survey-end)
  `(div (@ (id "content"))
        (h1 ,(survey-title))
        ,(survey-end)))

(define (save-survey-answers data-dir)
  (let ((out-file
         (make-pathname data-dir
                        (sprintf "~a-~a-~a.scm"
                                 (remote-address)
                                 (current-milliseconds)
                                 (random 1000))))
        (answers
         (map (lambda (widget)
                (let* ((id (survey-widget-id widget))
                       (val ($ id (if (survey-widget-multiple-choice? widget)
                                      as-list
                                      as-string))))
                  (cons id (if (survey-widget-options widget)
                               (if (list? val)
                                   (map option-text val)
                                   (option-text val))
                               val))))
              (reverse (map cdr *survey-widgets*)))))
    (with-output-to-file out-file
      (lambda ()
        (for-each pp answers)))
    (survey-end)))


(define (awful-survey base-path survey-file data-dir
                        #!key (awful-settings (lambda (_) (_))))
  (load survey-file)
  (create-directory data-dir 'with-parents)

  (define-app awful-survey
    matcher: identity ;; FIXME
    handler-hook: (lambda (continue)
                    (parameterize ((enable-sxml #t)
                                   (app-root-path base-path))
                      (awful-settings continue)))

    (define (define-survey-page matcher proc #!key (method 'get))
      (define-page matcher
        proc
        title: (survey-title)
        method: method))

    (define-survey-page base-path
      render-survey)

    (define-survey-page "submit"
      (lambda ()
        (save-survey-answers data-dir)
        (render-survey-end))
      method: 'post)

    ))
