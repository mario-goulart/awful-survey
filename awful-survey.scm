(module awful-survey

 ;; Exported stuff
 (awful-survey
  survey
  survey-title
  survey-end
  survey-blocked
  survey-data-dir
  save-survey-answers
  multiple-choices
  single-choice
  single-choice/dropdown
  text-box
  text-box/multiline
  max-surveys/ip/time-window
  allowed-to-answer-survey?)

(import scheme)
(cond-expand
 (chicken-4
  (import chicken)
  (use awful intarweb spiffy simple-sha1)
  (use data-structures extras files irregex posix srfi-1 srfi-13)
  (define pseudo-random-integer random))
 (chicken-5
  (import (chicken base)
          (chicken file)
          (chicken file posix)
          (chicken format)
          (chicken irregex)
          (chicken pathname)
          (chicken pretty-print)
          (chicken random)
          (chicken string)
          (chicken time))
  (import awful intarweb spiffy simple-sha1 srfi-1 srfi-13))
 (else
  (error "Unsupported CHICKEN version.")))


(define-record survey-widget
  id
  multiple-choice?
  dropdown?
  mandatory?
  options)

;; User configurable parameters

(define max-surveys/ip/time-window
  ;; Maximum number of surveys (car) per IP address in the time window
  ;; in seconds (cdr).
  (make-parameter '(100 . 3600)))

(define survey (make-parameter '()))

(define survey-title (make-parameter ""))

(define survey-end
  (make-parameter
   '(p "Thanks for participating in the survey.")))

(define survey-blocked
  ;; What to render when an IP has reached max-surveys/ip/time-window.
  (make-parameter
   '(h1 (@ (id "survey-blocked"))
        "You've already answered enough surveys.  Thanks.")))

(define missing-field-highlighter
  (make-parameter
   (lambda (id widget-sxml)
     `(span (@ (class "missing-field"))
            ,widget-sxml))))

(define survey-data-dir (make-parameter "data"))

(define save-survey-answers
  (make-parameter
   (lambda (answers)
     (let* ((dir (make-pathname (survey-data-dir) (remote-address)))
            (out-file
             (make-pathname dir
                            (sprintf "~a-~a-~a.scm"
                                     (current-seconds)
                                     (current-milliseconds)
                                     (pseudo-random-integer 1000)))))
       (create-directory dir 'with-parents)
       (with-output-to-file out-file
         (lambda ()
           (for-each pp answers)))))))

(define allowed-to-answer-survey?
  ;; Given an IP, return #t if it is allowed to answer the
  ;; survey. Return #f otherwise.
  (make-parameter
   (lambda (ip)
     (let* ((age-threshold (- (current-seconds)
                              (cdr (max-surveys/ip/time-window))))
            (recent-surveys
             (filter
              (lambda (survey-file)
                (> (file-modification-time survey-file) age-threshold))
              (glob (make-pathname (list (survey-data-dir) ip) "*.scm")))))
       (<= (length recent-surveys) (car (max-surveys/ip/time-window)))))))

;; Internal parameters
(define %survey-answers (make-parameter '()))

;; User widgets
(define *survey-widgets* '())

(define (add-survey-widget . args)
  (if *form-loaded?*
      (alist-ref (car args) *survey-widgets*)
      (let ((survey-widget (apply make-survey-widget args)))
        (set! *survey-widgets*
              (cons (cons (car args) survey-widget)
                    *survey-widgets*))
        survey-widget)))

(define (multiple-choices id options #!key (mandatory? #t))
  (register-options! id options)
  (render-options (add-survey-widget id #t #f mandatory? options)))

(define (single-choice id options #!key (mandatory? #t))
  (register-options! id options)
  (render-options (add-survey-widget id #f #f mandatory? options)))

(define (single-choice/dropdown id options #!key (mandatory? #t))
  (register-options! id options)
  (render-options (add-survey-widget id #f #t mandatory? options)))

(define (text-box id #!key (mandatory? #t))
  (render-text-box (add-survey-widget id #f #f mandatory? #f)))

(define (text-box/multiline id #!key (mandatory? #t))
  ;; hack: use multiple-choice? to indicate multiline
  (render-text-box (add-survey-widget id #t #f mandatory? #f)))


;; Internal stuff
(define *options*
  ;; ((hash name text) ...)
  '())

(define *form-loaded?* #f)

(define (register-option! name option)
  (unless *form-loaded?*
    (let ((hash (string->sha1sum (sprintf "i~a-~a" name option))))
      (when (alist-ref hash *options* equal?)
        (error 'register-option
               (sprintf "Hash ~a for ~a ~a exists."
                        hash name option)))
      (set! *options* (cons (list hash name option)
                            *options*)))))

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

(define (field-missing? widget)
  (and (eq? 'POST (request-method (current-request)))
       (survey-widget-mandatory? widget)
       (not (answer-by-id (survey-widget-id widget)))))

(define (wrap-widget id widget-sxml)
  (let ((widget (alist-ref id *survey-widgets*)))
    (if (field-missing? widget)
        ((missing-field-highlighter) id widget-sxml)
        widget-sxml)))

(define (combo-box name options)
  `(select (@ (name ,name) (id ,name))
           ,(map (lambda (option)
                   `(option (@ (value ,(option-hash name option))
                               ,(if (equal? option (answer-by-id name))
                                    '(selected)
                                    '()))
                            ,option))
                 (cons "" options))))

(define (option-checked? id option multiple-choice?)
  ((if multiple-choice? member equal?)
   option
   (or (answer-by-id id)
       (if multiple-choice? '() #f))))

(define (render-options survey-widget)
  (let ((id (survey-widget-id survey-widget))
        (options (survey-widget-options survey-widget))
        (multiple-choice? (survey-widget-multiple-choice? survey-widget)))
    (wrap-widget
     id
     (if (survey-widget-dropdown? survey-widget)
         (combo-box id options)
         (map (lambda (option)
                `((input (@ (type ,(if multiple-choice?
                                       "checkbox"
                                       "radio"))
                            (name ,id)
                            (id ,id)
                            ,(if (option-checked? id option multiple-choice?)
                                 '(checked)
                                 '())
                            (value ,(option-hash id option)))
                         ,option)
                  (br)))
              options)))))

(define (render-text-box survey-widget)
  (let ((id (survey-widget-id survey-widget)))
    (wrap-widget
     id
     (if (survey-widget-multiple-choice? survey-widget)
         `(textarea (@ (name ,id)
                       (id ,id))
                    ,(or (answer-by-id id) ""))
         `(input (@ (type "text")
                    (name ,id)
                    (id ,id)
                    (value ,(or (answer-by-id id) ""))))))))

(define (render-survey base-path)
  (let ((content
         `(div (@ (id "content"))
               (h1 ,(survey-title))
               (form (@ (method "post")
                        (action ,base-path))
                     ,((survey))
                     (p (input (@ (type "submit"))))))))
    (set! *form-loaded?* #t)
    content))

(define (render-survey-end)
  `(div (@ (id "content"))
        (h1 ,(survey-title))
        ,(survey-end)))

(define (answers-from-request)
  (map (lambda (widget)
         (let* ((id (survey-widget-id widget))
                (val ($ id (nonempty
                            (if (survey-widget-multiple-choice? widget)
                                as-list
                                as-string)))))
           (cons id (if (survey-widget-options widget)
                        (if (list? val)
                            (map option-text val)
                            (option-text val))
                        val))))
       (reverse (map cdr *survey-widgets*))))

(define (answer-by-id id)
  (alist-ref id (%survey-answers)))

(define (form-submission-ok? answers)
  (let loop ((answers answers))
    (if (null? answers)
        #t
        (let* ((var (caar answers))
               (val (cdar answers))
               (widget (alist-ref var *survey-widgets*)))
          (if (and (survey-widget-mandatory? widget)
                   (not val))
              #f
              (loop (cdr answers)))))))


(define (awful-survey base-path survey-file
                        #!key (awful-settings (lambda (_) (_))))
  (load survey-file)
  (when (survey-data-dir)
    (create-directory (survey-data-dir) 'with-parents))

  (define-app awful-survey
    matcher: (let ((base-regex
                    (irregex (string-append (string-chomp base-path "/")
                                            ".*"))))
               (lambda (path)
                 (irregex-match? base-regex path)))
    handler-hook: (lambda (continue)
                    (parameterize ((enable-sxml #t)
                                   (app-root-path base-path))
                      (awful-settings continue)))

    (define (define-survey-page matcher proc #!key (method 'get))
      (define-page matcher
        proc
        title: (survey-title)
        headers: '(meta (@ (name "viewport")
                           (content "width=device-width, initial-scale=1")))
        method: method))

    (define-survey-page (main-page-path)
      (lambda ()
        (if ((allowed-to-answer-survey?) (remote-address))
            (let ((answers (answers-from-request)))
              (if (and (not (null? *survey-widgets*))
                       (eq? (request-method (current-request)) 'POST)
                       (form-submission-ok? answers))
                  (begin
                    ((save-survey-answers) answers)
                    (render-survey-end))
                  (parameterize ((%survey-answers answers))
                    (render-survey base-path))))
            (survey-blocked)))
      method: '(get post head))

    ))
) ;; end module
