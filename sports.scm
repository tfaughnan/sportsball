(import (chicken io)
        (chicken string)
        (chicken time)
        (chicken time posix)
        html-parser
        http-client
        srfi-1
        srfi-13
        sxml-transforms
        sxpath
        sxpath-lolevel)

;; FIXME: "philadelphia-76ers" => "Philadelphia 76Ers"
;; FIXME: "la-clippers" => "La Clippers"
(define (team-url->team-name url)
  (string-titlecase (string-translate (string-drop url (+ (string-index-right url #\/) 1)) #\- #\space)))

(define (game->team position sxml)
  (team-url->team-name (sxml:attr (position ((sxpath '(// (a 2))) sxml)) 'href)))

(define (game->time sxml)
  (sxml:string-value (third ((sxpath '(// (a 1))) sxml))))

(define (game->game-info sxml)
  (list (game->team first sxml)
        (game->team second sxml)
        (game->time sxml)))

(define (page->games sxml)
  ((sxpath '(tbody tr)) (car ((sxpath '(// table)) sxml))))

(define (make-table sport data)
  (list 'table
        (list 'caption sport)
        '(thead (tr (td "Away")
                    (td "Home")
                    (td "Time / Result")))
        (list 'tbody (map (lambda (row) (cons 'tr (map (lambda (col) (list 'td col)) row))) data))))

(define (source->table source)
  (let ((sxml (html->sxml (with-input-from-request (second source)
                                                   #f
                                                   read-string))))
    (make-table (first source)
                            (if (eq? ((sxpath '(// (section (@ (equal? (class "EmptyTable"))))))
                                      sxml)
                                     '())
                              (map game->game-info (page->games sxml))
                              '(("-" "-" "-"))))))

(define (page-title)
  (string-append "Sportsball: "
                 (time->string (seconds->local-time (current-seconds)) "%A, %e %B %Y")))

(define (make-site sources style)
    (list 'html
          (list 'head
             (list 'title (page-title))
             (list 'style style))
          (list 'body
                (list 'h1 (page-title))
                (map source->table sources)
                (list 'footer (string-append "Last Update: "
                                             (time->string (seconds->local-time (current-seconds))))))))

(define (build-source sport)
  (list sport
        (string-append "https://www.espn.com/"
                       (string-downcase sport)
                       "/schedule/_/date/"
                       (time->string (seconds->local-time (current-seconds)) "%Y%m%d"))))

;; TODO: add NFL (may be tricky b/c ESPN treats it differently than others)
(define *sources* (map build-source '("NBA" "MLB")))
(define-constant *style* "body{font-family:sans-serif;margin:16px}table{background-color:#eee;padding:16px;margin-top:24px;border:1px solid #333;border-collapse:collapse;width:32rem}caption{font-weight:700;font-size:1.4rem}thead tr{background-color:#aaa}tbody tr:nth-child(even){background-color:#ddd}td{padding:6px}footer{font-style:italic;margin-top:32px}")

(print (sxml->html (make-site *sources* *style*)))
