#!/usr/local/bin/gosh

(add-load-path "./Gauche-net-yahoo-jp")

(use net.yahoo-jp)
(use rfc.uri)
(use srfi-1)
(use srfi-13)
(use srfi-27)
(use srfi-42)
(use sxml.sxpath)
(use sxml.serializer)
(use text.tree)
(use text.html-lite)
(use util.list)
(use www.cgi)

;;; configurations

(yj-application-id "YahooDemo") ; set your own application ID

(define *sites*
  '("anond.hatelabo.jp"
    "ja.uncyclopedia.info"
    "komachi.yomiuri.co.jp"
    "okwave.jp"
    "q.hatena.ne.jp"))

(define *style* "
/**** title ****/

h1 {
    font-family: sans-serif;
    font-size: 1.2em;
    color: #666;
}

h1.top {
    margin: 10% 0;
    text-align: center;
    font-size: 3em;
}

span.T, span.A, span.B, span.U, span.N, span.X {
    font-family: \"BlairMdITC TT\";
    font-weight: 900;
    color: #f03;
    text-shadow: 1px 1px 1px #333;
}

span.T {
    position: relative;
    top: 0.15em;
    font-size: 1.6em;
    margin-right: -0.15em;
}

span.A {
    position: relative;
    top: 0.12em;
    font-size: 1.2em;
}

span.B {
    margin-left: -0.05em;
    font-size: 1.2em;
}

span.U {
    margin-left: -0.05em;
    font-size: 1.1em;
}

span.N {
    margin-left: -0.04em;
    font-size: 1.3em;
}

span.X {
    position: relative;
    top: 0.06em;
    margin-left: -0.15em;
    margin-right: 0.2em;
    font-size: 1.6em;
    font-style: italic;
}

/**** Form ****/

form.generate {
    margin-bottom: 0em;
    padding: 0.5em;
    background-color: #dce;
    border-style: solid none none none;
    border-width: 1px 0 0 0;
    border-color: #989;
}

form.top {
    width: 20em;
    margin: 10% auto 0 auto;
    padding: 0.6em 0.5em 0.5em 0.5em;
    text-align: center;
    background-color: #dce;
    border-style: solid;
    border-width: 1px;
    border-color: #989;
    -moz-border-radius: 4px;
    -webkit-border-radius: 4px;
}

form.generate input.query {
    width: 20em;
}

form.generate p, form.generate span {
    font-size: 0.8em;
}

form.generate p {
    margin: 0em;
    padding: 0em;
}

form.generate span {
    margin: 0 0 0 0.3em;
}

p.result {
    margin-top: 0em;
    padding: 0.3em 0.5em;
    font-size: 0.8em;
    background-color: #fef;
    border-style: none none solid none;
    border-width: 0 0 1px 0;
    border-color: #989;
}

p.result em {
    font-family: sans-serif;
    font-style: normal;
    font-weight: bold;
}

/**** Q & A ****/

dt, dd {
    margin-top: 1em;
    margin-bottom: 1em;
    line-height: 1.4em;
    padding: 0.8em 1em;
    border-style: solid;
    border-width: 2px;
    overflow-x: auto;
    -moz-border-radius: 4px;
    -webkit-border-radius: 4px;
}

dt:before, dd:before {
    font-size: 1.8em;
    font-weight: bold;
    margin-right: 0.4em;
}

dt:before {
    color: #693;
    content: \"Q\";
}

dt {
    background-color: #efe;
    border-color: #693;
}

dt hr {
    border-style: dotted;
    border-color: #693;
}

dd:before {
    color: #c30;
    content: \"A\";
}

dd {
    background-color: #fee;
    border-color: #c30;
}

/**** Link ****/

a:link {
    color: #482;
}

a:visited {
    color: #996;
}

a:hover {
    color: #f93;
}

p.pages {
    font-size: 1.2em;
    text-align: center;
}

p.pages em {
    font-style: normal;
    font-weight: bold;
}

p.pages a {
    margin: 0 0.2em;
}
")

;;; utilities

(define map/index
  (with-module gauche.sequence map-with-index))

(define (make-query arguments)
  (tree->string
   (map/index (lambda (i x)
                (let ((x* (uri-encode-string (x->string x))))
                  (cond ((zero? i) x*)
                        ((even? i) (cons '& x*))
                        (else (cons '= x*)))))
              arguments)))

(define (decode-entities string)
  (regexp-replace-all* string
    #/&apos\;/ "'"
    #/&quot\;/ "\""
    #/&nbsp\;/ " "
    #/&lt\;/ "<"
    #/&gt\;/ ">"
    #/&amp\;/ "&"))

(define-syntax define-sxpath-text-query
  (syntax-rules ()
    ((_ name abbrpath-expr)
     (define name
       (compose
        (map$ decode-entities)
        (let ((abbrpath abbrpath-expr))
          (sxpath (if (list? abbrpath)
                      `(,@abbrpath *text*)
                      `(,abbrpath *text*)))))))))

(define-syntax define-car-sxpath-text-query
  (syntax-rules ()
    ((_ name abbrpath-expr)
     (begin
       (define-sxpath-text-query name* abbrpath-expr)
       (define name (compose car name*))))))

;;; text generator

(define (not-replaceable? word)
  (#/['’\"”‘’“”―—!?！？,，。、.．()（）｟｠「」『』\[\]［］{}｛｝〔〕〘〙〈〉《》«»【】〖〗]/ word))

(define (replaceable? word)
  (not (not-replaceable? word)))

(define-class <text-generator> ()
  ((table
    :init-keyword :table
    :init-form (make-hash-table 'equal?))))

(define-method add-seed! ((generator <text-generator>) words)
  (do-ec (:parallel (:list prev `("" ,@words))
                    (:list word words)
                    (:list next `(,@(cdr words) "")))
    (hash-table-update! (slot-ref generator 'table)
                        (cons prev next)
                        (lambda (table)
                          (hash-table-update! table word (cut + <> 1) 0)
                          table)
                        (make-hash-table 'equal?))))

(define-method generate-word ((generator <text-generator>) prev word next)
  (let ((empty-table (make-hash-table 'equal?)))
    (car (hash-table-fold
          (hash-table-get (slot-ref generator 'table)
                          (cons prev next)
                          empty-table)
          (lambda (candidate* frequency candidate&count)
            (receive (candidate count) (car+cdr candidate&count)
              (let ((count* (+ count frequency)))
                (cons (if (and (replaceable? candidate*)
                               (< (random-integer count*) frequency))
                          candidate*
                          candidate)
                      count*))))
          (cons word 0)))))

(define-method generate-text ((generator <text-generator>) template)
  (let ((text (list->vector template)))
    (dotimes (_ (* 3 (vector-length text)))
      (let ((i (random-integer (vector-length text))))
        (when (replaceable? (vector-ref text i))
          (let ((word (generate-word generator
                                     (vector-ref text (- i 1) "")
                                     (vector-ref text i)
                                     (vector-ref text (+ i 1) ""))))
            (vector-set! text i word)))))
    (vector->list text)))

;;; Q & A generator

(define Q&A-list:
  (sxpath '(ResultSet Result Question)))

(define total-results-available:
  (compose x->integer cadr (car-sxpath '(ResultSet @ totalResultsAvailable))))

(define-car-sxpath-text-query question: '(Content))
(define-car-sxpath-text-query answer: '(BestAnswer))
(define-car-sxpath-text-query url: '(Url))

(define (search-chiebukuro-Q&A query s n)
  (let ((result-set (/Chiebukuro/V1/questionSearch
                     :query query :type "all"
                     :condition "solved" :start s :results n)))
    (values (total-results-available: result-set)
            (filter-map (lambda (Q&A)
                          (let ((Q (question: Q&A))
                                (A (answer: Q&A)))
                            (and (not (string-null? Q))
                                 (not (string-null? A))
                                 (list Q A (url: Q&A)))))
                        (Q&A-list: result-set)))))

(define-sxpath-text-query summaries:
  '(ResultSet Result Summary))

(define (search-web-summaries query n)
  (remove string-null?
          (append-map (cut string-split <> "...")
                      (summaries:
                       (apply /WebSearchService/V1/webSearch
                              :query query :results n
                              (fold (cut cons* :site <> <>) '() *sites*))))))

(define-sxpath-text-query surfaces:
  '(ResultSet ma_result word_list word surface))

(define (split-into-words string)
  (surfaces:
   (/MAService/V1/parse :sentence string :results "ma" :response "surface")))

(define *delimiter* "|||")

(define (blocks->words blocks)
  (reverse! (fold (cute cons* (tree->string `(" " ,*delimiter* " ")) <> <>)
                  '()
                  blocks)))

(define (words->blocks words)
  (let iter ((block '()) (blocks '()) (words* words))
    (if (null? words*)
        (reverse! blocks)
        (if (equal? *delimiter* (car words*))
            (iter '()
                  (cons (reverse! block) blocks)
                  (cdr words*))
            (iter (cons (car words*) block)
                  blocks
                  (cdr words*))))))

(define (uri-encoded-size string)
  (string-size (uri-encode-string string)))

(define (size-based-take strings max size-of)
  (let iter ((sum 0) (strings* '()) (strings strings))
    (if (null? strings)
        (reverse! strings*)
        (let* ((string (car strings))
               (sum* (+ sum (size-of string))))
          (if (< max sum*)
              (reverse! strings*)
              (iter sum* (cons string strings*) (cdr strings)))))))

(define (parse string-list)
  (remove null?
          (words->blocks
           (split-into-words
            (string-concatenate
             (size-based-take (blocks->words string-list)
                              98000
                              uri-encoded-size))))))

(define (generate-Q&A-list query s n)
  (receive (total-results-available Q&A-list)
      (search-chiebukuro-Q&A query s 100)
    (let* ((web-summaries (search-web-summaries query 50))
           (search-results (parse (append (append-map (cut take <> 2) Q&A-list)
                                          web-summaries))))
      (let ((text-generator (make <text-generator>)))
        (for-each (cut add-seed! text-generator <>) search-results)
        (values total-results-available
                (map (lambda (Q&A url)
                       (list (generate-text text-generator (car Q&A))
                             (generate-text text-generator (cadr Q&A))
                             url))
                     (slices (take* search-results (* 2 n)) 2)
                     (map caddr Q&A-list)))))))

;;; main

(define (Q&A-list->html Q&A-list)
  `(dl ,@(append-map (lambda (Q&A)
                       `((dt ,(string-concatenate (car Q&A))
                             (hr)
                             (a (@ (href ,(caddr Q&A))) ,(caddr Q&A)))
                         (dd ,(string-concatenate (cadr Q&A)))))
                     Q&A-list)))

(define (header)
  `(head (title "TABUN?知恵袋")
         (style ,*style*)))

(define (title class)
  `(h1 ,@(if class `((@ (class ,class))) '())
       (span (@ (class "T")) "T")
       (span (@ (class "A")) "A")
       (span (@ (class "B")) "B")
       (span (@ (class "U")) "U")
       (span (@ (class "N")) "N")
       (span (@ (class "X")) "?")
       "知恵袋"))

(define (form class q n)
  `(form (@ ,@(if class `((class ,class)) '())
            (action "./tabun.cgi"))
         (p "Yahoo!知恵袋に投稿された知識から")
         (input (@ (class "query") (name "q") (value ,q)))
         (span "を")
         (input (@ (type "submit") (value "検索？")))
         (input (@ (type "hidden") (name "s") (value 1)))
         (input (@ (type "hidden") (name "n") (value ,n)))))

(define (quotient* m n)
  (quotient (+ m n -1) n))

(define (other-pages q s n total-results-available)
  (let ((links (list-ec (: i 0 (quotient* (min 1000 total-results-available) n))
                        (:let query
                              (make-query `(:q ,q :s ,(+ 1 (* i n)) :n ,n)))
                 (if (= (- s 1) (* i n))
                     `(em ,(+ i 1))
                     `(a (@ (href ,(string-append "./tabun.cgi?" query)))
                         ,(+ i 1))))))
    `(p (@ (class "pages"))
        ,@(intersperse " "
           (take* (drop* links (max 0 (- (quotient s n) 5))) 11)))))

(define (result-page q s n)
  (receive (total-results-available Q&A-list) (generate-Q&A-list q s n)
    `(html
      ,(header)
      (body ,(title #f)
            ,(form "result generate" q n)
            (p (@ (class "result"))
               (em ,q) "を検索した結果？"
               ,@(if (null? Q&A-list)
                     '()
                     `(" (" ,s "件目～" ,(+ s (length Q&A-list) -1) "件目)")))
            ,(if (zero? (length Q&A-list))
                 '(p "何も見つかりませんでした。"
                     "他のキーワードをお試し下さい。")
                 (Q&A-list->html Q&A-list))
            ,(other-pages q s n total-results-available)))))

(define (top-page s n)
  `(html
    ,(header)
    (body ,(title "top")
          ,(form "top generate" "" n))))

(define (main args)
  (random-source-randomize! default-random-source)
  (cgi-main
   (lambda (params)
     (let* ((q (cgi-get-parameter "q" params))
            (s (cgi-get-parameter "s" params :convert x->integer :default 1))
            (n (cgi-get-parameter "n" params :convert x->integer :default 5)))
       (list (cgi-header)
             (srl:sxml->xml
              `(*TOP*
                (*PI* xml "version=\"1.0\" encoding=\"UTF-8\"")
                ,(if q (result-page q s n) (top-page s n)))))))))

;; Local variables:
;; mode: scheme
;; end:
