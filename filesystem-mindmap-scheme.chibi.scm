#!/usr/bin/chibi-scheme -rnewermain

;;>#!
;;>Time-stamp: <2021-04-07 15:31:26 lockywolf>
;;>#+description: A port of dir2dot by Darxus@ChaosReighs.com into scheme.
;;>#+author: lockywolf
;;>#+created: <2021-03-17 Wed>
;;>#+category: utilities
;;>#+tags: programming, mind, mind-map, scheme
;;>#+creator: Emacs 27.1/org-mode 9.4
;;>Usage:
;;>time filesystem-mindmap-scheme root > /tmp/dirtree.dot
;;>time circo -Tpng dirtree.dot > dirtree.png
;;>You can try other engines.
;;>On my machine sfdp takes 3 hours on ~3000 nodes.
;;>Graphviz: https://www.graphviz.org/
;;>
;;>Legend:
;;>Files: boxes
;;>File types: font colours
;;>Directory shapes: ordinary:oval, project:house, repo:doubleoctagon
;;>Fillcolor: incompletely displayed trees
;;>
;;>TODO:
;;>3. File name colouring.
;;>4. Displaying images.
;;>5. Handling of dangling symlinks.
;;>
;;>Far TODO:
;;>1. Music albums.
;;>2. Other kinds of projects.
;;>3. README.org

;;>!#

;; (if (zero? (run (ls)))
;;    (display "LS successful.\n")
;;    (display "LS unsuccessful."))
;; (display "Hello\n")
;; (display "arguments:")
;; (display command-line-arguments)
;; (newline)
;; (display "(argv 1 \".\"): ") (display (argv 1 ".")) (newline)
;; (display "string-join test:") (display (string-join  (list "string1" "string2") "/" ))
;; (newline)



;; (define-interface mindmap-interface (export newmain))

(import (chibi))
(import (chibi modules))
(import (meta))


(define-library (mindmap)
  (export newmain)
  (import
    ;; scheme-with-scsh
    ;; os-strings
    ;; (subset srfi-13 (string-join))
    (only (srfi 1) lset-intersection lset<= lset-difference remove length+ take lset=)
    ;; (subset i/o (set-port-text-codec!))
    ;; (subset text-codecs (utf-8-codec))
    ;; (subset tables (make-string-table table-ref table-set! table-walk))
    ;; (subset srfi-27 (random-integer))
    ;; (subset usual-commands (preview))
    (chibi trace)
    (scheme small)
    (only (srfi 125) make-hash-table hash-table-ref/default hash-table-set! hash-table-for-each)
    (only (srfi 130) string-index-right string-cursor->index)
    (rename (prefix (chibi filesystem) cfs-))
    (only (chibi time) current-seconds seconds->string)
   )

  (begin

    (define config-album-show-max-files 5)
    (define config-recurse-into-dotfiles #f)
    (define config-stop-at-repos #t)
    (define config-stop-at-projects #t)
    (define config-stop-deliberately #t)
    (define config-stop-at-filesystem-boundaries #f)
    (define config-stop-at-maildir #t)


    (define (directory-files dir dotfiles?)
      (let ((files (lset-difference equal? (cfs-directory-files dir) (list "." ".."))))
        (if dotfiles?
         files
         (remove (lambda (x) (string=? "." (substring x 0 1))) files))))
    
    (define (directory-files-without-ignores . o)
      (let ((ans (apply directory-files o)))
        ;; (display "directory-files-without-ignores:o ") (display o) (newline)
        ;; (display "directory-files-without-ignores:ans ") (display ans) (newline)
        (lset-difference equal? ans
                         (list)))) ; was "cache"

    (define (newmain args)
      ;; (display "debug: args: ") (display args) (newline)
      ;; (display (length args))
      ;; (define actual-directory (absolute-file-name
      ;;                                (expand-file-name (if (< (length+ args) 2)
      ;;                                                     "."
      ;;                                                     (list-ref args 1)))))
      (define actual-directory (list-ref args 1))
      (print-prologue actual-directory)
      ;; (call-with-current-continuation
      ;;     (lambda (continue)
      ;;       (with-errno-handler*
      ;;        (lambda (errno packet)
      ;;          (display "caught error")(newline)
      ;;          (display "errno=") (display errno) (newline)
      ;;          (display "packet=") (display packet) (newline)
      ;;          (display "backtrace:") (newline)
      ;;          (preview) (newline)
      ;;          (exit))
      ;;        (lambda ()
      (walk-tree-and-print-edges "" actual-directory)
      (newline)
      (print-nodes)
      (print-epilogue)
               ;))))
)

    (define filesep "/")

    (define (directory-something? path detector-list)
      (not (null? (lset-intersection equal? detector-list (directory-files path #t)))))

    (define directory-repo-marker-list (list ".git" ".svn" ".cvs"))

    (define (directory-repo? path)
      (directory-something? path directory-repo-marker-list))

    (define directory-project-marker-list (list "Makefile"
                                           "configure.ac"
                                           "Makefile.am"
                                           "configure"
                                           "index.html"))

    (define (directory-project? path)
      (directory-something? path directory-project-marker-list))

    (define directory-deliberately-terminate-list (list "LWF-MINDMAP-TERMINATE"))

    (define (directory-deliberately-terminate? path)
      (directory-something? path directory-deliberately-terminate-list))

    (define image-extensions-list
      (list ".jpg" ".JPG" ".jpeg" ".JPEG" ".tif" ".TIF" ".tiff" ".TIFF" ".gif" ".GIF" ".webp"))

    (define (directory-only-directories . o)
      (let* ((raw-contents (apply directory-files-without-ignores o)))
        (cfs-with-directory (car o)
                  (lambda () (remove (lambda (x) (not (file-directory? x #f))) raw-contents)))))

    (define (directory-only-files . o)
      (let* ((raw-contents (apply directory-files-without-ignores o))
             ;; (discard1 (begin (display "\nraw-contents: ") (display raw-contents)(newline)))
             )
        (cfs-with-directory (car o)
                  (lambda () (remove (lambda (x) (file-directory? x #f)) raw-contents)))))

    
    (define (file-name-extension-index fname)
       (let ((dot (string-cursor->index fname (string-index-right fname #\.)))
             (slash (string-cursor->index fname (string-index-right fname #\/))))
         (if (and dot
                  (> dot 0)
                  (if slash (> dot slash) #t)
                  (not (char=? #\/ (string-ref fname (- dot 1)))))
             dot
             (string-length fname))))
     
     (define (file-name-sans-extension fname)
       (substring fname 0 (file-name-extension-index fname)))
     
     (define (file-name-extension fname)
       (substring fname (file-name-extension-index fname)
                        (string-length fname)))

    
    (define (directory-album? path)
      ;; (display "\ndebug:directory-album?: ") (display path) (newline)
      (if (not (file-directory? path #f))
         (begin (display "error: directory-album?, argument path is not a directory: ")
                (display path)
                (newline)
                (exit)))
      (let* ((d-files (directory-only-files path #t))
             ;; (discard1 (begin (display "\nfiles:")(display d-files) (newline)))
             (d-cur-exts (map (lambda (name) (file-name-extension name)) d-files))
             (retval (and (not (null? d-cur-exts))
                        (lset<= equal? d-cur-exts image-extensions-list))))
        retval))

    (define (directory-maildir? path)
;      (display "directory-maildir?: ") (display path) (newline)
      (let ((files (directory-files path #t)))
        ;;(display files)
        (lset= equal? files (list "cur" "new" "tmp"))))

    ;; (define (parse-filename filename)
    ;;   (regexp-fold (rx (+ (~ "/")))
    ;;                (lambda (i m lis)
    ;;                  (cons (match:substring m 0) lis))
    ;;                '() filename))

    (define (escape-like-write s)
      (parameterize
          ((current-output-port
            (open-output-string)))
        (write s)
        (get-output-string (current-output-port))))
      ;(let ((p (make-string-output-port)))
        ;(set-port-text-codec! p utf-8-codec)
      ;  (write s p)
      ;  (string-output-port-output p)))


    (define inode-filename-table
      (make-hash-table string?)                                  ;
      ;; (make-string-table)
      )

    (define (make-file-record description shape fillcolor pos)
      (list description shape fillcolor pos))
    (define (file-record-description f-r)
      (list-ref f-r 0))
    (define (file-record-shape f-r)
      (list-ref f-r 1))
    (define (file-record-fillcolor f-r)
      (list-ref f-r 2))
    (define (file-record-pos f-r)
      (list-ref f-r 3))

    (define (file-exists? f chase?)
      (call/cc
       (lambda (continue)
       (with-exception-handler
           (lambda (ex)
             (display ex)(exit)(continue #f))
         (lambda () ((if chase?
                   cfs-file-status
                   cfs-file-link-status
                   ) f))))))

    (define (file-info f chase?)
      ((if chase?
         cfs-file-status
         cfs-file-link-status) f))
    (define (file-info:inode f)
      (cfs-file-inode f))
    (define (file-info:device f)
      (cfs-file-device f))
    
    (define (lwf-make-dev-plus-inode path)
      (if (file-exists? path #f)
       (let ((f-i (file-info path #f)))
        (string-append "id_"
                       (number->string (file-info:device f-i))
                       "_"
                       (number->string (file-info:inode  f-i))))
       (string-append "\"dangling_" 
                      (escape-like-write-without-quotes path)
                      "\""
                      )))
    
    
    (define (inode-filename-table-insert-or-update  full-path basename pos)
      (define (combine wrapped-name-1 wrapped-name-2)
        (string-append wrapped-name-1 "\n---\n" wrapped-name-2))
      (define my-id-dev-inode (lwf-make-dev-plus-inode full-path))
      (define wrapped-name (wrap-string-hack basename))
      ;; (display "inserting:") (display full-path) (display " ") (display my-id-dev-inode) 
      (let* ((cunzai (hash-table-ref/default inode-filename-table my-id-dev-inode #f))
             (to-insert (if cunzai
                            (make-file-record (combine (file-record-description cunzai)
                                                       wrapped-name)
                                              (file-record-shape cunzai)
                                              (file-record-fillcolor cunzai)
                                              (file-record-pos cunzai))
                            (make-file-record wrapped-name
                                              (select-node-shape full-path)
                                              (select-node-fillcolor full-path)
                                              pos))))
          (hash-table-set! inode-filename-table
                      my-id-dev-inode
                      to-insert)))

    (define (print-nodes)
      (hash-table-for-each
       (lambda (k v)
         (display (format-node k
                               (file-record-description v)
                               (file-record-shape v)
                               (file-record-fillcolor v)
                               (file-record-pos v)))
         (newline))
       inode-filename-table))

    (define (format-node node-id label shape fillcolor pos)
      (string-append
       node-id "  [label="
       (escape-like-write label)
       " , shape=\""
       shape
       "\" , fillcolor=\""
       fillcolor
       "\" "
       pos
       ", fontsize=7, margin=\"0,0!\", width=0.01, height=0.01] //  " ))

    (define (filesystem-boundary? a b)
      (let ((retval (and (not (= 0 (string-length a)))
                       (not (= 0 (string-length b)))
                       (not (= (file-info:device (file-info a #f))
                               (file-info:device (file-info b #f)))))))
        retval))

    (define (should-stop-at? full-path parent)
;      (display "stop? ") (display full-path) 
      (let ((retval (or (let ((r (not (file-directory? full-path #f))))
                         ;; (display "debug:not dir?: ") (display r) (newline)
                         r)
                       (and config-stop-at-filesystem-boundaries
                          (filesystem-boundary? parent full-path))
                       (and config-stop-at-repos
                          (directory-repo? full-path))
                       (and config-stop-at-projects
                          (directory-project? full-path))
                       (and config-stop-at-filesystem-boundaries
                          (directory-deliberately-terminate? full-path))
                       (and config-stop-at-maildir
                          (directory-maildir? full-path))
                       ;; (directory-album? full-path)
                       )))
       ;; (display "debug:")(display retval)
        retval))

    (define (directory-semantic-type path)
      (let ((retval (cond ((not (file-directory? path #f)) 'ordinary)
                          ((directory-repo? path) 'repo)
                          ((directory-project? path) 'project)
                          ((directory-album? path) 'album)
                          (else 'unknown))))
        ;; (display "\ndirectory type: ") (display retval) (newline)
        retval))

    (define (identity x )
      x)
    
    (define (album-filter l dotfiles)
      (let* ((files (directory-only-files l dotfiles))
             (dirs (directory-only-directories l dotfiles))
             (ntrunc (min (length+ files) config-album-show-max-files))
             (truncated-files (take files ntrunc)))
        (append dirs truncated-files)))
    
    (define (get-children-filter symbol)
      (case symbol
        ((album) album-filter)
        (else directory-files-without-ignores)))

    (define (file-directory? f chase?)
      ;; (display "\nfile-directory?:f:") (display f) (newline)
      ;; (display "\nfile-directory?:chase?:") (display chase?) (newline)
      (let* ((c (if chase?
                               cfs-file-status
                               cfs-file-link-status))
             ;; (discard1 (begin (display "tool:") (display c) (newline)))
             (retval (cfs-file-directory? (c f))))
        ;; (display "file-directory?retval:") (display retval) (newline)
        ;; (display "debug: ") (display (cfs-file-directory? (cfs-file-link-status "/home/lockywolf"))) (display "marker") (newline)
        retval
        ))
    
    (define (walk-tree-and-print-edges parent file)
      ;; (define full-path
      ;;   (call-with-current-continuation
      ;;    (lambda (continue)
      ;;    (with-errno-handler*
      ;;     (lambda (errno packet)
      ;;          (display "caught error")(newline)
      ;;          (display "errno=") (display errno) (newline)
      ;;          (display "packet=") (display packet) (newline)
      ;;          (display "parent=") (display parent) (newline)
      ;;          (display "file=") (display file) (newline)
      ;;          (display "backtrace:") (newline)
      ;;          (preview) (newline)
      ;;          ;(exit)
      ;;          )
      ;;     (lambda () (string-append (expand-file-name parent "/")
      ;;                          filesep
      ;;                          file))))))
      ;;      (display "processing:") (display full-path)(newline)
      (define full-path (if (equal? parent "")
                      file
                      (string-append parent filesep file)))
      (define pos (if (equal? parent "")
                ", pos=\"0,0!\", pin=true, root=true"
                ""))
      ;; (display "parent=") (display parent) (newline)
      ;; (display "file=") (display file) (newline)
      ;; (display "full-path=") (display full-path) (newline)
      ;; (display "pos=") (display pos) (newline)
      (inode-filename-table-insert-or-update full-path file pos)

      ;; (newline)
      (if (not (equal? parent ""))
         (display (format-edge parent full-path)))
      (if (should-stop-at? full-path parent)
         #t ;; (begin (display " // (terminal)") (newline))
         (let ((children ((get-children-filter (directory-semantic-type full-path))
                          full-path config-recurse-into-dotfiles)))
;           (display "\nmain loop: ") (display children) (newline)
           (map (lambda (f) (newline) (walk-tree-and-print-edges full-path f))
                children))))

    (define (escape-like-write-without-quotes str1)
      (let* ((str (escape-like-write str1))
             (lenstr (string-length str)))
        (substring str 1 (- lenstr 1))))

    ; overlap can be "false" (use Prism algorithm) or "scale", which preserves symmetry, but makes graphs larger
    (define (print-prologue title)
      (display "digraph \"directory tree\" {\n")
      ; https://stackoverflow.com/questions/3967600/how-to-prevent-edges-in-graphviz-to-overlap-each-other
      (display (string-append
                "graph [overlap=false, splines=true, size=45, dpi=300, rankdir=LR label=\"Filesystem map, Directory: '"
                (escape-like-write-without-quotes title)
                "', Generated: '"
                ;; (format-date "(~A) ~Y-~m-~dT~H:~M~Z" (date))
                (seconds->string (current-seconds))
                "'\"];\n")) ; 45 inches ~ISO-A0 ; dpi typographic
      (display "node [style=filled];\n"))

    (define (print-epilogue)
      (display "}\n"))

    (define (select-node-fillcolor full-path)
      (cond ((not (file-directory? full-path #f)) "white")
            ((directory-album? full-path) "pink")
            ((directory-deliberately-terminate? full-path) "red")
            ((directory-repo? full-path) "green")
            ((directory-project? full-path) "tomato")
            ((directory-maildir? full-path) "teal")
            (else "white")))

    (define (file-regular? f chase?)
      (cfs-file-regular? ((if chase?
                             cfs-file-status
                             cfs-file-link-status) f)))
    
    (define (select-node-shape filename)
      (cond
       ((and (file-directory? filename #f) (directory-repo? filename)) "doubleoctagon")
       ((and (file-directory? filename #f) (directory-project? filename)) "house")
       ((file-directory? filename #f) "ellipse")
       ((file-regular? filename #f) "box")
       (else "underline")))

    

    (define (wrap-string-hack str)
      (define hack-width 14)
      (if (< (string-length str) hack-width)
         str
         (string-append (substring str 0 hack-width)
                        "\n"
                        (wrap-string-hack (substring str hack-width (string-length str))))))
    (define read-symlink cfs-read-link)
    (define (format-edge source-name target-name)
      (string-append (lwf-make-dev-plus-inode source-name) ; (escape-like-write source-name)
                     " -> "
                     (lwf-make-dev-plus-inode target-name) ; (escape-like-write target-name)
                     " [arrowhead=\"vee\", arrowsize=0.318] // [len=3]"
                     (if (not (cfs-file-link? target-name))
                        ""
                        (string-append "\n"
                                       (lwf-make-dev-plus-inode target-name) ; (escape-like-write target-name)
                                       " -> "
                                       (lwf-make-dev-plus-inode (read-symlink target-name)) ; (escape-like-write (read-symlink target-name))
                                       " [color=\"cornflowerblue\"] // [len=3]"
                                       ))))
    ) ; begin
  ) ; define-library

(define (newermain . args)
  ;; (display (module? (analyze-module '(mindmap))))
  (apply (module-ref (analyze-module '(mindmap)) 'newmain) args)
  )



;; Local Variables:
;; mode: scheme
;; scheme-program-name: "chibi"
;; End:

