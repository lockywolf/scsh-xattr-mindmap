#!/usr/bin/chibi-scheme -rnewermain
;;>#!
;;>Time-stamp: <2021-04-18 21:26:31 lockywolf>
;;>#+description: A port of dir2dot by Darxus@ChaosReighs.com into scheme.
;;>#+author: lockywolf
;;>#+created: <2021-03-17 Wed>
;;>#+category: utilities
;;>#+tags: programming, mind, mind-map, scheme
;;>#+creator: Emacs 27.1/org-mode 9.4
;;> Usage:
;;> time filesystem-mindmap-scheme root > /tmp/dirtree.dot
;;> time circo -Tpng dirtree.dot > dirtree.png
;;> For me, for smaller trees (~500 nodes), circo works best. For large -- sfdp.
;;> Graphviz: https://www.graphviz.org/
;;> Requirements: (independentresearch xattr)
;;> Install from snow-fort.org
;;>
;;> Legend:
;;> Files: boxes
;;> File types: font colours
;;> Directory shapes: ordinary:oval, project:house, repo:doubleoctagon
;;> Fillcolor: incompletely displayed trees
;;>
;;> TODO:
;;> 4. Displaying images.
;;>
;;> Far TODO:
;;> 1. Music albums.
;;> 2. Other kinds of projects.
;;> 3. README.org

;;>!#


(import (chibi))
(import (chibi modules))
(import (meta))


(define-library (mindmap)
  (export newmain)
  (import
    ;; scheme-with-scsh
    ;; os-strings
    ;; (subset srfi-13 (string-join))
    ;; (subset i/o (set-port-text-codec!))
    ;; (subset text-codecs (utf-8-codec))
    ;; (subset tables (make-string-table table-ref table-set! table-walk))
    ;; (subset srfi-27 (random-integer))
    ;; (subset usual-commands (preview))
    (only (srfi 1) lset-intersection lset<= lset-difference remove length+ take
                   lset= filter)
    (chibi trace)
    (except (scheme small) file-exists?)
    (only (srfi 125) make-hash-table hash-table-ref/default hash-table-set! hash-table-for-each)
    (only (srfi 130) string-index-right string-cursor->index string-split
                     string-contains string-suffix?)
    (rename (prefix (chibi filesystem) cfs-))
    (only (chibi time) current-seconds seconds->string)
    (only (chibi process) process->output+error+status)
    (only (independentresearch xattr) lgetxattr)
    ;(only (srfi 115) rx regexp-matches?)
   )

  (begin

    (define config-album-show-max-files 5)
    (define config-recurse-into-dotfiles #f)
    (define config-stop-at-repos #t)
    (define config-stop-at-projects #t)
    (define config-stop-deliberately #t)
    ;;> Lets you use the user.lwf.mindmap.skip-grandchildren xattr on a directory
    ;;> to only traverse it one level deep.
    (define config-use-xattrs #t)
    (define config-stop-at-filesystem-boundaries #t)
    (define config-stop-at-maildir #t)
    (define config-stop-at-vdir #t)
    ;;> Sometimes your tree is just too big.
    ;;> Try skipping files altogether.
    (define config-ignore-all-files #t)
    (define config-no-labels #f)
    (define config-use-colour-names #t)
    ;;> https://www.mail-archive.com/bug-coreutils@gnu.org/msg11030.html
    ;;> a simplified version
    ;;> attributes (unsupported): 00=none 01=bold 04=underscore 05=blink 07=reverse
    ;;> Text: 30=black 31=red 32=green 33=yellow 34=blue 35=magenta 36=cyan 37=white
    ;;> Background (unsupported): 40=black 41=red 42=green 43=yellow 44=blue 45=magenta 46=cyan 47=white
    ;;> Extended Foreground: TODO # Text color coding: # 38;5;COLOR_NUMBER #
    ;;> Extended Background: unsupported # 48;5;COLOR_NUMBER
    
    
    (define (dircolor-spec->graphviz-color dircolor-spec)
      (define dirspec-gcolor-alist ;; implement themability if you want
        '(("30;" "black")
          ("31;" "red")
          ("32;" "green")
          ("33;" "yellow")
          ("34;" "blue")
          ("35;" "magenta")
          ("36;" "cyan")
          ("37;" "white")
          ("38;5;" "teal"))) ;; patches for extended support welcome
      #;(define color-bg-alist ;; implement this if you want
        '(("40;" "black")
          ("41;" "red")
          ("42;" "green")
          ("43;" "yellow")
          ("44;" "blue")
          ("45;" "magenta")
          ("46;" "cyan")
          ("47;" "white")
          ("48;5;" 'extended)))
      (list 'foreground (let ((r (assoc dircolor-spec dirspec-gcolor-alist
                                        (lambda (map-key dircolor-spec)
                                          (if (string-contains map-key dircolor-spec) #t #f)))))
                          (if r (cadr r) #f))))

    (define theme-system-colors
      (map (lambda (l-2-pat-spec)
             (list (list-ref l-2-pat-spec 0)
                   (dircolor-spec->graphviz-color
                    (list-ref l-2-pat-spec 1))
                   ))
           (map (lambda (x) (let ((s (string-split x "=")))
                         (list (substring (list-ref s 0) 1)
                               (string-append (list-ref s 1) ";"))))
                (filter
                 (lambda (x) (and (> (string-length x) 0) (string=? "*" (substring x 0 1))))
                 (string-split (let ((ans (get-environment-variable "LS_COLORS")))
                                 (if ans
                                    ans
                                    "")) ":")))))
    
    (define (select-node-fontcolor full-path)
      (let ((r (assoc full-path
                      theme-system-colors
                      (lambda (full-path map-key)
                        ;; (display "debug:") (display "map-key:") (display map-key) (display "full-path:") (display full-path) (newline)
                        (string-suffix? map-key full-path)))))
        (if r
           (cadadr r)
           "black")))


    (define (directory-files dir dotfiles?)
      (let ((files (lset-difference equal? (cfs-directory-files dir) (list "." ".."))))
        (if dotfiles?
         files
         (remove (lambda (x) (string=? "." (substring x 0 1))) files))))
    (define config-ignore-entirely-list
      (list "ltximg" "auto"))
    
    (define (directory-files-without-ignores . o)
      (let ((ans (apply directory-files o)))
        ;; (display "directory-files-without-ignores:o ") (display o) (newline)
        ;; (display "directory-files-without-ignores:ans ") (display ans) (newline)
        (lset-difference equal? ans
                         config-ignore-entirely-list))) ; was "cache"

    (define (newmain args)
      (define actual-directory (if (< (length+ args) 3)
                             "."
                             (list-ref args 2)))
      ;;  (display "debug: args: ") (display args) (newline)
      ;; (display "debug:length-args:")(display (length args)) (newline)
      ;; (define actual-directory (absolute-file-name
      ;;                                (expand-file-name (if (< (length+ args) 2)
      ;;                                                     "."
      ;;                                                     (list-ref args 1)))))
      (if (not (file-exists? actual-directory #f))
         (error "Target file/directory does not exist."))
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
      (not (null? (lset-intersection
                   ;; regexp-matches?
                   (lambda (suf str-body) (string-suffix? suf str-body))
                   detector-list (directory-files path #t)))))

    (define directory-repo-marker-list (list ".git" ".svn" "CVS"))

    (define (directory-repo? path)
      (directory-something? path directory-repo-marker-list))

    ;;> strings (are valid sre expressions) match in full,
    ;;> or just write SRE
    (define directory-project-marker-list (list "Makefile"
                                           "configure.ac"
                                           "Makefile.am"
                                           "configure"
                                           "index.html"
                                           "readme.org"
                                           "CMakeLists.txt"
                                           "readme.txt"
                                           "README.txt"
                                           "README.md"
                                           "src" ;; a bit dangerous
                                           ".aup"
                                           ;; (rx (: (* any) "aup"))
                                           )) ; .*aup

    (define (directory-project? path)
      (directory-something? path directory-project-marker-list))

    (define directory-deliberately-terminate-list (list "LWF-MINDMAP-TERMINATE"))

    (define (directory-deliberately-terminate? path)
      (directory-something? path directory-deliberately-terminate-list))

    (define directory-force-continue-list
      (list "LWF-MINDMAP-FORCE-CONTINUE"))

    (define (directory-force-continue? path)
      (directory-something? path directory-force-continue-list))
    
    (define directory-skip-grandchildren-list
      (list "LWF-MINDMAP-SKIP-GRANDCHILDREN"))

    (define (directory-skip-grandchildren? path)
      (or (directory-something? path directory-skip-grandchildren-list)
          (and config-use-xattrs
           (cfs-with-directory path
                             (lambda ()
                               ;; (display "debug:xattr:path=") (display path) (newline)
                               (string=?
                                (let* (
                                       ;; (r1 (process->output+error+status
                                       ;;    "getfattr --no-dereference --only-values -n user.lwf.mindmap.skip-grandchildren ."))
                                       ;; (r2 (cadr r1))
                                       ;; (r (car r1))
                                       (x (lgetxattr "." "user.lwf.mindmap.skip-grandchildren"))
                                       )
                                  ;; (display "debug:") (display "path=")
                                  ;; (display path) (newline) (display "answer=")
                                  ;; (display "response=")(write r) (newline)
                                  (if x
                                     x
                                     ""))
                                "true")
                               ;; (system? "getfattr" "--no-dereference" "--only-values" "-n" "user.lwf.mindmap.skip-grandchildren" ".")
                               )))))

    (define extensions-list-image
      (list ".jpg" ".JPG" ".jpeg" ".JPEG" ".tif" ".TIF" ".tiff" ".TIFF" ".gif" ".GIF" ".webp"))

    (define extensions-list-video
      (list ".mp4" ".avi" ".mpg" ".mpeg" ".webm" ".mov" ".ogm"))

    (define extensions-vcard
      (list ".vcf" ".VCF"))

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
       (let ((dot (- (string-cursor->index fname (string-index-right fname #\.)) 1))
             (slash (- (string-cursor->index fname (string-index-right fname #\/)) 1)))
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
             ;;(d1 (begin (display d-files) (newline) (display d-cur-exts)(newline) (display image-extensions-list) (newline)))
             (retval (and (not (null? d-cur-exts))
                        (lset<= string=? d-cur-exts
                                (append extensions-list-image
                                        extensions-list-video)))))
        ;;(display retval)(newline)
        retval))

    (define (directory-maildir? path)
;      (display "directory-maildir?: ") (display path) (newline)
      (let ((dirs (directory-only-directories path #f))) ; no ignores, hidden files and symlinks
        ;;(display files)
        (lset= equal? dirs (list "cur" "new" "tmp"))))
    (define (directory-vdir? path)
      (directory-something? path extensions-vcard))
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
      (make-hash-table string=?);
      ;; (make-string-table)
      )

    (define (make-file-record description shape fillcolor pos fontcolor)
      (list description shape fillcolor pos fontcolor))
    (define (file-record-description f-r)
      (list-ref f-r 0))
    (define (file-record-shape f-r)
      (list-ref f-r 1))
    (define (file-record-fillcolor f-r)
      (list-ref f-r 2))
    (define (file-record-pos f-r)
      (list-ref f-r 3))
    (define (file-record-fontcolor f-r)
      (list-ref f-r 4))

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
                                              (file-record-pos cunzai)
                                              (file-record-fontcolor cunzai))
                            (make-file-record wrapped-name
                                              (select-node-shape full-path)
                                              (select-node-fillcolor full-path)
                                              pos
                                              (select-node-fontcolor full-path)))))
          (hash-table-set! inode-filename-table
                      my-id-dev-inode
                      to-insert)))

    (define (print-nodes)
      (hash-table-for-each
       (lambda (k v)
         (display (format-node k v))
         (newline))
       inode-filename-table))

    (define (format-node node-id entry)
      (string-append
       node-id
       "  ["
       "label=\""
       (if (not config-no-labels)
          (escape-like-write-without-quotes (file-record-description entry))
          "")
       "\", "
       " shape=\""
       (file-record-shape entry)
       "\" , fillcolor=\""
       (file-record-fillcolor entry)
       "\" "
       (file-record-pos entry)
       ", fontcolor=\""
       (file-record-fontcolor entry)
       "\""
       ", fontsize=7, margin=\"0,0!\", width=0.01, height=0.01] //  " ))

    (define (filesystem-boundary? a b)
      (let ((retval (and (not (= 0 (string-length a)))
                       (not (= 0 (string-length b)))
                       (not (= (file-info:device (file-info a #f))
                               (file-info:device (file-info b #f)))))))
        retval))

    (define (should-stop-at? full-path parent)
      ;; (display "stop? ") (display full-path) 
      (let ((retval (and (not (directory-force-continue? full-path))
                     (or (let ((r (not (file-directory? full-path #f)))) r)
                       (and config-stop-at-vdir
                          (directory-vdir? full-path))
                       (and config-stop-at-filesystem-boundaries
                          (filesystem-boundary? parent full-path))
                       (and config-stop-at-repos
                          (directory-repo? full-path))
                       (and config-stop-at-projects
                          (directory-project? full-path))
                       (and config-stop-deliberately
                          (directory-deliberately-terminate? full-path))
                       (and config-stop-at-maildir
                          (directory-maildir? full-path))
                       (and config-stop-deliberately
                          (not (string=? parent ""))
                          (directory-skip-grandchildren? parent))
                       ;; (directory-album? full-path)
                       ))))
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
             (exists? (file-exists? f chase?))
             (retval (if exists?
                        (cfs-file-directory? (c f))
                        #f)))
        ;; (display "file-directory?retval:") (display retval) (newline)
        ;; (display "debug: ") (display (cfs-file-directory? (cfs-file-link-status "/home/lockywolf"))) (display "marker") (newline)
        retval
        ))
    
    (define (ignore-by-config? full-path)
      (cond ((and config-ignore-all-files (not (file-directory? full-path #t))) #t)
            (else #f)))
    
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
      (if (ignore-by-config? full-path)
         'ignored
         (begin
           (inode-filename-table-insert-or-update full-path file pos)
           (if (not (equal? parent ""))
              (begin (display "\n\n // debug:full-path=") (display full-path) (newline)
                     (display (format-edge parent full-path)) (newline)
                     ))
           (if (should-stop-at? full-path parent)
              'terminal ;; (begin (display " // (terminal)") (newline))
              (let ((children ((get-children-filter (directory-semantic-type full-path))
                               full-path config-recurse-into-dotfiles)))
                ;; (display "\nmain loop: ") (display children) (newline)
                (map (lambda (f)  (walk-tree-and-print-edges full-path f))
                     children))))))

    (define (escape-like-write-without-quotes str1)
      (let* ((str (escape-like-write str1))
             (lenstr (string-length str)))
        (substring str 1 (- lenstr 1))))

    ; overlap can be "false" (use Prism algorithm) or "scale", which preserves symmetry, but makes graphs larger
    (define (print-prologue title)
      (display "digraph \"directory tree\" {\n")
      ; https://stackoverflow.com/questions/3967600/how-to-prevent-edges-in-graphviz-to-overlap-each-other
      (display (string-append
                "graph [overlap=false, splines=false"
                ;; ", size=\"32.0,45.0\""
                ", ratio=\"0.4\""
                ", size=\"100.0!\""
                ", dpi=\"300.0\", rankdir=LR label=\"Filesystem map, Directory: '"
                (escape-like-write-without-quotes title)
                "', Generated: '"
                ;; (format-date "(~A) ~Y-~m-~dT~H:~M~Z" (date))
                (let* ((r (seconds->string (current-seconds)))
                       (l (string-length r))
                       (rv (substring r 0 (- l 1)))) rv)
                "'\"];\n")) ; 45 inches ~ISO-A0 ; dpi typographic
      (display "node [style=filled];\n"))

    (define (print-epilogue)
      (display "}\n"))

    (define (select-node-fillcolor full-path)
      (cond ((not (file-directory? full-path #f)) "white")
            ((directory-album? full-path) "pink")
            ((or (directory-deliberately-terminate? full-path)
                (directory-skip-grandchildren? full-path)) "red")
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
    #;(define (absolutize-symlink-path symlink-path)
      (define bad-path (read-link symlink-path))
      (define (absolute-path? path)
        (and (> (string-length path) 0)
           (= "/" (substring path 0 1))))
      (define (file-directory ))
      (if (absolute-path? bad-path)
         bad-path
         
         )
      )
    (define read-symlink cfs-read-link)
    (define (format-edge source-name target-name)
      (string-append (lwf-make-dev-plus-inode source-name) ; (escape-like-write source-name)
                     " -> "
                     (lwf-make-dev-plus-inode target-name) ; (escape-like-write target-name)
                     " [arrowhead=\"vee\", arrowsize=0.318] // [len=3]"
                     " // source=" source-name
                     " // target=" target-name
                     (if (not (cfs-file-link? target-name))
                        ""
                        (string-append "\n"
                                       (lwf-make-dev-plus-inode target-name) ; (escape-like-write target-name)
                                       " -> "
                                       (lwf-make-dev-plus-inode
                                        (cfs-with-directory target-name
                                                                (lambda () (cfs-current-directory)))
                                        #;(read-symlink target-name)
                                        ) ; (escape-like-write (read-symlink target-name))
                                       " [color=\"cornflowerblue\"] // [len=3]"
                                       " // link-to=" (read-symlink target-name)
                                       ))))
    ) ; begin
  ) ; define-library

(define (newermain . args)
  ;; (display (module? (analyze-module '(mindmap))))
  (apply (module-ref (analyze-module '(mindmap)) 'newmain) args)
  )



;; Local Variables:
;; mode: scheme
;; scheme-program-name: "chibi-scheme"
;; End:

