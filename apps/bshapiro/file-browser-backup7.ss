(load "/home/hci/dynapad/workspace.ss")
(load "/home/hci/dynapad/arrange.ss")
(load "/home/hci/dynapad/bbox.ss")
(require (lib "process.ss"))
(require (lib "file.ss"))
(require (lib "trace.ss"))
(require (lib "date.ss"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The icon class groups together an icon and its associated text objects ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define icon%
  (class group%
    (init-field icon             ;;a visual representation for a file
                file-path        ;;a text object that stores the absolute path of the file
                file-attribs)    ;;a list of text objects which store various attributes of the file

    (public get-icon get-file-path get-file-attribs)
    (override bbox)

    ;;The bounding box for the icon does not include the bounding box of the text objects
    ;;beneath the icon
    (define (bbox)
      (send icon bbox))

    (define (get-icon)
      icon)

    (define (get-file-path)
      file-path)

    (define (get-file-attribs)
      file-attribs)

    (super-instantiate (dynapad (append (list icon) (list file-path) file-attribs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The dir-object class stores information about the representation of a directory in the file browser ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define dir-object%
  (class object%
    (init-field dir-name              ;;the name of the directory
                subdirs               ;;a string list of the subdirectories in the directory
                subdir-boxes          ;;a list of the subdirectory rectangles
                bbox                  ;;the bounding box of the directory box
                file-paths            ;;a list of all the text object non-image file paths
                icon-text-groups      ;;a list of icon% objects associated with files in the directory
                dir-icon              ;;the icon% object for the directory
                dir-color)            ;;the color of the directory box

    (public get-dir-name get-bbox get-subdirs
            get-icon-text-groups get-text-objs get-file-icons
            set-icon-text-groups get-color get-shadow-offset arrange-icons-by-size arrange-icons-by-time
            arrange-icons-by-name get-dir-icon get-subdir-boxes)

    (define (get-dir-name)
      dir-name)

    (define (get-dir-icon)
      dir-icon)

    (define (get-subdir-boxes)
      subdir-boxes)

    (define (get-icon-text-groups)
      icon-text-groups)

    (define (get-text-objs)
      file-paths)

    (define (get-bbox)
      bbox)

    (define (get-subdirs)
      subdirs)

    (define (get-color)
      dir-color)

    ;;Returns the #pixels the shadow for the directory box should be offset southeast of the directory box
    (define (get-shadow-offset)
      (let ((coord-list (send (get-bbox) coords)))
        (* (- (third coord-list) (first coord-list)) 0.02)))

    (define (get-file-icons)
      (map (lambda (icon-text)
             (send icon-text get-icon)) icon-text-groups))

    (define (get-file-icons2 icon-text-groupings)
      (map (lambda (icon-text)
             (send icon-text get-icon)) icon-text-groupings))

    ;;fix: get rid of this function
    (define (get-file-paths)
      (map (lambda (icon-text)
             (send icon-text get-file-path)) icon-text-groups))

    (define (set-icon-text-groups icons)
      (set! icon-text-groups icons))

    ;;Arrranges the files and subdirectories in a directory in alphabetical order
    (define (arrange-icons-by-name)
      (let ((sorted-file-icons (get-file-icons2 (mergesort icon-text-groups alphabet-less-than?))))
        (delete-directory-icon dir-name)
        (arrange-in-grid-onto-object-with-spread (append subdir-boxes sorted-file-icons) bbox 1.5)))

    ;;Arrranges the files and subdirectories in a directory in order of their size
    (define (arrange-icons-by-size)
      (let ((sorted-file-icons (get-file-icons2 (mergesort icon-text-groups size-less-than?))))
        (delete-directory-icon dir-name)
        (arrange-in-grid-onto-object-with-spread (append subdir-boxes sorted-file-icons) bbox 1.25)))

    ;;Arrranges the files and subdirectories in a directory in order of the time they were last modified
    (define (arrange-icons-by-time)
      (let ;;((sorted-file-icons (get-file-icons2 (mergesort icon-text-groups time-less-than?))))
          ((sorted-file-icons (mergesort icon-text-groups time-less-than?))
           (rect null)
           (dir-box null))

        ;;(send bbox fill dir-color)  ;;filling the dir box here doesn't do anything
        ;;erasing directory box here wipes everything out
        ;;(set! dir-box (make-object rect% dynapad (send bbox coords)))
        ;;(send dir-box pen dir-color)
        ;;(send dir-box fill dir-color)
        (display "subdir boxes: ")
        (display subdir-boxes)
        (newline)
        (do ((subdir-boxes subdir-boxes (cdr subdir-boxes)))
          ((null? subdir-boxes) null)
          ;;(set! rect (make-object rect% dynapad (send (car subdir-boxes) coords)))
          ;;(send rect fill "yellow")
          (display "subdir box coords: ")
          (display (send (car subdir-boxes) coords))
          (newline))
        (display "sorted file icons: ")
        (display sorted-file-icons)
        (newline)
        (display "bbox: ")
        (display bbox)
        (newline)
        (display "bbox coords: ")
        (display (send bbox coords))
        (newline)
        (do ((sorted-file-icons sorted-file-icons (cdr sorted-file-icons)))
          ((null? sorted-file-icons) null)
          (display "icon coords: ")
          (display (send (car sorted-file-icons) bbox))
          (newline))
        ;;fix: deleting causes an error with sch_position:
        ;;sch_position: expects argument of type <dynaobject%>; given #f
        ;;(delete-directory-icon dir-name)

        ;;arranging subdir-boxes by itself or in combination with file icons causes an error
        ;;error in raise method of dynaobject
        ;;raise method called by arrange-in-grid-onto-object
        ;; sch_raise: failed, given #<c-pointer:rect> #<c-pointer:rect>
        ;;error in dynapad-c-api.ss at this point
        ;;((abovethis) (sch_raise cptr (send abovethis get-cptr)))))
        (arrange-in-grid-onto-object-with-spread (append subdir-boxes sorted-file-icons) bbox 1.25)
        ;;(send bbox fill dir-color)  ;;filling the dir box here causes the dir box to not be filled
        ;;erasing directory box here wipes everything out
        ;;(set! dir-box (make-object rect% dynapad (send bbox coords)))
        ;;(send dir-box pen dir-color)
        ;;(send dir-box fill dir-color)
        ))

    (super-instantiate ())))

(define dir-obj-list null)                                    ;;list of directory objects
(define transparent-box null)
(define temp-path (find-system-path->string 'temp-dir))               ;;the designated operating system temporary directory
(define thumbnails-path (build-path->string temp-path "thumbnails"))  ;;the location of the thumbnails that will be generated for non-image files
(define select-box (make-object rect% dynapad))               ;;a box that is drawn around a file when it is selected
(send select-box pen "cyan")
(define max-thumbnail-width 300)
(define open-file-text null)
;;(set! open-file-text (make-object text% dynapad "opening file ..."))
;;(send open-file-text transparency 0.75)
;;(send open-file-text position (list 0 0 0.5))
(define top-level-directory "")                               ;;the parent directory of all the directories that are drawn

;;association list where each list element is a pair of the form (application extension application)
(define app-extensions (list (list "html" "netscape") (list "pdf" "acroread") (list "ps" "ghostview")
                             (list #f "emacs") (list "ss" "drscheme") (list "gif" "xv") (list "xbm" "xv")
                             (list "png" "xv") (list "jpg" "xv") (list "scm" "drscheme") (list "htm" "netscape")
                             ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arguments:     two icon text objects                                                    ;;
;; Postcondition: Returns true if the file associated with icon1 was modified earlier than ;;
;;                the file associated with icon2. Otherwise returns false.                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (time-less-than? icon1 icon2)
  (let ((time1 (file-or-directory-modify-seconds (send (send icon1 get-file-path) text)))
        (time2 (file-or-directory-modify-seconds (send (send icon2 get-file-path) text))))
    (< time1 time2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arguments:     two icon text objects                                                   ;;
;; Postcondition: Returns true if the file associated with icon1 is smaller than the file ;;
;;                associated with icon2. Otherwise, returns false                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (size-less-than? icon1 icon2)
  (let ((size1 (file-size (send (send icon1 get-file-path) text)))
        (size2 (file-size (send (send icon2 get-file-path) text))))
    (< size1 size2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arguments:     two icon objects                                                         ;;
;; Postcondition: Returns true if the filename associated with icon1 comes alphabetically  ;;
;;                before the filename associated with icon2. Otherwise returns false.      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (alphabet-less-than? icon1 icon2)
  (let ((file-path1 (send (send icon1 get-file-path) text))
        (file-path2 (send (send icon2 get-file-path) text)))
    (string<? file-path1 file-path2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Argument:      The absolute path of a file                                                                   ;;
;; Postcondition: Opens the application associated with the argument file with the argument file as an argument ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (open-app-with-file file)
  (let* ((application (cadr (assoc (filename-extension file) app-extensions)))
         (dynapad-bbox (send dynapad bbox))
         (mid-y (+ (/ (- (fourth dynapad-bbox) (second dynapad-bbox)) 2) (second dynapad-bbox))))

    ;;fix: open-file-text is drawn too late because the position method is non-blocking while
    ;;the system call method is blocking
    ;;(center-text open-file-text (first dynapad-bbox) (third dynapad-bbox) mid-y 0.5)

    ;;netscape requires a file argument in the format file:/file-name. For example, to open the file
    ;;/home/index.html, you would have to type netscape file:/home/index.html
    (if (equal? application "netscape")
        (system (string-append application " file:" file " &"))
        (system (string-append application " " file " &")))

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Arguments: lst: a list of objects to arrange, obj: the object to arrange onto,       ;;
;;           spread: a measure of the spacing between the objects                      ;;
;;Note:      This function is identical to arrange-in-grid-onto-object in "arrange.ss" ;;
;;           except it includes a spread argument                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (arrange-in-grid-onto-object-with-spread lst obj spread)
  (arrange-objs-in-grid lst (send obj bbox) spread "nw")
  (arrange-objs-in-bbox  lst (send obj bbox) 0.8)
  (for-each
   (lambda (id)
     (send id raise obj))
   lst)
  (send obj maxsize (list 62500 #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arguments:     text-obj: a text object, left-x: left x-coordinate, right-x: right x-coordinate ;;
;;                y: y-coordinate of text, zoomf: zoom factor of text                             ;;
;; Precondition:  (- right-x left-x) >= width of bounding box of text-obj                         ;;
;;                the text-obj must be scaled at zoomf                                            ;;
;; Postcondition: Centers the text object between the two argument x-coordinates                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (center-text text-obj left-x right-x y zoomf)
  (let* ((bbox (send text-obj bbox))
         (text-width (- (third bbox) (first bbox)))
         (boundary-width (- right-x left-x))
         (start-x (+ left-x (/ (- boundary-width text-width) 2))))
    (send text-obj position (list start-x y zoomf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Arguments:     filepath: The absolute path to a file                            ;;
;;Postcondition: Returns the time on which a file was last modified in the format ;;
;;               Month, Day, Year Hour: Minutes: Seconds AM/PM                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (modify-time filepath)

  ;;We remove the day of the week from the beginning of the modify-string by looking for the
  ;;comma that separates the day of the week from the rest of the date and truncating the
  ;;day of the week
  (define (iterate-through-string modify-string index)
    (if (>= index (string-length modify-string))
        "modify-time: Error: unable to return modify time \n"
        (if (equal? (string-ref modify-string index) #\,)
            ;;The starting index is (+ index 2) because we skip over a comma and a space
            (substring modify-string (+ index 2) (string-length modify-string))
            (iterate-through-string modify-string (+ index 1)))))

  ;;Note: seconds->date converts a number in seconds to a date of the format
  ;;Day of week, Month, Day, Year Hour: Minutes: Seconds AM/PM
  (iterate-through-string (date->string (seconds->date (file-or-directory-modify-seconds filepath)) #t) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Argument:    directory: the absolute path of a directory,                                       ;;
;;              file-list: a list of files in the directory for which thumbnails should be created ;;
;; Side Effect: Creates a thumbnail for each of the filenames in file-list                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (create-thumbnails directory file-list)
  ;;Remove the slash from the directory path to convert it from an absolute pathname to a relative pathname
  (set! directory (substring directory 1 (string-length directory)))
  (do ((file-list file-list (cdr file-list)))
    ((null? file-list) null)
    ;;fix: uncomment
    ;;(system (string-append "cp /home/alvin/dynapad/pad/images/padicon.gif" (build-path->string thumbnails-path directory (car file-list))))
    ;;(system (string-append "./KPreviewer 300 " (build-path->string directory (car file-list))
    ;;                       (build-path->string thumbnails-path directory (car file-list)) ))))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Precondition: The user has finished using the file browser                       ;;
;; Side Effect:  Deletes all the thumbnails that were generated by the file browser ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (delete-thumbnails)
  (system (string-append "rm -r " thumbnails-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arguments:     dir-color: a directory box color                      ;;
;; Postcondition: returns the color of a directory's subdirectory boxes ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (alternate-color dir-color)
  (if (equal? dir-color "grey")
      "light grey"
      "grey"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Argument:      The absolute path of a directory                   ;;
;; Postcondition: Returns the size of all the files in the directory ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (directory-size directory)
  (define (dir-size-helper directory sum)
    (let*  ;;a list of all the subdirectories in the directory
        ((directories (filter
                       (lambda (item)
                         (directory-exists?
                          (build-path->string directory item )))
                       (directory-list->string directory )))
         ;a list of all the files in the directory excluding any subdirectories
         (file-list (filter
                     (lambda (file)
                       (not (directory-exists? (build-path->string directory file))))
                     (directory-list->string directory))))

      ;;append the directory path to all the filenames
      (set! file-list (map (lambda (file)
                           (build-path->string directory file)) file-list))

      ;;append the directory path to all the subdirectory names
      (set! directories (map (lambda (relative-dir)
                               (build-path->string directory relative-dir)) directories))

      ;;sum the sizes of all the files in the directory
      (do ((file-list file-list (cdr file-list)))
        ((null? file-list) sum)
        (set! sum (+ sum (file-size (car file-list)))))

      ;;recursively sum the sizes of all the subdirectories in the directory
      (do ((directories directories (cdr directories)))
        ((null? directories) sum)
        (set! sum (dir-size-helper (car directories) sum)))))

  (dir-size-helper directory 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Argument:      A number that is given as a string                                             ;;
;; Postcondition: Returns the argument string with commas inserted in between every three digits ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (insert-commas number-string)
  (let ((new-string "")
        (counter 0))    ;;counts off every three characters of the string

    (do ((i (- (string-length number-string) 1) (- i 1)))
      ((< i 0) new-string)
      (set! counter (+ counter 1))
      (set! new-string (string-append (substring number-string i (+ i 1)) new-string))
      (if (and (equal? (modulo counter 3) 0) (not (equal? i 0)))
          (set! new-string (string-append "," new-string))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Argument:    directory: the absolute path of a directory           ;;
;; Author:      Jim Hollan                                            ;;
;; Side Effect: Draws all the files and subdirectories in a directory ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (draw-subdirectories directory)
  (let ((save-view (send dynapad view )))
    (set! top-level-directory directory)
    (system (string-append "mkdir -p " thumbnails-path))
    (draw-subdirectories-helper directory "grey" "light grey")
    (send dynapad moveto
         (list (car save-view) (cadr save-view) (caddr save-view )))))   ;;fix: missing 0.75 factor

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arguments:   directory: the absolute path of the directory, dir-color: the color of the directory box, ;;
;;              subdir-color: the color of subdirectory boxes                                             ;;
;; Note:        Based off of Jim Hollan's directory browsing code                                         ;;
;; Side Effect: Recursively draws a directory structure                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (draw-subdirectories-helper directory dir-color subdir-color)
  (let*  ((subdir-boxes null)   ;;subdirectory boxes
          (ilist null)          ;;image list
          (box (make-object rect% dynapad (send dynapad bbox )))    ;;box around directory
          (path (make-object text% dynapad directory ))
          (file-path null)
          (file-size-text null)
          (modify-text null)
          (file-paths null)    ;;a list of text objects which display the absolute path of all images in the directory
          (file-icons null)
          (file-box null)
          (text-zoomf 0)
          (icon-bbox null)
          (thumbnail null)
          (shadow null)
          (coord-list null)
          (shadow-offset 0)
          (shadow-offset-fraction 0.02)
          (relative-dir-path "")
          (subdir-box-group null)
          (dir-icon null)
          (icon-text-groups null)

          ;;a list of all the subdirectories in the directory
          (directories (filter
                              (lambda (item)
                               (directory-exists?
                                 (build-path->string directory item )))  ;;build-path->string concatenates paths together
                               (directory-list->string directory )))

                       (image-names (hires-list directory ))    ;;returns a list of the images in the directory

          ;;a list of all the files in all the subdirectories of the directory
          (subdir-files (map (lambda (subdir)
                               (directory-list->string (build-path->string directory subdir)))
                             directories))

          ;;a list of all the non image files in the directory not including any non image files in any subdirectories
          (non-image-file-list (filter
                                (lambda (file)
                                  (and (not (member file image-names)) (not (member file subdir-files))  (not (member file directories))))
                                (directory-list->string directory))))

          ;;fill in the directory box
          (send box fill dir-color)
          (send box pen dir-color)

          ;;create an image object list from the list of image names in the directory
          (for-each
           (lambda (image)
                    (set! ilist (cons
                                 (make-object image% dynapad
                                   (build-path->string directory image ))
                                 ilist )))
             image-names)

           ;;create a box for each of the subdirectories in the directory
           (for-each
            (lambda (dir )
              (set! subdir-boxes (cons
                           (make-object rect% dynapad
                             (send dynapad bbox ))subdir-boxes )))
              directories)

         ;;color all the subdirectory boxes
         (do ((subdir-boxes subdir-boxes (cdr subdir-boxes)))
           ((null? subdir-boxes) subdir-boxes)
           (send (car subdir-boxes) pen subdir-color)
           (send (car subdir-boxes) fill subdir-color))

         ;;create a thumbnail for each of the non-image files in the directory
         (create-thumbnails directory non-image-file-list)

         ;;create an icon for each of the non-image files in the directory
         (do ((non-image-file-list non-image-file-list (cdr non-image-file-list)))
           ((null? non-image-file-list) null)
           (set! file-box (make-object rect% dynapad (send dynapad bbox)))
           (send file-box fill "#0096ff")
           (send file-box pen "#0096ff")
           ;;(set! path-icon-list (append path-icon-list (list (cons (build-path->string directory (car non-image-file-list)) file-box))))
           (set! file-icons (append file-icons (list file-box))))

         ;;arrange file icons and subdirectory boxes onto the directory box
         ;;note: text objects are not arranged with the file icons but are attached to the icons after they've been arranged
         (if (or (not (null? subdir-boxes))
                 (not (null? ilist))
                 (not (null? file-icons)))
             (arrange-in-grid-onto-object-with-spread (append subdir-boxes ilist file-icons) box 1.25))

         ;;create a shadow for each subdirectory box by drawing a blackened copy of the subdirectory box behind the subdirectory box
         ;;and slightly offset southeast of the subdirectory box
         (do ((subdir-boxes subdir-boxes (cdr subdir-boxes)))
           ((null? subdir-boxes) null)
           (set! shadow (make-object rect% dynapad))
           (send shadow fill "black")
           (send shadow pen "black")
           (set! coord-list (send (car subdir-boxes) coords))

           ;;the shadow-offset is two percent of the width of the directory box
           (set! shadow-offset (* (- (third coord-list) (first coord-list)) shadow-offset-fraction))
           (send shadow coords (list (+ (first coord-list) shadow-offset) (- (second coord-list) shadow-offset)
                                     (+ (third coord-list) shadow-offset) (- (fourth coord-list) shadow-offset)))
           (set! subdir-box-group (make-object group% dynapad (list (car subdir-boxes) shadow))))

         ;;fix: uncomment this line
         ;;(set! icon-text-groups (append icon-text-groups (list (make-object icon% subdir-box-group null null)))))

         ;;Beneath each non-image file icon, draw various file attributes
         (do ((non-image-file-list non-image-file-list (cdr non-image-file-list))
              (file-icons file-icons (cdr file-icons)))
           ((null? non-image-file-list) null)

           ;;draw the absolute path of the file
           (set! icon-bbox (send (car file-icons) bbox))
           (set! file-path (make-object text% dynapad (build-path->string directory (car non-image-file-list))))

           ;;the absolute path is scaled to the width of the file icon
           (set! text-zoomf (/ (bbwidth icon-bbox) (bbwidth (send file-path bbox ))))
           (send file-path position (list (car (send (car file-icons) bbox ))
                   (cadr icon-bbox) text-zoomf))

           ;;draw the last modified time of the file
           (set! modify-text (make-object text% dynapad (modify-time (build-path->string directory (car non-image-file-list)))))
           (send modify-text position (list 0 0 text-zoomf))
           (center-text modify-text (car icon-bbox) (third icon-bbox) (second (send file-path bbox)) text-zoomf)

           ;;draw the size of the file
           (set! file-size-text (make-object text% dynapad (string-append (insert-commas (expr->string (file-size (build-path->string directory (car non-image-file-list)))))
                                                                          " bytes")))
           (send file-size-text position (list 0 0 text-zoomf))
           (center-text file-size-text (car icon-bbox) (third icon-bbox) (second (send modify-text bbox)) text-zoomf)


           (set! icon-text-groups (append icon-text-groups (list (make-object icon% (car file-icons) file-path (list file-size-text modify-text)))))
           (set! file-paths (append file-paths (list file-path))))

          ;;Beneath each image icon, draw various file attributes
          (do ((image-names image-names (cdr image-names))
               (ilist ilist (cdr ilist)))
            ((null? image-names) null)

            ;;draw the absolute path of the file
            (set! file-path (make-object text% dynapad (send (car ilist) hirespath)))
            (set! icon-bbox (send (car ilist) bbox))
            (set! text-zoomf (/ (bbwidth icon-bbox) (bbwidth (send file-path bbox ))))
            (send file-path position (list (car (send (car ilist) bbox ))
                   (cadr (send (car ilist) bbox ))
                   text-zoomf))

            ;;draw the last modified time of the file
            (set! modify-text (make-object text% dynapad (modify-time (send (car ilist) hirespath))))
            (send modify-text position (list 0 0 text-zoomf))
            (center-text modify-text (car icon-bbox) (third icon-bbox) (second (send file-path bbox)) text-zoomf)

            ;;draw the size of the file
            (set! file-size-text (make-object text% dynapad (string-append (insert-commas (expr->string (file-size (send (car ilist) hirespath))))
                                                                          " bytes")))
            (send file-size-text position (list 0 0 text-zoomf))
            (center-text file-size-text (car icon-bbox) (third icon-bbox) (second (send modify-text bbox)) text-zoomf)

            (set! file-icons (append file-icons (list (car ilist))))
            (set! icon-text-groups (append icon-text-groups (list (make-object icon% (car ilist) file-path (list file-size-text modify-text)))))
            ;;(set! path-icon-list (append path-icon-list (list (cons (send (car ilist) hirespath) (car ilist)))))
            (set! file-paths (append file-paths (list file-path))))

         ;;position the name of the path starting at the bottom left of the directory box below its shadow and
         ;;scale the width of the path so that its length is equal to the width of the directory box
         (set! coord-list (send box bbox))
         (set! shadow-offset (* (- (third coord-list) (first coord-list)) shadow-offset-fraction))
         (set! text-zoomf (/ (bbwidth (send box bbox ))(bbwidth (send path bbox ))))
         (send path position
          (list
               (car (send box bbox))
               (- (cadr (send box bbox)) shadow-offset)
               (/ (bbwidth (send box bbox ))(bbwidth (send path bbox )))))
         (set! file-paths (append file-paths (list path)))

         ;;draw the last modified time of the directory
         (set! modify-text (make-object text% dynapad (modify-time directory)))
         (send modify-text position (list 0 0 text-zoomf))
         (center-text modify-text (car coord-list) (third coord-list) (second (send path bbox)) text-zoomf)

         ;;draw the size of the directory
         (set! file-size-text (make-object text% dynapad (string-append (insert-commas (expr->string (directory-size directory))) " bytes")))
         (send file-size-text position (list 0 0 text-zoomf))
         (center-text file-size-text (car coord-list) (third coord-list) (second (send modify-text bbox)) text-zoomf)

         ;;fix: uncommenting these lines of code causes all file icons to disappear!
         ;;(set! icon-text-groups (append icon-text-groups (list (make-object icon% box path (list modify-text)))))

         ;;(set! dir-icon (make-object icon% box path (list file-size-text modify-text)))
         (set! file-paths (append file-paths (list path file-size-text modify-text)))

         ;;add the directory object for this directory to the directory object list
         (set! dir-obj-list (append dir-obj-list (list (make-object dir-object% directory directories subdir-boxes box file-paths
                                                         icon-text-groups dir-icon dir-color))))

          ;;recursively draw all the subdirectories
          (for-each (lambda (dir box )
                      (send box center 1 #f .5 .5 1.0)   ;;center the view on a subdirectory
                      (draw-subdirectories-helper (build-path->string directory dir)  (alternate-color dir-color) (alternate-color subdir-color) ))
                    directories subdir-boxes)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Arguments:     file-path: an absolute path for a file, dir-obj: a directory object       ;;
;;Postcondition: Returns the file box associated with the given file-path from the dir-obj ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (find-file-icon file-path dir-obj)
  (define (find-file-helper file-path icon-list)
    (if (null? icon-list)
        null
        (if (equal? (send (send (car icon-list) get-file-path) text) file-path)
            (car icon-list)
            (find-file-helper file-path (cdr icon-list)))))
  (find-file-helper file-path (send dir-obj get-icon-text-groups)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arguments:     dir-name: the absolute path of a directory, dir-obj-list: a list of dir-objects ;;
;; Postcondition: Returns the directory object with name dir-name from dir-obj-list               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (find-dir-obj dir-name dir-obj-list)
   (if (null? dir-obj-list)
       null
       ;;Return the directory object if it's at the head of the directory object list.
       ;;Otherwise go to the next element
       (if (equal? dir-name (send (car dir-obj-list) get-dir-name))
           (car dir-obj-list)
           (find-dir-obj dir-name (cdr dir-obj-list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arguments:   The absolute path of the file concatenated with the name of the file, ;;
;; Side Effect: Deletes an image from the given directory from the dynapad surface    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (delete-file-icon file-path)
  (let* ((icon-to-delete null)
         (directory (path-only file-path))
         (dir-obj null)
         (file-paths null))

         ;;remove the last slash from the directory path
         (set! directory (substring directory 0 (- (string-length directory) 1)))

         (set! dir-obj (find-dir-obj directory dir-obj-list))

         ;;find the icon to delete
         (set! icon-to-delete (find-file-icon file-path dir-obj))

         (if (null? icon-to-delete)
             (display "Error: unable to delete file icon \n")
             (begin
             (send icon-to-delete delete)))

         ;;remove the file icon from the set of file icons in the directory object
         (send dir-obj set-icon-text-groups (remove icon-to-delete (send dir-obj get-icon-text-groups) equal?))

         #|
         (set! file-paths (send dir-obj get-text-objs))

         ;;search the list of file paths in the directory object and find the right file path
         ;;text object to delete
         (do ((file-paths file-paths (cdr file-paths)))
             ((null? file-paths) null)
             (if (equal? file-path (send (car file-paths) text))
                 (send (car file-paths) delete)))
         |#
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Argument:    The absolute path to a directory             ;;
;; Side Effect: Deletes a directory from the dynapad surface ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (delete-directory-icon directory)
  (let ((dir-obj (find-dir-obj directory dir-obj-list))
        (subdirectories null)
        (file-paths null)
        (file-icons null)
        (dir-color null)
        (shadow-offset 1)
        (erase-color null)
        (subdir-boxes null)
        (coord-list null))

    (if (not (null? dir-obj))
        (begin

          ;;delete all the file icons in the directory
          (set! file-icons (send dir-obj get-icon-text-groups))
          (do ((file-icons file-icons (cdr file-icons)))
            ((null? file-icons) null)
            (send (car file-icons) delete))

          ;;delete all the subdirectory boxes
          ;;fix: this doesn't work
          (set! subdir-boxes (send dir-obj get-subdir-boxes))
          (do ((subdir-boxes subdir-boxes (cdr subdir-boxes)))
            ((null? subdir-boxes) null)
            (display "subdir coords: ")
            (display (send (car subdir-boxes) coords))
            (newline)
            (send (car subdir-boxes) delete))

          ;;delete the directory box by drawing a box with the color of its superdirectory box on top of the directory box
          (set! dir-color (send dir-obj get-color))
          (if (equal? directory top-level-directory)
              (set! erase-color "black")
              (set! erase-color (alternate-color dir-color)))

          (let ((box (make-object rect% dynapad))      ;;box is drawn to erase the subdirectory box
                (box2 (make-object rect% dynapad)))    ;;box2 is drawn to erase the shadow
            (send box coords (send (send dir-obj get-bbox) coords))
            (send box pen erase-color)
            (send box fill erase-color)
            (set! coord-list (send (send dir-obj get-bbox) coords))
            (send box2 pen erase-color)
            (send box2 fill erase-color)
            (set! shadow-offset (send dir-obj get-shadow-offset))
            (send box2 coords (list (+ (first coord-list) shadow-offset) (- (second coord-list) shadow-offset)
                                     (+ (third coord-list) shadow-offset) (- (fourth coord-list) shadow-offset))))

          (send (send dir-obj get-bbox) delete)  ;;fix: this doesn't work
          ;;(send (send dir-obj get-dir-icon) delete)  ;;fix: uncomment when dir-icon is initialized

          ;;delete all the file path text objects
          (set! file-paths (send dir-obj get-text-objs))
          (do ((file-paths file-paths (cdr file-paths)))
            ((null? file-paths) null)
            (send (car file-paths) delete))

          ;;remove the directory object corresponding to the directory from the dir-obj-list
          (set! dir-obj-list (remove dir-obj dir-obj-list equal?))

          ;;recursively delete all subdirectories
          (set! subdirectories (send dir-obj get-subdirs))
          (do ((subdirectories subdirectories (cdr subdirectories)))
            ((null? subdirectories) null)
            (delete-directory-icon (build-path->string directory (car subdirectories))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arguments:   directory: the name of the directory to update               ;;
;; Side Effect: Creates a representation for a directory in the file browser ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (update-directory-icon directory)
  (let* ((dir-obj (find-dir-obj directory dir-obj-list))  ;;get the directory object
         (bbox null))
    (if (not (null? dir-obj))
        (begin
        ;;delete the directory and redraw it to include any new changes in the directory
        (set! bbox (make-object rect% dynapad (send (send dir-obj get-bbox) coords)))
        (delete-directory-icon directory)
        (send bbox center 1 #f .5 .5 1.0)    ;;center the view on the directory box
        (draw-subdirectories directory)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arguments:     Two bounding boxes                                                  ;;
;; Postcondition: Returns true if the area of bbox1 is smaller than the area of bbox2 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (bbox-smaller-than? bbox1 bbox2)
  (let* ((width1 (- (third bbox1) (first bbox1)))
         (height1 (- (fourth bbox1) (second bbox1)))
         (width2 (- (third bbox2) (first bbox2)))
         (height2 (- (fourth bbox2) (second bbox2)))
         (area1 (* width1 height1))
         (area2 (* width2 height2)))
    (< area1 area2)))

;;fix: comment
(define (update-file-browser directory)
  (let ((objects (send dynapad objects)))
    (do ((objects objects (cdr objects)))
      ((null? objects) null)
      (send (car objects) delete))
    (draw-subdirectories directory)))

(define (display-times path-icon-list)
  (do ((path-icon-list path-icon-list (cdr path-icon-list)))
    ((null? path-icon-list) null)
    (display (file-or-directory-modify-seconds (car (car path-icon-list))))
    (newline))
  (newline)
  (newline))

;;When the mouse button 1 is double clicked on a file or directory icon, the file or directory icon is centered in the view
(send dynapad bind "<Run-Shift-ButtonPress-1>"
 (lambda(dynapad e)
    (let
      ((x (event-x e))
       (y (event-y e))
       (dir-box null)
       (boxes-clicked null)
       (smallest-box-clicked null)
       (file-icons null)
       (icon-box null))

      (send dynapad set!-prev_x x)
      (send dynapad set!-prev_y y)

      ;;Find all directory boxes that were clicked and add them to the boxes-clicked list
      (do ((dir-obj-list dir-obj-list (cdr dir-obj-list)))
        ((null? dir-obj-list) null)
        (set! dir-box (send (car dir-obj-list) get-bbox))
        (set! file-icons (send (car dir-obj-list) get-file-icons))

        ;;Test if any of the file icons within the directory box were clicked
        (do ((file-icons file-icons (cdr file-icons)))
          ((null? file-icons) null)
          (set! icon-box (send (car file-icons) bbox))
          (if (bbenclosed x y icon-box)
            (set! boxes-clicked (append boxes-clicked (list (car file-icons))))))

        ;;Check if the position of the mouse click is enclosed by the bounding box of the directory box
        (if (bbenclosed x y (send dir-box bbox))
            (set! boxes-clicked (append boxes-clicked (list dir-box)))))

      ;;Find the smallest directory box that was clicked
      (do ((boxes-clicked boxes-clicked (cdr boxes-clicked)))
        ((null? boxes-clicked) null)
        (if (or (null? smallest-box-clicked) (bbox-smaller-than? (send (car boxes-clicked) bbox) (send smallest-box-clicked bbox)))
            (set! smallest-box-clicked (car boxes-clicked))))

      ;;Center the view on the smallest box that was clicked
      (if (not (null? smallest-box-clicked))
          (begin
            ;;(send transparent-box coords (list 0 0 0 0))
            ;;(send transparent-box delete)
          (send dynapad center smallest-box-clicked 2000 #f)))
      )))

;;Converts a floating point number to an integer
(define (float->int x)
  (inexact->exact (round x)))

#|
;;These definitions define a pop-up menu for sorting file icons
(define rect-width 100)
(define rect-height 25)
(define neg-rect-height (* rect-height -1))
(define name-rect (make-object rect% dynapad (list 0 0 rect-width neg-rect-height)))
(define size-rect (make-object rect% dynapad (list 0 neg-rect-height rect-width (* neg-rect-height 2))))
(define time-rect (make-object rect% dynapad (list 0 (* neg-rect-height 2) rect-width (* neg-rect-height 3))))
(define name-bbox (send name-rect bbox))
(define size-bbox (send size-rect bbox))
(define time-bbox (send time-rect bbox))
(define name-text (make-object text% dynapad "sort by name"))
(define size-text (make-object text% dynapad "sort by size"))
(define time-text (make-object text% dynapad "sort by time"))
(define text-side-margin 5)
(define rect-width (- (bbwidth name-bbox) (* text-side-margin 2)))
(define name-zoomf (/ rect-width (bbwidth (send name-text bbox))))
(define size-zoomf (/ rect-width (bbwidth (send size-text bbox))))
(define time-zoomf (/ rect-width (bbwidth (send time-text bbox))))
(send name-text position (list (+ (first name-bbox) text-side-margin) (fourth name-bbox) name-zoomf))
(send size-text position (list (+ (first size-bbox) text-side-margin) (fourth size-bbox) size-zoomf))
(send time-text position (list (+ (first time-bbox) text-side-margin) (fourth time-bbox) time-zoomf))
(define sort-menu null)
;;(make-object group% dynapad (list name-rect size-rect time-rect
                                    ;;name-sort-text size-text time-text)))

(send dynapad bind "<Run-Shift-ButtonPress-2>"
  (lambda(dynapad e)
    (let*
      ((x (event-x e))
       (y (event-y e)))
       (display x)
       (display " ")
       (display y)
       (newline)
       (send name-rect slide x y)
       (send size-rect slide x y)
       (send time-rect slide x y)
       (send name-text slide x y)
       (send size-text slide x y)
       (send time-text slide x y)
       (set! sort-menu (make-object group% dynapad (list name-rect size-rect time-rect
                                                    name-text size-text time-text)))
       )))

|#

(define frame (make-object frame% "" #f #f #f 200 200))

(define sort-pane (make-object vertical-pane% frame))
(make-object button% "size sort" sort-pane
  (lambda(button event)
    (display button)
    (display event)
    (newline)))
(make-object button% "name sort" sort-pane
  (lambda(button event)
    (display button)
    (display event)
    (newline)))
(make-object button% "time sort" sort-pane
  (lambda(button event)
    (display button)
    (display event)
    (newline)))

;;(send frame show #t)

(send dynapad bind "<Run-Shift-ButtonPress-2>"
  (lambda(dynapad e)
    (let*
      ((x (float->int (event-x e)))
       (y (float->int (event-y e))))
      (set! frame (make-object frame% "" #f #f #f x y))
      (send frame show #t))))

(send dynapad bind "<Run-Shift-ButtonPress-3>"
  (lambda(dynapad e)
    (let*
      ((x (event-x e))
       (y (event-y e))
       (dir-box null)
       (smallest-dir-clicked null)
       (smallest-box-clicked null)
       (dirs-clicked null)
       (boxes-clicked null))

      (display "shift click")
      (newline)
      (send dynapad set!-prev_x x)
      (send dynapad set!-prev_y  y)
      ;;Find all directory boxes that were clicked and add them to the boxes-clicked list
      (do ((dir-obj-list dir-obj-list (cdr dir-obj-list)))
        ((null? dir-obj-list) null)
        (set! dir-box (send (car dir-obj-list) get-bbox))

        ;;Check if the position of the mouse click is enclosed by the bounding box of the directory box
        (if (bbenclosed x y (send dir-box bbox))
            (begin
            (set! boxes-clicked (append boxes-clicked (list dir-box)))
            (set! dirs-clicked (append dirs-clicked (list (car dir-obj-list))))))

      ;;Find the smallest directory box that was clicked
      (do ((boxes-clicked boxes-clicked (cdr boxes-clicked))
           (dirs-clicked dirs-clicked (cdr dirs-clicked)))
        ((null? boxes-clicked) null)
        (if (or (null? smallest-box-clicked) (bbox-smaller-than? (send (car boxes-clicked) bbox) (send smallest-box-clicked bbox)))
            (begin
            (set! smallest-box-clicked (car boxes-clicked))
            (set! smallest-dir-clicked (car dirs-clicked)))))

      ;;sort the files in the directory by the time they were last modified
      (if (not (null? smallest-dir-clicked))
            (send smallest-dir-clicked arrange-icons-by-time))
      ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arguments:     x and y coordinates of mouse click                                       ;;
;; Postcondition: Returns the icon object that was clicked. If no icon object was clicked, ;;
;;                null is returned                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (icon-clicked x y)
  (let ((icon-clicked null)
        (file-icons null)
        (icon-box null))

      ;;Iterate through all the directory boxes and all the file icons within each directory box
      (do ((dir-obj-list dir-obj-list (cdr dir-obj-list)))
        ;;((and (null? dir-obj-list) (not (null? icon-clicked))) null)
        ((null? dir-obj-list) null)
        (set! file-icons (send (car dir-obj-list) get-icon-text-groups))

        ;;Test if any of the file icons within the directory box were clicked
        (do ((file-icons file-icons (cdr file-icons)))
          ;;((and (null? file-icons) (not (null? icon-clicked))) null)
          ((null? file-icons) null)
          (set! icon-box (send (car file-icons) bbox))
          (if (bbenclosed x y icon-box)
              (set! icon-clicked (car file-icons)))))

    icon-clicked))

;;When the first mouse button is double clicked on a file, the appropriate application is loaded with the file that
;;was double clicked
(send dynapad bind "<Run-Double-ButtonPress-1>"
  (lambda(dynapad e)
    (let*
      ((x (event-x e))
       (y (event-y e))
       (icon (icon-clicked x y)))

      ;;If an icon was double clicked, open the file associated with the icon
      (if (not (null? icon))
          (open-app-with-file (send (send icon get-file-path) text))))))

;;(draw-subdirectories "/home/alvin/backups")
;;(draw-subdirectories "/home/alvin/dynapad/pad")
;;(draw-subdirectories "/home/alvin/dynapad/pad/images")   ;;fix: uncomment
;;(draw-subdirectories "/home/alvin/dynapad/pad/html")
;;(delete-file-icon "/home/alvin/dynapad/pad/images/UNMSEALc.gif")
;;(delete-directory-icon "/home/alvin/dynapad/pad/images/CVS")
;;(delete-directory-icon "/home/alvin/dynapad/pad/images")
;;(create-directory-icon "alvin4" "/home/alvin/dynapad/pad/images")
;;(create-file "UNMSEALc.gif" "/home/alvin/dynapad/pad/images/alvin2")
;;(update-directory-icon "/home/alvin/dynapad/pad/images/CVS")

;;(delete-file-icon "/home/alvin/dynapad/pad/images/UNMSEALc.gif")
;;(delete-directory-icon "/home/alvin/dynapad/pad/images/CVS")
;;(create-directory-icon "test-dir" "/home/alvin/dynapad/pad/images")
;;(update-file-browser "/home/alvin/dynapad/pad/images")

#|
DISCARDED CODE:
 (display "text-width: ")
    (display text-width)
    (newline)
    (display "boundary-width: ")
    (display boundary-width)
    (newline)
    (display "left-x: ")
    (display left-x)
    (newline)
    (display "start-x: ")
    (display start-x)
    (newline)
    (newline)

TEST NOTES:
delete-file seems to work
delete-directory seems to work
update-directory seems to work
deleted a directory then recreated it worked
directory-size worked for /home/alvin/dynapad/pad/images/CVS with one subdirectory

BUGS:
-can't seem to fully delete bounding box
-delete-directory-icon doesn't delete subdir boxes

TO DO LIST:
-arrange text and icons together instead of only arranging icons
as it is, there is no way to arrange the icons alphabetically on the first pass because that
would mix up file icons and their corresponding file names
-group together all the objects in a directory
subdirectories can only be rearranged if all the objects in the subdirectory are treated as a group
-figure out how to scale file attribute text properly
-make a class for the file browser
-combine processing of images and non-image files when drawing icons
-add subdir boxes to file icons for consistency
-figure out when to delete thumbnails
-convert date format of files into 11/02/81
-implement some sort of file search mechanism
-display permissions for a file
-sort files alphabetically by default


NOTES:
-file browser is slow when a lot of files are displayed
-how to know when to delete thumbnails?
-will the thumbnail generator work if the output path hasn't been created yet?
-the directory structure of the thumbnails should match the directory structure of the original files so that
duplicate files in different directories can be handled
-scrapped transparent select box idea
-displaying only the filename of a file requires centering the filename
-draw a box around images to more clearly delineate their boundaries
-without persistent directory objects, any change to the directory structure requires regenerating all thumbnails,
reading files and repositioning all objects again

Views for the filesystem:
-time
scale the alpha value for files in accordance with their timestamp
-size
-frequently accessed

./KPreviewer max-width input-file output-file
~rshapiro/devel/kde-hack/previewer

./KPreviewer 300 /home/alvin/dynapad/dynapad.ss /home/alvin/dynapad.png

(define rect (make-object rect% dynapad (list 0 0 500 500)))
(define text (make-object text% dynapad "hello world"))
(define group (make-object group% dynapad (list rect text)))

add subdir-boxes to file-icons
this changes delete directory procedure

cvs update
make

DISCARDED CODE:
          (display "file-icons: ")
          (display file-icons)
          (newline)
          (display "null: ")
          (display (null? file-icons))
          (newline)
          (display "condition: ")
          (display (and (null? file-icons) (not (null? icon-clicked))))
          (newline)

SORT BUG:

shift click
subdir boxes: (#<struct:object:rect%>)
sorted file icons: (#<struct:object:rect%> #<struct:object:rect%> #<struct:object:rect%> #<struct:object:rect%> #<struct:object:rect%> #<struct:object:rect%> #<struct:object:rect%> #<struct:object:rect%> #<struct:object:rect%> #<struct:object:rect%>)
bbox: #<struct:object:rect%>
bbox coords: (-200.0 -200.0 200.0 200.0)
icon coords: (-76.016845703125 50.67790603637695 -8.446310043334961 118.24844360351562)
icon coords: (92.90948486328125 50.67790603637695 160.4800262451172 118.24844360351562)
icon coords: (8.446317672729492 50.67790603637695 76.01685333251953 118.24844360351562)
icon coords: (92.90948486328125 -33.7852668762207 160.4800262451172 33.78527069091797)
icon coords: (-76.016845703125 -33.7852668762207 -8.446310043334961 33.78527069091797)
icon coords: (-76.016845703125 -118.2484359741211 -8.446310043334961 -50.67789840698242)
icon coords: (-160.48001098632812 -33.7852668762207 -92.90946960449219 33.78527069091797)
icon coords: (-160.48001098632812 -118.2484359741211 -92.90946960449219 -50.67789840698242)
icon coords: (8.446317672729492 -33.7852668762207 76.01685333251953 33.78527069091797)
icon coords: (8.446317672729492 -118.2484359741211 76.01685333251953 -50.67789840698242)

sch_raise: failed, given #<c-pointer:rect> #<c-pointer:rect>

|#

































