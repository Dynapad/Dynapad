(dynaload "metadata.ss")

(image-aftermake-callbacks 'add
                           (lambda (img)
                             (send (assign-handle (send img hirespath) car image-handle%)
                                   add img) ;makes, attaches handle
                             ;(register-object-with-brush-set img 'images)
                             (add-object-menu make-metadata-menu-for-image img)))

(define (make-metadata-menu-for-image img)
  (let ((menu (new-popup "Image Details")))
    (date-display-format (pad-date-format))
    (make-submenu-Select-Highlighted menu img)
    (make-submenu-DateFormat menu img)
    (add-menu-item menu (format "File: ~a" (send img hirespath)) void #f)
    (add-menu-item menu (format "Created: ~a"
                                (let* ((pair (get-image-date img))
                                       (date (pair->date pair)))
                                  (if date
                                      (date->string date (show-metadata-time?))
                                      "(unknown)")))
                   void #f)
    (add-menu-item menu (format "Acquired: ~a"
                                (let* ((pair (get-image-filedate img))
                                       (date (pair->date pair)))
                                  (if date
                                      (date->string date (show-metadata-time?))
                                      "(unknown)")))
                   void #f)
    (add-menu-item menu (format "Focus Distance: ~a"
                                (or (get-image-focus-dist img)
                                    "(unknown)"))
                   void #f)
    (add-menu-item menu (format "Camera Aperture: ~a"
                                (or (get-image-fstop img)
                                    "(unknown)"))
                   void #f)
    (include-custom-popup-items menu img)

    menu))
