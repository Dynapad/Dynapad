(module mysql mzscheme

(require (lib "class.ss"))
(require (lib "etc.ss"))

(provide mysql%)

(define mysql% #f)

(let-values (((
  mysql_init
  mysql_close
  mysql_connect
  mysql_select_db
  mysql_change_user
  mysql_query
  mysql_store_result
  mysql_affected_rows
  mysql_insert_id
  mysql_escape_string
  mysql_errno
  mysql_error
  mysql_real_connect
  ) 
  (load-relative-extension
    (build-path
      (this-expression-source-directory)
      "mysql-prims.so"))))

(set! mysql%
  (class object%
    (field (_conn #f) (_host #f) (_user #f) (_database #f) (_port #f))
    (define/public connect
      (case-lambda
        ((host user password) (connect host user password 0))
	((host user password port)
          (if (mysql_real_connect _conn host user password "" port "" 0)
	    (begin
	      (set! _host host)
	      (set! _user user)
	      (set! _port port)
	      #t)
	    #f))))
    (define/public database
      (case-lambda
        (() _database)
	((newdb)
          (if (mysql_select_db _conn newdb)
	    (begin
	      (set! _database newdb)
	      #t)
	    #f))))
    (define/public (query querystring)
      (mysql_query _conn querystring))
    (define/public (store_result)
      (mysql_store_result _conn))
    (define/public (affected_rows)
      (mysql_affected_rows _conn))
    (define/public (insert_id)
      (mysql_insert_id _conn))
    (define/public (cmd string)
      (let ((result (query string)))
        (if result
          (store_result)
	  #f)))
    (define/public (select string)
      (let ((result (query (format "select ~a" string))))
        (if result
          (store_result)
	  #f)))
    (define/public (insert string)
      (let ((result (query (format "insert ~a" string))))
	(if result
          (insert_id)
	  #f)))
    (define/public (update string)
      (let ((result (query (format "update ~a" string))))
	(if result
          (affected_rows)
	  #f)))
    (define/public (delete string)
      (let ((result (query (format "delete ~a" string))))
	(if result
          (affected_rows)
	  #f)))
    (define/public (close)
      (mysql_close _conn))
    (define/public (conn) _conn)
    (define/public (host) _host)
    (define/public (port) _port)
    (define/public user
      (case-lambda
        (() _user)
	((newuser newpassword)
	  (user newuser newpassword #f))
	((newuser newpassword newdb)
	  (if (mysql_change_user _conn newuser newpassword newdb)
	    (begin
	      (set! _user newuser)
	      (set! _database newdb)
	      #t)
	    #f))))
    (define/public (errno)
      (mysql_errno _conn))
    (define/public (error)
      (mysql_error _conn))

    (super-instantiate())
    (set! _conn (mysql_init))))
))
