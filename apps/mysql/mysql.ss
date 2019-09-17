(require (lib "class.ss"))

(load-relative-extension "mysql.so")

(define mysql%
  (class object%
    (field (_conn #f) (_host #f) (_user #f) (_database #f))
    (define/public (connect host user password)
      (if (mysql_connect _conn host user password)
    (begin
      (set! _host host)
      (set! _user user)
      #t)
    #f))
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
