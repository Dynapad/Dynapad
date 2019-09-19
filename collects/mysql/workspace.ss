(require "mysql.ss")

(define workspace%
  (class mysql%
    (inherit connect database select)
    (define/public names
      (case-lambda
        (() (map car (select (format "name from name"))))
        ((name) (select
                 (format "name,nameid from name where name = ~s"
                         (mysql_escape_string name))))))
    (super-instantiate())
    (connect "hci" "workspace" "kArumbA")
    (database "workspace")))

; name
;| name      | varchar(255)  |
;| owner     | varchar(64)   |
;| timestamp | timestamp(14) |
;| nameid    | bigint(20)    |

; object
;| object       | bigint(20) |
;| nameid       | bigint(20) |
;| buildstring  | text       |
;| displayorder | bigint(20) |
;| collection   | bigint(20) |
;| collectionp  | tinyint(4) |
;| x            | float      |
;| y            | float      |
;| zoom         | float      |
;| image        | text       |
