#include "escheme.h"
#include <mysql/mysql.h>
#include <stdio.h>

#define SCH_UNIXSTR(arg) \
  SCHEME_BYTE_STR_VAL(scheme_char_string_to_byte_string(arg))

#define UNIX_SCHSTR(arg) \
  scheme_byte_string_to_char_string(scheme_make_byte_string(arg))

Scheme_Object *mysql_typetag = NULL;

static Scheme_Object *
sch_mysql_init(int argc, Scheme_Object **argv)
{
    Scheme_Object *result = scheme_false;
    MYSQL *conn;

    if (conn = mysql_init(NULL))
      result = scheme_make_cptr(conn, mysql_typetag);
    return result;
}

static Scheme_Object *
sch_mysql_close(int argc, Scheme_Object **argv)
{
    Scheme_Object *result = scheme_true;
    MYSQL *conn;

    if (!SCHEME_CPTRP(argv[0]) || mysql_typetag != SCHEME_CPTR_TYPE(argv[0]))
      scheme_wrong_type("sch_mysql_close", "mysql%", 0, argc, argv);
    conn = (MYSQL *)SCHEME_CPTR_VAL(argv[0]);
    mysql_close(conn);
    return result;
}

static Scheme_Object *
sch_mysql_connect(int argc, Scheme_Object **argv)
{
    Scheme_Object *result = scheme_false;
    MYSQL *conn;
    char *host, *user, *password;

    if (!SCHEME_CPTRP(argv[0]) || mysql_typetag != SCHEME_CPTR_TYPE(argv[0]))
      scheme_wrong_type("sch_mysql_connect", "mysql%", 0, argc, argv);
    if (!SCHEME_CHAR_STRINGP(argv[1]))
      scheme_wrong_type("sch_mysql_connect", "host", 1, argc, argv);
    if (!SCHEME_CHAR_STRINGP(argv[2]))
      scheme_wrong_type("sch_mysql_connect", "user", 2, argc, argv);
    if (!SCHEME_CHAR_STRINGP(argv[3]))
      scheme_wrong_type("sch_mysql_connect", "password", 3, argc, argv);
    conn = (MYSQL *)SCHEME_CPTR_VAL(argv[0]);
    host = SCH_UNIXSTR(argv[1]);
    user = SCH_UNIXSTR(argv[2]);
    password = SCH_UNIXSTR(argv[3]);
    if (mysql_real_connect(conn, host, user, password, 0, 0, 0, 0))
        result = scheme_true;
    return result;
}

static Scheme_Object *
sch_mysql_select_db(int argc, Scheme_Object **argv)
{
    Scheme_Object *result = scheme_false;
    MYSQL *conn;
    char *database;

    if (!SCHEME_CPTRP(argv[0]) || mysql_typetag != SCHEME_CPTR_TYPE(argv[0]))
      scheme_wrong_type("sch_mysql_select_db", "mysql%", 0, argc, argv);
    if (!SCHEME_CHAR_STRINGP(argv[1]))
      scheme_wrong_type("sch_mysql_select_db", "database", 1, argc, argv);
    conn = (MYSQL *)SCHEME_CPTR_VAL(argv[0]);
    database = SCH_UNIXSTR(argv[1]);
    if (!mysql_select_db(conn, database))
      result = scheme_true;
    return result;
}

static Scheme_Object *
sch_mysql_query(int argc, Scheme_Object **argv)
{
    MYSQL *conn;
    char *query;

    if (!SCHEME_CPTRP(argv[0]) || mysql_typetag != SCHEME_CPTR_TYPE(argv[0]))
      scheme_wrong_type("sch_mysql_query", "mysql%", 0, argc, argv);
    if (!SCHEME_CHAR_STRINGP(argv[1]))
      scheme_wrong_type("sch_mysql_query", "database", 1, argc, argv);
    conn = (MYSQL *)SCHEME_CPTR_VAL(argv[0]);
    query = SCH_UNIXSTR(argv[1]);
    if (mysql_query(conn, query)) {
      return scheme_false;
    } else
      return scheme_true;
}

static Scheme_Object *
sch_mysql_store_result(int argc, Scheme_Object **argv)
{
    Scheme_Object *result = scheme_false;
    MYSQL *conn;
    MYSQL_RES *mysql_res;
    MYSQL_ROW mysql_row;
    int nrows, nfields, i, j;
    Scheme_Object *srow;

    if (!SCHEME_CPTRP(argv[0]) || mysql_typetag != SCHEME_CPTR_TYPE(argv[0]))
      scheme_wrong_type("sch_mysql_store_result", "mysql%", 0, argc, argv);
    conn = (MYSQL *)SCHEME_CPTR_VAL(argv[0]);
    if (!(mysql_res = mysql_store_result(conn))) {
      return scheme_false;
    }
    nrows = mysql_num_rows(mysql_res);
    nfields = mysql_num_fields(mysql_res);
    result = scheme_null;
    for (i = 0; i < nrows; i++) {
      if (!(mysql_row = mysql_fetch_row(mysql_res))) {
        return scheme_false;
      }
      srow = scheme_null;
      for (j = 0; j < nfields; j++) {
	srow = scheme_append(srow,
	  scheme_make_pair(UNIX_SCHSTR(mysql_row[j]), scheme_null));
      }
      result = scheme_append(result, scheme_make_pair(srow, scheme_null));
    }
    mysql_free_result(mysql_res);
    return result;
}

static Scheme_Object *
sch_mysql_escape_string(int argc, Scheme_Object **argv)
{
    Scheme_Object *result = scheme_false;
    char *from, *to;
    int from_len;

    if (!SCHEME_CHAR_STRINGP(argv[0]))
      scheme_wrong_type("sch_mysql_escape_string", "string", 0, argc, argv);
    from = SCH_UNIXSTR(argv[0]);
    from_len = strlen(from);
    to = (char *)malloc(2*from_len + 1);
    /* check result ??? */
    mysql_escape_string(to, from, from_len);
    result = UNIX_SCHSTR(to);
    free(to);
    return result;
}

static Scheme_Object *
sch_mysql_affected_rows(int argc, Scheme_Object **argv)
{
    MYSQL *conn;

    if (!SCHEME_CPTRP(argv[0]) || mysql_typetag != SCHEME_CPTR_TYPE(argv[0]))
      scheme_wrong_type("sch_mysql_affected_rows", "mysql%", 0, argc, argv);
    conn = (MYSQL *)SCHEME_CPTR_VAL(argv[0]);
    return scheme_make_integer(mysql_affected_rows(conn));
}

static Scheme_Object *
sch_mysql_insert_id(int argc, Scheme_Object **argv)
{
    MYSQL *conn;

    if (!SCHEME_CPTRP(argv[0]) || mysql_typetag != SCHEME_CPTR_TYPE(argv[0]))
      scheme_wrong_type("sch_mysql_insert_id", "mysql%", 0, argc, argv);
    conn = (MYSQL *)SCHEME_CPTR_VAL(argv[0]);
    return scheme_make_integer(mysql_insert_id(conn));
}

static Scheme_Object *
sch_mysql_change_user(int argc, Scheme_Object **argv)
{
    Scheme_Object *result = scheme_false;
    MYSQL *conn;
    char *user, *password, *database;

    if (!SCHEME_CPTRP(argv[0]) || mysql_typetag != SCHEME_CPTR_TYPE(argv[0]))
      scheme_wrong_type("sch_mysql_change_user", "mysql%", 0, argc, argv);
    if (!SCHEME_CHAR_STRINGP(argv[1]))
      scheme_wrong_type("sch_mysql_change_user", "user", 1, argc, argv);
    if (!SCHEME_CHAR_STRINGP(argv[2]))
      scheme_wrong_type("sch_mysql_change_user", "password", 2, argc, argv);
    if (!SCHEME_CHAR_STRINGP(argv[3]) && argv[3] != scheme_false)
      scheme_wrong_type("sch_mysql_change_user", "database or #f", 3, argc, argv);
    conn = (MYSQL *)SCHEME_CPTR_VAL(argv[0]);
    user = SCH_UNIXSTR(argv[1]);
    password = SCH_UNIXSTR(argv[2]);
    database = argv[3] == scheme_false ? NULL : SCH_UNIXSTR(argv[3]);
    if (!mysql_change_user(conn, user, password, database))
        result = scheme_true;
    return result;
}

static Scheme_Object *
sch_mysql_errno(int argc, Scheme_Object **argv)
{
    MYSQL *conn;

    if (!SCHEME_CPTRP(argv[0]) || mysql_typetag != SCHEME_CPTR_TYPE(argv[0]))
      scheme_wrong_type("sch_mysql_errno", "mysql%", 0, argc, argv);
    conn = (MYSQL *)SCHEME_CPTR_VAL(argv[0]);
    return scheme_make_integer(mysql_errno(conn));
}

static Scheme_Object *
sch_mysql_error(int argc, Scheme_Object **argv)
{
    MYSQL *conn;

    if (!SCHEME_CPTRP(argv[0]) || mysql_typetag != SCHEME_CPTR_TYPE(argv[0]))
      scheme_wrong_type("sch_mysql_error", "mysql%", 0, argc, argv);
    conn = (MYSQL *)SCHEME_CPTR_VAL(argv[0]);
    return UNIX_SCHSTR(mysql_error(conn));
}

static Scheme_Object *
sch_mysql_real_connect(int argc, Scheme_Object **argv)
{
    Scheme_Object *result = scheme_false;
    MYSQL *conn;
    char *host, *user, *password, *db, *socket;
    int port, flags;

    if (!SCHEME_CPTRP(argv[0]) || mysql_typetag != SCHEME_CPTR_TYPE(argv[0]))
      scheme_wrong_type("sch_mysql_real_connect", "mysql%", 0, argc, argv);
    if (!SCHEME_CHAR_STRINGP(argv[1]))
      scheme_wrong_type("sch_mysql_real_connect", "host", 1, argc, argv);
    if (!SCHEME_CHAR_STRINGP(argv[2]))
      scheme_wrong_type("sch_mysql_real_connect", "user", 2, argc, argv);
    if (!SCHEME_CHAR_STRINGP(argv[3]))
      scheme_wrong_type("sch_mysql_real_connect", "password", 3, argc, argv);
    if (!SCHEME_CHAR_STRINGP(argv[4]))
      scheme_wrong_type("sch_mysql_real_connect", "database", 4, argc, argv);
    if (!SCHEME_INTP(argv[5]))
      scheme_wrong_type("sch_mysql_real_connect", "port", 5, argc, argv);
    if (!SCHEME_CHAR_STRINGP(argv[6]))
      scheme_wrong_type("sch_mysql_real_connect", "socket", 6, argc, argv);
    if (!SCHEME_INTP(argv[7]))
      scheme_wrong_type("sch_mysql_real_connect", "flags", 7, argc, argv);
    conn = (MYSQL *)SCHEME_CPTR_VAL(argv[0]);
    host = SCH_UNIXSTR(argv[1]);
    user = SCH_UNIXSTR(argv[2]);
    password = SCH_UNIXSTR(argv[3]);
    db = SCH_UNIXSTR(argv[4]);
    if (strlen(db) == 0)
      db = NULL;
    port = SCHEME_INT_VAL(argv[5]);
    socket = SCH_UNIXSTR(argv[6]);
    if (strlen(socket) == 0)
      socket = NULL;
    flags = SCHEME_INT_VAL(argv[7]);
    if (mysql_real_connect(conn, host, user, password, db, port, socket, flags))
        result = scheme_true;
    return result;
}

Scheme_Object *scheme_reload(Scheme_Env *env)
{
  Scheme_Object *func[13];

  func[0] =
    scheme_make_prim_w_arity(sch_mysql_init, "mysql_init", 0, 0);
  func[1] =
    scheme_make_prim_w_arity(sch_mysql_close, "mysql_close", 1, 1);
  func[2] =
    scheme_make_prim_w_arity(sch_mysql_connect, "mysql_connect", 4, 4);
  func[3] =
    scheme_make_prim_w_arity(sch_mysql_select_db, "mysql_select_db", 2, 2);
  func[4] =
    scheme_make_prim_w_arity(sch_mysql_change_user, "mysql_change_user", 4, 4);
  func[5] =
    scheme_make_prim_w_arity(sch_mysql_query, "mysql_query", 2, 2);
  func[6] =
    scheme_make_prim_w_arity(sch_mysql_store_result, "mysql_store_result", 1, 1);
  func[7] =
    scheme_make_prim_w_arity(sch_mysql_affected_rows, "mysql_affected_rows", 1, 1);
  func[8] =
    scheme_make_prim_w_arity(sch_mysql_insert_id, "mysql_insert_id", 1, 1);
  func[9] =
    scheme_make_prim_w_arity(sch_mysql_escape_string, "mysql_escape_string", 1, 1);
  func[10] =
    scheme_make_prim_w_arity(sch_mysql_errno, "mysql_errno", 1, 1);
  func[11] =
    scheme_make_prim_w_arity(sch_mysql_error, "mysql_error", 1, 1);
  func[12] =
    scheme_make_prim_w_arity(sch_mysql_real_connect, "mysql_real_connect", 8, 8);

  mysql_typetag = scheme_intern_symbol("MYSQL");
  scheme_dont_gc_ptr(mysql_typetag);

  return scheme_values(13, func);
}

Scheme_Object *scheme_initialize(Scheme_Env *env)
{
  return scheme_reload(env);
}

Scheme_Object *scheme_module_name()
{
  /* This extension doesn't define a module: */
  return scheme_false;
}
