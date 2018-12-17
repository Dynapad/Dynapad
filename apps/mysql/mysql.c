#include "escheme.h"
#include <mysql/mysql.h>
#include <stdio.h>

static Scheme_Object *
sch_mysql_init(int argc, Scheme_Object **argv)
{
    Scheme_Object *result = scheme_false;
    MYSQL *conn;

    if (conn = mysql_init(NULL))
      result = scheme_make_cptr(conn, "MYSQL");
    return result;
}

static Scheme_Object *
sch_mysql_close(int argc, Scheme_Object **argv)
{
    Scheme_Object *result = scheme_true;
    MYSQL *conn;

    if (!SCHEME_CPTRP(argv[0]) || strcmp("MYSQL", SCHEME_CPTR_TYPE(argv[0])))
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

    if (!SCHEME_CPTRP(argv[0]) || strcmp("MYSQL", SCHEME_CPTR_TYPE(argv[0])))
      scheme_wrong_type("sch_mysql_connect", "mysql%", 0, argc, argv);
    if (!SCHEME_STRINGP(argv[1]))
      scheme_wrong_type("sch_mysql_connect", "host", 1, argc, argv);
    if (!SCHEME_STRINGP(argv[2]))
      scheme_wrong_type("sch_mysql_connect", "user", 2, argc, argv);
    if (!SCHEME_STRINGP(argv[3]))
      scheme_wrong_type("sch_mysql_connect", "password", 3, argc, argv);
    conn = (MYSQL *)SCHEME_CPTR_VAL(argv[0]);
    host = SCHEME_STR_VAL(argv[1]);
    user = SCHEME_STR_VAL(argv[2]);
    password = SCHEME_STR_VAL(argv[3]);
    if (mysql_connect(conn, host, user, password))
        result = scheme_true;
    return result;
}

static Scheme_Object *
sch_mysql_select_db(int argc, Scheme_Object **argv)
{
    Scheme_Object *result = scheme_false;
    MYSQL *conn;
    char *database;

    if (!SCHEME_CPTRP(argv[0]) || strcmp("MYSQL", SCHEME_CPTR_TYPE(argv[0])))
      scheme_wrong_type("sch_mysql_select_db", "mysql%", 0, argc, argv);
    if (!SCHEME_STRINGP(argv[1]))
      scheme_wrong_type("sch_mysql_select_db", "database", 1, argc, argv);
    conn = (MYSQL *)SCHEME_CPTR_VAL(argv[0]);
    database = SCHEME_STR_VAL(argv[1]);
    if (!mysql_select_db(conn, database))
      result = scheme_true;
    return result;
}

static Scheme_Object *
sch_mysql_query(int argc, Scheme_Object **argv)
{
    MYSQL *conn;
    char *query;

    if (!SCHEME_CPTRP(argv[0]) || strcmp("MYSQL", SCHEME_CPTR_TYPE(argv[0])))
      scheme_wrong_type("sch_mysql_query", "mysql%", 0, argc, argv);
    if (!SCHEME_STRINGP(argv[1]))
      scheme_wrong_type("sch_mysql_query", "database", 1, argc, argv);
    conn = (MYSQL *)SCHEME_CPTR_VAL(argv[0]);
    query = SCHEME_STR_VAL(argv[1]);
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

    if (!SCHEME_CPTRP(argv[0]) || strcmp("MYSQL", SCHEME_CPTR_TYPE(argv[0])))
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
	  scheme_make_pair(scheme_make_string(mysql_row[j]), scheme_null));
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

    if (!SCHEME_STRINGP(argv[0]))
      scheme_wrong_type("sch_mysql_escape_string", "string", 0, argc, argv);
    from = SCHEME_STR_VAL(argv[0]);
    from_len = strlen(from);
    to = (char *)malloc(2*from_len + 1);
    /* check result ??? */
    mysql_escape_string(to, from, from_len);
    result = scheme_make_string(to);
    free(to);
    return result;
}

static Scheme_Object *
sch_mysql_affected_rows(int argc, Scheme_Object **argv)
{
    MYSQL *conn;

    if (!SCHEME_CPTRP(argv[0]) || strcmp("MYSQL", SCHEME_CPTR_TYPE(argv[0])))
      scheme_wrong_type("sch_mysql_affected_rows", "mysql%", 0, argc, argv);
    conn = (MYSQL *)SCHEME_CPTR_VAL(argv[0]);
    return scheme_make_integer(mysql_affected_rows(conn));
}

static Scheme_Object *
sch_mysql_insert_id(int argc, Scheme_Object **argv)
{
    MYSQL *conn;

    if (!SCHEME_CPTRP(argv[0]) || strcmp("MYSQL", SCHEME_CPTR_TYPE(argv[0])))
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

    if (!SCHEME_CPTRP(argv[0]) || strcmp("MYSQL", SCHEME_CPTR_TYPE(argv[0])))
      scheme_wrong_type("sch_mysql_change_user", "mysql%", 0, argc, argv);
    if (!SCHEME_STRINGP(argv[1]))
      scheme_wrong_type("sch_mysql_change_user", "user", 1, argc, argv);
    if (!SCHEME_STRINGP(argv[2]))
      scheme_wrong_type("sch_mysql_change_user", "password", 2, argc, argv);
    if (!SCHEME_STRINGP(argv[3]) && argv[3] != scheme_false)
      scheme_wrong_type("sch_mysql_change_user", "database or #f", 3, argc, argv);
    conn = (MYSQL *)SCHEME_CPTR_VAL(argv[0]);
    user = SCHEME_STR_VAL(argv[1]);
    password = SCHEME_STR_VAL(argv[2]);
    database = argv[3] == scheme_false ? NULL : SCHEME_STR_VAL(argv[3]);
    if (!mysql_change_user(conn, user, password, database))
        result = scheme_true;
    return result;
}

static Scheme_Object *
sch_mysql_errno(int argc, Scheme_Object **argv)
{
    MYSQL *conn;

    if (!SCHEME_CPTRP(argv[0]) || strcmp("MYSQL", SCHEME_CPTR_TYPE(argv[0])))
      scheme_wrong_type("sch_mysql_errno", "mysql%", 0, argc, argv);
    conn = (MYSQL *)SCHEME_CPTR_VAL(argv[0]);
    return scheme_make_integer(mysql_errno(conn));
}

static Scheme_Object *
sch_mysql_error(int argc, Scheme_Object **argv)
{
    MYSQL *conn;

    if (!SCHEME_CPTRP(argv[0]) || strcmp("MYSQL", SCHEME_CPTR_TYPE(argv[0])))
      scheme_wrong_type("sch_mysql_error", "mysql%", 0, argc, argv);
    conn = (MYSQL *)SCHEME_CPTR_VAL(argv[0]);
    return scheme_make_string(mysql_error(conn));
}

Scheme_Object *scheme_reload(Scheme_Env *env)
{
  scheme_add_global("mysql_init",
    scheme_make_prim_w_arity(sch_mysql_init, "mysql_init", 0, 0), env);
  scheme_add_global("mysql_close",
    scheme_make_prim_w_arity(sch_mysql_close, "mysql_close", 1, 1), env);
  scheme_add_global("mysql_connect",
    scheme_make_prim_w_arity(sch_mysql_connect, "mysql_connect", 4, 4), env);
  scheme_add_global("mysql_select_db",
    scheme_make_prim_w_arity(sch_mysql_select_db, "mysql_select_db", 2, 2), env);
  scheme_add_global("mysql_change_user",
    scheme_make_prim_w_arity(sch_mysql_change_user, "mysql_change_user", 4, 4), env);
  scheme_add_global("mysql_query",
    scheme_make_prim_w_arity(sch_mysql_query, "mysql_query", 2, 2), env);
  scheme_add_global("mysql_store_result",
    scheme_make_prim_w_arity(sch_mysql_store_result, "mysql_store_result", 1, 1), env);
  scheme_add_global("mysql_affected_rows",
    scheme_make_prim_w_arity(sch_mysql_affected_rows, "mysql_affected_rows", 1, 1), env);
  scheme_add_global("mysql_insert_id",
    scheme_make_prim_w_arity(sch_mysql_insert_id, "mysql_insert_id", 1, 1), env);
  scheme_add_global("mysql_escape_string",
    scheme_make_prim_w_arity(sch_mysql_escape_string, "mysql_escape_string", 1, 1), env);
  scheme_add_global("mysql_errno",
    scheme_make_prim_w_arity(sch_mysql_errno, "mysql_errno", 1, 1), env);
  scheme_add_global("mysql_error",
    scheme_make_prim_w_arity(sch_mysql_error, "mysql_error", 1, 1), env);
  return scheme_make_string("Hello MySQL!");
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
