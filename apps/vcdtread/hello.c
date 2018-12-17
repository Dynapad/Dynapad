/* 
   MzScheme extension example that returns the string "Hello, world!"
   when loaded.

   Compile with:
     mzc --cc hello.c
     mzc --ld hello.so hello.o
   And load with
     (load-extension "hello.so")
*/

#include "escheme.h"

extern Scheme_Object *sch_dtopen(int argc, Scheme_Object **argv);
extern Scheme_Object *sch_dtread(int argc, Scheme_Object **argv);
extern Scheme_Object *sch_dtclose(int argc, Scheme_Object **argv);

Scheme_Object *arf(int argc, Scheme_Object **argv)
{
	Scheme_Object *result;

	if (!SCHEME_STRINGP(argv[0]))
		scheme_wrong_type("arf", "name", 0, argc, argv);
	result = scheme_append_string(scheme_make_string("hi "), argv[0]);

	return result;
}

Scheme_Object *scheme_reload(Scheme_Env *env)
{
	scheme_add_global("sch_dtopen",
		scheme_make_prim_w_arity(sch_dtopen, "sch_dtopen", 0, 0), env);
	scheme_add_global("sch_dtread",
		scheme_make_prim_w_arity(sch_dtread, "sch_dtread", 1, 1), env);
	scheme_add_global("sch_dtclose",
		scheme_make_prim_w_arity(sch_dtclose, "sch_dtclose", 0, 0), env);
	scheme_add_global("arf",
		scheme_make_prim_w_arity(arf, "arf", 1, 1), env);
  /* When the extension is loaded, return a Scheme string: */
  return scheme_make_string("Hello, world!");
}

Scheme_Object *scheme_initialize(Scheme_Env *env)
{
  /* First load is same as every load: */
  return scheme_reload(env);
}

Scheme_Object *scheme_module_name()
{
  /* This extension doesn't define a module: */
  return scheme_false;
}
