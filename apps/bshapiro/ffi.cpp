#include <escheme.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/select.h>
#include "KPreviewer.h"

Scheme_Object * sch_createPreview(int argc, Scheme_Object ** argv)
{
  KApplication a(false,false);
  int width=SCHEME_INT_VAL(argv[0]);
  char * fileType=SCHEME_STR_VAL(argv[3]);
  char * url=SCHEME_STR_VAL(argv[1]);
  char * dest=SCHEME_STR_VAL(argv[2]);

  KPreviewer *w = new KPreviewer(width, url, dest, fileType);
  a.setMainWidget(w);
  w->show(); //try to avoid this
  a.exec();  // you might need to modify this code to just close down and not exit the app
  return scheme_make_string("i am not doing error checking");

}

Scheme_Object *scheme_initialize(Scheme_Env *env)
{
  scheme_add_global("CreatePreview",
		    scheme_make_prim_w_arity(sch_createPreview,
					     "CreatePreview",
					     4, 4),
		    env);


  return scheme_true;
}

Scheme_Object *scheme_reload(Scheme_Env *env)
{
   return scheme_initialize(env);
}

Scheme_Object *scheme_module_name()
{
   return scheme_false;
}



