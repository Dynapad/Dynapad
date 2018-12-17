#include "api.h"
#include "renderer.h"

static int  TestAPI(ClientData clientData, Tcl_Interp *interp, int argc, char **argv);

int
TestAPIInit(Tcl_Interp *interp)
{
    Tcl_CreateCommand(interp, "testapi", TestAPI, NULL, NULL);
    return(TCL_OK);
}

static int
Press(Pad_Object *obj, ClientData, Pad_Event *)
{
    Pad_ObjHandle handle(obj);

    handle.Slide(10, 0);

    return(TCL_OK);
}

//
// A Render callback that replaces a rectangle with an array of
// a hundred little crosses.
//
static int
RectRender(Pad_Object *, ClientData, Pad_Event *)
{
    int i, j;
    Pad_ColorRef color;

    color.Set(100, 100, 100);
    Pad_renderer->Set_color(color);
    Pad_renderer->Set_line_width(1);
    Pad_renderer->Set_join_style("miter");

    for (i=0; i<100; i+=10) {
	for (j=0; j<100; j+=10) {
	    Pad_renderer->Begin_line();
	    Pad_renderer->V2f(i, j);
	    Pad_renderer->V2f(i+5, j+5);
	    Pad_renderer->End_line();

	    Pad_renderer->Begin_line();
	    Pad_renderer->V2f(i+5, j);
	    Pad_renderer->V2f(i, j+5);
	    Pad_renderer->End_line();
	}
    }

    return(TCL_OK);
}

static int
TestAPI(ClientData, Tcl_Interp *interp, int argc, char **argv)
{
    Pad_ObjHandle handle;
    Pad_ObjHandle *handlePtr;
    Pad_HandleList handles;
    Pad_SurfaceHandle surface;
    Pad_ViewHandle view;
    Pad_String result;
    Pad_Iterator oi;
    Pad_Callback *eventCallback, *renderCallback;
    Pad_PList pts;
    Pad_Point pt;
    char *surfaceName;

    if (argc == 2) {
	surfaceName = argv[1];

	if (!surface.Attach(surfaceName)) {
				// Pad widget doesn't exist, so try and create one
	    if (!Pad_UniverseMgr::Get_instance()->Create_win(interp, surfaceName)) {
				// Can't create a pad
		Tcl_ResetResult(interp);
		Tcl_AppendResult(interp, "padWidget does not specify a valid pad and one can not be created: ", 
				 surfaceName, (char *)NULL);
		return(TCL_ERROR);
	    }
				// Pad was created, so pack it, and get handle to surface
	    Pad_String cmd;
	    cmd.Printf("pack %s", surfaceName);
	    Tcl_Eval(interp, cmd.Get());
	    surface.Attach(surfaceName);
	}

				// Create some things
	surface.Create_obj(Pad_RectangleType);

	surface.Create_obj(Pad_LineType, handle);

				// Create three pieces of text with different angles
	surface.Create_obj(Pad_TextType, handle);
	handle.Add_tag("stuff");
	handle.Set_option(Pad_TextOption, "Hello World!");

	surface.Create_obj(Pad_TextType, handle);
	handle.Add_tag("stuff");
	handle.Set_options(Pad_TextOption, "Second World!", 
			   Pad_AngleOption, 30.0,
			   Pad_PenOption, 255, 0, 0,
			   Pad_LayerOption, "foo",
			   Pad_NoOption);

	eventCallback = new Pad_Callback(Press);
	surface.Create_obj(Pad_TextType, handle,
			Pad_TextOption, "Third World!", 
			Pad_AngleOption, 60.0,
			Pad_PenOption, 128, 128, 0,
			Pad_NoOption);
	handle.Add_tag("stuff");
	handle.Bind("<ButtonPress-1>", eventCallback);

				// Create a portal and set its visiblelayers
	surface.Create_obj(Pad_PortalType, handle,
			   Pad_LayerOption, "foo",
			   Pad_NoOption);
	pts.Make_empty();
	pt.Set(-100,  0);  pts.Push_last(&pt);
	pt.Set(-50, 0);  pts.Push_last(&pt);
	pt.Set(-50, 100);  pts.Push_last(&pt);
	pt.Set(-100, 100);  pts.Push_last(&pt);
	handle.Set_coords(pts);
	view.Attach(handle);
	view.Set_visiblelayers("all -foo");

				// Create a rectangle, and set up a render callback
				// to render it manually by calling the renderer
				// Set the fill color so that the center of the
				// rectangle gets events.
	surface.Create_obj(Pad_RectangleType, handle,
			   Pad_FillOption, 0, 0, 0,
			   Pad_NoOption);
	pts.Make_empty();
	pt.Set(0, 0);  pts.Push_last(&pt);
	pt.Set(100, 100);  pts.Push_last(&pt);
	handle.Set_coords(pts);
	renderCallback = new Pad_Callback(RectRender);
	handle.Set_render_callback(renderCallback);

				// Create a triangle
	surface.Create_obj(Pad_PolygonType, handle,
			   Pad_FillOption, 80, 80, 80,
			   Pad_PenOption, 200, 200, 200,
			   Pad_PenWidthOption, 1.0,
			   Pad_NoOption);
	pts.Make_empty();
	pt.Set(0,  0);  pts.Push_last(&pt);
	pt.Set(20, 0);  pts.Push_last(&pt);
	pt.Set(20, 5);  pts.Push_last(&pt);
	handle.Set_coords(pts);
	handle.Set_option(surface.Get_option_type("-place"), 100.0, 100.0, 2.0);
	handle.Add_tag("stuff");
	handle.Add_tag("polygon");

				// Define event binding to click on polygons and move them
        handle.Attach(surface, "polygon");
	handle.Bind("<ButtonPress-1>", eventCallback);

				// Move "stuff" around a bit
	handle.Attach(surface, "stuff");
	handle.Slide(-100, 0);
	handle.Scale(0, 0, 2);

				// Delete all objects with the tag "foo"
	handle.Attach(surface, "foo");
	handle.Delete_obj();

				// Modify the view
	view.Attach(surfaceName);
	view.Set_view(0, 0, 2, 1000);

				// Return a list of all objects with the tag "bar"
	surface.Find_with_tagorid("bar", handles);
	DOLIST(oi, handles, Pad_ObjHandle, handlePtr) {
	    result += handlePtr->Get_id();
	    result += " ";
	}

	Tcl_AppendResult(interp, result.Get(), NULL);
    } else {
	Tcl_ResetResult(interp);
	Tcl_AppendResult(interp, "Usage: testapi padWidget", (char *)NULL);
	return(TCL_ERROR);
    }

    return(TCL_OK);
}
