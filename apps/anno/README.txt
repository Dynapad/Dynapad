anno.ss  03/10/03 

This app allows you to add short text annotations to individual objects
on the pad surface.  The annotations appear as icons attached to the 
objects.  When the mouse passes over the icon, the content of the annotation
is displayed.

This is one part of a larger annotation system which handles a greater
variety of types of annotation: long text, audio, video, webpage, etc.
The larger system is not yet ready for release.

--------------------
USER GUIDE

To run it:
 * set your DYNAPADHOME environment variable & put (load "./apps/anno/anno.ss")
  into your DYNAPADHOME/apps.ss file.  Then anno.ss will load whenever
  you run dynapad.

To annotate an object:
 * select the object, and press the left mouse button to bring up the
   dynapad/object popup menu.  select "Annotate".  (note that pressing 
   the left mouse button over the object will both select it AND bring up
   the menu).
 * a box for text entry will appear under the selected object.  type in 
   the text of your annotation.  you do not need to click in the box to 
   move the focus there... in fact you SHOULD NOT (see "BUGS/ISSUES" below).
 * you'll have to do your own word-wrapping to stay inside the box for now,
   until the TextArea object is available in dynapad.
 * when you're done, hit "ok".  (if there's a problem, hit "cancel" and
   there will be no changes made).
 * an icon will appear on the object. 

To display an annotation:
 * just move the mouse over the annotation icon.

To toggle the display of icons on and off:
 * select Toggle Anno Icons from the dynapad/object popup menu.


BUGS/ISSUES:
 NOTE: the first 3 (biggest) bugs may be handled semi-automatically once
 textarea% objects are available in dynapad:
* right now, no line-wrap in text entry box.  waiting for TextArea objects.
* text is not always big enough to read.. it should be, regardless of 
  combinations of zooming/cowboy zooming, etc.
* if the focus is lost before you've finished (or started) entering text
  annotations, all is lost.  hit cancel & re-enter it for now.
* currently, no way to delete annotations from the UI
* deleting annotated objects may cause trouble... an easy fix i'll put in 
  soon.
* save/load features are there in the code, but i don't think there's
  a place to hook them up to yet...
* all the text-entry boxes, etc., should be group'ed with the target object
* the color scheme should be easily modifiable.


 



