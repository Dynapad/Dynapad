#ifndef PAD_STRING_H
#define PAD_STRING_H 1
#include "defs.h"
#include <iostream>
using namespace std;

class Pad_String {
    friend ostream &operator<<(ostream &, Pad_String &);
 private:
    char *_data;		    // Actual string
    int _space;			    // Allocated data length
    int _length;		    // Length of current string
    void  Expand(int expansion);    // Increase space by expansion bytes
    void  Allocate(int len);	    // Allocate len bytes
    void  Allocate(const char *d);  // Allocate enough space for d and initialize with d
    void  Allocate(const Pad_String *ps); // Allocate enough space for ps and initialize with ps
    void  Allocate(const Pad_String &ps); // Allocate enough space for ps and initialize with ps

 public:
    void  Append(float f);	    // Append float to end
    void  Append(double d);	    // Append double to end
    void  Append(int c);	    // Append int to end
    void  Append(char c);	    // Append char to end
    void  Append(const char *d); // Append string to end
    void  Append(const Pad_String &ps);   // Append Pad_String to end
    void  Append(const Pad_String *ps);
    void  Append_with_escapes(char *d);  // Append string, escaping special characters
    void  Erase(int index);	         // Delete character at index
    void  Erase(int index, int len);     // Delete len character at index
    char *Get(void) const;	         // Return string
    char *Get(int index) const;	         // Return string starting at index
    char  Get_char(int index) const;     // Return character at index
    void  Insert(char c, int index);     // Insert char at index
    void  Insert(const char *d, int index); // Insert string at index
    void  Insert(const Pad_String *ps, int index);  // Insert Pad_String at index
    int   Length(void) const;		     // Return string length (not available space)
    char *Set(const char *d);		     // Set to specified string (grow if necessary)
    char *Set(const char *d, int len);       // Copy len bytes starting at d (grow if necessary)
    char *Set(const Pad_String *ps);         // Set to ps (grow if necessary)
    int   Strchr(char c, int index=0) const; // Return index of first instance of c, or -1 if none
    int   Strrchr(char c, int index=0) const;	// Return index of last instance of c, or -1 if none
    int   Strncmp(const Pad_String &ps, int n) const;  // Do a length-limited string comparison like strncmp
  
  Pad_String& Printf(const char *format, ...); // Build up a complex string

  Pad_Bool operator==(const char *string) const;
  Pad_Bool operator==(const Pad_String *string) const;
  Pad_Bool operator==(const Pad_String &string) const;

  Pad_Bool operator!=(const char *string) const;
  Pad_Bool operator!=(const Pad_String *string) const;
  Pad_Bool operator!=(const Pad_String &string) const;

  Pad_String& operator=(int i);
  Pad_String& operator=(double d);
  Pad_String& operator=(float f);
  Pad_String& operator=(const char *string);
  Pad_String& operator=(const Pad_String *string);
  Pad_String& operator=(const Pad_String &string);

  Pad_String& operator+=(int i);
  Pad_String& operator+=(double d);
  Pad_String& operator+=(float f);
  Pad_String& operator+=(char c);
  Pad_String& operator+=(const char *string);
  Pad_String& operator+=(const Pad_String *string);
  Pad_String& operator+=(const Pad_String &string);

  Pad_String& operator<<(int i);
  Pad_String& operator<<(double d);
  Pad_String& operator<<(float f);
  Pad_String& operator<<(char c);
  Pad_String& operator<<(const char *string);
  Pad_String& operator<<(const Pad_String *string);
  Pad_String& operator<<(const Pad_String &string);  

  Pad_String& operator+(int i) const;
  Pad_String& operator+(double d) const;
  Pad_String& operator+(float f) const;
  Pad_String& operator+(const char *string) const;
  Pad_String& operator+(const Pad_String *string) const;
  Pad_String& operator+(const Pad_String &string) const;

  virtual ~Pad_String();
           Pad_String();
           Pad_String(const char *d);
           Pad_String(int len);
           Pad_String(char *d, int len);
           Pad_String(const Pad_String *ps);
           Pad_String(const Pad_String &ps);
};

#endif
