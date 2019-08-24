/*
"(c) Copyright 1993-1997 Pad++ Consortium {University of New Mexico (UNM),
and New York University (NYU)}, All Rights Reserved."  
Licensee can not remove or obscure any of the
copyright or trademark notices in this software.

IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY
FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
ARISING OUT OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY
DERIVATIVES THEREOF, EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE
IS PROVIDED ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE
NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
MODIFICATIONS.

See the file "License" for general information on usage and
redistribution, and the file "LicenseTerms" for the specific license
agreement on usage and redistribution of this file, and the Pad++
software in general.
*/

#include "defs.h"
#include "misc.h"
#include "pad-string.h"
#include <assert.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

//////////////////////////////////////////////
//              Pad_String definitions
//////////////////////////////////////////////  

/////////////////////////////////////////////////////////////////////////////
//
// (Con/De)structors
//
/////////////////////////////////////////////////////////////////////////////

Pad_String::~Pad_String() {
    if (_data) {
        delete[] _data;
        _data = NULL;
    }
}

Pad_String::Pad_String() {
    _space = 0;
    _length = 0;
    _data = NULL;
}

Pad_String::Pad_String(const char *d) {
    Allocate(d);
}

Pad_String::Pad_String(int l) {
    Allocate(l);
}

Pad_String::Pad_String(const Pad_String *ps) {
    Allocate(ps);
}

Pad_String::Pad_String(const Pad_String &ps) {
    Allocate(ps);
}

Pad_String::Pad_String(char *d, int len) {
    if (len) {
        _space = len + 1;
        _data = new char[_space];
        memcpy(_data, d, len);
        _data[len] = '\0';
        _length = strlen(_data);
    } else {
        _space = 0;
        _length = 0;
        _data = NULL;
    }
}

/////////////////////////////////////////////////////////////////////////////
//
// Stream handlers
//
/////////////////////////////////////////////////////////////////////////////

ostream &
operator<<(ostream &os, Pad_String &str) {
    os << str.Get();

    return os;
}

/////////////////////////////////////////////////////////////////////////////
//
// Private functions
//
/////////////////////////////////////////////////////////////////////////////

//
// Increase space by expansion bytes
//
void
Pad_String::Expand(int expansion) {
    char *s;

    // Make sure there's room for terminating NULL char.
    if (_space == 0) {
        _space = 1;
    }

    _space += expansion;
    s = new char[_space];
    if (_data) {
        memcpy(s, _data, _length + 1);
        delete[] _data;
    } else {
        s[0] = '\0';
    }
    _data = s;
}

void
Pad_String::Allocate(int len) {
    _space = len;
    _length = 0;
    if (_space > 0) {
        _data = new char[_space];
        _data[0] = '\0';
    } else {
        _data = NULL;
    }
}

void
Pad_String::Allocate(const char *d) {
    if (d == NULL) {
        _space = 0;
        _length = 0;
        _data = NULL;
    } else {
        _length = strlen(d);
        _space = _length + 1;
        _data = new char[_space];
        strcpy(_data, d);
    }
}

void
Pad_String::Allocate(const Pad_String *ps) {
    if (ps->_space > 0) {
        _length = ps->_length;
        _space = ps->_space;
        _data = new char[_space];
        memcpy(_data, ps->_data, _space);
    } else {
        _space = 0;
        _length = 0;
        _data = NULL;
    }
}

void
Pad_String::Allocate(const Pad_String &ps) {
    Allocate(&ps);
}

/////////////////////////////////////////////////////////////////////////////
//
// Operators
//
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// Operator: ==
/////////////////////////////////////////////////////////////////////////////

Pad_Bool
Pad_String::operator==(const char *ch) const {
    if (_data == NULL) {
        if (ch[0] == '\0') {
            return (TRUE);
        } else {
            return (FALSE);
        }
    } else if (strcasecmp(_data, ch)) {
        return (FALSE);
    } else {
        return (TRUE);
    }
}

Pad_Bool
Pad_String::operator==(const Pad_String &ps) const {
    if (_data == NULL) {
        if (ps == "") {
            return (TRUE);
        } else {
            return (FALSE);
        }
    } else if (strcasecmp(_data, ps.Get())) {
        return (FALSE);
    } else {
        return (TRUE);
    }
}

Pad_Bool
Pad_String::operator==(const Pad_String *ps) const {
    if (_data == NULL) {
        if (*ps == "") {
            return (TRUE);
        } else {
            return (FALSE);
        }
    } else if (strcasecmp(_data, ps->Get())) {
        return (FALSE);
    } else {
        return (TRUE);
    }
}

/////////////////////////////////////////////////////////////////////////////
// Operator: !=
/////////////////////////////////////////////////////////////////////////////

Pad_Bool
Pad_String::operator!=(const char *ch) const {
    if (_data == NULL) {
        if (ch[0] != '\0') {
            return (TRUE);
        } else {
            return (FALSE);
        }
    } else if (strcasecmp(_data, ch)) {
        return (TRUE);
    } else {
        return (FALSE);
    }
}

Pad_Bool
Pad_String::operator!=(const Pad_String &ps) const {
    if (_data == NULL) {
        if (ps != "") {
            return (TRUE);
        } else {
            return (FALSE);
        }
    } else if (strcasecmp(_data, ps.Get())) {
        return (TRUE);
    } else {
        return (FALSE);
    }
}

Pad_Bool
Pad_String::operator!=(const Pad_String *ps) const {
    if (_data == NULL) {
        if (*ps != "") {
            return (TRUE);
        } else {
            return (FALSE);
        }
    } else if (strcasecmp(_data, ps->Get())) {
        return (TRUE);
    } else {
        return (FALSE);
    }
}

/////////////////////////////////////////////////////////////////////////////
// Operator: =
/////////////////////////////////////////////////////////////////////////////

Pad_String &
Pad_String::operator=(int i) {
    this->Printf("%d", i);
    return (*this);
}

Pad_String &
Pad_String::operator=(double d) {
    this->Printf("%lf", d);
    return (*this);
}

Pad_String &
Pad_String::operator=(float t) {
    // Floats get converted to doubles anyway, and
    // va_args seems to have trouble with floats,
    // so make the conversion explicit.
    this->Printf("%lf", (double) t);
    return (*this);
}

Pad_String &
Pad_String::operator=(const char *string) {
    this->Set(string);
    return (*this);
}

Pad_String &
Pad_String::operator=(const Pad_String *string) {
    this->Set(string);
    return (*this);
}

Pad_String &
Pad_String::operator=(const Pad_String &string) {
    this->Set(&string);
    return (*this);
}

/////////////////////////////////////////////////////////////////////////////
// Operator: +
/////////////////////////////////////////////////////////////////////////////

Pad_String &
Pad_String::operator+(int i) const {
    Pad_String *ns = new Pad_String(this);

    *ns += i;
    return (*ns);
}

Pad_String &
Pad_String::operator+(float f) const {
    Pad_String *ns = new Pad_String(this);

    *ns += f;
    return (*ns);
}

Pad_String &
Pad_String::operator+(double d) const {
    Pad_String *ns = new Pad_String(this);

    *ns += d;
    return (*ns);
}

Pad_String &
Pad_String::operator+(const char *string) const {
    Pad_String *ns = new Pad_String(this);

    ns->Append(string);
    return (*ns);
}

Pad_String &
Pad_String::operator+(const Pad_String *string) const {
    Pad_String *ns = new Pad_String(this);

    ns->Append(string);
    return (*ns);
}

Pad_String &
Pad_String::operator+(const Pad_String &string) const {
    Pad_String *ns = new Pad_String(this);

    ns->Append(&string);
    return (*ns);
}

/////////////////////////////////////////////////////////////////////////////
// Operator: +=
/////////////////////////////////////////////////////////////////////////////

Pad_String &
Pad_String::operator+=(int i) {
    this->Append(i);
    return (*this);
}

Pad_String &
Pad_String::operator+=(double d) {
    this->Append(d);
    return (*this);
}

Pad_String &
Pad_String::operator+=(float f) {
    this->Append(f);
    return (*this);
}

Pad_String &
Pad_String::operator+=(char c) {
    this->Append(c);
    return (*this);
}

Pad_String &
Pad_String::operator+=(const char *string) {
    this->Append(string);
    return (*this);
}

Pad_String &
Pad_String::operator+=(const Pad_String *string) {
    this->Append(string);
    return (*this);
}

Pad_String &
Pad_String::operator+=(const Pad_String &string) {
    this->Append(&string);
    return (*this);
}

/////////////////////////////////////////////////////////////////////////////
// Operator: <<
/////////////////////////////////////////////////////////////////////////////

Pad_String &
Pad_String::operator<<(int i) {
    return (*this += i);
}

Pad_String &
Pad_String::operator<<(double d) {
    return (*this += d);
}

Pad_String &
Pad_String::operator<<(float f) {
    return (*this += f);
}

Pad_String &
Pad_String::operator<<(char c) {
    return (*this += c);
}

Pad_String &
Pad_String::operator<<(const char *string) {
    return (*this += string);
}

Pad_String &
Pad_String::operator<<(const Pad_String *string) {
    return (*this += string);
}

Pad_String &
Pad_String::operator<<(const Pad_String &string) {
    return (*this += string);
}

/////////////////////////////////////////////////////////////////////////////
//
// Public functions
//
/////////////////////////////////////////////////////////////////////////////

//
// Return index of first instance of c, or -1 if none
// (starting at index)
//
int
Pad_String::Strchr(char c, int index) const {
    char *p;
    int result;

    if ((_data == NULL) || (index < 0) || (index > _length)) {
        return (-1);
    }

    p = strchr(&_data[index], c);
    if (p) {
        result = p - _data;
    } else {
        result = -1;
    }

    return (result);
}

//
// Return index of last instance of c, or -1 if none
// (starting at index)
//
int
Pad_String::Strrchr(char c, int index) const {
    char *p;
    int result;

    if ((_data == NULL) || (index < 0) || (index > _length)) {
        return (-1);
    }

    p = strrchr(&_data[index], c);
    if (p) {
        result = p - _data;
    } else {
        result = -1;
    }

    return (result);
}

//
// Do a length-limited string comparison like strncmp
//
int
Pad_String::Strncmp(const Pad_String &ps, int n) const {
    if (_data) {
        return (strncmp(_data, ps._data, n));
    } else {
        return (-1);
    }
}


Pad_String &
Pad_String::Printf(char const *format, ...) {
    Pad_String *pval;
    va_list arg_ptr;
    const char *p;
    char *sval;
    int ival;
    double dval;
    Pad_Bool l;
    Pad_String formatString;
    char tmp[30];
    Pad_Bool minLen;

    Set("");
    va_start(arg_ptr, format);
    for (p = format; *p; p++) {
        if (p[0] != '%') {
            if (p[0] == '\\') { // parse any \t's or \n's
                ++p;
                if (p[0] == 'n') {
                    *this += "\n";
                } else if (p[0] == 't') {
                    *this += "\t";
                } else {
                    fprintf(stderr, "bad special character: \\%c", p[0]);
                }
            } else {
                *this += (char) p[0];
            }
            continue;
        }
        p++;
        formatString = "%";
        // Check for 'l' flag - long
        if (*p == 'l') {
            l = TRUE;
            p++;
        } else {
            l = FALSE;
        }
        // Check for '0' flag - Use 0's for padding
        if (*p == '0') {
            formatString += "0";
            p++;
        }
        // Check for format length
        if ((*p >= '1') && (*p <= '9')) {
            formatString += (*p - '0');
            minLen = TRUE;
            p++;
        } else {
            minLen = FALSE;
        }
        switch ((p)[0]) {
            case 'p':
                pval = va_arg(arg_ptr, Pad_String *);
                *this += pval;
                break;
            case 'd':
                ival = va_arg(arg_ptr, int);
                if (minLen == FALSE) {
                    *this += ival;
                } else {
                    formatString += "d";
                    sprintf(tmp, formatString.Get(), ival);
                    *this += tmp;
                }
                break;
            case 'f':
                // Floats get promoted to doubles, so never
                // try and do this as a float (especially
                // since it doesn't work - at least with SGI's CC).
                dval = va_arg(arg_ptr, double);
                if (minLen == FALSE) {
                    *this += dval;
                } else {
                    formatString += "f";
                    sprintf(tmp, formatString.Get(), ival);
                    *this += tmp;
                }
                break;
            case 's':
                sval = va_arg(arg_ptr, char *);
                *this += sval;
                break;
            case 'x':
                ival = va_arg(arg_ptr, int);
                formatString += "x";
                sprintf(tmp, formatString.Get(), ival);
                *this += tmp;
                break;
            default:
                fprintf(stderr, "Bad format: %s", p);
        }
    }
    va_end(arg_ptr);
    return (*this);
}

char *
Pad_String::Set(const char *d) {
    int new_len;

    if (!d) {
        d = "";
    }

    new_len = strlen(d);
    if (new_len >= _space) {
        if (_data) {
            delete[] _data;
        }
        Allocate(d);
    } else {
        strcpy(_data, d);
        _length = new_len;
    }

    return _data;
}

char *
Pad_String::Set(const char *d, int len) {
    if (!d) {
        d = "";
    }

    if (len >= _space) {
        if (_data) {
            delete[] _data;
        }
        Allocate(len + 1);
    }

    strncpy(_data, d, len);
    _data[len] = '\0';
    _length = strlen(_data);
    return _data;
}

char *
Pad_String::Set(const Pad_String *ps) {
    if (!ps || !ps->_data) {
        if (_data) {
            _data[0] = '\0';;
            _length = 0;
        }
    } else {
        if (ps->_length >= _space) {
            if (_data) {
                delete[] _data;
            }
            Allocate(ps);
        } else {
            strncpy(_data, ps->_data, ps->_length + 1);
            _length = ps->_length;
        }
    }

    return _data;
}

//
// Make sure that there is some data to return!
//
char *
Pad_String::Get(void) const {
    return (char *) ((_data ? _data : ""));
}

char *
Pad_String::Get(int index) const {
    if (_data && (index < _space)) {
        return (&_data[index]);
    } else {
        return "";
    }
}

char
Pad_String::Get_char(int index) const {
    if (_data && (index < _space)) {
        return _data[index];
    } else {
        return '\0';
    }
}

int
Pad_String::Length(void) const {
    return _length;
}

void
Pad_String::Append(int num) {
    char numStorage[40];

    sprintf(numStorage, "%d", num);
    Append(numStorage);
}

void
Pad_String::Append(float num) {
    char numStorage[40];

    sprintf(numStorage, "%g", num);
    Append(numStorage);
}

void
Pad_String::Append(double num) {
    char numStorage[40];

    sprintf(numStorage, "%g", num);
    Append(numStorage);
}

void
Pad_String::Append(char c) {
    if (c == '\0') {
        return;
    }

    if (_length + 1 >= _space) {
        Expand(1 + _space);
    }
    _data[_length++] = c;
    _data[_length] = '\0';
}

void
Pad_String::Append(const char *d) {
    int new_len;

    if (!d) {
        return;
    }

    new_len = strlen(d);
    if (_length + new_len >= _space) {
        Expand(_space + new_len + 1);
    }
    memcpy(&_data[_length], d, new_len);
    _data[_length + new_len] = '\0';
    _length += new_len;
}

void
Pad_String::Append(const Pad_String &ps) {
    Append(ps._data);
}

void
Pad_String::Append(const Pad_String *ps) {
    Append(ps->_data);
}

void
Pad_String::Append_with_escapes(char *ptr1) {
#define ESCAPES "$[]{}\"\t\n\\"

    char *ptr2 = strpbrk(ptr1, ESCAPES);

    if (ptr2) {
        char c;
        do {
            c = *ptr2;
            *ptr2 = 0;
            Append(ptr1);
            Append("\\");
            switch (c) {
                case '$':
                case '[':
                case ']':
                case '{':
                case '}':
                case '\"':
                    Append(c);
                    break;
                case '\t':
                    Append('t');
                    break;
                case '\n':
                    Append('n');
                    break;
            }
            *ptr2 = c;
            ptr1 = ptr2 + 1;
        } while ((ptr2 = strpbrk(ptr1, ESCAPES)));
    }

    if (ptr1) {
        Append(ptr1);
    }

#undef ESCAPES
}

void
Pad_String::Insert(char c, int index) {
    char *ptr, *endptr;

    assert(index <= _length);

    if (index == _length) {
        Append(c);
        return;
    }

    if (c == '\0') {
        _data[index] = c;
        _length = index;
        return;
    }

    if (_length + 1 >= _space) {
        Expand(_space + 1);
    }
    // Copy string over one manually from the end
    // Can't use strpy because buffers overlap
    ptr = &_data[_length];
    endptr = &_data[index];
    do {
        *(ptr + 1) = *ptr;
        ptr--;
    } while (ptr >= endptr);

    _data[index] = c;

    _length++;
}

void
Pad_String::Insert(const char *d, int index) {
    int new_len;
    char *ptr, *endptr;

    assert(index <= _length);

    if (index == _length) {
        Append(d);
        return;
    }

    new_len = strlen(d);
    if (_length + new_len >= _space) {
        Expand(_space + new_len + 1);
    }
    // Copy string over one manually from the end
    // Can't use strpy because buffers overlap
    ptr = &_data[_length];
    endptr = &_data[index];
    do {
        *(ptr + new_len) = *ptr;
        ptr--;
    } while (ptr >= endptr);

    memcpy(endptr, d, new_len);

    _length += new_len;
}

void
Pad_String::Insert(const Pad_String *ps, int index) {
    Insert(ps->_data, index);
}

//
// Erase the single character at index
//
void
Pad_String::Erase(int index) {
    assert(index < _length);

    memcpy(&_data[index], &_data[index + 1], (_length - index));
    _length--;
}

//
// Erase len characters starting at index
//
void
Pad_String::Erase(int index, int len) {
    if (len > 0) {
        assert(index + len - 1 < _length);

        memcpy(&_data[index], &_data[index + len], (_length - index - len + 1));
        _length -= len;
    }
}
