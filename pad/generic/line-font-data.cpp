/*
"(c) Copyright 1993-1995 Pad++ Consortium {University of New Mexico (UNM),
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

#include "line-font.h"

#include <stdarg.h>

Pad_LineFontChar::Pad_LineFontChar()
{
    index = 0;
    segments = 0;
    segment_length = NULL;
    point = NULL;
    width = 0.0;
}

void
Pad_LineFontChar::Init(int new_segments, ...)
{
    int i;
    int points = 0;
    va_list arg_ptr;

    va_start(arg_ptr, new_segments);

    segments = new_segments;
    if (segments > 0) {
	segment_length = new int[segments];
	for (i=0; i<segments; i++) {
	    segment_length[i] = va_arg(arg_ptr, int);
	    points += segment_length[i];
	}
	point = new Pad_Point[points];
	for (i=0; i<points; i++) {
	    point[i].Set(0.0, 0.0);
	}
    }
    width = 1.0;
    
    va_end(arg_ptr);
}

void
Pad_LineFontChar::Add_point(float x, float y)
{
    point[index++].Set(x, y);
}

Pad_LineFontChar *Pad_LineFont::letters[256];

static void
InitLowercaseAlpha(Pad_LineFontChar *letters[256])
{
    letters['a']->Init(1, 13);
    letters['a']->Add_point(0.1624, 0.78005);
    letters['a']->Add_point(0.3319, 0.8871);
    letters['a']->Add_point(0.6144, 0.8871);
    letters['a']->Add_point(0.7274, 0.7265);
    letters['a']->Add_point(0.7274, 0.24475);
    letters['a']->Add_point(0.53475, 0.11515);
    letters['a']->Add_point(0.2179, 0.11515);
    letters['a']->Add_point(0.0821, 0.2009);
    letters['a']->Add_point(0.0821, 0.41535);
    letters['a']->Add_point(0.26315, 0.544);
    letters['a']->Add_point(0.71585, 0.544);
    letters['a']->Add_point(0.7274, 0.1377);
    letters['a']->Add_point(0.8969, 0.1377);

    letters['b']->Init(1, 16);
    letters['b']->Add_point(0.0392, 1.23015);
    letters['b']->Add_point(0.1679, 1.23015);
    letters['b']->Add_point(0.1679, 0.41535);
    letters['b']->Add_point(0.25365, 0.2438);
    letters['b']->Add_point(0.39995, 0.11515);
    letters['b']->Add_point(0.65225, 0.11515);
    letters['b']->Add_point(0.8036, 0.2222);
    letters['b']->Add_point(0.85405, 0.32925);
    letters['b']->Add_point(0.85405, 0.6504);
    letters['b']->Add_point(0.7683, 0.8013);
    letters['b']->Add_point(0.6396, 0.8442);
    letters['b']->Add_point(0.3394, 0.8442);
    letters['b']->Add_point(0.25365, 0.7584);
    letters['b']->Add_point(0.1679, 0.62975);
    letters['b']->Add_point(0.1679, 0.11515);
    letters['b']->Add_point(0.0392, 0.11515);

    letters['c']->Init(1, 13);
    letters['c']->Add_point(0.8112, 0.8871);
    letters['c']->Add_point(0.8112, 0.67265);
    letters['c']->Add_point(0.8112, 0.7584);
    letters['c']->Add_point(0.6709, 0.87045);
    letters['c']->Add_point(0.29625, 0.87045);
    letters['c']->Add_point(0.1892, 0.8169);
    letters['c']->Add_point(0.08215, 0.65635);
    letters['c']->Add_point(0.08215, 0.3352);
    letters['c']->Add_point(0.1892, 0.1746);
    letters['c']->Add_point(0.33945, 0.11515);
    letters['c']->Add_point(0.5539, 0.11515);
    letters['c']->Add_point(0.68255, 0.158);
    letters['c']->Add_point(0.8315, 0.28165);

    letters['d']->Init(1, 17);
    letters['d']->Add_point(0.5968, 1.23015);
    letters['d']->Add_point(0.7729, 1.23015);
    letters['d']->Add_point(0.7729, 0.67265);
    letters['d']->Add_point(0.7729, 0.2867);
    letters['d']->Add_point(0.6964, 0.1746);
    letters['d']->Add_point(0.54435, 0.1211);
    letters['d']->Add_point(0.2909, 0.1211);
    letters['d']->Add_point(0.13885, 0.22815);
    letters['d']->Add_point(0.08815, 0.3352);
    letters['d']->Add_point(0.08815, 0.65635);
    letters['d']->Add_point(0.18955, 0.8169);
    letters['d']->Add_point(0.2909, 0.87045);
    letters['d']->Add_point(0.59505, 0.87045);
    letters['d']->Add_point(0.6964, 0.8169);
    letters['d']->Add_point(0.7729, 0.71555);
    letters['d']->Add_point(0.7729, 0.11515);
    letters['d']->Add_point(0.897, 0.11515);

    letters['e']->Init(1, 13);
    letters['e']->Add_point(0.0822, 0.5011);
    letters['e']->Add_point(0.81125, 0.5011);
    letters['e']->Add_point(0.81125, 0.67265);
    letters['e']->Add_point(0.72545, 0.8013);
    letters['e']->Add_point(0.61745, 0.8645);
    letters['e']->Add_point(0.2963, 0.8645);
    letters['e']->Add_point(0.16795, 0.8013);
    letters['e']->Add_point(0.0822, 0.6504);
    letters['e']->Add_point(0.0822, 0.32925);
    letters['e']->Add_point(0.16795, 0.2009);
    letters['e']->Add_point(0.2963, 0.11515);
    letters['e']->Add_point(0.5968, 0.11515);
    letters['e']->Add_point(0.81125, 0.2009);
  
    letters['f']->Init(1, 10);
    letters['f']->Add_point(0.85415, 1.1963);
    letters['f']->Add_point(0.533, 1.1963);
    letters['f']->Add_point(0.3824, 1.01575);
    letters['f']->Add_point(0.3824, 0.8442);
    letters['f']->Add_point(0.168, 0.8442);
    letters['f']->Add_point(0.81125, 0.8442);
    letters['f']->Add_point(0.3824, 0.8442);
    letters['f']->Add_point(0.3824, 0.11515);
    letters['f']->Add_point(0.168, 0.11515);
    letters['f']->Add_point(0.7684, 0.11515);

    letters['g']->Init(1, 17);
    letters['g']->Add_point(0.8859, 0.8871);
    letters['g']->Add_point(0.7684, 0.8871);
    letters['g']->Add_point(0.7684, 0.2867);
    letters['g']->Add_point(0.7069, 0.24475);
    letters['g']->Add_point(0.554, 0.158);
    letters['g']->Add_point(0.33955, 0.158);
    letters['g']->Add_point(0.2109, 0.2009);
    letters['g']->Add_point(0.1251, 0.3518);
    letters['g']->Add_point(0.1251, 0.673);
    letters['g']->Add_point(0.29665, 0.8442);
    letters['g']->Add_point(0.5111, 0.8442);
    letters['g']->Add_point(0.63975, 0.8013);
    letters['g']->Add_point(0.7684, 0.62975);
    letters['g']->Add_point(0.7684, -0.0135);
    letters['g']->Add_point(0.7069, -0.18345);
    letters['g']->Add_point(0.57265, -0.237);
    letters['g']->Add_point(0.25935, -0.237);

    letters['h']->Init(1, 13);
    letters['h']->Add_point(0.08225, 1.23015);
    letters['h']->Add_point(0.1906, 1.23015);
    letters['h']->Add_point(0.1906, 0.10615);
    letters['h']->Add_point(0.03005, 0.10615);
    letters['h']->Add_point(0.3512, 0.10615);
    letters['h']->Add_point(0.1906, 0.10615);
    letters['h']->Add_point(0.1906, 0.6949);
    letters['h']->Add_point(0.4047, 0.8555);
    letters['h']->Add_point(0.67235, 0.8555);
    letters['h']->Add_point(0.7794, 0.6949);
    letters['h']->Add_point(0.7794, 0.10615);
    letters['h']->Add_point(0.6188, 0.10615);
    letters['h']->Add_point(0.94, 0.10615);

    letters['i']->Init(2, 5, 2);
    letters['i']->Add_point(0.23045, 0.8555);
    letters['i']->Add_point(0.47995, 0.8555);
    letters['i']->Add_point(0.47995, 0.10615);
    letters['i']->Add_point(0.16805, 0.10615);
    letters['i']->Add_point(0.85425, 0.10615);
    letters['i']->Add_point(0.47995, 1.23015);
    letters['i']->Add_point(0.47995, 1.0696);

    letters['j']->Init(2, 7, 2);
    letters['j']->Add_point(0.1959, 0.87045);
    letters['j']->Add_point(0.6827, 0.87045);
    letters['j']->Add_point(0.6827, -0.0395);
    letters['j']->Add_point(0.63405, -0.14655);
    letters['j']->Add_point(0.488, -0.25365);
    letters['j']->Add_point(0.1959, -0.25365);
    letters['j']->Add_point(0.24455, -0.25365);
    letters['j']->Add_point(0.59695, 1.27305);
    letters['j']->Add_point(0.59695, 1.11245);

    letters['k']->Init(1, 15);
    letters['k']->Add_point(0.03945, 1.2451);
    letters['k']->Add_point(0.224, 1.2451);
    letters['k']->Add_point(0.224, 0.1211);
    letters['k']->Add_point(0.03945, 0.1211);
    letters['k']->Add_point(0.224, 0.1211);
    letters['k']->Add_point(0.224, 0.49575);
    letters['k']->Add_point(0.4086, 0.49575);
    letters['k']->Add_point(0.65465, 0.87045);
    letters['k']->Add_point(0.5316, 0.87045);
    letters['k']->Add_point(0.8392, 0.87045);
    letters['k']->Add_point(0.65465, 0.87045);
    letters['k']->Add_point(0.4086, 0.49575);
    letters['k']->Add_point(0.7777, 0.1211);
    letters['k']->Add_point(0.59315, 0.1211);
    letters['k']->Add_point(0.90075, 0.1211);

    letters['l']->Init(1, 5);
    letters['l']->Add_point(0.1788, 1.2451);
    letters['l']->Add_point(0.49995, 1.2451);
    letters['l']->Add_point(0.49995, 0.1211);
    letters['l']->Add_point(0.12525, 0.1211);
    letters['l']->Add_point(0.8746, 0.1211);

    letters['m']->Init(1, 19);
    letters['m']->Add_point(-0.03535, 0.8645);
    letters['m']->Add_point(0.1044, 0.8645);
    letters['m']->Add_point(0.1044, 0.11515);
    letters['m']->Add_point(-0.054, 0.11515);
    letters['m']->Add_point(0.2628, 0.11515);
    letters['m']->Add_point(0.1044, 0.11515);
    letters['m']->Add_point(0.1044, 0.81095);
    letters['m']->Add_point(0.2628, 0.8645);
    letters['m']->Add_point(0.42115, 0.8645);
    letters['m']->Add_point(0.47395, 0.75745);
    letters['m']->Add_point(0.47395, 0.11515);
    letters['m']->Add_point(0.57955, 0.11515);
    letters['m']->Add_point(0.47395, 0.11515);
    letters['m']->Add_point(0.47395, 0.75745);
    letters['m']->Add_point(0.63235, 0.8645);
    letters['m']->Add_point(0.79075, 0.8645);
    letters['m']->Add_point(0.84355, 0.75745);
    letters['m']->Add_point(0.84355, 0.11515);
    letters['m']->Add_point(0.94715, 0.11515);

    letters['n']->Init(1, 13);
    letters['n']->Add_point(0.06025, 0.87045);
    letters['n']->Add_point(0.21495, 0.87045);
    letters['n']->Add_point(0.21495, 0.1211);
    letters['n']->Add_point(0.06025, 0.1211);
    letters['n']->Add_point(0.3696, 0.1211);
    letters['n']->Add_point(0.21495, 0.1211);
    letters['n']->Add_point(0.21495, 0.70985);
    letters['n']->Add_point(0.42115, 0.87045);
    letters['n']->Add_point(0.6789, 0.87045);
    letters['n']->Add_point(0.782, 0.70985);
    letters['n']->Add_point(0.782, 0.1211);
    letters['n']->Add_point(0.62735, 0.1211);
    letters['n']->Add_point(0.9367, 0.1211);

    letters['o']->Init(1, 13);
    letters['o']->Add_point(0.33975, 0.8871);
    letters['o']->Add_point(0.5542, 0.8871);
    letters['o']->Add_point(0.7686, 0.8013);
    letters['o']->Add_point(0.8544, 0.62975);
    letters['o']->Add_point(0.8544, 0.41535);
    letters['o']->Add_point(0.7686, 0.2438);
    letters['o']->Add_point(0.56315, 0.1211);
    letters['o']->Add_point(0.33975, 0.11515);
    letters['o']->Add_point(0.1682, 0.2438);
    letters['o']->Add_point(0.08245, 0.41535);
    letters['o']->Add_point(0.08245, 0.62975);
    letters['o']->Add_point(0.1682, 0.7584);
    letters['o']->Add_point(0.33975, 0.8871);

    letters['p']->Init(1, 17);
    letters['p']->Add_point(0.0396, 0.85325);
    letters['p']->Add_point(0.19095, 0.85325);
    letters['p']->Add_point(0.2013, 0.32955);
    letters['p']->Add_point(0.28215, 0.2009);
    letters['p']->Add_point(0.39275, 0.1039);
    letters['p']->Add_point(0.6955, 0.1039);
    letters['p']->Add_point(0.8115, 0.2438);
    letters['p']->Add_point(0.8973, 0.37155);
    letters['p']->Add_point(0.8973, 0.63915);
    letters['p']->Add_point(0.7964, 0.7462);
    letters['p']->Add_point(0.64505, 0.85325);
    letters['p']->Add_point(0.39275, 0.85325);
    letters['p']->Add_point(0.2414, 0.7462);
    letters['p']->Add_point(0.19095, 0.63915);
    letters['p']->Add_point(0.19095, -0.27085);
    letters['p']->Add_point(0.0396, -0.27085);
    letters['p']->Add_point(0.46845, -0.27085);
  
    letters['q']->Init(1, 15);
    letters['q']->Add_point(0.8973, 0.8871);
    letters['q']->Add_point(0.76865, 0.8871);
    letters['q']->Add_point(0.76865, 0.32955);
    letters['q']->Add_point(0.51135, 0.11515);
    letters['q']->Add_point(0.2969, 0.11515);
    letters['q']->Add_point(0.16825, 0.2009);
    letters['q']->Add_point(0.0825, 0.32955);
    letters['q']->Add_point(0.0825, 0.62975);
    letters['q']->Add_point(0.1455, 0.7634);
    letters['q']->Add_point(0.2969, 0.8442);
    letters['q']->Add_point(0.55425, 0.8442);
    letters['q']->Add_point(0.76865, 0.67265);
    letters['q']->Add_point(0.76865, -0.22795);
    letters['q']->Add_point(0.8973, -0.25365);
    letters['q']->Add_point(0.5214, -0.25365);

    letters['r']->Init(1, 11);
    letters['r']->Add_point(0.1051, 0.11515);
    letters['r']->Add_point(0.72755, 0.11515);
    letters['r']->Add_point(0.33985, 0.11515);
    letters['r']->Add_point(0.33985, 0.8871);
    letters['r']->Add_point(0.1254, 0.8871);
    letters['r']->Add_point(0.33145, 0.8871);
    letters['r']->Add_point(0.33985, 0.67265);
    letters['r']->Add_point(0.44465, 0.72165);
    letters['r']->Add_point(0.671, 0.8871);
    letters['r']->Add_point(0.8116, 0.8871);
    letters['r']->Add_point(0.89735, 0.8013);

    letters['s']->Init(1, 14);
    letters['s']->Add_point(0.7687, 0.8645);
    letters['s']->Add_point(0.7687, 0.7039);
    letters['s']->Add_point(0.5543, 0.8871);
    letters['s']->Add_point(0.2541, 0.8871);
    letters['s']->Add_point(0.1683, 0.7584);
    letters['s']->Add_point(0.1683, 0.5869);
    letters['s']->Add_point(0.29695, 0.5011);
    letters['s']->Add_point(0.59715, 0.5011);
    letters['s']->Add_point(0.7687, 0.4363);
    letters['s']->Add_point(0.7687, 0.2222);
    letters['s']->Add_point(0.64395, 0.11515);
    letters['s']->Add_point(0.33985, 0.11515);
    letters['s']->Add_point(0.12545, 0.2867);
    letters['s']->Add_point(0.12545, 0.11515);

    letters['t']->Init(1, 10);
    letters['t']->Add_point(0.08255, 0.8013);
    letters['t']->Add_point(0.297, 0.8013);
    letters['t']->Add_point(0.297, 1.1015);
    letters['t']->Add_point(0.297, 0.8013);
    letters['t']->Add_point(0.76875, 0.8013);
    letters['t']->Add_point(0.297, 0.8013);
    letters['t']->Add_point(0.297, 0.2438);
    letters['t']->Add_point(0.42565, 0.11515);
    letters['t']->Add_point(0.6401, 0.11515);
    letters['t']->Add_point(0.8545, 0.2009);

    letters['u']->Init(1, 11);
    letters['u']->Add_point(0.0397, 0.8645);
    letters['u']->Add_point(0.19225, 0.8645);
    letters['u']->Add_point(0.19225, 0.2757);
    letters['u']->Add_point(0.3448, 0.11515);
    letters['u']->Add_point(0.54815, 0.11515);
    letters['u']->Add_point(0.75155, 0.2757);
    letters['u']->Add_point(0.75155, 0.8645);
    letters['u']->Add_point(0.599, 0.8645);
    letters['u']->Add_point(0.75155, 0.8645);
    letters['u']->Add_point(0.75155, 0.11515);
    letters['u']->Add_point(0.85325, 0.11515);

    letters['v']->Init(1, 7);
    letters['v']->Add_point(-0.04605, 0.8871);
    letters['v']->Add_point(0.32865, 0.8871);
    letters['v']->Add_point(0.11455, 0.8871);
    letters['v']->Add_point(0.4357, 0.09485);
    letters['v']->Add_point(0.81035, 0.8871);
    letters['v']->Add_point(0.59625, 0.8871);
    letters['v']->Add_point(0.9174, 0.8871);

    letters['w']->Init(1, 9);
    letters['w']->Add_point(-0.0889, 0.8871);
    letters['w']->Add_point(0.2113, 0.8871);
    letters['w']->Add_point(0.03975, 0.8871);
    letters['w']->Add_point(0.2089, 0.1211);
    letters['w']->Add_point(0.44715, 0.65635);
    letters['w']->Add_point(0.6854, 0.1211);
    letters['w']->Add_point(0.8546, 0.8871);
    letters['w']->Add_point(0.68305, 0.8871);
    letters['w']->Add_point(0.98325, 0.8871);

    letters['x']->Init(1, 17);
    letters['x']->Add_point(-0.0031, 0.8871);
    letters['x']->Add_point(0.3716, 0.8871);
    letters['x']->Add_point(0.1575, 0.8871);
    letters['x']->Add_point(0.47865, 0.5011);
    letters['x']->Add_point(0.1575, 0.11515);
    letters['x']->Add_point(0.05045, 0.11515);
    letters['x']->Add_point(0.3716, 0.11515);
    letters['x']->Add_point(0.1575, 0.11515);
    letters['x']->Add_point(0.47865, 0.5011);
    letters['x']->Add_point(0.7998, 0.11515);
    letters['x']->Add_point(0.5857, 0.11515);
    letters['x']->Add_point(0.96035, 0.11515);
    letters['x']->Add_point(0.7998, 0.11515);
    letters['x']->Add_point(0.47865, 0.5011);
    letters['x']->Add_point(0.7998, 0.8871);
    letters['x']->Add_point(0.5857, 0.8871);
    letters['x']->Add_point(0.90685, 0.8871);

    letters['y']->Init(1, 12);
    letters['y']->Add_point(-0.00305, 0.8871);
    letters['y']->Add_point(0.28545, 0.8871);
    letters['y']->Add_point(0.1256, 0.8871);
    letters['y']->Add_point(0.46865, 0.158);
    letters['y']->Add_point(0.29715, -0.22795);
    letters['y']->Add_point(-0.00305, -0.237);
    letters['y']->Add_point(0.51625, -0.237);
    letters['y']->Add_point(0.29715, -0.22795);
    letters['y']->Add_point(0.46865, 0.158);
    letters['y']->Add_point(0.86245, 0.8871);
    letters['y']->Add_point(0.63165, 0.8871);
    letters['y']->Add_point(0.92015, 0.8871);

    letters['z']->Init(1, 8);
    letters['z']->Add_point(0.1685, 0.7584);
    letters['z']->Add_point(0.1685, 0.8871);
    letters['z']->Add_point(0.7689, 0.8871);
    letters['z']->Add_point(0.7689, 0.8013);
    letters['z']->Add_point(0.1685, 0.2009);
    letters['z']->Add_point(0.1685, 0.11515);
    letters['z']->Add_point(0.7689, 0.11515);
    letters['z']->Add_point(0.7689, 0.2867);
}

static void
InitUppercaseAlpha(Pad_LineFontChar *letters[256])
{
    letters['A']->Init(1, 12);
    letters['A']->Add_point(0.29695, 0.1025);
    letters['A']->Add_point(-0.0245, 0.1025);
    letters['A']->Add_point(0.08265, 0.1025);
    letters['A']->Add_point(0.4041, 1.141);
    letters['A']->Add_point(0.1898, 1.141);
    letters['A']->Add_point(0.5648, 1.141);
    letters['A']->Add_point(0.8862, 0.1025);
    letters['A']->Add_point(0.9398, 0.1025);
    letters['A']->Add_point(0.6184, 0.1025);
    letters['A']->Add_point(0.8862, 0.1025);
    letters['A']->Add_point(0.76825, 0.4756);
    letters['A']->Add_point(0.21075, 0.4756);

    letters['B']->Init(1, 13);
    letters['B']->Add_point(0.1729, 1.12995);
    letters['B']->Add_point(0.1729, 0.11435);
    letters['B']->Add_point(0.07245, 0.11435);
    letters['B']->Add_point(0.67515, 0.11435);
    letters['B']->Add_point(0.8761, 0.2272);
    letters['B']->Add_point(0.8761, 0.56575);
    letters['B']->Add_point(0.67515, 0.6786);
    letters['B']->Add_point(0.1729, 0.6786);
    letters['B']->Add_point(0.67515, 0.6786);
    letters['B']->Add_point(0.8761, 0.79145);
    letters['B']->Add_point(0.8761, 1.01715);
    letters['B']->Add_point(0.67515, 1.12995);
    letters['B']->Add_point(0.07245, 1.12995);
  
    letters['C']->Init(1, 11);
    letters['C']->Add_point(0.8541, 1.17465);
    letters['C']->Add_point(0.8541, 0.8551);
    letters['C']->Add_point(0.8541, 1.0377);
    letters['C']->Add_point(0.66075, 1.129);
    letters['C']->Add_point(0.3393, 1.129);
    letters['C']->Add_point(0.125, 0.9009);
    letters['C']->Add_point(0.125, 0.4447);
    letters['C']->Add_point(0.23215, 0.2166);
    letters['C']->Add_point(0.44645, 0.1025);
    letters['C']->Add_point(0.66075, 0.1025);
    letters['C']->Add_point(0.89695, 0.216);

    letters['D']->Init(1, 9);
    letters['D']->Add_point(0.08215, 1.1185);
    letters['D']->Add_point(0.62895, 1.1185);
    letters['D']->Add_point(0.8112, 0.8953);
    letters['D']->Add_point(0.8541, 0.61715);
    letters['D']->Add_point(0.8112, 0.3372);
    letters['D']->Add_point(0.62895, 0.114);
    letters['D']->Add_point(0.08215, 0.114);
    letters['D']->Add_point(0.1733, 0.114);
    letters['D']->Add_point(0.1733, 1.1185);
  
    letters['E']->Init(1, 14);
    letters['E']->Add_point(0.8378, 0.9042);
    letters['E']->Add_point(0.8378, 1.12995);
    letters['E']->Add_point(0.03425, 1.12995);
    letters['E']->Add_point(0.23515, 1.12995);
    letters['E']->Add_point(0.23515, 0.67845);
    letters['E']->Add_point(0.53645, 0.67845);
    letters['E']->Add_point(0.53645, 0.9042);
    letters['E']->Add_point(0.53645, 0.45265);
    letters['E']->Add_point(0.53645, 0.67845);
    letters['E']->Add_point(0.23515, 0.67845);
    letters['E']->Add_point(0.23515, 0.114);
    letters['E']->Add_point(0.03425, 0.114);
    letters['E']->Add_point(0.8378, 0.114);
    letters['E']->Add_point(0.8378, 0.45265);

    letters['F']->Init(1, 13);
    letters['F']->Add_point(0.9014, 0.8972);
    letters['F']->Add_point(0.9014, 1.1195);
    letters['F']->Add_point(0.03935, 1.1195);
    letters['F']->Add_point(0.25485, 1.1195);
    letters['F']->Add_point(0.25485, 0.67485);
    letters['F']->Add_point(0.5781, 0.67485);
    letters['F']->Add_point(0.5781, 0.8972);
    letters['F']->Add_point(0.5781, 0.45255);
    letters['F']->Add_point(0.5781, 0.67485);
    letters['F']->Add_point(0.25485, 0.67485);
    letters['F']->Add_point(0.25485, 0.11905);
    letters['F']->Add_point(0.03935, 0.11905);
    letters['F']->Add_point(0.5781, 0.11905);

    letters['G']->Init(1, 15);
    letters['G']->Add_point(0.8044, 1.1195);
    letters['G']->Add_point(0.8044, 0.8972);
    letters['G']->Add_point(0.8044, 1.0084);
    letters['G']->Add_point(0.554, 1.13175);
    letters['G']->Add_point(0.38245, 1.13175);
    letters['G']->Add_point(0.2109, 1.046);
    letters['G']->Add_point(0.05435, 0.786);
    letters['G']->Add_point(0.05435, 0.45255);
    letters['G']->Add_point(0.1615, 0.2302);
    letters['G']->Add_point(0.38245, 0.1025);
    letters['G']->Add_point(0.59685, 0.1025);
    letters['G']->Add_point(0.8113, 0.18825);
    letters['G']->Add_point(0.8113, 0.48845);
    letters['G']->Add_point(0.89705, 0.48845);
    letters['G']->Add_point(0.5111, 0.48845);

    letters['H']->Init(1, 16);
    letters['H']->Add_point(0.05005, 1.13095);
    letters['H']->Add_point(0.3715, 1.13095);
    letters['H']->Add_point(0.1989, 1.13095);
    letters['H']->Add_point(0.1989, 0.13045);
    letters['H']->Add_point(-0.0035, 0.13045);
    letters['H']->Add_point(0.3001, 0.13045);
    letters['H']->Add_point(0.1989, 0.13045);
    letters['H']->Add_point(0.1989, 0.6863);
    letters['H']->Add_point(0.7465, 0.6863);
    letters['H']->Add_point(0.7465, 1.13095);
    letters['H']->Add_point(0.60375, 1.13095);
    letters['H']->Add_point(0.90735, 1.13095);
    letters['H']->Add_point(0.7465, 1.13095);
    letters['H']->Add_point(0.7465, 0.13045);
    letters['H']->Add_point(0.60375, 0.13045);
    letters['H']->Add_point(0.90735, 0.13045);

    letters['I']->Init(1, 6);
    letters['I']->Add_point(0.16805, 1.1195);
    letters['I']->Add_point(0.7575, 1.1195);
    letters['I']->Add_point(0.43605, 1.1195);
    letters['I']->Add_point(0.43605, 0.11905);
    letters['I']->Add_point(0.16805, 0.11905);
    letters['I']->Add_point(0.7575, 0.11905);

    letters['J']->Init(1, 9);
    letters['J']->Add_point(0.3641, 1.10805);
    letters['J']->Add_point(0.94005, 1.10805);
    letters['J']->Add_point(0.6827, 1.08885);
    letters['J']->Add_point(0.6827, 0.31695);
    letters['J']->Add_point(0.6186, 0.21875);
    letters['J']->Add_point(0.45785, 0.1076);
    letters['J']->Add_point(0.25025, 0.1076);
    letters['J']->Add_point(0.08285, 0.23115);
    letters['J']->Add_point(0.08285, 0.49665);

    letters['K']->Init(1, 15);
    letters['K']->Add_point(0.09455, 1.1195);
    letters['K']->Add_point(0.416, 1.1195);
    letters['K']->Add_point(0.2017, 1.1195);
    letters['K']->Add_point(0.2017, 0.11905);
    letters['K']->Add_point(0.03945, 0.1076);
    letters['K']->Add_point(0.38255, 0.1076);
    letters['K']->Add_point(0.2017, 0.11905);
    letters['K']->Add_point(0.2017, 0.45255);
    letters['K']->Add_point(0.73745, 1.1195);
    letters['K']->Add_point(0.6303, 1.1195);
    letters['K']->Add_point(0.95175, 1.1195);
    letters['K']->Add_point(0.73745, 1.1195);
    letters['K']->Add_point(0.4694, 0.7305);
    letters['K']->Add_point(0.8543, 0.1076);
    letters['K']->Add_point(0.94005, 0.1076);

    letters['L']->Init(1, 7);
    letters['L']->Add_point(0.0395, 1.1195);
    letters['L']->Add_point(0.47385, 1.1195);
    letters['L']->Add_point(0.25665, 1.1195);
    letters['L']->Add_point(0.25665, 0.11905);
    letters['L']->Add_point(0.0395, 0.11905);
    letters['L']->Add_point(0.9082, 0.11905);
    letters['L']->Add_point(0.9082, 0.45255);

    letters['M']->Init(1, 15);
    letters['M']->Add_point(0.2968, 0.1076);
    letters['M']->Add_point(-0.04625, 0.1076);
    letters['M']->Add_point(0.1253, 0.1025);
    letters['M']->Add_point(0.1253, 1.13175);
    letters['M']->Add_point(0.0395, 1.13175);
    letters['M']->Add_point(0.21105, 1.13175);
    letters['M']->Add_point(0.1157, 1.11955);
    letters['M']->Add_point(0.48345, 0.50815);
    letters['M']->Add_point(0.81145, 1.13175);
    letters['M']->Add_point(0.6828, 1.13175);
    letters['M']->Add_point(0.8972, 1.13175);
    letters['M']->Add_point(0.81145, 1.13175);
    letters['M']->Add_point(0.81145, 0.1025);
    letters['M']->Add_point(0.597, 0.1076);
    letters['M']->Add_point(0.9401, 0.1076);

    letters['N']->Init(1, 11);
    letters['N']->Add_point(-0.0891, 0.11905);
    letters['N']->Add_point(0.33945, 0.11905);
    letters['N']->Add_point(0.12515, 0.11905);
    letters['N']->Add_point(0.12515, 1.1195);
    letters['N']->Add_point(-0.0891, 1.1195);
    letters['N']->Add_point(0.2323, 1.1195);
    letters['N']->Add_point(0.7143, 0.1191);
    letters['N']->Add_point(0.76805, 0.1191);
    letters['N']->Add_point(0.76805, 1.1195);
    letters['N']->Add_point(0.55355, 1.1195);
    letters['N']->Add_point(0.8752, 1.1195);

    letters['O']->Init(1, 13);
    letters['O']->Add_point(0.3565, 1.1195);
    letters['O']->Add_point(0.5678, 1.1195);
    letters['O']->Add_point(0.7791, 1.00835);
    letters['O']->Add_point(0.8848, 0.786);
    letters['O']->Add_point(0.8848, 0.45255);
    letters['O']->Add_point(0.7791, 0.2302);
    letters['O']->Add_point(0.5678, 0.11905);
    letters['O']->Add_point(0.3565, 0.11905);
    letters['O']->Add_point(0.1452, 0.2302);
    letters['O']->Add_point(0.03955, 0.45255);
    letters['O']->Add_point(0.03955, 0.786);
    letters['O']->Add_point(0.1452, 1.00835);
    letters['O']->Add_point(0.3565, 1.1195);

    letters['P']->Init(1, 10);
    letters['P']->Add_point(0.09295, 0.13045);
    letters['P']->Add_point(0.5417, 0.13045);
    letters['P']->Add_point(0.24045, 0.13045);
    letters['P']->Add_point(0.24045, 1.13095);
    letters['P']->Add_point(0.0396, 1.13095);
    letters['P']->Add_point(0.6421, 1.13095);
    letters['P']->Add_point(0.84295, 1.01975);
    letters['P']->Add_point(0.84295, 0.6863);
    letters['P']->Add_point(0.6421, 0.5751);
    letters['P']->Add_point(0.24045, 0.5751);

    letters['Q']->Init(1, 17);
    letters['Q']->Add_point(0.46845, 0.11225);
    letters['Q']->Add_point(0.5971, 0.11225);
    letters['Q']->Add_point(0.79775, 0.23575);
    letters['Q']->Add_point(0.9049, 0.45975);
    letters['Q']->Add_point(0.9049, 0.79575);
    letters['Q']->Add_point(0.79775, 1.01975);
    letters['Q']->Add_point(0.58345, 1.13175);
    letters['Q']->Add_point(0.36915, 1.13175);
    letters['Q']->Add_point(0.15485, 1.01975);
    letters['Q']->Add_point(0.0477, 0.79575);
    letters['Q']->Add_point(0.0477, 0.45975);
    letters['Q']->Add_point(0.15485, 0.23575);
    letters['Q']->Add_point(0.4763, 0.12375);
    letters['Q']->Add_point(0.21115, -0.11195);
    letters['Q']->Add_point(0.55425, -0.0671);
    letters['Q']->Add_point(0.76865, -0.11195);
    letters['Q']->Add_point(0.8973, -0.0671);

    letters['R']->Init(1, 13);
    letters['R']->Add_point(0.4205, 0.11905);
    letters['R']->Add_point(0.03965, 0.11905);
    letters['R']->Add_point(0.23005, 0.11905);
    letters['R']->Add_point(0.23005, 1.1195);
    letters['R']->Add_point(0.03965, 1.1195);
    letters['R']->Add_point(0.6109, 1.1195);
    letters['R']->Add_point(0.80135, 1.00835);
    letters['R']->Add_point(0.80135, 0.67485);
    letters['R']->Add_point(0.6109, 0.5637);
    letters['R']->Add_point(0.23005, 0.5637);
    letters['R']->Add_point(0.5157, 0.5637);
    letters['R']->Add_point(0.843, 0.11905);
    letters['R']->Add_point(1.0037, 0.11905);

    letters['S']->Init(1, 15);
    letters['S']->Add_point(0.81785, 1.1195);
    letters['S']->Add_point(0.81785, 0.8972);
    letters['S']->Add_point(0.7107, 1.00835);
    letters['S']->Add_point(0.60355, 1.1195);
    letters['S']->Add_point(0.38925, 1.1195);
    letters['S']->Add_point(0.17495, 1.00835);
    letters['S']->Add_point(0.17495, 0.786);
    letters['S']->Add_point(0.2821, 0.67485);
    letters['S']->Add_point(0.60355, 0.67485);
    letters['S']->Add_point(0.81785, 0.5637);
    letters['S']->Add_point(0.81785, 0.2302);
    letters['S']->Add_point(0.7107, 0.11905);
    letters['S']->Add_point(0.38925, 0.11905);
    letters['S']->Add_point(0.17495, 0.34135);
    letters['S']->Add_point(0.17495, 0.11905);

    letters['T']->Init(1, 10);
    letters['T']->Add_point(0.0397, 0.79825);
    letters['T']->Add_point(0.0397, 1.13175);
    letters['T']->Add_point(0.4468, 1.13175);
    letters['T']->Add_point(0.4468, 0.1313);
    letters['T']->Add_point(0.1561, 0.1313);
    letters['T']->Add_point(0.7381, 0.1313);
    letters['T']->Add_point(0.4468, 0.1313);
    letters['T']->Add_point(0.4468, 1.13175);
    letters['T']->Add_point(0.8545, 1.13175);
    letters['T']->Add_point(0.8545, 0.79825);

    letters['U']->Init(1, 10);
    letters['U']->Add_point(0.0397, 1.13175);
    letters['U']->Add_point(0.2918, 1.13175);
    letters['U']->Add_point(0.14055, 1.13175);
    letters['U']->Add_point(0.14055, 0.24245);
    letters['U']->Add_point(0.2918, 0.1313);
    letters['U']->Add_point(0.64475, 0.1313);
    letters['U']->Add_point(0.79605, 0.29805);
    letters['U']->Add_point(0.79605, 1.13175);
    letters['U']->Add_point(0.64475, 1.13175);
    letters['U']->Add_point(0.8969, 1.13175);

    letters['V']->Init(1, 7);
    letters['V']->Add_point(-0.00315, 1.10295);
    letters['V']->Add_point(0.32565, 1.10295);
    letters['V']->Add_point(0.10645, 1.10295);
    letters['V']->Add_point(0.49005, 0.1025);
    letters['V']->Add_point(0.8736, 1.10295);
    letters['V']->Add_point(0.7092, 1.10295);
    letters['V']->Add_point(0.9832, 1.10295);

    letters['W']->Init(1, 11);
    letters['W']->Add_point(-0.0031, 1.13175);
    letters['W']->Add_point(0.31475, 1.13175);
    letters['W']->Add_point(0.10285, 1.13175);
    letters['W']->Add_point(0.10285, 0.83155);
    letters['W']->Add_point(0.2088, 0.1519);
    letters['W']->Add_point(0.4737, 0.914);
    letters['W']->Add_point(0.73855, 0.1519);
    letters['W']->Add_point(0.84455, 0.87445);
    letters['W']->Add_point(0.84455, 1.13175);
    letters['W']->Add_point(0.57965, 1.13175);
    letters['W']->Add_point(0.9505, 1.13175);

    letters['X']->Init(1, 16);
    letters['X']->Add_point(0.0398, 1.11955);
    letters['X']->Add_point(0.3423, 1.11955);
    letters['X']->Add_point(0.19105, 1.11955);
    letters['X']->Add_point(0.4663, 0.62535);
    letters['X']->Add_point(0.14065, 0.1454);
    letters['X']->Add_point(0.0398, 0.1454);
    letters['X']->Add_point(0.39275, 0.1454);
    letters['X']->Add_point(0.14065, 0.1454);
    letters['X']->Add_point(0.7961, 1.11955);
    letters['X']->Add_point(0.5944, 1.11955);
    letters['X']->Add_point(0.89695, 1.11955);
    letters['X']->Add_point(0.7961, 1.11955);
    letters['X']->Add_point(0.4663, 0.62535);
    letters['X']->Add_point(0.7457, 0.1454);
    letters['X']->Add_point(0.5944, 0.1454);
    letters['X']->Add_point(0.89695, 0.1454);

    letters['Y']->Init(1, 12);
    letters['Y']->Add_point(-0.00305, 1.13175);
    letters['Y']->Add_point(0.25425, 1.13175);
    letters['Y']->Add_point(0.1256, 1.13175);
    letters['Y']->Add_point(0.4258, 0.61715);
    letters['Y']->Add_point(0.4258, 0.1454);
    letters['Y']->Add_point(0.1685, 0.1454);
    letters['Y']->Add_point(0.6402, 0.1454);
    letters['Y']->Add_point(0.4258, 0.1454);
    letters['Y']->Add_point(0.4258, 0.61715);
    letters['Y']->Add_point(0.76885, 1.13175);
    letters['Y']->Add_point(0.59735, 1.13175);
    letters['Y']->Add_point(0.85465, 1.13175);

    letters['Z']->Init(1, 6);
    letters['Z']->Add_point(0.1685, 0.84165);
    letters['Z']->Add_point(0.1685, 1.11955);
    letters['Z']->Add_point(0.75105, 1.11955);
    letters['Z']->Add_point(0.1685, 0.1191);
    letters['Z']->Add_point(0.75105, 0.1191);
    letters['Z']->Add_point(0.75105, 0.4526);
}

static void
InitNumbers(Pad_LineFontChar *letters[256])
{
    letters['0']->Init(1, 13);
    letters['0']->Add_point(0.38245, 1.16155);
    letters['0']->Add_point(0.5432, 1.16155);
    letters['0']->Add_point(0.70395, 1.05435);
    letters['0']->Add_point(0.75755, 0.8936);
    letters['0']->Add_point(0.75755, 0.30415);
    letters['0']->Add_point(0.70395, 0.19695);
    letters['0']->Add_point(0.5432, 0.0898);
    letters['0']->Add_point(0.38245, 0.0898);
    letters['0']->Add_point(0.22165, 0.19695);
    letters['0']->Add_point(0.1681, 0.30415);
    letters['0']->Add_point(0.1681, 0.9472);
    letters['0']->Add_point(0.22165, 1.05435);
    letters['0']->Add_point(0.38245, 1.16155);

    letters['1']->Init(1, 5);
    letters['1']->Add_point(0.21075, 1.07125);
    letters['1']->Add_point(0.4983, 1.16155);
    letters['1']->Add_point(0.4983, 0.0898);
    letters['1']->Add_point(0.1768, 0.0898);
    letters['1']->Add_point(0.81985, 0.0898);

    letters['2']->Init(1, 9);
    letters['2']->Add_point(0.17525, 1.0008);
    letters['2']->Add_point(0.22725, 1.06885);
    letters['2']->Add_point(0.3253, 1.16155);
    letters['2']->Add_point(0.52535, 1.16155);
    letters['2']->Add_point(0.7254, 1.0008);
    letters['2']->Add_point(0.7254, 0.73285);
    letters['2']->Add_point(0.12525, 0.0898);
    letters['2']->Add_point(0.7254, 0.0898);
    letters['2']->Add_point(0.7254, 0.19695);

    letters['3']->Init(1, 13);
    letters['3']->Add_point(0.1709, 1.07125);
    letters['3']->Add_point(0.3285, 1.16155);
    letters['3']->Add_point(0.615, 1.16155);
    letters['3']->Add_point(0.7869, 1.0008);
    letters['3']->Add_point(0.7869, 0.84);
    letters['3']->Add_point(0.615, 0.67925);
    letters['3']->Add_point(0.3858, 0.67925);
    letters['3']->Add_point(0.615, 0.67925);
    letters['3']->Add_point(0.7869, 0.4649);
    letters['3']->Add_point(0.7869, 0.19695);
    letters['3']->Add_point(0.6723, 0.0898);
    letters['3']->Add_point(0.2712, 0.0898);
    letters['3']->Add_point(0.125, 0.17065);

    letters['4']->Init(1, 7);
    letters['4']->Add_point(0.4065, 0.0849);
    letters['4']->Add_point(0.72815, 0.0849);
    letters['4']->Add_point(0.6075, 0.0849);
    letters['4']->Add_point(0.6075, 1.15705);
    letters['4']->Add_point(0.5271, 1.15705);
    letters['4']->Add_point(0.12505, 0.3851);
    letters['4']->Add_point(0.76835, 0.3851);

    letters['5']->Init(1, 11);
    letters['5']->Add_point(0.75395, 1.15705);
    letters['5']->Add_point(0.21085, 1.15705);
    letters['5']->Add_point(0.21085, 0.6424);
    letters['5']->Add_point(0.3395, 0.72815);
    letters['5']->Add_point(0.6397, 0.72815);
    letters['5']->Add_point(0.80755, 0.5167);
    letters['5']->Add_point(0.80755, 0.2499);
    letters['5']->Add_point(0.7004, 0.1432);
    letters['5']->Add_point(0.5932, 0.0898);
    letters['5']->Add_point(0.2966, 0.0898);
    letters['5']->Add_point(0.16795, 0.17065);

    letters['6']->Init(1, 13);
    letters['6']->Add_point(0.8179, 1.16155);
    letters['6']->Add_point(0.56495, 1.16155);
    letters['6']->Add_point(0.31205, 1.0008);
    letters['6']->Add_point(0.21085, 0.78645);
    letters['6']->Add_point(0.21085, 0.30415);
    letters['6']->Add_point(0.31205, 0.1434);
    letters['6']->Add_point(0.4253, 0.0849);
    letters['6']->Add_point(0.59685, 0.0849);
    letters['6']->Add_point(0.7684, 0.25645);
    letters['6']->Add_point(0.7684, 0.51375);
    letters['6']->Add_point(0.59685, 0.6853);
    letters['6']->Add_point(0.4253, 0.6853);
    letters['6']->Add_point(0.21085, 0.4649);

    letters['7']->Init(1, 4);
    letters['7']->Add_point(0.1251, 1.02835);
    letters['7']->Add_point(0.1251, 1.15735);
    letters['7']->Add_point(0.76815, 1.15705);
    letters['7']->Add_point(0.39305, 0.0853);

    letters['8']->Init(1, 21);
    letters['8']->Add_point(0.31815, 1.15705);
    letters['8']->Add_point(0.61835, 1.15705);
    letters['8']->Add_point(0.7184, 1.04985);
    letters['8']->Add_point(0.76845, 0.9427);
    letters['8']->Add_point(0.76845, 0.7819);
    letters['8']->Add_point(0.66835, 0.67475);
    letters['8']->Add_point(0.2681, 0.67475);
    letters['8']->Add_point(0.16805, 0.514);
    letters['8']->Add_point(0.16805, 0.29965);
    letters['8']->Add_point(0.21805, 0.19245);
    letters['8']->Add_point(0.36815, 0.0853);
    letters['8']->Add_point(0.61835, 0.0853);
    letters['8']->Add_point(0.7184, 0.19245);
    letters['8']->Add_point(0.76845, 0.29965);
    letters['8']->Add_point(0.76845, 0.514);
    letters['8']->Add_point(0.66835, 0.67475);
    letters['8']->Add_point(0.2681, 0.67475);
    letters['8']->Add_point(0.16805, 0.7819);
    letters['8']->Add_point(0.16805, 0.9427);
    letters['8']->Add_point(0.21805, 1.04985);
    letters['8']->Add_point(0.31815, 1.15705);

    letters['9']->Init(1, 12);
    letters['9']->Add_point(0.72555, 0.67925);
    letters['9']->Add_point(0.5735, 0.5185);
    letters['9']->Add_point(0.3708, 0.5185);
    letters['9']->Add_point(0.16805, 0.67925);
    letters['9']->Add_point(0.16805, 0.9472);
    letters['9']->Add_point(0.21875, 1.05435);
    letters['9']->Add_point(0.3708, 1.16155);
    letters['9']->Add_point(0.5735, 1.16155);
    letters['9']->Add_point(0.72555, 0.9472);
    letters['9']->Add_point(0.72555, 0.35775);
    letters['9']->Add_point(0.47215, 0.0898);
    letters['9']->Add_point(0.16805, 0.0898);
}

static void
InitSymbols(Pad_LineFontChar *letters[256])
{
    letters[' ']->Init(0);

    letters['.']->Init(1, 5);
    letters['.']->Add_point(0.33975, 0.1209);
    letters['.']->Add_point(0.33975, 0.31195);
    letters['.']->Add_point(0.57315, 0.31195);
    letters['.']->Add_point(0.57315, 0.1209);
    letters['.']->Add_point(0.33975, 0.1209);

    letters[',']->Init(1, 4);
    letters[',']->Add_point(0.3397, -0.1169);
    letters[',']->Add_point(0.42435, 0.31195);
    letters[',']->Add_point(0.58435, 0.31195);
    letters[',']->Add_point(0.3397, -0.1169);

    letters['?']->Init(2, 16, 5);
    letters['?']->Add_point(0.26465, 1.0752);
    letters['?']->Add_point(0.31465, 0.9752);
    letters['?']->Add_point(0.26465, 0.9252);
    letters['?']->Add_point(0.21465, 1.0252);
    letters['?']->Add_point(0.31465, 1.1752);
    letters['?']->Add_point(0.51465, 1.22515);
    letters['?']->Add_point(0.71465, 1.1252);
    letters['?']->Add_point(0.71465, 0.8752);
    letters['?']->Add_point(0.71465, 1.1252);
    letters['?']->Add_point(0.61465, 1.1752);
    letters['?']->Add_point(0.66465, 0.9752);
    letters['?']->Add_point(0.61465, 0.7252);
    letters['?']->Add_point(0.71465, 0.8752);
    letters['?']->Add_point(0.61465, 0.7252);
    letters['?']->Add_point(0.51465, 0.5752);
    letters['?']->Add_point(0.46465, 0.3752);
    letters['?']->Add_point(0.41465, 0.2252);
    letters['?']->Add_point(0.41465, 0.1252);
    letters['?']->Add_point(0.51465, 0.1252);
    letters['?']->Add_point(0.51465, 0.2252);
    letters['?']->Add_point(0.41465, 0.2252);

    letters['\\']->Init(1, 2);
    letters['\\']->Add_point(0.168, 1.2983);
    letters['\\']->Add_point(0.7684, 0.01175);

    letters['/']->Init(1, 2);
    letters['/']->Add_point(0.85, 1.31175);
    letters['/']->Add_point(0.2496, 0.0252);

    letters['^']->Init(1, 3);
    letters['^']->Add_point(0.21125, 1.07125);
    letters['^']->Add_point(0.46855, 1.2428);
    letters['^']->Add_point(0.72585, 1.07125);

    letters['*']->Init(1, 12);
    letters['*']->Add_point(0.1684, 0.88885);
    letters['*']->Add_point(0.32905, 0.81395);
    letters['*']->Add_point(0.4899, 0.7817);
    letters['*']->Add_point(0.28615, 0.47085);
    letters['*']->Add_point(0.4899, 0.7817);
    letters['*']->Add_point(0.4899, 1.1032);
    letters['*']->Add_point(0.4899, 0.7817);
    letters['*']->Add_point(0.6721, 0.81395);
    letters['*']->Add_point(0.81145, 0.88885);
    letters['*']->Add_point(0.6721, 0.81395);
    letters['*']->Add_point(0.4899, 0.7817);
    letters['*']->Add_point(0.62925, 0.51375);

    letters['&']->Init(1, 24);
    letters['&']->Add_point(0.6401, 1.02835);
    letters['&']->Add_point(0.59725, 1.07125);
    letters['&']->Add_point(0.47945, 1.10925);
    letters['&']->Add_point(0.3399, 1.07125);
    letters['&']->Add_point(0.29705, 0.9855);
    letters['&']->Add_point(0.29705, 0.81395);
    letters['&']->Add_point(0.3828, 0.6424);
    letters['&']->Add_point(0.3399, 0.55665);
    letters['&']->Add_point(0.16835, 0.42795);
    letters['&']->Add_point(0.1579, 0.26565);
    letters['&']->Add_point(0.2651, 0.0849);
    letters['&']->Add_point(0.47945, 0.0849);
    letters['&']->Add_point(0.62905, 0.12285);
    letters['&']->Add_point(0.7148, 0.38015);
    letters['&']->Add_point(0.7148, 0.50885);
    letters['&']->Add_point(0.8006, 0.50885);
    letters['&']->Add_point(0.7148, 0.38015);
    letters['&']->Add_point(0.62905, 0.2515);
    letters['&']->Add_point(0.37225, 0.68745);
    letters['&']->Add_point(0.5888, 0.2944);
    letters['&']->Add_point(0.5888, 0.3066);
    letters['&']->Add_point(0.66555, 0.21245);
    letters['&']->Add_point(0.74735, 0.0849);
    letters['&']->Add_point(0.85455, 0.0849);

    letters['%']->Init(3, 9, 2, 9);
    letters['%']->Add_point(0.6615, 0.0849);
    letters['%']->Add_point(0.7687, 0.21355);
    letters['%']->Add_point(0.7687, 0.3422);
    letters['%']->Add_point(0.6615, 0.47085);
    letters['%']->Add_point(0.44705, 0.47085);
    letters['%']->Add_point(0.33985, 0.3422);
    letters['%']->Add_point(0.33985, 0.21355);
    letters['%']->Add_point(0.44705, 0.0849);
    letters['%']->Add_point(0.6615, 0.0849);
    letters['%']->Add_point(0.1683, 0.51375);
    letters['%']->Add_point(0.8116, 0.77105);
    letters['%']->Add_point(0.48995, 0.77105);
    letters['%']->Add_point(0.59715, 0.8997);
    letters['%']->Add_point(0.59715, 1.02835);
    letters['%']->Add_point(0.48995, 1.15705);
    letters['%']->Add_point(0.27555, 1.15705);
    letters['%']->Add_point(0.1683, 1.02835);
    letters['%']->Add_point(0.1683, 0.8997);
    letters['%']->Add_point(0.27555, 0.77105);
    letters['%']->Add_point(0.48995, 0.77105);

    letters['$']->Init(1, 21);
    letters['$']->Add_point(0.7258, 1.15705);
    letters['$']->Add_point(0.7258, 0.9855);
    letters['$']->Add_point(0.59715, 1.15705);
    letters['$']->Add_point(0.4685, 1.1999);
    letters['$']->Add_point(0.4685, 1.37145);
    letters['$']->Add_point(0.4685, 1.1999);
    letters['$']->Add_point(0.29695, 1.15705);
    letters['$']->Add_point(0.1683, 0.9855);
    letters['$']->Add_point(0.1683, 0.85685);
    letters['$']->Add_point(0.29695, 0.72815);
    letters['$']->Add_point(0.59715, 0.6424);
    letters['$']->Add_point(0.7687, 0.5055);
    letters['$']->Add_point(0.7687, 0.3323);
    letters['$']->Add_point(0.59715, 0.21355);
    letters['$']->Add_point(0.44705, 0.17065);
    letters['$']->Add_point(0.44705, -0.12955);
    letters['$']->Add_point(0.44705, 0.17065);
    letters['$']->Add_point(0.2908, 0.21685);
    letters['$']->Add_point(0.1683, 0.25645);
    letters['$']->Add_point(0.1683, 0.3851);
    letters['$']->Add_point(0.1683, 0.17065);

    letters['#']->Init(1, 19);
    letters['#']->Add_point(0.3827, 1.2428);
    letters['#']->Add_point(0.3827, 0.81395);
    letters['#']->Add_point(0.16825, 0.81395);
    letters['#']->Add_point(0.34255, 0.81395);
    letters['#']->Add_point(0.34255, 0.42795);
    letters['#']->Add_point(0.1254, 0.42795);
    letters['#']->Add_point(0.34255, 0.42795);
    letters['#']->Add_point(0.34255, -0.0009);
    letters['#']->Add_point(0.34255, 0.42795);
    letters['#']->Add_point(0.55425, 0.42795);
    letters['#']->Add_point(0.55425, -0.0009);
    letters['#']->Add_point(0.55425, 0.42795);
    letters['#']->Add_point(0.81155, 0.42795);
    letters['#']->Add_point(0.5971, 0.42795);
    letters['#']->Add_point(0.5971, 0.81395);
    letters['#']->Add_point(0.3827, 0.81395);
    letters['#']->Add_point(0.85445, 0.81395);
    letters['#']->Add_point(0.5971, 0.81395);
    letters['#']->Add_point(0.5971, 1.2428);

    letters['@']->Init(1, 17);
    letters['@']->Add_point(0.7492, 0.05295);
    letters['@']->Add_point(0.64205, -0.0006);
    letters['@']->Add_point(0.42215, 0.01905);
    letters['@']->Add_point(0.2937, 0.15305);
    letters['@']->Add_point(0.21335, 0.248);
    letters['@']->Add_point(0.20435, 0.9572);
    letters['@']->Add_point(0.2602, 1.1582);
    letters['@']->Add_point(0.3741, 1.2319);
    letters['@']->Add_point(0.58845, 1.2319);
    letters['@']->Add_point(0.74045, 1.0298);
    letters['@']->Add_point(0.73485, 0.4211);
    letters['@']->Add_point(0.77955, 0.4155);
    letters['@']->Add_point(0.49475, 0.40435);
    letters['@']->Add_point(0.38865, 0.51045);
    letters['@']->Add_point(0.38865, 0.7115);
    letters['@']->Add_point(0.53485, 0.85675);
    letters['@']->Add_point(0.7492, 0.85675);

    letters['_']->Init(1, 2);
    letters['_']->Add_point(0, 0);
    letters['_']->Add_point(1.05, 0);

    letters['-']->Init(1, 2);
    letters['-']->Add_point(0.13045, 0.4693);
    letters['-']->Add_point(0.8807, 0.4693);

    letters['+']->Init(1, 5);
    letters['+']->Add_point(0.09295, 0.61005);
    letters['+']->Add_point(0.8994, 0.61005);
    letters['+']->Add_point(0.46805, 0.61005);
    letters['+']->Add_point(0.46805, 0.12775);
    letters['+']->Add_point(0.46805, 1.03875);

    letters['=']->Init(2, 2, 2);
    letters['=']->Add_point(0.08945, 0.8021);
    letters['=']->Add_point(1.00045, 0.8021);
    letters['=']->Add_point(0.08945, 0.47605);
    letters['=']->Add_point(1.00045, 0.47605);

    letters['<']->Init(1, 3);
    letters['<']->Add_point(0.8395, 0.9981);
    letters['<']->Add_point(0.03945, 0.62475);
    letters['<']->Add_point(0.8395, 0.2514);

    letters['>']->Init(1, 3);
    letters['>']->Add_point(0.1253, 0.9981);
    letters['>']->Add_point(0.9253, 0.62475);
    letters['>']->Add_point(0.1253, 0.2514);

    letters['\"']->Init(2, 7, 7);
    letters['\"']->Add_point(0.2817, 0.86945);
    letters['\"']->Add_point(0.21095, 0.86945);
    letters['\"']->Add_point(0.21095, 1.0839);
    letters['\"']->Add_point(0.35245, 1.0839);
    letters['\"']->Add_point(0.35245, 0.86945);
    letters['\"']->Add_point(0.2817, 0.86945);
    letters['\"']->Add_point(0.2817, 0.6979);
    letters['\"']->Add_point(0.6119, 0.86945);
    letters['\"']->Add_point(0.54115, 0.86945);
    letters['\"']->Add_point(0.54115, 1.0839);
    letters['\"']->Add_point(0.6827, 1.0839);
    letters['\"']->Add_point(0.6827, 0.86945);
    letters['\"']->Add_point(0.6119, 0.86945);
    letters['\"']->Add_point(0.6119, 0.6979);

    letters['\'']->Init(1, 7);
    letters['\'']->Add_point(0.4404, 0.86945);
    letters['\'']->Add_point(0.36965, 0.86945);
    letters['\'']->Add_point(0.36965, 1.0839);
    letters['\'']->Add_point(0.51115, 1.0839);
    letters['\'']->Add_point(0.51115, 0.86945);
    letters['\'']->Add_point(0.4404, 0.86945);
    letters['\'']->Add_point(0.4404, 0.6979);

    letters['`']->Init(1, 4);
    letters['`']->Add_point(0.1253, 1.15705);
    letters['`']->Add_point(0.21105, 1.2857);
    letters['`']->Add_point(0.57295, 1.02505);
    letters['`']->Add_point(0.1253, 1.15705);

    letters['!']->Init(2, 4, 5);
    letters['!']->Add_point(0.46085, 0.40575);
    letters['!']->Add_point(0.38265, 1.15595);
    letters['!']->Add_point(0.53905, 1.15595);
    letters['!']->Add_point(0.46085, 0.40575);
    letters['!']->Add_point(0.38265, 0.0842);
    letters['!']->Add_point(0.38265, 0.1914);
    letters['!']->Add_point(0.53905, 0.1914);
    letters['!']->Add_point(0.53905, 0.0842);
    letters['!']->Add_point(0.38265, 0.0842);

    letters['|']->Init(1, 2);
    letters['|']->Add_point(0.46815, 1.46985);
    letters['|']->Add_point(0.46815, -0.3748);

    letters['~']->Init(1, 6);
    letters['~']->Add_point(0.1682, 1.07125);
    letters['~']->Add_point(0.25395, 1.1999);
    letters['~']->Add_point(0.4255, 1.1999);
    letters['~']->Add_point(0.55415, 1.07125);
    letters['~']->Add_point(0.6828, 1.07125);
    letters['~']->Add_point(0.7686, 1.1999);

    letters[';']->Init(2, 5, 4);
    letters[';']->Add_point(0.38245, 0.61215);
    letters[';']->Add_point(0.38245, 0.8266);
    letters[';']->Add_point(0.58535, 0.8266);
    letters[';']->Add_point(0.58535, 0.61215);
    letters[';']->Add_point(0.38245, 0.61215);
    letters[';']->Add_point(0.62715, 0.26905);
    letters[';']->Add_point(0.4138, 0.26905);
    letters[';']->Add_point(0.3396, -0.2027);
    letters[';']->Add_point(0.62715, 0.26905);

    letters[':']->Init(2, 5, 5);
    letters[':']->Add_point(0.33955, 0.61325);
    letters[':']->Add_point(0.33955, 0.8266);
    letters[':']->Add_point(0.59685, 0.8266);
    letters[':']->Add_point(0.59685, 0.61325);
    letters[':']->Add_point(0.33955, 0.61325);
    letters[':']->Add_point(0.33955, 0.02655);
    letters[':']->Add_point(0.33955, 0.2399);
    letters[':']->Add_point(0.59685, 0.2399);
    letters[':']->Add_point(0.59685, 0.02655);
    letters[':']->Add_point(0.33955, 0.02655);

    letters['(']->Init(1, 6);
    letters['(']->Add_point(0.6508, 1.0922);
    letters['(']->Add_point(0.5436, 0.985);
    letters['(']->Add_point(0.38285, 0.6635);
    letters['(']->Add_point(0.38285, 0.1812);
    letters['(']->Add_point(0.5436, -0.1939);
    letters['(']->Add_point(0.6508, -0.3011);
  
    letters[')']->Init(1, 6);
    letters[')']->Add_point(0.2971, 1.0922);
    letters[')']->Add_point(0.4043, 0.985);
    letters[')']->Add_point(0.56505, 0.6635);
    letters[')']->Add_point(0.56505, 0.1812);
    letters[')']->Add_point(0.4043, -0.1939);
    letters[')']->Add_point(0.2971, -0.3011);
  
    letters['[']->Init(1, 4);
    letters['[']->Add_point(0.5528, 1.0839);
    letters['[']->Add_point(0.33945, 1.0839);
    letters['[']->Add_point(0.33945, -0.24555);
    letters['[']->Add_point(0.5528, -0.24555);

    letters[']']->Init(1, 4);
    letters[']']->Add_point(0.34055, 1.0839);
    letters[']']->Add_point(0.5539, 1.0839);
    letters[']']->Add_point(0.5539, -0.24555);
    letters[']']->Add_point(0.34055, -0.24555);

    letters['{']->Init(1, 9);
    letters['{']->Add_point(0.6825, 1.0839);
    letters['{']->Add_point(0.5538, 1.041);
    letters['{']->Add_point(0.46805, 0.91235);
    letters['{']->Add_point(0.46805, 0.5264);
    letters['{']->Add_point(0.2965, 0.4192);
    letters['{']->Add_point(0.46805, 0.31195);
    letters['{']->Add_point(0.46805, -0.074);
    letters['{']->Add_point(0.5538, -0.2027);
    letters['{']->Add_point(0.6825, -0.24555);

    letters['}']->Init(1, 9);
    letters['}']->Add_point(0.21075, 1.0839);
    letters['}']->Add_point(0.3394, 1.041);
    letters['}']->Add_point(0.4252, 0.91235);
    letters['}']->Add_point(0.4252, 0.5264);
    letters['}']->Add_point(0.59675, 0.4192);
    letters['}']->Add_point(0.4252, 0.31195);
    letters['}']->Add_point(0.4252, -0.074);
    letters['}']->Add_point(0.3394, -0.2027);
    letters['}']->Add_point(0.21075, -0.24555);
}

void
Pad_LineFont::InitLineFonts(void)
{
    int i;
    
    for (i=0; i<256; i++) {
	letters[i] = new Pad_LineFontChar;
    }

                         // Need to break up initialization because otherwise
                         // CC compiler on SGIs complains that it is unable
                         // to optimize this routine because it is too big.
    InitLowercaseAlpha(letters);
    InitUppercaseAlpha(letters);
    InitNumbers(letters);
    InitSymbols(letters);
}

