#!/usr/bin/env python3
"""Apply minimal status2d patch to dwm.c — supports ^c#RRGGBB^ and ^d^ in status text."""
import sys

with open('dwm.c') as f:
    src = f.read()

replacements = [
    # Forward declaration: add drawstatusbar before drawbar
    ('static void drawbar(Monitor *m);',
     'static int drawstatusbar(Monitor *m);\nstatic void drawbar(Monitor *m);'),

    # Add drawstatusbar function definition before drawbar
    ('void\ndrawbar(Monitor *m)\n{',
     'int\ndrawstatusbar(Monitor *m)\n{\n'
     '\tchar *p, *seg;\n'
     '\tchar buf[256], tmp[256], hex[8];\n'
     '\tint buflen, tw, x, len, w;\n'
     '\tClr statusscheme[3];\n'
     '\n'
     '\t/* First pass: compute text width excluding color commands */\n'
     '\tbuflen = 0;\n'
     '\tfor (p = stext; *p; p++) {\n'
     '\t\tif (*p == \'^\' && p[1] == \'c\' && p[2] == \'#\' && p[9] == \'^\') {\n'
     '\t\t\tp += 9;\n'
     '\t\t} else if (*p == \'^\' && p[1] == \'d\' && p[2] == \'^\') {\n'
     '\t\t\tp += 2;\n'
     '\t\t} else {\n'
     '\t\t\tbuf[buflen++] = *p;\n'
     '\t\t}\n'
     '\t}\n'
     '\tbuf[buflen] = \'\\0\';\n'
     '\ttw = TEXTW(buf) - lrpad + 2;\n'
     '\n'
     '\t/* Second pass: draw text segments with color changes */\n'
     '\tx = m->ww - tw;\n'
     '\tmemcpy(statusscheme, scheme[SchemeNorm], sizeof(Clr) * 3);\n'
     '\tdrw_setscheme(drw, statusscheme);\n'
     '\tseg = stext;\n'
     '\tfor (p = stext; *p; p++) {\n'
     '\t\tif (*p == \'^\' && p[1] == \'c\' && p[2] == \'#\' && p[9] == \'^\') {\n'
     '\t\t\tlen = p - seg;\n'
     '\t\t\tif (len > 0) {\n'
     '\t\t\t\tmemcpy(tmp, seg, len);\n'
     '\t\t\t\ttmp[len] = \'\\0\';\n'
     '\t\t\t\tw = TEXTW(tmp) - lrpad;\n'
     '\t\t\t\tx = drw_text(drw, x, 0, w, bh, 0, tmp, 0);\n'
     '\t\t\t}\n'
     '\t\t\tmemcpy(hex, p + 2, 7);\n'
     '\t\t\thex[7] = \'\\0\';\n'
     '\t\t\tdrw_clr_create(drw, &statusscheme[ColFg], hex);\n'
     '\t\t\tp += 9;\n'
     '\t\t\tseg = p + 1;\n'
     '\t\t} else if (*p == \'^\' && p[1] == \'d\' && p[2] == \'^\') {\n'
     '\t\t\tlen = p - seg;\n'
     '\t\t\tif (len > 0) {\n'
     '\t\t\t\tmemcpy(tmp, seg, len);\n'
     '\t\t\t\ttmp[len] = \'\\0\';\n'
     '\t\t\t\tw = TEXTW(tmp) - lrpad;\n'
     '\t\t\t\tx = drw_text(drw, x, 0, w, bh, 0, tmp, 0);\n'
     '\t\t\t}\n'
     '\t\t\tmemcpy(statusscheme, scheme[SchemeNorm], sizeof(Clr) * 3);\n'
     '\t\t\tp += 2;\n'
     '\t\t\tseg = p + 1;\n'
     '\t\t}\n'
     '\t}\n'
     '\t/* Draw remaining text */\n'
     '\tlen = p - seg;\n'
     '\tif (len > 0) {\n'
     '\t\tmemcpy(tmp, seg, len);\n'
     '\t\ttmp[len] = \'\\0\';\n'
     '\t\tw = TEXTW(tmp) - lrpad + 2;\n'
     '\t\tdrw_text(drw, x, 0, w, bh, 0, tmp, 0);\n'
     '\t}\n'
     '\n'
     '\tdrw_setscheme(drw, scheme[SchemeNorm]);\n'
     '\treturn tw;\n'
     '}\n'
     '\nvoid\ndrawbar(Monitor *m)\n{'),

    # drawbar(): replace status text drawing with drawstatusbar call
    ('\tif (m == selmon) { /* status is only drawn on selected monitor */\n'
     '\t\tdrw_setscheme(drw, scheme[SchemeNorm]);\n'
     '\t\ttw = TEXTW(stext) - lrpad + 2; /* 2px right padding */\n'
     '\t\tdrw_text(drw, m->ww - tw, 0, tw, bh, 0, stext, 0);\n'
     '\t}',
     '\tif (m == selmon) { /* status is only drawn on selected monitor */\n'
     '\t\ttw = drawstatusbar(m);\n'
     '\t}'),
]

ok = True
for old, new in replacements:
    if old not in src:
        print(f"ERROR: pattern not found: {old[:70]}...", file=sys.stderr)
        ok = False
    src = src.replace(old, new, 1)

if not ok:
    sys.exit(1)

with open('dwm.c', 'w') as f:
    f.write(src)

print("Status2d patch applied successfully.")
