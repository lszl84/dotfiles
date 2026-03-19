#!/usr/bin/env python3
"""
dwm-floating-xproperty-patch.py

Sets _DWM_FLOATING X11 property on floating windows so picom can
apply rounded corners only to floating windows.

Must be applied AFTER all other patches (gaps, status2d, lock, vtblack).
"""
import sys

with open('dwm.c') as f:
    src = f.read()

replacements = [
    # 1. Add dwm_floating atom declaration next to other atoms
    ('static Atom wmatom[WMLast], netatom[NetLast];',
     'static Atom wmatom[WMLast], netatom[NetLast];\nstatic Atom dwm_floating;'),

    # 2. Initialize the atom in setup(), right after NetClientList
    ('\tnetatom[NetClientList] = XInternAtom(dpy, "_NET_CLIENT_LIST", False);',
     '\tnetatom[NetClientList] = XInternAtom(dpy, "_NET_CLIENT_LIST", False);\n\tdwm_floating = XInternAtom(dpy, "_DWM_FLOATING", False);'),

    # 3. In manage(): set _DWM_FLOATING when window is initially floating
    ('\tif (c->isfloating) {\n\t\tc->x = c->mon->wx + (c->mon->ww - WIDTH(c)) / 2;\n\t\tc->y = c->mon->wy + (c->mon->wh - HEIGHT(c)) / 2;\n\t\tXRaiseWindow(dpy, c->win);\n\t}',
     '\tif (c->isfloating) {\n\t\tc->x = c->mon->wx + (c->mon->ww - WIDTH(c)) / 2;\n\t\tc->y = c->mon->wy + (c->mon->wh - HEIGHT(c)) / 2;\n\t\tXRaiseWindow(dpy, c->win);\n\t\tunsigned char val = 1;\n\t\tXChangeProperty(dpy, c->win, dwm_floating, XA_CARDINAL, 8, PropModeReplace, &val, 1);\n\t}'),

    # 4. In togglefloating(): set property when becoming floating
    ('\tselmon->sel->isfloating = !selmon->sel->isfloating || selmon->sel->isfixed;\n\tif (selmon->sel->isfloating) {\n\t\t/* Resize to 80% of screen */\n\t\tint neww = (int)(selmon->ww * 0.8);\n\t\tint newh = (int)(selmon->wh * 0.8);\n\t\t/* Center on screen */\n\t\tselmon->sel->x = selmon->wx + (selmon->ww - neww) / 2;\n\t\tselmon->sel->y = selmon->wy + (selmon->wh - newh) / 2;\n\t\tselmon->sel->bw = borderpx;\n\t\tresize(selmon->sel, selmon->sel->x, selmon->sel->y, neww, newh, 0);\n\t} else {\n\t\t/* Switching back to tiled: remove border */\n\t\tselmon->sel->bw = 0;\n\t\tresize(selmon->sel, selmon->sel->x, selmon->sel->y, selmon->sel->w, selmon->sel->h, 0);\n\t}',
     '\tselmon->sel->isfloating = !selmon->sel->isfloating || selmon->sel->isfixed;\n\tif (selmon->sel->isfloating) {\n\t\t/* Resize to 80% of screen */\n\t\tint neww = (int)(selmon->ww * 0.8);\n\t\tint newh = (int)(selmon->wh * 0.8);\n\t\t/* Center on screen */\n\t\tselmon->sel->x = selmon->wx + (selmon->ww - neww) / 2;\n\t\tselmon->sel->y = selmon->wy + (selmon->wh - newh) / 2;\n\t\tselmon->sel->bw = borderpx;\n\t\tresize(selmon->sel, selmon->sel->x, selmon->sel->y, neww, newh, 0);\n\t\tunsigned char val = 1;\n\t\tXChangeProperty(dpy, selmon->sel->win, dwm_floating, XA_CARDINAL, 8, PropModeReplace, &val, 1);\n\t} else {\n\t\t/* Switching back to tiled: remove border */\n\t\tselmon->sel->bw = 0;\n\t\tresize(selmon->sel, selmon->sel->x, selmon->sel->y, selmon->sel->w, selmon->sel->h, 0);\n\t\tXDeleteProperty(dpy, selmon->sel->win, dwm_floating);\n\t}'),

    # 5. In updatewindowtype(): set property when dialog detected as floating
    ('\tif (wtype == netatom[NetWMWindowTypeDialog])\n\t\tc->isfloating = 1;',
     '\tif (wtype == netatom[NetWMWindowTypeDialog]) {\n\t\tc->isfloating = 1;\n\t\tunsigned char val = 1;\n\t\tXChangeProperty(dpy, c->win, dwm_floating, XA_CARDINAL, 8, PropModeReplace, &val, 1);\n\t}'),

    # 6. In setfullscreen(): entering fullscreen - remove floating property
    ('\t\tc->isfloating = 1;\n\t\tresizeclient(c, c->mon->mx, c->mon->my, c->mon->mw, c->mon->mh);\n\t\tXRaiseWindow(dpy, c->win);',
     '\t\tc->isfloating = 1;\n\t\tXDeleteProperty(dpy, c->win, dwm_floating);\n\t\tresizeclient(c, c->mon->mx, c->mon->my, c->mon->mw, c->mon->mh);\n\t\tXRaiseWindow(dpy, c->win);'),

    # 7. In setfullscreen(): exiting fullscreen - restore property if was floating
    ('\t\tc->isfloating = c->oldstate;\n\t\tc->bw = c->oldbw;',
     '\t\tc->isfloating = c->oldstate;\n\t\tc->bw = c->oldbw;\n\t\tif (c->isfloating) {\n\t\t\tunsigned char val = 1;\n\t\t\tXChangeProperty(dpy, c->win, dwm_floating, XA_CARDINAL, 8, PropModeReplace, &val, 1);\n\t\t} else {\n\t\t\tXDeleteProperty(dpy, c->win, dwm_floating);\n\t\t}'),

    # 8. In applyrules(): set property when rules mark window as floating
    ('\t\t\tc->isfloating = r->isfloating;',
     '\t\t\tc->isfloating = r->isfloating;\n\t\t\tif (c->isfloating) {\n\t\t\t\tunsigned char val = 1;\n\t\t\t\tXChangeProperty(dpy, c->win, dwm_floating, XA_CARDINAL, 8, PropModeReplace, &val, 1);\n\t\t\t}'),
]

ok = True
for old, new in replacements:
    if old not in src:
        print(f"ERROR: pattern not found:\n{old[:120]}...", file=sys.stderr)
        ok = False
    src = src.replace(old, new, 1)

if not ok:
    sys.exit(1)

with open('dwm.c', 'w') as f:
    f.write(src)

print("Floating X property patch applied successfully.")
