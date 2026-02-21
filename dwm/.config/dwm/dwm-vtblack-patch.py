#!/usr/bin/env python3
"""Apply VT blacker patch to dwm — blacks screen on VT switch to prevent flash."""

import sys

with open("dwm.c") as f:
    src = f.read()

# Add includes after X11/Xutil.h
src = src.replace(
    "#include <X11/Xutil.h>",
    "#include <X11/Xutil.h>\n#include <sys/ioctl.h>\n#include <fcntl.h>\n#include <linux/vt.h>",
)

# Add global variables after 'static int locked = 0;'
src = src.replace(
    "static int locked = 0;",
    "static int locked = 0;\nstatic Window blacker = None;\nstatic int current_vt = -1;\nstatic int home_vt = -1;",
)

# Add function declarations after 'static void focusin(XEvent *e);'
src = src.replace(
    "static void focusin(XEvent *e);",
    "static void focusin(XEvent *e);\nstatic void focusout(XEvent *e);\nstatic int get_current_vt(void);\nstatic void create_blacker(void);\nstatic void remove_blacker(void);",
)

# Add FocusOut to handler array
src = src.replace(
    "\t[FocusIn] = focusin,", "\t[FocusIn] = focusin,\n\t[FocusOut] = focusout,"
)

# Replace focusin function (with functions inserted before it)
old_focusin = """/* there are some broken focus acquiring clients needing extra handling */
void
focusin(XEvent *e)
{
\tXFocusChangeEvent *ev = &e->xfocus;

\tif (selmon->sel && ev->window != selmon->sel->win)
\t\tsetfocus(selmon->sel);
}"""

new_focusin = """static int
get_current_vt(void)
{
\tint fd;
\tstruct vt_stat vts;
\tFILE *f;
\tstatic int cached_vt = -1;
\tchar vt_str[32] = {0};

\t/* Try /sys first - faster than ioctl */
\tf = fopen("/sys/class/tty/tty0/active", "r");
\tif (f) {
\t\tif (fgets(vt_str, sizeof(vt_str), f)) {
\t\t\t/* Format: "tty1 tty2 ... ttyN" - last is active */
\t\t\tchar *last = strrchr(vt_str, ' ');
\t\t\tint vt = last ? atoi(last + 4) : atoi(vt_str + 3);
\t\t\tfclose(f);
\t\t\tif (vt > 0 && vt != cached_vt) {
\t\t\t\tcached_vt = vt;
\t\t\t\treturn vt;
\t\t\t}
\t\t\treturn cached_vt;
\t\t}
\t\tfclose(f);
\t}

\t/* Fallback to ioctl */
\tfd = open("/dev/tty", O_RDONLY | O_NOCTTY);
\tif (fd < 0)
\t\treturn cached_vt;

\tif (ioctl(fd, VT_GETSTATE, &vts) < 0) {
\t\tclose(fd);
\t\treturn cached_vt;
\t}

\tclose(fd);
\tcached_vt = vts.v_active;
\treturn vts.v_active;
}

static void
create_blacker(void)
{
\tXSetWindowAttributes wa;

\tif (blacker != None)
\t\treturn;

\twa.background_pixel = BlackPixel(dpy, screen);
\twa.override_redirect = True;

\tblacker = XCreateWindow(dpy, root, 0, 0, DisplayWidth(dpy, screen),
\t\tDisplayHeight(dpy, screen), 0, CopyFromParent, InputOutput,
\t\tCopyFromParent, CWBackPixel | CWOverrideRedirect, &wa);

\tXMapWindow(dpy, blacker);
\tXFlush(dpy);
}

static void
remove_blacker(void)
{
\tif (blacker == None)
\t\treturn;

\tXUnmapWindow(dpy, blacker);
\tXDestroyWindow(dpy, blacker);
\tblacker = None;
}

/* there are some broken focus acquiring clients needing extra handling */
void
focusin(XEvent *e)
{
\tXFocusChangeEvent *ev = &e->xfocus;

\t/* Check for VT return */
\tint new_vt = get_current_vt();
\tif (home_vt != -1 && new_vt == home_vt && current_vt != home_vt) {
\t\tremove_blacker();
\t\tcurrent_vt = new_vt;
\t} else if (home_vt == -1 && new_vt > 0) {
\t\thome_vt = new_vt;
\t\tcurrent_vt = new_vt;
\t}

\tif (selmon->sel && ev->window != selmon->sel->win)
\t\tsetfocus(selmon->sel);
}

static void
focusout(XEvent *e)
{
\tXFocusChangeEvent *ev = &e->xfocus;

\t/* Check for VT switch away */
\tint new_vt = get_current_vt();
\tif (home_vt != -1 && new_vt != home_vt && current_vt == home_vt) {
\t\tcreate_blacker();
\t\tcurrent_vt = new_vt;
\t} else if (home_vt == -1 && new_vt > 0) {
\t\thome_vt = new_vt;
\t\tcurrent_vt = new_vt;
\t}
\t(void)ev;
}"""

if old_focusin not in src:
    print("ERROR: focusin pattern not found", file=sys.stderr)
    sys.exit(1)

src = src.replace(old_focusin, new_focusin)

with open("dwm.c", "w") as f:
    f.write(src)

print("VT blacker patch applied successfully.")
