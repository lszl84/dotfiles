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

new_focusin = """static FILE *vtlog;

static void
vtlog_open(void)
{
\tchar path[256];
\tif (vtlog) return;
\tsnprintf(path, sizeof(path), "%s/.cache/dwm-vt.log",
\t\tgetenv("HOME") ? getenv("HOME") : "/tmp");
\tvtlog = fopen(path, "a");
\tif (vtlog) setlinebuf(vtlog);
}

static void
vtlog_msg(const char *fmt, ...)
{
\tstruct timespec ts;
\tva_list ap;
\tif (!vtlog) return;
\tclock_gettime(CLOCK_MONOTONIC, &ts);
\tfprintf(vtlog, "[%ld.%03ld] ", (long)ts.tv_sec, ts.tv_nsec / 1000000);
\tva_start(ap, fmt);
\tvfprintf(vtlog, fmt, ap);
\tva_end(ap);
\tfputc('\\n', vtlog);
}

static int
get_current_vt(void)
{
\tint fd;
\tstruct vt_stat vts;
\tFILE *f;
\tchar vt_str[32] = {0};

\t/* Try /sys first - faster than ioctl */
\tf = fopen("/sys/class/tty/tty0/active", "r");
\tif (f) {
\t\tif (fgets(vt_str, sizeof(vt_str), f)) {
\t\t\tint vt = atoi(vt_str + 3);
\t\t\tfclose(f);
\t\t\tif (vt > 0)
\t\t\t\treturn vt;
\t\t}
\t\tfclose(f);
\t}

\t/* Fallback to ioctl */
\tfd = open("/dev/tty", O_RDONLY | O_NOCTTY);
\tif (fd < 0)
\t\treturn -1;

\tif (ioctl(fd, VT_GETSTATE, &vts) < 0) {
\t\tclose(fd);
\t\treturn -1;
\t}

\tclose(fd);
\treturn vts.v_active;
}

static void
create_blacker(void)
{
\tXSetWindowAttributes wa;

\tif (blacker != None) {
\t\tvtlog_msg("create_blacker: already exists (0x%lx)", blacker);
\t\treturn;
\t}

\twa.background_pixel = BlackPixel(dpy, screen);
\twa.override_redirect = True;

\tblacker = XCreateWindow(dpy, root, 0, 0, DisplayWidth(dpy, screen),
\t\tDisplayHeight(dpy, screen), 0, CopyFromParent, InputOutput,
\t\tCopyFromParent, CWBackPixel | CWOverrideRedirect, &wa);

\tXMapRaised(dpy, blacker);
\tXSync(dpy, False);
\tvtlog_msg("create_blacker: created and mapped (0x%lx)", blacker);
}

static void
remove_blacker(void)
{
\tif (blacker == None) {
\t\tvtlog_msg("remove_blacker: none to remove");
\t\treturn;
\t}

\tvtlog_msg("remove_blacker: destroying (0x%lx), locked=%d", blacker, locked);

\t/* If locked, don't remove — the lock window handles display */
\tif (locked)
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
\tint new_vt;

\tvtlog_open();
\tnew_vt = get_current_vt();
\tvtlog_msg("FocusIn: mode=%d detail=%d window=0x%lx vt=%d home=%d cur=%d locked=%d blacker=0x%lx",
\t\tev->mode, ev->detail, ev->window, new_vt, home_vt, current_vt, locked, blacker);

\tif (home_vt != -1 && new_vt == home_vt && current_vt != home_vt) {
\t\tvtlog_msg("FocusIn: VT returned to home, removing blacker");
\t\tremove_blacker();
\t\tcurrent_vt = new_vt;
\t} else if (home_vt == -1 && new_vt > 0) {
\t\thome_vt = new_vt;
\t\tcurrent_vt = new_vt;
\t\tvtlog_msg("FocusIn: initialized home_vt=%d", home_vt);
\t} else {
\t\tcurrent_vt = new_vt;
\t}

\tif (selmon->sel && ev->window != selmon->sel->win)
\t\tsetfocus(selmon->sel);
}

static void
focusout(XEvent *e)
{
\tXFocusChangeEvent *ev = &e->xfocus;
\tint new_vt;

\tvtlog_open();
\tnew_vt = get_current_vt();
\tvtlog_msg("FocusOut: mode=%d detail=%d window=0x%lx vt=%d home=%d cur=%d locked=%d",
\t\tev->mode, ev->detail, ev->window, new_vt, home_vt, current_vt, locked);

\tif (home_vt != -1 && new_vt != home_vt && current_vt == home_vt) {
\t\tvtlog_msg("FocusOut: VT switched away (%d -> %d), creating blacker", home_vt, new_vt);
\t\tcreate_blacker();
\t\tcurrent_vt = new_vt;
\t} else if (home_vt != -1 && new_vt == home_vt) {
\t\t/* VT hasn't actually switched yet — this is the suspected race condition.
\t\t * FocusOut fired but /sys still shows home VT. Just log for now. */
\t\tvtlog_msg("FocusOut: vt still shows home (%d) — RACE? (not creating blacker yet)", new_vt);
\t} else if (home_vt == -1 && new_vt > 0) {
\t\thome_vt = new_vt;
\t\tcurrent_vt = new_vt;
\t\tvtlog_msg("FocusOut: initialized home_vt=%d", home_vt);
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
