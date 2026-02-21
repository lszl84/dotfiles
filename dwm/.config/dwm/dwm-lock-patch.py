#!/usr/bin/env python3
"""Apply built-in screen locker patch to dwm — PAM auth with OverrideRedirect lock window."""
import sys

with open('dwm.c') as f:
    src = f.read()

# --- Lock functions (inserted before spawn()) ---

lock_functions = r'''
static FILE *locklog;

static void
locklog_open(void)
{
	char path[256];
	snprintf(path, sizeof(path), "%s/.cache/dwm-lock.log",
		getenv("HOME") ? getenv("HOME") : "/tmp");
	locklog = fopen(path, "a");
	if (locklog) setlinebuf(locklog);
}

static void
locklog_msg(const char *fmt, ...)
{
	struct timespec ts;
	va_list ap;
	if (!locklog) return;
	clock_gettime(CLOCK_MONOTONIC, &ts);
	fprintf(locklog, "[%ld.%03ld] ", (long)ts.tv_sec, ts.tv_nsec / 1000000);
	va_start(ap, fmt);
	vfprintf(locklog, fmt, ap);
	va_end(ap);
	fputc('\n', locklog);
}

static const char *lockpass;

static int
lock_conv(int num_msg, const struct pam_message **msg,
	struct pam_response **resp, void *data)
{
	int i;
	*resp = calloc(num_msg, sizeof(struct pam_response));
	if (!*resp)
		return PAM_BUF_ERR;
	for (i = 0; i < num_msg; i++) {
		if (msg[i]->msg_style == PAM_PROMPT_ECHO_OFF ||
		    msg[i]->msg_style == PAM_PROMPT_ECHO_ON)
			(*resp)[i].resp = strdup(lockpass);
	}
	return PAM_SUCCESS;
}

static int
lock_auth(const char *passwd)
{
	struct passwd *pw = getpwuid(getuid());
	struct pam_conv conv = { lock_conv, NULL };
	pam_handle_t *pamh;
	int ret;

	if (!pw)
		return 0;
	lockpass = passwd;
	if (pam_start("dwm", pw->pw_name, &conv, &pamh) != PAM_SUCCESS)
		return 0;
	ret = pam_authenticate(pamh, 0);
	if (ret == PAM_SUCCESS)
		ret = pam_acct_mgmt(pamh, 0);
	pam_end(pamh, ret);
	lockpass = NULL;
	return ret == PAM_SUCCESS;
}

void
lock(const Arg *arg)
{
	XSetWindowAttributes wa;
	Window lockwin;
	XEvent ev;
	char passwd[256];
	int len = 0;
	int pipefd[2];
	int grabbed = 0;
	int i;
	char result;
	pid_t pid;
	KeySym ksym;
	char buf[32];
	int n;

	XColor xc, exact;
	Colormap cmap = DefaultColormap(dpy, screen);
	unsigned long col_black, col_input, col_fail;

	if (locked)
		return;

	locklog_open();
	locklog_msg("lock() entered");

	col_black = BlackPixel(dpy, screen);
	col_input = col_black;
	col_fail = col_black;
	if (XAllocNamedColor(dpy, cmap, "#0a1a3a", &xc, &exact))
		col_input = xc.pixel;
	if (XAllocNamedColor(dpy, cmap, "#3a2a00", &xc, &exact))
		col_fail = xc.pixel;

	wa.override_redirect = 1;
	wa.background_pixel = col_black;
	lockwin = XCreateWindow(dpy, root, 0, 0, sw, sh, 0,
		DefaultDepth(dpy, screen), InputOutput,
		DefaultVisual(dpy, screen),
		CWOverrideRedirect | CWBackPixel, &wa);
	XMapRaised(dpy, lockwin);
	locklog_msg("window created and mapped (0x%lx)", lockwin);

	for (i = 0; i < 10 && !grabbed; i++) {
		if (XGrabKeyboard(dpy, root, True, GrabModeAsync,
		    GrabModeAsync, CurrentTime) == GrabSuccess &&
		    XGrabPointer(dpy, root, False,
		    ButtonPressMask | ButtonReleaseMask | PointerMotionMask,
		    GrabModeAsync, GrabModeAsync, None, None,
		    CurrentTime) == GrabSuccess)
			grabbed = 1;
		else
			usleep(100000);
	}
	if (!grabbed) {
		locklog_msg("grab FAILED after %d attempts", i);
		XDestroyWindow(dpy, lockwin);
		if (locklog) fclose(locklog);
		locklog = NULL;
		return;
	}
	locklog_msg("grabs acquired after %d attempt(s)", i);

	XSync(dpy, False);
	locklog_msg("XSync complete, entering event loop");
	locked = 1;
	memset(passwd, 0, sizeof(passwd));

	while (locked) {
		XNextEvent(dpy, &ev);
		if (ev.type == KeyPress) {
			n = XLookupString(&ev.xkey, buf, sizeof(buf), &ksym, NULL);
			if (ksym == XK_Return) {
				passwd[len] = '\0';
				XSetWindowBackground(dpy, lockwin, col_black);
				XClearWindow(dpy, lockwin);
				XFlush(dpy);
				if (pipe(pipefd) == -1) {
					len = 0;
					memset(passwd, 0, sizeof(passwd));
					continue;
				}
				pid = fork();
				if (pid == 0) {
					close(pipefd[0]);
					close(ConnectionNumber(dpy));
					result = lock_auth(passwd) ? 0 : 1;
					write(pipefd[1], &result, 1);
					close(pipefd[1]);
					_exit(0);
				}
				close(pipefd[1]);
				result = 1;
				if (read(pipefd[0], &result, 1) != 1)
					result = 1;
				close(pipefd[0]);
				if (result == 0)
					locked = 0;
				else {
					explicit_bzero(passwd, sizeof(passwd));
					len = 0;
					XSetWindowBackground(dpy, lockwin, col_fail);
					XClearWindow(dpy, lockwin);
				}
			} else if (ksym == XK_Escape ||
			    (ksym == XK_u && (ev.xkey.state & ControlMask))) {
				explicit_bzero(passwd, sizeof(passwd));
				len = 0;
				XSetWindowBackground(dpy, lockwin, col_black);
				XClearWindow(dpy, lockwin);
			} else if (ksym == XK_BackSpace) {
				if (len > 0) {
					passwd[--len] = '\0';
					if (len == 0) {
						XSetWindowBackground(dpy, lockwin, col_black);
						XClearWindow(dpy, lockwin);
					}
				}
			} else if (n > 0 && !iscntrl(buf[0]) &&
			    len + n < (int)sizeof(passwd) - 1) {
				memcpy(passwd + len, buf, n);
				len += n;
				if (len == n) {
					XSetWindowBackground(dpy, lockwin, col_input);
					XClearWindow(dpy, lockwin);
				}
			}
		} else {
			locklog_msg("non-key event type=%d, raising lock window", ev.type);
			XRaiseWindow(dpy, lockwin);
		}
	}

	locklog_msg("unlock — destroying window, ungrabbing");
	explicit_bzero(passwd, sizeof(passwd));
	XDeleteProperty(dpy, root, lockatom);
	XDestroyWindow(dpy, lockwin);
	XUngrabKeyboard(dpy, CurrentTime);
	XUngrabPointer(dpy, CurrentTime);
	XForceScreenSaver(dpy, ScreenSaverReset);

	locklog_msg("unlock complete");
	if (locklog) fclose(locklog);
	locklog = NULL;

	focus(NULL);
	arrange(selmon);
	drawbars();
}

'''

replacements = [
    # Add includes for PAM, ctype, pwd
    ('#include <X11/Xft/Xft.h>',
     '#include <X11/Xft/Xft.h>\n#include <ctype.h>\n#include <pwd.h>\n#include <time.h>\n#include <security/pam_appl.h>'),

    # Add forward declaration
    ('static void zoom(const Arg *arg);',
     'static void lock(const Arg *arg);\nstatic void zoom(const Arg *arg);'),

    # Add global variables
    ('static Window root, wmcheckwin;',
     'static Window root, wmcheckwin;\nstatic Atom lockatom;\nstatic int locked = 0;'),

    # Initialize atom in setup()
    ('\tnetatom[NetClientList] = XInternAtom(dpy, "_NET_CLIENT_LIST", False);',
     '\tnetatom[NetClientList] = XInternAtom(dpy, "_NET_CLIENT_LIST", False);\n'
     '\tlockatom = XInternAtom(dpy, "_DWM_LOCK", False);'),

    # Add lock trigger in propertynotify() before existing WM_NAME check
    ('\tif ((ev->window == root) && (ev->atom == XA_WM_NAME))\n\t\tupdatestatus();',
     '\tif (ev->window == root && ev->atom == lockatom && !locked\n'
     '\t    && ev->state == PropertyNewValue) {\n'
     '\t\tlock(NULL); /* _DWM_LOCK property set on root */\n'
     '\t\treturn;\n'
     '\t}\n'
     '\tif ((ev->window == root) && (ev->atom == XA_WM_NAME))\n\t\tupdatestatus();'),

    # Insert lock functions before spawn()
    ('void\nspawn(const Arg *arg)',
     lock_functions + 'void\nspawn(const Arg *arg)'),
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

# Patch config.mk to add -lpam
with open('config.mk') as f:
    mk = f.read()

old_libs = 'LIBS = -L${X11LIB} -lX11 ${XINERAMALIBS} ${FREETYPELIBS}'
new_libs = 'LIBS = -L${X11LIB} -lX11 ${XINERAMALIBS} ${FREETYPELIBS} -lpam'
if old_libs not in mk:
    print("ERROR: LIBS pattern not found in config.mk", file=sys.stderr)
    sys.exit(1)
mk = mk.replace(old_libs, new_libs, 1)

with open('config.mk', 'w') as f:
    f.write(mk)

print("Lock patch applied successfully.")
