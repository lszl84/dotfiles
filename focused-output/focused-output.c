/*
 * focused-output — print the wl_output name that the currently activated
 * toplevel lives on. Uses zwlr_foreign_toplevel_management_v1.
 *
 * Exit 0 with the output name on stdout, or exit 1 with no output if no
 * toplevel is activated (empty desktop, lock screen, etc).
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wayland-client.h>
#include "wlr-foreign-toplevel-management-unstable-v1.h"

struct out {
    struct wl_output *wl_output;
    char *name;
    struct out *next;
};

struct top {
    struct zwlr_foreign_toplevel_handle_v1 *handle;
    int activated;
    struct wl_output *output;
    struct top *next;
};

static struct out *outs;
static struct top *tops;
static struct zwlr_foreign_toplevel_manager_v1 *mgr;

static char *output_name_for(struct wl_output *o) {
    for (struct out *out = outs; out; out = out->next)
        if (out->wl_output == o) return out->name;
    return NULL;
}

static void out_name(void *d, struct wl_output *wo, const char *n) {
    struct out *o = d; free(o->name); o->name = strdup(n);
}
static void out_noop_g(void *d, struct wl_output *o, int32_t x, int32_t y,
    int32_t pw, int32_t ph, int32_t sb, const char *mk, const char *md, int32_t t) {}
static void out_noop_m(void *d, struct wl_output *o, uint32_t f, int32_t w, int32_t h, int32_t r) {}
static void out_noop_done(void *d, struct wl_output *o) {}
static void out_noop_s(void *d, struct wl_output *o, int32_t f) {}
static void out_noop_d(void *d, struct wl_output *o, const char *s) {}
static const struct wl_output_listener out_listener = {
    .geometry=out_noop_g, .mode=out_noop_m, .done=out_noop_done,
    .scale=out_noop_s, .name=out_name, .description=out_noop_d,
};

static void top_title(void *d, struct zwlr_foreign_toplevel_handle_v1 *h, const char *s) {}
static void top_app_id(void *d, struct zwlr_foreign_toplevel_handle_v1 *h, const char *s) {}
static void top_output_enter(void *d, struct zwlr_foreign_toplevel_handle_v1 *h,
                             struct wl_output *o) {
    struct top *t = d;
    if (!t->output) t->output = o;
}
static void top_output_leave(void *d, struct zwlr_foreign_toplevel_handle_v1 *h,
                             struct wl_output *o) {
    struct top *t = d;
    if (t->output == o) t->output = NULL;
}
static void top_state(void *d, struct zwlr_foreign_toplevel_handle_v1 *h,
                      struct wl_array *states) {
    struct top *t = d;
    t->activated = 0;
    uint32_t *s;
    wl_array_for_each(s, states) {
        if (*s == ZWLR_FOREIGN_TOPLEVEL_HANDLE_V1_STATE_ACTIVATED)
            t->activated = 1;
    }
}
static void top_done(void *d, struct zwlr_foreign_toplevel_handle_v1 *h) {}
static void top_closed(void *d, struct zwlr_foreign_toplevel_handle_v1 *h) {
    struct top *t = d; t->activated = 0; t->output = NULL;
}
static void top_parent(void *d, struct zwlr_foreign_toplevel_handle_v1 *h,
                       struct zwlr_foreign_toplevel_handle_v1 *p) {}
static const struct zwlr_foreign_toplevel_handle_v1_listener top_listener = {
    .title = top_title, .app_id = top_app_id,
    .output_enter = top_output_enter, .output_leave = top_output_leave,
    .state = top_state, .done = top_done, .closed = top_closed,
    .parent = top_parent,
};

static void mgr_toplevel(void *d, struct zwlr_foreign_toplevel_manager_v1 *m,
                         struct zwlr_foreign_toplevel_handle_v1 *handle) {
    struct top *t = calloc(1, sizeof(*t));
    t->handle = handle;
    t->next = tops;
    tops = t;
    zwlr_foreign_toplevel_handle_v1_add_listener(handle, &top_listener, t);
}
static void mgr_finished(void *d, struct zwlr_foreign_toplevel_manager_v1 *m) {}
static const struct zwlr_foreign_toplevel_manager_v1_listener mgr_listener = {
    .toplevel = mgr_toplevel, .finished = mgr_finished,
};

static void reg_global(void *d, struct wl_registry *r, uint32_t id,
                       const char *iface, uint32_t v) {
    if (!strcmp(iface, zwlr_foreign_toplevel_manager_v1_interface.name)) {
        mgr = wl_registry_bind(r, id, &zwlr_foreign_toplevel_manager_v1_interface, 3);
        zwlr_foreign_toplevel_manager_v1_add_listener(mgr, &mgr_listener, NULL);
    } else if (!strcmp(iface, wl_output_interface.name)) {
        struct out *o = calloc(1, sizeof(*o));
        o->wl_output = wl_registry_bind(r, id, &wl_output_interface, 4);
        wl_output_add_listener(o->wl_output, &out_listener, o);
        o->next = outs;
        outs = o;
    }
}
static void reg_rm(void *d, struct wl_registry *r, uint32_t id) {}
static const struct wl_registry_listener reg_listener = {
    .global = reg_global, .global_remove = reg_rm,
};

int main(void) {
    struct wl_display *dpy = wl_display_connect(NULL);
    if (!dpy) { fprintf(stderr, "focused-output: cannot connect\n"); return 1; }

    struct wl_registry *reg = wl_display_get_registry(dpy);
    wl_registry_add_listener(reg, &reg_listener, NULL);
    wl_display_roundtrip(dpy);  /* globals */
    if (!mgr) {
        fprintf(stderr, "focused-output: zwlr_foreign_toplevel_manager_v1 unavailable\n");
        return 1;
    }
    wl_display_roundtrip(dpy);  /* output names + toplevel announcements */
    wl_display_roundtrip(dpy);  /* per-toplevel state + output_enter + done */

    int rc = 1;
    for (struct top *t = tops; t; t = t->next) {
        if (t->activated && t->output) {
            char *name = output_name_for(t->output);
            if (name) { printf("%s\n", name); rc = 0; break; }
        }
    }
    return rc;
}
