// Reads the active workspace from labwc via ext-workspace-v1 Wayland protocol
// and prints the workspace name to stdout for use with waybar custom module.
//
// Build:
//   wayland-scanner client-header \
//     /usr/share/wayland-protocols/staging/ext-workspace/ext-workspace-v1.xml \
//     ext-workspace-v1-client-protocol.h
//   wayland-scanner private-code \
//     /usr/share/wayland-protocols/staging/ext-workspace/ext-workspace-v1.xml \
//     ext-workspace-v1-protocol.c
//   gcc -o workspace-watch workspace-watch.c ext-workspace-v1-protocol.c \
//     $(pkg-config --cflags --libs wayland-client) -Wall

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wayland-client.h>
#include "ext-workspace-v1-client-protocol.h"

static struct ext_workspace_manager_v1 *workspace_manager = NULL;

struct workspace_info {
    struct ext_workspace_handle_v1 *handle;
    char *name;
    int active;
    struct workspace_info *next;
};

static struct workspace_info *workspaces = NULL;

static void workspace_name(void *data, struct ext_workspace_handle_v1 *handle,
                           const char *name) {
    struct workspace_info *ws = data;
    free(ws->name);
    ws->name = strdup(name);
}

static void workspace_coordinates(void *data,
                                  struct ext_workspace_handle_v1 *handle,
                                  struct wl_array *coords) {
    (void)data; (void)handle; (void)coords;
}

static void workspace_state(void *data, struct ext_workspace_handle_v1 *handle,
                            struct wl_array *state) {
    struct workspace_info *ws = data;
    ws->active = 0;
    uint32_t *s;
    wl_array_for_each(s, state) {
        if (*s == EXT_WORKSPACE_HANDLE_V1_STATE_ACTIVE)
            ws->active = 1;
    }
}

static void workspace_capabilities(void *data,
                                   struct ext_workspace_handle_v1 *handle,
                                   struct wl_array *caps) {
    (void)data; (void)handle; (void)caps;
}

static void workspace_removed(void *data,
                              struct ext_workspace_handle_v1 *handle) {
    (void)data; (void)handle;
}

static void workspace_id(void *data, struct ext_workspace_handle_v1 *handle,
                         const char *id) {
    (void)data; (void)handle; (void)id;
}

static const struct ext_workspace_handle_v1_listener workspace_listener = {
    .name = workspace_name,
    .coordinates = workspace_coordinates,
    .state = workspace_state,
    .capabilities = workspace_capabilities,
    .removed = workspace_removed,
    .id = workspace_id,
};

static void workspace_group_workspace(
    void *data, struct ext_workspace_group_handle_v1 *group,
    struct ext_workspace_handle_v1 *workspace) {
    (void)data; (void)group;
    struct workspace_info *ws = calloc(1, sizeof(*ws));
    ws->handle = workspace;
    ws->next = workspaces;
    workspaces = ws;
    ext_workspace_handle_v1_add_listener(workspace, &workspace_listener, ws);
}

static void workspace_group_capabilities(
    void *data, struct ext_workspace_group_handle_v1 *group,
    struct wl_array *caps) {
    (void)data; (void)group; (void)caps;
}

static void workspace_group_removed(
    void *data, struct ext_workspace_group_handle_v1 *group) {
    (void)data; (void)group;
}

static const struct ext_workspace_group_handle_v1_listener group_listener = {
    .workspace = workspace_group_workspace,
    .capabilities = workspace_group_capabilities,
    .removed = workspace_group_removed,
};

static void manager_workspace_group(
    void *data, struct ext_workspace_manager_v1 *manager,
    struct ext_workspace_group_handle_v1 *group) {
    (void)data; (void)manager;
    ext_workspace_group_handle_v1_add_listener(group, &group_listener, NULL);
}

static void manager_workspace(void *data,
                              struct ext_workspace_manager_v1 *manager,
                              struct ext_workspace_handle_v1 *workspace) {
    (void)data; (void)manager;
    struct workspace_info *ws = calloc(1, sizeof(*ws));
    ws->handle = workspace;
    ws->next = workspaces;
    workspaces = ws;
    ext_workspace_handle_v1_add_listener(workspace, &workspace_listener, ws);
}

static void manager_done(void *data,
                         struct ext_workspace_manager_v1 *manager) {
    (void)data; (void)manager;
    for (struct workspace_info *ws = workspaces; ws; ws = ws->next) {
        if (ws->active && ws->name) {
            printf("%s\n", ws->name);
            fflush(stdout);
            break;
        }
    }
}

static void manager_finished(void *data,
                             struct ext_workspace_manager_v1 *manager) {
    (void)data; (void)manager;
    exit(0);
}

static const struct ext_workspace_manager_v1_listener manager_listener = {
    .workspace_group = manager_workspace_group,
    .workspace = manager_workspace,
    .done = manager_done,
    .finished = manager_finished,
};

static void registry_global(void *data, struct wl_registry *registry,
                            uint32_t name, const char *interface,
                            uint32_t version) {
    (void)data;
    if (strcmp(interface, ext_workspace_manager_v1_interface.name) == 0) {
        workspace_manager = wl_registry_bind(registry, name,
            &ext_workspace_manager_v1_interface, 1);
        ext_workspace_manager_v1_add_listener(workspace_manager,
            &manager_listener, NULL);
    }
}

static void registry_global_remove(void *data, struct wl_registry *registry,
                                   uint32_t name) {
    (void)data; (void)registry; (void)name;
}

static const struct wl_registry_listener registry_listener = {
    .global = registry_global,
    .global_remove = registry_global_remove,
};

int main(void) {
    struct wl_display *display = wl_display_connect(NULL);
    if (!display) {
        fprintf(stderr, "Failed to connect to Wayland display\n");
        return 1;
    }
    struct wl_registry *registry = wl_display_get_registry(display);
    wl_registry_add_listener(registry, &registry_listener, NULL);
    wl_display_roundtrip(display);
    if (!workspace_manager) {
        fprintf(stderr, "Compositor does not support ext-workspace-v1\n");
        return 1;
    }
    while (wl_display_dispatch(display) != -1) {}
    return 0;
}
