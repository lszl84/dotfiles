#!/usr/bin/env python3
"""Apply gaps, dim-unfocused, and border patches to post-bar-patch dwl.c.

Targets dwl 0.7 with bar-0.7.patch already applied. Uses exact string
replacement (same approach as dwm-gaps-patch.py). All patterns are verified
against the post-bar-patch source before any writes.
"""
import sys

with open('dwl.c') as f:
    src = f.read()

# ---------------------------------------------------------------------------
# 1. GAPS — adapted from dwl-patches gaps.patch for bar-patched source
# ---------------------------------------------------------------------------

gaps_replacements = [
    # Monitor struct: add gaps field
    ('\tunsigned int seltags;',
     '\tunsigned int seltags;\n\tint gaps;'),

    # Forward declaration: add togglegaps
    ('static void togglefloating(const Arg *arg);',
     'static void togglefloating(const Arg *arg);\nstatic void togglegaps(const Arg *arg);'),

    # createmon(): init gaps
    ('\tm->tagset[0] = m->tagset[1] = 1;',
     '\tm->gaps = gaps;\n\tm->tagset[0] = m->tagset[1] = 1;'),

    # tile(): variable declaration — add h, r, e
    ('\tunsigned int mw, my, ty;',
     '\tunsigned int h, r, e = m->gaps, mw, my, ty;'),

    # tile(): smartgaps check after early return
    ('\tif (n == 0)\n\t\treturn;\n\n\tif (n > m->nmaster)',
     '\tif (n == 0)\n\t\treturn;\n\tif (smartgaps == n)\n\t\te = 0;\n\n\tif (n > m->nmaster)'),

    # tile(): master width with gap factor
    ('\t\tmw = m->nmaster ? (int)roundf(m->w.width * m->mfact) : 0;',
     '\t\tmw = m->nmaster ? (int)roundf((m->w.width + gappx*e) * m->mfact) : 0;'),

    # tile(): init my/ty with gap offset
    ('\ti = my = ty = 0;',
     '\ti = 0;\n\tmy = ty = gappx*e;'),

    # tile(): master window — calculate height, remove borders, add gaps
    ('\t\tif (i < m->nmaster) {\n'
     '\t\t\tresize(c, (struct wlr_box){.x = m->w.x, .y = m->w.y + my, .width = mw,\n'
     '\t\t\t\t.height = (m->w.height - my) / (MIN(n, m->nmaster) - i)}, 0);\n'
     '\t\t\tmy += c->geom.height;',

     '\t\tif (i < m->nmaster) {\n'
     '\t\t\tr = MIN(n, m->nmaster) - i;\n'
     '\t\t\th = (m->w.height - my - gappx*e - gappx*e * (r - 1)) / r;\n'
     '\t\t\tc->bw = 0;\n'
     '\t\t\tresize(c, (struct wlr_box){.x = m->w.x + gappx*e, .y = m->w.y + my,\n'
     '\t\t\t\t.width = mw - 2*gappx*e, .height = h}, 0);\n'
     '\t\t\tmy += c->geom.height + gappx*e;'),

    # tile(): stack window — calculate height, remove borders, add gaps
    ('\t\t} else {\n'
     '\t\t\tresize(c, (struct wlr_box){.x = m->w.x + mw, .y = m->w.y + ty,\n'
     '\t\t\t\t.width = m->w.width - mw, .height = (m->w.height - ty) / (n - i)}, 0);\n'
     '\t\t\tty += c->geom.height;\n'
     '\t\t}',

     '\t\t} else {\n'
     '\t\t\tr = n - i;\n'
     '\t\t\th = (m->w.height - ty - gappx*e - gappx*e * (r - 1)) / r;\n'
     '\t\t\tc->bw = 0;\n'
     '\t\t\tresize(c, (struct wlr_box){.x = m->w.x + mw, .y = m->w.y + ty,\n'
     '\t\t\t\t.width = m->w.width - mw - gappx*e, .height = h}, 0);\n'
     '\t\t\tty += c->geom.height + gappx*e;\n'
     '\t\t}'),

    # monocle(): gaps and no borders
    ('\t\tresize(c, m->w, 0);',
     '\t\tc->bw = 0;\n'
     '\t\tresize(c, (struct wlr_box){.x = m->w.x + gappx, .y = m->w.y + gappx,\n'
     '\t\t\t.width = m->w.width - 2 * gappx, .height = m->w.height - 2 * gappx}, 0);'),
]

# togglegaps() function — insert before toggletag()
togglegaps_fn = (
    'void\n'
    'togglegaps(const Arg *arg)\n'
    '{\n'
    '\tselmon->gaps = !selmon->gaps;\n'
    '\tarrange(selmon);\n'
    '}\n'
    '\n'
    'void\n'
    'toggletag'
)
gaps_replacements.append(
    ('void\ntoggleta' + 'g', togglegaps_fn.replace('toggletag', 'toggleta' + 'g'))
)

# ---------------------------------------------------------------------------
# 2. DIM-UNFOCUSED — opacity-based (no overlay rect, no input blocking)
#    Uses wlr_scene_buffer_set_opacity() on unfocused client surfaces.
# ---------------------------------------------------------------------------

dim_replacements = [
    # Client struct: add opacity fields
    ('\tint isfloating, isurgent, isfullscreen;',
     '\tint isfloating, isurgent, isfullscreen;\n'
     '\tfloat opacity, opacity_focus, opacity_unfocus;'),

    # Rule struct: add opacity_unfocus field
    ('\tint isfloating;\n\tint monitor;\n} Rule;',
     '\tint isfloating;\n\tfloat opacity_unfocus;\n\tint monitor;\n} Rule;'),

    # Forward declarations: add scenebuffersetopacity
    ('static void resize(Client *c, struct wlr_box geo, int interact);',
     'static void resize(Client *c, struct wlr_box geo, int interact);\n'
     'static void scenebuffersetopacity(struct wlr_scene_buffer *buffer, int sx, int sy, void *user_data);'),

    # applyrules(): apply opacity from rule
    ('\t\t\tc->isfloating = r->isfloating;',
     '\t\t\tc->isfloating = r->isfloating;\n'
     '\t\t\tif (r->opacity_unfocus > 0)\n'
     '\t\t\t\tc->opacity_unfocus = r->opacity_unfocus;'),

    # createnotify(): init default opacity (after c->bw = borderpx)
    ('\tc->bw = borderpx;\n\n\tLISTEN',
     '\tc->bw = borderpx;\n'
     '\tc->opacity_unfocus = default_opacity_unfocus;\n'
     '\tc->opacity_focus = default_opacity_focus;\n'
     '\tc->opacity = default_opacity_unfocus;\n\n'
     '\tLISTEN'),

    # focusclient(): set focused client to full opacity (wrap single-line if)
    ('\t\tif (!exclusive_focus && !seat->drag)\n'
     '\t\t\tclient_set_border_color(c, (float[])COLOR(colors[SchemeSel][ColBorder]));',

     '\t\tif (!exclusive_focus && !seat->drag) {\n'
     '\t\t\tclient_set_border_color(c, (float[])COLOR(colors[SchemeSel][ColBorder]));\n'
     '\t\t\tc->opacity = c->opacity_focus;\n'
     '\t\t}'),

    # focusclient(): dim old client on defocus
    ('\t\t\tclient_set_border_color(old_c, (float[])COLOR(colors[SchemeNorm][ColBorder]));\n'
     '\t\t\tclient_activate_surface(old, 0);',

     '\t\t\tclient_set_border_color(old_c, (float[])COLOR(colors[SchemeNorm][ColBorder]));\n'
     '\t\t\told_c->opacity = old_c->opacity_unfocus;\n'
     '\t\t\tclient_activate_surface(old, 0);'),

    # rendermon(): apply opacity to all client buffers before rendering
    ('\twl_list_for_each(c, &clients, link) {\n'
     '\t\tif (c->resize && !c->isfloating',
     '\twl_list_for_each(c, &clients, link) {\n'
     '\t\twlr_scene_node_for_each_buffer(&c->scene_surface->node, scenebuffersetopacity, c);\n'
     '\t\tif (c->resize && !c->isfloating'),
]

# scenebuffersetopacity() function — insert before setcursor()
scenebuf_fn = (
    'void\n'
    'scenebuffersetopacity(struct wlr_scene_buffer *buffer, int sx, int sy, void *data)\n'
    '{\n'
    '\tClient *c = data;\n'
    '\twlr_scene_buffer_set_opacity(buffer, c->isfullscreen ? 1.0f : c->opacity);\n'
    '}\n'
    '\n'
    'void\n'
    'setcursor'
)
dim_replacements.append(
    ('void\nsetcursor', scenebuf_fn)
)

# ---------------------------------------------------------------------------
# 3. BORDER TWEAKS — tiled windows lose borders, floating get them back
# ---------------------------------------------------------------------------

border_replacements = [
    # setfloating(): restore border when becoming floating
    ('\tc->isfloating = floating;\n'
     '\t/* If in floating layout',
     '\tc->isfloating = floating;\n'
     '\tif (c->isfloating)\n'
     '\t\tc->bw = borderpx;\n'
     '\t/* If in floating layout'),
]

# ---------------------------------------------------------------------------
# Apply all replacements
# ---------------------------------------------------------------------------

ok = True
for label, replacements in [('gaps', gaps_replacements),
                             ('dim', dim_replacements),
                             ('border', border_replacements)]:
    for old, new in replacements:
        if old not in src:
            print(f"ERROR [{label}]: pattern not found:\n  {old[:120]!r}", file=sys.stderr)
            ok = False
        else:
            src = src.replace(old, new, 1)

if not ok:
    sys.exit(1)

with open('dwl.c', 'w') as f:
    f.write(src)

print("Patches applied: gaps, dim-unfocused (opacity), border tweaks.")
