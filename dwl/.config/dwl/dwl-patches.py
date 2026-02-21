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
# 2. DIM-UNFOCUSED — dimmer overlay on inactive windows
# ---------------------------------------------------------------------------

dim_replacements = [
    # Client struct: add dimmer rect after borders
    ('\tstruct wlr_scene_rect *border[4]; /* top, bottom, left, right */',
     '\tstruct wlr_scene_rect *border[4]; /* top, bottom, left, right */\n'
     '\tstruct wlr_scene_rect *dimmer;'),

    # Client struct: add neverdim flag
    ('\tint isfloating, isurgent, isfullscreen;',
     '\tint isfloating, isurgent, isfullscreen, neverdim;'),

    # Rule struct: add neverdim field
    ('\tint isfloating;\n\tint monitor;\n} Rule;',
     '\tint isfloating;\n\tint neverdim;\n\tint monitor;\n} Rule;'),

    # applyrules(): apply neverdim from rule
    ('\t\t\tc->isfloating = r->isfloating;',
     '\t\t\tc->isfloating = r->isfloating;\n'
     '\t\t\tc->neverdim = r->neverdim;'),

    # mapnotify(): add *d variable for focused-window undim
    ('\tClient *p = NULL;\n\tClient *w, *c = wl_container_of(listener, c, map);',
     '\tClient *p = NULL;\n\tClient *w, *d, *c = wl_container_of(listener, c, map);'),

    # mapnotify(): create dimmer rect after border loop
    ('\t\tc->border[i]->node.data = c;\n\t}\n\n\t/* Initialize client geometry',
     '\t\tc->border[i]->node.data = c;\n\t}\n\n'
     '\t/* Dim overlay (initially dimmed for unfocused windows) */\n'
     '\tc->dimmer = wlr_scene_rect_create(c->scene, 0, 0, unfocuseddim);\n'
     '\tc->dimmer->node.data = c;\n'
     '\twlr_scene_node_set_enabled(&c->dimmer->node, 1);\n\n'
     '\t/* Initialize client geometry'),

    # mapnotify(): undim focused client after applyrules
    ('\t} else {\n\t\tapplyrules(c);\n\t}\n\tdrawbars();',
     '\t} else {\n\t\tapplyrules(c);\n'
     '\t\td = focustop(selmon);\n'
     '\t\tif (d)\n'
     '\t\t\twlr_scene_node_set_enabled(&d->dimmer->node, 0);\n'
     '\t}\n\tdrawbars();'),

    # focusclient(): undim newly focused client (wrap single-line if in braces)
    ('\t\tif (!exclusive_focus && !seat->drag)\n'
     '\t\t\tclient_set_border_color(c, (float[])COLOR(colors[SchemeSel][ColBorder]));',

     '\t\tif (!exclusive_focus && !seat->drag) {\n'
     '\t\t\tclient_set_border_color(c, (float[])COLOR(colors[SchemeSel][ColBorder]));\n'
     '\t\t\twlr_scene_node_set_enabled(&c->dimmer->node, 0);\n'
     '\t\t}'),

    # focusclient(): dim old client on defocus
    ('\t\t\tclient_set_border_color(old_c, (float[])COLOR(colors[SchemeNorm][ColBorder]));\n'
     '\t\t\tclient_activate_surface(old, 0);',

     '\t\t\tclient_set_border_color(old_c, (float[])COLOR(colors[SchemeNorm][ColBorder]));\n'
     '\t\t\tif (!old_c->neverdim)\n'
     '\t\t\t\twlr_scene_node_set_enabled(&old_c->dimmer->node, 1);\n'
     '\t\t\tclient_activate_surface(old, 0);'),

    # resize(): update dimmer geometry after border positions
    ('\twlr_scene_node_set_position(&c->border[3]->node, c->geom.width - c->bw, c->bw);',
     '\twlr_scene_node_set_position(&c->border[3]->node, c->geom.width - c->bw, c->bw);\n'
     '\twlr_scene_rect_set_size(c->dimmer, c->geom.width - 2 * c->bw, c->geom.height - 2 * c->bw);\n'
     '\twlr_scene_node_set_position(&c->dimmer->node, c->bw, c->bw);'),
]

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

print("Patches applied: gaps, dim-unfocused, border tweaks.")
