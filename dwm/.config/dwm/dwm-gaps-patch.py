#!/usr/bin/env python3
"""Apply uselessgap-style patch to dwm.c — adds gappx gaps around all windows."""
import sys

with open('dwm.c') as f:
    src = f.read()

replacements = [
    # tile(): gap between master and stack areas
    ('mw = m->nmaster ? m->ww * m->mfact : 0;',
     'mw = m->nmaster ? (m->ww - gappx) * m->mfact : 0;'),

    # tile(): outer gap when all windows in master (no stack)
    ('\t\tmw = m->ww;',
     '\t\tmw = m->ww - gappx;'),

    # tile(): start vertical offset at gappx (top gap)
    ('for (i = my = ty = 0, c = nexttiled(m->clients)',
     'for (i = 0, my = ty = gappx, c = nexttiled(m->clients)'),

    # tile(): master window height with gap
    ('h = (m->wh - my) / (MIN(n, m->nmaster) - i);',
     'h = (m->wh - my) / (MIN(n, m->nmaster) - i) - gappx;'),

    # tile(): master window position and width with gaps
    ('resize(c, m->wx, m->wy + my, mw - (2*c->bw), h - (2*c->bw), 0);',
     'resize(c, m->wx + gappx, m->wy + my, mw - (2*c->bw) - gappx, h - (2*c->bw), 0);'),

    # tile(): master vertical advance — remove conditional, always add gap
    ('\t\t\tif (my + HEIGHT(c) < m->wh)\n\t\t\t\tmy += HEIGHT(c);',
     '\t\t\tmy += HEIGHT(c) + gappx;'),

    # tile(): stack window height with gap
    ('h = (m->wh - ty) / (n - i);',
     'h = (m->wh - ty) / (n - i) - gappx;'),

    # tile(): stack window position and width with gaps
    ('resize(c, m->wx + mw, m->wy + ty, m->ww - mw - (2*c->bw), h - (2*c->bw), 0);',
     'resize(c, m->wx + mw + gappx, m->wy + ty, m->ww - mw - (2*c->bw) - 2 * gappx, h - (2*c->bw), 0);'),

    # tile(): stack vertical advance — remove conditional, always add gap
    ('\t\t\tif (ty + HEIGHT(c) < m->wh)\n\t\t\t\tty += HEIGHT(c);',
     '\t\t\tty += HEIGHT(c) + gappx;'),

    # monocle(): add gaps around fullscreen window
    ('resize(c, m->wx, m->wy, m->ww - 2 * c->bw, m->wh - 2 * c->bw, 0);',
     'resize(c, m->wx + gappx, m->wy + gappx, m->ww - 2 * c->bw - 2 * gappx, m->wh - 2 * c->bw - 2 * gappx, 0);'),
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

print("Gaps patch applied successfully.")
