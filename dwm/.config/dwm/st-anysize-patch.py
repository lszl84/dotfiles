#!/usr/bin/env python3
"""Anysize patch for st — allows continuous window resizing by centering terminal content."""
import sys
import re

with open('x.c') as f:
    src = f.read()

replacements = [
    # 1. TermWindow struct: add dynamic border fields
    ('int tw, th; /* tty width and height */\n'
     '\tint w, h; /* window width and height */\n'
     '\tint ch; /* char height */\n'
     '\tint cw; /* char width  */\n'
     '\tint mode; /* window state/mode flags */\n'
     '\tint cursor; /* cursor style */\n'
     '} TermWindow;',
     'int tw, th; /* tty width and height */\n'
     '\tint w, h; /* window width and height */\n'
     '\tint hborderpx, vborderpx; /* dynamic centered padding */\n'
     '\tint ch; /* char height */\n'
     '\tint cw; /* char width  */\n'
     '\tint mode; /* window state/mode flags */\n'
     '\tint cursor; /* cursor style */\n'
     '} TermWindow;'),

    # 2. cresize(): compute dynamic borders after cols/rows
    ('\ttresize(col, row);\n'
     '\txresize(col, row);',
     '\twin.hborderpx = (win.w - col * win.cw) / 2;\n'
     '\twin.vborderpx = (win.h - row * win.ch) / 2;\n'
     '\n'
     '\ttresize(col, row);\n'
     '\txresize(col, row);'),

    # 3. xhints(): remove PResizeInc and base/increment sizes
    ('\tsizeh->flags = PSize | PResizeInc | PBaseSize | PMinSize;\n'
     '\tsizeh->height = win.h;\n'
     '\tsizeh->width = win.w;\n'
     '\tsizeh->height_inc = win.ch;\n'
     '\tsizeh->width_inc = win.cw;\n'
     '\tsizeh->base_height = 2 * borderpx;\n'
     '\tsizeh->base_width = 2 * borderpx;',
     '\tsizeh->flags = PSize | PMinSize;\n'
     '\tsizeh->height = win.h;\n'
     '\tsizeh->width = win.w;'),

    # 4. evcol(): mouse x → hborderpx
    ('int x = e->xbutton.x - borderpx;',
     'int x = e->xbutton.x - win.hborderpx;'),

    # 5. evrow(): mouse y → vborderpx
    ('int y = e->xbutton.y - borderpx;',
     'int y = e->xbutton.y - win.vborderpx;'),

    # 6. xdrawglyphfontspecs(): float winx/winy
    ('float winx = borderpx + x * win.cw, winy = borderpx + y * win.ch',
     'float winx = win.hborderpx + x * win.cw, winy = win.vborderpx + y * win.ch'),

    # 7. xdrawglyphfontspecs(): int winx/winy
    ('int winx = borderpx + x * win.cw, winy = borderpx + y * win.ch',
     'int winx = win.hborderpx + x * win.cw, winy = win.vborderpx + y * win.ch'),

    # 8. Border clearing: left border (x2 = hborderpx)
    ('xclear(0, (y == 0)? 0 : winy, borderpx,\n'
     '\t\t\twiny + win.ch +\n'
     '\t\t\t((winy + win.ch >= borderpx + win.th)? win.h : 0));',
     'xclear(0, (y == 0)? 0 : winy, win.hborderpx,\n'
     '\t\t\twiny + win.ch +\n'
     '\t\t\t((winy + win.ch >= win.vborderpx + win.th)? win.h : 0));'),

    # 9. Border clearing: right boundary
    ('if (winx + width >= borderpx + win.tw) {\n'
     '\t\txclear(winx + width, (y == 0)? 0 : winy, win.w,\n'
     '\t\t\t((winy + win.ch >= borderpx + win.th)? win.h : (winy + win.ch)));',
     'if (winx + width >= win.hborderpx + win.tw) {\n'
     '\t\txclear(winx + width, (y == 0)? 0 : winy, win.w,\n'
     '\t\t\t((winy + win.ch >= win.vborderpx + win.th)? win.h : (winy + win.ch)));'),

    # 10. Border clearing: top border (y2 = vborderpx)
    ('xclear(winx, 0, winx + width, borderpx);',
     'xclear(winx, 0, winx + width, win.vborderpx);'),

    # 11. Border clearing: bottom boundary
    ('if (winy + win.ch >= borderpx + win.th)\n'
     '\t\txclear(winx, winy + win.ch, winx + width, win.h);',
     'if (winy + win.ch >= win.vborderpx + win.th)\n'
     '\t\txclear(winx, winy + win.ch, winx + width, win.h);'),

    # 12. Cursor: underline
    ('\t\t\tXftDrawRect(xw.draw, &drawcol,\n'
     '\t\t\t\t\tborderpx + cx * win.cw,\n'
     '\t\t\t\t\tborderpx + (cy + 1) * win.ch - \\\n'
     '\t\t\t\t\t\tcursorthickness,\n'
     '\t\t\t\t\twin.cw, cursorthickness);',
     '\t\t\tXftDrawRect(xw.draw, &drawcol,\n'
     '\t\t\t\t\twin.hborderpx + cx * win.cw,\n'
     '\t\t\t\t\twin.vborderpx + (cy + 1) * win.ch - \\\n'
     '\t\t\t\t\t\tcursorthickness,\n'
     '\t\t\t\t\twin.cw, cursorthickness);'),

    # 13. Cursor: bar
    ('\t\t\tXftDrawRect(xw.draw, &drawcol,\n'
     '\t\t\t\t\tborderpx + cx * win.cw,\n'
     '\t\t\t\t\tborderpx + cy * win.ch,\n'
     '\t\t\t\t\tcursorthickness, win.ch);',
     '\t\t\tXftDrawRect(xw.draw, &drawcol,\n'
     '\t\t\t\t\twin.hborderpx + cx * win.cw,\n'
     '\t\t\t\t\twin.vborderpx + cy * win.ch,\n'
     '\t\t\t\t\tcursorthickness, win.ch);'),

    # 14. Cursor: outline rectangle (4 sides)
    ('\t\tXftDrawRect(xw.draw, &drawcol,\n'
     '\t\t\t\tborderpx + cx * win.cw,\n'
     '\t\t\t\tborderpx + cy * win.ch,\n'
     '\t\t\t\twin.cw - 1, 1);\n'
     '\t\tXftDrawRect(xw.draw, &drawcol,\n'
     '\t\t\t\tborderpx + cx * win.cw,\n'
     '\t\t\t\tborderpx + cy * win.ch,\n'
     '\t\t\t\t1, win.ch - 1);\n'
     '\t\tXftDrawRect(xw.draw, &drawcol,\n'
     '\t\t\t\tborderpx + (cx + 1) * win.cw - 1,\n'
     '\t\t\t\tborderpx + cy * win.ch,\n'
     '\t\t\t\t1, win.ch - 1);\n'
     '\t\tXftDrawRect(xw.draw, &drawcol,\n'
     '\t\t\t\tborderpx + cx * win.cw,\n'
     '\t\t\t\tborderpx + (cy + 1) * win.ch - 1,\n'
     '\t\t\t\twin.cw, 1);',
     '\t\tXftDrawRect(xw.draw, &drawcol,\n'
     '\t\t\t\twin.hborderpx + cx * win.cw,\n'
     '\t\t\t\twin.vborderpx + cy * win.ch,\n'
     '\t\t\t\twin.cw - 1, 1);\n'
     '\t\tXftDrawRect(xw.draw, &drawcol,\n'
     '\t\t\t\twin.hborderpx + cx * win.cw,\n'
     '\t\t\t\twin.vborderpx + cy * win.ch,\n'
     '\t\t\t\t1, win.ch - 1);\n'
     '\t\tXftDrawRect(xw.draw, &drawcol,\n'
     '\t\t\t\twin.hborderpx + (cx + 1) * win.cw - 1,\n'
     '\t\t\t\twin.vborderpx + cy * win.ch,\n'
     '\t\t\t\t1, win.ch - 1);\n'
     '\t\tXftDrawRect(xw.draw, &drawcol,\n'
     '\t\t\t\twin.hborderpx + cx * win.cw,\n'
     '\t\t\t\twin.vborderpx + (cy + 1) * win.ch - 1,\n'
     '\t\t\t\twin.cw, 1);'),

    # 15. xximspot(): IME position
    ('xw.ime.spot.x = borderpx + x * win.cw;\n'
     '\txw.ime.spot.y = borderpx + (y + 1) * win.ch;',
     'xw.ime.spot.x = win.hborderpx + x * win.cw;\n'
     '\txw.ime.spot.y = win.vborderpx + (y + 1) * win.ch;'),
]

ok = True
for old, new in replacements:
    if old not in src:
        print(f"ERROR: pattern not found: {old[:70]}...", file=sys.stderr)
        ok = False
    src = src.replace(old, new, 1)

if not ok:
    sys.exit(1)

with open('x.c', 'w') as f:
    f.write(src)

print("Anysize patch applied successfully.")
