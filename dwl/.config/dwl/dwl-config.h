/* See LICENSE file for copyright and license details. */

/* Taken from https://github.com/djpohly/dwl/issues/466 */
#define COLOR(hex)    { ((hex >> 24) & 0xFF) / 255.0f, \
                        ((hex >> 16) & 0xFF) / 255.0f, \
                        ((hex >> 8) & 0xFF) / 255.0f, \
                        (hex & 0xFF) / 255.0f }

/* appearance */
static const int sloppyfocus               = 1;
static const int bypass_surface_visibility  = 0;
static const int smartgaps                  = 0;
static int gaps                             = 1;
static const unsigned int gappx             = 14;   /* gap pixel (7 logical @ 2x HiDPI) */
static const unsigned int borderpx          = 2;    /* border pixel (floating windows only) */
static const int showbar                    = 1;
static const int topbar                     = 1;
static const char *fonts[]                  = { "monospace:size=11" };
static const float rootcolor[]              = COLOR(0x000000ff);
static const float fullscreen_bg[]          = {0.0f, 0.0f, 0.0f, 1.0f};
static const float unfocuseddim[]           = COLOR(0x00000059); /* ~35% dim overlay */

/* bar colors — matching dwm SchemeNorm/SchemeSel */
static uint32_t colors[][3]                 = {
	/*               fg          bg          border    */
	[SchemeNorm] = { 0xbbbbbbff, 0x000000ff, 0x444444ff },
	[SchemeSel]  = { 0xeeeeeeff, 0x333333ff, 0xffffffff },
	[SchemeUrg]  = { 0,          0,          0xff0000ff },
};

/* tagging */
static char *tags[] = { "1", "2", "3", "4" };

/* logging */
static int log_level = WLR_ERROR;

static const Rule rules[] = {
	/* app_id     title  tags  isfloating  neverdim  monitor */
	{ "Gimp",     NULL,  0,    1,          0,        -1 },
	{ "VibeCut",  NULL,  0,    1,          0,        -1 },
};

/* layout(s) */
static const Layout layouts[] = {
	/* symbol  arrange function */
	{ "[]=",   tile },    /* first entry is default */
	{ "><>",   NULL },    /* no layout = floating */
	{ "[M]",   monocle },
};

/* monitors */
static const MonitorRule monrules[] = {
	/* name    mfact  nmaster  scale  layout             transform                    x   y */
	{ "eDP-1", 0.55f, 1,      2,     &layouts[0],       WL_OUTPUT_TRANSFORM_NORMAL, -1, -1 },
	{ NULL,    0.55f, 1,      2,     &layouts[0],       WL_OUTPUT_TRANSFORM_NORMAL, -1, -1 },
};

/* keyboard */
static const struct xkb_rule_names xkb_rules = {
	.options = NULL,
};

static const int repeat_rate = 25;
static const int repeat_delay = 600;

/* trackpad (matching dwm/xorg settings) */
static const int tap_to_click = 0;
static const int tap_and_drag = 1;
static const int drag_lock = 1;
static const int natural_scrolling = 1;
static const int disable_while_typing = 1;
static const int left_handed = 0;
static const int middle_button_emulation = 0;
static const enum libinput_config_scroll_method scroll_method = LIBINPUT_CONFIG_SCROLL_2FG;
static const enum libinput_config_click_method click_method = LIBINPUT_CONFIG_CLICK_METHOD_CLICKFINGER;
static const uint32_t send_events_mode = LIBINPUT_CONFIG_SEND_EVENTS_ENABLED;
static const enum libinput_config_accel_profile accel_profile = LIBINPUT_CONFIG_ACCEL_PROFILE_ADAPTIVE;
static const double accel_speed = 0.0;
static const enum libinput_config_tap_button_map button_map = LIBINPUT_CONFIG_TAP_MAP_LRM;

/* shiftview: cycle to adjacent tag (wraps around) */
#include "shiftview.c"

/* key definitions */
#define MODKEY WLR_MODIFIER_LOGO
#define TAGKEYS(KEY,SKEY,TAG) \
	{ MODKEY,                    KEY,            view,            {.ui = 1 << TAG} }, \
	{ MODKEY|WLR_MODIFIER_CTRL,  KEY,            toggleview,      {.ui = 1 << TAG} }, \
	{ MODKEY|WLR_MODIFIER_SHIFT, SKEY,           tag,             {.ui = 1 << TAG} }, \
	{ MODKEY|WLR_MODIFIER_CTRL|WLR_MODIFIER_SHIFT,SKEY,toggletag, {.ui = 1 << TAG} }

/* helper for spawning shell commands */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

/* commands */
static const char *termcmd[]    = { "footclient", NULL };
static const char *menucmd[]    = { "fuzzel", NULL };
static const char *lockcmd[]    = { "swaylock", "-f", "-c", "000000", NULL };
static const char *bright_up[]  = { "brightnessctl", "set", "+10%", NULL };
static const char *bright_down[]= { "brightnessctl", "set", "10%-", NULL };

/* volume commands (PipeWire / WirePlumber)
 * --limit 1.0 prevents over-amplification; pkill signals dwl-status for instant update */
#define VOL_CMD(action) SHCMD("wpctl " action " && pkill -USR1 dwl-status")

static const Key keys[] = {
	/* modifier                     key                        function        argument */
	{ MODKEY,                       XKB_KEY_Return,            spawn,          {.v = termcmd } },
	{ MODKEY,                       XKB_KEY_space,             spawn,          {.v = menucmd } },
	{ MODKEY,                       XKB_KEY_BackSpace,         killclient,     {0} },
	{ MODKEY,                       XKB_KEY_b,                 togglebar,      {0} },
	{ MODKEY,                       XKB_KEY_j,                 focusstack,     {.i = +1 } },
	{ MODKEY,                       XKB_KEY_k,                 focusstack,     {.i = -1 } },
	{ MODKEY,                       XKB_KEY_i,                 incnmaster,     {.i = +1 } },
	{ MODKEY,                       XKB_KEY_d,                 incnmaster,     {.i = -1 } },
	{ MODKEY,                       XKB_KEY_h,                 setmfact,       {.f = -0.05f} },
	{ MODKEY,                       XKB_KEY_l,                 spawn,          {.v = lockcmd } },
	{ MODKEY|WLR_MODIFIER_SHIFT,    XKB_KEY_Return,            zoom,           {0} },
	{ MODKEY,                       XKB_KEY_Tab,               view,           {0} },
	{ MODKEY,                       XKB_KEY_t,                 setlayout,      {.v = &layouts[0]} },
	{ MODKEY,                       XKB_KEY_f,                 setlayout,      {.v = &layouts[1]} },
	{ MODKEY,                       XKB_KEY_m,                 setlayout,      {.v = &layouts[2]} },
	{ MODKEY|WLR_MODIFIER_SHIFT,    XKB_KEY_space,             togglefloating, {0} },
	{ MODKEY,                       XKB_KEY_0,                 view,           {.ui = ~0u } },
	{ MODKEY|WLR_MODIFIER_SHIFT,    XKB_KEY_parenright,        tag,            {.ui = ~0u } },
	{ MODKEY,                       XKB_KEY_comma,             setmfact,       {.f = -0.05f} },
	{ MODKEY,                       XKB_KEY_period,            setmfact,       {.f = +0.05f} },

	/* alt-tab: cycle window focus */
	{ WLR_MODIFIER_ALT,             XKB_KEY_Tab,               focusstack,     {.i = +1 } },
	{ WLR_MODIFIER_ALT|WLR_MODIFIER_SHIFT, XKB_KEY_ISO_Left_Tab, focusstack, {.i = -1 } },

	/* shiftview: adjacent tags */
	{ MODKEY,                       XKB_KEY_Left,              shiftview,      {.i = -1 } },
	{ MODKEY,                       XKB_KEY_Right,             shiftview,      {.i = +1 } },
	{ WLR_MODIFIER_CTRL|WLR_MODIFIER_ALT, XKB_KEY_Left,       shiftview,      {.i = -1 } },
	{ WLR_MODIFIER_CTRL|WLR_MODIFIER_ALT, XKB_KEY_Right,      shiftview,      {.i = +1 } },

	/* media keys */
	{ 0,                            XKB_KEY_XF86AudioRaiseVolume, spawn,       VOL_CMD("set-volume -l 1.0 @DEFAULT_AUDIO_SINK@ 5%+") },
	{ 0,                            XKB_KEY_XF86AudioLowerVolume, spawn,       VOL_CMD("set-volume @DEFAULT_AUDIO_SINK@ 5%-") },
	{ 0,                            XKB_KEY_XF86AudioMute,        spawn,       VOL_CMD("set-mute @DEFAULT_AUDIO_SINK@ toggle") },
	{ 0,                            XKB_KEY_XF86MonBrightnessUp,  spawn,       {.v = bright_up } },
	{ 0,                            XKB_KEY_XF86MonBrightnessDown,spawn,       {.v = bright_down } },

	TAGKEYS(XKB_KEY_1, XKB_KEY_exclam,     0),
	TAGKEYS(XKB_KEY_2, XKB_KEY_at,         1),
	TAGKEYS(XKB_KEY_3, XKB_KEY_numbersign, 2),
	TAGKEYS(XKB_KEY_4, XKB_KEY_dollar,     3),
	{ MODKEY|WLR_MODIFIER_SHIFT,    XKB_KEY_BackSpace,         quit,           {0} },

	/* Ctrl-Alt-Fx: VT switching (dwl built-in) */
#define CHVT(n) { WLR_MODIFIER_CTRL|WLR_MODIFIER_ALT, XKB_KEY_XF86Switch_VT_##n, chvt, {.ui = (n)} }
	CHVT(1), CHVT(2), CHVT(3), CHVT(4), CHVT(5), CHVT(6),
	CHVT(7), CHVT(8), CHVT(9), CHVT(10), CHVT(11), CHVT(12),
};

/* button definitions (bar patch format) */
static const Button buttons[] = {
	/* click        event mask  button     function        argument */
	{ ClkLtSymbol,  0,          BTN_LEFT,  setlayout,      {.v = &layouts[0]} },
	{ ClkLtSymbol,  0,          BTN_RIGHT, setlayout,      {.v = &layouts[2]} },
	{ ClkTitle,     0,          BTN_MIDDLE,zoom,            {0} },
	{ ClkStatus,    0,          BTN_MIDDLE,spawn,           {.v = termcmd} },
	{ ClkClient,    MODKEY,     BTN_LEFT,  moveresize,      {.ui = CurMove} },
	{ ClkClient,    MODKEY,     BTN_MIDDLE,togglefloating,  {0} },
	{ ClkClient,    MODKEY,     BTN_RIGHT, moveresize,      {.ui = CurResize} },
	{ ClkTagBar,    0,          BTN_LEFT,  view,            {0} },
	{ ClkTagBar,    0,          BTN_RIGHT, toggleview,      {0} },
	{ ClkTagBar,    MODKEY,     BTN_LEFT,  tag,             {0} },
	{ ClkTagBar,    MODKEY,     BTN_RIGHT, toggletag,       {0} },
};
