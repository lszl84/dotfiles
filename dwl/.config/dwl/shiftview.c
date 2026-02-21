/* shiftview: cycle to adjacent occupied or empty tag (wraps around) */
static void
shiftview(const Arg *arg)
{
	Arg shifted;
	Client *c;
	unsigned int tagmask = 0;
	unsigned int ntags = LENGTH(tags);

	wl_list_for_each(c, &clients, link)
		tagmask |= c->tags;

	shifted.ui = selmon->tagset[selmon->seltags];
	if (arg->i > 0) /* next tag */
		shifted.ui = (shifted.ui >> (ntags - 1))
		           | (shifted.ui << 1);
	else /* previous tag */
		shifted.ui = (shifted.ui << (ntags - 1))
		           | (shifted.ui >> 1);
	shifted.ui &= TAGMASK;
	view(&shifted);
}
