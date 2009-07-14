/* ishift routine from ncarg */


int ishift (i, nshift)
/* shift the 4 byte integer i by nshift bits  */
/* if nshift is negative, right shift end off zero fill  */
/* if nshift is positive, left shift end around  */
/* the routine behaves properly if magnitude of nshift > 32  */
	int		*i, *nshift;
{
	int		jshift, nbits;
	if (*nshift < 0) {
		nbits = (*nshift < -32 ? 32 : -*nshift);
		jshift = (*i >> nbits) & (017777777777 >> (nbits - 1));
	} else {
		nbits = *nshift % 32;
		jshift = (*i << nbits) | ((*i >> (32 - nbits))
					  & (~(037777777777 << nbits)));
	}
	return (jshift);
}
