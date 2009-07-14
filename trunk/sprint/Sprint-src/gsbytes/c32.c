#include <stdio.h>

/* Tools for storage/retrieval of arbitrary size bytes from 64 bit words
    (note - this version is not currently (6/30/88) described in the
    gbytes document)

    gbytes(p,u,q,b,s,n)
    gbyte (p,u,q,b)
    sbytes(p,u,q,b,s,n)
    sbyte (p,u,q,b)

             q >= 0     number of bits to be skipped preceeding first byte in p
      0 <    b < sword  byte size
             s >= 0     number of bits to be skipped between bytes
             n >= 0     number of bytes to be packed/unpacked

    gbytes unpacks n b bit bytes from p into u, starting by skipping
         q bits in p, then skipping s bits between bytes.
    gbyte unpacks one such byte.
    sbytes   packs n b bit bytes from u into p, starting by skipping
         q bits in p, then skipping s bits between bytes.
    sbyte  packs one such byte. */
# define SWORD 32                              /* Word size in bits */
# define MASK 0xffffffff                       /* Mask of sword bits */
# define G1BYTE(p,q,b) ((b==32 ? MASK : ~(MASK<<b)) & (p>>(SWORD-(q+b))))
                                               /* Get 1 word contained byte */
# define MASK1(q,b) (b==32 ? MASK : (~(MASK<<b)<<(SWORD-(q+b))))
                                               /* Mask of sword bits */
gsbytes(p,u,q,b,s,n,gsbyte)   /* Common code for gbytes, sbytes */
long p[],u[],*q,*b,*s,*n;
int (*gsbyte)();
{       long jp,jq,ju;
        jp = 0;
        jq = *q;
        for (ju = 0; ju < *n; ++ju) {
                 (*gsbyte)(&p[jp],&u[ju],&jq,b);
                 jq += *b + *s;
                 jp += jq/SWORD;
                 jq %= SWORD;
        }
}
gbytes_(p,u,q,b,s,n)
long p[],u[],*q,*b,*s,*n;
{
        int gbyte_();
        gsbytes(p,u,q,b,s,n,gbyte_);
}
gbyte_(p,u,q,b)
long p[],*u,*q,*b;
{
        long qb,j,lb,jq,jb;

        jq = *q;
        jb = *b;
        if (jq >= SWORD) {
                 j = jq/SWORD; /* number of words offset */
                 jq %= SWORD;  /* odd bits of offset     */
        }
        else {
                 j=0;
        }
        qb = jq + jb;
        if (qb > SWORD) {
                 qb = SWORD - jq;
                 jb -= qb;
                 lb = (G1BYTE(p[j],jq,qb)) << jb;
                 jq = 0;
                 j++;  /* increment to next word */
        }
        else lb = 0;
        *u = lb + (G1BYTE(p[j],jq,jb));
}
sbytes_(p,u,q,b,s,n)
long p[],u[],*q,*b,*s,*n;
{
        int sbyte_();
        gsbytes(p,u,q,b,s,n,sbyte_);
}
sbyte_(p,u,q,b)
long p[],*u,*q,*b;
{
        long qb,j,jq,jb,rb;

        jq = *q;
        jb = *b;
        if (jq >= SWORD) {
                 j = jq / SWORD;    /* number of words offset */
                 jq %= SWORD;       /* odd bit offset         */
        }
        else {
                 j = 0;
        }
        qb = jq + jb;
        if (qb > SWORD) {
                 qb = SWORD - jq;
                 jq = SWORD - jb;
                 jb -= qb;
                 p[j] = ((p[j] >> qb) << qb) + (G1BYTE(*u,jq,qb));
                 jq = 0;
                 j++;  /* point to next word */
        }
        rb = G1BYTE(*u,SWORD-jb,jb);
        p[j] = (p[j] & ~MASK1(jq,jb)) + (rb << SWORD-(jb+jq));
}

