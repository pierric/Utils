#include <stdlib.h>

void *my_malloc(unsigned bytes);
void my_free(void *ap);

typedef long Align;
typedef union header
{
    struct {
        union header *ptr;
        unsigned size;
    } s;
    Align x;
} Header;

static Header *morecore(unsigned nu);
static Header base;
static Header *freep = NULL;

void *my_malloc(unsigned bytes)
{
    unsigned nunits;
    Header *p, *prevp;

    nunits = (bytes + sizeof(Header) - 1) / sizeof(Header) + 1;
    if( (prevp=freep) == NULL ) {
        base.s.ptr = freep = prevp = &base;
        base.s.size = 0;
    }
    for(p=prevp->s.ptr; ; prevp = p, p = p->s.ptr) {
        if(p->s.size >= nunits) {
            if(p->s.size == nunits)
                prevp->s.ptr = p->s.ptr;
            else {
                p->s.size -= nunits;
                p += p->s.size;
                p->s.size = nunits;
            }
            freep = prevp;
            return (void*)(p+1);
        }
        if(p == freep)
            if( (p = morecore(nunits)) == NULL )
                return NULL;
    }
}

void my_free(void *ap)
{
    Header *bp, *p;
    bp = (Header*)ap - 1;

    // find the position of bp in the chain of free blocks
    for(p = freep; !(bp > p && bp < p->s.ptr); p=p->s.ptr)
        // if p reaches the end of chain,
        // (i.e. p stands for the end, p->s.ptr stands for the start)
        // the freed block is at the start or end
        if(p >= p->s.ptr && (bp > p || bp < p->s.ptr))
            break;

    if( bp+bp->s.size == p->s.ptr) {
        // join to upper block
        bp->s.size += p->s.ptr->s.size;
        bp->s.ptr  =  p->s.ptr->s.ptr;
    }
    else
        // append p after bp
        bp->s.ptr  =  p->s.ptr;
    
    if( p + p->s.size == bp ) {
        // join to lower block
        p->s.size += bp->s.size;
        p->s.ptr  =  bp->s.ptr;
    }
    else
        // append bp after p
        p->s.ptr  = bp;
    freep = p;            
}

#define NALLOC 1024

static Header *morecore(unsigned nu)
{
    char *cp, *sbrk(int);
    Header *up;

    if( nu < NALLOC)
        nu = NALLOC;
    cp = sbrk(nu * sizeof(Header));
    if (cp == (char*)-1)
        return NULL;
    up = (Header*) cp;
    up->s.size = nu;
    my_free((void*)(up+1));
    return freep;
}

int main()
{
    int *p = (int*)my_malloc(20);
    my_free(p);
    return 0;
}
