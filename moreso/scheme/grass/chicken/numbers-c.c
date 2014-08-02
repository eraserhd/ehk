/* numbers-c.c
 *
 * Copyright 2010-2012 The CHICKEN Team
 *
 * This contains a barely recognizable version of c/bignum.c from Scheme48 1.8:
 * Copyright (c) 1993-2008 Richard Kelsey and Jonathan Rees
 * Copyright 1986,1987,1988,1989,1990,1991 Massachusetts Institute of Technology
 * Copyright 1992,1993,1994,2004 Massachusetts Institute of Technology
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *    1. Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 * 
 *    2. Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials provided
 *       with the distribution.
 * 
 *    3. The name of the author may not be used to endorse or promote
 *       products derived from this software without specific prior
 *       written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include <errno.h>

static void *tags;

#define fix_to_flo(p, n, f)       C_flonum(p, C_unfix(f))
#define flonump(x)                C_mk_bool(!C_immediatep(x) && C_block_header(x) == C_FLONUM_TAG)
#define big_of(v)                 ((bignum_type)C_data_pointer(C_block_item(v, 1)))
#define big_negp(b)               C_mk_bool(BIGNUM_NEGATIVE_P(big_of(b)))
#define big_oddp(b)               C_mk_bool(BIGNUM_REF(big_of(b), 0) & 1)

static C_word
init_tags(___scheme_value tagvec)
{
  tags = CHICKEN_new_gc_root();
  CHICKEN_gc_root_set(tags, tagvec);
  return C_SCHEME_UNDEFINED;
}

/*
 * This is an odd one out. It doesn't accept a continuation.
 * I've put in the (more or less) verbatim code for s48_bignum_to_double.
 */
static C_word
big_to_flo(C_word **p, C_word n, C_word value)
{
  bignum_type bignum = big_of(value);
  double accumulator = 0;
  bignum_digit_type * start = (BIGNUM_START_PTR (bignum));
  bignum_digit_type * scan = (start + (BIGNUM_LENGTH (bignum)));
  while (start < scan)
    accumulator = ((accumulator * BIGNUM_RADIX) + (*--scan));
  return C_flonum(p, (BIGNUM_NEGATIVE_P(bignum) ? -accumulator : accumulator));
}


static C_word
check_number(C_word x)
{
  C_word tagvec;

  if((x & C_FIXNUM_BIT) != 0) return C_fix(FIX);
  else if(C_immediatep(x)) return C_fix(NONE);
  else if(C_header_bits(x) == C_FLONUM_TYPE) return C_fix(FLO);
  else if(C_header_bits(x) == C_STRUCTURE_TYPE) {
    tagvec = CHICKEN_gc_root_ref(tags);
    
    if (C_block_item(x, 0) == C_block_item(tagvec, BIG_TAG))
      return C_fix(BIG);
    else if (C_block_item(x, 0) == C_block_item(tagvec, COMP_TAG))
      return C_fix(COMP);
    else if (C_block_item(x, 0) == C_block_item(tagvec, RAT_TAG))
      return C_fix(RAT);
    else
      return C_fix(NONE);
  } else
      return C_fix(NONE);
}

static void
C_wrap_bignum(C_word c, C_word closure, C_word bigvec)
{
  C_word ab[3], *a = ab;
  bignum_type src = (bignum_type)C_block_item(C_block_item(closure, 2), 0);
  bignum_digit_type *scan = BIGNUM_TO_POINTER(src);
  bignum_digit_type *end = BIGNUM_START_PTR(src) + BIGNUM_LENGTH(src);
  bignum_digit_type *tgt = C_data_pointer(bigvec);
  C_word tagvec = CHICKEN_gc_root_ref(tags);
  C_word result = C_structure(&a, 2, C_block_item(tagvec, BIG_TAG), bigvec);
  while (scan < end)
    (*tgt++) = (*scan++);
  BIGNUM_DEALLOCATE(src);
  C_kontinue(C_block_item(closure, 1), result);
}

static void
C_return_bignum(C_word k, bignum_type b)
{
  C_word result;
  
  BIGNUM_ASSERT(b != BIGNUM_OUT_OF_BAND);

  switch(BIGNUM_LENGTH(b)) {
  case 0:
    BIGNUM_DEALLOCATE(b);
    C_kontinue(k, C_fix(0));
  /* This code "knows" that bignums have 2 "reserved" bits, like fixnums */
  case 1:
    result = C_fix(BIGNUM_NEGATIVE_P(b) ? -BIGNUM_REF(b, 0) : BIGNUM_REF(b, 0));
    BIGNUM_DEALLOCATE(b);
    C_kontinue(k, result);
  case 2:
    /* Edge case: most negative fixnum */
    if (BIGNUM_NEGATIVE_P(b) && BIGNUM_REF(b, 0) == 0 && BIGNUM_REF(b, 1) == 1) {
      BIGNUM_DEALLOCATE(b);
      C_kontinue(k, C_fix(C_MOST_NEGATIVE_FIXNUM));
    }
    /* FALLTHROUGH */
  default:
    {
      C_word pab[2], *pa = pab, kab[4], *ka = kab, k2;
      /* Make result a wrapped pointer because C_closure wants scheme objects */
      result = C_mpointer(&pa, b);
      k2 = C_closure(&ka, 3, (C_word)C_wrap_bignum, k, result);
      /* Here we assume bignum digits are C words.. */
      C_allocate_vector(6, (C_word)NULL, k2,
			C_fix(sizeof(C_word) * (BIGNUM_LENGTH(b) + 1)),
                        /* Byte vec, no initialization, align at 8 bytes */
                        C_SCHEME_TRUE, C_SCHEME_FALSE, C_SCHEME_FALSE);
    }
  }
}

static void
C_bignum_wrapped_return_bigobj(C_word c, C_word closure, C_word wrapped_big)
{
  C_word obj = C_block_item(closure, 2);
  C_values(4, C_SCHEME_UNDEFINED, C_block_item(closure, 1), wrapped_big, obj);
}

static void
C_return_big_fix(C_word k, bignum_type big, C_word fix)
{
  C_word kab[4], *ka = kab, k2;
  k2 = C_closure(&ka, 3, (C_word)C_bignum_wrapped_return_bigobj, k, fix);
  C_return_bignum(k2, big);
}

static void
C_b2_wrapped(C_word c, C_word closure, C_word wrapped_b2)
{
  C_word k = C_block_item(closure, 1); /* Original closure */
  C_word kab[4], *ka = kab, k2; /* Next continuation */
  k2 = C_closure(&ka, 3, (C_word)C_bignum_wrapped_return_bigobj, k, wrapped_b2);
  C_return_bignum(k2, (bignum_type)C_block_item(C_block_item(closure, 2), 0));
}

static void
C_return_2_bignums(C_word k, bignum_type b1, bignum_type b2)
{
  C_word bab[2], *ba = bab, kab[4], *ka = kab, k2, b1_ptr;
  /* Make b1 a wrapped pointer because C_closure wants scheme objects */
  b1_ptr = C_mpointer(&ba, b1);
  /* Wrap b2 first, then b1. Return them to k in the original (b1,b2) order */
  k2 = C_closure(&ka, 3, (C_word)C_b2_wrapped, k, b1_ptr);
  C_return_bignum(k2, b2);
}


static bignum_type
bignum_allocate(bignum_length_type length, int negative_p)
{
  bignum_type result;
  bignum_digit_type *digits;

  digits = (bignum_digit_type *)C_malloc(sizeof(bignum_digit_type)*(length + 1));
  
  if(digits == NULL) {
    fprintf(stderr, "out of memory - can not allocate number");
    exit(EXIT_FAILURE);
  }

  result = (bignum_type)digits;
  BIGNUM_SET_HEADER(result, length, negative_p);
  return result;
}

static bignum_type
bignum_allocate_zeroed(bignum_length_type length, int negative_p)
{
  BIGNUM_ASSERT ((length >= 0) || (length < BIGNUM_RADIX));
  {
    bignum_type result;
    bignum_digit_type *digits;
    bignum_digit_type *scan;
    bignum_digit_type *end;
    digits=(bignum_digit_type *)C_malloc(sizeof(bignum_digit_type)*(length+1));
  
    if(digits == NULL) {
      fprintf(stderr, "out of memory - can not allocate number");
      exit(EXIT_FAILURE);
    }

    result = (bignum_type)digits;
    BIGNUM_SET_HEADER(result, length, negative_p);
    scan = BIGNUM_START_PTR(result);
    end = scan + length;
    while (scan < end)
      (*scan++) = 0;
    return (result);
  }
}

static bignum_type
bignum_allocate_from_fixnum(C_word fix)
{
  bignum_type ret;

  if (fix == C_fix(0)) {
    ret = bignum_allocate(0, 0);
  } else if (fix == C_fix(C_MOST_NEGATIVE_FIXNUM)) {
    ret = bignum_allocate(2, 1);
    BIGNUM_REF(ret, 0) = 0;
    BIGNUM_REF(ret, 1) = 1;
  } else {
    bignum_digit_type digit = C_unfix(fix);
    ret = bignum_allocate(1, digit < 0);
    BIGNUM_REF(ret, 0) = ((digit < 0) ? -digit : digit);
  }
  return ret;
}

static bignum_type
bignum_digit_to_bignum(bignum_digit_type digit, int neg_p)
{
  bignum_type ret;
  if (digit == 0)
    return bignum_allocate(0, 0);

  ret = bignum_allocate(1, neg_p);
  BIGNUM_REF(ret, 0) = digit;
  return ret;
}

static bignum_type
shorten_bignum(bignum_type big, bignum_length_type newlength)
{
  bignum_digit_type *digits, *newdigits;
  digits = BIGNUM_TO_POINTER(big);
  newdigits = (bignum_digit_type *)C_realloc(digits, sizeof(bignum_digit_type)*(newlength + 1));
  if (newdigits == NULL) {
    fprintf(stderr, "out of memory - can not reallocate number");
    exit(EXIT_FAILURE);
  }
  return (bignum_type)newdigits;
}

static bignum_type
bignum_shorten_length(bignum_type bignum, bignum_length_type length)
{
  bignum_length_type current_length = (BIGNUM_LENGTH (bignum));
  BIGNUM_ASSERT ((length >= 0) || (length <= current_length));
  if (length < current_length)
    {
      BIGNUM_SET_HEADER
        (bignum, length, ((length != 0) && (BIGNUM_NEGATIVE_P (bignum))));
      BIGNUM_REDUCE_LENGTH (bignum, bignum, length);
    }
  return bignum;
}

static bignum_type
bignum_trim(bignum_type bignum)
{
  bignum_digit_type * start = (BIGNUM_START_PTR (bignum));
  bignum_digit_type * end = (start + (BIGNUM_LENGTH (bignum)));
  bignum_digit_type * scan = end;
  while ((start <= scan) && ((*--scan) == 0))
    ;
  scan += 1;
  if (scan < end)
    {
      bignum_length_type length = (scan - start);
      BIGNUM_SET_HEADER
        (bignum, length, ((length != 0) && (BIGNUM_NEGATIVE_P (bignum))));
      BIGNUM_REDUCE_LENGTH (bignum, bignum, length);
    }
  return (bignum);
}

/* Copying */

static bignum_type
bignum_copy(bignum_type source)
{
  bignum_type target =
    (bignum_allocate ((BIGNUM_LENGTH (source)), (BIGNUM_NEGATIVE_P (source))));
  bignum_destructive_copy (source, target);
  return (target);
}

static void
bignum_destructive_copy(bignum_type source, bignum_type target)
{
  bignum_digit_type * scan_source = (BIGNUM_START_PTR (source));
  bignum_digit_type * end_source =
    (scan_source + (BIGNUM_LENGTH (source)));
  bignum_digit_type * scan_target = (BIGNUM_START_PTR (target));
  while (scan_source < end_source)
    (*scan_target++) = (*scan_source++);
  return;
}

static bignum_type
bignum_new_sign(bignum_type bignum, int negative_p)
{
  bignum_type result =
    (bignum_allocate ((BIGNUM_LENGTH (bignum)), negative_p));
  bignum_destructive_copy (bignum, result);
  return (result);
}

#define fix_randomize  C_randomize
#define fix_random C_random_fixnum

/*
 * This random number generator is very simple. Probably too simple...
 */
static C_word
big_randomize(C_word bignum)
{
  bignum_type big;
  C_word seed;
  bignum_digit_type * scan, * end;

  big = big_of(bignum);
  scan = (BIGNUM_START_PTR (big));
  end = scan + BIGNUM_LENGTH(big);
  /* What a cheap way to initialize the random generator. I feel dirty! */
  while (scan < end)
    seed ^= *scan++;

  srand(C_unfix(seed));
  return C_SCHEME_UNDEFINED;
}

static void
big_random(C_word c, C_word self, C_word k, C_word max)
{
  bignum_type bigmax;
  bignum_type result;
  bignum_length_type randlen, max_len, max_bits;
  bignum_digit_type max_top_digit, d;
  bignum_digit_type * scan, * end;
  
  bigmax = big_of(max);

  max_len = BIGNUM_LENGTH(bigmax);
  max_top_digit = d = BIGNUM_REF(bigmax, max_len - 1);
  
  max_bits = (max_len - 1) * BIGNUM_DIGIT_LENGTH;
  while(d) {
    max_bits++;
    d >>= 1;
  }
  /* Subtract/add one because we don't want zero to be over-represented */
  randlen = (((double)rand())/(RAND_MAX + 1.0) * (double)(max_bits - 1));
  randlen = BIGNUM_BITS_TO_DIGITS(randlen + 1);
  
  result = bignum_allocate(randlen, BIGNUM_NEGATIVE_P(bigmax));
  scan = BIGNUM_START_PTR(result);
  end = scan + randlen - 1; /* Go to just before the end */
  while(scan < end)
    *scan++ = (((double)rand())/(RAND_MAX + 1.0) * (double)BIGNUM_RADIX);
  /*
   * Last word is special when length is max_len: It must be less than
   * max's most significant digit, instead of BIGNUM_RADIX.
   */
  if (max_len == randlen)
    *scan = (((double)rand())/(RAND_MAX + 1.0) * (double)max_top_digit);
  else
    *scan = (((double)rand())/(RAND_MAX + 1.0) * (double)BIGNUM_RADIX);

  result = bignum_trim(result);  
  C_return_bignum(k, result);
}

static void
fix_plus_fix(C_word c, C_word self, C_word k, C_word x, C_word y)
{
  C_word z;

  /* Exceptional situation: this will cause a real overflow */
  if (x == C_fix(C_MOST_NEGATIVE_FIXNUM) && y == C_fix(C_MOST_NEGATIVE_FIXNUM)) {
    bignum_type big;
    bignum_digit_type *ref;
    big = bignum_allocate(2, 1);
    ref = BIGNUM_START_PTR(big);
    *ref++ = 0;
    *ref = 2;
    C_return_bignum(k, big);
  }

  z = C_unfix(x) + C_unfix(y);

  /* This code "knows" that both fixnums and bignums have 2 reserved bits */
  if(!C_fitsinfixnump(z)) {
    bignum_type big;
    bignum_digit_type *ref;
    big = bignum_allocate(2, (z < 0));
    ref = BIGNUM_START_PTR(big);
    *ref++ = labs(z) & BIGNUM_DIGIT_MASK;
    *ref = 1;
    C_return_bignum(k, big);
  }

  C_kontinue(k, C_fix(z));
}


static void
fix_plus_big(C_word c, C_word self, C_word k, C_word x, C_word y)
{
  bignum_type bigx, result;
  bigx = bignum_allocate_from_fixnum(x);
  result = bignum_add(bigx, big_of(y));
  BIGNUM_DEALLOCATE(bigx);
  C_return_bignum(k, result);
}


static void
fix_minus_big(C_word c, C_word self, C_word k, C_word x, C_word y)
{
  bignum_type tmp, result;
  tmp = bignum_allocate_from_fixnum(x);

  result = bignum_subtract(tmp, big_of(y));
  BIGNUM_DEALLOCATE(tmp);
  C_return_bignum(k, result);
}


static void
big_minus_fix(C_word c, C_word self, C_word k, C_word x, C_word y)
{
  bignum_type tmp, result;
  tmp = bignum_allocate_from_fixnum(y);

  result = bignum_subtract(big_of(x), tmp);
  BIGNUM_DEALLOCATE(tmp);
  C_return_bignum(k, result);
}

static void
big_divrem_big(C_word c, C_word self, C_word k, C_word x, C_word y)
{
  bignum_type numerator = big_of(x);
  bignum_type denominator = big_of(y);
  int q_neg_p = ((BIGNUM_NEGATIVE_P (denominator))
                 ? (! (BIGNUM_NEGATIVE_P (numerator)))
                 : (BIGNUM_NEGATIVE_P (numerator)));

  switch (bignum_compare_unsigned (numerator, denominator))
    {
    case bignum_comparison_equal:
      C_values(4, C_SCHEME_UNDEFINED, k,
               q_neg_p ? C_fix(-1) : C_fix(1), C_fix(0));
    case bignum_comparison_less:
      C_values(4, C_SCHEME_UNDEFINED, k, C_fix(0), x);
    case bignum_comparison_greater:
    default:                                  /* to appease gcc -Wall */
      {
        bignum_type quotient, remainder;
	int r_neg_p = BIGNUM_NEGATIVE_P (numerator) ? 1 : 0;
	
        BIGNUM_ASSERT(BIGNUM_LENGTH(denominator) > 1);
        bignum_divide_unsigned_large_denominator
          (numerator, denominator, (&quotient), (&remainder), q_neg_p, r_neg_p);
        C_return_2_bignums(k, quotient, remainder);
      }
    }
}

static void
big_divrem_fix(C_word c, C_word self, C_word k, C_word x, C_word y)
{
  bignum_type bigx = big_of(x);
  y = C_unfix(y);

  if (y == 1)
    C_values(4, C_SCHEME_UNDEFINED, k, x, C_fix(0));
  else if (y == -1)
    C_return_big_fix(k, bignum_new_sign(bigx, !(BIGNUM_NEGATIVE_P(bigx))), C_fix(0));

  /* Too bad, we really need to do some work... */
  {
    int q_neg_p = (y < 0) ? !(BIGNUM_NEGATIVE_P(bigx)) : BIGNUM_NEGATIVE_P(bigx);
    int r_neg_p = BIGNUM_NEGATIVE_P(bigx);
    bignum_digit_type abs_y = (y < 0) ? -y : y;
    bignum_type quotient, remainder;
    
    if (y == C_MOST_NEGATIVE_FIXNUM) {
      if (!BIGNUM_NEGATIVE_P(bigx) && BIGNUM_LENGTH(bigx) == 1
          && BIGNUM_REF(bigx, 1) == 1 && BIGNUM_REF(bigx, 0) == 0) {
        /*
         * Very very special case:
         * quotient(MOST_NEGATIVE_FIXNUM, -(MOST_NEGATIVE_FIXNUM)) => -1
         */
        C_values(4, C_SCHEME_UNDEFINED, k, C_fix(-1), C_fix(0));
      } else {
        /* This is the only case we need to go allocate a bignum for */
        bignum_type bigy =
	  bignum_allocate_from_fixnum(C_fix(C_MOST_NEGATIVE_FIXNUM));

        bignum_divide_unsigned_large_denominator
          (bigx, bigy, (&quotient), (&remainder), q_neg_p, r_neg_p);
        BIGNUM_DEALLOCATE(bigy);
        C_return_2_bignums(k, quotient, remainder);
      }
    } else if (abs_y < BIGNUM_RADIX_ROOT) {
      bignum_divide_unsigned_small_denominator
        (bigx, abs_y, (&quotient), (&remainder), q_neg_p, r_neg_p);
      C_return_2_bignums(k, quotient, remainder);
    } else {
      bignum_divide_unsigned_medium_denominator
        (bigx, abs_y, (&quotient), (&remainder), q_neg_p, r_neg_p);
      C_return_2_bignums(k, quotient, remainder);
    }
  }
}

static void
C_fixnum_gcd(C_word c, C_word self, C_word k, C_word x, C_word y)
{
   C_word r;
   
   x = C_unfix(x);
   y = C_unfix(y);
   
   if (x < 0) x = -x;
   if (y < 0) y = -y;
   
   while(y != 0) {
     r = x % y;
     x = y;
     y = r;
   }
   C_kontinue(k, C_fix(x));
}

/*
 * Big_gcd is a huge function and it sucks that it needs to be in C.
 *
 * Why does it need to be in C?  Because if you have a very big bignum
 * that doesn't cleanly divide another big bignum, you end up calling
 * the remainder procedure a lot in Scheme.  This produces tons of
 * intermediate bignums, which means a lot of copies into GC'able memory
 * need to be made (and the GC will be triggered more often).
 * That's a major slowdown.  Doing the loop in C means the intermediate
 * results can be cleaned up right away each loop step, and returning
 * just one result to Scheme.
 * Once (if?) we find a way to avoid copying bignums (instead allocating
 * directly in GCable memory) this function can be cut out and replaced by
 * a recursive call to gcd-0 in Scheme.
 */
static void
big_gcd(C_word c, C_word self, C_word k, C_word x, C_word y)
{
  bignum_type bigx = big_of(x), bigy = big_of(y), bigr;

  BIGNUM_ASSERT(BIGNUM_LENGTH(bigx) > 1);
  BIGNUM_ASSERT(BIGNUM_LENGTH(bigy) > 1);
  
  switch(bignum_compare_unsigned (bigx, bigy)) {
  case bignum_comparison_equal:
    C_kontinue(k, x);
  case bignum_comparison_less:
    /* Swap, since remainder of bigx, bigy would be bigx, causing an extra loop */
    bigr = bigy;
    bigy = bigx;
    bigx = bigr;

    /* Make x and y match for the special case where gcd(x, y) = y */
    {
      C_word tmp = y;
      y = x;
      x = tmp;
    }
    /* FALLTHROUGH */
  default: /* Continue below */
    break;
  }
  
  /*
   * Be careful! Don't deallocate live objects. We could start with a copy
   * or compare pointers with big_of(x) or y every time but that seems wasteful.
   */
  bignum_divide_unsigned_large_denominator
   (bigx, bigy, ((bignum_type *) 0), (&bigr), 0, 0);
  bigx = bigy;
  bigy = bigr;
  /* Original bigx is forgotten now */
  BIGNUM_ASSERT(bigx != big_of(x));
  BIGNUM_ASSERT(bigy != big_of(x));
  /* Only bigx points to y */
  BIGNUM_ASSERT(bigy != big_of(y));
  BIGNUM_ASSERT(bigx == big_of(y));

  switch (BIGNUM_LENGTH(bigy)) {
  case 0:  /* bigy = 0 */
    /* remainder(x, y) = 0 => y  |  x < y */
    BIGNUM_DEALLOCATE(bigy); /* Got allocated in previous step */
    C_kontinue(k, y);
  case 1:
    if (BIGNUM_REF(bigy, 0) == 1) { /* y = 1? Then 1 is the result */
      BIGNUM_DEALLOCATE(bigy); /* Got allocated in previous step */
      C_kontinue(k, C_fix(1));
    } else if (BIGNUM_REF(bigy, 0) < BIGNUM_RADIX_ROOT)
      bigr = bignum_remainder_unsigned_small_denominator
              (bigx, BIGNUM_REF(bigy, 0), 0);
    else
      bignum_divide_unsigned_medium_denominator
       (bigx, BIGNUM_REF(bigy, 0), (bignum_type *)0, (&bigr), 0, 0);
    break;
  default:
    bignum_divide_unsigned_large_denominator
     (bigx, bigy, ((bignum_type *) 0), (&bigr), 0, 0);
  }
  /* Swap, but don't deallocate x since it holds the original value of y */
  bigx = bigy;
  bigy = bigr;
  
  /* Original bigy is forgotten now, we can safely always deallocate bigx */

  /* Assume that bignums coming from outside are never length 1 */
  BIGNUM_ASSERT(bigx != big_of(y));
  
  while(BIGNUM_LENGTH(bigy) > 1) {
    bignum_divide_unsigned_large_denominator
     (bigx, bigy, ((bignum_type *) 0), (&bigr), 0, 0);
    BIGNUM_DEALLOCATE(bigx);
    bigx = bigy;
    bigy = bigr;
  }

  /* Finish up with a faster loop until y = 0 (ie, length(bigy) = 0) */
  while (BIGNUM_LENGTH(bigy) == 1) {
    if (BIGNUM_REF(bigy, 0) == 1) {
      BIGNUM_DEALLOCATE(bigx);
      BIGNUM_DEALLOCATE(bigy);
      C_kontinue(k, C_fix(1));
      break;
    } else if (BIGNUM_REF(bigy, 0) < BIGNUM_RADIX_ROOT) {
      bigr = bignum_remainder_unsigned_small_denominator
              (bigx, BIGNUM_REF(bigy, 0), 0);
    } else {
      bignum_divide_unsigned_medium_denominator
       (bigx, BIGNUM_REF(bigy, 0), (bignum_type *)0, (&bigr), 0, 0);
    }
    BIGNUM_DEALLOCATE(bigx);
    bigx = bigy;
    bigy = bigr;
  }
  BIGNUM_DEALLOCATE(bigy);
  C_return_bignum(k, bigx);
}

static C_word
C_a_i_flonum_gcd(C_word **p, C_word n, C_word x, C_word y)
{
   double xub, yub, r;
   
   xub = C_flonum_magnitude(x);
   yub = C_flonum_magnitude(y);

   if (xub < 0.0) xub = -xub;
   if (yub < 0.0) yub = -yub;
   
   while(yub != 0.0) {
     r = fmod(xub, yub);
     xub = yub;
     yub = r;
   }
   C_return(C_flonum(p, xub));
}


static void
big_plus_big(C_word c, C_word self, C_word k, C_word x, C_word y)
{
  bignum_type result;
  result = bignum_add(big_of(x), big_of(y));
  C_return_bignum(k, result);
}

/*
 * This now makes the assumption it is never passed a bignum of LENGTH 0.
 * This should always be valid in Chicken.
 */
static bignum_type
bignum_add(bignum_type x, bignum_type y)
{
  return
    (((BIGNUM_NEGATIVE_P (x))
      ? ((BIGNUM_NEGATIVE_P (y))
         ? (bignum_add_unsigned (x, y, 1))
         : (bignum_subtract_unsigned (y, x)))
      : ((BIGNUM_NEGATIVE_P (y))
         ? (bignum_subtract_unsigned (x, y))
         : (bignum_add_unsigned (x, y, 0)))));
}

static bignum_type
bignum_add_unsigned(bignum_type x, bignum_type y, int negative_p)
{
  if ((BIGNUM_LENGTH (y)) > (BIGNUM_LENGTH (x)))
    {
      bignum_type z = x;
      x = y;
      y = z;
    }
  {
    bignum_length_type x_length = (BIGNUM_LENGTH (x));
    bignum_type r = (bignum_allocate ((x_length + 1), negative_p));
    bignum_digit_type sum;
    bignum_digit_type carry = 0;
    bignum_digit_type * scan_x = (BIGNUM_START_PTR (x));
    bignum_digit_type * scan_r = (BIGNUM_START_PTR (r));
    {
      bignum_digit_type * scan_y = (BIGNUM_START_PTR (y));
      bignum_digit_type * end_y = (scan_y + (BIGNUM_LENGTH (y)));
      while (scan_y < end_y)
        {
          sum = ((*scan_x++) + (*scan_y++) + carry);
          if (sum < BIGNUM_RADIX)
            {
              (*scan_r++) = sum;
              carry = 0;
            }
          else
            {
              (*scan_r++) = (sum - BIGNUM_RADIX);
              carry = 1;
            }
        }
    }
    {
      bignum_digit_type * end_x = ((BIGNUM_START_PTR (x)) + x_length);
      if (carry != 0)
        while (scan_x < end_x)
          {
            sum = ((*scan_x++) + 1);
            if (sum < BIGNUM_RADIX)
              {
                (*scan_r++) = sum;
                carry = 0;
                break;
              }
            else
              (*scan_r++) = (sum - BIGNUM_RADIX);
          }
      while (scan_x < end_x)
        (*scan_r++) = (*scan_x++);
    }
    if (carry != 0)
      {
        (*scan_r) = 1;
        return (r);
      }
    return (bignum_shorten_length (r, x_length));
  }
}

static void
big_minus_big(C_word c, C_word self, C_word k, C_word x, C_word y)
{
  bignum_type result;
  result = bignum_subtract(big_of(x), big_of(y));
  C_return_bignum(k, result);
}

/*
 * This now makes the assumption it is never passed a bignum of LENGTH 0.
 * This should always be valid in Chicken.
 */
static bignum_type
bignum_subtract(bignum_type x, bignum_type y)
{
  return
    (((BIGNUM_NEGATIVE_P (x))
       ? ((BIGNUM_NEGATIVE_P (y))
          ? (bignum_subtract_unsigned (y, x))
          : (bignum_add_unsigned (x, y, 1)))
       : ((BIGNUM_NEGATIVE_P (y))
          ? (bignum_add_unsigned (x, y, 0))
          : (bignum_subtract_unsigned (x, y)))));
}

static bignum_type
bignum_subtract_unsigned(bignum_type x, bignum_type y)
{
  int negative_p;
  switch (bignum_compare_unsigned (x, y))
    {
    case bignum_comparison_equal:
      return (BIGNUM_ZERO ());
    case bignum_comparison_less:
      {
        bignum_type z = x;
        x = y;
        y = z;
      }
      negative_p = 1;
      break;
    case bignum_comparison_greater:
      negative_p = 0;
      break;
    }
  {
    bignum_length_type x_length = (BIGNUM_LENGTH (x));
    bignum_type r = (bignum_allocate (x_length, negative_p));
    bignum_digit_type difference;
    bignum_digit_type borrow = 0;
    bignum_digit_type * scan_x = (BIGNUM_START_PTR (x));
    bignum_digit_type * scan_r = (BIGNUM_START_PTR (r));
    {
      bignum_digit_type * scan_y = (BIGNUM_START_PTR (y));
      bignum_digit_type * end_y = (scan_y + (BIGNUM_LENGTH (y)));
      while (scan_y < end_y)
        {
          difference = (((*scan_x++) - (*scan_y++)) - borrow);
          if (difference < 0)
            {
              (*scan_r++) = (difference + BIGNUM_RADIX);
              borrow = 1;
            }
          else
            {
              (*scan_r++) = difference;
              borrow = 0;
            }
        }
    }
    {
      bignum_digit_type * end_x = ((BIGNUM_START_PTR (x)) + x_length);
      if (borrow != 0)
        while (scan_x < end_x)
          {
            difference = ((*scan_x++) - borrow);
            if (difference < 0)
              (*scan_r++) = (difference + BIGNUM_RADIX);
            else
              {
                (*scan_r++) = difference;
                borrow = 0;
                break;
              }
          }
      BIGNUM_ASSERT (borrow == 0);
      while (scan_x < end_x)
        (*scan_r++) = (*scan_x++);
    }
    return (bignum_trim (r));
  }
}


static void
big_times_big(C_word c, C_word self, C_word k, C_word x, C_word y)
{
  bignum_type bigx = big_of(x), bigy = big_of(y);
  int neg_p = ((BIGNUM_NEGATIVE_P (bigx))
               ? (! (BIGNUM_NEGATIVE_P (bigy)))
               : (BIGNUM_NEGATIVE_P (bigy)));
  /* If length 1 or 0, it should be a fixnum */
  BIGNUM_ASSERT(BIGNUM_LENGTH(bigx) > 1);
  BIGNUM_ASSERT(BIGNUM_LENGTH(bigy) > 1);
  C_return_bignum(k, bignum_multiply_unsigned(bigx, bigy, neg_p));
}

static void
fix_times_fix(C_word c, C_word self, C_word k, C_word x, C_word y)
{
  bignum_type bigx, bigy, result;
  bignum_digit_type absx, absy;
  C_word neg_p;

  absx = C_unfix(x);
  absx = absx < 0 ? -absx : absx;
  absy = C_unfix(y);
  absy = absy < 0 ? -absy : absy;
  neg_p = ((x & C_INT_SIGN_BIT) ? !(y & C_INT_SIGN_BIT) : (y & C_INT_SIGN_BIT));

  if (absx < BIGNUM_RADIX_ROOT) {
     if (absx == 0 || absx == 1 || absy < BIGNUM_RADIX_ROOT) {
       C_kontinue(k, C_fix(neg_p ? -(absx * absy) : (absx * absy)));
     } else {
       bigy = bignum_allocate_from_fixnum(y);
       result = bignum_multiply_unsigned_small_factor(bigy, absx, neg_p ? 1 : 0);
       BIGNUM_DEALLOCATE(bigy);
       C_return_bignum(k, result);
     }
  } else if (absy < BIGNUM_RADIX_ROOT) {
     if (absy == 0 || absy == 1 /*|| absx < BIGNUM_RADIX_ROOT */) {
       C_kontinue(k, C_fix(neg_p ? -(absx * absy) : (absx * absy)));
     } else {
       bigx = bignum_allocate_from_fixnum(x);
       result = bignum_multiply_unsigned_small_factor(bigx, absy, neg_p ? 1 : 0);
       BIGNUM_DEALLOCATE(bigx);
       C_return_bignum(k, result);
     }
  } else {
    bigx = bignum_allocate_from_fixnum(x);
    bigy = bignum_allocate_from_fixnum(y);
    result = bignum_multiply_unsigned (bigx, bigy, neg_p ? 1 : 0);
    BIGNUM_DEALLOCATE(bigy);
    BIGNUM_DEALLOCATE(bigx);
    C_return_bignum(k, result);
  }
}


static void
fix_times_big(C_word c, C_word self, C_word k, C_word x, C_word y)
{
  bignum_type bigx, result, bigy;
  bignum_digit_type absx;
  C_word neg_p;
  
  bigy = big_of(y);
  
  if (x == C_fix(0))
    C_kontinue(k, C_fix(0));
  else if (x == C_fix(1))
    C_kontinue(k, y);
  else if (x == C_fix(-1))
    C_return_bignum(k, bignum_new_sign(bigy, !(BIGNUM_NEGATIVE_P(bigy))));

  absx = C_unfix(x);
  absx = absx < 0 ? -absx : absx;  
  neg_p = ((x & C_INT_SIGN_BIT)
           ? !(BIGNUM_NEGATIVE_P(bigy)) : (BIGNUM_NEGATIVE_P(bigy)));
  
  if (absx < BIGNUM_RADIX_ROOT) {
     C_return_bignum(k, bignum_multiply_unsigned_small_factor(bigy, absx, neg_p));
  } else {
    bigx = bignum_allocate_from_fixnum(x);
    result = bignum_multiply_unsigned (bigx, bigy, neg_p);
    BIGNUM_DEALLOCATE(bigx);
    C_return_bignum(k, result);
  }
}

/* Multiplication
   Maximum value for product_low or product_high:
        ((R * R) + (R * (R - 2)) + (R - 1))
   Maximum value for carry: ((R * (R - 1)) + (R - 1))
        where R == BIGNUM_RADIX_ROOT */

static bignum_type
bignum_multiply_unsigned(bignum_type x, bignum_type y, int negative_p)
{
  if ((BIGNUM_LENGTH (y)) > (BIGNUM_LENGTH (x)))
    {
      bignum_type z = x;
      x = y;
      y = z;
    }
  {
    bignum_digit_type carry;
    bignum_digit_type y_digit_low;
    bignum_digit_type y_digit_high;
    bignum_digit_type x_digit_low;
    bignum_digit_type x_digit_high;
    bignum_digit_type product_low;
    bignum_digit_type * scan_r;
    bignum_digit_type * scan_y;
    bignum_length_type x_length = (BIGNUM_LENGTH (x));
    bignum_length_type y_length = (BIGNUM_LENGTH (y));
    bignum_type r = (bignum_allocate_zeroed ((x_length + y_length), negative_p));
    bignum_digit_type * scan_x = (BIGNUM_START_PTR (x));
    bignum_digit_type * end_x = (scan_x + x_length);
    bignum_digit_type * start_y = (BIGNUM_START_PTR (y));
    bignum_digit_type * end_y = (start_y + y_length);
    bignum_digit_type * start_r = (BIGNUM_START_PTR (r));
#define x_digit x_digit_high
#define y_digit y_digit_high
#define product_high carry
    while (scan_x < end_x)
      {
        x_digit = (*scan_x++);
        x_digit_low = (HD_LOW (x_digit));
        x_digit_high = (HD_HIGH (x_digit));
        carry = 0;
        scan_y = start_y;
        scan_r = (start_r++);
        while (scan_y < end_y)
          {
            y_digit = (*scan_y++);
            y_digit_low = (HD_LOW (y_digit));
            y_digit_high = (HD_HIGH (y_digit));
            product_low =
              ((*scan_r) +
               (x_digit_low * y_digit_low) +
               (HD_LOW (carry)));
            product_high =
              ((x_digit_high * y_digit_low) +
               (x_digit_low * y_digit_high) +
               (HD_HIGH (product_low)) +
               (HD_HIGH (carry)));
            (*scan_r++) =
              (HD_CONS ((HD_LOW (product_high)), (HD_LOW (product_low))));
            carry =
              ((x_digit_high * y_digit_high) +
               (HD_HIGH (product_high)));
          }
        (*scan_r) += carry;
      }
    return (bignum_trim (r));
#undef x_digit
#undef y_digit
#undef product_high
  }
}

static bignum_type
bignum_multiply_unsigned_small_factor(bignum_type x, bignum_digit_type y,
                                      int negative_p)
{
  bignum_length_type length_x = (BIGNUM_LENGTH (x));
  bignum_type p = (bignum_allocate ((length_x + 1), negative_p));
  bignum_destructive_copy (x, p);
  (BIGNUM_REF (p, length_x)) = 0;
  bignum_destructive_scale_up (p, y);
  return (bignum_trim (p));
}

static void
bignum_destructive_scale_up(bignum_type bignum, bignum_digit_type factor)
{
  bignum_digit_type carry = 0;
  bignum_digit_type * scan = (BIGNUM_START_PTR (bignum));
  bignum_digit_type two_digits;
  bignum_digit_type product_low;
#define product_high carry
  bignum_digit_type * end = (scan + (BIGNUM_LENGTH (bignum)));
  BIGNUM_ASSERT ((factor > 1) && (factor < BIGNUM_RADIX_ROOT));
  while (scan < end)
    {
      two_digits = (*scan);
      product_low = ((factor * (HD_LOW (two_digits))) + (HD_LOW (carry)));
      product_high =
        ((factor * (HD_HIGH (two_digits))) +
         (HD_HIGH (product_low)) +
         (HD_HIGH (carry)));
      (*scan++) = (HD_CONS ((HD_LOW (product_high)), (HD_LOW (product_low))));
      carry = (HD_HIGH (product_high));
    }
  /* A carry here would be an overflow, i.e. it would not fit.
     Hopefully the callers allocate enough space that this will
     never happen.
   */
  BIGNUM_ASSERT (carry == 0);
  return;
#undef product_high
}

static void
bignum_destructive_add(bignum_type bignum, bignum_digit_type n)
{
  bignum_digit_type * scan = (BIGNUM_START_PTR (bignum));
  bignum_digit_type digit;
  digit = ((*scan) + n);
  if (digit < BIGNUM_RADIX)
    {
      (*scan) = digit;
      return;
    }
  (*scan++) = (digit - BIGNUM_RADIX);
  while (1)
    {
      digit = ((*scan) + 1);
      if (digit < BIGNUM_RADIX)
        {
          (*scan) = digit;
          return;
        }
      (*scan++) = (digit - BIGNUM_RADIX);
    }
}

/* Division */

/* For help understanding this algorithm, see:
   Knuth, Donald E., "The Art of Computer Programming",
   volume 2, "Seminumerical Algorithms"
   section 4.3.1, "Multiple-Precision Arithmetic". */

static void
bignum_divide_unsigned_large_denominator(bignum_type numerator,
                                         bignum_type denominator,
                                         bignum_type * quotient,
                                         bignum_type * remainder,
                                         int q_negative_p,
                                         int r_negative_p)
{
  bignum_length_type length_n = ((BIGNUM_LENGTH (numerator)) + 1);
  bignum_length_type length_d = (BIGNUM_LENGTH (denominator));
  bignum_type q =
    ((quotient != ((bignum_type *) 0))
     ? (bignum_allocate ((length_n - length_d), q_negative_p))
     : BIGNUM_OUT_OF_BAND);
  bignum_type u = (bignum_allocate (length_n, r_negative_p));
  int shift = 0;
  BIGNUM_ASSERT (length_d > 1);
  {
    bignum_digit_type v1 = (BIGNUM_REF ((denominator), (length_d - 1)));
    while (v1 < (BIGNUM_RADIX / 2))
      {
        v1 <<= 1;
        shift += 1;
      }
  }
  if (shift == 0)
    {
      bignum_destructive_copy (numerator, u);
      (BIGNUM_REF (u, (length_n - 1))) = 0;
      bignum_divide_unsigned_normalized (u, denominator, q);
      if (remainder != ((bignum_type *) 0))
        (*remainder) = (bignum_trim (u));
      else
        BIGNUM_DEALLOCATE (u);
    }
  else
    {
      bignum_type v = (bignum_allocate (length_d, 0));
      bignum_destructive_normalization (numerator, u, shift);
      bignum_destructive_normalization (denominator, v, shift);
      bignum_divide_unsigned_normalized (u, v, q);
      BIGNUM_DEALLOCATE (v);
      if (remainder != ((bignum_type *) 0))
        (*remainder) = bignum_destructive_unnormalization (u, shift);
      else
	BIGNUM_DEALLOCATE(u);
    }
  if (quotient != ((bignum_type *) 0))
    (*quotient) = (bignum_trim (q));
  return;
}

static void
bignum_divide_unsigned_normalized(bignum_type u, bignum_type v, bignum_type q)
{
  bignum_length_type u_length = (BIGNUM_LENGTH (u));
  bignum_length_type v_length = (BIGNUM_LENGTH (v));
  bignum_digit_type * u_start = (BIGNUM_START_PTR (u));
  bignum_digit_type * u_scan = (u_start + u_length);
  bignum_digit_type * u_scan_limit = (u_start + v_length);
  bignum_digit_type * u_scan_start = (u_scan - v_length);
  bignum_digit_type * v_start = (BIGNUM_START_PTR (v));
  bignum_digit_type * v_end = (v_start + v_length);
  bignum_digit_type * q_scan;
  bignum_digit_type v1 = (v_end[-1]);
  bignum_digit_type v2 = (v_end[-2]);
  bignum_digit_type ph; /* high half of double-digit product */
  bignum_digit_type pl; /* low half of double-digit product */
  bignum_digit_type guess;
  bignum_digit_type gh; /* high half-digit of guess */
  bignum_digit_type ch; /* high half of double-digit comparand */
  bignum_digit_type v2l = (HD_LOW (v2));
  bignum_digit_type v2h = (HD_HIGH (v2));
  bignum_digit_type cl; /* low half of double-digit comparand */
#define gl ph                   /* low half-digit of guess */
#define uj pl
#define qj ph
  bignum_digit_type gm;         /* memory loc for reference parameter */
  if (q != BIGNUM_OUT_OF_BAND)
    q_scan = ((BIGNUM_START_PTR (q)) + (BIGNUM_LENGTH (q)));
  while (u_scan_limit < u_scan)
    {
      uj = (*--u_scan);
      if (uj != v1)
        {
          /* comparand =
             (((((uj * BIGNUM_RADIX) + uj1) % v1) * BIGNUM_RADIX) + uj2);
             guess = (((uj * BIGNUM_RADIX) + uj1) / v1); */
          cl = (u_scan[-2]);
          ch = (bignum_digit_divide (uj, (u_scan[-1]), v1, (&gm)));
          guess = gm;
        }
      else
        {
          cl = (u_scan[-2]);
          ch = ((u_scan[-1]) + v1);
          guess = (BIGNUM_RADIX - 1);
        }
      while (1)
        {
          /* product = (guess * v2); */
          gl = (HD_LOW (guess));
          gh = (HD_HIGH (guess));
          pl = (v2l * gl);
          ph = ((v2l * gh) + (v2h * gl) + (HD_HIGH (pl)));
          pl = (HD_CONS ((HD_LOW (ph)), (HD_LOW (pl))));
          ph = ((v2h * gh) + (HD_HIGH (ph)));
          /* if (comparand >= product) */
          if ((ch > ph) || ((ch == ph) && (cl >= pl)))
            break;
          guess -= 1;
          /* comparand += (v1 << BIGNUM_DIGIT_LENGTH) */
          ch += v1;
          /* if (comparand >= (BIGNUM_RADIX * BIGNUM_RADIX)) */
          if (ch >= BIGNUM_RADIX)
            break;
        }
      qj = (bignum_divide_subtract (v_start, v_end, guess, (--u_scan_start)));
      if (q != BIGNUM_OUT_OF_BAND)
        (*--q_scan) = qj;
    }
  return;
#undef gl
#undef uj
#undef qj
}

static bignum_digit_type
bignum_divide_subtract(bignum_digit_type * v_start,
                       bignum_digit_type * v_end,
                       bignum_digit_type guess,
                       bignum_digit_type * u_start)
{
  bignum_digit_type * v_scan = v_start;
  bignum_digit_type * u_scan = u_start;
  bignum_digit_type carry = 0;
  if (guess == 0) return (0);
  {
    bignum_digit_type gl = (HD_LOW (guess));
    bignum_digit_type gh = (HD_HIGH (guess));
    bignum_digit_type v;
    bignum_digit_type pl;
    bignum_digit_type vl;
#define vh v
#define ph carry
#define diff pl
    while (v_scan < v_end)
      {
        v = (*v_scan++);
        vl = (HD_LOW (v));
        vh = (HD_HIGH (v));
        pl = ((vl * gl) + (HD_LOW (carry)));
        ph = ((vl * gh) + (vh * gl) + (HD_HIGH (pl)) + (HD_HIGH (carry)));
        diff = ((*u_scan) - (HD_CONS ((HD_LOW (ph)), (HD_LOW (pl)))));
        if (diff < 0)
          {
            (*u_scan++) = (diff + BIGNUM_RADIX);
            carry = ((vh * gh) + (HD_HIGH (ph)) + 1);
          }
        else
          {
            (*u_scan++) = diff;
            carry = ((vh * gh) + (HD_HIGH (ph)));
          }
      }
    if (carry == 0)
      return (guess);
    diff = ((*u_scan) - carry);
    if (diff < 0)
      (*u_scan) = (diff + BIGNUM_RADIX);
    else
      {
        (*u_scan) = diff;
        return (guess);
      }
#undef vh
#undef ph
#undef diff
  }
  /* Subtraction generated carry, implying guess is one too large.
     Add v back in to bring it back down. */
  v_scan = v_start;
  u_scan = u_start;
  carry = 0;
  while (v_scan < v_end)
    {
      bignum_digit_type sum = ((*v_scan++) + (*u_scan) + carry);
      if (sum < BIGNUM_RADIX)
        {
          (*u_scan++) = sum;
          carry = 0;
        }
      else
        {
          (*u_scan++) = (sum - BIGNUM_RADIX);
          carry = 1;
        }
    }
  if (carry == 1)
    {
      bignum_digit_type sum = ((*u_scan) + carry);
      (*u_scan) = ((sum < BIGNUM_RADIX) ? sum : (sum - BIGNUM_RADIX));
    }
  return (guess - 1);
}

static void
bignum_divide_unsigned_medium_denominator(bignum_type numerator,
                                          bignum_digit_type denominator,
                                          bignum_type * quotient,
                                          bignum_type * remainder,
                                          int q_negative_p,
                                          int r_negative_p)
{
  bignum_length_type length_n = (BIGNUM_LENGTH (numerator));
  bignum_length_type length_q;
  bignum_type q;
  int shift = 0;
  /* Because `bignum_digit_divide' requires a normalized denominator. */
  while (denominator < (BIGNUM_RADIX / 2))
    {
      denominator <<= 1;
      shift += 1;
    }
  if (shift == 0)
    {
      length_q = length_n;
      q = (bignum_allocate (length_q, q_negative_p));
      bignum_destructive_copy (numerator, q);
    }
  else
    {
      length_q = (length_n + 1);
      q = (bignum_allocate (length_q, q_negative_p));
      bignum_destructive_normalization (numerator, q, shift);
    }
  {
    bignum_digit_type r = 0;
    bignum_digit_type * start = (BIGNUM_START_PTR (q));
    bignum_digit_type * scan = (start + length_q);
    bignum_digit_type qj;
    if (quotient != ((bignum_type *) 0))
      {
        while (start < scan)
          {
            r = (bignum_digit_divide (r, (*--scan), denominator, (&qj)));
            (*scan) = qj;
          }
        (*quotient) = (bignum_trim (q));
      }
    else
      {
        while (start < scan)
          r = (bignum_digit_divide (r, (*--scan), denominator, (&qj)));
        BIGNUM_DEALLOCATE (q);
      }
    if (remainder != ((bignum_type *) 0))
      {
        if (shift != 0)
          r >>= shift;
        (*remainder) = (bignum_digit_to_bignum (r, r_negative_p));
      }
  }
  return;
}

static void
bignum_destructive_normalization(bignum_type source, bignum_type target,
                                 int shift_left)
{
  bignum_digit_type digit;
  bignum_digit_type * scan_source = (BIGNUM_START_PTR (source));
  bignum_digit_type carry = 0;
  bignum_digit_type * scan_target = (BIGNUM_START_PTR (target));
  bignum_digit_type * end_source = (scan_source + (BIGNUM_LENGTH (source)));
  bignum_digit_type * end_target = (scan_target + (BIGNUM_LENGTH (target)));
  int shift_right = (BIGNUM_DIGIT_LENGTH - shift_left);
  bignum_digit_type mask = ((1L << shift_right) - 1);
  while (scan_source < end_source)
    {
      digit = (*scan_source++);
      (*scan_target++) = (((digit & mask) << shift_left) | carry);
      carry = (digit >> shift_right);
    }
  if (scan_target < end_target)
    (*scan_target) = carry;
  else
    BIGNUM_ASSERT (carry == 0);
  return;
}

/* This will also trim the number if necessary */
static bignum_type
bignum_destructive_unnormalization(bignum_type bignum, int shift_right)
{
  bignum_length_type length = BIGNUM_LENGTH(bignum);
  bignum_digit_type * start = (BIGNUM_START_PTR (bignum));
  bignum_digit_type * scan = (start + length);
  bignum_digit_type digit;
  bignum_digit_type carry = 0;
  int shift_left = (BIGNUM_DIGIT_LENGTH - shift_right);
  bignum_digit_type mask = ((1L << shift_right) - 1);
  
  while (!(*--scan)) {
    if (start == scan) { /* Don't bother. */
      BIGNUM_SET_HEADER (bignum, 0, 0);
      BIGNUM_REDUCE_LENGTH (bignum, bignum, 0);
      return bignum;
    }
    --length;
  }

  digit = (*scan);
  (*scan) = (digit >> shift_right);
  length -= (*scan == 0); /* Add 1 or 0 */
  carry = ((digit & mask) << shift_left);
  
  while (start < scan)
    {
      digit = (*--scan);
      (*scan) = ((digit >> shift_right) | carry);
      carry = ((digit & mask) << shift_left);
    }
  BIGNUM_ASSERT (carry == 0);
  BIGNUM_ASSERT (BIGNUM_LENGTH(bignum) != length);
  BIGNUM_ASSERT(length != 1 || BIGNUM_REF(bignum, 0) != 0);
  BIGNUM_SET_HEADER
    (bignum, length, (BIGNUM_NEGATIVE_P (bignum)));
  BIGNUM_REDUCE_LENGTH (bignum, bignum, length);

  return bignum;
}

/* This is a reduced version of the division algorithm, applied to the
   case of dividing two bignum digits by one bignum digit.  It is
   assumed that the numerator, denominator are normalized. */

#define BDD_STEP(qn, j)                                                 \
{                                                                       \
  uj = (u[j]);                                                          \
  if (uj != v1)                                                         \
    {                                                                   \
      uj_uj1 = (HD_CONS (uj, (u[j + 1])));                              \
      guess = (uj_uj1 / v1);                                            \
      comparand = (HD_CONS ((uj_uj1 % v1), (u[j + 2])));                \
    }                                                                   \
  else                                                                  \
    {                                                                   \
      guess = (BIGNUM_RADIX_ROOT - 1);                                  \
      comparand = (HD_CONS (((u[j + 1]) + v1), (u[j + 2])));            \
    }                                                                   \
  while ((guess * v2) > comparand)                                      \
    {                                                                   \
      guess -= 1;                                                       \
      comparand += (v1 << BIGNUM_HALF_DIGIT_LENGTH);                    \
      if (comparand >= BIGNUM_RADIX)                                    \
        break;                                                          \
    }                                                                   \
  qn = (bignum_digit_divide_subtract (v1, v2, guess, (&u[j])));         \
}

static bignum_digit_type
bignum_digit_divide(bignum_digit_type uh, bignum_digit_type ul,
                    bignum_digit_type v,
                    bignum_digit_type * q) /* return value */
{
  bignum_digit_type guess;
  bignum_digit_type comparand;
  bignum_digit_type v1;
  bignum_digit_type v2;
  bignum_digit_type uj;
  bignum_digit_type uj_uj1;
  bignum_digit_type q1;
  bignum_digit_type q2;
  bignum_digit_type u [4];
  if (uh == 0)
    {
      if (ul < v)
        {
          (*q) = 0;
          return (ul);
        }
      else if (ul == v)
        {
          (*q) = 1;
          return (0);
        }
    }
  (u[0]) = (HD_HIGH (uh));
  (u[1]) = (HD_LOW (uh));
  (u[2]) = (HD_HIGH (ul));
  (u[3]) = (HD_LOW (ul));
  v1 = (HD_HIGH (v));
  v2 = (HD_LOW (v));
  BDD_STEP (q1, 0);
  BDD_STEP (q2, 1);
  (*q) = (HD_CONS (q1, q2));
  return (HD_CONS ((u[2]), (u[3])));
}

#undef BDD_STEP

#define BDDS_MULSUB(vn, un, carry_in)                                   \
{                                                                       \
  product = ((vn * guess) + carry_in);                                  \
  diff = (un - (HD_LOW (product)));                                     \
  if (diff < 0)                                                         \
    {                                                                   \
      un = (diff + BIGNUM_RADIX_ROOT);                                  \
      carry = ((HD_HIGH (product)) + 1);                                \
    }                                                                   \
  else                                                                  \
    {                                                                   \
      un = diff;                                                        \
      carry = (HD_HIGH (product));                                      \
    }                                                                   \
}

#define BDDS_ADD(vn, un, carry_in)                                      \
{                                                                       \
  sum = (vn + un + carry_in);                                           \
  if (sum < BIGNUM_RADIX_ROOT)                                          \
    {                                                                   \
      un = sum;                                                         \
      carry = 0;                                                        \
    }                                                                   \
  else                                                                  \
    {                                                                   \
      un = (sum - BIGNUM_RADIX_ROOT);                                   \
      carry = 1;                                                        \
    }                                                                   \
}

static bignum_digit_type
bignum_digit_divide_subtract(bignum_digit_type v1, bignum_digit_type v2,
                             bignum_digit_type guess, bignum_digit_type * u)
{
  {
    bignum_digit_type product;
    bignum_digit_type diff;
    bignum_digit_type carry;
    BDDS_MULSUB (v2, (u[2]), 0);
    BDDS_MULSUB (v1, (u[1]), carry);
    if (carry == 0)
      return (guess);
    diff = ((u[0]) - carry);
    if (diff < 0)
      (u[0]) = (diff + BIGNUM_RADIX);
    else
      {
        (u[0]) = diff;
        return (guess);
      }
  }
  {
    bignum_digit_type sum;
    bignum_digit_type carry;
    BDDS_ADD(v2, (u[2]), 0);
    BDDS_ADD(v1, (u[1]), carry);
    if (carry == 1)
      (u[0]) += 1;
  }
  return (guess - 1);
}

#undef BDDS_MULSUB
#undef BDDS_ADD

static void
bignum_divide_unsigned_small_denominator(bignum_type numerator,
                                         bignum_digit_type denominator,
                                         bignum_type * quotient,
                                         bignum_type * remainder,
                                         int q_negative_p,  int r_negative_p)
{
  bignum_type q = (bignum_new_sign (numerator, q_negative_p));
  bignum_digit_type r = (bignum_destructive_scale_down (q, denominator));
  (*quotient) = (bignum_trim (q));
  if (remainder != ((bignum_type *) 0))
    (*remainder) = (bignum_digit_to_bignum (r, r_negative_p));
  return;
}

/* Given (denominator > 1), it is fairly easy to show that
   (quotient_high < BIGNUM_RADIX_ROOT), after which it is easy to see
   that all digits are < BIGNUM_RADIX. */

static bignum_digit_type
bignum_destructive_scale_down(bignum_type bignum, bignum_digit_type denominator)
{
  bignum_digit_type numerator;
  bignum_digit_type remainder = 0;
  bignum_digit_type two_digits;
#define quotient_high remainder
  bignum_digit_type * start = (BIGNUM_START_PTR (bignum));
  bignum_digit_type * scan = (start + (BIGNUM_LENGTH (bignum)));
  BIGNUM_ASSERT ((denominator > 1) && (denominator < BIGNUM_RADIX_ROOT));
  while (start < scan)
    {
      two_digits = (*--scan);
      numerator = (HD_CONS (remainder, (HD_HIGH (two_digits))));
      quotient_high = (numerator / denominator);
      numerator = (HD_CONS ((numerator % denominator), (HD_LOW (two_digits))));
      (*scan) = (HD_CONS (quotient_high, (numerator / denominator)));
      remainder = (numerator % denominator);
    }
  return (remainder);
#undef quotient_high
}

static bignum_type
bignum_remainder_unsigned_small_denominator(bignum_type n, bignum_digit_type d,
                                            int negative_p)
{
  bignum_digit_type two_digits;
  bignum_digit_type * start = (BIGNUM_START_PTR (n));
  bignum_digit_type * scan = (start + (BIGNUM_LENGTH (n)));
  bignum_digit_type r = 0;
  BIGNUM_ASSERT ((d > 1) && (d < BIGNUM_RADIX_ROOT));
  while (start < scan)
    {
      two_digits = (*--scan);
      r =
        ((HD_CONS (((HD_CONS (r, (HD_HIGH (two_digits)))) % d),
                   (HD_LOW (two_digits))))
         % d);
    }
  return (bignum_digit_to_bignum (r, negative_p));
}

static void
fix_neg(C_word c, C_word self, C_word k, C_word x)
{
  x = C_unfix(x);
  /* This code "knows" that bignums have 2 "reserved" bits, like fixnums */
  if (x != C_MOST_NEGATIVE_FIXNUM) { /* C_fitsinfixnump(x) */
    C_kontinue(k, C_fix(-x));
  } else {
    bignum_digit_type *ref;
    bignum_type big;
    big = bignum_allocate(2, 0);
    ref = BIGNUM_START_PTR(big);
    *ref++ = 0;
    *ref = 1;
    C_return_bignum(k, big);
  }
}

static void
big_neg(C_word c, C_word self, C_word k, C_word x)
{
  bignum_type big = big_of(x);
  C_return_bignum(k, bignum_new_sign(big, !(BIGNUM_NEGATIVE_P (big))));
}

static C_word
big_comp_big(C_word x, C_word y)
{
  bignum_type bigx = big_of(x), bigy = big_of(y);
  return C_fix((BIGNUM_NEGATIVE_P (bigx))
               ? ((BIGNUM_NEGATIVE_P (bigy))
                  ? (bignum_compare_unsigned (bigy, bigx))
                  : (bignum_comparison_less))
               : ((BIGNUM_NEGATIVE_P (bigy))
                  ? (bignum_comparison_greater)
                  : (bignum_compare_unsigned (bigx, bigy))));
}

static enum bignum_comparison
bignum_compare_unsigned(bignum_type x, bignum_type y)
{
  bignum_length_type x_length;
  bignum_length_type y_length;
  if (x == y) /* Objects are the same? */
    return (bignum_comparison_equal);

  x_length = (BIGNUM_LENGTH (x));
  y_length = (BIGNUM_LENGTH (y));
  if (x_length < y_length)
    return (bignum_comparison_less);
  if (x_length > y_length)
    return (bignum_comparison_greater);
  {
    bignum_digit_type * start_x = (BIGNUM_START_PTR (x));
    bignum_digit_type * scan_x = (start_x + x_length);
    bignum_digit_type * scan_y = ((BIGNUM_START_PTR (y)) + y_length);
    while (start_x < scan_x)
      {
        bignum_digit_type digit_x = (*--scan_x);
        bignum_digit_type digit_y = (*--scan_y);
        if (digit_x < digit_y)
          return (bignum_comparison_less);
        if (digit_x > digit_y)
          return (bignum_comparison_greater);
      }
  }
  return (bignum_comparison_equal);
}

static void
big_abs(C_word c, C_word self, C_word k, C_word big)
{
  if (!BIGNUM_NEGATIVE_P(big_of(big)))
    C_kontinue(k, big);
  else
    C_return_bignum(k, bignum_new_sign(big_of(big), 0));
}

static void
big_quotient_fix(C_word c, C_word self, C_word k, C_word x, C_word y)
{
  bignum_type bigx = big_of(x);
  y = C_unfix(y);

  if (y == 1)
    C_kontinue(k, x);
  else if (y == -1)
    C_return_bignum(k, bignum_new_sign(bigx, !(BIGNUM_NEGATIVE_P(bigx))));

  /* Too bad, we really need to do some work... */
  {
    int neg_p = (y < 0) ? !(BIGNUM_NEGATIVE_P(bigx)) : BIGNUM_NEGATIVE_P(bigx);
    bignum_digit_type abs_y = (y < 0) ? -y : y;
    bignum_type quotient;
    
    if (y == C_MOST_NEGATIVE_FIXNUM) {
      if (!BIGNUM_NEGATIVE_P(bigx) && BIGNUM_LENGTH(bigx) == 1
          && BIGNUM_REF(bigx, 1) == 1 && BIGNUM_REF(bigx, 0) == 0) {
        /*
         * Very very special case:
         * quotient(MOST_NEGATIVE_FIXNUM, -(MOST_NEGATIVE_FIXNUM)) => -1
         */
        C_kontinue(k, C_fix(-1));
      } else {
        /* This is the only case we need to go allocate a bignum for */
        bignum_type bigy =
	  bignum_allocate_from_fixnum(C_fix(C_MOST_NEGATIVE_FIXNUM));

        bignum_divide_unsigned_large_denominator
          (bigx, bigy, (&quotient), ((bignum_type *) 0), neg_p, 0);
        BIGNUM_DEALLOCATE(bigy);
        C_return_bignum(k, quotient);
      }
    } else if (abs_y < BIGNUM_RADIX_ROOT) {
      bignum_divide_unsigned_small_denominator
        (bigx, abs_y, (&quotient), ((bignum_type *) 0), neg_p, 0);
      C_return_bignum(k, quotient);
    } else {
      bignum_divide_unsigned_medium_denominator
        (bigx, abs_y, (&quotient), ((bignum_type *) 0), neg_p, 0);
      C_return_bignum(k, quotient);
    }
  }
}

static void
big_quotient_big(C_word c, C_word self, C_word k, C_word x, C_word y)
{
  bignum_type numerator = big_of(x);
  bignum_type denominator = big_of(y);
  int neg_p = ((BIGNUM_NEGATIVE_P (denominator))
               ? (! (BIGNUM_NEGATIVE_P (numerator)))
               : (BIGNUM_NEGATIVE_P (numerator)));

  switch (bignum_compare_unsigned (numerator, denominator))
    {
    case bignum_comparison_equal:
      C_kontinue(k, neg_p ? C_fix(-1) : C_fix(1));
    case bignum_comparison_less:
      C_kontinue(k, C_fix(0));
    case bignum_comparison_greater:
    default:                                  /* to appease gcc -Wall */
      {
        bignum_type quotient;
        BIGNUM_ASSERT(BIGNUM_LENGTH(denominator) > 1);
        bignum_divide_unsigned_large_denominator
          (numerator, denominator, (&quotient), ((bignum_type *) 0), neg_p, 0);
        C_return_bignum(k, quotient);
      }
    }
}

static void
big_remainder_fix(C_word c, C_word self, C_word k, C_word x, C_word y)
{
   bignum_type remainder, bigy, bigx;
   int x_neg, y_neg;
   C_word abs_y;

   switch (y) {
   /* case 0:  SHOULD NOT HAPPEN (checked in Scheme)
     C_kontinue(k, C_SCHEME_FALSE); */
   case C_fix(1):
   case C_fix(-1):
     C_kontinue(k, C_fix(0));
   case C_fix(C_MOST_NEGATIVE_FIXNUM):
     bigx = big_of(x);
     if (BIGNUM_LENGTH(bigx) == 2 && BIGNUM_REF(bigx, 0) == 0 && BIGNUM_REF(bigx, 0))
       C_kontinue(k, C_fix(0));
     /* Don't handle 0 <= length(bigx) <= 1 since then it should be a fixnum */
     BIGNUM_ASSERT(BIGNUM_LENGTH(bigx) >= 2);
     
     bigy = bignum_allocate_from_fixnum(y);
     bignum_divide_unsigned_large_denominator
      (bigx, bigy, (bignum_type *)0, &remainder, 0, BIGNUM_NEGATIVE_P(bigx));
     BIGNUM_DEALLOCATE(bigy);
     
     C_return_bignum(k, remainder);
   default:
     bigx = big_of(x);
     y = C_unfix(y);
     y_neg = (y < 0);
     abs_y = y_neg ? -y : y;
     x_neg = BIGNUM_NEGATIVE_P(bigx);
     
     if (abs_y < BIGNUM_RADIX_ROOT)
       remainder =
         bignum_remainder_unsigned_small_denominator(bigx, abs_y, x_neg);
     else
       bignum_divide_unsigned_medium_denominator(bigx, abs_y,
                                                 (bignum_type *) 0, &remainder,
						 x_neg, x_neg);
     C_return_bignum(k, remainder);
   }
}

static void
big_remainder_big(C_word c, C_word self, C_word k, C_word x, C_word y)
{
  bignum_type numerator = big_of(x), denominator = big_of(y);
  
  switch (bignum_compare_unsigned (numerator, denominator))
    {
      case bignum_comparison_equal:
        C_kontinue(k, C_fix(0));
      case bignum_comparison_less:
        C_kontinue(k, x);
      case bignum_comparison_greater:
      default:                                  /* to appease gcc -Wall */
        {
          bignum_type remainder;
          bignum_divide_unsigned_large_denominator
            (numerator, denominator,
             ((bignum_type *) 0), (&remainder),
             0, BIGNUM_NEGATIVE_P(numerator));
          C_return_bignum(k, remainder);
        }
    }
}

static void
flo_to_int(C_word c, C_word self, C_word k, C_word x)
{
  C_return_bignum(k, double_to_bignum(C_flonum_magnitude(x)));
}

#define DTB_WRITE_DIGIT(factor)                                         \
{                                                                       \
  significand *= (factor);                                              \
  digit = ((bignum_digit_type) significand);                            \
  (*--scan) = digit;                                                    \
  significand -= ((double) digit);                                      \
}

static bignum_type
double_to_bignum(double x)
{
  int exponent;
  double significand = (frexp (x, (&exponent)));
  if (exponent <= 0) return (BIGNUM_ZERO ());
  if (exponent == 1) return (BIGNUM_ONE (x < 0));
  if (significand < 0) significand = (-significand);
  {
    bignum_length_type length = (BIGNUM_BITS_TO_DIGITS (exponent));
    bignum_type result = (bignum_allocate (length, (x < 0)));
    bignum_digit_type * start = (BIGNUM_START_PTR (result));
    bignum_digit_type * scan = (start + length);
    bignum_digit_type digit;
    int odd_bits = (exponent % BIGNUM_DIGIT_LENGTH);
    if (odd_bits > 0)
      DTB_WRITE_DIGIT (1L << odd_bits);
    while (start < scan)
      {
        if (significand == 0)
          {
            while (start < scan)
              (*--scan) = 0;
            break;
          }
        DTB_WRITE_DIGIT (BIGNUM_RADIX);
      }
    return (result);
  }
}

#undef DTB_WRITE_DIGIT

static void 
int_bitwise_int(C_word c, C_word self, C_word k, C_word op, C_word x, C_word y)
{
  bignum_type bigx, bigy, result;
  
  if((x & C_FIXNUM_BIT) != 0)
    bigx = bignum_allocate_from_fixnum(x);
  else
    bigx = big_of(x);

  if((y & C_FIXNUM_BIT) != 0)
    bigy = bignum_allocate_from_fixnum(y);
  else
    bigy = big_of(y);

  result = ((BIGNUM_NEGATIVE_P (bigx))
             ? (BIGNUM_NEGATIVE_P (bigy))
               ? bignum_negneg_bitwise_op(C_unfix(op), bigx, bigy)
               : bignum_posneg_bitwise_op(C_unfix(op), bigy, bigx)
             : (BIGNUM_NEGATIVE_P (bigy))
               ? bignum_posneg_bitwise_op(C_unfix(op), bigx, bigy)
               : bignum_pospos_bitwise_op(C_unfix(op), bigx, bigy));
        
  if((x & C_FIXNUM_BIT) != 0)
    BIGNUM_DEALLOCATE(bigx);
  if((y & C_FIXNUM_BIT) != 0)
    BIGNUM_DEALLOCATE(bigy);

  C_return_bignum(k, result);
}

static void 
int_not(C_word c, C_word self, C_word k, C_word x)
{
  bignum_type bigx, result;
  
  if((x & C_FIXNUM_BIT) != 0)
    bigx = bignum_allocate_from_fixnum(x);
  else
    bigx = big_of(x);

  result = bignum_bitwise_not(bigx);

  if((x & C_FIXNUM_BIT) != 0)
    BIGNUM_DEALLOCATE(bigx);

  C_return_bignum(k, result);
}

bignum_type
bignum_bitwise_not(bignum_type x)
{
  static C_word invbits[] = { BIGNUM_RADIX | 1, 1 }; /* bignum representing -1 */
  return bignum_subtract((bignum_type)invbits, x);
}

/*
 * From Hacker's Delight by Frank Warren
 * based on a modified nlz() from section 5-3 (fig. 5-7)
 */
static C_word
ilen(C_uword x) {
  C_uword y;
  C_word n = 0;

#ifdef C_SIXTY_FOUR
  y = x >> 32; if (y != 0) { n += 32; x = y; }
#endif
  y = x >> 16; if (y != 0) { n += 16; x = y; }
  y = x >>  8; if (y != 0) { n +=  8; x = y; }
  y = x >>  4; if (y != 0) { n +=  4; x = y; }
  y = x >>  2; if (y != 0) { n +=  2; x = y; }
  y = x >>  1; if (y != 0) return n + 2;
  return n + x;
}

static void
int_length(C_word c, C_word self, C_word k, C_word x)
{
  if (x & C_FIXNUM_BIT) {
    x = C_unfix(x);
    C_kontinue(k, C_fix(ilen((x < 0) ? ~x : x)));
  } else { /* bignum */
    bignum_type bigx;
    bignum_digit_type bd;
    int len_1;

    bigx = big_of(x);
    if (BIGNUM_NEGATIVE_P(bigx)) {
      /* TODO: This can probably be done faster, by juggling with borrow */
      bignum_type notx = bignum_bitwise_not(bigx);
      len_1 = BIGNUM_LENGTH(notx) - 1;
      bd = *(BIGNUM_START_PTR(notx) + len_1); /* Most significant digit */
      BIGNUM_DEALLOCATE(notx);
    } else {
      len_1 = BIGNUM_LENGTH(bigx) - 1;
      bd = *(BIGNUM_START_PTR(bigx) + len_1); /* Most significant digit */
    }
    C_kontinue(k, C_fix(ilen(bd) + len_1 * BIGNUM_DIGIT_LENGTH));
  }
}

static void 
int_shift_fix(C_word c, C_word self, C_word k, C_word x, C_word y)
{
  bignum_type bigx, result;

  if (y == C_fix(0)) C_kontinue(k, x); /* Done too (no shift) */
  
  /* Ensure x is a bignum */
  if((x & C_FIXNUM_BIT) != 0) {
    if (x == C_fix(0)) C_kontinue(k, x); /* Skip everything else */
    else bigx = bignum_allocate_from_fixnum(x);
  } else {
    bigx = big_of(x);
  }
   
  result = bignum_arithmetic_shift(bigx, C_unfix(y));
  if ((x & C_FIXNUM_BIT) != 0)
    BIGNUM_DEALLOCATE(bigx);
  C_return_bignum(k, result);
}

static bignum_type
bignum_arithmetic_shift(bignum_type arg1, C_word n)
{
  bignum_type tmp1, tmp2, result;
  if (BIGNUM_NEGATIVE_P(arg1) && n < 0) {
    tmp1 = bignum_bitwise_not(arg1);
    tmp2 = bignum_magnitude_ash(tmp1, n);
    BIGNUM_DEALLOCATE(tmp1);
    result = bignum_bitwise_not(tmp2);
    BIGNUM_DEALLOCATE(tmp2);
    return result;
  } else {
    return bignum_magnitude_ash(arg1, n);
  }
}

static bignum_type
bignum_magnitude_ash(bignum_type arg1, C_word n)
{
  bignum_type result;
  bignum_digit_type *scan1;
  bignum_digit_type *scanr;
  bignum_digit_type *end;

  C_word digit_offset,bit_offset;

  if (n > 0) {
    digit_offset = n / BIGNUM_DIGIT_LENGTH;
    bit_offset =   n % BIGNUM_DIGIT_LENGTH;
    
    result = bignum_allocate_zeroed (BIGNUM_LENGTH (arg1) + digit_offset + 1,
                                     BIGNUM_NEGATIVE_P(arg1));

    scanr = BIGNUM_START_PTR (result) + digit_offset;
    scan1 = BIGNUM_START_PTR (arg1);
    end = scan1 + BIGNUM_LENGTH (arg1);
    
    while (scan1 < end) {
      *scanr = *scanr | (*scan1 & BIGNUM_DIGIT_MASK) << bit_offset;
      *scanr = *scanr & BIGNUM_DIGIT_MASK;
      scanr++;
      *scanr = *scan1++ >> (BIGNUM_DIGIT_LENGTH - bit_offset);
      *scanr = *scanr & BIGNUM_DIGIT_MASK;
    }
  }
  else if (n < 0
           && (-n >= (BIGNUM_LENGTH (arg1) * (bignum_length_type) BIGNUM_DIGIT_LENGTH)))
    result = BIGNUM_ZERO ();

  else if (n < 0) {
    digit_offset = -n / BIGNUM_DIGIT_LENGTH;
    bit_offset =   -n % BIGNUM_DIGIT_LENGTH;
    
    result = bignum_allocate_zeroed (BIGNUM_LENGTH (arg1) - digit_offset,
                                     BIGNUM_NEGATIVE_P(arg1));
    
    scanr = BIGNUM_START_PTR (result);
    scan1 = BIGNUM_START_PTR (arg1) + digit_offset;
    end = scanr + BIGNUM_LENGTH (result) - 1;
    
    while (scanr < end) {
      *scanr =  (*scan1++ & BIGNUM_DIGIT_MASK) >> bit_offset ;
      *scanr = (*scanr | 
        *scan1 << (BIGNUM_DIGIT_LENGTH - bit_offset)) & BIGNUM_DIGIT_MASK;
      scanr++;
    }
    *scanr =  (*scan1++ & BIGNUM_DIGIT_MASK) >> bit_offset ;
  }
  
  return (bignum_trim (result));
}

static bignum_type
bignum_pospos_bitwise_op(int op, bignum_type arg1, bignum_type arg2)
{
  bignum_type result;
  bignum_length_type max_length;

  bignum_digit_type *scan1, *end1, digit1;
  bignum_digit_type *scan2, *end2, digit2;
  bignum_digit_type *scanr, *endr;

  max_length =  (BIGNUM_LENGTH(arg1) > BIGNUM_LENGTH(arg2))
               ? BIGNUM_LENGTH(arg1) : BIGNUM_LENGTH(arg2);

  result = bignum_allocate(max_length, 0);

  scanr = BIGNUM_START_PTR(result);
  scan1 = BIGNUM_START_PTR(arg1);
  scan2 = BIGNUM_START_PTR(arg2);
  endr = scanr + max_length;
  end1 = scan1 + BIGNUM_LENGTH(arg1);
  end2 = scan2 + BIGNUM_LENGTH(arg2);

  while (scanr < endr) {
    digit1 = (scan1 < end1) ? *scan1++ : 0;
    digit2 = (scan2 < end2) ? *scan2++ : 0;
    /*
    fprintf(stderr, "[pospos op = %d, i = %ld, d1 = %lx, d2 = %lx]\n",
            op, endr - scanr, digit1, digit2);
            */
    *scanr++ = (op == bignum_and_op) ? digit1 & digit2 :
               (op == bignum_ior_op) ? digit1 | digit2 :
                                       digit1 ^ digit2;
  }
  return bignum_trim(result);
}

static bignum_type
bignum_posneg_bitwise_op(int op, bignum_type arg1, bignum_type arg2)
{
  bignum_type result;
  bignum_length_type max_length;

  bignum_digit_type *scan1, *end1, digit1;
  bignum_digit_type *scan2, *end2, digit2, carry2;
  bignum_digit_type *scanr, *endr;

  char neg_p = op == bignum_ior_op || op == bignum_xor_op;

  max_length =  (BIGNUM_LENGTH(arg1) > BIGNUM_LENGTH(arg2) + 1)
               ? BIGNUM_LENGTH(arg1) : BIGNUM_LENGTH(arg2) + 1;

  result = bignum_allocate(max_length, neg_p);

  scanr = BIGNUM_START_PTR(result);
  scan1 = BIGNUM_START_PTR(arg1);
  scan2 = BIGNUM_START_PTR(arg2);
  endr = scanr + max_length;
  end1 = scan1 + BIGNUM_LENGTH(arg1);
  end2 = scan2 + BIGNUM_LENGTH(arg2);

  carry2 = 1;

  while (scanr < endr) {
    digit1 = (scan1 < end1) ? *scan1++ : 0;
    digit2 = (~((scan2 < end2) ? *scan2++ : 0) & BIGNUM_DIGIT_MASK)
             + carry2;

    if (digit2 < BIGNUM_RADIX)
      carry2 = 0;
    else
      {
        digit2 = (digit2 - BIGNUM_RADIX);
        carry2 = 1;
      }
    
    *scanr++ = (op == bignum_and_op) ? digit1 & digit2 :
               (op == bignum_ior_op) ? digit1 | digit2 :
                                       digit1 ^ digit2;
  }
  
  if (neg_p)
    bignum_negate_magnitude(result);

  return bignum_trim(result);
}

static bignum_type
bignum_negneg_bitwise_op(int op, bignum_type arg1, bignum_type arg2)
{
  bignum_type result;
  bignum_length_type max_length;

  bignum_digit_type *scan1, *end1, digit1, carry1;
  bignum_digit_type *scan2, *end2, digit2, carry2;
  bignum_digit_type *scanr, *endr;

  char neg_p = op == bignum_and_op || op == bignum_ior_op;

  max_length =  (BIGNUM_LENGTH(arg1) > BIGNUM_LENGTH(arg2))
               ? BIGNUM_LENGTH(arg1) + 1 : BIGNUM_LENGTH(arg2) + 1;

  result = bignum_allocate(max_length, neg_p);

  scanr = BIGNUM_START_PTR(result);
  scan1 = BIGNUM_START_PTR(arg1);
  scan2 = BIGNUM_START_PTR(arg2);
  endr = scanr + max_length;
  end1 = scan1 + BIGNUM_LENGTH(arg1);
  end2 = scan2 + BIGNUM_LENGTH(arg2);

  carry1 = 1;
  carry2 = 1;

  while (scanr < endr) {
    digit1 = (~((scan1 < end1) ? *scan1++ : 0) & BIGNUM_DIGIT_MASK) + carry1;
    digit2 = (~((scan2 < end2) ? *scan2++ : 0) & BIGNUM_DIGIT_MASK) + carry2;

    if (digit1 < BIGNUM_RADIX)
      carry1 = 0;
    else
      {
        digit1 = (digit1 - BIGNUM_RADIX);
        carry1 = 1;
      }
    
    if (digit2 < BIGNUM_RADIX)
      carry2 = 0;
    else
      {
        digit2 = (digit2 - BIGNUM_RADIX);
        carry2 = 1;
      }
    
    *scanr++ = (op == bignum_and_op) ? digit1 & digit2 :
               (op == bignum_ior_op) ? digit1 | digit2 :
                                       digit1 ^ digit2;
  }

  if (neg_p)
    bignum_negate_magnitude(result);

  return bignum_trim(result);
}

static void
bignum_negate_magnitude(bignum_type arg)
{
  bignum_digit_type *scan;
  bignum_digit_type *end;
  bignum_digit_type digit;
  bignum_digit_type carry;

  scan = BIGNUM_START_PTR(arg);
  end = scan + BIGNUM_LENGTH(arg);

  carry = 1;

  while (scan < end) {
    digit = (~*scan & BIGNUM_DIGIT_MASK) + carry;

    if (digit < BIGNUM_RADIX)
      carry = 0;
    else
      {
        digit = (digit - BIGNUM_RADIX);
        carry = 1;
      }
    
    *scan++ = digit;
  }
}

#define BIGNUM_STR_BLOCK_SIZE 8

/*
 * Contains s48_bignum_to_digit_stream.  We may want to separate that out
 * again in case of huge numbers; it may be more efficient to write
 * those straight to files without going through the string conversion.
 */
static void
big_to_string(C_word c, C_word self, C_word k, C_word value, C_word radix)
{
  char *buf, *index;
  C_word len, counter=0;
  char *characters = "0123456789abcdef";
  C_word *tmp, ret;
  bignum_type bignum;

  bignum = big_of(value);
  len = BIGNUM_STR_BLOCK_SIZE;
  buf = C_malloc(len);
  if (buf == NULL) {
    fprintf(stderr, "out of memory - can not allocate string");
    exit(EXIT_FAILURE);
  }
  index = buf + len - 1;

  if (BIGNUM_NEGATIVE_P(bignum)) {
    *index-- = '-';
    counter++;
  }

  radix = C_unfix(radix);
  
  BIGNUM_ASSERT ((radix > 1) && (radix <= BIGNUM_RADIX_ROOT));
  if (! (BIGNUM_ZERO_P (bignum)))
    {
      bignum_type working_copy = (bignum_copy (bignum));
      bignum_digit_type * start = (BIGNUM_START_PTR (working_copy));
      bignum_digit_type * scan = (start + (BIGNUM_LENGTH (working_copy)));
      bignum_digit_type digit;
      while (start < scan)
        {
          if ((scan[-1]) == 0) {
            scan -= 1;
          } else {
            digit = bignum_destructive_scale_down (working_copy, radix);
            *index-- = characters[digit];
            if (++counter == len) {
              char *newbuf;
              newbuf = C_malloc(len + BIGNUM_STR_BLOCK_SIZE);
              if (newbuf == NULL) {
                fprintf(stderr, "out of memory - can not allocate string");
                exit(EXIT_FAILURE);
              }
              C_memcpy(newbuf + BIGNUM_STR_BLOCK_SIZE, buf, len);
              C_free(buf);
              buf = newbuf;
              index = newbuf + BIGNUM_STR_BLOCK_SIZE - 1;
              len += BIGNUM_STR_BLOCK_SIZE;
            }
          }
        }
      BIGNUM_DEALLOCATE (working_copy);
    }

  if (BIGNUM_NEGATIVE_P(bignum))
    *index-- = '-';

  tmp = C_alloc(C_SIZEOF_STRING(counter));
  ret = C_string(&tmp, counter, index+1);
  C_free(buf);
  C_kontinue(k, ret);
}

static void
digits_to_big(C_word c, C_word self, C_word k, C_word n,
	      C_word start, C_word end, C_word radix, C_word negp)
{
  char *str;
  size_t n_digits;
  int negative_p = (negp != C_SCHEME_FALSE);
  int digit;
  int hash = 0;

  str = C_c_string(n) + C_unfix(start);
  n_digits = C_unfix(end)-C_unfix(start);
  radix = C_unfix(radix);
  hash = /* abs(radix / 2) */ 0;
  
#define DIGIT_TO_INT(x)         \
  (((x) == '#') ? hash :        \
   (((x) >= (int)'a') ?((x) - (int)'a' + 10) : ((x) - (int)'0')))

  BIGNUM_ASSERT ((radix > 1) && (radix < BIGNUM_RADIX_ROOT));
  if (n_digits == 0)
    C_kontinue(k, C_SCHEME_FALSE);
  if (n_digits == 1)
    {
      digit = DIGIT_TO_INT(C_tolower((int)*str));
      if (digit >= radix || digit < 0)
        C_kontinue(k, C_SCHEME_FALSE);
      else
        C_return_bignum(k, bignum_digit_to_bignum(digit, negative_p));
    }
  {
    bignum_length_type length;
    {
      unsigned int radix_copy = radix;
      unsigned int log_radix = 0;
      while (radix_copy > 0)
        {
          radix_copy >>= 1;
          log_radix += 1;
        }
      /* This length will be at least as large as needed. */
      length = (BIGNUM_BITS_TO_DIGITS (n_digits * log_radix));
    }
    {
      bignum_type result = (bignum_allocate_zeroed (length, negative_p));
      while ((n_digits--) > 0)
        {
          digit = DIGIT_TO_INT(C_tolower((int)*str));
          str++;
          if (digit >= radix || digit < 0) {
            BIGNUM_DEALLOCATE(result);
            C_kontinue(k, C_SCHEME_FALSE);
          }
        
          bignum_destructive_scale_up (result, ((bignum_digit_type) radix));
          bignum_destructive_add (result, digit);
        }
      C_return_bignum (k, bignum_trim (result));
    }
  }
}
