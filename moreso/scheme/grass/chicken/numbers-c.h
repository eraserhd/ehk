/* numbers-c.h */

#include <limits.h>
#include <math.h> /* frexp() */

#define FIX    0
#define FLO    1
#define BIG    2
#define RAT    3
#define COMP   4
#define NONE   5

#define BIG_TAG       0
#define RAT_TAG       1
#define COMP_TAG      2

#define BIG_FREE      3

#define FORCE_FINALIZERS     4

enum bignum_bitwise_ops
{
  bignum_and_op,
  bignum_ior_op,
  bignum_xor_op
};

enum bignum_comparison
{
  bignum_comparison_equal = 0,
  bignum_comparison_less = -1,
  bignum_comparison_greater = 1
};

typedef void * bignum_type;
typedef C_word bignum_digit_type;
typedef C_word bignum_length_type;

/* Internal bignum interface */
static bignum_type bignum_allocate(bignum_length_type, int);
static bignum_type bignum_allocate_zeroed(bignum_length_type, int);
static bignum_type shorten_bignum(bignum_type, bignum_length_type);
static bignum_type bignum_trim(bignum_type);
static bignum_type bignum_copy(bignum_type);
static void bignum_destructive_copy(bignum_type, bignum_type);
static bignum_type bignum_new_sign(bignum_type, int);
static bignum_type bignum_add(bignum_type, bignum_type);
static bignum_type bignum_add_unsigned(bignum_type, bignum_type, int);
static bignum_type bignum_subtract(bignum_type, bignum_type);
static bignum_type bignum_subtract_unsigned(bignum_type, bignum_type);
static bignum_type bignum_multiply_unsigned(bignum_type, bignum_type, int);
static bignum_type bignum_multiply_unsigned_small_factor(bignum_type,
                                                         bignum_digit_type, int);
static void bignum_destructive_scale_up(bignum_type, bignum_digit_type);
static void bignum_destructive_add(bignum_type, bignum_digit_type);
static void bignum_divide_unsigned_large_denominator(bignum_type, bignum_type,
                                                     bignum_type *,
                                                     bignum_type *, int, int);
static void bignum_divide_unsigned_normalized(bignum_type, bignum_type,
                                              bignum_type);
static bignum_digit_type bignum_divide_subtract(bignum_digit_type *,
                                                bignum_digit_type *,
                                                bignum_digit_type,
                                                bignum_digit_type *);
static void bignum_divide_unsigned_medium_denominator(bignum_type,
                                                      bignum_digit_type,
                                                      bignum_type *,
                                                      bignum_type *,
                                                      int, int);
static void bignum_destructive_normalization(bignum_type, bignum_type, int);
static bignum_type bignum_destructive_unnormalization(bignum_type, int);
static bignum_digit_type bignum_digit_divide(bignum_digit_type,
                                             bignum_digit_type,
                                             bignum_digit_type,
                                             bignum_digit_type *);
static bignum_digit_type bignum_digit_divide_subtract(bignum_digit_type,
                                                      bignum_digit_type,
                                                      bignum_digit_type,
                                                      bignum_digit_type *);
static void bignum_divide_unsigned_small_denominator(bignum_type,
                                                     bignum_digit_type,
                                                     bignum_type *,
                                                     bignum_type *,
                                                     int, int);
static bignum_digit_type bignum_destructive_scale_down(bignum_type,
                                                       bignum_digit_type);
static bignum_type bignum_remainder_unsigned_small_denominator(bignum_type,
                                                               bignum_digit_type,
                                                               int);
static enum bignum_comparison bignum_compare_unsigned(bignum_type, bignum_type);
static bignum_type double_to_bignum(double);
static bignum_type bignum_bitwise_and(bignum_type, bignum_type);
static bignum_type bignum_bitwise_ior(bignum_type, bignum_type);
static bignum_type bignum_bitwise_xor(bignum_type, bignum_type);
static bignum_type bignum_bitwise_not(bignum_type);
static bignum_type bignum_arithmetic_shift(bignum_type, C_word);
static bignum_type bignum_magnitude_ash(bignum_type, C_word);
static bignum_type bignum_pospos_bitwise_op(int, bignum_type, bignum_type);
static bignum_type bignum_posneg_bitwise_op(int, bignum_type, bignum_type);
static bignum_type bignum_negneg_bitwise_op(int, bignum_type, bignum_type);
static void bignum_negate_magnitude(bignum_type);

#define BIGNUM_OUT_OF_BAND NULL

/* BIGNUM_TO_POINTER casts a bignum object to a digit array pointer. */
#define BIGNUM_TO_POINTER(bignum) ((bignum_digit_type *) (bignum))

/* BIGNUM_REDUCE_LENGTH allows the memory system to reclaim some
   space when a bignum's length is reduced from its original value. */
#define BIGNUM_REDUCE_LENGTH(target, source, length)                    \
     target = shorten_bignum(source, length)

#define BIGNUM_DEALLOCATE(b) (C_free((void *)b))

/* CHAR_BIT is from <limits.h>, and it equals the number of bits in a char */
#define BIGNUM_DIGIT_LENGTH (((sizeof (bignum_digit_type)) * CHAR_BIT) - 2)
#define BIGNUM_HALF_DIGIT_LENGTH (BIGNUM_DIGIT_LENGTH / 2)
/* Radix = highest bit of header word: 1 if number negative, 0 if positive */
#define BIGNUM_RADIX (((C_uword) 1) << BIGNUM_DIGIT_LENGTH)
#define BIGNUM_RADIX_ROOT (((C_uword) 1) << BIGNUM_HALF_DIGIT_LENGTH)
#define BIGNUM_DIGIT_MASK        (BIGNUM_RADIX - 1)
#define BIGNUM_HALF_DIGIT_MASK   (BIGNUM_RADIX_ROOT - 1)

#define BIGNUM_START_PTR(bignum)                                        \
  ((BIGNUM_TO_POINTER (bignum)) + 1)

#define BIGNUM_SET_HEADER(bignum, length, negative_p)                   \
  (* (BIGNUM_TO_POINTER (bignum))) =                                    \
    ((length) | ((negative_p) ? BIGNUM_RADIX : 0))

#define BIGNUM_LENGTH(bignum)                                           \
  ((* (BIGNUM_TO_POINTER (bignum))) & ((bignum_length_type) BIGNUM_DIGIT_MASK))

#define BIGNUM_NEGATIVE_P(bignum)                                       \
  (((* (BIGNUM_TO_POINTER (bignum))) & BIGNUM_RADIX) != 0)

#define BIGNUM_ZERO_P(bignum)                                           \
  ((BIGNUM_LENGTH (bignum)) == 0)

#define BIGNUM_REF(bignum, index)                                       \
  (* ((BIGNUM_START_PTR (bignum)) + (index)))


/* These definitions are here to facilitate caching of the constants
   0, 1, and -1. */
/*
 * We don't cache because it complicates the conversion to fixnum code
 * since it would need additional checks before freeing the bignum.
 * Most cases where BIGNUM_ONE/ZERO are returned are removed anyway.
 */
#define BIGNUM_ZERO() (bignum_digit_to_bignum(0, 0))
#define BIGNUM_ONE(neg_p) (bignum_digit_to_bignum(1, neg_p))

#define HD_LOW(digit) ((digit) & BIGNUM_HALF_DIGIT_MASK)
#define HD_HIGH(digit) ((digit) >> BIGNUM_HALF_DIGIT_LENGTH)
#define HD_CONS(high, low) (((high) << BIGNUM_HALF_DIGIT_LENGTH) | (low))

#define BIGNUM_BITS_TO_DIGITS(n)                                        \
  (((n) + (BIGNUM_DIGIT_LENGTH - 1)) / BIGNUM_DIGIT_LENGTH)

#ifndef BIGNUM_DISABLE_ASSERTION_CHECKS

#define BIGNUM_EXCEPTION abort

#define BIGNUM_ASSERT(expression)                                       \
{                                                                       \
  if (! (expression))                                                   \
    BIGNUM_EXCEPTION ();                                                \
}

#else

#define BIGNUM_ASSERT(expression)

#endif /* BIGNUM_DISABLE_ASSERTION_CHECKS */

/* Chicken-specific */
#define BIGNUM_FITS_IN_FIXNUM_P(b)                                      \
  ((BIGNUM_LENGTH(b)) == 0 || (BIGNUM_LENGTH(b)) == 1 && BIGNUM_REF())
