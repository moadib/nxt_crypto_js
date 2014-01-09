// Long class from Google Closure Library (Apache license)
// https://code.google.com/p/closure-library/source/browse/closure/goog/math/long.js

/**
 * @fileoverview Defines a Long class for representing a 64-bit two's-complement
 * integer value, which faithfully simulates the behavior of a Java "long". This
 * implementation is derived from LongLib in GWT.
 *
 */

/**
 * Constructs a 64-bit two's-complement integer, given its low and high 32-bit
 * values as *signed* integers.  See the from* functions below for more
 * convenient ways of constructing Longs.
 *
 * The internal representation of a long is the two given signed, 32-bit values.
 * We use 32-bit pieces because these are the size of integers on which
 * Javascript performs bit-operations.  For operations like addition and
 * multiplication, we split each number into 16-bit pieces, which can easily be
 * multiplied within Javascript's floating-point representation without overflow
 * or change in sign.
 *
 * In the algorithms below, we frequently reduce the negative case to the
 * positive case by negating the input(s) and then post-processing the result.
 * Note that we must ALWAYS check specially whether those values are MIN_VALUE
 * (-2^63) because -MIN_VALUE == MIN_VALUE (since 2^63 cannot be represented as
 * a positive number, it overflows back into a negative).  Not handling this
 * case would often result in infinite recursion.
 *
 * @param {number} low  The low (signed) 32 bits of the long.
 * @param {number} high  The high (signed) 32 bits of the long.
 * @constructor
 */
Long = function(low, high) {
    /**
     * @type {number}
     * @private
     */
    this.low_ = low | 0;  // force into 32 signed bits.

    /**
     * @type {number}
     * @private
     */
    this.high_ = high | 0;  // force into 32 signed bits.
};


// NOTE: Common constant values ZERO, ONE, NEG_ONE, etc. are defined below the
// from* methods on which they depend.


/**
 * A cache of the Long representations of small integer values.
 * @type {!Object}
 * @private
 */
Long.IntCache_ = {};


/**
 * Returns a Long representing the given (32-bit) integer value.
 * @param {number} value The 32-bit integer in question.
 * @return {!Long} The corresponding Long value.
 */
Long.fromInt = function(value) {
    if (-128 <= value && value < 128) {
        var cachedObj = Long.IntCache_[value];
        if (cachedObj) {
            return cachedObj;
        }
    }

    var obj = new Long(value | 0, value < 0 ? -1 : 0);
    if (-128 <= value && value < 128) {
        Long.IntCache_[value] = obj;
    }
    return obj;
};


/**
 * Returns a Long representing the given value, provided that it is a finite
 * number.  Otherwise, zero is returned.
 * @param {number} value The number in question.
 * @return {!Long} The corresponding Long value.
 */
Long.fromNumber = function(value) {
    if (isNaN(value) || !isFinite(value)) {
        return Long.ZERO;
    } else if (value <= -Long.TWO_PWR_63_DBL_) {
        return Long.MIN_VALUE;
    } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
        return Long.MAX_VALUE;
    } else if (value < 0) {
        return Long.fromNumber(-value).negate();
    } else {
        return new Long(
            (value % Long.TWO_PWR_32_DBL_) | 0,
            (value / Long.TWO_PWR_32_DBL_) | 0);
    }
};


/**
 * Returns a Long representing the 64-bit integer that comes by concatenating
 * the given high and low bits.  Each is assumed to use 32 bits.
 * @param {number} lowBits The low 32-bits.
 * @param {number} highBits The high 32-bits.
 * @return {!Long} The corresponding Long value.
 */
Long.fromBits = function(lowBits, highBits) {
    return new Long(lowBits, highBits);
};


/**
 * Returns a Long representation of the given string, written using the given
 * radix.
 * @param {string} str The textual representation of the Long.
 * @param {number=} opt_radix The radix in which the text is written.
 * @return {!Long} The corresponding Long value.
 */
Long.fromString = function(str, opt_radix) {
    if (str.length == 0) {
        throw Error('number format error: empty string');
    }

    var radix = opt_radix || 10;
    if (radix < 2 || 36 < radix) {
        throw Error('radix out of range: ' + radix);
    }

    if (str.charAt(0) == '-') {
        return Long.fromString(str.substring(1), radix).negate();
    } else if (str.indexOf('-') >= 0) {
        throw Error('number format error: interior "-" character: ' + str);
    }

    // Do several (8) digits each time through the loop, so as to
    // minimize the calls to the very expensive emulated div.
    var radixToPower = Long.fromNumber(Math.pow(radix, 8));

    var result = Long.ZERO;
    for (var i = 0; i < str.length; i += 8) {
        var size = Math.min(8, str.length - i);
        var value = parseInt(str.substring(i, i + size), radix);
        if (size < 8) {
            var power = Long.fromNumber(Math.pow(radix, size));
            result = result.multiply(power).add(Long.fromNumber(value));
        } else {
            result = result.multiply(radixToPower);
            result = result.add(Long.fromNumber(value));
        }
    }
    return result;
};


// NOTE: the compiler should inline these constant values below and then remove
// these variables, so there should be no runtime penalty for these.


/**
 * Number used repeated below in calculations.  This must appear before the
 * first call to any from* function below.
 * @type {number}
 * @private
 */
Long.TWO_PWR_16_DBL_ = 1 << 16;


/**
 * @type {number}
 * @private
 */
Long.TWO_PWR_24_DBL_ = 1 << 24;


/**
 * @type {number}
 * @private
 */
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;


/**
 * @type {number}
 * @private
 */
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;


/**
 * @type {number}
 * @private
 */
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;


/**
 * @type {number}
 * @private
 */
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;


/**
 * @type {number}
 * @private
 */
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;


/** @type {!Long} */
Long.ZERO = Long.fromInt(0);


/** @type {!Long} */
Long.ONE = Long.fromInt(1);


/** @type {!Long} */
Long.NEG_ONE = Long.fromInt(-1);


/** @type {!Long} */
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);


/** @type {!Long} */
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);


/**
 * @type {!Long}
 * @private
 */
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);


/** @return {number} The value, assuming it is a 32-bit integer. */
Long.prototype.toInt = function() {
    return this.low_;
};


/** @return {number} The closest floating-point representation to this value. */
Long.prototype.toNumber = function() {
    return this.high_ * Long.TWO_PWR_32_DBL_ +
        this.getLowBitsUnsigned();
};


/**
 * @param {number=} opt_radix The radix in which the text should be written.
 * @return {string} The textual representation of this value.
 * @override
 */
Long.prototype.toString = function(opt_radix) {
    var radix = opt_radix || 10;
    if (radix < 2 || 36 < radix) {
        throw Error('radix out of range: ' + radix);
    }

    if (this.isZero()) {
        return '0';
    }

    if (this.isNegative()) {
        if (this.equals(Long.MIN_VALUE)) {
            // We need to change the Long value before it can be negated, so we remove
            // the bottom-most digit in this base and then recurse to do the rest.
            var radixLong = Long.fromNumber(radix);
            var div = this.div(radixLong);
            var rem = div.multiply(radixLong).subtract(this);
            return div.toString(radix) + rem.toInt().toString(radix);
        } else {
            return '-' + this.negate().toString(radix);
        }
    }

    // Do several (6) digits each time through the loop, so as to
    // minimize the calls to the very expensive emulated div.
    var radixToPower = Long.fromNumber(Math.pow(radix, 6));

    var rem = this;
    var result = '';
    while (true) {
        var remDiv = rem.div(radixToPower);
        var intval = rem.subtract(remDiv.multiply(radixToPower)).toInt();
        var digits = intval.toString(radix);

        rem = remDiv;
        if (rem.isZero()) {
            return digits + result;
        } else {
            while (digits.length < 6) {
                digits = '0' + digits;
            }
            result = '' + digits + result;
        }
    }
};


/** @return {number} The high 32-bits as a signed value. */
Long.prototype.getHighBits = function() {
    return this.high_;
};


/** @return {number} The low 32-bits as a signed value. */
Long.prototype.getLowBits = function() {
    return this.low_;
};


/** @return {number} The low 32-bits as an unsigned value. */
Long.prototype.getLowBitsUnsigned = function() {
    return (this.low_ >= 0) ?
        this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};


/**
 * @return {number} Returns the number of bits needed to represent the absolute
 *     value of this Long.
 */
Long.prototype.getNumBitsAbs = function() {
    if (this.isNegative()) {
        if (this.equals(Long.MIN_VALUE)) {
            return 64;
        } else {
            return this.negate().getNumBitsAbs();
        }
    } else {
        var val = this.high_ != 0 ? this.high_ : this.low_;
        for (var bit = 31; bit > 0; bit--) {
            if ((val & (1 << bit)) != 0) {
                break;
            }
        }
        return this.high_ != 0 ? bit + 33 : bit + 1;
    }
};


/** @return {boolean} Whether this value is zero. */
Long.prototype.isZero = function() {
    return this.high_ == 0 && this.low_ == 0;
};


/** @return {boolean} Whether this value is negative. */
Long.prototype.isNegative = function() {
    return this.high_ < 0;
};


/** @return {boolean} Whether this value is odd. */
Long.prototype.isOdd = function() {
    return (this.low_ & 1) == 1;
};


/**
 * @param {Long} other Long to compare against.
 * @return {boolean} Whether this Long equals the other.
 */
Long.prototype.equals = function(other) {
    return (this.high_ == other.high_) && (this.low_ == other.low_);
};


/**
 * @param {Long} other Long to compare against.
 * @return {boolean} Whether this Long does not equal the other.
 */
Long.prototype.notEquals = function(other) {
    return (this.high_ != other.high_) || (this.low_ != other.low_);
};


/**
 * @param {Long} other Long to compare against.
 * @return {boolean} Whether this Long is less than the other.
 */
Long.prototype.lessThan = function(other) {
    return this.compare(other) < 0;
};


/**
 * @param {Long} other Long to compare against.
 * @return {boolean} Whether this Long is less than or equal to the other.
 */
Long.prototype.lessThanOrEqual = function(other) {
    return this.compare(other) <= 0;
};


/**
 * @param {Long} other Long to compare against.
 * @return {boolean} Whether this Long is greater than the other.
 */
Long.prototype.greaterThan = function(other) {
    return this.compare(other) > 0;
};


/**
 * @param {Long} other Long to compare against.
 * @return {boolean} Whether this Long is greater than or equal to the other.
 */
Long.prototype.greaterThanOrEqual = function(other) {
    return this.compare(other) >= 0;
};


/**
 * Compares this Long with the given one.
 * @param {Long} other Long to compare against.
 * @return {number} 0 if they are the same, 1 if the this is greater, and -1
 *     if the given one is greater.
 */
Long.prototype.compare = function(other) {
    if (this.equals(other)) {
        return 0;
    }

    var thisNeg = this.isNegative();
    var otherNeg = other.isNegative();
    if (thisNeg && !otherNeg) {
        return -1;
    }
    if (!thisNeg && otherNeg) {
        return 1;
    }

    // at this point, the signs are the same, so subtraction will not overflow
    if (this.subtract(other).isNegative()) {
        return -1;
    } else {
        return 1;
    }
};


/** @return {!Long} The negation of this value. */
Long.prototype.negate = function() {
    if (this.equals(Long.MIN_VALUE)) {
        return Long.MIN_VALUE;
    } else {
        return this.not().add(Long.ONE);
    }
};


/**
 * Returns the sum of this and the given Long.
 * @param {Long} other Long to add to this one.
 * @return {!Long} The sum of this and the given Long.
 */
Long.prototype.add = function(other) {
    // Divide each number into 4 chunks of 16 bits, and then sum the chunks.

    var a48 = this.high_ >>> 16;
    var a32 = this.high_ & 0xFFFF;
    var a16 = this.low_ >>> 16;
    var a00 = this.low_ & 0xFFFF;

    var b48 = other.high_ >>> 16;
    var b32 = other.high_ & 0xFFFF;
    var b16 = other.low_ >>> 16;
    var b00 = other.low_ & 0xFFFF;

    var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
    c00 += a00 + b00;
    c16 += c00 >>> 16;
    c00 &= 0xFFFF;
    c16 += a16 + b16;
    c32 += c16 >>> 16;
    c16 &= 0xFFFF;
    c32 += a32 + b32;
    c48 += c32 >>> 16;
    c32 &= 0xFFFF;
    c48 += a48 + b48;
    c48 &= 0xFFFF;
    return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};


/**
 * Returns the difference of this and the given Long.
 * @param {Long} other Long to subtract from this.
 * @return {!Long} The difference of this and the given Long.
 */
Long.prototype.subtract = function(other) {
    return this.add(other.negate());
};


/**
 * Returns the product of this and the given long.
 * @param {Long} other Long to multiply with this.
 * @return {!Long} The product of this and the other.
 */
Long.prototype.multiply = function(other) {
    if (this.isZero()) {
        return Long.ZERO;
    } else if (other.isZero()) {
        return Long.ZERO;
    }

    if (this.equals(Long.MIN_VALUE)) {
        return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
    } else if (other.equals(Long.MIN_VALUE)) {
        return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
    }

    if (this.isNegative()) {
        if (other.isNegative()) {
            return this.negate().multiply(other.negate());
        } else {
            return this.negate().multiply(other).negate();
        }
    } else if (other.isNegative()) {
        return this.multiply(other.negate()).negate();
    }

    // If both longs are small, use float multiplication
    if (this.lessThan(Long.TWO_PWR_24_) &&
        other.lessThan(Long.TWO_PWR_24_)) {
        return Long.fromNumber(this.toNumber() * other.toNumber());
    }

    // Divide each long into 4 chunks of 16 bits, and then add up 4x4 products.
    // We can skip products that would overflow.

    var a48 = this.high_ >>> 16;
    var a32 = this.high_ & 0xFFFF;
    var a16 = this.low_ >>> 16;
    var a00 = this.low_ & 0xFFFF;

    var b48 = other.high_ >>> 16;
    var b32 = other.high_ & 0xFFFF;
    var b16 = other.low_ >>> 16;
    var b00 = other.low_ & 0xFFFF;

    var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
    c00 += a00 * b00;
    c16 += c00 >>> 16;
    c00 &= 0xFFFF;
    c16 += a16 * b00;
    c32 += c16 >>> 16;
    c16 &= 0xFFFF;
    c16 += a00 * b16;
    c32 += c16 >>> 16;
    c16 &= 0xFFFF;
    c32 += a32 * b00;
    c48 += c32 >>> 16;
    c32 &= 0xFFFF;
    c32 += a16 * b16;
    c48 += c32 >>> 16;
    c32 &= 0xFFFF;
    c32 += a00 * b32;
    c48 += c32 >>> 16;
    c32 &= 0xFFFF;
    c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
    c48 &= 0xFFFF;
    return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};


/**
 * Returns this Long divided by the given one.
 * @param {Long} other Long by which to divide.
 * @return {!Long} This Long divided by the given one.
 */
Long.prototype.div = function(other) {
    if (other.isZero()) {
        throw Error('division by zero');
    } else if (this.isZero()) {
        return Long.ZERO;
    }

    if (this.equals(Long.MIN_VALUE)) {
        if (other.equals(Long.ONE) ||
            other.equals(Long.NEG_ONE)) {
            return Long.MIN_VALUE;  // recall that -MIN_VALUE == MIN_VALUE
        } else if (other.equals(Long.MIN_VALUE)) {
            return Long.ONE;
        } else {
            // At this point, we have |other| >= 2, so |this/other| < |MIN_VALUE|.
            var halfThis = this.shiftRight(1);
            var approx = halfThis.div(other).shiftLeft(1);
            if (approx.equals(Long.ZERO)) {
                return other.isNegative() ? Long.ONE : Long.NEG_ONE;
            } else {
                var rem = this.subtract(other.multiply(approx));
                var result = approx.add(rem.div(other));
                return result;
            }
        }
    } else if (other.equals(Long.MIN_VALUE)) {
        return Long.ZERO;
    }

    if (this.isNegative()) {
        if (other.isNegative()) {
            return this.negate().div(other.negate());
        } else {
            return this.negate().div(other).negate();
        }
    } else if (other.isNegative()) {
        return this.div(other.negate()).negate();
    }

    // Repeat the following until the remainder is less than other:  find a
    // floating-point that approximates remainder / other *from below*, add this
    // into the result, and subtract it from the remainder.  It is critical that
    // the approximate value is less than or equal to the real value so that the
    // remainder never becomes negative.
    var res = Long.ZERO;
    var rem = this;
    while (rem.greaterThanOrEqual(other)) {
        // Approximate the result of division. This may be a little greater or
        // smaller than the actual value.
        var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

        // We will tweak the approximate result by changing it in the 48-th digit or
        // the smallest non-fractional digit, whichever is larger.
        var log2 = Math.ceil(Math.log(approx) / Math.LN2);
        var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

        // Decrease the approximation until it is smaller than the remainder.  Note
        // that if it is too large, the product overflows and is negative.
        var approxRes = Long.fromNumber(approx);
        var approxRem = approxRes.multiply(other);
        while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
            approx -= delta;
            approxRes = Long.fromNumber(approx);
            approxRem = approxRes.multiply(other);
        }

        // We know the answer can't be zero... and actually, zero would cause
        // infinite recursion since we would make no progress.
        if (approxRes.isZero()) {
            approxRes = Long.ONE;
        }

        res = res.add(approxRes);
        rem = rem.subtract(approxRem);
    }
    return res;
};


/**
 * Returns this Long modulo the given one.
 * @param {Long} other Long by which to mod.
 * @return {!Long} This Long modulo the given one.
 */
Long.prototype.modulo = function(other) {
    return this.subtract(this.div(other).multiply(other));
};


/** @return {!Long} The bitwise-NOT of this value. */
Long.prototype.not = function() {
    return Long.fromBits(~this.low_, ~this.high_);
};


/**
 * Returns the bitwise-AND of this Long and the given one.
 * @param {Long} other The Long with which to AND.
 * @return {!Long} The bitwise-AND of this and the other.
 */
Long.prototype.and = function(other) {
    return Long.fromBits(this.low_ & other.low_,
        this.high_ & other.high_);
};


/**
 * Returns the bitwise-OR of this Long and the given one.
 * @param {Long} other The Long with which to OR.
 * @return {!Long} The bitwise-OR of this and the other.
 */
Long.prototype.or = function(other) {
    return Long.fromBits(this.low_ | other.low_,
        this.high_ | other.high_);
};


/**
 * Returns the bitwise-XOR of this Long and the given one.
 * @param {Long} other The Long with which to XOR.
 * @return {!Long} The bitwise-XOR of this and the other.
 */
Long.prototype.xor = function(other) {
    return Long.fromBits(this.low_ ^ other.low_,
        this.high_ ^ other.high_);
};


/**
 * Returns this Long with bits shifted to the left by the given amount.
 * @param {number} numBits The number of bits by which to shift.
 * @return {!Long} This shifted to the left by the given amount.
 */
Long.prototype.shiftLeft = function(numBits) {
    numBits &= 63;
    if (numBits == 0) {
        return this;
    } else {
        var low = this.low_;
        if (numBits < 32) {
            var high = this.high_;
            return Long.fromBits(
                low << numBits,
                (high << numBits) | (low >>> (32 - numBits)));
        } else {
            return Long.fromBits(0, low << (numBits - 32));
        }
    }
};


/**
 * Returns this Long with bits shifted to the right by the given amount.
 * @param {number} numBits The number of bits by which to shift.
 * @return {!Long} This shifted to the right by the given amount.
 */
Long.prototype.shiftRight = function(numBits) {
    numBits &= 63;
    if (numBits == 0) {
        return this;
    } else {
        var high = this.high_;
        if (numBits < 32) {
            var low = this.low_;
            return Long.fromBits(
                (low >>> numBits) | (high << (32 - numBits)),
                high >> numBits);
        } else {
            return Long.fromBits(
                high >> (numBits - 32),
                high >= 0 ? 0 : -1);
        }
    }
};


/**
 * Returns this Long with bits shifted to the right by the given amount, with
 * the new top bits matching the current sign bit.
 * @param {number} numBits The number of bits by which to shift.
 * @return {!Long} This shifted to the right by the given amount, with
 *     zeros placed into the new leading bits.
 */
Long.prototype.shiftRightUnsigned = function(numBits) {
    numBits &= 63;
    if (numBits == 0) {
        return this;
    } else {
        var high = this.high_;
        if (numBits < 32) {
            var low = this.low_;
            return Long.fromBits(
                (low >>> numBits) | (high << (32 - numBits)),
                high >>> numBits);
        } else if (numBits == 32) {
            return Long.fromBits(high, 0);
        } else {
            return Long.fromBits(high >>> (numBits - 32), 0);
        }
    }
};


// jssha256 version 0.1  -  Copyright 2006 B. Poettering (GNU GPL)
// http://point-at-infinity.org/jssha256/

 /*
 * This is a JavaScript implementation of the SHA256 secure hash function
 * and the HMAC-SHA256 message authentication code (MAC).
 *
 * The routines' well-functioning has been verified with the test vectors 
 * given in FIPS-180-2, Appendix B and IETF RFC 4231. The HMAC algorithm 
 * conforms to IETF RFC 2104. 
 *
 * The following code example computes the hash value of the string "abc".
 *
 *    SHA256_init();
 *    SHA256_write("abc");
 *    digest = SHA256_finalize();  
 *    digest_hex = array_to_hex_string(digest);
 * 
 * Get the same result by calling the shortcut function SHA256_hash:
 * 
 *    digest_hex = SHA256_hash("abc");
 * 
 * In the following example the calculation of the HMAC of the string "abc" 
 * using the key "secret key" is shown:
 * 
 *    HMAC_SHA256_init("secret key");
 *    HMAC_SHA256_write("abc");
 *    mac = HMAC_SHA256_finalize();
 *    mac_hex = array_to_hex_string(mac);
 *
 * Again, the same can be done more conveniently:
 * 
 *    mac_hex = HMAC_SHA256_MAC("secret key", "abc");
 *
 * Note that the internal state of the hash function is held in global
 * variables. Therefore one hash value calculation has to be completed 
 * before the next is begun. The same applies the the HMAC routines.
 *
 * Report bugs to: jssha256 AT point-at-infinity.org
 *
 */

/******************************************************************************/

/* Two all purpose helper functions follow */

/* string_to_array: convert a string to a character (byte) array */

function string_to_array(str) {
    var len = str.length;
    var res = new Array(len);
    for(var i = 0; i < len; i++)
        res[i] = str.charCodeAt(i);
    return res;
}

/* array_to_hex_string: convert a byte array to a hexadecimal string */
function array_to_hex_string(ary) {
    var res = "";
    for(var i = 0; i < ary.length; i++)
        res += SHA256_hexchars[ary[i] >> 4] + SHA256_hexchars[ary[i] & 0x0f];
    return res;
}

/******************************************************************************/

/* The following are the SHA256 routines */

/* 
 SHA256_init: initialize the internal state of the hash function. Call this
 function before calling the SHA256_write function.
 */

function SHA256_init() {
    SHA256_H = new Array(0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
        0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19);
    SHA256_buf = new Array();
    SHA256_len = 0;
}

/*
 SHA256_write: add a message fragment to the hash function's internal state.
 'msg' may be given as string or as byte array and may have arbitrary length.

 */

function SHA256_write(msg) {
    if (typeof(msg) == "string")
        SHA256_buf = SHA256_buf.concat(string_to_array(msg));
    else
        SHA256_buf = SHA256_buf.concat(msg);
    for(var i = 0; i + 64 <= SHA256_buf.length; i += 64)
        SHA256_Hash_Byte_Block(SHA256_H, SHA256_buf.slice(i, i + 64));
    SHA256_buf = SHA256_buf.slice(i);
    SHA256_len += msg.length;
}

/*
 SHA256_finalize: finalize the hash value calculation. Call this function
 after the last call to SHA256_write. An array of 32 bytes (= 256 bits)
 is returned.
 */

function SHA256_finalize() {
    SHA256_buf[SHA256_buf.length] = 0x80;

    if (SHA256_buf.length > 64 - 8) {
        for(var i = SHA256_buf.length; i < 64; i++)
            SHA256_buf[i] = 0;
        SHA256_Hash_Byte_Block(SHA256_H, SHA256_buf);
        SHA256_buf.length = 0;
    }

    for(var i = SHA256_buf.length; i < 64 - 5; i++)
        SHA256_buf[i] = 0;
    SHA256_buf[59] = (SHA256_len >>> 29) & 0xff;
    SHA256_buf[60] = (SHA256_len >>> 21) & 0xff;
    SHA256_buf[61] = (SHA256_len >>> 13) & 0xff;
    SHA256_buf[62] = (SHA256_len >>> 5) & 0xff;
    SHA256_buf[63] = (SHA256_len << 3) & 0xff;
    SHA256_Hash_Byte_Block(SHA256_H, SHA256_buf);

    var res = new Array(32);
    for(var i = 0; i < 8; i++) {
        res[4 * i + 0] = SHA256_H[i] >>> 24;
        res[4 * i + 1] = (SHA256_H[i] >> 16) & 0xff;
        res[4 * i + 2] = (SHA256_H[i] >> 8) & 0xff;
        res[4 * i + 3] = SHA256_H[i] & 0xff;
    }

    delete SHA256_H;
    delete SHA256_buf;
    delete SHA256_len;
    return res;
}

/*
 SHA256_hash: calculate the hash value of the string or byte array 'msg'
 and return it as hexadecimal string. This shortcut fuпction may be more
 convenient than calling SHA256_init, SHA256_write, SHA256_finalize
 and array_to_hex_string explicitly.
 */

function SHA256_hash(msg) {
    var res;
    SHA256_init();
    SHA256_write(msg);
    return SHA256_finalize();
}


/******************************************************************************/

/* The following lookup tables and functions are for internal use only! */

SHA256_hexchars = new Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    'a', 'b', 'c', 'd', 'e', 'f');

SHA256_K = new Array(
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1,
    0x923f82a4, 0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
    0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786,
    0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147,
    0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b,
    0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a,
    0x5b9cca4f, 0x682e6ff3, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
    0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
);

function SHA256_sigma0(x) {
    return ((x >>> 7) | (x << 25)) ^ ((x >>> 18) | (x << 14)) ^ (x >>> 3);
}

function SHA256_sigma1(x) {
    return ((x >>> 17) | (x << 15)) ^ ((x >>> 19) | (x << 13)) ^ (x >>> 10);
}

function SHA256_Sigma0(x) {
    return ((x >>> 2) | (x << 30)) ^ ((x >>> 13) | (x << 19)) ^
        ((x >>> 22) | (x << 10));
}

function SHA256_Sigma1(x) {
    return ((x >>> 6) | (x << 26)) ^ ((x >>> 11) | (x << 21)) ^
        ((x >>> 25) | (x << 7));
}

function SHA256_Ch(x, y, z) {
    return z ^ (x & (y ^ z));
}

function SHA256_Maj(x, y, z) {
    return (x & y) ^ (z & (x ^ y));
}

function SHA256_Hash_Word_Block(H, W) {
    for(var i = 16; i < 64; i++)
        W[i] = (SHA256_sigma1(W[i - 2]) +  W[i - 7] +
            SHA256_sigma0(W[i - 15]) + W[i - 16]) & 0xffffffff;
    var state = new Array().concat(H);
    for(var i = 0; i < 64; i++) {
        var T1 = state[7] + SHA256_Sigma1(state[4]) +
            SHA256_Ch(state[4], state[5], state[6]) + SHA256_K[i] + W[i];
        var T2 = SHA256_Sigma0(state[0]) + SHA256_Maj(state[0], state[1], state[2]);
        state.pop();
        state.unshift((T1 + T2) & 0xffffffff);
        state[4] = (state[4] + T1) & 0xffffffff;
    }
    for(var i = 0; i < 8; i++)
        H[i] = (H[i] + state[i]) & 0xffffffff;
}

function SHA256_Hash_Byte_Block(H, w) {
    var W = new Array(16);
    for(var i = 0; i < 16; i++)
        W[i] = w[4 * i + 0] << 24 | w[4 * i + 1] << 16 |
            w[4 * i + 2] << 8 | w[4 * i + 3];
    SHA256_Hash_Word_Block(H, W);
}

// helper function
function int10() {
    return new Array(10);
}

// Ported from c++ to js
// Original: https://code.google.com/p/gvl/source/browse/crypt/curve25519.cpp?r=3c26171fd2d914484f03b7b63ca2065fa03a7aa3 (New BSD License)
KEY_SIZE = 32;

/* 0 */
ZERO = new Array(
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
);

/* the prime 2^255-19 */
PRIME = new Array(
    237, 255, 255, 255,
    255, 255, 255, 255,
    255, 255, 255, 255,
    255, 255, 255, 255,
    255, 255, 255, 255,
    255, 255, 255, 255,
    255, 255, 255, 255,
    255, 255, 255, 127
);

/* group order (a prime near 2^252+2^124) */
ORDER = new Array(
    237, 211, 245, 92,
    26,  99,  18,  88,
    214, 156, 247, 162,
    222, 249, 222, 20,
    0,   0,   0,   0,
    0,   0,   0,   0,
    0,   0,   0,   0,
    0,   0,   0,   16
);

/* smallest multiple of the order that's >= 2^255 */
ORDER_TIMES_8 = new Array(
    104, 159, 174, 231,
    210, 24,  147, 192,
    178, 230, 188, 23,
    245, 206, 247, 166,
    0,   0,   0,   0,
    0,   0,   0,   0,
    0,   0,   0,   0,
    0,   0,   0,   128
);

/* constants 2Gy and 1/(2Gy) */
BASE_2Y = new Array(
    39999547, 18689728, 59995525, 1648697, 57546132,
        24010086, 19059592, 5425144, 63499247, 16420658
);

BASE_R2Y = new Array(
    5744, 8160848, 4790893, 13779497, 35730846,
        12541209, 49101323, 30047407, 40071253, 6226132
);


P25 = 33554431;	/* 0x2000000 - 1 */
P26 = 67108863;	/* 0x4000000 - 1 */


function unpack(x, m)
{
    x[0] = ((m[0] & 0xFF))         | ((m[1] & 0xFF))<<8 |
        (m[2] & 0xFF)<<16      | ((m[3] & 0xFF)& 3)<<24;
    x[1] = ((m[3] & 0xFF)&~ 3)>>2  | (m[4] & 0xFF)<<6 |
        (m[5] & 0xFF)<<14 | ((m[6] & 0xFF)& 7)<<22;
    x[2] = ((m[6] & 0xFF)&~ 7)>>3  | (m[7] & 0xFF)<<5 |
        (m[8] & 0xFF)<<13 | ((m[9] & 0xFF)&31)<<21;
    x[3] = ((m[9] & 0xFF)&~31)>>5  | (m[10] & 0xFF)<<3 |
        (m[11] & 0xFF)<<11 | ((m[12] & 0xFF)&63)<<19;
    x[4] = ((m[12] & 0xFF)&~63)>>6 | (m[13] & 0xFF)<<2 |
        (m[14] & 0xFF)<<10 |  (m[15] & 0xFF)    <<18;
    x[5] =  (m[16] & 0xFF)         | (m[17] & 0xFF)<<8 |
        (m[18] & 0xFF)<<16 | ((m[19] & 0xFF)& 1)<<24;
    x[6] = ((m[19] & 0xFF)&~ 1)>>1 | (m[20] & 0xFF)<<7 |
        (m[21] & 0xFF)<<15 | ((m[22] & 0xFF)& 7)<<23;
    x[7] = ((m[22] & 0xFF)&~ 7)>>3 | (m[23] & 0xFF)<<5 |
        (m[24] & 0xFF)<<13 | ((m[25] & 0xFF)&15)<<21;
    x[8] = ((m[25] & 0xFF)&~15)>>4 | (m[26] & 0xFF)<<4 |
        (m[27] & 0xFF)<<12 | ((m[28] & 0xFF)&63)<<20;
    x[9] = ((m[28] & 0xFF)&~63)>>6 | (m[29] & 0xFF)<<2 |
        (m[30] & 0xFF)<<10 |  (m[31] & 0xFF)    <<18;
}

/* Check if reduced-form input >= 2^255-19 */
function is_overflow(x) {
    return (
        ((x[0] > P26-19)) &&
            ((x[1] & x[3] & x[5] & x[7] & x[9]) == P25) &&
            ((x[2] & x[4] & x[6] & x[8]) == P26)
        ) || (x[9] > P25);
}

function modulo(a, b) {
    return a - Math.floor(a/b)*b;

}

function ToInt8(x) {
    if (typeof x !== 'number')
        x = x.getLowBits();

    var uint = x - Math.floor(x/0x100)*0x100;
    if (uint >= 0x80)
        return uint - 0x100;
    else
        return uint;
}

/* Convert from internal format to little-endian uint8_t format.  The 
 * number must be in a reduced form which is output by the following ops:
 *     unpack, mul, sqr
 *     set --  if input in range 0 .. P25
 * If you're unsure if the number is reduced, first multiply it by 1.  */
function pack(x, m)
{
    var ld = (is_overflow(x)?1:0) - ((x[9] < 0)?1:0);
    var ud = ld * -(P25+1);
    ld *= 19;
    var t = Long.fromInt(ld).add(Long.fromInt(x[0])).add(Long.fromInt(x[1]).shiftLeft(26));
    m[ 0] = t.getLowBits() & 0xFF;
    m[ 1] = t.shiftRight(8).getLowBits() & 0xFF;
    m[ 2] = t.shiftRight(16).getLowBits() & 0xFF;
    m[ 3] = t.shiftRight(24).getLowBits() & 0xFF;

    t = t.shiftRight(32).add(Long.fromInt(x[2]).shiftLeft(19));
    m[ 4] = t.getLowBits() & 0xFF;
    m[ 5] = t.shiftRight(8).getLowBits() & 0xFF;
    m[ 6] = t.shiftRight(16).getLowBits() & 0xFF;
    m[ 7] = t.shiftRight(24).getLowBits() & 0xFF

    t = t.shiftRight(32).add(Long.fromInt(x[3]).shiftLeft(13));
    m[ 8] = t.getLowBits() & 0xFF;
    m[ 9] = t.shiftRight(8).getLowBits() & 0xFF;
    m[10] = t.shiftRight(16).getLowBits() & 0xFF;
    m[11] = t.shiftRight(24).getLowBits() & 0xFF;

    t = t.shiftRight(32).add(Long.fromInt(x[4]).shiftLeft(6));
    m[12] = t.getLowBits() & 0xFF;
    m[13] = t.shiftRight(8).getLowBits() & 0xFF;
    m[14] = t.shiftRight(16).getLowBits() & 0xFF;
    m[15] = t.shiftRight(24).getLowBits() & 0xFF;

    t = t.shiftRight(32).add(Long.fromInt(x[5])).add(Long.fromInt(x[6]).shiftLeft(25));
    m[16] = t.getLowBits() & 0xFF;
    m[17] = t.shiftRight(8).getLowBits() & 0xFF;
    m[18] = t.shiftRight(16).getLowBits() & 0xFF;
    m[19] = t.shiftRight(24).getLowBits() & 0xFF;

    t = t.shiftRight(32).add(Long.fromInt(x[7]).shiftLeft(19));
    m[20] = t.getLowBits() & 0xFF;
    m[21] = t.shiftRight(8).getLowBits() & 0xFF;
    m[22] = t.shiftRight(16).getLowBits() & 0xFF;
    m[23] = t.shiftRight(24).getLowBits() & 0xFF;

    t = t.shiftRight(32).add(Long.fromInt(x[8]).shiftLeft(12));
    m[24] = t.getLowBits() & 0xFF;
    m[25] = t.shiftRight(8).getLowBits() & 0xFF;
    m[26] = t.shiftRight(16).getLowBits() & 0xFF;
    m[27] = t.shiftRight(24).getLowBits() & 0xFF;

    t = t.shiftRight(32).add(Long.fromInt(x[9]).add(Long.fromInt(ud)).shiftLeft(6));
    m[28] = t.getLowBits() & 0xFF;
    m[29] = t.shiftRight(8).getLowBits() & 0xFF;
    m[30] = t.shiftRight(16).getLowBits() & 0xFF;
    m[31] = t.shiftRight(24).getLowBits() & 0xFF;
}

function set(o, i)
{
    o[0]=i;	o[1]=0;
    o[2]=0;	o[3]=0;
    o[4]=0;	o[5]=0;
    o[6]=0;	o[7]=0;
    o[8]=0;	o[9]=0;
}

function cpy(o, i)
{
    for (var j = 0; j < 10; j++)
        o[j] = i[j];
}

function add(xy, x, y)
{
    xy[0] = x[0] + y[0];	xy[1] = x[1] + y[1];
    xy[2] = x[2] + y[2];	xy[3] = x[3] + y[3];
    xy[4] = x[4] + y[4];	xy[5] = x[5] + y[5];
    xy[6] = x[6] + y[6];	xy[7] = x[7] + y[7];
    xy[8] = x[8] + y[8];	xy[9] = x[9] + y[9];
}

function sub(xy, x, y)
{
    xy[0] = x[0] - y[0];	xy[1] = x[1] - y[1];
    xy[2] = x[2] - y[2];	xy[3] = x[3] - y[3];
    xy[4] = x[4] - y[4];	xy[5] = x[5] - y[5];
    xy[6] = x[6] - y[6];	xy[7] = x[7] - y[7];
    xy[8] = x[8] - y[8];	xy[9] = x[9] - y[9];
}

function mul32_to64(/*int32_t */a, /*int32_t */b)
{
    return Long.fromInt(a).multiply(Long.fromInt(b));
}

/********************* radix 2^8 math *********************/

function cpy32(d, s)
{
    for (var i = 0; i < 32; i++)
        d[i] = s[i];
}

/* p[m..n+m-1] = q[m..n+m-1] + z * x */
/* n is the size of x */
/* n+m is the size of p and q */
function mula_small(p, q, m, x, n, z)
{
    var v=0;
    for (var i=0;i<n;++i)
    {
        v += (q[i+m] & 0xFF)+z*(x[i] & 0xFF);
        p[i+m] = v & 0xFF;
        v >>= 8;
    }
    return v;
}

/* p += x * y * z  where z is a small integer
 * x is size 32, y is size t, p is size 32+t
 * y is allowed to overlap with p+32 if you don't care about the upper half  */
function mula32(p, x, y, t, z)
{
    var n = 31;
    var w = 0;
    var i = 0;
    for (; i < t; i++) {
        var zy = z * (y[i] & 0xFF);
        w += mula_small(p, p, i, x, n, zy) +
            (p[i+n] & 0xFF) + zy * (x[n] & 0xFF);
        p[i+n] = w & 0xFF;
        w >>= 8;
    }
    p[i+n] = (w + (p[i+n] & 0xFF)) & 0xFF;
    return w >> 8;
}

/* divide r (size n) by d (size t), returning quotient q and remainder r
 * quotient is size n-t+1, remainder is size t
 * requires t > 0 && d[t-1] != 0
 * requires that r[-1] and d[-1] are valid memory locations
 * q may overlap with r+t */
function divmod(q, r, n, d, t)
{
    var rn = 0;
    var dt = ((d[t-1] & 0xFF) << 8);
    if (t>1)
    {
        dt |= (d[t-2] & 0xFF);
    }
    while (n-- >= t)
    {
        var z = (rn << 16) | ((r[n] & 0xFF) << 8);
        if (n>0)
        {
            z |= (r[n-1] & 0xFF);
        }
        z = Math.floor(z / dt);
        rn += mula_small(r,r, n-t+1, d, t, -z);
        q[n-t+1] = (z + rn) & 0xFF; /* rn is 0 or -1 (underflow) */
        mula_small(r,r, n-t+1, d, t, -rn);
        rn = r[n] & 0xFF;
        r[n] = 0;
    }
    r[t-1] = rn & 0xFF;
}

function numsize(x, n)
{
    while (n--!=0 && x[n]==0)
        ;
    return n+1;
}

/* Returns x if a contains the gcd, y if b.
 * Also, the returned buffer contains the inverse of a mod b,
 * as 32-uint8_t signed.
 * x and y must have 64 bytes space for temporary use.
 * requires that a[-1] and b[-1] are valid memory locations  */
function egcd32(x, y, a, b)
{
    var an, bn = 32, qn, i;
    for (i = 0; i < 32; i++)
        x[i] = y[i] = 0;
    x[0] = 1;
    an = numsize(a, 32);
    if (an==0)
        return y;	/* division by zero */
    /*uint8_t* */temp= new Array(32);
    while (true)
    {
        qn = bn - an + 1;
        divmod(temp, b, bn, a, an);
        bn = numsize(b, bn);
        if (bn==0)
            return x;
        mula32(y, x, temp, qn, -1);

        qn = an - bn + 1;
        divmod(temp, a, an, b, bn);
        an = numsize(a, an);
        if (an==0)
            return y;
        mula32(x, y, temp, qn, -1);
    }
}


/* Multiply a number by a small integer i range -185861411 .. 185861411.
 * The output is i reduced form, the input x need not be.  x and xy may point
 * to the same buffer. */
function mul_small(xy, x, y)
{
    var t = mul32_to64(x[8], y);
    xy[8] = t.getLowBits() & P26;
    t = t.shiftRight(26).add(mul32_to64(x[9],y));
    xy[9] = t & P25;
    t = Long.fromInt(19).multiply(t.shiftRight(25)).add(mul32_to64(x[0],y));
    xy[0] = t & P26;
    t = t.shiftRight(26).add(mul32_to64(x[1],y));
    xy[1] = t & P25;
    t = t.shiftRight(25).add(mul32_to64(x[2],y));
    xy[2] = t & P26;
    t = t.shiftRight(26).add(mul32_to64(x[3],y));
    xy[3] = t & P25;
    t = t.shiftRight(25).add(mul32_to64(x[4],y));
    xy[4] = t & P26;
    t = t.shiftRight(26).add(mul32_to64(x[5],y));
    xy[5] = t & P25;
    t = t.shiftRight(25).add(mul32_to64(x[6],y));
    xy[6] = t & P26;
    t = t.shiftRight(26).add(mul32_to64(x[7],y));
    xy[7] = t & P25;
    t = t.shiftRight(25).add(Long.fromInt(xy[8]));
    xy[8] = t & P26;
    xy[9] += t.shiftRight(26).getLowBits();
}

function mul(dest, x, y)
{
    var x_0=x[0],x_1=x[1],x_2=x[2],x_3=x[3],x_4=x[4],
    x_5=x[5],x_6=x[6],x_7=x[7],x_8=x[8],x_9=x[9];
    var y_0=y[0],y_1=y[1],y_2=y[2],y_3=y[3],y_4=y[4],
    y_5=y[5],y_6=y[6],y_7=y[7],y_8=y[8],y_9=y[9];

    var t = mul32_to64(x_0, y_8).add(mul32_to64(x_2, y_6)).add(mul32_to64(x_4, y_4)).add(mul32_to64(x_6, y_2)).add(mul32_to64(x_8, y_0));
    t = t.add(Long.fromInt(2).multiply(mul32_to64(x_1, y_7).add(mul32_to64(x_3, y_5)).add(mul32_to64(x_5, y_3)).add(mul32_to64(x_7, y_1))));
    t = t.add(Long.fromInt(38).multiply(mul32_to64(x_9, y_9)));
    dest[8] = t.getLowBits() & P26;
    t = t.shiftRight(26).add(mul32_to64(x_0, y_9)).add(mul32_to64(x_1, y_8)).add( mul32_to64(x_2, y_7)).
        add(mul32_to64(x_3, y_6)).add(mul32_to64(x_4, y_5)).add(mul32_to64(x_5, y_4)).
        add(mul32_to64(x_6, y_3)).add(mul32_to64(x_7, y_2)).add(mul32_to64(x_8, y_1)).
        add(mul32_to64(x_9, y_0));
    dest[9] = t.getLowBits() & P25;
    t = mul32_to64(x_0, y_0).
        add(Long.fromInt(19).multiply(t.shiftRight(25).add(mul32_to64(x_2, y_8)).add(mul32_to64(x_4, y_6)).add(mul32_to64(x_6, y_4)).add(mul32_to64(x_8, y_2)))).
        add(Long.fromInt(38).multiply(mul32_to64(x_1, y_9).add(mul32_to64(x_3, y_7)).add(mul32_to64(x_5, y_5)).add(mul32_to64(x_7, y_3)).add(mul32_to64(x_9, y_1))));
    dest[0] = t.getLowBits() & P26;
    t = t.shiftRight(26).add(mul32_to64(x_0, y_1)).add(mul32_to64(x_1, y_0)).
        add(Long.fromInt(19).multiply(mul32_to64(x_2, y_9).add(mul32_to64(x_3, y_8)).add(mul32_to64(x_4, y_7)).add(mul32_to64(x_5, y_6)).add(mul32_to64(x_6, y_5)).
            add(mul32_to64(x_7, y_4)).add(mul32_to64(x_8, y_3)).add(mul32_to64(x_9, y_2))));
    dest[1] = t.getLowBits() & P25;
    t = t.shiftRight(25).add(mul32_to64(x_0, y_2)).add(mul32_to64(x_2, y_0)).
        add(Long.fromInt(19).multiply(mul32_to64(x_4, y_8).add(mul32_to64(x_6, y_6)).add(mul32_to64(x_8, y_4)))).
        add(Long.fromInt(2).multiply(mul32_to64(x_1, y_1))).
        add(Long.fromInt(38).multiply(mul32_to64(x_3, y_9).add(mul32_to64(x_5, y_7)).add(mul32_to64(x_7, y_5)).add(mul32_to64(x_9, y_3))));
    dest[2] = t.getLowBits() & P26;
    t = t.shiftRight(26).add(mul32_to64(x_0, y_3)).add(mul32_to64(x_1, y_2)).add(mul32_to64(x_2, y_1)).add(mul32_to64(x_3, y_0)).
        add(Long.fromInt(19).multiply(mul32_to64(x_4, y_9).add(mul32_to64(x_5, y_8)).add(mul32_to64(x_6, y_7)).add(mul32_to64(x_7, y_6)).add(mul32_to64(x_8, y_5)).add(mul32_to64(x_9, y_4))));
    dest[3] = t.getLowBits() & P25;
    t = t.shiftRight(25).add(mul32_to64(x_0, y_4)).add(mul32_to64(x_2, y_2)).add(mul32_to64(x_4, y_0)).
        add(Long.fromInt(19).multiply(mul32_to64(x_6, y_8).add(mul32_to64(x_8, y_6)))).
        add(Long.fromInt(2).multiply(mul32_to64(x_1, y_3).add(mul32_to64(x_3, y_1)))).
        add(Long.fromInt(38).multiply(mul32_to64(x_5, y_9).add(mul32_to64(x_7, y_7)).add(mul32_to64(x_9, y_5))));
    dest[4] = t.getLowBits() & P26;
    t = t.shiftRight(26).add(mul32_to64(x_0, y_5)).add(mul32_to64(x_1, y_4)).add(mul32_to64(x_2, y_3)).
        add(mul32_to64(x_3, y_2)).add(mul32_to64(x_4, y_1)).add(mul32_to64(x_5, y_0)).
        add(Long.fromInt(19).multiply(mul32_to64(x_6, y_9).add(mul32_to64(x_7, y_8)).add(mul32_to64(x_8, y_7)).add(mul32_to64(x_9, y_6))));
    dest[5] = t.getLowBits() & P25;
    t = t.shiftRight(25).add(mul32_to64(x_0, y_6)).add(mul32_to64(x_2, y_4)).add(mul32_to64(x_4, y_2)).add(mul32_to64(x_6, y_0)).
        add(Long.fromInt(19).multiply(mul32_to64(x_8, y_8))).
        add(Long.fromInt(2).multiply(mul32_to64(x_1, y_5).add(mul32_to64(x_3, y_3)).add(mul32_to64(x_5, y_1)))).
        add(Long.fromInt(38).multiply(mul32_to64(x_7, y_9).add(mul32_to64(x_9, y_7))));
    dest[6] = t.getLowBits() & P26;
    t = t.shiftRight(26).add(mul32_to64(x_0, y_7)).add(mul32_to64(x_1, y_6)).add(mul32_to64(x_2, y_5)).add(mul32_to64(x_3, y_4)).
        add(mul32_to64(x_4, y_3)).add(mul32_to64(x_5, y_2)).add(mul32_to64(x_6, y_1)).add(mul32_to64(x_7, y_0)).
        add(Long.fromInt(19).multiply(mul32_to64(x_8, y_9).add(mul32_to64(x_9, y_8))));
    dest[7] = t.getLowBits() & P25;
    t = t.shiftRight(25).add(Long.fromInt(dest[8]));
    dest[8] = t.getLowBits() & P26;
    dest[9] += t.shiftRight(26).getLowBits();
}

function sqr(y, x)
{

    var x_0=x[0],x_1=x[1],x_2=x[2],x_3=x[3],x_4=x[4],
    x_5=x[5],x_6=x[6],x_7=x[7],x_8=x[8],x_9=x[9];

    var t = mul32_to64(x_4, x_4).
        add(Long.fromInt(2).multiply(mul32_to64(x_0, x_8).add(mul32_to64(x_2, x_6)))).
        add(Long.fromInt(38).multiply(mul32_to64(x_9, x_9))).add(Long.fromInt(4).multiply(mul32_to64(x_1, x_7).add(mul32_to64(x_3, x_5))));
    y[8] = t.getLowBits() & P26;
    t = t.shiftRight(26).add(Long.fromInt(2).multiply(mul32_to64(x_0, x_9).add(mul32_to64(x_1, x_8)).add(mul32_to64(x_2, x_7)).
        add(mul32_to64(x_3, x_6)).add(mul32_to64(x_4, x_5))));
    y[9] = t.getLowBits() & P25;
    t = Long.fromInt(19).multiply(t.shiftRight(25)).add(mul32_to64(x_0, x_0)).
        add(Long.fromInt(38).multiply(mul32_to64(x_2, x_8).add(mul32_to64(x_4, x_6).add(mul32_to64(x_5, x_5))))).
        add(Long.fromInt(76).multiply(mul32_to64(x_1, x_9).add(mul32_to64(x_3, x_7))));
    y[0] = t.getLowBits() & P26;
    t = t.shiftRight(26).add(Long.fromInt(2).multiply(mul32_to64(x_0, x_1)).
        add(Long.fromInt(38).multiply(mul32_to64(x_2, x_9).add(mul32_to64(x_3, x_8)).add(mul32_to64(x_4, x_7)).add(mul32_to64(x_5, x_6)))));
    y[1] = t.getLowBits() & P25;
    t = t.shiftRight(25).add(Long.fromInt(19).multiply(mul32_to64(x_6, x_6))).
        add(Long.fromInt(2).multiply(mul32_to64(x_0, x_2).add(mul32_to64(x_1, x_1)))).
        add(Long.fromInt(38).multiply(mul32_to64(x_4, x_8))).
        add(Long.fromInt(76).multiply(mul32_to64(x_3, x_9).add(mul32_to64(x_5, x_7))));
    y[2] = t.getLowBits() & P26;
    t = t.shiftRight(26).
        add(Long.fromInt(2).multiply(mul32_to64(x_0, x_3).add(mul32_to64(x_1, x_2)))).
        add(Long.fromInt(38).multiply(mul32_to64(x_4, x_9).add(mul32_to64(x_5, x_8)).add(mul32_to64(x_6, x_7))));
    y[3] = t.getLowBits() & P25;
    t = t.shiftRight(25).add(mul32_to64(x_2, x_2)).
        add(Long.fromInt(2).multiply(mul32_to64(x_0, x_4))).
        add(Long.fromInt(38).multiply(mul32_to64(x_6, x_8).add(mul32_to64(x_7, x_7)))).
        add(Long.fromInt(4).multiply(mul32_to64(x_1, x_3))).
        add(Long.fromInt(76).multiply(mul32_to64(x_5, x_9)));
    y[4] = t.getLowBits() & P26;
    t = t.shiftRight(26).
        add(Long.fromInt(2).multiply(mul32_to64(x_0, x_5).add(mul32_to64(x_1, x_4)).add(mul32_to64(x_2, x_3)))).
        add(Long.fromInt(38).multiply(mul32_to64(x_6, x_9).add(mul32_to64(x_7, x_8))));
    y[5] = t.getLowBits() & P25;
    t = t.shiftRight(25).
        add(Long.fromInt(19).multiply(mul32_to64(x_8, x_8))).
        add(Long.fromInt(2).multiply(mul32_to64(x_0, x_6).add(mul32_to64(x_2, x_4)).add(mul32_to64(x_3, x_3)))).
        add(Long.fromInt(4).multiply(mul32_to64(x_1, x_5))).
        add(Long.fromInt(76).multiply(mul32_to64(x_7, x_9)));
    y[6] = t.getLowBits() & P26;
    t = t.shiftRight(26).
        add(Long.fromInt(2).multiply(mul32_to64(x_0, x_7).add(mul32_to64(x_1, x_6)).add(mul32_to64(x_2, x_5)).add(mul32_to64(x_3, x_4)))).
        add(Long.fromInt(38).multiply(mul32_to64(x_8, x_9)));
    y[7] = t.getLowBits() & P25;
    t = t.shiftRight(25).add(Long.fromInt(y[8]));
    y[8] = t.getLowBits() & P26;
    y[9] += t.shiftRight(26).getLowBits();
}

function recip(y, x, sqrtassist)
{
    var t0 = int10(); var t1 = int10();  var t2 = int10();    var t3 = int10(); var t4 = int10();

    /* the chain for x^(2^255-21) is straight from djb's implementation */
    sqr(t1, x);	/*  2 == 2 * 1	*/
    sqr(t2, t1);	/*  4 == 2 * 2	*/
    sqr(t0, t2);	/*  8 == 2 * 4	*/
    mul(t2, t0, x);	/*  9 == 8 + 1	*/
    mul(t0, t2, t1);	/* 11 == 9 + 2	*/
    sqr(t1, t0);	/* 22 == 2 * 11	*/
    mul(t3, t1, t2);	/* 31 == 22 + 9
 == 2^5   - 2^0	*/
    sqr(t1, t3);	/* 2^6   - 2^1	*/
    sqr(t2, t1);	/* 2^7   - 2^2	*/
    sqr(t1, t2);	/* 2^8   - 2^3	*/
    sqr(t2, t1);	/* 2^9   - 2^4	*/
    sqr(t1, t2);	/* 2^10  - 2^5	*/
    mul(t2, t1, t3);	/* 2^10  - 2^0	*/
    sqr(t1, t2);	/* 2^11  - 2^1	*/
    sqr(t3, t1);	/* 2^12  - 2^2	*/
    for (var i = 1; i < 5; i++)
    {
        sqr(t1, t3);
        sqr(t3, t1);
    } /* t3 */		/* 2^20  - 2^10	*/
    mul(t1, t3, t2);	/* 2^20  - 2^0	*/
    sqr(t3, t1);	/* 2^21  - 2^1	*/
    sqr(t4, t3);	/* 2^22  - 2^2	*/
    for (var i = 1; i < 10; i++)
    {
        sqr(t3, t4);
        sqr(t4, t3);
    } /* t4 */		/* 2^40  - 2^20	*/
    mul(t3, t4, t1);	/* 2^40  - 2^0	*/
    for (var i = 0; i < 5; i++) {
    sqr(t1, t3);
    sqr(t3, t1);
} /* t3 */		/* 2^50  - 2^10	*/
    mul(t1, t3, t2);	/* 2^50  - 2^0	*/
    sqr(t2, t1);	/* 2^51  - 2^1	*/
    sqr(t3, t2);	/* 2^52  - 2^2	*/
    for (var i = 1; i < 25; i++)
    {
        sqr(t2, t3);
        sqr(t3, t2);
    } /* t3 */		/* 2^100 - 2^50 */
    mul(t2, t3, t1);	/* 2^100 - 2^0	*/
    sqr(t3, t2);	/* 2^101 - 2^1	*/
    sqr(t4, t3);	/* 2^102 - 2^2	*/
    for (var i = 1; i < 50; i++)
    {
        sqr(t3, t4);
        sqr(t4, t3);
    } /* t4 */		/* 2^200 - 2^100 */
    mul(t3, t4, t2);	/* 2^200 - 2^0	*/
    for (var i = 0; i < 25; i++)
    {
        sqr(t4, t3);
        sqr(t3, t4);
    } /* t3 */		/* 2^250 - 2^50	*/
    mul(t2, t3, t1);	/* 2^250 - 2^0	*/
    sqr(t1, t2);	/* 2^251 - 2^1	*/
    sqr(t2, t1);	/* 2^252 - 2^2	*/
    if (sqrtassist!=0)
    {
        mul(y, x, t2);	/* 2^252 - 3 */
    }
    else
    {
        sqr(t1, t2);	/* 2^253 - 2^3	*/
        sqr(t2, t1);	/* 2^254 - 2^4	*/
        sqr(t1, t2);	/* 2^255 - 2^5	*/
        mul(y, t1, t0);	/* 2^255 - 21	*/
    }
}

/* checks if x is "negative", requires reduced input */
function is_negative(x)
{
    return (((is_overflow(x) || (x[9] < 0))?1:0) ^ (x[0] & 1));
}

function sqrt(x, u)
{
    var v = int10();  var t1 = int10();    var t2 = int10();

    add(t1, u, u);	/* t1 = 2u		*/
    recip(v, t1, 1);	/* v = (2u)^((p-5)/8)	*/
    sqr(x, v);		/* x = v^2		*/
    mul(t2, t1, x);	/* t2 = 2uv^2		*/
    --t2[0];		/* t2 = 2uv^2-1		*/
    mul(t1, v, t2);	/* t1 = v(2uv^2-1)	*/
    mul(x, u, t1);	/* x = uv(2uv^2-1)	*/
}


/********************* Elliptic curve *********************/

/* y^2 = x^3 + 486662 x^2 + x  over GF(2^255-19) */

/* t1 = ax + az
 * t2 = ax - az  */
function mont_prep(t1, t2, ax, az)
{
    add(t1, ax, az);
    sub(t2, ax, az);
}

/* A = P + Q   where
 *  X(A) = ax/az
 *  X(P) = (t1+t2)/(t1-t2)
 *  X(Q) = (t3+t4)/(t3-t4)
 *  X(P-Q) = dx
 * clobbers t1 and t2, preserves t3 and t4  */
function mont_add(t1, t2, t3, t4, ax, az, dx)
{
    mul(ax, t2, t3);
    mul(az, t1, t4);
    add(t1, ax, az);
    sub(t2, ax, az);
    sqr(ax, t1);
    sqr(t1, t2);
    mul(az, t1, dx);
}

/* B = 2 * Q   where
 *  X(B) = bx/bz
 *  X(Q) = (t3+t4)/(t3-t4)
 * clobbers t1 and t2, preserves t3 and t4  */
function mont_dbl(t1, t2, t3, t4, bx, bz)
{
    sqr(t1, t3);
    sqr(t2, t4);
    mul(bx, t1, t2);
    sub(t2, t1, t2);
    mul_small(bz, t2, 121665);
    add(t1, t1, bz);
    mul(bz, t1, t2);
}

/* Y^2 = X^3 + 486662 X^2 + X
 * t is a temporary  */
function x_to_y2(t, y2, x)
{
    sqr(t, x);
    mul_small(y2, x, 486662);
    add(t, t, y2);
    ++t[0];
    mul(y2, t, x);
}

/* P = kG   and  s = sign(P)/k  */
function curve25519_core(Px, s, k, Gx)
{
    var dx = int10(); var t1 = int10(); var t2 = int10(); var t3 = int10(); var t4 = int10();

    var x = new Array(int10(), int10());
    var z = new Array(int10(), int10());

    /* unpack the base */
    if (Gx)
        unpack(dx, Gx);
    else
        set(dx, 9);

    /* 0G = point-at-infinity */
    set(x[0], 1);
    set(z[0], 0);

    /* 1G = G */
    cpy(x[1], dx);
    set(z[1], 1);

    for (var i = 32; i-- > 0; )
    {
        for (var j = 8; j--> 0; )
        {
            /* swap arguments depending on bit */
            var bit1 = ((k[i] & 0xFF) >> j) & 1;
            var bit0 = (~(k[i] & 0xFF) >> j) & 1;

            var ax = x[bit0];
            var az = z[bit0];
            var bx = x[bit1];
            var bz = z[bit1];

            /* a' = a + b	*/
            /* b' = 2 b	*/
            mont_prep(t1, t2, ax, az);
            mont_prep(t3, t4, bx, bz);
            mont_add(t1, t2, t3, t4, ax, az, dx);
            mont_dbl(t1, t2, t3, t4, bx, bz);
        }
    }

    recip(t1, z[0], 0);
    mul(dx, x[0], t1);
    pack(dx, Px);

    /* calculate s such that s abs(P) = G  .. assumes G is std base point */
    if (s !== null)
    {
        x_to_y2(t2, t1, dx);	/* t1 = Py^2  */
        recip(t3, z[1], 0);	/* where Q=P+G ... */
        mul(t2, x[1], t3);	/* t2 = Qx  */
        add(t2, t2, dx);	/* t2 = Qx + Px  */
        t2[0] += 9 + 486662;	/* t2 = Qx + Px + Gx + 486662  */
        dx[0] -= 9;		/* dx = Px - Gx  */
        sqr(t3, dx);	/* t3 = (Px - Gx)^2  */
        mul(dx, t2, t3);	/* dx = t2 (Px - Gx)^2  */
        sub(dx, dx, t1);	/* dx = t2 (Px - Gx)^2 - Py^2  */
        dx[0] -= 39420360;	/* dx = t2 (Px - Gx)^2 - Py^2 - Gy^2  */
        mul(t1, dx, BASE_R2Y);	/* t1 = -Py  */
        if (is_negative(t1)!=0)	/* sign is 1, so just copy  */
            cpy32(s, k);
        else			/* sign is -1, so negate  */
            mula_small(s, ORDER_TIMES_8, 0, k, 32, -1);

        /* reduce s mod q
         * (is this needed?  do it just in case, it's fast anyway) */
        //divmod((dstptr) t1, s, 32, order25519, 32);

        /* take reciprocal of s mod q */
        temp1= new Array(32);
        temp2= new Array(64);
        temp3= new Array(64);
        cpy32(temp1, ORDER);
        cpy32(s, egcd32(temp2, temp3, s, temp1));
        if ((s[31] & 0x80)!=0)
            mula_small(s, s, 0, ORDER, 32, 1);
    }
}

/********* DIGITAL SIGNATURES *********/

/* deterministic EC-KCDSA
 *
 *    s is the private key for signing
 *    P is the corresponding public key
 *    Z is the context data (signer public key or certificate, etc)
 *
 * signing:
 *
 *    m = hash(Z, message)
 *    x = hash(m, s)
 *    keygen25519(Y, NULL, x);
 *    r = hash(Y);
 *    h = m XOR r
 *    sign25519(v, h, x, s);
 *
 *    output (v,r) as the signature
 *
 * verification:
 *
 *    m = hash(Z, message);
 *    h = m XOR r
 *    verify25519(Y, v, h, P)
 *
 *    confirm  r == hash(Y)
 *
 * It would seem to me that it would be simpler to have the signer directly do
 * h = hash(m, Y) and send that to the recipient instead of r, who can verify
 * the signature by checking h == hash(m, Y).  If there are any problems with
 * such a scheme, please let me know.
 *
 * Also, EC-KCDSA (like most DS algorithms) picks x random, which is a waste of
 * perfectly good entropy, but does allow Y to be calculated in advance of (or
 * parallel to) hashing the message.
 */

/* Signature generation primitive, calculates (x-h)s mod q
 *   v  [out] signature value
 *   h  [in]  signature hash (of message, signature pub key, and context data)
 *   x  [in]  signature private key
 *   s  [in]  private key for signing
 * returns true on success, false on failure (use different x or h)
 */
/*bool*/ function curve25519_sign(v, h, x, s)
{
    /* v = (x - h) s  mod q  */
    var tmp1 = new Array(65);
    var tmp2 = new Array(33);
    var w;
    var i;
    for (i = 0; i < 32; i++)
        v[i] = 0;
    i = mula_small(v, x, 0, h, 32, -1);
    mula_small(v, v, 0, ORDER, 32, Math.floor((15-v[31])/16));
    mula32(tmp1, v, s, 32, 1);
    divmod(tmp2, tmp1, 64, ORDER, 32);
    for (w = 0, i = 0; i < 32; i++)
        w |= v[i] = tmp1[i];
    return w != 0;
}

/* Signature verification primitive, calculates Y = vP + hG
 *   Y  [out] signature public key
 *   v  [in]  signature value
 *   h  [in]  signature hash
 *   P  [in]  public key
 */
function curve25519_verify(Y, v, h, P)
{
    /* Y = v abs(P) + h G  */
    var d = new Array(32);

    var p = new Array(int10(), int10());
    var s = new Array(int10(), int10());
    var yx = new Array(int10(), int10(), int10());
    var yz = new Array(int10(), int10(), int10());
    var t1 = new Array(int10(), int10(), int10());
    var t2 = new Array(int10(), int10(), int10());

    var vi = 0, hi = 0, di = 0, nvh=0, i, j, k;

    /* set p[0] to G and p[1] to P  */

    set(p[0], 9);
    unpack(p[1], P);

    /* set s[0] to P+G and s[1] to P-G  */

    /* s[0] = (Py^2 + Gy^2 - 2 Py Gy)/(Px - Gx)^2 - Px - Gx - 486662  */
    /* s[1] = (Py^2 + Gy^2 + 2 Py Gy)/(Px - Gx)^2 - Px - Gx - 486662  */

    x_to_y2(t1[0], t2[0], p[1]);	/* t2[0] = Py^2  */
    sqrt(t1[0], t2[0]);	/* t1[0] = Py or -Py  */
    j = is_negative(t1[0]);		/*      ... check which  */
    t2[0][0] += 39420360;		/* t2[0] = Py^2 + Gy^2  */
    mul(t2[1], BASE_2Y, t1[0]);/* t2[1] = 2 Py Gy or -2 Py Gy  */
    sub(t1[j], t2[0], t2[1]);	/* t1[0] = Py^2 + Gy^2 - 2 Py Gy  */
    add(t1[1-j], t2[0], t2[1]);/* t1[1] = Py^2 + Gy^2 + 2 Py Gy  */
    cpy(t2[0],p[1]);		/* t2[0] = Px  */
    t2[0][0] -= 9;			/* t2[0] = Px - Gx  */
    sqr(t2[1], t2[0]);		/* t2[1] = (Px - Gx)^2  */
    recip(t2[0], t2[1], 0);	/* t2[0] = 1/(Px - Gx)^2  */
    mul(s[0], t1[0], t2[0]);	/* s[0] = t1[0]/(Px - Gx)^2  */
    sub(s[0], s[0], p[1]);	/* s[0] = t1[0]/(Px - Gx)^2 - Px  */
    s[0][0] -= 9 + 486662;		/* s[0] = X(P+G)  */
    mul(s[1], t1[1], t2[0]);	/* s[1] = t1[1]/(Px - Gx)^2  */
    sub(s[1], s[1], p[1]);	/* s[1] = t1[1]/(Px - Gx)^2 - Px  */
    s[1][0] -= 9 + 486662;		/* s[1] = X(P-G)  */
    mul_small(s[0], s[0], 1);	/* reduce s[0] */
    mul_small(s[1], s[1], 1);	/* reduce s[1] */


    /* prepare the chain  */
    for (i = 0; i < 32; i++)
    {
        vi = (vi >> 8) ^ (v[i] & 0xFF) ^ ((v[i] & 0xFF) << 1);
        hi = (hi >> 8) ^ (h[i] & 0xFF) ^ ((h[i] & 0xFF) << 1);
        nvh = ~(vi ^ hi);
        di = (nvh & (di & 0x80) >> 7) ^ vi;
        di ^= nvh & (di & 0x01) << 1;
        di ^= nvh & (di & 0x02) << 1;
        di ^= nvh & (di & 0x04) << 1;
        di ^= nvh & (di & 0x08) << 1;
        di ^= nvh & (di & 0x10) << 1;
        di ^= nvh & (di & 0x20) << 1;
        di ^= nvh & (di & 0x40) << 1;
        d[i] = di & 0xFF;
    }

    di = ((nvh & (di & 0x80) << 1) ^ vi) >> 8;

    /* initialize state */
    set(yx[0], 1);
    cpy(yx[1], p[di]);
    cpy(yx[2], s[0]);
    set(yz[0], 0);
    set(yz[1], 1);
    set(yz[2], 1);

    /* y[0] is (even)P + (even)G
     * y[1] is (even)P + (odd)G  if current d-bit is 0
     * y[1] is (odd)P + (even)G  if current d-bit is 1
     * y[2] is (odd)P + (odd)G
     */

    vi = 0;
    hi = 0;

    /* and go for it! */
    for (i = 32; i-- > 0; )
    {
        vi = (vi << 8) | (v[i] & 0xFF);
        hi = (hi << 8) | (h[i] & 0xFF);
        di = (di << 8) | (d[i] & 0xFF);

        for (j = 8; j-- > 0; )
        {
            mont_prep(t1[0], t2[0], yx[0], yz[0]);
            mont_prep(t1[1], t2[1], yx[1], yz[1]);
            mont_prep(t1[2], t2[2], yx[2], yz[2]);

            k = ((vi ^ vi >> 1) >> j & 1)
                + ((hi ^ hi >> 1) >> j & 1);
            mont_dbl(yx[2], yz[2], t1[k], t2[k], yx[0], yz[0]);

            k = (di >> j & 2) ^ ((di >> j & 1) << 1);
            mont_add(t1[1], t2[1], t1[k], t2[k], yx[1], yz[1],
                p[di >> j & 1]);

            mont_add(t1[2], t2[2], t1[0], t2[0], yx[2], yz[2],
                s[((vi ^ hi) >> j & 2) >> 1]);
        }
    }

    k = (vi & 1) + (hi & 1);
    recip(t1[0], yz[k], 0);
    mul(t1[1], yx[k], t1[0]);

    pack(t1[1], Y);
}


function curve25519_clamp(k)
{
    k[31] &= 0x7F;
    k[31] |= 0x40;
    k[ 0] &= 0xF8;
}

/* Key-pair generation
 *   P  [out] your public key
 *   s  [out] your private key for signing
 *   k  [out] your private key for key agreement
 *   k  [in]  32 random bytes
 * s may be NULL if you don't care
 *
 * WARNING: if s is not NULL, this function has data-dependent timing */
function curve25519_keygen(P, s, k)
{
    curve25519_clamp(k);
    curve25519_core(P, s, k, 0);
}

/* Key agreement
 *   Z  [out] shared secret (needs hashing before use)
 *   k  [in]  your private key for key agreement
 *   P  [in]  peer's public key
 */
function curve25519_curve(Z, k, P)
{
    curve25519_core(Z, null, k, P);
}

// Crypto
function crypto_sign(message, secretPhrase) {
    var P = new Array(32);
    var s = new Array(32);
    var k = SHA256_hash(secretPhrase);
    curve25519_keygen(P, s, k);

    var m = SHA256_hash(message);
    SHA256_init();
    SHA256_write(m, m.length);
    SHA256_write(s, s.length);
    var x = SHA256_finalize();

    var Y = new Array(32);
    curve25519_keygen(Y, null, x);

    SHA256_init();
    SHA256_write(m, m.length);
    SHA256_write(Y, Y.length);
    var h = SHA256_finalize();

    var v = new Array(32);
    curve25519_sign(v, h, x, s);

    var signature = v.concat(h);
    return array_to_hex_string(signature);
}

function crypto_verify(signature, message, publicKey) {
    hex_str_to_array = function (str) {
        return str.match(/../g).map( function(chars){ return parseInt(chars,16) });
    }

    signature = hex_str_to_array(signature);
    message = hex_str_to_array(message);
    publicKey = hex_str_to_array(publicKey);

    var Y = new Array(32);
    var v = signature.slice(0, 32);
    var h = signature.slice(32, 64);

    curve25519_verify(Y, v, h, publicKey);

    var m = SHA256_hash(message);
    SHA256_init();
    SHA256_write(m, m.length);
    SHA256_write(Y, Y.length);
    var h2 = SHA256_finalize();

    // compare results
    for (var i = 0; i < 32; i++)
        if (h[i] != h2[i])
            return false;
    return true;
}