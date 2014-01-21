// based on source of Long class from Google Closure Library

Long = function(low, high) {
    this.low_ = low | 0;  // force into 32 signed bits.
    this.high_ = high | 0;  // force into 32 signed bits.
};

Long.fromInt = function(value) {
    return new Long(value | 0, value < 0 ? -1 : 0);
};

Long.fromBits = function(lowBits, highBits) {
    return new Long(lowBits, highBits);
};

Long.prototype.toInt = function() {
    return this.low_;
};

Long.prototype.getLowBits = function() {
    return this.low_;
};

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

Long.prototype.multiply = function(other) {
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

// multiple by small number (16bit)
Long.prototype.multiplySmall = function(s) {
    var a48 = this.high_ >>> 16;
    var a32 = this.high_ & 0xFFFF;
    var a16 = this.low_ >>> 16;
    var a00 = this.low_ & 0xFFFF;

    var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
    c00 += a00 * s;
    c16 += c00 >>> 16;
    c00 &= 0xFFFF;
    c16 += a16 * s;
    c32 += c16 >>> 16;
    c16 &= 0xFFFF;
    c32 += a32 * s;
    c48 += c32 >>> 16;
    c32 &= 0xFFFF;
    c48 += a48 * s;
    c48 &= 0xFFFF;
    return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

// assume that numBits < 32
Long.prototype.shiftLeft = function(numBits) {
    return Long.fromBits(
        this.low_ << numBits,
        (this.high_ << numBits) | (this.low_ >>> (32 - numBits)));
};

// assume that numBits < 32
Long.prototype.shiftRight = function(numBits) {
    return Long.fromBits(
        (this.low_ >>> numBits) | (this.high_ << (32 - numBits)),
        this.high_ >> numBits);
};


// jssha256 version 0.1  -  Copyright 2006 B. Poettering (GNU GPL)
// http://point-at-infinity.org/jssha256/

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
function int10(val) {
    return [val || 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
}

// Ported from c++ to js
// Original: https://code.google.com/p/gvl/source/browse/crypt/curve25519.cpp?r=3c26171fd2d914484f03b7b63ca2065fa03a7aa3 (New BSD License)
KEY_SIZE = 32;

/* 0 */
ZERO = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
];

/* the prime 2^255-19 */
PRIME = [
    237, 255, 255, 255,
    255, 255, 255, 255,
    255, 255, 255, 255,
    255, 255, 255, 255,
    255, 255, 255, 255,
    255, 255, 255, 255,
    255, 255, 255, 255,
    255, 255, 255, 127
];

/* group order (a prime near 2^252+2^124) */
ORDER = [
    237, 211, 245, 92,
    26,  99,  18,  88,
    214, 156, 247, 162,
    222, 249, 222, 20,
    0,   0,   0,   0,
    0,   0,   0,   0,
    0,   0,   0,   0,
    0,   0,   0,   16
];

/* smallest multiple of the order that's >= 2^255 */
ORDER_TIMES_8 = [
    104, 159, 174, 231,
    210, 24,  147, 192,
    178, 230, 188, 23,
    245, 206, 247, 166,
    0,   0,   0,   0,
    0,   0,   0,   0,
    0,   0,   0,   0,
    0,   0,   0,   128
];

/* constants 2Gy and 1/(2Gy) */
BASE_2Y = [
    39999547, 18689728, 59995525, 1648697, 57546132,
    24010086, 19059592, 5425144, 63499247, 16420658
];

BASE_R2Y = [
    5744, 8160848, 4790893, 13779497, 35730846,
    12541209, 49101323, 30047407, 40071253, 6226132
];

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
    var t = ld + x[0] + (x[1] * 0x4000000);
    m[ 0] = t & 0xFF;
    m[ 1] = Math.floor(t / 0x100) & 0xFF;
    m[ 2] = Math.floor(t / 0x10000) & 0xFF;
    m[ 3] = Math.floor(t / 0x1000000) & 0xFF;
    t = Math.floor(t / 0x100000000) + (x[2] * 0x80000);
    m[ 4] = t & 0xFF;
    m[ 5] = Math.floor(t / 0x100) & 0xFF;
    m[ 6] = Math.floor(t / 0x10000) & 0xFF;
    m[ 7] = Math.floor(t / 0x1000000) & 0xFF;
    t = Math.floor(t / 0x100000000) + (x[3] * 0x2000);
    m[ 8] = t & 0xFF;
    m[ 9] = Math.floor(t / 0x100) & 0xFF;
    m[10] = Math.floor(t / 0x10000) & 0xFF;
    m[11] = Math.floor(t / 0x1000000) & 0xFF;
    t = Math.floor(t / 0x100000000) + (x[4] * 0x40);
    m[12] = t & 0xFF;
    m[13] = Math.floor(t / 0x100) & 0xFF;
    m[14] = Math.floor(t / 0x10000) & 0xFF;
    m[15] = Math.floor(t / 0x1000000) & 0xFF;
    t = Math.floor(t / 0x100000000) + x[5] + (x[6] * 0x2000000);
    m[16] = t & 0xFF;
    m[17] = Math.floor(t / 0x100) & 0xFF;
    m[18] = Math.floor(t / 0x10000) & 0xFF;
    m[19] = Math.floor(t / 0x1000000) & 0xFF;
    t = Math.floor(t / 0x100000000) + (x[7] * 0x80000);
    m[20] = t & 0xFF;
    m[21] = Math.floor(t / 0x100) & 0xFF;
    m[22] = Math.floor(t / 0x10000) & 0xFF;
    m[23] = Math.floor(t / 0x1000000) & 0xFF;
    t = Math.floor(t / 0x100000000) + (x[8] * 0x1000);
    m[24] = t & 0xFF;
    m[25] = Math.floor(t / 0x100) & 0xFF;
    m[26] = Math.floor(t / 0x10000) & 0xFF;
    m[27] = Math.floor(t / 0x1000000) & 0xFF;
    t = Math.floor(t / 0x100000000) + ((x[9] + ud) * 0x40);
    m[28] = t & 0xFF;
    m[29] = Math.floor(t / 0x100) & 0xFF;
    m[30] = Math.floor(t / 0x10000) & 0xFF;
    m[31] = Math.floor(t / 0x1000000) & 0xFF;
}

function cpy(d, s)
{
    for (var j = 0; j < 10; j++)
        d[j] = s[j];
}

function add(xy, x, y)
{
    for (var j = 0; j < 10; j++)
        xy[j] = x[j] + y[j];
}

function sub(xy, x, y)
{
    for (var j = 0; j < 10; j++)
        xy[j] = x[j] - y[j];
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
    temp= new Array(32);
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
    var t = x[8]*y;
    xy[8] = t & P26;
    t = Math.floor(t / 0x4000000) + x[9]*y;
    xy[9] = t & P25;
    t = 19*Math.floor(t / 0x2000000) + x[0]*y;
    xy[0] = t & P26;
    t = Math.floor(t / 0x4000000) + x[1]*y;
    xy[1] = t & P25;
    t = Math.floor(t / 0x2000000) + x[2]*y;
    xy[2] = t & P26;
    t = Math.floor(t / 0x4000000) + x[3]*y;
    xy[3] = t & P25;
    t = Math.floor(t / 0x2000000) + x[4]*y;
    xy[4] = t & P26;
    t = Math.floor(t / 0x4000000) + x[5]*y;
    xy[5] = t & P25;
    t = Math.floor(t / 0x2000000) + x[6]*y;
    xy[6] = t & P26;
    t = Math.floor(t / 0x4000000) + x[7]*y;
    xy[7] = t & P25;
    t = Math.floor(t / 0x2000000) + xy[8];
    xy[8] = t & P26;
    xy[9] += Math.floor(t / 0x4000000);
}

function mul(dest, x, y)
{
    var x_0 = Long.fromInt(x[0]), x_1 = Long.fromInt(x[1]), x_2 = Long.fromInt(x[2]), x_3 = Long.fromInt(x[3]), x_4 = Long.fromInt(x[4]),
        x_5 = Long.fromInt(x[5]), x_6 = Long.fromInt(x[6]), x_7 = Long.fromInt(x[7]), x_8 = Long.fromInt(x[8]), x_9 = Long.fromInt(x[9]);
    var y_0 = Long.fromInt(y[0]), y_1 = Long.fromInt(y[1]), y_2 = Long.fromInt(y[2]), y_3 = Long.fromInt(y[3]), y_4 = Long.fromInt(y[4]),
        y_5 = Long.fromInt(y[5]), y_6 = Long.fromInt(y[6]), y_7 = Long.fromInt(y[7]), y_8 = Long.fromInt(y[8]), y_9 = Long.fromInt(y[9]);

    var t = x_0.multiply(y_8).add(x_2.multiply(y_6)).add(x_4.multiply(y_4)).add(x_6.multiply(y_2)).add(x_8.multiply(y_0));
    t = t.add(x_1.multiply(y_7).add(x_3.multiply(y_5)).add(x_5.multiply(y_3)).add(x_7.multiply(y_1)).multiplySmall(2));
    t = t.add(x_9.multiply(y_9).multiplySmall(38));
    dest[8] = t.getLowBits() & P26;
    t = t.shiftRight(26).add(x_0.multiply(y_9)).add(x_1.multiply(y_8)).add( x_2.multiply(y_7)).
        add(x_3.multiply(y_6)).add(x_4.multiply(y_5)).add(x_5.multiply(y_4)).
        add(x_6.multiply(y_3)).add(x_7.multiply(y_2)).add(x_8.multiply(y_1)).
        add(x_9.multiply(y_0));
    dest[9] = t.getLowBits() & P25;
    t = x_0.multiply(y_0).
        add(t.shiftRight(25).add(x_2.multiply(y_8)).add(x_4.multiply(y_6)).add(x_6.multiply(y_4)).add(x_8.multiply(y_2)).multiplySmall(19)).
        add(x_1.multiply(y_9).add(x_3.multiply(y_7)).add(x_5.multiply(y_5)).add(x_7.multiply(y_3)).add(x_9.multiply(y_1)).multiplySmall(38));
    dest[0] = t.getLowBits() & P26;
    t = t.shiftRight(26).add(x_0.multiply(y_1)).add(x_1.multiply(y_0)).
        add(x_2.multiply(y_9).add(x_3.multiply(y_8)).add(x_4.multiply(y_7)).add(x_5.multiply(y_6)).add(x_6.multiply(y_5)).
            add(x_7.multiply(y_4)).add(x_8.multiply(y_3)).add(x_9.multiply(y_2)).multiplySmall(19));
    dest[1] = t.getLowBits() & P25;
    t = t.shiftRight(25).add(x_0.multiply(y_2)).add(x_2.multiply(y_0)).
        add(x_4.multiply(y_8).add(x_6.multiply(y_6)).add(x_8.multiply(y_4)).multiplySmall(19)).
        add(x_1.multiply(y_1).multiplySmall(2)).
        add(x_3.multiply(y_9).add(x_5.multiply(y_7)).add(x_7.multiply(y_5)).add(x_9.multiply(y_3)).multiplySmall(38));
    dest[2] = t.getLowBits() & P26;
    t = t.shiftRight(26).add(x_0.multiply(y_3)).add(x_1.multiply(y_2)).add(x_2.multiply(y_1)).add(x_3.multiply(y_0)).
        add(x_4.multiply(y_9).add(x_5.multiply(y_8)).add(x_6.multiply(y_7)).add(x_7.multiply(y_6)).add(x_8.multiply(y_5)).add(x_9.multiply(y_4)).multiplySmall(19));
    dest[3] = t.getLowBits() & P25;
    t = t.shiftRight(25).add(x_0.multiply(y_4)).add(x_2.multiply(y_2)).add(x_4.multiply(y_0)).
        add(x_6.multiply(y_8).add(x_8.multiply(y_6)).multiplySmall(19)).
        add(x_1.multiply(y_3).add(x_3.multiply(y_1)).multiplySmall(2)).
        add(x_5.multiply(y_9).add(x_7.multiply(y_7)).add(x_9.multiply(y_5)).multiplySmall(38));
    dest[4] = t.getLowBits() & P26;
    t = t.shiftRight(26).add(x_0.multiply(y_5)).add(x_1.multiply(y_4)).add(x_2.multiply(y_3)).
        add(x_3.multiply(y_2)).add(x_4.multiply(y_1)).add(x_5.multiply(y_0)).
        add(x_6.multiply(y_9).add(x_7.multiply(y_8)).add(x_8.multiply(y_7)).add(x_9.multiply(y_6)).multiplySmall(19));
    dest[5] = t.getLowBits() & P25;
    t = t.shiftRight(25).add(x_0.multiply(y_6)).add(x_2.multiply(y_4)).add(x_4.multiply(y_2)).add(x_6.multiply(y_0)).
        add(x_8.multiply(y_8).multiplySmall(19)).
        add(x_1.multiply(y_5).add(x_3.multiply(y_3)).add(x_5.multiply(y_1)).multiplySmall(2)).
        add(x_7.multiply(y_9).add(x_9.multiply(y_7)).multiplySmall(38));
    dest[6] = t.getLowBits() & P26;
    t = t.shiftRight(26).add(x_0.multiply(y_7)).add(x_1.multiply(y_6)).add(x_2.multiply(y_5)).add(x_3.multiply(y_4)).
        add(x_4.multiply(y_3)).add(x_5.multiply(y_2)).add(x_6.multiply(y_1)).add(x_7.multiply(y_0)).
        add(x_8.multiply(y_9).add(x_9.multiply(y_8)).multiplySmall(19));
    dest[7] = t.getLowBits() & P25;
    t = t.shiftRight(25).add(Long.fromInt(dest[8]));
    dest[8] = t.getLowBits() & P26;
    dest[9] += t.shiftRight(26).getLowBits();
}

function sqr(y, x)
{
    var x_0 = Long.fromInt(x[0]), x_1 = Long.fromInt(x[1]), x_2 = Long.fromInt(x[2]), x_3 = Long.fromInt(x[3]), x_4 = Long.fromInt(x[4]),
        x_5 = Long.fromInt(x[5]), x_6 = Long.fromInt(x[6]), x_7 = Long.fromInt(x[7]), x_8 = Long.fromInt(x[8]), x_9 = Long.fromInt(x[9]);

    var t = x_4.multiply(x_4).
        add(x_0.multiply(x_8).add(x_2.multiply(x_6)).multiplySmall(2)).
        add(x_9.multiply(x_9).multiplySmall(38)).add(x_1.multiply(x_7).add(x_3.multiply(x_5)).multiplySmall(4));
    y[8] = t.getLowBits() & P26;
    t = t.shiftRight(26).add(x_0.multiply(x_9).add(x_1.multiply(x_8)).add(x_2.multiply(x_7)).
        add(x_3.multiply(x_6)).add(x_4.multiply(x_5)).multiplySmall(2));
    y[9] = t.getLowBits() & P25;
    t = t.shiftRight(25).multiplySmall(19).add(x_0.multiply(x_0)).
        add(x_2.multiply(x_8).add(x_4.multiply(x_6).add(x_5.multiply(x_5))).multiplySmall(38)).
        add(x_1.multiply(x_9).add(x_3.multiply(x_7)).multiplySmall(76));
    y[0] = t.getLowBits() & P26;
    t = t.shiftRight(26).add(x_0.multiply(x_1).multiplySmall(2).
        add(x_2.multiply(x_9).add(x_3.multiply(x_8)).add(x_4.multiply(x_7)).add(x_5.multiply(x_6)).multiplySmall(38)));
    y[1] = t.getLowBits() & P25;
    t = t.shiftRight(25).add(x_6.multiply(x_6).multiplySmall(19)).
        add(x_0.multiply(x_2).add(x_1.multiply(x_1)).multiplySmall(2)).
        add(x_4.multiply(x_8).multiplySmall(38)).
        add(x_3.multiply(x_9).add(x_5.multiply(x_7)).multiplySmall(76));
    y[2] = t.getLowBits() & P26;
    t = t.shiftRight(26).
        add(x_0.multiply(x_3).add(x_1.multiply(x_2)).multiplySmall(2)).
        add(x_4.multiply(x_9).add(x_5.multiply(x_8)).add(x_6.multiply(x_7)).multiplySmall(38));
    y[3] = t.getLowBits() & P25;
    t = t.shiftRight(25).add(x_2.multiply(x_2)).
        add(x_0.multiply(x_4).multiplySmall(2)).
        add(x_6.multiply(x_8).add(x_7.multiply(x_7)).multiplySmall(38)).
        add(x_1.multiply(x_3).multiplySmall(4)).
        add(x_5.multiply(x_9).multiplySmall(76));
    y[4] = t.getLowBits() & P26;
    t = t.shiftRight(26).
        add(x_0.multiply(x_5).add(x_1.multiply(x_4)).add(x_2.multiply(x_3)).multiplySmall(2)).
        add(x_6.multiply(x_9).add(x_7.multiply(x_8)).multiplySmall(38));
    y[5] = t.getLowBits() & P25;
    t = t.shiftRight(25).
        add(x_8.multiply(x_8).multiplySmall(19)).
        add(x_0.multiply(x_6).add(x_2.multiply(x_4)).add(x_3.multiply(x_3)).multiplySmall(2)).
        add(x_1.multiply(x_5).multiplySmall(4)).
        add(x_7.multiply(x_9).multiplySmall(76));
    y[6] = t.getLowBits() & P26;
    t = t.shiftRight(26).
        add(x_0.multiply(x_7).add(x_1.multiply(x_6)).add(x_2.multiply(x_5)).add(x_3.multiply(x_4)).multiplySmall(2)).
        add(x_8.multiply(x_9).multiplySmall(38));
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

    var x = [int10(), int10()];
    var z = [int10(), int10()];

    /* unpack the base */
    if (Gx)
        unpack(dx, Gx);
    else
        dx = int10(9);

    /* 0G = point-at-infinity */
    x[0] = int10(1);
    z[0] = int10();

    /* 1G = G */
    cpy(x[1], dx);
    z[1] = int10(1);

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
    var v31 = v[31];
    if ((v31 & 0x80) != 0)
        v31 |= 0xFFFFFF00;
    mula_small(v, v, 0, ORDER, 32, Math.floor((15-v31)/16));
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

    var p = [int10(), int10()];
    var s = [int10(), int10()];
    var yx = [int10(), int10(), int10()];
    var yz = [int10(), int10(), int10()];
    var t1 = [int10(), int10(), int10()];
    var t2 = [int10(), int10(), int10()];

    var vi = 0, hi = 0, di = 0, nvh=0, i, j, k;

    /* set p[0] to G and p[1] to P  */

    p[0] = int10(9);
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
    yx[0] = int10(1);
    cpy(yx[1], p[di]);
    cpy(yx[2], s[0]);
    yz[0] = int10();
    yz[1] = int10(1);
    yz[2] = int10(1);

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

hex_str_to_array = function (str) {
	return str.match(/../g).map( function(chars){ return parseInt(chars,16) });
}

// Crypto
function crypto_sign(message, secretPhrase) {
	message = hex_str_to_array(message);
    secretPhrase = hex_str_to_array(secretPhrase);

    var P = new Array(32);
    var s = new Array(32);
    var k = SHA256_hash(secretPhrase);
    curve25519_keygen(P, s, k);

    var m = SHA256_hash(message);
    SHA256_init();
    SHA256_write(m);
    SHA256_write(s);
    var x = SHA256_finalize();

    var Y = new Array(32);
    curve25519_keygen(Y, null, x);

    SHA256_init();
    SHA256_write(m);
    SHA256_write(Y);
    var h = SHA256_finalize();

    var v = new Array(32);
    curve25519_sign(v, h, x, s);

    var signature = v.concat(h);
    return array_to_hex_string(signature);
}

function crypto_verify(signature, message, publicKey) {
    signature = hex_str_to_array(signature);
    message = hex_str_to_array(message);
    publicKey = hex_str_to_array(publicKey);

    var Y = new Array(32);
    var v = signature.slice(0, 32);
    var h = signature.slice(32, 64);

    curve25519_verify(Y, v, h, publicKey);

    var m = SHA256_hash(message);
    SHA256_init();
    SHA256_write(m);
    SHA256_write(Y);
    var h2 = SHA256_finalize();

    // compare results
    for (var i = 0; i < 32; i++)
        if (h[i] != h2[i])
            return false;
    return true;
}