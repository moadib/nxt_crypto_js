
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
    22587, 610,  29883, 44076, 15515, 9479,  25859, 56197,
    23910, 4462, 17831, 16322, 62102, 36542, 52412, 16035
);

BASE_R2Y = new Array(
    5744,  16384, 61977, 54121, 8776,  18501, 26522, 34893,
    23833, 5823,  55924, 58749, 24147, 14085, 13606, 6080
);

C0 = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
C1 = [1,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
C9 = [9,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
C486671 = [0x6D0F,0x0007,0,0,0,0,0,0,0,0,0,0,0,0,0,0];
C39420360 = [0x81C8,0x0259,0,0,0,0,0,0,0,0,0,0,0,0,0,0];

function pack(x, m)
{
    for (var i = 0; i < 16; i++)
        x[i] = (m[i*2+1] << 8) | m[i*2];
}

function unpack(x, m)
{
    for (var i = 0; i < 16; i++)
    {
        m[i*2] = x[i] & 0xFF;
        m[i*2+1] = (x[i] >> 8) & 0xFF;
    }
}

function cpy(o, i)
{
    for (var j = 0; j < 16; j++)
        o[j] = i[j];
}

function add(r, a, b)
{
    var v;
    r[0] = (v = (Math.floor(a[15] / 0x8000) + Math.floor(b[15] / 0x8000)) * 19 + a[0] + b[0]) & 0xFFFF;
    r[1] = (v = Math.floor(v / 0x10000) + a[1] + b[1]) & 0xFFFF;
    r[2] = (v = Math.floor(v / 0x10000) + a[2] + b[2]) & 0xFFFF;
    r[3] = (v = Math.floor(v / 0x10000) + a[3] + b[3]) & 0xFFFF;
    r[4] = (v = Math.floor(v / 0x10000) + a[4] + b[4]) & 0xFFFF;
    r[5] = (v = Math.floor(v / 0x10000) + a[5] + b[5]) & 0xFFFF;
    r[6] = (v = Math.floor(v / 0x10000) + a[6] + b[6]) & 0xFFFF;
    r[7] = (v = Math.floor(v / 0x10000) + a[7] + b[7]) & 0xFFFF;
    r[8] = (v = Math.floor(v / 0x10000) + a[8] + b[8]) & 0xFFFF;
    r[9] = (v = Math.floor(v / 0x10000) + a[9] + b[9]) & 0xFFFF;
    r[10] = (v = Math.floor(v / 0x10000) + a[10] + b[10]) & 0xFFFF;
    r[11] = (v = Math.floor(v / 0x10000) + a[11] + b[11]) & 0xFFFF;
    r[12] = (v = Math.floor(v / 0x10000) + a[12] + b[12]) & 0xFFFF;
    r[13] = (v = Math.floor(v / 0x10000) + a[13] + b[13]) & 0xFFFF;
    r[14] = (v = Math.floor(v / 0x10000) + a[14] + b[14]) & 0xFFFF;
    r[15] = Math.floor(v / 0x10000) + a[15] % 0x8000 + b[15] % 0x8000;
}

function sub(r, a, b)
{
    var v;
    r[0] = (v = 0x80000 + (Math.floor(a[15] / 0x8000) - Math.floor(b[15] / 0x8000) - 1) * 19 + a[0] - b[0]) & 0xFFFF;
    r[1] = (v = Math.floor(v / 0x10000) + 0x7fff8 + a[1] - b[1]) & 0xFFFF;
    r[2] = (v = Math.floor(v / 0x10000) + 0x7fff8 + a[2] - b[2]) & 0xFFFF;
    r[3] = (v = Math.floor(v / 0x10000) + 0x7fff8 + a[3] - b[3]) & 0xFFFF;
    r[4] = (v = Math.floor(v / 0x10000) + 0x7fff8 + a[4] - b[4]) & 0xFFFF;
    r[5] = (v = Math.floor(v / 0x10000) + 0x7fff8 + a[5] - b[5]) & 0xFFFF;
    r[6] = (v = Math.floor(v / 0x10000) + 0x7fff8 + a[6] - b[6]) & 0xFFFF;
    r[7] = (v = Math.floor(v / 0x10000) + 0x7fff8 + a[7] - b[7]) & 0xFFFF;
    r[8] = (v = Math.floor(v / 0x10000) + 0x7fff8 + a[8] - b[8]) & 0xFFFF;
    r[9] = (v = Math.floor(v / 0x10000) + 0x7fff8 + a[9] - b[9]) & 0xFFFF;
    r[10] = (v = Math.floor(v / 0x10000) + 0x7fff8 + a[10] - b[10]) & 0xFFFF;
    r[11] = (v = Math.floor(v / 0x10000) + 0x7fff8 + a[11] - b[11]) & 0xFFFF;
    r[12] = (v = Math.floor(v / 0x10000) + 0x7fff8 + a[12] - b[12]) & 0xFFFF;
    r[13] = (v = Math.floor(v / 0x10000) + 0x7fff8 + a[13] - b[13]) & 0xFFFF;
    r[14] = (v = Math.floor(v / 0x10000) + 0x7fff8 + a[14] - b[14]) & 0xFFFF;
    r[15] = Math.floor(v / 0x10000) + 0x7ff8 + a[15] % 0x8000 - b[15] % 0x8000;
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

function reduce(a) {
    var v = a[15];
    if (v < 0x8000) return;
    a[15] = v % 0x8000;
    v = Math.floor(v / 0x8000) * 19;
    for (var i = 0; i <=14; i++ ) {
        a[i] = (v += a[i]) & 0xFFFF;
        if ((v = Math.floor(v / 0x10000)) < 1)
            return;
    }
    a[15] += v;
}

function sqr8h(r, a7, a6, a5, a4, a3, a2, a1, a0) {
    var v;
    r[0] = (v = a0*a0) & 0xFFFF;
    r[1] = (v = Math.floor(v / 0x10000) + 2*a0*a1) & 0xFFFF;
    r[2] = (v = Math.floor(v / 0x10000) + 2*a0*a2 + a1*a1) & 0xFFFF;
    r[3] = (v = Math.floor(v / 0x10000) + 2*a0*a3 + 2*a1*a2) & 0xFFFF;
    r[4] = (v = Math.floor(v / 0x10000) + 2*a0*a4 + 2*a1*a3 + a2*a2) & 0xFFFF;
    r[5] = (v = Math.floor(v / 0x10000) + 2*a0*a5 + 2*a1*a4 + 2*a2*a3) & 0xFFFF;
    r[6] = (v = Math.floor(v / 0x10000) + 2*a0*a6 + 2*a1*a5 + 2*a2*a4 + a3*a3) & 0xFFFF;
    r[7] = (v = Math.floor(v / 0x10000) + 2*a0*a7 + 2*a1*a6 + 2*a2*a5 + 2*a3*a4) & 0xFFFF;
    r[8] = (v = Math.floor(v / 0x10000) + 2*a1*a7 + 2*a2*a6 + 2*a3*a5 + a4*a4) & 0xFFFF;
    r[9] = (v = Math.floor(v / 0x10000) + 2*a2*a7 + 2*a3*a6 + 2*a4*a5) & 0xFFFF;
    r[10] = (v = Math.floor(v / 0x10000) + 2*a3*a7 + 2*a4*a6 + a5*a5) & 0xFFFF;
    r[11] = (v = Math.floor(v / 0x10000) + 2*a4*a7 + 2*a5*a6) & 0xFFFF;
    r[12] = (v = Math.floor(v / 0x10000) + 2*a5*a7 + a6*a6) & 0xFFFF;
    r[13] = (v = Math.floor(v / 0x10000) + 2*a6*a7) & 0xFFFF;
    r[14] = (v = Math.floor(v / 0x10000) + a7*a7) & 0xFFFF;
    r[15] = Math.floor(v / 0x10000);
}

function sqr(r, a) {
    var x = new Array(16);
    sqr8h(x, a[15], a[14], a[13], a[12], a[11], a[10], a[9], a[8]);
    var z =  new Array(16);
    sqr8h(z, a[7], a[6], a[5], a[4], a[3], a[2], a[1], a[0]);
    var y =  new Array(16);
    sqr8h(y, a[15] + a[7], a[14] + a[6], a[13] + a[5], a[12] + a[4], a[11] + a[3], a[10] + a[2], a[9] + a[1], a[8] + a[0]);
    var v;
    r[0] = (v = 0x800000 + z[0] + (y[8] -x[8] -z[8] + x[0] -0x80) * 38) & 0xFFFF;
    r[1] = (v = 0x7fff80 + Math.floor(v / 0x10000) + z[1] + (y[9] -x[9] -z[9] + x[1]) * 38) & 0xFFFF;
    r[2] = (v = 0x7fff80 + Math.floor(v / 0x10000) + z[2] + (y[10] -x[10] -z[10] + x[2]) * 38) & 0xFFFF;
    r[3] = (v = 0x7fff80 + Math.floor(v / 0x10000) + z[3] + (y[11] -x[11] -z[11] + x[3]) * 38) & 0xFFFF;
    r[4] = (v = 0x7fff80 + Math.floor(v / 0x10000) + z[4] + (y[12] -x[12] -z[12] + x[4]) * 38) & 0xFFFF;
    r[5] = (v = 0x7fff80 + Math.floor(v / 0x10000) + z[5] + (y[13] -x[13] -z[13] + x[5]) * 38) & 0xFFFF;
    r[6] = (v = 0x7fff80 + Math.floor(v / 0x10000) + z[6] + (y[14] -x[14] -z[14] + x[6]) * 38) & 0xFFFF;
    r[7] = (v = 0x7fff80 + Math.floor(v / 0x10000) + z[7] + (y[15] -x[15] -z[15] + x[7]) * 38) & 0xFFFF;
    r[8] = (v = 0x7fff80 + Math.floor(v / 0x10000) + z[8] + y[0] -x[0] -z[0] + x[8] * 38) & 0xFFFF;
    r[9] = (v = 0x7fff80 + Math.floor(v / 0x10000) + z[9] + y[1] -x[1] -z[1] + x[9] * 38) & 0xFFFF;
    r[10] = (v = 0x7fff80 + Math.floor(v / 0x10000) + z[10] + y[2] -x[2] -z[2] + x[10] * 38) & 0xFFFF;
    r[11] = (v = 0x7fff80 + Math.floor(v / 0x10000) + z[11] + y[3] -x[3] -z[3] + x[11] * 38) & 0xFFFF;
    r[12] = (v = 0x7fff80 + Math.floor(v / 0x10000) + z[12] + y[4] -x[4] -z[4] + x[12] * 38) & 0xFFFF;
    r[13] = (v = 0x7fff80 + Math.floor(v / 0x10000) + z[13] + y[5] -x[5] -z[5] + x[13] * 38) & 0xFFFF;
    r[14] = (v = 0x7fff80 + Math.floor(v / 0x10000) + z[14] + y[6] -x[6] -z[6] + x[14] * 38) & 0xFFFF;
    r[15] = 0x7fff80 + Math.floor(v / 0x10000) + z[15] + y[7] -x[7] -z[7] + x[15] * 38;
    reduce(r);
}

function mul8h(r, a7, a6, a5, a4, a3, a2, a1, a0, b7, b6, b5, b4, b3, b2, b1, b0) {
    var v;
    r[0] = (v = a0*b0) & 0xFFFF;
    r[1] = (v = Math.floor(v / 0x10000) + a0*b1 + a1*b0) & 0xFFFF;
    r[2] = (v = Math.floor(v / 0x10000) + a0*b2 + a1*b1 + a2*b0) & 0xFFFF;
    r[3] = (v = Math.floor(v / 0x10000) + a0*b3 + a1*b2 + a2*b1 + a3*b0) & 0xFFFF;
    r[4] = (v = Math.floor(v / 0x10000) + a0*b4 + a1*b3 + a2*b2 + a3*b1 + a4*b0) & 0xFFFF;
    r[5] = (v = Math.floor(v / 0x10000) + a0*b5 + a1*b4 + a2*b3 + a3*b2 + a4*b1 + a5*b0) & 0xFFFF;
    r[6] = (v = Math.floor(v / 0x10000) + a0*b6 + a1*b5 + a2*b4 + a3*b3 + a4*b2 + a5*b1 + a6*b0) & 0xFFFF;
    r[7] = (v = Math.floor(v / 0x10000) + a0*b7 + a1*b6 + a2*b5 + a3*b4 + a4*b3 + a5*b2 + a6*b1 + a7*b0) & 0xFFFF;
    r[8] = (v = Math.floor(v / 0x10000) + a1*b7 + a2*b6 + a3*b5 + a4*b4 + a5*b3 + a6*b2 + a7*b1) & 0xFFFF;
    r[9] = (v = Math.floor(v / 0x10000) + a2*b7 + a3*b6 + a4*b5 + a5*b4 + a6*b3 + a7*b2) & 0xFFFF;
    r[10] = (v = Math.floor(v / 0x10000) + a3*b7 + a4*b6 + a5*b5 + a6*b4 + a7*b3) & 0xFFFF;
    r[11] = (v = Math.floor(v / 0x10000) + a4*b7 + a5*b6 + a6*b5 + a7*b4) & 0xFFFF;
    r[12] = (v = Math.floor(v / 0x10000) + a5*b7 + a6*b6 + a7*b5) & 0xFFFF;
    r[13] = (v = Math.floor(v / 0x10000) + a6*b7 + a7*b6) & 0xFFFF;
    r[14] = (v = Math.floor(v / 0x10000) + a7*b7) & 0xFFFF;
    r[15] = Math.floor(v / 0x10000);
}

function mul(r, a, b) {
    // Karatsuba multiplication scheme: x*y = (b^2+b)*x1*y1 - b*(x1-x0)*(y1-y0) + (b+1)*x0*y0
    var x = new Array(16);
    mul8h(x, a[15], a[14], a[13], a[12], a[11], a[10], a[9], a[8], b[15], b[14], b[13], b[12], b[11], b[10], b[9], b[8]);
    var z = new Array(16);
    mul8h(z, a[7], a[6], a[5], a[4], a[3], a[2], a[1], a[0], b[7], b[6], b[5], b[4], b[3], b[2], b[1], b[0]);
    var y = new Array(16);
    mul8h(y, a[15] + a[7], a[14] + a[6], a[13] + a[5], a[12] + a[4], a[11] + a[3], a[10] + a[2], a[9] + a[1], a[8] + a[0],
        b[15] + b[7], b[14] + b[6], b[13] + b[5], b[12] + b[4], b[11] + b[3], b[10] + b[2], b[9] + b[1], b[8] + b[0]);
    var v;
    r[0] = (v = 0x800000 + z[0] + (y[8] -x[8] -z[8] + x[0] -0x80) * 38) & 0xFFFF;
    r[1] = (v = 0x7fff80 + Math.floor(v / 0x10000) + z[1] + (y[9] -x[9] -z[9] + x[1]) * 38) & 0xFFFF;
    r[2] = (v = 0x7fff80 + Math.floor(v / 0x10000) + z[2] + (y[10] -x[10] -z[10] + x[2]) * 38) & 0xFFFF;
    r[3] = (v = 0x7fff80 + Math.floor(v / 0x10000) + z[3] + (y[11] -x[11] -z[11] + x[3]) * 38) & 0xFFFF;
    r[4] = (v = 0x7fff80 + Math.floor(v / 0x10000) + z[4] + (y[12] -x[12] -z[12] + x[4]) * 38) & 0xFFFF;
    r[5] = (v = 0x7fff80 + Math.floor(v / 0x10000) + z[5] + (y[13] -x[13] -z[13] + x[5]) * 38) & 0xFFFF;
    r[6] = (v = 0x7fff80 + Math.floor(v / 0x10000) + z[6] + (y[14] -x[14] -z[14] + x[6]) * 38) & 0xFFFF;
    r[7] = (v = 0x7fff80 + Math.floor(v / 0x10000) + z[7] + (y[15] -x[15] -z[15] + x[7]) * 38) & 0xFFFF;
    r[8] = (v = 0x7fff80 + Math.floor(v / 0x10000) + z[8] + y[0] -x[0] -z[0] + x[8] * 38) & 0xFFFF;
    r[9] = (v = 0x7fff80 + Math.floor(v / 0x10000) + z[9] + y[1] -x[1] -z[1] + x[9] * 38) & 0xFFFF;
    r[10] = (v = 0x7fff80 + Math.floor(v / 0x10000) + z[10] + y[2] -x[2] -z[2] + x[10] * 38) & 0xFFFF;
    r[11] = (v = 0x7fff80 + Math.floor(v / 0x10000) + z[11] + y[3] -x[3] -z[3] + x[11] * 38) & 0xFFFF;
    r[12] = (v = 0x7fff80 + Math.floor(v / 0x10000) + z[12] + y[4] -x[4] -z[4] + x[12] * 38) & 0xFFFF;
    r[13] = (v = 0x7fff80 + Math.floor(v / 0x10000) + z[13] + y[5] -x[5] -z[5] + x[13] * 38) & 0xFFFF;
    r[14] = (v = 0x7fff80 + Math.floor(v / 0x10000) + z[14] + y[6] -x[6] -z[6] + x[14] * 38) & 0xFFFF;
    r[15] = 0x7fff80 + Math.floor(v / 0x10000) + z[15] + y[7] -x[7] -z[7] + x[15] * 38;
    reduce(r);
}

function mul_small(r, a, b)
{
    var v;
    r[0] = (v = a[0] * b) & 0xFFFF;
    for (var i = 1; i <= 14; i++)
        r[i] = (v = Math.floor(v / 0x10000) + a[i]*b) & 0xFFFF;
    r[15] = Math.floor(v / 0x10000) + a[15]*b;
    reduce(r);
}

function recip(y, x, sqrtassist)
{
    var t0 = new Array(16); var t1 = new Array(16);  var t2 = new Array(16);    var t3 = new Array(16); var t4 = new Array(16);

    /* the chain for x^(2^255-21) is straight from djb's implementation */
    sqr(t1, x);	/*  2 == 2 * 1	*/
    sqr(t2, t1);	/*  4 == 2 * 2	*/
    sqr(t0, t2);	/*  8 == 2 * 4	*/
    mul(t2, t0, x);	/*  9 == 8 + 1	*/
    mul(t0, t2, t1);	/* 11 == 9 + 2	*/
    sqr(t1, t0);	/* 22 == 2 * 11	*/
    mul(t3, t1, t2);	/* 31 == 22 + 9 == 2^5   - 2^0	*/
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

compare = function (a ,b) {
    var c;
    for (c = 15; c >= 0; c--) {
        var x = a[c];
        var y = b[c];
        if (x > y) {
            return 1;
        }
        if (x < y) {
            return -1;
        }
    }
    return 0;
}

function is_overflow(x) {
    return (
        ((x[0] > 0xFFFF-19)) &&
            ((x[15]) == 0x7FFF) &&
            ((x[1] & x[2] & x[3] & x[4] & x[5] & x[6] & x[7] & x[8] & x[9] & x[10] & x[11]  & x[12] & x[13] & x[14]) == 0xFFFF)
        ) || (x[15] > 0x7FFF);
}

/* checks if x is "negative", requires reduced input */
function is_negative(x)
{
    return (((is_overflow(x) || (x[15] & 0x8000)) ? 1 : 0) ^ (x[0] & 1));
}

function sqrt(x, u)
{
    var v = new Array(16);  var t1 = new Array(16);    var t2 = new Array(16);

    add(t1, u, u);	/* t1 = 2u		*/
    recip(v, t1, 1);	/* v = (2u)^((p-5)/8)	*/
    sqr(x, v);		/* x = v^2		*/
    mul(t2, t1, x);	/* t2 = 2uv^2		*/
    sub(t2, t2, C1);	/* t2 = 2uv^2-1		*/
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
    var dx = new Array(16); var t1 = new Array(16); var t2 = new Array(16); var t3 = new Array(16); var t4 = new Array(16);

    var x = [new Array(16), new Array(16)];
    var z = [new Array(16), new Array(16)];

    /* unpack the base */
    if (Gx)
        pack(dx, Gx);
    else
        cpy(dx, C9);

    /* 0G = point-at-infinity */
    cpy(x[0], C1);
    cpy(z[0], C0);

    /* 1G = G */
    cpy(x[1], dx);
    cpy(z[1], C1);

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
    unpack(dx, Px);

    /* calculate s such that s abs(P) = G  .. assumes G is std base point */
    if (s !== null)
    {
        x_to_y2(t2, t1, dx);	/* t1 = Py^2  */
        recip(t3, z[1], 0);	/* where Q=P+G ... */
        mul(t2, x[1], t3);	/* t2 = Qx  */
        add(t2, t2, dx);	/* t2 = Qx + Px  */
        add(t2, t2, C486671);	/* t2 = Qx + Px + Gx + 486662  */
        sub(dx, dx, C9);		/* dx = Px - Gx  */
        sqr(t3, dx);	/* t3 = (Px - Gx)^2  */
        mul(dx, t2, t3);	/* dx = t2 (Px - Gx)^2  */
        sub(dx, dx, t1);	/* dx = t2 (Px - Gx)^2 - Py^2  */
        sub(dx, dx, C39420360); /* dx = t2 (Px - Gx)^2 - Py^2 - Gy^2  */
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
function curve25519_sign(v, h, x, s)
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
    if (v31 >= 0x80)
        v31 -= 0x100;
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

    var p = new Array(new Array(16), new Array(16));
    var s = new Array(new Array(16), new Array(16));
    var yx = new Array(new Array(16), new Array(16), new Array(16));
    var yz = new Array(new Array(16), new Array(16), new Array(16));
    var t1 = new Array(new Array(16), new Array(16), new Array(16));
    var t2 = new Array(new Array(16), new Array(16), new Array(16));

    var vi = 0, hi = 0, di = 0, nvh = 0, i, j, k;

    /* set p[0] to G and p[1] to P  */

    cpy(p[0], C9);
    pack(p[1], P);

    /* set s[0] to P+G and s[1] to P-G  */

    /* s[0] = (Py^2 + Gy^2 - 2 Py Gy)/(Px - Gx)^2 - Px - Gx - 486662  */
    /* s[1] = (Py^2 + Gy^2 + 2 Py Gy)/(Px - Gx)^2 - Px - Gx - 486662  */
    x_to_y2(t1[0], t2[0], p[1]);	/* t2[0] = Py^2  */
    sqrt(t1[0], t2[0]);	/* t1[0] = Py or -Py  */
    j = is_negative(t1[0]);		/*      ... check which  */
    add(t2[0], t2[0], C39420360); /* t2[0] = Py^2 + Gy^2  */
    mul(t2[1], BASE_2Y, t1[0]);/* t2[1] = 2 Py Gy or -2 Py Gy  */
    sub(t1[j], t2[0], t2[1]);	/* t1[0] = Py^2 + Gy^2 - 2 Py Gy  */
    add(t1[1-j], t2[0], t2[1]);/* t1[1] = Py^2 + Gy^2 + 2 Py Gy  */
    cpy(t2[0],p[1]);		/* t2[0] = Px  */
    sub(t2[0], t2[0], C9); /* t2[0] = Px - Gx  */
    sqr(t2[1], t2[0]);		/* t2[1] = (Px - Gx)^2  */
    recip(t2[0], t2[1], 0);	/* t2[0] = 1/(Px - Gx)^2  */
    mul(s[0], t1[0], t2[0]);	/* s[0] = t1[0]/(Px - Gx)^2  */
    sub(s[0], s[0], p[1]);	/* s[0] = t1[0]/(Px - Gx)^2 - Px  */
    sub(s[0], s[0], C486671); /* s[0] = X(P+G)  */
    mul(s[1], t1[1], t2[0]);	/* s[1] = t1[1]/(Px - Gx)^2  */
    sub(s[1], s[1], p[1]);	/* s[1] = t1[1]/(Px - Gx)^2 - Px  */
    sub(s[1], s[1], C486671); /* s[1] = X(P-G)  */

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
    cpy(yx[0], C1);
    cpy(yx[1], p[di]);
    cpy(yx[2], s[0]);
    cpy(yz[0], C0);
    cpy(yz[1], C1);
    cpy(yz[2], C1);

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
    unpack(t1[1], Y);
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