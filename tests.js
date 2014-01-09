Tests = TestCase("Tests");

Tests.prototype.testSign = function() {
    var message = "Hello World!";
    var secretPhrase = "secretPhrase";
    var signature = crypto_sign(message, secretPhrase);
    assertEquals("af60d3f0e20d431cb1d35691221c5302f1de93448dca16d3100ba014d8d0a10bf5b6c43db210e7b056ba623783e717b7fb98446faee1859f2f4f077418c448e7", signature);
};

Tests.prototype.testVerify = function() {
    // https://bitcointalk.org/index.php?topic=345619.msg4345475#msg4345475
    var publicKey = "7c3ff12215636a7b246dea25d5e446a91fa59a9150c6ed50d8126ee86a648b68";
    var message = "0000a9d63800a0057c3ff12215636a7b246dea25d5e446a91fa59a9150c6ed50d8126ee86a648b687e2fad81dbf18f2da086010064000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000";
    var signature = "4f0626ccd4edebb17e9d06e928b5b4e944aa7ef88a111081919794a3e265c206f9d9b0ce42a8d2e7f6d902a172159bcd39dcaab8468373258fccea9e5d2ed319";
    var result = crypto_verify(signature, message, publicKey);
    assertTrue(result);
};