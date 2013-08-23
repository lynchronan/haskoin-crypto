# haskoin-crypto

Implementation of the Bitcoin cryptographic primitives in Haskell

Project Status: **Experimental**

## Description

**haskoin-crypto** is a component of **haskoin**, an ecosystem of haskell
libraries implementing the various parts of the bitcoin protocol. Specifically,
haskoin-crypto provides the elliptic curve cryptography required for creating
and validating bitcoin transactions. Only operations on the bitcoin-specific
SECP256k1 curve are available in this package. haskoin-crypto also implements
the SHA-256 and RIPEMP-160 digest algorithms.

The philosophy behind haskoin-crypto is to provide a sound implementation of
the elliptic curve cryptography by favouring elegance and safety over
performance. We do, however, consider performance as an important goal to
achieve when it doesn't conflict with code safety. Instead of hiding behind
abstractions, we implement the elliptic curve cryptography in pure Haskell
which provides the following advantages:

- Provide an alternative to openssl for the cloud of bitcoin nodes
- Describe precisely the canonical crypto formats used in bitcoin
- Reduce trust on third party code

## Synopsis

```haskell
    import Haskoin.Crypto
    import Haskoin.Crypto.Util (bsToInteger)

    -- For serializing/de-serializing interface
    import Data.Binary
    import Data.Binary.Put
    import Data.Binary.Get

    -- Access to /dev/random
    import System.IO
    import qualified Data.ByteString as BS

    import Control.Applicative ((<$>))

    -- Generate a random Integer with 256 bits of entropy
    random256 :: IO Integer
    random256 = withBinaryFile "/dev/random" ReadMode $ \h -> do
        bs <- BS.hGet h 32 -- Read 32 bytes
        return $ bsToInteger bs

    main :: IO ()
    main = do

        -- Create a private key from a random source
        priv <- makePrivateKey <$> random256

            -- Derive the public key from a private key
        let pub   = derivePublicKey priv
            -- Compute the bitcoin address from the public key
            addr  = publicKeyAddress pub
            -- Serialize the private key to WIF format
            wif   = toWIF priv
            -- Deserialize a private key from WIF format
            priv' = fromWIF wif

            -- Serialize a public key
        let pubBin = runPut $ put pub
            -- Deserialize a public key
            pub'   = runGet get pubBin :: PublicKey

        -- Generate a random seed to create signature nonces
        seed <- random256
        -- Initialize a safe environment for creating signatures
        withECDSA seed $ do 
                -- Create a message in ByteString format
            let msg = BS.pack [1,3,3,7]
                -- Compute two rounds of SHA-256
                hash = doubleHash256 msg

            -- Signatures are guaranteed to use different nonces
            sig1 <- signMessage hash priv
            sig2 <- signMessage hash priv

            -- Verify signatures
            let ver1 = verifyMessage hash sig1 pub
                ver2 = verifyMessage hash sig2 pub

                -- Serialize a signature 
            let sigBin = runPut $ put sig1
                -- Deserialize a signature
                sig1'  = runGet get sigBin :: Signature

            return ()
```
## Usage

All the types and functions in this section are exported by `Haskoin.Crypto`

```haskell
    import Haskoin.Crypto
```

### Keys

```haskell
    data PublicKey  = PublicKey  Point | 
                      PublicKeyU Point

    data PrivateKey = PrivateKey  FieldN | 
                      PrivateKeyU FieldN
```

Public and private keys each have an associated data type. They each have two
data constructors corresponding to either the compressed or uncompressed
versions of the keys. The default format used across this library is the
compressed format, so uncompressed versions are usually explicitly postfixed
with an upper-case U. The data constructors are mainly used internally for
serialization and are not exported by the library.

The `PublicKey` type is an instance of `Data.Binary` so it can be serialized
and de-serialized through the `Get` and `Put monads. Below is a sample code
describing how to use the serialization interface.

```haskell
    import Data.Binary 
    import Data.Binary.Get 
    import Data.Binary.Put 
    import Data.ByteString

    -- toByteString and fromByteString are only example functions
    -- They are not exported by Haskoin.Crypto
    toByteString :: PublicKey -> Data.ByteString
    toByteString key = runPut $ put key
    
    fromByteString :: Data.ByteString -> PublicKey
    fromByteString bs = runGet get bs
```

An uncompressed public key will store both **x** and **y** components of a
point and will start with an `0x04` byte. Compressed public keys are more space
efficient as they only store the **x** component and one additional byte `0x02`
if **y** is even or `0x03` if **y** is odd. You don't loose any security by
using compressed keys. In fact, the **y** component can be fully deduced from
the elliptic curve equation knowing the **x** component of the point and the
parity of **y**.

To create a private key from an Integer, you can use either:

```haskell
    makePrivateKey  :: Integer -> PrivateKey -- Compressed format
    makePrivateKeyU :: Integer -> PrivateKey -- Uncompressed format
```

Note that the Integer is your secret for the private key and it needs to be
drawn from a random source containing at least 256 bits of entropy. We can not
be held accountable if you are using a bad random number generator. 

You can derive a `PublicKey` from a `PrivateKey`:

```haskell
    derivePublicKey :: PrivateKey -> PublicKey
```

If you need to test whether you are dealing with a compressed or uncompressed
key:

```haskell
    isCompressed :: PublicKey -> Bool
    isPrivateKeyCompressed :: PrivateKey -> Bool
```

You can also test if a `PublicKey` is valid. This will check that the elliptic
curve point associated with the public key is not the point at infinity and
that the **x** and **y** coordinates of the point lie on the SECP256k1 curve.

```haskell
    validatePublicKey :: PublicKey -> Bool
``` 

To derive a base58 Bitcoin address from a public key (like
176CwMCWMq1y9CxFZWk7Vfoka5PoaNzxRq):

```haskell
    publicKeyAddress :: PublicKey -> Data.ByteString
```

You can also import and export private keys to the WIF (Wallet Import Format)
format which is compatible with the reference Satoshi client:

```haskell
    -- fromWIF returns Nothing if the ByteString format is bad
    fromWIF :: Data.ByteString -> Maybe PrivateKey
    toWIF :: PrivateKey -> Data.ByteString
```

For more details on the WIF format, check out:

[en.bitcoin.it/wiki/Wallet_import_format](en.bitcoin.it/wiki/Wallet_import_format)

### ECDSA

```haskell
    newtype ECDSA m a = ECDSA StateT Nonce m a
```

The `ECDSA` monad provides a safe context in which to call `signMessage` for
signature creations. `signMessage` calls within the `ECDSA` monad are
guaranteed not to re-use the same **k** nonce. The `ECDSA` monad has an
internal state containing the current **k** value. Whenever you ask for this
value, it is hashed with SHA-256 and a new value is stored inside the `ECDSA`
monad by hashing it a second time with SHA-256. This guarantees that the **k**
value you are going to use for you signature is not stored anywhere and can not
accidentally be re-used.

```haskell
    withECDSA :: Monad m => Integer -> ECDSA m a -> m a
```

Runs an `ECDSA` monad by seeding it with the initial **k** nonce used for
signature creations. This library doesn't provide the random number generator
(RNG) for seeding this initial value. You need to make sure you provide an
Integer drawn from a random pool of at least 256 bits of entropy. 

```haskell
    data Signature = Signature FieldN FieldN
```

Data type describing an `ECDSA` signature.

```haskell
    signMessage :: Monad m => Hash256 -> PrivateKey -> ECDSA m Signature
```

`signMessage` should be called withing the `ECDSA` monad to safely sign the
hash of a message.

A `Signature` is an instance of `Data.Binary` and can be
serialized/de-serialized using the `Get` and `Put` monads. Below is a code
example describing how to use the serialization interface.

```haskell
    import Data.Binary 
    import Data.Binary.Get 
    import Data.Binary.Put 
    import Data.ByteString

    -- toByteString and fromByteString are only example functions
    -- They are not exported by Haskoin.Crypto
    toByteString :: Signature -> Data.ByteString
    toByteString sig = runPut $ put key
    
    fromByteString :: Data.ByteString -> Signature
    fromByteString bs = runGet get bs
```

### Digests

The `Hash256` and `Hash160` data types represent hashes of either 256 or 160
bits. They are essentially unsigned integers modulo 2^256 or modulo 2^160. They
behave the same way as the `Word8`, `Word16`, `Word32` and `Word64` types of
the `Data.Word` package, except with 160 and 256 bits. We use these types as
opposed to Integers to convey the information that we are dealing with hashes
produced by digest functions rather than arbitrary integers. 

```haskell
    type Hash256 = Ring Mod256
    type Hash160 = Ring Mod160
```

The following message digest functions are exported by the library

```haskell
    -- Single round of SHA-256
    hash256 :: Data.ByteString -> Hash256
    hash256BS :: Data.ByteString -> Data.ByteString

    -- Single round of RIPEMD-160
    hash160 :: Data.ByteString -> Hash160
    hash160BS :: Data.ByteString -> Data.ByteString

    -- Double round of SHA-256
    doubleHash256 :: Data.ByteString -> Hash256
    doubleHash256BS :: Data.ByteString -> Data.ByteString
```

A 32 bit checksum is represented as a `CheckSum32` data type

```haskell
    newtype CheckSum32 = CheckSum32 Word32
```

It is an instance of `Data.Binary` so you can serialize/de-serialize it easily.
To compute a `CheckSum32`, use:

```haskell
    chksum32 :: BS.ByteString -> CheckSum32
```

## Installing

```sh
    git clone https://github.com/plaprade/haskoin-crypto.git
    cabal install
```

For running the test suites

```sh
    cabal configure --enable-test
    cabal build
    cabal test
```

For running the benchmarks

```sh
    cabal configure --enable-benchmark
    cabal build
    cabal bench
```

## Benchmarks

Here are the results of some benchmarks running on a single core I7:

```sh
    Ring multiplication (mod n) (10000000 samples)
    Total time: 1.868806s
    Op/sec    : 5351010.217218908757s
    ----------------------------
    Ring inversion (mod n) (100000 samples)
    Total time: 7.244239s
    Op/sec    : 13804.072449846008s
    ----------------------------
    Point multiplications (2000 samples)
    Total time: 4.372304s
    Op/sec    : 457.424735334048s
    ----------------------------
    Point additions (100000 samples)
    Total time: 0.684761s
    Op/sec    : 146036.354290036961s
    ----------------------------
    Point doubling (100000 samples)
    Total time: 0.455613s
    Op/sec    : 219484.518659476353s
    ----------------------------
    Shamirs trick (2000 samples)
    Total time: 4.366215s
    Op/sec    : 458.062646937908s
    ----------------------------
    Signature creations (2000 samples)
    Total time: 4.786426s
    Op/sec    : 417.848306857768s
    ----------------------------
    Signature verifications (2000 samples)
    Total time: 5.70411s
    Op/sec    : 350.624374354632s
```

## Bugs

Please report any bugs in the projects bug tracker:

[github.com/plaprade/haskoin-crypto/issues](http://github.com/plaprade/haskoin-crypto/issues)

## Contributing

We're glad you want to contribute! It's simple:

- Fork haskoin-crypto
- Create a branch `git checkout -b my_branch`
- Commit your changes `git commit -am 'comments'`
- Push the branch `git push origin my_branch`
- Open a pull request

Code guidelines:

- 80 colums. If you need more, you're doing something wrong. It's not readable.
- 4 space indentation. No tabs.
- Follow the general style of the code, whenever it makes sense.

## Supporting

You can support the project by donating in [Bitcoins](http://www.bitcoin.org)
to:

**176CwMCWMq1y9CxFZWk7Vfoka5PoaNzxRq**
