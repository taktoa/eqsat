--------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

--------------------------------------------------------------------------------

-- | FIXME: doc
module EqSat.Internal.Compact
  (
  ) where

--------------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 802
import qualified Data.Compact                 as Compact
import qualified Data.Compact.Serialize       as Compact
#endif

import           Data.Coerce                  (coerce)
import           Data.Typeable                (Typeable)
import           Data.Void                    (Void)
import           Data.Word                    (Word)
import           Flow                         ((.>))
import           System.IO                    (Handle)

import           Data.Functor.Contravariant   (Contravariant (contramap))

import qualified Data.ByteString.Lazy         as LBS

import qualified Data.Text                    as Text

import qualified EqSat.Internal.PrettyPrinter as PP

import qualified Codec.CBOR.Decoding          as CBOR.Decoding
import qualified Codec.CBOR.Encoding          as CBOR.Encoding
import qualified Codec.CBOR.JSON              as CBOR.JSON
import qualified Codec.CBOR.Read              as CBOR.Read
import qualified Codec.CBOR.Write             as CBOR.Write
import qualified Data.Aeson                   as Aeson
import qualified Data.Binary                  as Binary

--------------------------------------------------------------------------------

-- | FIXME: doc
newtype Encoder a
  = MkEncoder (a -> LBS.ByteString)

instance Contravariant Encoder where
  contramap f (MkEncoder cb) = MkEncoder (f .> cb)

runEncoder :: Encoder a -> a -> LBS.ByteString
runEncoder (MkEncoder f) = f

--------------------------------------------------------------------------------

-- | FIXME: doc
newtype Decoder a
  = MkDecoder (LBS.ByteString -> Either (PP.Doc Void) a)

instance Functor Decoder where
  fmap f (MkDecoder cb) = MkDecoder (\lbs -> f <$> cb lbs)

runDecoder :: Decoder a -> LBS.ByteString -> Either (PP.Doc Void) a
runDecoder (MkDecoder f) = f

--------------------------------------------------------------------------------

-- | FIXME: doc
aesonEncoder
  :: (Aeson.ToJSON a)
  => Encoder a
aesonEncoder = cborEncoder (Aeson.toJSON .> CBOR.JSON.encodeValue)

-- | FIXME: doc
cborEncoder
  :: (Aeson.ToJSON a)
  => (a -> CBOR.Encoding.Encoding)
  -> Encoder a
cborEncoder f = MkEncoder (f .> CBOR.Write.toLazyByteString)

--------------------------------------------------------------------------------

-- | FIXME: doc
#if __GLASGOW_HASKELL__ >= 802
type Compact a = Compact.Compact a
#else
type Compact a = a
#endif

-- | FIXME: doc
compact :: a -> IO (Compact a)

-- | FIXME: doc
compactWithSharing :: a -> IO (Compact a)

-- | FIXME: doc
compactAdd :: Compact b -> a -> IO (Compact a)

-- | FIXME: doc
compactAddWithSharing :: Compact b -> a -> IO (Compact a)

-- | FIXME: doc
compactSized :: Int -> Bool -> a -> IO (Compact a)

-- | FIXME: doc
getCompact :: Compact a -> a

-- | FIXME: doc
isCompact :: a -> IO Bool

-- | FIXME: doc
compactSize :: Compact a -> IO Word

-- | FIXME: doc
writeCompact
  :: (Typeable a)
  => Encoder a
  -> FilePath
  -> Compact a
  -> IO ()

-- | FIXME: doc
unsafeReadCompact
  :: (Typeable a)
  => Decoder a
  -> FilePath
  -> IO (Either String (Compact a))

-- | FIXME: doc
hPutCompact
  :: (Typeable a)
  => Encoder a
  -> Handle
  -> Compact a
  -> IO ()

-- | FIXME: doc
hUnsafeGetCompact
  :: (Typeable a)
  => Decoder a
  -> Handle
  -> IO (Either String (Compact a))

#if __GLASGOW_HASKELL__ >= 802

compact               = Compact.compact
compactWithSharing    = Compact.compactWithSharing
compactAdd            = Compact.compactAdd
compactAddWithSharing = Compact.compactAddWithSharing
compactSized          = Compact.compactSized
getCompact            = Compact.getCompact
isCompact             = Compact.isCompact
compactSize           = Compact.compactSize

writeCompact          = const Compact.writeCompact
unsafeReadCompact     = const Compact.unsafeReadCompact
hPutCompact           = const Compact.hPutCompact
hUnsafeGetCompact     = const Compact.hUnsafeGetCompact

#else

compact
  = MkCompact .> pure

compactWithSharing
  = MkCompact .> pure

compactAdd
  = undefined

compactAddWithSharing
  = undefined

compactSized
  = undefined

getCompact
  = (\(MkCompact x) -> x)

isCompact
  = undefined

compactSize
  = undefined

writeCompact
  = undefined

unsafeReadCompact
  = undefined

hPutCompact
  = undefined

hUnsafeGetCompact
  = undefined

#endif

--------------------------------------------------------------------------------
