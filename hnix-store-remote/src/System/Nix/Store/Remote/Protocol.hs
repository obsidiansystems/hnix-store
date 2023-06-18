{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
module System.Nix.Store.Remote.Protocol where

workerMagic1 :: Int32
workerMagic1 = 0x6e697863

workerMagic2 :: Int32
workerMagic2 = 0x6478696f

ourProtoVersion :: ProtoVersion
ourProtoVersion = ProtoVersion
  { protoVersion_major = 1
  , protoVersion_minor = 21
  }

data ProtoVersion = ProtoVersion
  { protoVersion_major :: Word16
  , protoVersion_minor :: Word8
  }
  deriving (Eq, Ord, Show)

class HasProtoVersion r where
  protoVersion :: r -> ProtoVersion

type ActivityID = Word64
type ActivityParentID = Word64
type ActivityType = Word64
type Verbosity = Word64
type ResultType = Word64

pattern StderrNext :: Word32
pattern StderrNext = 0x6f6c6d67

pattern StderrRead :: Word32
pattern StderrRead = 0x32617461 -- data needed from source

pattern StderrWrite :: Word32
pattern StderrWrite = 0x32617416 -- data for sink

pattern StderrLast :: Word32
pattern StderrLast = 0x616c7473

pattern StderrError :: Word32
pattern StderrError = 0x63787470

pattern StderrStartActivity :: Word32
pattern StderrStartActivity = 0x53545254

pattern StderrStopActivity :: Word32
pattern StderrStopActivity = 0x53544f50

pattern StderrResult :: Word32
pattern StderrResult = 0x52534c54

data Field = LogStr ByteString | LogInt Int
  deriving (Eq, Ord, Show)

data Logger =
    Next          ByteString
  | Read          Word        -- data needed from source
  | Write         ByteString -- data for sink
  | Last
  | Error         Word ByteString
  | StartActivity ActivityID Verbosity ActivityType ByteString [Field] ActivityParentID
  | StopActivity  ActivityID
  | Result        ActivityID ResultType [Field]
  deriving (Eq, Ord, Show)

type Trace = Text -- should also contain an errorpos, but nix always writes a 0 anyway

data ErrorInfo = ErrorInfo
  { _errorInfo_level :: Verbosity
  , _errorInfo_name :: Text
  , _errorInfo_msg :: Text
  -- _errorInfo_errPos :: Maybe ErrPos .. nix always writes 0 for this.
  , _errorInfo_traces :: [Trace]
  } deriving Show

-- | worker opcode
--
-- This type has gaps filled in so that the GHC builtin Enum instance lands on the right values.
data WorkerOp
  = Reserved_0__ -- 0
  | IsValidPath -- 1
  | Reserved_2__ -- 2
  | HasSubstitutes -- 3
  | QueryPathHash -- 4 // obsolete
  | QueryReferences --  5 // obsolete
  | QueryReferrers --  6
  | AddToStore --  7
  | AddTextToStore --  8 // obsolete since 1.25, Nix 3.0. Use wopAddToStore
  | BuildPaths --  9
  | EnsurePath --  10 0xa
  | AddTempRoot --  11 0xb
  | AddIndirectRoot --  12 0xc
  | SyncWithGC --  13 0xd
  | FindRoots --  14 0xe
  | Reserved_15__ -- 15 0xf
  | ExportPath --  16 0x10 // obsolete
  | Reserved_17__ -- 17 0x11
  | QueryDeriver --  18 0x12 // obsolete
  | SetOptions --  19 0x13
  | CollectGarbage --  20 0x14
  | QuerySubstitutablePathInfo --  21 0x15
  | QueryDerivationOutputs --  22 0x16 // obsolete
  | QueryAllValidPaths --  23 0x17
  | QueryFailedPaths --  24 0x18
  | ClearFailedPaths --  25 0x19
  | QueryPathInfo --  26 0x1a
  | ImportPaths --  27 0x1b // obsolete
  | QueryDerivationOutputNames --  28 0x1c // obsolete
  | QueryPathFromHashPart --  29 0x1d
  | QuerySubstitutablePathInfos --  30 0x1e
  | QueryValidPaths --  31 0x1f
  | QuerySubstitutablePaths --  32 0x20
  | QueryValidDerivers --  33 0x21
  | OptimiseStore --  34 0x22
  | VerifyStore --  35 0x23
  | BuildDerivation --  36 0x24
  | AddSignatures --  37 0x25
  | NarFromPath --  38 0x26
  | AddToStoreNar --  39 0x27
  | QueryMissing --  40 0x28
  | QueryDerivationOutputMap --  41 0x29
  | RegisterDrvOutput --  42 0x2a
  | QueryRealisation --  43 0x2b
  | AddMultipleToStore --  44 0x2c
  | AddBuildLog --  45 0x2d
  | BuildPathsWithResults --  46 0x2e
  deriving (Eq, Ord, Show, Read, Bounded, Enum)
