#!/usr/bin/env bash

pkgs=(
    -hide-package 'base'
    -package-id 'base-4.14.3.0 (Control.Applicative, Control.Arrow, Control.Category, Control.Concurrent, Control.Concurrent.Chan, Control.Concurrent.MVar, Control.Concurrent.QSem, Control.Concurrent.QSemN, Control.Exception, Control.Exception.Base, Control.Monad, Control.Monad.Fail, Control.Monad.Fix, Control.Monad.IO.Class, Control.Monad.Instances, Control.Monad.ST, Control.Monad.ST.Lazy, Control.Monad.ST.Lazy.Safe, Control.Monad.ST.Lazy.Unsafe, Control.Monad.ST.Safe, Control.Monad.ST.Strict, Control.Monad.ST.Unsafe, Control.Monad.Zip, Data.Bifoldable, Data.Bifunctor, Data.Bitraversable, Data.Bits, Data.Bool, Data.Char, Data.Coerce, Data.Complex, Data.Data, Data.Dynamic, Data.Either, Data.Eq, Data.Fixed, Data.Foldable, Data.Function, Data.Functor, Data.Functor.Classes, Data.Functor.Compose, Data.Functor.Const, Data.Functor.Contravariant, Data.Functor.Identity, Data.Functor.Product, Data.Functor.Sum, Data.IORef, Data.Int, Data.Ix, Data.Kind, Data.List, Data.List.NonEmpty, Data.Maybe, Data.Monoid, Data.Ord, Data.Proxy, Data.Ratio, Data.STRef, Data.STRef.Lazy, Data.STRef.Strict, Data.Semigroup, Data.String, Data.Traversable, Data.Tuple, Data.Type.Bool, Data.Type.Coercion, Data.Type.Equality, Data.Typeable, Data.Unique, Data.Version, Data.Void, Data.Word, Debug.Trace, Foreign, Foreign.C, Foreign.C.Error, Foreign.C.String, Foreign.C.Types, Foreign.Concurrent, Foreign.ForeignPtr, Foreign.ForeignPtr.Safe, Foreign.ForeignPtr.Unsafe, Foreign.Marshal, Foreign.Marshal.Alloc, Foreign.Marshal.Array, Foreign.Marshal.Error, Foreign.Marshal.Pool, Foreign.Marshal.Safe, Foreign.Marshal.Unsafe, Foreign.Marshal.Utils, Foreign.Ptr, Foreign.Safe, Foreign.StablePtr, Foreign.Storable, GHC.Arr, GHC.Base, GHC.ByteOrder, GHC.Char, GHC.Clock, GHC.Conc, GHC.Conc.IO, GHC.Conc.Signal, GHC.Conc.Sync, GHC.ConsoleHandler, GHC.Constants, GHC.Desugar, GHC.Enum, GHC.Environment, GHC.Err, GHC.Event, GHC.Exception, GHC.Exception.Type, GHC.ExecutionStack, GHC.ExecutionStack.Internal, GHC.Exts, GHC.Fingerprint, GHC.Fingerprint.Type, GHC.Float, GHC.Float.ConversionUtils, GHC.Float.RealFracMethods, GHC.Foreign, GHC.ForeignPtr, GHC.GHCi, GHC.GHCi.Helpers, GHC.Generics, GHC.IO, GHC.IO.Buffer, GHC.IO.BufferedIO, GHC.IO.Device, GHC.IO.Encoding, GHC.IO.Encoding.CodePage, GHC.IO.Encoding.Failure, GHC.IO.Encoding.Iconv, GHC.IO.Encoding.Latin1, GHC.IO.Encoding.Types, GHC.IO.Encoding.UTF16, GHC.IO.Encoding.UTF32, GHC.IO.Encoding.UTF8, GHC.IO.Exception, GHC.IO.FD, GHC.IO.Handle, GHC.IO.Handle.FD, GHC.IO.Handle.Internals, GHC.IO.Handle.Lock, GHC.IO.Handle.Text, GHC.IO.Handle.Types, GHC.IO.IOMode, GHC.IO.Unsafe, GHC.IOArray, GHC.IORef, GHC.Int, GHC.Ix, GHC.List, GHC.MVar, GHC.Maybe, GHC.Natural, GHC.Num, GHC.OldList, GHC.OverloadedLabels, GHC.Pack, GHC.Profiling, GHC.Ptr, GHC.RTS.Flags, GHC.Read, GHC.Real, GHC.Records, GHC.ResponseFile, GHC.ST, GHC.STRef, GHC.Show, GHC.Stable, GHC.StableName, GHC.Stack, GHC.Stack.CCS, GHC.Stack.Types, GHC.StaticPtr, GHC.Stats, GHC.Storable, GHC.TopHandler, GHC.TypeLits, GHC.TypeNats, GHC.Unicode, GHC.Weak, GHC.Word, Numeric, Numeric.Natural, System.CPUTime, System.Console.GetOpt, System.Environment, System.Environment.Blank, System.Exit, System.IO, System.IO.Error, System.IO.Unsafe, System.Info, System.Mem, System.Mem.StableName, System.Mem.Weak, System.Posix.Internals, System.Posix.Types, System.Timeout, Text.ParserCombinators.ReadP, Text.ParserCombinators.ReadPrec, Text.Printf, Text.Read, Text.Read.Lex, Text.Show, Text.Show.Functions, Type.Reflection, Type.Reflection.Unsafe, Unsafe.Coerce)'
    -package-id 'relude-1.2.0.0-5TwSLtsvuDerkKsbQoUSN (Relude as Prelude)'
)

extensions=(
    -XBangPatterns
    -XDeriveDataTypeable
    -XDeriveFoldable
    -XDeriveFunctor
    -XDeriveGeneric
    -XDeriveLift
    -XDeriveTraversable
    -XFlexibleContexts
    -XFlexibleInstances
    -XHaskell2010
    -XImportQualifiedPost
    -XInstanceSigs
    -XLambdaCase
    -XMultiParamTypeClasses
    -XOverloadedStrings
    -XStandaloneDeriving
    -XTupleSections
    -XTypeApplications
    -XTypeSynonymInstances
    -XViewPatterns
)

exec ghci \
     "${pkgs[@]}" \
     "${extensions[@]}" \
     -i${PWD}/hnix-store-{core,remote}/{src,tests} \
     "$@"
