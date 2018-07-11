{-# LANGUAGE CPP, DeriveGeneric, StandaloneDeriving,
    UnboxedTuples, MagicHash, RecordWildCards,
    DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-orphans #-}

module GHCi.Closure
  ( ClosureType(..)
  , GenClosure(..)
  , Closure
  , getClosureData
  ) where

import GHCi.Closure.Utils
import GHCi.RemoteTypes
import GHCi.InfoTable as InfoTable

import Data.Binary
import Foreign
import GHC.Arr          ( Array(..) )
import GHC.Exts
import GHC.Generics

-------------------------------------------------------------------------
-- Runtime Closure Datatype and functions for retrieving closure related stuff
-------------------------------------------------------------------------
data ClosureType = Constr
                 | Fun
                 | Thunk Int
                 | ThunkSelector
                 | Blackhole
                 | AP
                 | PAP
                 | Indirection Int
                 | MutVar Int
                 | MVar   Int
                 | ArrWords
                 | Other  Int
 deriving (Show, Eq, Generic)


data GenClosure a = GenClosure
  { tipe         :: ClosureType
  , constrName   :: Maybe (String,String,String)
  , infoPtr      :: Ptr ()
  , infoTable    :: StgInfoTable
  , ptrs         :: Array Int a
  , nonPtrs      :: [Word]
  }
  deriving (Generic, Functor, Foldable, Traversable)

deriving instance Show a => Show (GenClosure a)
           
type Closure = GenClosure ForeignHValue

-- Orphan instances of Binary for Ptr / FunPtr by conversion to Word64.
-- This is to support Binary StgInfoTable which includes these.
instance Binary (Ptr a) where
  put p = put (fromIntegral (ptrToWordPtr p) :: Word64)
  get = (wordPtrToPtr . fromIntegral) <$> (get :: Get Word64)

instance Binary (FunPtr a) where
  put = put . castFunPtrToPtr
  get = castPtrToFunPtr <$> get

-- Binary instances to support the GetClosure message
instance Binary StgInfoTable
instance Binary ClosureType
instance Binary a => Binary (GenClosure a)

##include "../../../../../includes/rts/storage/ClosureTypes.h"

aP_CODE, pAP_CODE :: Int
aP_CODE = AP
pAP_CODE = PAP
##undef AP
##undef PAP

#include "Rts.h"

ghciTablesNextToCode :: Bool
#if defined(TABLES_NEXT_TO_CODE)
ghciTablesNextToCode = True
#else
ghciTablesNextToCode = False
#endif

getClosureData :: a -> IO (GenClosure HValue)
getClosureData a =
   case unpackClosure## a of
     (## iptr, ptrs, nptrs ##) -> do
           let iptr0 = Ptr iptr
           let iptr1
                | ghciTablesNextToCode = iptr0
                | otherwise =
                   -- the info pointer we get back from unpackClosure#
                   -- is to the beginning of the standard info table,
                   -- but the Storable instance for info tables takes
                   -- into account the extra entry pointer when
                   -- !ghciTablesNextToCode, so we must adjust here:
                   iptr0 `plusPtr` negate (#const SIZEOF_HSINT)
           itbl <- peekItbl iptr1
           let tipe = readCType (InfoTable.tipe itbl)
               elems = I## (sizeofArray## ptrs)
               ptrsList = Array 0 (elems - 1) elems ptrs
               nptrs_data = [W## (indexWordArray## nptrs i)
                            | I## i <- [0.. fromIntegral (InfoTable.nptrs itbl)-1] ]
           names <- case tipe of
             Constr -> Just <$> dataConNames iptr0
             _other -> return Nothing
           ptrsList `seq`
            return (GenClosure tipe names iptr0 itbl ptrsList nptrs_data)

readCType :: Integral a => a -> ClosureType
readCType i
 | i >= CONSTR && i <= CONSTR_NOCAF        = Constr
 | i >= FUN    && i <= FUN_STATIC          = Fun
 | i >= THUNK  && i < THUNK_SELECTOR       = Thunk i'
 | i == THUNK_SELECTOR                     = ThunkSelector
 | i == BLACKHOLE                          = Blackhole
 | i >= IND    && i <= IND_STATIC          = Indirection i'
 | i' == aP_CODE                           = AP
 | i == AP_STACK                           = AP
 | i' == pAP_CODE                          = PAP
 | i == MUT_VAR_CLEAN || i == MUT_VAR_DIRTY= MutVar i'
 | i == MVAR_CLEAN    || i == MVAR_DIRTY   = MVar i'
 | i == ARR_WORDS                          = ArrWords
 | otherwise                               = Other  i'
  where i' = fromIntegral i
