{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Tyche.Par where
import Tyche.Abs
import Tyche.Lex
import Tyche.ErrM
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.11

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
newtype HappyWrap31 = HappyWrap31 ((Maybe (Int, Int), Ident))
happyIn31 :: ((Maybe (Int, Int), Ident)) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap31 x)
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> HappyWrap31
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
newtype HappyWrap32 = HappyWrap32 ((Maybe (Int, Int), Integer))
happyIn32 :: ((Maybe (Int, Int), Integer)) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap32 x)
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> HappyWrap32
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
newtype HappyWrap33 = HappyWrap33 ((Maybe (Int, Int), String))
happyIn33 :: ((Maybe (Int, Int), String)) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap33 x)
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> HappyWrap33
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
newtype HappyWrap34 = HappyWrap34 ((Maybe (Int, Int), Double))
happyIn34 :: ((Maybe (Int, Int), Double)) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap34 x)
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> HappyWrap34
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
newtype HappyWrap35 = HappyWrap35 ((Maybe (Int, Int), Program (Maybe (Int, Int))))
happyIn35 :: ((Maybe (Int, Int), Program (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap35 x)
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> HappyWrap35
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
newtype HappyWrap36 = HappyWrap36 ((Maybe (Int, Int), Arg (Maybe (Int, Int))))
happyIn36 :: ((Maybe (Int, Int), Arg (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap36 x)
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> HappyWrap36
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
newtype HappyWrap37 = HappyWrap37 ((Maybe (Int, Int), [Arg (Maybe (Int, Int))]))
happyIn37 :: ((Maybe (Int, Int), [Arg (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap37 x)
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> HappyWrap37
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
newtype HappyWrap38 = HappyWrap38 ((Maybe (Int, Int), Stmt (Maybe (Int, Int))))
happyIn38 :: ((Maybe (Int, Int), Stmt (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap38 x)
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> HappyWrap38
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
newtype HappyWrap39 = HappyWrap39 ((Maybe (Int, Int), [Stmt (Maybe (Int, Int))]))
happyIn39 :: ((Maybe (Int, Int), [Stmt (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap39 x)
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> HappyWrap39
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
newtype HappyWrap40 = HappyWrap40 ((Maybe (Int, Int), Type (Maybe (Int, Int))))
happyIn40 :: ((Maybe (Int, Int), Type (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap40 x)
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> HappyWrap40
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
newtype HappyWrap41 = HappyWrap41 ((Maybe (Int, Int), ArgType (Maybe (Int, Int))))
happyIn41 :: ((Maybe (Int, Int), ArgType (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap41 x)
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> HappyWrap41
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
newtype HappyWrap42 = HappyWrap42 ((Maybe (Int, Int), FullType (Maybe (Int, Int))))
happyIn42 :: ((Maybe (Int, Int), FullType (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap42 x)
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> HappyWrap42
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
newtype HappyWrap43 = HappyWrap43 ((Maybe (Int, Int), [ArgType (Maybe (Int, Int))]))
happyIn43 :: ((Maybe (Int, Int), [ArgType (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap43 x)
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> HappyWrap43
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
newtype HappyWrap44 = HappyWrap44 ((Maybe (Int, Int), ArgMod (Maybe (Int, Int))))
happyIn44 :: ((Maybe (Int, Int), ArgMod (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap44 x)
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> HappyWrap44
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
newtype HappyWrap45 = HappyWrap45 ((Maybe (Int, Int), TypeMod (Maybe (Int, Int))))
happyIn45 :: ((Maybe (Int, Int), TypeMod (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap45 x)
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> HappyWrap45
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
newtype HappyWrap46 = HappyWrap46 ((Maybe (Int, Int), [TypeMod (Maybe (Int, Int))]))
happyIn46 :: ((Maybe (Int, Int), [TypeMod (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap46 x)
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> HappyWrap46
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
newtype HappyWrap47 = HappyWrap47 ((Maybe (Int, Int), Expr (Maybe (Int, Int))))
happyIn47 :: ((Maybe (Int, Int), Expr (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap47 x)
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> HappyWrap47
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
newtype HappyWrap48 = HappyWrap48 ((Maybe (Int, Int), Expr (Maybe (Int, Int))))
happyIn48 :: ((Maybe (Int, Int), Expr (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap48 x)
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> HappyWrap48
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
newtype HappyWrap49 = HappyWrap49 ((Maybe (Int, Int), Expr (Maybe (Int, Int))))
happyIn49 :: ((Maybe (Int, Int), Expr (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap49 x)
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> HappyWrap49
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
newtype HappyWrap50 = HappyWrap50 ((Maybe (Int, Int), Expr (Maybe (Int, Int))))
happyIn50 :: ((Maybe (Int, Int), Expr (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap50 x)
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> HappyWrap50
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
newtype HappyWrap51 = HappyWrap51 ((Maybe (Int, Int), Expr (Maybe (Int, Int))))
happyIn51 :: ((Maybe (Int, Int), Expr (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap51 x)
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> HappyWrap51
happyOut51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut51 #-}
newtype HappyWrap52 = HappyWrap52 ((Maybe (Int, Int), Expr (Maybe (Int, Int))))
happyIn52 :: ((Maybe (Int, Int), Expr (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn52 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap52 x)
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> HappyWrap52
happyOut52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut52 #-}
newtype HappyWrap53 = HappyWrap53 ((Maybe (Int, Int), Expr (Maybe (Int, Int))))
happyIn53 :: ((Maybe (Int, Int), Expr (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn53 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap53 x)
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> HappyWrap53
happyOut53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut53 #-}
newtype HappyWrap54 = HappyWrap54 ((Maybe (Int, Int), Expr (Maybe (Int, Int))))
happyIn54 :: ((Maybe (Int, Int), Expr (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn54 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap54 x)
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn ) -> HappyWrap54
happyOut54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut54 #-}
newtype HappyWrap55 = HappyWrap55 ((Maybe (Int, Int), Expr (Maybe (Int, Int))))
happyIn55 :: ((Maybe (Int, Int), Expr (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn55 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap55 x)
{-# INLINE happyIn55 #-}
happyOut55 :: (HappyAbsSyn ) -> HappyWrap55
happyOut55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut55 #-}
newtype HappyWrap56 = HappyWrap56 ((Maybe (Int, Int), Expr (Maybe (Int, Int))))
happyIn56 :: ((Maybe (Int, Int), Expr (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn56 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap56 x)
{-# INLINE happyIn56 #-}
happyOut56 :: (HappyAbsSyn ) -> HappyWrap56
happyOut56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut56 #-}
newtype HappyWrap57 = HappyWrap57 ((Maybe (Int, Int), [Expr (Maybe (Int, Int))]))
happyIn57 :: ((Maybe (Int, Int), [Expr (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn57 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap57 x)
{-# INLINE happyIn57 #-}
happyOut57 :: (HappyAbsSyn ) -> HappyWrap57
happyOut57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut57 #-}
newtype HappyWrap58 = HappyWrap58 ((Maybe (Int, Int), AddOp (Maybe (Int, Int))))
happyIn58 :: ((Maybe (Int, Int), AddOp (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn58 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap58 x)
{-# INLINE happyIn58 #-}
happyOut58 :: (HappyAbsSyn ) -> HappyWrap58
happyOut58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut58 #-}
newtype HappyWrap59 = HappyWrap59 ((Maybe (Int, Int), MulOp (Maybe (Int, Int))))
happyIn59 :: ((Maybe (Int, Int), MulOp (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn59 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap59 x)
{-# INLINE happyIn59 #-}
happyOut59 :: (HappyAbsSyn ) -> HappyWrap59
happyOut59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut59 #-}
newtype HappyWrap60 = HappyWrap60 ((Maybe (Int, Int), RelOp (Maybe (Int, Int))))
happyIn60 :: ((Maybe (Int, Int), RelOp (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn60 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap60 x)
{-# INLINE happyIn60 #-}
happyOut60 :: (HappyAbsSyn ) -> HappyWrap60
happyOut60 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut60 #-}
newtype HappyWrap61 = HappyWrap61 ((Maybe (Int, Int), OrOp (Maybe (Int, Int))))
happyIn61 :: ((Maybe (Int, Int), OrOp (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn61 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap61 x)
{-# INLINE happyIn61 #-}
happyOut61 :: (HappyAbsSyn ) -> HappyWrap61
happyOut61 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut61 #-}
newtype HappyWrap62 = HappyWrap62 ((Maybe (Int, Int), AndOp (Maybe (Int, Int))))
happyIn62 :: ((Maybe (Int, Int), AndOp (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn62 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap62 x)
{-# INLINE happyIn62 #-}
happyOut62 :: (HappyAbsSyn ) -> HappyWrap62
happyOut62 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut62 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x80\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x60\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x70\x40\x01\x48\x40\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1c\x50\x00\x12\x10\x02\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x84\x00\x42\x00\x08\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x60\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x80\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x20\x00\x04\x00\x00\x49\x1e\x00\x00\x00\x00\x00\x00\x00\x10\x21\x00\x08\x00\x01\x02\x40\x92\x07\x00\x00\x00\x00\x00\x00\x00\x44\x08\x00\x02\x40\x80\x00\x90\xe4\x01\x00\x00\x00\x00\x00\x00\x00\x11\x02\x80\x00\x10\x20\x00\x24\x79\x00\x00\x00\x00\x00\x00\x00\x40\x84\x00\x20\x00\x04\x08\x00\x49\x1e\x00\x00\x00\x00\x00\x00\x00\x10\x21\x00\x08\x00\x01\x02\x40\x92\x07\x00\x00\x00\x00\x00\x00\x00\x44\x08\x00\x02\x40\x80\x00\x90\xe4\x01\x00\x00\x00\x00\x00\x00\x00\x11\x02\x80\x00\x10\x20\x00\x24\x79\x00\x00\x00\x00\x00\x00\x00\x40\x84\x00\x30\x01\x04\x08\x00\x69\x1e\x00\x00\x00\x00\x00\x00\x00\x10\x21\x00\x4c\x00\x81\x32\x40\x9a\x07\x00\x00\x00\x00\x00\x00\x00\x44\x08\x00\x13\x40\xa0\x0c\x90\xe6\x01\x00\x00\x00\x00\x00\x00\x00\x80\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x11\x04\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\xd8\x01\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x10\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x00\xd8\x21\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x80\x00\x10\x00\x00\x24\x79\x00\x00\x00\x00\x00\x00\x00\x40\x84\x00\x30\x01\x04\xca\x00\x69\x1e\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x08\x00\x01\x00\x40\x92\x07\x00\x00\x00\x00\x00\x00\x00\x44\x08\x00\x13\x40\xa0\x0c\x90\xe6\x01\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x84\x00\x30\x01\x04\xca\x00\x69\x1e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x80\x00\x10\x00\x00\x24\x79\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x84\x00\x30\x01\x04\xca\x00\x69\x1e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\xd8\x01\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x10\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x21\x80\x10\x10\x82\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x80\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x40\x84\x00\x30\x01\x04\xca\x00\x69\x1e\x00\x00\x00\x00\x00\x00\x00\x10\x21\x00\x4c\x00\x81\x32\x40\x9a\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x11\x02\xc0\x04\x10\x28\x03\xa4\x79\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x70\x40\x01\x48\x40\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x80\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1c\x50\x00\x12\x10\x02\x00\x00\x00\x00\x00\x00\x00\x10\x21\x00\x4c\x00\x81\x32\x40\x9a\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x10\x21\x00\x4c\x00\x81\x32\x40\x9a\x07\x00\x00\x00\x00\x00\x00\x00\x44\x08\x00\x13\x40\xa0\x0c\x90\xe6\x01\x00\x00\x00\x00\x00\x00\x00\x11\x02\x80\x00\x10\x20\x00\x24\x79\x00\x00\x00\x00\x00\x00\x00\x40\x84\x00\x20\x00\x04\x08\x00\x49\x1e\x00\x00\x00\x00\x00\x00\x00\x10\x21\x00\x08\x00\x01\x02\x40\x92\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x40\x84\x00\x30\x01\x04\x08\x00\x69\x1e\x00\x00\x00\x00\x00\x00\x00\x10\x21\x00\x4c\x00\x01\x02\x40\x9a\x07\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x08\x00\x13\x40\xa0\x0c\x90\xe6\x01\x00\x00\x00\x00\x00\x00\x00\x11\x02\xc0\x04\x10\x20\x00\xa4\x79\x00\x00\x00\x00\x00\x00\x00\x40\x84\x00\x20\x00\x04\x08\x00\x49\x1e\x00\x00\x00\x00\x00\x00\x00\x10\x21\x00\x08\x00\x01\x02\x40\x92\x07\x00\x00\x00\x00\x00\x00\x00\x44\x08\x00\x02\x40\x80\x00\x90\xe4\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x11\x02\xc0\x04\x10\x28\x03\xa4\x79\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x41\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x08\x00\x13\x40\xa0\x0c\x90\xe6\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x01\x05\x20\x01\x21\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x70\x40\x01\x48\x40\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x21\x00\x4c\x00\x81\x32\x40\x9a\x07\x00\x00\x00\x00\x00\x00\x00\x40\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x08\x00\x13\x40\x80\x00\x90\xe6\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x08\x00\x13\x40\x80\x00\x90\xe6\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x80\x01\x00\x00\x00\x00\x00\x00\x00\x00\x44\x08\x00\x13\x40\xa0\x0c\x90\xe6\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x84\x00\x30\x01\x04\xca\x00\x69\x1e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x07\x14\x80\x04\x84\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x70\x40\x01\x48\x40\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x70\x40\x01\x48\x40\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x70\x40\x01\x48\x40\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x10\x21\x00\x4c\x00\x01\x02\x40\x9a\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x01\x05\x20\x01\x21\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x07\x14\x80\x04\x84\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram_internal","%start_pArg_internal","%start_pListArg_internal","%start_pStmt_internal","%start_pListStmt_internal","%start_pType_internal","%start_pArgType_internal","%start_pFullType_internal","%start_pListArgType_internal","%start_pArgMod_internal","%start_pTypeMod_internal","%start_pListTypeMod_internal","%start_pExpr9_internal","%start_pExpr8_internal","%start_pExpr7_internal","%start_pExpr6_internal","%start_pExpr5_internal","%start_pExpr4_internal","%start_pExpr3_internal","%start_pExpr2_internal","%start_pExpr1_internal","%start_pExpr_internal","%start_pListExpr_internal","%start_pAddOp_internal","%start_pMulOp_internal","%start_pRelOp_internal","%start_pOrOp_internal","%start_pAndOp_internal","Ident","Integer","String","Double","Program","Arg","ListArg","Stmt","ListStmt","Type","ArgType","FullType","ListArgType","ArgMod","TypeMod","ListTypeMod","Expr9","Expr8","Expr7","Expr6","Expr5","Expr4","Expr3","Expr2","Expr1","Expr","ListExpr","AddOp","MulOp","RelOp","OrOp","AndOp","'!'","'!='","'%'","'&&'","'('","')'","'*'","'+'","','","'-'","'->'","'.'","'/'","':'","';'","'<'","'<='","'='","'=='","'>'","'>='","'?'","'['","'[]'","']'","'and'","'array'","'boolean'","'break'","'continue'","'def'","'distribution'","'do'","'each'","'else'","'equals'","'false'","'float'","'for'","'from'","'if'","'inout'","'int'","'lambda'","'mod'","'not'","'of'","'or'","'probability'","'random'","'readonly'","'return'","'sampled'","'satisfying'","'skip'","'string'","'times'","'to'","'true'","'val'","'var'","'void'","'while'","'{'","'{}'","'||'","'}'","L_ident","L_integ","L_quoted","L_doubl","%eof"]
        bit_start = st * 134
        bit_end = (st + 1) * 134
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..133]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\xf6\xff\xf4\xff\xf4\xff\x70\x00\x70\x00\x75\x00\xf4\xff\x00\x00\xf4\xff\xf4\xff\x08\x00\x00\x00\x44\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x10\x00\x01\x00\x01\x00\x22\x00\x09\x00\x60\x00\xe3\xff\x03\x00\x0e\x00\x00\x00\xf2\xff\x00\x00\x00\x00\xf2\xff\x00\x00\x00\x00\xf2\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf2\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf2\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0a\x00\x62\x00\x00\x00\x09\x00\x22\x00\xa7\x00\xfe\xff\x00\x00\x66\x00\x74\x00\x6e\x00\x44\x00\x01\x00\x44\x00\x01\x00\x88\x00\x01\x00\x00\x00\x8e\x00\x44\x00\x79\x00\x9d\x00\x00\x00\x00\x00\x01\x00\xbe\x00\x00\x00\x00\x00\x00\x00\x7e\x00\x7e\x00\x7e\x00\x7e\x00\x57\x00\xfb\xff\x07\x00\x7e\x00\x7e\x00\x04\x00\xda\xff\x7e\x00\x00\x00\x7e\x00\x00\x00\x00\x00\x00\x00\xd6\x00\x9f\x00\x00\x00\x9f\x00\x6a\x00\x9f\x00\x9f\x00\xf4\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf1\x00\xf5\x00\xc5\x00\x00\x00\x00\x00\xe4\x00\xdf\xff\x01\x00\x01\x00\x00\x00\x01\x00\xe1\x00\x43\x01\x05\x01\x2c\x01\x29\x01\x29\x01\x32\x01\x70\x00\x66\x01\xf4\xff\x54\x01\x00\x00\x74\x01\x6e\x01\x50\x01\x8a\x01\x70\x00\x01\x00\x56\x01\x9f\x01\xb3\x01\x00\x00\x00\x00\x00\x00\xf4\xff\x01\x00\x01\x00\x1f\x00\x1f\x00\x1f\x00\x00\x00\x77\x01\x10\x00\x10\x00\x33\x00\x00\x00\x83\x01\x00\x00\xa4\x01\x33\x00\xd6\x01\x33\x00\x01\x00\x10\x00\x1f\x00\x1f\x00\x1f\x00\x00\x00\x00\x00\x00\x00\xcf\x01\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\xd9\x01\xa7\x01\xc1\x01\x00\x00\x00\x00\x22\x00\x09\x00\x00\x00\xe7\x01\xfb\x01\x00\x00\xf7\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdc\x01\x01\x00\xc5\x01\xe3\x01\x00\x00\x00\x00\xe1\x01\x00\x00\x00\x00\x70\x00\x70\x00\xec\x01\x01\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x10\x00\xf8\x01\xf4\xff\x20\x02\x10\x00\x00\x00\x00\x00\x3f\x02\x08\x02\x00\x00\x00\x00\xf4\xff\x01\x00\x2f\x02\x01\x00\x24\x02\x26\x02\x00\x00\x23\x02\x49\x02\x09\x02\x00\x00\x62\x02\x70\x00\x4f\x02\x00\x00\x2b\x02\x2e\x02\x51\x02\x70\x00\x4b\x02\x68\x02\x4c\x02\x70\x00\x4e\x02\x52\x02\x54\x02\x70\x00\x50\x02\x10\x00\x70\x00\x00\x00\x66\x02\x70\x00\x67\x02\x00\x00\x6d\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x90\x02\x4e\x00\x71\x00\x58\x00\x9e\x00\xa2\x02\x54\x00\xf9\xff\xfb\x00\xa4\x02\xa6\x02\xa3\x02\x0a\x03\x06\x03\xeb\x02\xe3\x02\xc9\x02\xc3\x02\xa5\x02\x85\x02\x06\x02\x24\x01\xbd\x00\x98\x02\xab\x02\xb0\x02\xaa\x02\xb3\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe2\x02\xfa\x02\x84\x00\xfb\x02\x00\x00\x00\x00\x00\x00\x00\x00\x0e\x03\x2e\x01\x1f\x03\xd8\x00\x00\x00\x48\x01\x00\x00\x00\x00\x23\x03\x00\x00\x00\x00\x00\x00\x00\x00\xe3\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfe\x02\xfd\x02\x00\x03\x00\x00\x00\x00\x00\x00\x0f\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2e\x00\x00\x00\x64\x00\x00\x00\x00\x00\x20\x01\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x89\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x03\x28\x03\x52\x01\x6c\x01\x00\x00\x76\x01\x00\x00\x00\x00\x00\x00\x29\x03\x00\x00\x00\x00\x00\x00\xc2\x00\x00\x00\x73\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2a\x03\x00\x00\x2b\x01\x90\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x01\xfe\x00\x9a\x01\xef\x02\xe7\x02\xce\x02\xa1\x00\x00\x00\x1f\x02\x28\x02\x00\x00\xb8\x00\x00\x00\xb9\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x01\x41\x02\x8d\x02\xac\x02\x02\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb4\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x03\x11\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd1\x00\x00\x00\xbe\x01\x00\x00\x00\x00\x00\x00\xd2\x00\x00\x00\x00\x00\x00\x00\x4f\x01\x73\x01\x00\x00\xd8\x01\x00\x00\xd3\x00\x00\x00\x00\x00\x4a\x02\x00\x00\x8d\x00\x00\x00\x63\x02\x00\x00\xf7\x00\x00\x00\x00\x00\x00\x00\x00\x00\xac\x00\xe2\x01\x00\x00\xfc\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x97\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbb\x01\x00\x00\x00\x00\x00\x00\xdf\x01\x00\x00\x00\x00\x00\x00\x03\x02\x00\x00\x6c\x02\x25\x02\x00\x00\x00\x00\x47\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\xdd\xff\x00\x00\x00\x00\x00\x00\x00\x00\xbb\xff\xc2\xff\x00\x00\x00\x00\xbb\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x93\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe3\xff\x00\x00\x80\xff\x81\xff\x00\x00\x83\xff\x82\xff\x00\x00\x84\xff\x8a\xff\x89\xff\x86\xff\x88\xff\x87\xff\x85\xff\x00\x00\x8c\xff\x8e\xff\x8d\xff\x8b\xff\x00\x00\x90\xff\x8f\xff\xb8\xff\xb7\xff\xb4\xff\xb3\xff\xaa\xff\xa8\xff\xa6\xff\xa4\xff\xa2\xff\xa0\xff\x9e\xff\x9a\xff\x94\xff\x92\xff\x00\x00\x00\x00\x00\x00\x00\x00\x93\xff\x00\x00\x00\x00\xb5\xff\x00\x00\x00\x00\x00\x00\x00\x00\xb6\xff\xb9\xff\x93\xff\x00\x00\xe2\xff\xe1\xff\xe0\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbc\xff\x00\x00\xbd\xff\xbe\xff\xbf\xff\xc1\xff\x00\x00\xbb\xff\x00\x00\x00\x00\x00\x00\x00\x00\xc2\xff\xbb\xff\xca\xff\xc8\xff\xcc\xff\xcb\xff\xc9\xff\xbb\xff\x00\x00\xce\xff\x00\x00\xd9\xff\xd8\xff\x00\x00\x00\x00\x00\x00\x00\x00\xda\xff\x00\x00\x00\x00\xdc\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdd\xff\x00\x00\xd7\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc3\xff\xba\xff\xc4\xff\xc2\xff\x93\xff\x00\x00\x00\x00\x00\x00\x00\x00\xbb\xff\x00\x00\x00\x00\x00\x00\xab\xff\xbb\xff\x00\x00\xbb\xff\x00\x00\xad\xff\x00\x00\xac\xff\x93\xff\x00\x00\x00\x00\x00\x00\x00\x00\xa9\xff\xa1\xff\x9f\xff\x00\x00\x91\xff\xae\xff\x9d\xff\xb2\xff\x00\x00\x00\x00\x00\x00\x97\xff\x9c\xff\xb1\xff\xa3\xff\xa5\xff\xa7\xff\x00\x00\x00\x00\xc0\xff\x00\x00\xc7\xff\xc6\xff\xd5\xff\xcd\xff\xbb\xff\x00\x00\x00\x00\x00\x00\x00\x00\xdb\xff\xbb\xff\x00\x00\xdf\xff\xde\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbb\xff\xb0\xff\xaf\xff\x00\x00\x00\x00\xdd\xff\x00\x00\x00\x00\x99\xff\xbb\xff\x00\x00\x00\x00\x96\xff\xc5\xff\xdd\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd1\xff\xd3\xff\x00\x00\x00\x00\xd6\xff\x00\x00\x00\x00\x00\x00\x9b\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd0\xff\x00\x00\x00\x00\x00\x00\xcf\xff\x00\x00\x95\xff\x98\xff\xd4\xff\xd2\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x22\x00\x01\x00\x08\x00\x0b\x00\x0a\x00\x05\x00\x04\x00\x0f\x00\x05\x00\x03\x00\x0a\x00\x03\x00\x33\x00\x07\x00\x05\x00\x07\x00\x01\x00\x05\x00\x30\x00\x0d\x00\x05\x00\x0d\x00\x21\x00\x17\x00\x18\x00\x0a\x00\x17\x00\x1b\x00\x1a\x00\x2a\x00\x12\x00\x01\x00\x17\x00\x48\x00\x44\x00\x05\x00\x42\x00\x25\x00\x17\x00\x18\x00\x0a\x00\x08\x00\x1b\x00\x0a\x00\x2c\x00\x30\x00\x2e\x00\x3c\x00\x3d\x00\x31\x00\x32\x00\x2d\x00\x25\x00\x2d\x00\x18\x00\x05\x00\x0b\x00\x48\x00\x33\x00\x3b\x00\x0f\x00\x2e\x00\x3e\x00\x42\x00\x40\x00\x41\x00\x48\x00\x25\x00\x44\x00\x45\x00\x46\x00\x47\x00\x05\x00\x17\x00\x3b\x00\x48\x00\x2e\x00\x3e\x00\x48\x00\x40\x00\x41\x00\x44\x00\x05\x00\x44\x00\x45\x00\x46\x00\x47\x00\x00\x00\x02\x00\x3b\x00\x0d\x00\x18\x00\x3e\x00\x0a\x00\x07\x00\x41\x00\x0d\x00\x02\x00\x44\x00\x45\x00\x46\x00\x47\x00\x10\x00\x11\x00\x25\x00\x13\x00\x14\x00\x15\x00\x09\x00\x0c\x00\x05\x00\x10\x00\x11\x00\x0e\x00\x13\x00\x14\x00\x15\x00\x05\x00\x06\x00\x05\x00\x06\x00\x05\x00\x24\x00\x16\x00\x09\x00\x0d\x00\x3b\x00\x0d\x00\x17\x00\x3e\x00\x0b\x00\x24\x00\x41\x00\x1c\x00\x0f\x00\x44\x00\x45\x00\x46\x00\x47\x00\x17\x00\x1d\x00\x1e\x00\x1f\x00\x26\x00\x1c\x00\x05\x00\x06\x00\x0b\x00\x2b\x00\x0e\x00\x27\x00\x0f\x00\x29\x00\x0d\x00\x26\x00\x0e\x00\x33\x00\x00\x00\x48\x00\x2b\x00\x1d\x00\x38\x00\x1f\x00\x34\x00\x07\x00\x08\x00\x37\x00\x3e\x00\x02\x00\x40\x00\x04\x00\x0b\x00\x38\x00\x35\x00\x3f\x00\x0f\x00\x05\x00\x06\x00\x3e\x00\x44\x00\x40\x00\x48\x00\x10\x00\x11\x00\x0d\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1a\x00\x00\x00\x0b\x00\x0b\x00\x28\x00\x48\x00\x0f\x00\x0f\x00\x07\x00\x08\x00\x24\x00\x0e\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\x02\x00\x03\x00\x0b\x00\x0b\x00\x0b\x00\x09\x00\x0f\x00\x0f\x00\x0f\x00\x00\x00\x01\x00\x02\x00\x03\x00\x48\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\x02\x00\x03\x00\x0b\x00\x12\x00\x0f\x00\x0a\x00\x0f\x00\x0c\x00\x0d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x48\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\x02\x00\x03\x00\x44\x00\x48\x00\x0a\x00\x00\x00\x0c\x00\x0d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x07\x00\x08\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x00\x00\x01\x00\x02\x00\x03\x00\x09\x00\x48\x00\x0a\x00\x00\x00\x0c\x00\x0d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x07\x00\x08\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x00\x00\x01\x00\x02\x00\x03\x00\x44\x00\x48\x00\x40\x00\x00\x00\x0e\x00\x21\x00\x00\x00\x01\x00\x02\x00\x03\x00\x07\x00\x08\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x00\x00\x01\x00\x02\x00\x03\x00\x44\x00\x21\x00\x28\x00\x00\x00\x0e\x00\x43\x00\x00\x00\x01\x00\x02\x00\x03\x00\x07\x00\x08\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x00\x00\x01\x00\x02\x00\x03\x00\x19\x00\x06\x00\x43\x00\x00\x00\x39\x00\x19\x00\x00\x00\x01\x00\x02\x00\x03\x00\x07\x00\x08\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x00\x00\x01\x00\x02\x00\x03\x00\x06\x00\x0e\x00\x05\x00\x00\x00\x39\x00\x20\x00\x00\x00\x01\x00\x02\x00\x03\x00\x07\x00\x08\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x00\x00\x01\x00\x02\x00\x03\x00\x19\x00\x06\x00\x0b\x00\x00\x00\x28\x00\x40\x00\x00\x00\x01\x00\x02\x00\x03\x00\x07\x00\x08\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x01\x00\x02\x00\x03\x00\x40\x00\x43\x00\x00\x00\x3a\x00\x2f\x00\x00\x00\x01\x00\x02\x00\x03\x00\x07\x00\x08\x00\x0e\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x01\x00\x02\x00\x03\x00\x06\x00\x23\x00\x00\x00\x40\x00\x40\x00\x00\x00\x01\x00\x02\x00\x03\x00\x07\x00\x08\x00\x21\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x01\x00\x02\x00\x03\x00\x43\x00\x06\x00\x43\x00\x21\x00\x40\x00\x00\x00\x01\x00\x02\x00\x03\x00\x21\x00\x43\x00\x21\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x01\x00\x02\x00\x03\x00\x21\x00\x36\x00\x40\x00\x40\x00\x00\x00\x01\x00\x02\x00\x03\x00\x43\x00\x40\x00\x43\x00\x04\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x00\x00\x01\x00\x02\x00\x03\x00\x43\x00\x43\x00\x09\x00\x00\x00\x01\x00\x02\x00\x03\x00\x43\x00\x0d\x00\x0f\x00\x1b\x00\x0e\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1c\x00\x1e\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x10\x00\x11\x00\x12\x00\x13\x00\x10\x00\x11\x00\x12\x00\x13\x00\x10\x00\x11\x00\x12\x00\x1c\x00\x10\x00\x11\x00\x12\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x10\x00\x11\x00\x12\x00\x1b\x00\x10\x00\x11\x00\x1b\x00\x1e\x00\x10\x00\x1d\x00\x1c\x00\x0e\x00\x10\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1b\x00\xff\xff\x1c\x00\xff\xff\x10\x00\xff\xff\xff\xff\xff\xff\x10\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x8f\x00\x44\x00\x33\x00\x69\x00\x34\x00\x45\x00\x20\x00\x6a\x00\x9a\x00\x2e\x00\x46\x00\x2e\x00\x62\x00\x2f\x00\x9a\x00\x2f\x00\x44\x00\xe6\x00\x23\x00\x30\x00\x45\x00\x30\x00\x87\x00\x47\x00\x48\x00\x46\x00\x9b\x00\x49\x00\x21\x00\x64\x00\xe7\x00\x44\x00\x9b\x00\xff\xff\x1e\x00\x45\x00\x24\x00\x4a\x00\x47\x00\x48\x00\x46\x00\x33\x00\x49\x00\x34\x00\x4b\x00\x23\x00\x4c\x00\x65\x00\x66\x00\x4d\x00\x4e\x00\x31\x00\x4a\x00\x31\x00\x48\x00\x9a\x00\x97\x00\xff\xff\x62\x00\x4f\x00\x6a\x00\x4c\x00\x50\x00\x24\x00\x51\x00\x52\x00\xff\xff\x4a\x00\x1e\x00\x53\x00\x54\x00\x55\x00\x45\x00\x9b\x00\x4f\x00\xff\xff\x4c\x00\x50\x00\xff\xff\x51\x00\x52\x00\x1e\x00\x84\x00\x1e\x00\x53\x00\x54\x00\x55\x00\x75\x00\x26\x00\x4f\x00\x83\x00\x48\x00\x50\x00\x6b\x00\x80\x00\x52\x00\x68\x00\x26\x00\x1e\x00\x53\x00\x54\x00\x55\x00\x27\x00\x28\x00\x4a\x00\x29\x00\x2a\x00\x2b\x00\x95\x00\xaf\x00\x6e\x00\x27\x00\x28\x00\x96\x00\x29\x00\x2a\x00\x2b\x00\x81\x00\x82\x00\x81\x00\xcd\x00\x6e\x00\x2c\x00\xac\x00\xab\x00\x83\x00\x4f\x00\x83\x00\x6f\x00\x50\x00\x93\x00\x2c\x00\x52\x00\x70\x00\x6a\x00\x1e\x00\x53\x00\x54\x00\x55\x00\x6f\x00\x79\x00\x7a\x00\x7b\x00\x71\x00\x70\x00\x81\x00\xe1\x00\x92\x00\x72\x00\xa6\x00\x7c\x00\x6a\x00\x7d\x00\x83\x00\x71\x00\xa4\x00\x62\x00\x75\x00\xff\xff\x72\x00\x9d\x00\x73\x00\xad\x00\x7e\x00\x76\x00\x77\x00\x7f\x00\x74\x00\x26\x00\x75\x00\x20\x00\xbc\x00\x73\x00\xa2\x00\x80\x00\x6a\x00\x81\x00\xf0\x00\x74\x00\x1e\x00\x75\x00\xff\xff\x27\x00\x28\x00\x83\x00\x29\x00\x2a\x00\x2b\x00\x34\x00\x35\x00\x36\x00\x37\x00\x21\x00\x75\x00\xb8\x00\xb6\x00\xa1\x00\xff\xff\x6a\x00\x6a\x00\x76\x00\xcf\x00\x2c\x00\x9f\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x34\x00\x35\x00\x36\x00\x37\x00\xd6\x00\xd1\x00\xe4\x00\x99\x00\x6a\x00\x6a\x00\x6a\x00\x34\x00\x35\x00\x36\x00\x37\x00\xff\xff\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\xa6\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x9f\x00\x34\x00\x35\x00\x36\x00\x37\x00\xf3\x00\x92\x00\x91\x00\x66\x00\x6a\x00\x67\x00\x68\x00\x34\x00\x35\x00\x36\x00\x37\x00\xff\xff\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\xc1\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\xb3\x00\x34\x00\x35\x00\x36\x00\x37\x00\x1e\x00\xff\xff\x66\x00\x75\x00\x94\x00\x68\x00\x34\x00\x35\x00\x36\x00\x37\x00\x76\x00\xc7\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x55\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xa8\x00\x34\x00\x35\x00\x36\x00\x37\x00\x8a\x00\xff\xff\x66\x00\x75\x00\xc2\x00\x68\x00\x34\x00\x35\x00\x36\x00\x37\x00\x76\x00\xea\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xa4\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x8c\x00\x34\x00\x35\x00\x36\x00\x37\x00\x1e\x00\xff\xff\x88\x00\x75\x00\xcf\x00\xcd\x00\x34\x00\x35\x00\x36\x00\x37\x00\x76\x00\xe9\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x8b\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x8a\x00\x34\x00\x35\x00\x36\x00\x37\x00\x1e\x00\xcc\x00\xcb\x00\x75\x00\xc9\x00\xc6\x00\x34\x00\x35\x00\x36\x00\x37\x00\x76\x00\xf5\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xc6\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xc0\x00\x34\x00\x35\x00\x36\x00\x37\x00\xc5\x00\xc4\x00\xbc\x00\x75\x00\xb8\x00\xb6\x00\x34\x00\x35\x00\x36\x00\x37\x00\x76\x00\xfc\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xdd\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xd4\x00\x34\x00\x35\x00\x36\x00\x37\x00\xb5\x00\xdf\x00\xdd\x00\x75\x00\xdc\x00\xdb\x00\x34\x00\x35\x00\x36\x00\x37\x00\x76\x00\x04\x01\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xe7\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xef\x00\x34\x00\x35\x00\x36\x00\x37\x00\xda\x00\xd9\x00\xd8\x00\x75\x00\xd6\x00\xd4\x00\x34\x00\x35\x00\x36\x00\x37\x00\x76\x00\x00\x01\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xed\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x56\x00\x34\x00\x35\x00\x36\x00\x37\x00\xd3\x00\xd1\x00\x75\x00\xe9\x00\xe3\x00\x34\x00\x35\x00\x36\x00\x37\x00\x76\x00\x08\x01\xe1\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\xba\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\xb9\x00\x34\x00\x35\x00\x36\x00\x37\x00\xf3\x00\xfa\x00\x75\x00\xf2\x00\xf8\x00\x34\x00\x35\x00\x36\x00\x37\x00\x76\x00\x06\x01\xef\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\xb2\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\xe3\x00\x34\x00\x35\x00\x36\x00\x37\x00\xed\x00\xf7\x00\xec\x00\xf9\x00\x00\x01\x34\x00\x35\x00\x36\x00\x37\x00\xf5\x00\xff\x00\xfe\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\xdf\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x09\x01\x34\x00\x35\x00\x36\x00\x37\x00\xfb\x00\x02\x01\xfc\x00\x06\x01\x34\x00\x35\x00\x36\x00\x37\x00\x04\x01\x03\x01\x0b\x01\x85\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x57\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\xb1\x00\x34\x00\x35\x00\x36\x00\x37\x00\x08\x01\x0d\x01\x6c\x00\x34\x00\x35\x00\x36\x00\x37\x00\x0c\x01\x62\x00\x5f\x00\x31\x00\x60\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x58\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\xb0\x00\x34\x00\x35\x00\x36\x00\x37\x00\x2c\x00\x21\x00\x34\x00\x35\x00\x36\x00\x37\x00\x24\x00\x34\x00\x35\x00\x36\x00\x37\x00\x1e\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x59\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x5a\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\xbd\x00\x34\x00\x35\x00\x36\x00\x37\x00\x34\x00\x35\x00\x36\x00\x37\x00\x34\x00\x35\x00\x36\x00\x37\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x5b\x00\x38\x00\x39\x00\x3a\x00\xbe\x00\x38\x00\x39\x00\x5c\x00\x9b\x00\x38\x00\x39\x00\xbf\x00\x34\x00\x35\x00\x36\x00\x37\x00\x34\x00\x35\x00\x36\x00\x37\x00\x34\x00\x35\x00\x36\x00\x37\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\xaf\x00\x9c\x00\x38\x00\x5d\x00\x9c\x00\xac\x00\x5e\x00\x9d\x00\x9b\x00\x96\x00\xa9\x00\x34\x00\x35\x00\x36\x00\x37\x00\x34\x00\x35\x00\x36\x00\x37\x00\x8f\x00\x8d\x00\x88\x00\xc9\x00\x9c\x00\x00\x00\x9b\x00\x00\x00\xa7\x00\x00\x00\x00\x00\x00\x00\xa2\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (28, 127) [
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109),
	(110 , happyReduce_110),
	(111 , happyReduce_111),
	(112 , happyReduce_112),
	(113 , happyReduce_113),
	(114 , happyReduce_114),
	(115 , happyReduce_115),
	(116 , happyReduce_116),
	(117 , happyReduce_117),
	(118 , happyReduce_118),
	(119 , happyReduce_119),
	(120 , happyReduce_120),
	(121 , happyReduce_121),
	(122 , happyReduce_122),
	(123 , happyReduce_123),
	(124 , happyReduce_124),
	(125 , happyReduce_125),
	(126 , happyReduce_126),
	(127 , happyReduce_127)
	]

happy_n_terms = 73 :: Int
happy_n_nonterms = 32 :: Int

happyReduce_28 = happySpecReduce_1  0# happyReduction_28
happyReduction_28 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn31
		 ((Just (tokenLineCol happy_var_1), Ident (prToken happy_var_1))
	)}

happyReduce_29 = happySpecReduce_1  1# happyReduction_29
happyReduction_29 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn32
		 ((Just (tokenLineCol happy_var_1), read (prToken happy_var_1))
	)}

happyReduce_30 = happySpecReduce_1  2# happyReduction_30
happyReduction_30 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn33
		 ((Just (tokenLineCol happy_var_1), prToken happy_var_1)
	)}

happyReduce_31 = happySpecReduce_1  3# happyReduction_31
happyReduction_31 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn34
		 ((Just (tokenLineCol happy_var_1), read (prToken happy_var_1))
	)}

happyReduce_32 = happyReduce 4# 4# happyReduction_32
happyReduction_32 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_3 of { (HappyWrap39 happy_var_3) -> 
	happyIn35
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.Program (Just (tokenLineCol happy_var_1)) (snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_33 = happyReduce 4# 5# happyReduction_33
happyReduction_33 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut44 happy_x_1 of { (HappyWrap44 happy_var_1) -> 
	case happyOut31 happy_x_2 of { (HappyWrap31 happy_var_2) -> 
	case happyOut42 happy_x_4 of { (HappyWrap42 happy_var_4) -> 
	happyIn36
		 ((fst happy_var_1, Tyche.Abs.Arg (fst happy_var_1)(snd happy_var_1)(snd happy_var_2)(snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_34 = happySpecReduce_0  6# happyReduction_34
happyReduction_34  =  happyIn37
		 ((Nothing, [])
	)

happyReduce_35 = happySpecReduce_1  6# happyReduction_35
happyReduction_35 happy_x_1
	 =  case happyOut36 happy_x_1 of { (HappyWrap36 happy_var_1) -> 
	happyIn37
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_36 = happySpecReduce_3  6# happyReduction_36
happyReduction_36 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { (HappyWrap36 happy_var_1) -> 
	case happyOut37 happy_x_3 of { (HappyWrap37 happy_var_3) -> 
	happyIn37
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_37 = happySpecReduce_1  7# happyReduction_37
happyReduction_37 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn38
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.Skip (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_38 = happySpecReduce_1  7# happyReduction_38
happyReduction_38 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn38
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.Break (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_39 = happySpecReduce_1  7# happyReduction_39
happyReduction_39 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn38
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.Continue (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_40 = happySpecReduce_2  7# happyReduction_40
happyReduction_40 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	happyIn38
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.Ret (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_41 = happyReduce 6# 7# happyReduction_41
happyReduction_41 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { (HappyWrap31 happy_var_2) -> 
	case happyOut42 happy_x_4 of { (HappyWrap42 happy_var_4) -> 
	case happyOut56 happy_x_6 of { (HappyWrap56 happy_var_6) -> 
	happyIn38
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.VarDef (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_4)(snd happy_var_6))
	) `HappyStk` happyRest}}}}

happyReduce_42 = happySpecReduce_3  7# happyReduction_42
happyReduction_42 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { (HappyWrap31 happy_var_1) -> 
	case happyOut56 happy_x_3 of { (HappyWrap56 happy_var_3) -> 
	happyIn38
		 ((fst happy_var_1, Tyche.Abs.Ass (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_43 = happyReduce 11# 7# happyReduction_43
happyReduction_43 (happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { (HappyWrap31 happy_var_2) -> 
	case happyOut42 happy_x_4 of { (HappyWrap42 happy_var_4) -> 
	case happyOut37 happy_x_6 of { (HappyWrap37 happy_var_6) -> 
	case happyOut39 happy_x_10 of { (HappyWrap39 happy_var_10) -> 
	happyIn38
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.FnDef (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_4)(snd happy_var_6)(snd happy_var_10))
	) `HappyStk` happyRest}}}}}

happyReduce_44 = happyReduce 6# 7# happyReduction_44
happyReduction_44 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	case happyOut39 happy_x_5 of { (HappyWrap39 happy_var_5) -> 
	happyIn38
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.Cond (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_5))
	) `HappyStk` happyRest}}}

happyReduce_45 = happyReduce 11# 7# happyReduction_45
happyReduction_45 (happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	case happyOut39 happy_x_5 of { (HappyWrap39 happy_var_5) -> 
	case happyOut39 happy_x_10 of { (HappyWrap39 happy_var_10) -> 
	happyIn38
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.CondElse (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_5)(snd happy_var_10))
	) `HappyStk` happyRest}}}}

happyReduce_46 = happyReduce 6# 7# happyReduction_46
happyReduction_46 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	case happyOut39 happy_x_5 of { (HappyWrap39 happy_var_5) -> 
	happyIn38
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.While (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_5))
	) `HappyStk` happyRest}}}

happyReduce_47 = happyReduce 9# 7# happyReduction_47
happyReduction_47 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { (HappyWrap31 happy_var_3) -> 
	case happyOut56 happy_x_5 of { (HappyWrap56 happy_var_5) -> 
	case happyOut39 happy_x_8 of { (HappyWrap39 happy_var_8) -> 
	happyIn38
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.ForList (Just (tokenLineCol happy_var_1)) (snd happy_var_3)(snd happy_var_5)(snd happy_var_8))
	) `HappyStk` happyRest}}}}

happyReduce_48 = happyReduce 10# 7# happyReduction_48
happyReduction_48 (happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { (HappyWrap31 happy_var_2) -> 
	case happyOut56 happy_x_4 of { (HappyWrap56 happy_var_4) -> 
	case happyOut56 happy_x_6 of { (HappyWrap56 happy_var_6) -> 
	case happyOut39 happy_x_9 of { (HappyWrap39 happy_var_9) -> 
	happyIn38
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.ForRange (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_4)(snd happy_var_6)(snd happy_var_9))
	) `HappyStk` happyRest}}}}}

happyReduce_49 = happySpecReduce_1  8# happyReduction_49
happyReduction_49 happy_x_1
	 =  case happyOut38 happy_x_1 of { (HappyWrap38 happy_var_1) -> 
	happyIn39
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_50 = happySpecReduce_3  8# happyReduction_50
happyReduction_50 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_1 of { (HappyWrap38 happy_var_1) -> 
	case happyOut39 happy_x_3 of { (HappyWrap39 happy_var_3) -> 
	happyIn39
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_51 = happySpecReduce_1  9# happyReduction_51
happyReduction_51 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn40
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.Int (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_52 = happySpecReduce_1  9# happyReduction_52
happyReduction_52 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn40
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.Str (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_53 = happySpecReduce_1  9# happyReduction_53
happyReduction_53 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn40
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.Bool (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_54 = happySpecReduce_1  9# happyReduction_54
happyReduction_54 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn40
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.Void (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_55 = happySpecReduce_1  9# happyReduction_55
happyReduction_55 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn40
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.Float (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_56 = happySpecReduce_3  9# happyReduction_56
happyReduction_56 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut42 happy_x_2 of { (HappyWrap42 happy_var_2) -> 
	happyIn40
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.List (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_57 = happySpecReduce_3  9# happyReduction_57
happyReduction_57 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut42 happy_x_2 of { (HappyWrap42 happy_var_2) -> 
	happyIn40
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.Array (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_58 = happyReduce 5# 9# happyReduction_58
happyReduction_58 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { (HappyWrap43 happy_var_2) -> 
	case happyOut42 happy_x_5 of { (HappyWrap42 happy_var_5) -> 
	happyIn40
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.Fun (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_5))
	) `HappyStk` happyRest}}}

happyReduce_59 = happySpecReduce_2  10# happyReduction_59
happyReduction_59 happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_1 of { (HappyWrap44 happy_var_1) -> 
	case happyOut42 happy_x_2 of { (HappyWrap42 happy_var_2) -> 
	happyIn41
		 ((fst happy_var_1, Tyche.Abs.ArgType (fst happy_var_1)(snd happy_var_1)(snd happy_var_2))
	)}}

happyReduce_60 = happySpecReduce_2  11# happyReduction_60
happyReduction_60 happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_1 of { (HappyWrap46 happy_var_1) -> 
	case happyOut40 happy_x_2 of { (HappyWrap40 happy_var_2) -> 
	happyIn42
		 ((fst happy_var_1, Tyche.Abs.FullType (fst happy_var_1)(reverse (snd happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_61 = happySpecReduce_0  12# happyReduction_61
happyReduction_61  =  happyIn43
		 ((Nothing, [])
	)

happyReduce_62 = happySpecReduce_1  12# happyReduction_62
happyReduction_62 happy_x_1
	 =  case happyOut41 happy_x_1 of { (HappyWrap41 happy_var_1) -> 
	happyIn43
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_63 = happySpecReduce_3  12# happyReduction_63
happyReduction_63 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut41 happy_x_1 of { (HappyWrap41 happy_var_1) -> 
	case happyOut43 happy_x_3 of { (HappyWrap43 happy_var_3) -> 
	happyIn43
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_64 = happySpecReduce_1  13# happyReduction_64
happyReduction_64 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn44
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.AModVar (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_65 = happySpecReduce_1  13# happyReduction_65
happyReduction_65 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn44
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.AModVal (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_66 = happySpecReduce_1  13# happyReduction_66
happyReduction_66 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn44
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.AModInOut (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_67 = happySpecReduce_1  14# happyReduction_67
happyReduction_67 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn45
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.TModReadonly (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_68 = happySpecReduce_0  15# happyReduction_68
happyReduction_68  =  happyIn46
		 ((Nothing, [])
	)

happyReduce_69 = happySpecReduce_2  15# happyReduction_69
happyReduction_69 happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_1 of { (HappyWrap46 happy_var_1) -> 
	case happyOut45 happy_x_2 of { (HappyWrap45 happy_var_2) -> 
	happyIn46
		 ((fst happy_var_1, flip (:) (snd happy_var_1)(snd happy_var_2))
	)}}

happyReduce_70 = happySpecReduce_1  16# happyReduction_70
happyReduction_70 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn47
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.ELitVoid (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_71 = happySpecReduce_1  16# happyReduction_71
happyReduction_71 happy_x_1
	 =  case happyOut31 happy_x_1 of { (HappyWrap31 happy_var_1) -> 
	happyIn47
		 ((fst happy_var_1, Tyche.Abs.EVar (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_72 = happySpecReduce_1  16# happyReduction_72
happyReduction_72 happy_x_1
	 =  case happyOut32 happy_x_1 of { (HappyWrap32 happy_var_1) -> 
	happyIn47
		 ((fst happy_var_1, Tyche.Abs.ELitInt (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_73 = happySpecReduce_1  16# happyReduction_73
happyReduction_73 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn47
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.ELitTrue (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_74 = happySpecReduce_1  16# happyReduction_74
happyReduction_74 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn47
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.ELitFalse (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_75 = happySpecReduce_1  16# happyReduction_75
happyReduction_75 happy_x_1
	 =  case happyOut33 happy_x_1 of { (HappyWrap33 happy_var_1) -> 
	happyIn47
		 ((fst happy_var_1, Tyche.Abs.EString (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_76 = happySpecReduce_1  16# happyReduction_76
happyReduction_76 happy_x_1
	 =  case happyOut34 happy_x_1 of { (HappyWrap34 happy_var_1) -> 
	happyIn47
		 ((fst happy_var_1, Tyche.Abs.ELitFloat (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_77 = happySpecReduce_3  16# happyReduction_77
happyReduction_77 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut42 happy_x_3 of { (HappyWrap42 happy_var_3) -> 
	happyIn47
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.EEmpList (Just (tokenLineCol happy_var_1)) (snd happy_var_3))
	)}}

happyReduce_78 = happySpecReduce_3  16# happyReduction_78
happyReduction_78 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut42 happy_x_3 of { (HappyWrap42 happy_var_3) -> 
	happyIn47
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.EEmpArray (Just (tokenLineCol happy_var_1)) (snd happy_var_3))
	)}}

happyReduce_79 = happyReduce 4# 16# happyReduction_79
happyReduction_79 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut47 happy_x_1 of { (HappyWrap47 happy_var_1) -> 
	case happyOut57 happy_x_3 of { (HappyWrap57 happy_var_3) -> 
	happyIn47
		 ((fst happy_var_1, Tyche.Abs.EApp (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_80 = happyReduce 4# 16# happyReduction_80
happyReduction_80 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut47 happy_x_1 of { (HappyWrap47 happy_var_1) -> 
	case happyOut56 happy_x_3 of { (HappyWrap56 happy_var_3) -> 
	happyIn47
		 ((fst happy_var_1, Tyche.Abs.EArrApp (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_81 = happySpecReduce_3  16# happyReduction_81
happyReduction_81 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	happyIn47
		 ((Just (tokenLineCol happy_var_1), snd happy_var_2)
	)}}

happyReduce_82 = happySpecReduce_2  17# happyReduction_82
happyReduction_82 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut47 happy_x_2 of { (HappyWrap47 happy_var_2) -> 
	happyIn48
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.Neg (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_83 = happySpecReduce_2  17# happyReduction_83
happyReduction_83 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut47 happy_x_2 of { (HappyWrap47 happy_var_2) -> 
	happyIn48
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.Not (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_84 = happySpecReduce_2  17# happyReduction_84
happyReduction_84 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut47 happy_x_2 of { (HappyWrap47 happy_var_2) -> 
	happyIn48
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.Not (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_85 = happySpecReduce_1  17# happyReduction_85
happyReduction_85 happy_x_1
	 =  case happyOut47 happy_x_1 of { (HappyWrap47 happy_var_1) -> 
	happyIn48
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_86 = happySpecReduce_3  18# happyReduction_86
happyReduction_86 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_1 of { (HappyWrap48 happy_var_1) -> 
	case happyOut49 happy_x_3 of { (HappyWrap49 happy_var_3) -> 
	happyIn49
		 ((fst happy_var_1, Tyche.Abs.ECons (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_87 = happySpecReduce_1  18# happyReduction_87
happyReduction_87 happy_x_1
	 =  case happyOut48 happy_x_1 of { (HappyWrap48 happy_var_1) -> 
	happyIn49
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_88 = happySpecReduce_3  19# happyReduction_88
happyReduction_88 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut50 happy_x_1 of { (HappyWrap50 happy_var_1) -> 
	case happyOut59 happy_x_2 of { (HappyWrap59 happy_var_2) -> 
	case happyOut49 happy_x_3 of { (HappyWrap49 happy_var_3) -> 
	happyIn50
		 ((fst happy_var_1, Tyche.Abs.EMul (fst happy_var_1)(snd happy_var_1)(snd happy_var_2)(snd happy_var_3))
	)}}}

happyReduce_89 = happySpecReduce_1  19# happyReduction_89
happyReduction_89 happy_x_1
	 =  case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	happyIn50
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_90 = happySpecReduce_3  20# happyReduction_90
happyReduction_90 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut51 happy_x_1 of { (HappyWrap51 happy_var_1) -> 
	case happyOut58 happy_x_2 of { (HappyWrap58 happy_var_2) -> 
	case happyOut50 happy_x_3 of { (HappyWrap50 happy_var_3) -> 
	happyIn51
		 ((fst happy_var_1, Tyche.Abs.EAdd (fst happy_var_1)(snd happy_var_1)(snd happy_var_2)(snd happy_var_3))
	)}}}

happyReduce_91 = happySpecReduce_1  20# happyReduction_91
happyReduction_91 happy_x_1
	 =  case happyOut50 happy_x_1 of { (HappyWrap50 happy_var_1) -> 
	happyIn51
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_92 = happySpecReduce_3  21# happyReduction_92
happyReduction_92 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_1 of { (HappyWrap52 happy_var_1) -> 
	case happyOut60 happy_x_2 of { (HappyWrap60 happy_var_2) -> 
	case happyOut51 happy_x_3 of { (HappyWrap51 happy_var_3) -> 
	happyIn52
		 ((fst happy_var_1, Tyche.Abs.ERel (fst happy_var_1)(snd happy_var_1)(snd happy_var_2)(snd happy_var_3))
	)}}}

happyReduce_93 = happySpecReduce_1  21# happyReduction_93
happyReduction_93 happy_x_1
	 =  case happyOut51 happy_x_1 of { (HappyWrap51 happy_var_1) -> 
	happyIn52
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_94 = happySpecReduce_3  22# happyReduction_94
happyReduction_94 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_1 of { (HappyWrap52 happy_var_1) -> 
	case happyOut62 happy_x_2 of { (HappyWrap62 happy_var_2) -> 
	case happyOut53 happy_x_3 of { (HappyWrap53 happy_var_3) -> 
	happyIn53
		 ((fst happy_var_1, Tyche.Abs.EAnd (fst happy_var_1)(snd happy_var_1)(snd happy_var_2)(snd happy_var_3))
	)}}}

happyReduce_95 = happySpecReduce_1  22# happyReduction_95
happyReduction_95 happy_x_1
	 =  case happyOut52 happy_x_1 of { (HappyWrap52 happy_var_1) -> 
	happyIn53
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_96 = happySpecReduce_3  23# happyReduction_96
happyReduction_96 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_1 of { (HappyWrap53 happy_var_1) -> 
	case happyOut61 happy_x_2 of { (HappyWrap61 happy_var_2) -> 
	case happyOut54 happy_x_3 of { (HappyWrap54 happy_var_3) -> 
	happyIn54
		 ((fst happy_var_1, Tyche.Abs.EOr (fst happy_var_1)(snd happy_var_1)(snd happy_var_2)(snd happy_var_3))
	)}}}

happyReduce_97 = happySpecReduce_1  23# happyReduction_97
happyReduction_97 happy_x_1
	 =  case happyOut53 happy_x_1 of { (HappyWrap53 happy_var_1) -> 
	happyIn54
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_98 = happySpecReduce_3  24# happyReduction_98
happyReduction_98 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut57 happy_x_2 of { (HappyWrap57 happy_var_2) -> 
	happyIn55
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.EList (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_99 = happySpecReduce_3  24# happyReduction_99
happyReduction_99 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut57 happy_x_2 of { (HappyWrap57 happy_var_2) -> 
	happyIn55
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.EArr (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_100 = happyReduce 6# 24# happyReduction_100
happyReduction_100 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	case happyOut56 happy_x_4 of { (HappyWrap56 happy_var_4) -> 
	case happyOut42 happy_x_6 of { (HappyWrap42 happy_var_6) -> 
	happyIn55
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.EArrSize (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_4)(snd happy_var_6))
	) `HappyStk` happyRest}}}}

happyReduce_101 = happySpecReduce_1  24# happyReduction_101
happyReduction_101 happy_x_1
	 =  case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	happyIn55
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_102 = happyReduce 5# 25# happyReduction_102
happyReduction_102 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	case happyOut55 happy_x_3 of { (HappyWrap55 happy_var_3) -> 
	case happyOut55 happy_x_5 of { (HappyWrap55 happy_var_5) -> 
	happyIn56
		 ((fst happy_var_1, Tyche.Abs.EIf (fst happy_var_1)(snd happy_var_1)(snd happy_var_3)(snd happy_var_5))
	) `HappyStk` happyRest}}}

happyReduce_103 = happyReduce 10# 25# happyReduction_103
happyReduction_103 (happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut42 happy_x_3 of { (HappyWrap42 happy_var_3) -> 
	case happyOut37 happy_x_5 of { (HappyWrap37 happy_var_5) -> 
	case happyOut39 happy_x_9 of { (HappyWrap39 happy_var_9) -> 
	happyIn56
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.ELambda (Just (tokenLineCol happy_var_1)) (snd happy_var_3)(snd happy_var_5)(snd happy_var_9))
	) `HappyStk` happyRest}}}}

happyReduce_104 = happySpecReduce_3  25# happyReduction_104
happyReduction_104 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut55 happy_x_3 of { (HappyWrap55 happy_var_3) -> 
	happyIn56
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.ERand (Just (tokenLineCol happy_var_1)) (snd happy_var_3))
	)}}

happyReduce_105 = happyReduce 5# 25# happyReduction_105
happyReduction_105 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut55 happy_x_3 of { (HappyWrap55 happy_var_3) -> 
	case happyOut55 happy_x_5 of { (HappyWrap55 happy_var_5) -> 
	happyIn56
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.ERandDist (Just (tokenLineCol happy_var_1)) (snd happy_var_3)(snd happy_var_5))
	) `HappyStk` happyRest}}}

happyReduce_106 = happyReduce 10# 25# happyReduction_106
happyReduction_106 (happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut55 happy_x_3 of { (HappyWrap55 happy_var_3) -> 
	case happyOut39 happy_x_7 of { (HappyWrap39 happy_var_7) -> 
	case happyOut55 happy_x_10 of { (HappyWrap55 happy_var_10) -> 
	happyIn56
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.EProbSamp (Just (tokenLineCol happy_var_1)) (snd happy_var_3)(snd happy_var_7)(snd happy_var_10))
	) `HappyStk` happyRest}}}}

happyReduce_107 = happySpecReduce_1  25# happyReduction_107
happyReduction_107 happy_x_1
	 =  case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	happyIn56
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_108 = happySpecReduce_0  26# happyReduction_108
happyReduction_108  =  happyIn57
		 ((Nothing, [])
	)

happyReduce_109 = happySpecReduce_1  26# happyReduction_109
happyReduction_109 happy_x_1
	 =  case happyOut56 happy_x_1 of { (HappyWrap56 happy_var_1) -> 
	happyIn57
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_110 = happySpecReduce_3  26# happyReduction_110
happyReduction_110 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut56 happy_x_1 of { (HappyWrap56 happy_var_1) -> 
	case happyOut57 happy_x_3 of { (HappyWrap57 happy_var_3) -> 
	happyIn57
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_111 = happySpecReduce_1  27# happyReduction_111
happyReduction_111 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn58
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.Plus (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_112 = happySpecReduce_1  27# happyReduction_112
happyReduction_112 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn58
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.Minus (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_113 = happySpecReduce_1  28# happyReduction_113
happyReduction_113 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn59
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.Times (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_114 = happySpecReduce_1  28# happyReduction_114
happyReduction_114 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn59
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.Div (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_115 = happySpecReduce_1  28# happyReduction_115
happyReduction_115 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn59
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.Mod (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_116 = happySpecReduce_1  28# happyReduction_116
happyReduction_116 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn59
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.Mod (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_117 = happySpecReduce_1  29# happyReduction_117
happyReduction_117 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn60
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.LTH (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_118 = happySpecReduce_1  29# happyReduction_118
happyReduction_118 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn60
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.LE (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_119 = happySpecReduce_1  29# happyReduction_119
happyReduction_119 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn60
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.GTH (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_120 = happySpecReduce_1  29# happyReduction_120
happyReduction_120 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn60
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.GE (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_121 = happySpecReduce_1  29# happyReduction_121
happyReduction_121 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn60
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.EQU (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_122 = happySpecReduce_1  29# happyReduction_122
happyReduction_122 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn60
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.EQU (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_123 = happySpecReduce_1  29# happyReduction_123
happyReduction_123 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn60
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.NE (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_124 = happySpecReduce_1  30# happyReduction_124
happyReduction_124 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn61
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.Or (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_125 = happySpecReduce_1  30# happyReduction_125
happyReduction_125 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn61
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.Or (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_126 = happySpecReduce_1  31# happyReduction_126
happyReduction_126 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn62
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.And (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_127 = happySpecReduce_1  31# happyReduction_127
happyReduction_127 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn62
		 ((Just (tokenLineCol happy_var_1), Tyche.Abs.And (Just (tokenLineCol happy_var_1)))
	)}

happyNewToken action sts stk [] =
	happyDoAction 72# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TS _ 30) -> cont 30#;
	PT _ (TS _ 31) -> cont 31#;
	PT _ (TS _ 32) -> cont 32#;
	PT _ (TS _ 33) -> cont 33#;
	PT _ (TS _ 34) -> cont 34#;
	PT _ (TS _ 35) -> cont 35#;
	PT _ (TS _ 36) -> cont 36#;
	PT _ (TS _ 37) -> cont 37#;
	PT _ (TS _ 38) -> cont 38#;
	PT _ (TS _ 39) -> cont 39#;
	PT _ (TS _ 40) -> cont 40#;
	PT _ (TS _ 41) -> cont 41#;
	PT _ (TS _ 42) -> cont 42#;
	PT _ (TS _ 43) -> cont 43#;
	PT _ (TS _ 44) -> cont 44#;
	PT _ (TS _ 45) -> cont 45#;
	PT _ (TS _ 46) -> cont 46#;
	PT _ (TS _ 47) -> cont 47#;
	PT _ (TS _ 48) -> cont 48#;
	PT _ (TS _ 49) -> cont 49#;
	PT _ (TS _ 50) -> cont 50#;
	PT _ (TS _ 51) -> cont 51#;
	PT _ (TS _ 52) -> cont 52#;
	PT _ (TS _ 53) -> cont 53#;
	PT _ (TS _ 54) -> cont 54#;
	PT _ (TS _ 55) -> cont 55#;
	PT _ (TS _ 56) -> cont 56#;
	PT _ (TS _ 57) -> cont 57#;
	PT _ (TS _ 58) -> cont 58#;
	PT _ (TS _ 59) -> cont 59#;
	PT _ (TS _ 60) -> cont 60#;
	PT _ (TS _ 61) -> cont 61#;
	PT _ (TS _ 62) -> cont 62#;
	PT _ (TS _ 63) -> cont 63#;
	PT _ (TS _ 64) -> cont 64#;
	PT _ (TS _ 65) -> cont 65#;
	PT _ (TS _ 66) -> cont 66#;
	PT _ (TS _ 67) -> cont 67#;
	PT _ (TV _) -> cont 68#;
	PT _ (TI _) -> cont 69#;
	PT _ (TL _) -> cont 70#;
	PT _ (TD _) -> cont 71#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 72# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => ([(Token)], [String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pProgram_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {(HappyWrap35 x') = happyOut35 x} in x'))

pArg_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (let {(HappyWrap36 x') = happyOut36 x} in x'))

pListArg_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (let {(HappyWrap37 x') = happyOut37 x} in x'))

pStmt_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (let {(HappyWrap38 x') = happyOut38 x} in x'))

pListStmt_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (let {(HappyWrap39 x') = happyOut39 x} in x'))

pType_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (let {(HappyWrap40 x') = happyOut40 x} in x'))

pArgType_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (let {(HappyWrap41 x') = happyOut41 x} in x'))

pFullType_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (let {(HappyWrap42 x') = happyOut42 x} in x'))

pListArgType_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (let {(HappyWrap43 x') = happyOut43 x} in x'))

pArgMod_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (let {(HappyWrap44 x') = happyOut44 x} in x'))

pTypeMod_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (let {(HappyWrap45 x') = happyOut45 x} in x'))

pListTypeMod_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 11# tks) (\x -> happyReturn (let {(HappyWrap46 x') = happyOut46 x} in x'))

pExpr9_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 12# tks) (\x -> happyReturn (let {(HappyWrap47 x') = happyOut47 x} in x'))

pExpr8_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 13# tks) (\x -> happyReturn (let {(HappyWrap48 x') = happyOut48 x} in x'))

pExpr7_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 14# tks) (\x -> happyReturn (let {(HappyWrap49 x') = happyOut49 x} in x'))

pExpr6_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 15# tks) (\x -> happyReturn (let {(HappyWrap50 x') = happyOut50 x} in x'))

pExpr5_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 16# tks) (\x -> happyReturn (let {(HappyWrap51 x') = happyOut51 x} in x'))

pExpr4_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 17# tks) (\x -> happyReturn (let {(HappyWrap52 x') = happyOut52 x} in x'))

pExpr3_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 18# tks) (\x -> happyReturn (let {(HappyWrap53 x') = happyOut53 x} in x'))

pExpr2_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 19# tks) (\x -> happyReturn (let {(HappyWrap54 x') = happyOut54 x} in x'))

pExpr1_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 20# tks) (\x -> happyReturn (let {(HappyWrap55 x') = happyOut55 x} in x'))

pExpr_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 21# tks) (\x -> happyReturn (let {(HappyWrap56 x') = happyOut56 x} in x'))

pListExpr_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 22# tks) (\x -> happyReturn (let {(HappyWrap57 x') = happyOut57 x} in x'))

pAddOp_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 23# tks) (\x -> happyReturn (let {(HappyWrap58 x') = happyOut58 x} in x'))

pMulOp_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 24# tks) (\x -> happyReturn (let {(HappyWrap59 x') = happyOut59 x} in x'))

pRelOp_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 25# tks) (\x -> happyReturn (let {(HappyWrap60 x') = happyOut60 x} in x'))

pOrOp_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 26# tks) (\x -> happyReturn (let {(HappyWrap61 x') = happyOut61 x} in x'))

pAndOp_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 27# tks) (\x -> happyReturn (let {(HappyWrap62 x') = happyOut62 x} in x'))

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    t:_ -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens

pProgram = (>>= return . snd) . pProgram_internal
pArg = (>>= return . snd) . pArg_internal
pListArg = (>>= return . snd) . pListArg_internal
pStmt = (>>= return . snd) . pStmt_internal
pListStmt = (>>= return . snd) . pListStmt_internal
pType = (>>= return . snd) . pType_internal
pArgType = (>>= return . snd) . pArgType_internal
pFullType = (>>= return . snd) . pFullType_internal
pListArgType = (>>= return . snd) . pListArgType_internal
pArgMod = (>>= return . snd) . pArgMod_internal
pTypeMod = (>>= return . snd) . pTypeMod_internal
pListTypeMod = (>>= return . snd) . pListTypeMod_internal
pExpr9 = (>>= return . snd) . pExpr9_internal
pExpr8 = (>>= return . snd) . pExpr8_internal
pExpr7 = (>>= return . snd) . pExpr7_internal
pExpr6 = (>>= return . snd) . pExpr6_internal
pExpr5 = (>>= return . snd) . pExpr5_internal
pExpr4 = (>>= return . snd) . pExpr4_internal
pExpr3 = (>>= return . snd) . pExpr3_internal
pExpr2 = (>>= return . snd) . pExpr2_internal
pExpr1 = (>>= return . snd) . pExpr1_internal
pExpr = (>>= return . snd) . pExpr_internal
pListExpr = (>>= return . snd) . pListExpr_internal
pAddOp = (>>= return . snd) . pAddOp_internal
pMulOp = (>>= return . snd) . pMulOp_internal
pRelOp = (>>= return . snd) . pRelOp_internal
pOrOp = (>>= return . snd) . pOrOp_internal
pAndOp = (>>= return . snd) . pAndOp_internal
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 10 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}















{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc8371_0/ghc_2.h" #-}
































































































































































































{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}


          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}


                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+#  i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 180 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+#  nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+#  nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
