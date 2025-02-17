{-
    BNF Converter: Generate main/test module for OCaml
    Copyright (C) 2005  Author:  Kristofer Johannisson

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

module BNFC.Backend.OCaml.CFtoOCamlTest where

import Prelude hiding((<>))
import Text.PrettyPrint

import BNFC.CF
import BNFC.Backend.OCaml.OCamlUtil

-- | OCaml comment
-- >>> comment "I'm a comment"
-- (* I'm a comment *)
comment :: Doc -> Doc
comment d = "(*" <+> d <+> "*)"

-- | OCaml String concatenation
-- >>> "print a" <^> doubleQuotes "abc"
-- print a ^ "abc"
a <^> b = a <+> "^" <+> b

-- | Generate a test program in OCaml
ocamlTestfile :: String -> String -> String -> String -> String -> CF -> Doc
ocamlTestfile absM lexM parM printM showM cf =
    let
        lexerName = text lexM <> ".token"
        parserName = text parM <> ".p" <> topTypeC
        printerName =
            text printM <> ".printTree " <> text printM <> ".prt" <> topTypeC
        showFun =
            parens ("fun x ->" <+> text showM <> ".show"
                    <+> parens (text showM <> ".show" <> topTypeC <+> "x"))
        topTypeC = text $ fixTypeUpper (firstEntry cf)
        topType = text absM <> "." <> text (fixType (firstEntry cf))
    in vcat
        [ comment "automatically generated by the BNF Converter"
        , ""
        , "open Lexing"
        , ""
        , "let parse (c : in_channel) :" <+> topType <+> "="
        , nest 4 (parserName <+> lexerName <+> "(Lexing.from_channel c)")
        , ";;"
        , ""
        , "let showTree (t : " <> topType <> ") : string ="
        , nest 4 (fsep ( punctuate "^"
            [ doubleQuotes "[Abstract syntax]\\n\\n"
            , showFun <+> "t"
            , doubleQuotes "\\n\\n"
            , doubleQuotes "[Linearized tree]\\n\\n"
            , printerName <+> "t"
            , doubleQuotes "\\n" ] ) )
        , ";;"
        , ""
        , "let main () ="
        , nest 4 $ vcat
            [ "let channel ="
            , nest 4 $ vcat
                [ "if Array.length Sys.argv > 1 then open_in Sys.argv.(1)"
                , "else stdin" ]
            , "in"
            , "try"
            , nest 4 $ vcat
                [ "print_string (showTree (parse channel));"
                , "flush stdout;"
                , "exit 0"]
            , "with BNFC_Util.Parse_error (start_pos, end_pos) ->"
            , nest 4 $ vcat
                [ "Printf.printf \"Parse error at %d.%d-%d.%d\\n\""
                , nest 4 $ vcat
                    [ "start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol)"
                    , "end_pos.pos_lnum (end_pos.pos_cnum - end_pos.pos_bol);" ]
                , "exit 1" ]]
        , ";;"
        , ""
        , "main ();;" ]
