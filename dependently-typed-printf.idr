module Printf

%default total

-- Formatting AST.
data Format
  = FInt Format
  | FString Format
  | FOther Char Format
  | FEnd

-- Parse the format string (list of characters) into an AST.
-- Example: "%d,%s"  →   (FInt (FOther ',' (FString FEnd)))
format : List Char -> Format
format ('%' :: 'd' :: cs ) = FInt ( format cs )
format ('%' :: 's' :: cs ) = FString ( format cs )
format ( c :: cs )         = FOther c ( format cs )
format []                  = FEnd

-- Convenience function to unpack a string into a list of chars, then
-- run format on it.
formatString : String -> Format
formatString s = format ( unpack s )

-- Convert a format AST into a type.
-- Example: FInt (FOther ',' (FString FEnd))   →   Int -> String -> String
interpFormat : Format -> Type
interpFormat (FInt f)     = Int -> interpFormat f
interpFormat (FString f)  = String -> interpFormat f
interpFormat (FOther _ f) = interpFormat f
interpFormat FEnd         = String

-- Dependently-typed part: turn a formatting AST into a well-typed
-- function accepting n arguments.
-- Example:
--      toFunction (FInt (FString FEnd))
--    →
--      \a i s => a ++ (show i) ++ s
toFunction : (fmt : Format) -> String -> interpFormat fmt
toFunction ( FInt f ) a     = \i => toFunction f ( a ++ show i )
toFunction ( FString f ) a  = \s => toFunction f ( a ++ s )
toFunction ( FOther c f ) a = toFunction f ( a ++ singleton c )
toFunction FEnd a           = a

-- Dependently-typed part: turn a formatting string into a well-typed
-- function accepting n arguments.
-- Example: printf "%d%s" → \i s => (show i) ++ s
printf : (s : String) -> interpFormat ( formatString s )
printf s = toFunction ( formatString s ) ""
