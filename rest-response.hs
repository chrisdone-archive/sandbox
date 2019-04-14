{-

> :t (header "X-Header" % body)
(header "X-Header" % body) :: Response r2 (String -> String -> r2)

> meta (header "X-Header" % body)
MappendM [HeaderM "X-Header",BodyM]

> run (header "X-Header" % body) "Header Value" "Hello, Body!"
Mappend [Header "X-Header" "Header Value",Body "Hello, Body!"]

-}

module RestResponse
  ( Output(..)
  , Meta(..)
  , (%)
  , header
  , body
  , meta
  , run
  ) where

-- A response generator.
data Response r a = Response { meta :: Meta, runHM :: (Output -> r) -> a }

data Output = Body String | Header String String | Mappend [Output] deriving (Show)

instance Semigroup Output where x <> y = Mappend [x, y]

data Meta = BodyM | HeaderM String | MappendM [Meta] deriving (Show)

instance Semigroup Meta where x <> y = MappendM [x,y]

-- | Combine two responses.
(%) :: Response r1 a -> Response r2 r1 -> Response r2 a
r1 % r2 =
  Response (meta r1 <> meta r2) (\k -> runHM r1 (\output -> runHM r2 (\output2 -> k (output <> output2))))
infixr 9 %

-- | Run the response on arguments.
run :: Response Output a -> a
run m = runHM m id

-- | Declare a header.
header :: String -> Response r (String -> r)
header key = later (HeaderM key) (Header key)

-- | Declare body output.
body :: Response r (String -> r)
body = later BodyM Body

-- Do something later.
later :: Meta -> (a -> Output) -> Response r (a -> r)
later m f = Response m (. f)
