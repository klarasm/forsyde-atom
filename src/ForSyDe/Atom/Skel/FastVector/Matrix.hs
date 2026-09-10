module ForSyDe.Atom.Skel.FastVector.Matrix where

import qualified GHC.List as L (take, drop)
import Prelude hiding (take, drop)
import qualified Data.List as L
import ForSyDe.Atom.Skel.FastVector (Vector(..), vector, fromVector, (<++>))
import qualified ForSyDe.Atom.Skel.FastVector as V
import qualified Data.Sequence as Seq

-- | 'Matrix' is simply a type synonym for vector of vectors. This
-- means that /any/ function on 'Vector' works also on 'Matrix'.
type Matrix a = Vector (Vector a)

-- | Prints out to the terminal a matrix in a readable format, where
-- all elements are right-aligned and separated by a custom separator.
--
-- >>> let m = matrix 3 3 [1,2,3,3,100,4,12,32,67]
-- >>> pretty "|" m
--  1|  2| 3
--  3|100| 4
-- 12| 32|67
pretty :: Show a
       => String   -- ^ separator string
       -> Matrix a -- ^ input matrix
       -> IO ()
pretty sep mat = mapM_ putStrLn $ fromVector $ printMat maxWdt strMat
  where
    maxWdt = V.reduce (V.farm21 max) $ farm11 length strMat
    strMat = farm11 show mat
    printMat w  = V.farm11 (printRow w)
    printRow w  = L.intercalate sep . fromVector . V.farm21 align w
    align n str = replicate (n - length str) ' ' ++ str

-- | See 'ForSyDe.Atom.Skel.Vector.Matrix.isNull'.
isNull :: Matrix a -> Bool
isNull (Vector Seq.Empty) = True
isNull (Vector (Vector Seq.Empty Seq.:<| Seq.Empty)) = True
isNull _ = False

-- | See 'ForSyDe.Atom.Skel.Vector.Matrix.size'.
size :: Matrix a -> (Int,Int)
size m = (x,y)
  where
    y = V.length m
    x = (V.length . V.first) (m)

-- | See 'ForSyDe.Atom.Skel.Vector.Matrix.wellFormed'.
wellFormed :: Matrix a -> Matrix a
wellFormed (Vector Seq.Empty) = Vector Seq.Empty
wellFormed m@(Vector (_ Seq.:<| Seq.Empty)) = m
wellFormed m@(Vector (x Seq.:<| xs))
  | all (\r -> V.length r == V.length x) xs = m
  | otherwise = error "matrix ill-formed: rows are of unequal lengths"

groupEvery :: Int -> [a] -> [[a]]
groupEvery _ [] = []
groupEvery n l
  | n < 0        = error $ "cannot group list by negative n: " ++ show n
  | length l < n = error "input list cannot be split into all-equal parts"
  | otherwise    = L.take n l : groupEvery n (L.drop n l)

-- | See 'ForSyDe.Atom.Skel.Vector.Matrix.Matrix'.
matrix :: Int      -- ^ number of columns (X dimension) @= x@
       -> Int      -- ^ number of rows (Y dimension) @= y@
       -> [a]      -- ^ list of values; /length/ = @x * y@
       -> Matrix a -- ^ 'Matrix' of values; /size/ = @(x,y)@
matrix x y = vector . map vector . groupEvery x . check
  where
    check l | length l == x * y = l
            | otherwise
      = error $ "cannot form matrix (" ++ show x ++ ","
              ++ show y ++ ") from a list with "
              ++ show (length l) ++ " elements"

-- | See 'ForSyDe.Atom.Skel.Vector.Matrix.fromMatrix'.
fromMatrix :: Matrix a -- ^ /size/ = @(x,y)@
           -> [a]      -- ^ /length/ = @x * y@
fromMatrix = concatMap fromVector . fromVector

-- | See 'ForSyDe.Atom.Skel.Vector.Matrix.fanout'.
fanoutn :: Int -> a -> Matrix a
fanoutn n v = V.fanoutn n $ V.fanoutn n v

fanout = fanoutn (maxBound :: Int)

-- | See 'ForSyDe.Atom.Skel.Vector.Matrix.indexes'.
-- indexes :: Matrix (Int, Int)
-- indexes = farm21 (,) colix rowix
--   where
--     colix = vector $ repeat $ vector [0..]
--     rowix = transpose colix

-- | See 'ForSyDe.Atom.Skel.Vector.Matrix.farm11'.
farm11 :: (a -> b)
       -> Matrix a -- ^ /size/ = @(xa,ya)@
       -> Matrix b -- ^ /size/ = @(xa,ya)@
farm11 = V.farm11 . V.farm11

-- | See 'ForSyDe.Atom.Skel.Vector.Matrix.farm21'.
farm21 :: (a -> b -> c)
           -> Matrix a -- ^ /size/ = @(xa,ya)@
           -> Matrix b -- ^ /size/ = @(xb,yb)@
           -> Matrix c -- ^ /size/ = @(minimum [xa,xb], minimum [ya,yb])@
farm21 f = V.farm21 (V.farm21 f)

-- | See 'ForSyDe.Atom.Skel.Vector.Matrix.farm31'.
farm31 :: (a -> b -> c -> d)
            -> Matrix a -- ^ /size/ = @(xa,ya)@
            -> Matrix b -- ^ /size/ = @(xb,yb)@
            -> Matrix c -- ^ /size/ = @(xc,yc)@
            -> Matrix d -- ^ /size/ = @(minimum [xa,xb,xc], minimum [ya,yb,yc])@
farm31 f = V.farm31 (V.farm31 f)

-- | See 'ForSyDe.Atom.Skel.Vector.Matrix.reduce'.
reduce :: (a -> a -> a) -> Matrix a -> a
reduce f = V.reduce f . V.farm11 (V.reduce f)

-- | See 'ForSyDe.Atom.Skel.Vector.Matrix.fotV'.
dotV :: (a -> a -> a)
     -- ^ kernel function for a row/column reduction, e.g. @(+)@ for dot product
     -> (b -> a -> a)
     -- ^ binary operation for pair-wise elements, e.g. @(*)@ for dot product
     -> Matrix b      -- ^ /size/ = @(xa,ya)@
     -> Vector a      -- ^ /length/ = @xa@
     -> Vector a      -- ^ /length/ = @xa@
dotV f g mA y = V.farm11 (\x -> V.reduce f $ V.farm21 g x y) mA

-- | See 'ForSyDe.Atom.Skel.Vector.Matrix.dot'.
dot :: (a -> a -> a) -- ^ kernel function for a row/column reduction, e.g. @(+)@ for dot product
    -> (b -> a -> a) -- ^ binary operation for pair-wise elements, e.g. @(*)@ for dot product
    -> Matrix b      -- ^ /size/ = @(xa,ya)@
    -> Matrix a      -- ^ /size/ = @(ya,xa)@
    -> Matrix a      -- ^ /size/ = @(xa,xa)@
dot f g m = V.farm11 (dotV f g m) . transpose

-- | See 'ForSyDe.Atom.Skel.Vector.Matrix.get'.
get :: Int       -- ^ X index starting from zero
    -> Int       -- ^ Y index starting from zero
    -> Matrix a
    -> Maybe a
get x y mat = getMaybe (V.get y mat)
  where getMaybe Nothing = Nothing
        getMaybe (Just a) = V.get x a

-- | See 'ForSyDe.Atom.Skel.Vector.Matrix.take'.
take :: Int       -- ^ X index starting from zero
     -> Int       -- ^ Y index starting from zero
     -> Matrix a
     -> Matrix a
take x y = V.farm11 (V.take x) . V.take y

-- | See 'ForSyDe.Atom.Skel.Vector.Matrix.drop'.
drop :: Int       -- ^ X index starting from zero
     -> Int       -- ^ Y index starting from zero
     -> Matrix a
     -> Matrix a
drop x y = V.farm11 (V.drop x) . V.drop y

-- | See 'ForSyDe.Atom.Skel.Vector.Matrix.crop'.
crop :: Int      -- ^ crop width  = @w@
     -> Int      -- ^ crop height = @h@
     -> Int      -- ^ X start position = @x0@
     -> Int      -- ^ Y start position = @y0@
     -> Matrix a -- ^ /size/ = @(xa,ya)@
     -> Matrix a -- ^ /size/ = @(minimum [w,xa-x0], minimum [h,xa-x0])@
crop w h pX pY = take w h . drop pX pY

-- cropMat w h pX pY = V.farm11 (crop w pX) . crop h pY
--   where crop size pos = V.drop pos . V.take (pos + size) 

-- | See 'ForSyDe.Atom.Skel.Vector.Matrix.group'.
group :: Int      -- ^ width of groups = @w@
      -> Int      -- ^ height of groups = @h@
      -> Matrix a -- ^ /size/ = @(xa,ya)@
      -> Matrix (Matrix a) -- ^ /size/ = @(xa `div` w,ya `div` h)@
group w h = V.farm11 transpose . V.group h . V.farm11 (V.group w)


-- | See 'ForSyDe.Atom.Skel.Vector.Matrix.stencil'.
stencil :: Int -> Int -> Matrix a -> Matrix (Matrix a)
stencil r c = arrange . groupCols . groupRows
  where
    groupRows =         V.farm11 (V.take r) . dropFromEnd r . V.tails
    groupCols = farm11 (V.farm11 (V.take c) . dropFromEnd c . V.tails)
    arrange   = V.farm11 transpose
    dropFromEnd n v = V.take (V.length v - n + 1) v

-- | Pad the matrix with r row elements and c column elements of e
--
-- >>> padConst 0 2 1 $ matrix 2 2 [1,2,3,4]
-- <<0,0,0,0,0,0>,<0,0,1,2,0,0>,<0,0,3,4,0,0>,<0,0,0,0,0,0>>
padConst :: a -> Int -> Int -> Vector (Vector a) -> Vector (Vector a)
padConst e r c m = colPadding <++> middle <++> colPadding
  where
    s = 2 * r + (V.length $ V.first m :: Int)
    middle = V.farm11 padRow m
    padRow row = rowPadding <++> row <++> rowPadding
    rowPadding = V.fanoutn r e
    colPadding = V.fanoutn c $ V.fanoutn s e

-- | Pad the matrix with r row elements and c column elements of the closest neighbor
--
-- >>> padDup 2 1 $ matrix 2 2 [1,2,3,4]
-- <<1,1,1,2,2,2>,<1,1,1,2,2,2>,<3,3,3,4,4,4>,<3,3,3,4,4,4>>
padDup :: Int -> Int -> Vector (Vector a) -> Vector (Vector a)
padDup r c m = top <++> middle <++> bottom
  where
    top = V.fanoutn c $ V.first middle
    bottom = V.fanoutn c $ V.last middle
    middle = V.farm11 padRow m
    padRow row = left <++> row <++> right
      where left = V.fanoutn r $ V.first row
            right = V.fanoutn r $ V.last row

-- | Pad the matrix with r row elements and c column elements cyclically from the opposite boundary
--
-- >>> padCycl 2 1 $ matrix 2 2 [1,2,3,4]
-- <<3,4,3,4,3,4>,<1,2,1,2,1,2>,<3,4,3,4,3,4>,<1,2,1,2,1,2>>
padCycl :: Int -> Int -> Vector (Vector a) -> Vector (Vector a)
padCycl r c m = top <++> middle <++> bottom
  where
    (w, h) = size m
    top = V.take c $ V.drop (h - c) middle
    bottom = V.take c middle
    middle = V.farm11 padRow m
    padRow row = left <++> row <++> right
      where left = V.take r $ V.drop (w - r) row
            right = V.take r row

-- | See 'ForSyDe.Atom.Skel.Vector.Matrix.reverse'.
reverse :: Matrix a -> Matrix a
reverse = V.reverse . V.farm11 V.reverse

-- | See 'ForSyDe.Atom.Skel.Vector.Matrix.transpose'.
transpose :: Matrix a     -- ^ @X:Y@ orientation
          -> Matrix a     -- ^ @Y:X@ orientation
transpose =  vector . map vector . L.transpose . map fromVector . fromVector


-- | See 'ForSyDe.Atom.Skel.Vector.Matrix.replace'.
replace :: Int -> Int -> Matrix a -> Matrix a -> Matrix a
replace x y mask = replace y h (V.farm21 (\m o -> replace x w (const m) o) mask)
  where
    (w,h) = size mask
    replace start size replaceF vec
      = let begin  = V.take start vec
            middle = replaceF $ V.drop start $ V.take (start + size) vec
            end    = V.drop (start + size) vec
        in begin <++> middle <++> end

