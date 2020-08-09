{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving        #-}
module Lib where
import           Control.Monad   (forM, forM_)
import           Data.List       (intercalate)
import           Data.Map.Strict (Map, fromList, toList)
someFunc = undefined

data Quantity = Gram Int | Unit Int | TableSpool Int | Some
  deriving (Show)

instance Semigroup Quantity where
  a <> b = Some

instance Monoid Quantity where
  mempty = Gram 0

data Ingredient = Ingredient { unIngredient :: String }
  deriving (Show, Eq, Ord)

type Measured = (Ingredient, Quantity)
type SumIngredient = Map Ingredient Quantity

type Method = String
data Recipe a =
  Empty a
  | Raw Measured a
  | Raws [Measured] a
  | Cook Method (Recipe a)
  | forall b. Seq (Recipe b) (Recipe a)
-- deriving Show
--deriving instance (Show a, Show b) => Show (Recipe a)

toTextMeasured (x, q) = unIngredient x <> "[" <> show q <> "]"

toText :: Recipe a -> String
toText (Empty _)   = ""
toText (Raw x _)   = toTextMeasured x
toText (Raws xs _) = intercalate "," $ map toTextMeasured xs
toText (Cook m x)  = m <> "(" <> toText x <> ")"
toText (Seq x y)   = toText x <> "\n" <> toText y

recipeValue :: Recipe a -> a
recipeValue (Empty x)  = x
recipeValue (Raw _ x)  = x
recipeValue (Raws _ x) = x
recipeValue (Cook _ x) = recipeValue x
recipeValue (Seq _ x)  = recipeValue x


recipeSum :: Recipe a -> SumIngredient
recipeSum (Empty _)   = mempty
recipeSum (Raw x _)   = fromList [x]
recipeSum (Raws xs _) = fromList xs
recipeSum (Cook _ x)  = recipeSum x
recipeSum (Seq x y)   = recipeSum x <> recipeSum y

printRecipeSum :: SumIngredient -> IO ()
printRecipeSum m = forM_ (toList m) $ \(k,v) -> do
  putStrLn $ unIngredient k <> ": " <> show v

-- TODO: モナド則とか確認

instance Functor Recipe where
  fmap f x  = Seq x (Empty $ f (recipeValue x))

-- TODO: ApplicativeはSeqではなくしたい
instance Applicative Recipe where
  pure = Empty
  a <*> b = Seq (Seq a b) (Empty $ recipeValue a (recipeValue b))
  (*>) = Seq

instance Monad Recipe where
  return = Empty
  (>>) = Seq
  x >>= f = Seq x (f (recipeValue x))

instance Foldable Recipe where
  foldMap f m = f $ recipeValue m

raw :: String -> Quantity -> Recipe Ingredient
raw x q = Raw (Ingredient x, q) (Ingredient x)

cook :: Method -> Measured -> Recipe Measured
cook m (x, q) = Cook m (Raw (x, q) cooked)
  where cooked = (Ingredient $ "cooked" <> (unIngredient x), Unit 1)

-- TODO: よくある、手順の成果物を「1を〜する」と使うやつやりたい
cooks :: Method -> [Measured] -> Recipe Measured
cooks m xs = Cook m (Raws xs cooked)
  where cooked = (Ingredient $ "cooked [unknown]", Unit 1)

r1 = Cook "boil" (Raw (Ingredient "Egg", Gram 50) ())

r2 = do
  cc <- cook "一口大に切る" $ (Ingredient "鶏もも肉", Gram 150)
  m <- cooks "ジップロックに入れて揉み込む" [ (Ingredient "ケチャップ", TableSpool 3)
                                       , (Ingredient "砂糖", TableSpool 1)
                                       , (Ingredient "みりん", TableSpool 1)
                                       , (Ingredient "醤油", TableSpool 1)
                                       , cc]
  cook "フライパンで焼く" m
  -- issue: 直前のステートメントとマージしたい
  -- issue: Methodに材料がでてくるパターンンを解決したい
  --        特に、複数の材料から選択されケース
  cook "フライパンに盛り付け、パセリをちらす" (Ingredient "パセル", Some)

r3 = do
  cook "茹でる" (Ingredient "卵", Unit 1)

r4 = do
  Cook "蒸す" r3

r5 = do
  let x = cook "茹でる" (Ingredient "卵", Unit 1)
  let y = cook "焼く" (Ingredient "卵", Unit 1)
  Cook "盛り付ける" x

r6 = do
  cook "焼く" (Ingredient "魚", Unit 1)
  cook "焼く" (Ingredient "肉", Unit 1)



{-
案1
Cook Method [Measured]に変更
　recipeがRecipe Measuredを返すように使う
　　Rawは廃止
      素材を用意するだけのステップを想定してたが要らない気がする
　　例えばcook a xsはaされたxs１つを返す
　　Seq a bはbをしてからaをした際のaを返す
　　　aが使われてなければ関係ない（「手順２: 庭に塩[1kg]を巻く」とか）
-}
