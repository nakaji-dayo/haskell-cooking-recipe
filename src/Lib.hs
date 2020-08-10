{-# LANGUAGE ApplicativeDo             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE StandaloneDeriving        #-}
module Lib where
import qualified Clay                 as C
import           Control.Monad        (forM, forM_, join)
import qualified Data.ByteString.Lazy as BS
import           Data.List            (intercalate)
import           Data.Map.Strict      (Map, fromList, toList)
import           Data.Maybe           (catMaybes)
import           Lucid

someFunc = undefined

-- todo: module Style
defaultStyle =
  C.td C.? C.border C.solid (C.px 1) (C.rgb 0 0 0)

data Quantity = Gram Int | Unit Int | TableSpool Int | Litre Double | Some

instance Show Quantity where
  show (Gram x) = show x <> " g"
  show (Unit x) = show x <> " 個"
  show (TableSpool x) = "大さじ " <> show x
  show (Litre x) = show x <> " l"
  show Some = "適量"

instance Semigroup Quantity where
  a <> b = Some

instance Monoid Quantity where
  mempty = Gram 0

data Ingredient = Ingredient { unIngredient :: String }
  deriving (Show, Eq, Ord)

type Measured = (Ingredient, Maybe Quantity)
type IngredientTable = Map Ingredient Quantity

-- memo: レシピの記述としては自由記述は必要なはず
--   「煮る」だけにしても材料を入れるタイミングや、具体的なアドバイスとか
--     本当に?
--   構造化(-> 記述が楽?, i18n)
--     method内で材料を参照?
-- memo: 材料の参照
--   - 自然文で書く->冗長・ずれるかも
--     - TH用意?, MeasuredがIsStringになってれば十分に簡素かも?
--     - "フライパンをよく熱し、油をたっぷり敷いて、鶏肉200gを皮を下にして焼く、3分後ひっくり返しもう3分焼く"
--     -   |機材が登場                |分量に現れない詳細   |{i}を{m}する   | 時間が登場(合計とか)(並列実行も)
--   - "$0を"みたいな参照は入れるがランタイムで解決->用途的にはランタイムでコケて問題ない
--   - templateかつ$0みたいのまで解決(名前で参照したり、参照番号振ったり)
type Method = String
data Recipe a =
  Empty a
  | Cook Method [Measured] a
  | forall b. Seq (Recipe b) (Recipe a)
  | forall b. Par (Recipe b) (Recipe a)
-- deriving Show
--deriving instance (Show a, Show b) => Show (Recipe a)

toTextMeasured :: ((Ingredient, Maybe Quantity) -> String)
toTextMeasured (x, Nothing) = unIngredient x
toTextMeasured (x, Just q)  = unIngredient x <> "  " <> show q

toText :: Recipe a -> (String, a)
toText x = go 0 x
  where
    go :: Int -> Recipe a -> (String, a)
    go h (Empty r)   = ("{- Finished -}", r)
    go h (Cook m xs r) = (("Thread" <> show h <> ": ") <> m <> "(" <> intercalate ", " (fmap toTextMeasured xs) <> ")", r)
    go h (Seq x y)   =
      (fst (go h x) <> "\n" <> s, r)
      where (s, r) = go h y
    go h (Par x y)   =
      (fst (go h x) <> "\t\t" <> s, a)
      where (s, a) = go (h+1) y

testHtml x = BS.writeFile "./tmp.html" $ renderBS $ h
  where h = do
          doctype_
          html_ $ do
            head_ $ do
            style_ [type_ "text/css"] $ C.render defaultStyle
          body_ $ runHtml x

runHtml :: Recipe Measured -> Html ()
runHtml x = do
  let (dir, arc) = go 0 x
  h1_ . toHtml . toTextMeasured $ arc
  table_
    dir
  where
    go :: Int -> Recipe a -> (Html (), a)
    go h (Empty r)   = (mempty, r)
    go h (Cook m xs r) = (
      do
        td_ $
          ol_ $
            forM_ xs $ li_ . toHtml . toTextMeasured
        td_ $
          p_ . toHtml $ m
      , r)
    go h (Seq x y)   =
      (fst (go h x) >> tr_ s, r)
      where (s, r) = go h y
    go h (Par x y)   =
      (fst (go h x) >>  s, a)
      where (s, a) = go (h+1) y

recipeValue :: Recipe a -> a
recipeValue (Empty x)    = x
recipeValue (Cook _ _ x) = x
recipeValue (Seq _ x)    = recipeValue x
recipeValue (Par _ x)    = recipeValue x

recipeSum :: Recipe a -> IngredientTable
recipeSum (Empty _)     = mempty
recipeSum (Cook _ xs _) = fromList [(k,v) | (k, Just v) <- xs]
recipeSum (Seq x y)     = recipeSum x <> recipeSum y

printRecipeSum :: IngredientTable -> IO ()
printRecipeSum m = forM_ (toList m) $ \(k,v) -> do
  putStrLn $ unIngredient k <> ": " <> show v

printRecipe x = do
  let (step, artifact) = toText x
  putStrLn step
  putStrLn $ toTextMeasured artifact

instance Functor Recipe where
  fmap f x  = Par x (Empty $ f (recipeValue x))

instance Applicative Recipe where
  pure = Empty
  f <*> y = Par (Par f y) (Empty $ recipeValue f (recipeValue y))
  (*>) = Seq

instance Monad Recipe where
  return = Empty
  (>>) = Seq
  x >>= f = Seq x (f (recipeValue x))

instance Foldable Recipe where
  foldMap f m = f $ recipeValue m

cooks :: Method -> [Measured] -> Recipe Measured
cooks m xs = Cook m xs cooked
  where cooked = (Ingredient $ "調理済(" <> overview <> ")", Nothing)
        overview = intercalate "," $ unIngredient . fst <$> xs

cook m x = cooks m [x]

cook_ m x y = cooks m [x, y]

archive x = Empty (Ingredient x, Nothing)

r1 = cook "method1" (Ingredient "x", Just $ Gram 5)
  *> cook "method2" (Ingredient "y", Just $ Gram 10)

r2 = do
  cc <- cook "一口大に切る" $ (Ingredient "鶏もも肉", Just $ Gram 150)
  m <- cooks "ジップロックに入れて揉み込む" [ (Ingredient "ケチャップ", Just $ TableSpool 3)
                                       , (Ingredient "砂糖", Just $ TableSpool 1)
                                       , (Ingredient "みりん", Just $ TableSpool 1)
                                       , (Ingredient "醤油", Just $ TableSpool 1)
                                       , cc]
  cook "フライパンで焼く" m
  -- issue: Methodに材料がでてくるパターンを解決したい
  --        特に、複数の材料から選択されケース
    >>= cook_ "フライパンに盛り付け、パセリをちらす" (Ingredient "パセリ", Just Some)
  archive "揉むだけ簡単！やみつきチキン"

r3 = do
  cook "茹でる" (Ingredient "卵", Just $ Unit 1)

r4 = do
  cook "揚げる" =<< r3

r5 = do
  x <- cook "焼く" (Ingredient "肉", Just $ Unit 1)
  y <- cook "蒸す" x
  cooks "盛り付ける" [y, (Ingredient "トマト", Just $ Unit 6)]

r6 = do
  a <- cook "蒸す" (Ingredient "魚", Just $ Unit 1)
  b <- cook "焼く" (Ingredient "肉", Just $ Unit 1)
  cooks "盛り付ける" [a,b, (Ingredient "トマト", Just $ Unit 6)]

r6' =
  (\a -> cooks "盛り付ける" [a, (Ingredient "トマト", Just $ Unit 6)])
  <$> cook "蒸す" (Ingredient "魚", Just $ Unit 1)

r6'' =
  join $ (\a b -> cooks "盛り付ける" [a, b, (Ingredient "トマト", Just $ Unit 6)])
  <$> cook "蒸す" (Ingredient "魚", Just $ Unit 1)
  <*> cook "焼く" (Ingredient "肉", Just $ Unit 1)

{-
成果物
  - 材料になる
  - 名前付ける
  - 何人前か、保存期間
TODO: よくある、手順の成果物を「1を〜する」と使うやつやりたい
-}
