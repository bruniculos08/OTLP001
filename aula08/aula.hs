-- Funtor é relacionado a teoria das categorias (e a ideia é que ele leva de uma...
-- categoria a outra).

-- A função map é um um funtor pois mapeia uma lista de tipo X em uma lista de tipo Y,...
-- ... ou melhor falando, e transforma lista em um funtor pois permite um mapeamento...
-- ... para outro tipo ((?) não sei se entendi direito o que foi falado).

import Prelude hiding (Functor, fmap, (<$>))

-- Definição da classe Funtor:
class Functor f where
    fmap :: (a -> b) -> f a -> f b
    (<$>) :: (a -> b) -> f a -> f b
    (<$>) = fmap

-- se coloca '<$>' entre parentes pois é (para que seja) infixo.

-- Diagrama de teoria das categorias:
--          A ---- f -----> B
--          \               |
--            \             |
--              \           |
--                \         | g
--            f.g   \       |
--                    \     |
--                      \   |
--                        V V
--                          C

instance Functor [] where
    fmap = map

data Arvore a = No a (Arvore a) (Arvore a) | Folha

instance Functor Arvore where
    fmap f Folha = Folha
    fmap f (No a tl tr) = No (f a) (fmap f tl) (fmap f tr)

instance Functor Maybe where
    fmap f Nothing  = Nothing
    fmap f (Just x) = Just (f x)

-- É possível usar o derive ao invés de instance (... myData deriving Functor).
-- <$> é a forma infixa do fmap, assim pode se fazer por exemplo:
-- par <$> arvore

-- Tarefa: testar hlint (comando: hlint nome_do_programa.hs) no código da última...
-- ... tarefa entregue.
