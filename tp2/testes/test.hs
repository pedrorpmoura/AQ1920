import Cp
import List
import Nat


eta :: x -> [([a], (Integer, x))]
eta = singl . split nil (split zero id)


kleislin :: (x -> [([a], (Integer, y))]) -> [([a], (Integer, x))] -> [([a], (Integer, y))]
kleislin f = concatMap (\(l, (t, x)) -> map ((l ++) >< ((+ t) >< id)) (f x))

ex :: x -> [([String], (Integer, x))]
ex x = [(["Ola"], (2,x))]