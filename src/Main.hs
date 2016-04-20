module Main(main) where

import Printype.Lambda
import Printype.Lambda.Combinators
import Printype.Principal
import Printype.Types
import qualified Printype.Latex as L

pretty_principal_type_deduction :: Term -> IO ()
pretty_principal_type_deduction t = 
  let (result, log) = principal_type_deduction_log t in
    case result of
	    Right d ->
	      do
	        putStrLn $ ""
	        mapM_ putStrLn $ pretty_deduction d
	        putStrLn $ ""
	        putStrLn $ "=== LOG ==="
	        mapM_ putStrLn $ log
	    Left err ->
	      do
	        putStrLn $ ""
	        putStrLn $ "The term " ++ show t ++ " is not typbable!"
	        putStrLn $ show err
	        putStrLn $ ""

pretty_deduction :: Deduction -> [String]
pretty_deduction d =
	let conclusion = show (d_context d) ++ "  |-->  " ++ show (d_subject d) ++ ":" ++ show (d_type d) in
	case d_premise d of
		Axiom -> [conclusion]
		Unary d1 -> [conclusion] ++ mapp f3 f1 f5 ("":(pretty_deduction d1))
		Binary d1 d2 -> [conclusion] ++ mapp f4 f2 f4 ("":(pretty_deduction d1)) ++ mapp f4 f2 f5 ("":(pretty_deduction d2))
    where f1 = (\s -> " +--> " ++ s)
          f2 = (\s -> " +o-> " ++ s)
          f3 = (\s -> " :    " ++ s)
          f4 = (\s -> " |    " ++ s)
          f5 = (\s -> "      " ++ s)
          mapp f g h (x:y:xs) = f x : g y : map h xs
          mapp f g h [x] = [f x]

--main :: IO ()
--main = mapM_ pretty_principal_type_deduction [
--                 identity,
--                 sCombinator,
--                 --appL' "thequickbrownfox",
--                 twicetwicetwice,
--                 numeral 0,
--                 appL' "xxx",
--                 true,
--                 andCombinator]

main :: IO ()
main = pretty_principal_type_deduction sCombinator

--main :: IO ()
--main = L.render_term_deductions [ identity,
--					              sCombinator,
--					              numeral 2,
--					              selfapply,
--					              numeral 0,
--					              appL' "xxx",
--					              true,
--					              andCombinator ]