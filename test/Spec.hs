import Set01Spec
import Set02Spec
import Set03Spec
import Set04Spec
import Set05Spec

main :: IO ()
main = do
  putStrLn "Set01Spec ---"
  Set01Spec.run
  putStrLn "Set02Spec ---"
  Set02Spec.run
  putStrLn "Set03Spec ---"
  Set03Spec.run
  putStrLn "Set04Spec ---"
  Set04Spec.run
  putStrLn "Set05Spec ---"
  Set05Spec.run
