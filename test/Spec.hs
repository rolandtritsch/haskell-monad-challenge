import Set01Spec
import Set02Spec
import Set03Spec

main :: IO ()
main = do
  putStrLn "Set01Spec ---"
  Set01Spec.run
  putStrLn "Set02Spec ---"
  Set02Spec.run
  putStrLn "Set03Spec ---"
  Set03Spec.run
