
bmi :: Float -> Float -> String
bmi peso altura =
    let imc = peso / altura^2
    in if imc<=18.5 then "ABAIXO" else if imc>=30 then "ACIMA" else "NORMAL"


bmi' :: Float -> Float -> String
bmi' peso altura
  | imc<=18.5 = "ABAIXO"
  | imc>=30 = "ACIMA"
  | otherwise = "NORMAL"
  where
      imc = peso / altura ^ 2

