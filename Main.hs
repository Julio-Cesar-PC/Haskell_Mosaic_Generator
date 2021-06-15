import Text.Printf

type Point     = (Float,Float)
type Rect      = (Point,Float,Float)
type Circle    = (Point,Float)
type RGB = (Int,Int,Int)

-------------------------------------------------------------------------------
-- Paletas
-------------------------------------------------------------------------------

-- Paleta preta e branca
bwPalette :: Int -> [RGB]
bwPalette n = take n $ cycle [(255,255,255),(0,0,0)]

-- Paleta de cores do Mondrian
mondrianPalette :: Int -> [RGB]
mondrianPalette n = take n $ cycle [(250,201,1),(255,255,255),(34,80,149),(221,1,0)]

-- Função que retorna a lista RGB pseudo aleatória a partir de uma seed
lcgPalette :: Int -> Int -> [RGB]
lcgPalette 0 seed = []
lcgPalette qtd seed = (seedR,seedG,seedB) : lcgPalette (qtd-1) (seed+1)
  where seedR = mod (a * seed + c) 255
        seedG = mod ((a+seed) * seed+1 + c) 255
        seedB = mod ((a+(seed-c)) * seed+2 + c) 255
        a = 752145 -- Pode mudar o valor de a e c tambem se quiser
        c = 951334

-- Retorna a quantidade de cores ou retângulos que vai precisar
nPalette :: Float -> Float -> Float
nPalette w h = (w)*(h)

-------------------------------------------------------------------------------
-- Geração de figuras
-------------------------------------------------------------------------------

-- Cria a lista de tuplas com a posição e o tamanho de cada retângulo
genMosaic :: Float -> Float -> Float -> Float -> [Rect]
genMosaic w h w_tela h_tela = [((x+(w-1)*x, y+(h-1)*y), w, h) | y <- [0.. (h_tela-h/h)], x <- [0.. (w_tela-w)/w]]

-------------------------------------------------------------------------------
-- Strings SVG
-------------------------------------------------------------------------------

svgRect :: Rect -> String -> String 
svgRect ((x,y),w,h) style = 
  printf "<rect x='%.3f' y='%.3f' width='%.2f' height='%.2f' style='%s' filter='nada' />\n" x y w h style -- filter='url(#blur) para um filtro de blur

svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 

svgFil_blur :: String
svgFil_blur = printf "<filter id='blur'><feGaussianBlur in='SourceGraphic' stdDeviation='2' /></filter>\n" -- stdDeviation quanto maior mais desfoque

svgEnd :: String
svgEnd = "</svg>"

svgStyle :: RGB -> String
svgStyle (r,g,b) = printf "fill:rgb(%d,%d,%d); stroke:rgb(0,0,0); stroke-width:0; mix-blend-mode: screen;" r g b

svgElements :: (a -> String -> String) -> [a] -> [String] -> String
svgElements func elements styles = concat $ zipWith func elements styles

main :: IO ()
main = do
  let (w,h,w_tela,h_tela) = (50,5,1000,1000)    -- Edita o tamanho dos retângulos e da tela.
      -- Calculo do numero de retângulos
      nrects = round $ nPalette (w_tela/w) (h_tela/h)
      -- Paleta de cores
      paletter = lcgPalette nrects (87174654)  -- O segundo argumento é a seed, para cada numero terá uma paleta de cores
      -- Geração da lista de tuplas para retângulos
      rects = genMosaic w h (w_tela) (h_tela)
      -- Gera string das figuras
      svgfigs = svgElements svgRect rects (map svgStyle paletter)
      -- Gera string do SVG
      svgstrs = svgBegin (w_tela) (h_tela) ++ svgFil_blur ++ svgfigs ++ svgEnd
  -- Gera o documento SVG
  writeFile "exemplo5.svg" $ svgstrs
