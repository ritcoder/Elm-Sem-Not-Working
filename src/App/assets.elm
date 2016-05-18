module App.Assets
  exposing ( smallLogo, image, imagesUrl )


smallLogo : String
smallLogo = image "small_logo.png"


image : String -> String
image path =
  imagesUrl ++ path


baseUrl : String
baseUrl = "assets/"


imagesUrl : String
imagesUrl = baseUrl ++ "images/"