module Assets exposing (Environment(..), env, noImage, server)


type Environment
    = Environment String


noImage : Environment
noImage =
    Environment "/images/no_image.png"


server : Environment
server =
    Environment "server.env"


env : Environment -> String
env (Environment env) =
    env
