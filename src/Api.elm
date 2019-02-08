module Api exposing (apiUrl)


apiUrl : String -> String
apiUrl str =
    --    "@@@apiServer@@@" ++ str
    "https://producer.officeiko.co.jp/" ++ str
