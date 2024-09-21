module Asset exposing (..)


type Src
    = Src String


toString : Src -> String
toString src =
    case src of
        Src innerSrc ->
            innerSrc


type Content
    = Binary
    | Text
    | Markdown
        { title : String
        , frontmatter : { source_ : String }
        , headers : List { level : Int, text : String }
        }