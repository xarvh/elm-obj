module WavefrontObject
    exposing
        ( Container
        , FaceVertices(..)
        , Group
        , Object
        , parseContainer
        )

-- http://www.martinreddy.net/gfx/3d/OBJ.spec

import Array exposing (Array)
import Char
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)


-- API


type alias Container =
    { customId : String
    , materialLibraries : List String
    , objects : List Object
    }


type alias Object =
    { name : String
    , geometricVertices : List Vec3
    , textureVertices : List Vec3
    , normalVertices : List Vec3
    , groups : List Group
    }


type alias Group =
    { names : List String
    , maybeMaterialId : Maybe String
    , faces : List FaceVertices
    }


type FaceVertices
    = V (List Int)
    | VT (List ( Int, Int ))
    | VN (List ( Int, Int ))
    | VTN (List ( Int, Int, Int ))


containerParser : String -> Parser Container
containerParser customId =
    Parser.loop (emptyContainer customId) parseHelp


parseContainer : String -> String -> Result String Container
parseContainer customId string =
    Err "ni"



-- Parser bits and pieces


newline : Parser ()
newline =
    Parser.symbol "\n"


isSpace : Char -> Bool
isSpace c =
    c == ' ' || c == '\t'


spaces : Parser ()
spaces =
    Parser.chompWhile isSpace


atLeastOneSpace : Parser ()
atLeastOneSpace =
    Parser.succeed ()
        |. Parser.chompIf isSpace
        |. spaces


maybeLeadingMinus : Parser number -> Parser number
maybeLeadingMinus p =
    Parser.oneOf
        [ Parser.succeed negate
            |. Parser.symbol "-"
            |= p
        , p
        ]


index : Parser Int
index =
    let
        failIfNegative n =
            if n < 0 then
                Parser.problem "Negative indices are not (yet) supported"
            else
                Parser.succeed n
    in
    maybeLeadingMinus Parser.int
        |> Parser.andThen failIfNegative


float : Parser Float
float =
    maybeLeadingMinus Parser.float


maybe : Parser a -> Parser (Maybe a)
maybe parser =
    Parser.oneOf
        [ Parser.map Just parser
        , Parser.succeed Nothing
        ]


resultToParser : Result String a -> Parser a
resultToParser r =
    case r of
        Ok a ->
            Parser.succeed a

        Err s ->
            Parser.problem s


nameIdentifier : Parser String
nameIdentifier =
    let
        validSymbols =
            "_.-"
                |> String.toList
                |> Set.fromList

        isValid char =
            Char.isAlphaNum char || Set.member char validSymbols
    in
    Parser.variable
        { start = isValid
        , inner = isValid
        , reserved = Set.empty
        }


fileName : Parser String
fileName =
    let
        validSymbols =
            " \\_.@~/%+-=;:"
                |> String.toList
                |> Set.fromList

        isValid char =
            Char.isAlphaNum char || Set.member char validSymbols
    in
    Parser.variable
        { start = isValid
        , inner = isValid
        , reserved = Set.empty
        }



-- Line Parser


type Line
    = Empty
    | MaterialLibraryFileName String
    | ObjectName String
    | GeometricVertex Vec3
    | VertexNormal Vec3
    | TextureVertex Vec3
    | GroupNames (List String)
    | UseMaterialId String
    | SmoothingGroupNumber Int
    | Face FaceVertices


parseLine : Parser Line
parseLine =
    Parser.oneOf
        [ comment
        , materialLibrary
        , objectName
        , vertex "v" GeometricVertex
        , vertex "vn" VertexNormal
        , vertex "vt" TextureVertex
        , groupNames
        , useMaterial
        , smoothingGroup
        , face
        , empty
        ]


empty : Parser Line
empty =
    Parser.succeed Empty
        |. spaces



--         |. Parser.end


comment : Parser Line
comment =
    Parser.succeed Empty
        |. Parser.lineComment "#"


materialLibrary : Parser Line
materialLibrary =
    Parser.succeed MaterialLibraryFileName
        |. Parser.keyword "mtllib"
        |. spaces
        |= fileName
        |. spaces



--         |. Parser.end


objectName : Parser Line
objectName =
    Parser.succeed ObjectName
        |. Parser.keyword "o"
        |. spaces
        |= nameIdentifier
        |. spaces



--         |. Parser.end


vertex : String -> (Vec3 -> Line) -> Parser Line
vertex keyword lineConstructor =
    Parser.succeed (\x y z -> vec3 x y z |> lineConstructor)
        |. Parser.keyword keyword
        |. spaces
        |= float
        |. spaces
        |= float
        |. spaces
        |= Parser.oneOf
            [ float
            , Parser.succeed 0
            ]
        |. spaces



--         |. Parser.end


groupNames : Parser Line
groupNames =
    -- TODO: support more than one group name
    Parser.succeed (List.singleton >> GroupNames)
        |. Parser.keyword "g"
        |. atLeastOneSpace
        |= nameIdentifier
        |. spaces



--         |. Parser.end


useMaterial : Parser Line
useMaterial =
    Parser.succeed UseMaterialId
        |. Parser.keyword "usemtl"
        |. spaces
        |= nameIdentifier
        |. spaces



--         |. Parser.end


smoothingGroup : Parser Line
smoothingGroup =
    Parser.succeed SmoothingGroupNumber
        |. Parser.keyword "s"
        |. spaces
        |= Parser.int
        |. spaces



--         |. Parser.end


face : Parser Line
face =
    Parser.succeed Face
        |. Parser.keyword "f"
        |= Parser.loop Nothing faceHelp
        |. spaces



--         |. Parser.end


faceHelp : Maybe FaceVertices -> Parser (Parser.Step (Maybe FaceVertices) FaceVertices)
faceHelp maybeVertexList =
    Parser.oneOf
        [ Parser.map (Just >> Parser.Loop) (parseVertexListNext maybeVertexList)
        , Parser.map Parser.Done (parseVertexListEnd maybeVertexList)
        ]


parseVertexListEnd : Maybe FaceVertices -> Parser FaceVertices
parseVertexListEnd maybeVertexList =
    case maybeVertexList of
        Nothing ->
            Parser.problem "face has no vertices"

        Just faceVertices ->
            Parser.succeed <|
                case faceVertices of
                    V list ->
                        V (List.reverse list)

                    VN list ->
                        VN (List.reverse list)

                    VT list ->
                        VT (List.reverse list)

                    VTN list ->
                        VTN (List.reverse list)


maybesToFaceVertices : Int -> Maybe Int -> Maybe Int -> FaceVertices
maybesToFaceVertices v maybeT maybeN =
    case ( maybeT, maybeN ) of
        ( Nothing, Nothing ) ->
            V [ v ]

        ( Nothing, Just n ) ->
            VN [ ( v, n ) ]

        ( Just t, Nothing ) ->
            VT [ ( v, t ) ]

        ( Just t, Just n ) ->
            VTN [ ( v, t, n ) ]


parseVertexListNext : Maybe FaceVertices -> Parser FaceVertices
parseVertexListNext maybeVertexList =
    case maybeVertexList of
        Nothing ->
            -- TODO: replace all the parseV* functions with this block
            Parser.succeed maybesToFaceVertices
                |. atLeastOneSpace
                |= index
                |. Parser.symbol "/"
                |= maybe index
                |. Parser.symbol "/"
                |= maybe index

        Just (V list) ->
            parseV list

        Just (VT list) ->
            parseVT list

        Just (VN list) ->
            parseVN list

        Just (VTN list) ->
            parseVTN list


parseV : List Int -> Parser FaceVertices
parseV v_list =
    Parser.succeed (\v -> V (v :: v_list))
        |. atLeastOneSpace
        |= index
        |. maybe (Parser.symbol "//")


parseVT : List ( Int, Int ) -> Parser FaceVertices
parseVT vt_list =
    Parser.succeed (\v vt -> VT (( v, vt ) :: vt_list))
        |. atLeastOneSpace
        |= index
        |. Parser.symbol "/"
        |= index
        |. Parser.symbol "/"


parseVN : List ( Int, Int ) -> Parser FaceVertices
parseVN vn_list =
    Parser.succeed (\v vn -> VN (( v, vn ) :: vn_list))
        |. atLeastOneSpace
        |= index
        |. Parser.symbol "//"
        |= index


parseVTN : List ( Int, Int, Int ) -> Parser FaceVertices
parseVTN vtn_list =
    Parser.succeed (\v vt vn -> VTN (( v, vt, vn ) :: vtn_list))
        |. atLeastOneSpace
        |= index
        |. Parser.symbol "/"
        |= index
        |. Parser.symbol "/"
        |= index



-- Object parser


emptyContainer : String -> Container
emptyContainer id =
    { customId = id
    , materialLibraries = []
    , objects = []
    }


emptyObject : String -> Object
emptyObject name =
    { name = name
    , geometricVertices = []
    , textureVertices = []
    , normalVertices = []
    , groups = []
    }


emptyGroup : List String -> Group
emptyGroup names =
    { names = names
    , maybeMaterialId = Nothing
    , faces = []
    }


withLastObject : Container -> (Object -> Result String Object) -> Result String Container
withLastObject container updateObject =
    case container.objects of
        [] ->
            Err "you have to declare an 'o'bject before using this"

        oldObj :: os ->
            updateObject oldObj
                |> Result.map (\newObj -> { container | objects = newObj :: os })


withLastGroup : Container -> (Group -> Result String Group) -> Result String Container
withLastGroup container updateGroup =
    let
        updateObject object =
            case object.groups of
                [] ->
                    Err "you have to declare an 'g'roup before using this"

                oldGroup :: gs ->
                    updateGroup oldGroup
                        |> Result.map (\newGroup -> { object | groups = newGroup :: gs })
    in
    withLastObject container updateObject


apply : Line -> Container -> Result String Container
apply line c =
    case line of
        -- container
        Empty ->
            Ok c

        MaterialLibraryFileName name ->
            Ok { c | materialLibraries = name :: c.materialLibraries }

        ObjectName name ->
            Ok { c | objects = emptyObject name :: c.objects }

        -- object
        GeometricVertex v ->
            withLastObject c <| \obj -> Ok { obj | geometricVertices = v :: obj.geometricVertices }

        VertexNormal vn ->
            withLastObject c <| \obj -> Ok { obj | normalVertices = vn :: obj.normalVertices }

        TextureVertex vt ->
            withLastObject c <| \obj -> Ok { obj | textureVertices = vt :: obj.textureVertices }

        GroupNames names ->
            withLastObject c <| \obj -> Ok { obj | groups = emptyGroup names :: obj.groups }

        -- group
        UseMaterialId id ->
            withLastGroup c <| \group -> Ok { group | maybeMaterialId = Just id }

        SmoothingGroupNumber number ->
            -- TODO do not ignore
            Ok c

        Face faceVertices ->
            withLastGroup c <| \group -> Ok { group | faces = faceVertices :: group.faces }


parseHelp : Container -> Parser (Parser.Step Container Container)
parseHelp container =
    let
        makeResult line loopControl =
            apply line container
                |> Result.map loopControl
    in
    Parser.succeed makeResult
        |= parseLine
        |= Parser.oneOf
            [ Parser.succeed Parser.Loop
                |. newline
            , Parser.succeed Parser.Done
                |. Parser.end
            ]
        |> Parser.andThen resultToParser
