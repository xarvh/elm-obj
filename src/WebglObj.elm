module WebglObj exposing (..)

import Array exposing (Array)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Maybe.Extra
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)
import WavefrontObject exposing (FaceVertices(..), Line(..), lineParser)


-- Main types


type alias VertexAttributes =
    { v : Vec3
    , n : Vec3
    }


type alias Mesh =
    { attributes : List VertexAttributes
    , indices : List ( Int, Int, Int )
    }



-- Temporary types


type alias MeshAccumulator =
    { attributes : List VertexAttributes
    , indexByAttributes : Dict String Int
    , nextIndex : Int
    , tris : List ( Int, Int, Int )
    }


emptyMA : MeshAccumulator
emptyMA =
    { attributes = []
    , indexByAttributes = Dict.empty
    , nextIndex = 0
    , tris = []
    }


type alias ObjectAccumulator =
    { vs : Array Vec3
    , vn : Array Vec3
    , tris : List ( VertexAttributes, VertexAttributes, VertexAttributes )
    }


emptyOA : ObjectAccumulator
emptyOA =
    { vs = Array.fromList [ vec3 0 0 0 ]
    , vn = Array.fromList [ vec3 0 0 0 ]
    , tris = []
    }



--


meshParser : Parser Mesh
meshParser =
    Parser.loop [] parseHelp



--|> Parser.map containerReverseLists


parseHelp : List ObjectAccumulator -> Parser (Parser.Step (List ObjectAccumulator) (List ObjectAccumulator))
parseHelp objects =
    let
        makeResult line loopControl =
            Result.map loopControl (apply line container)
    in
    Parser.succeed makeResult
        |= lineParser
        |= Parser.oneOf
            [ Parser.succeed Parser.Loop
                |. newline
            , Parser.succeed Parser.Done
                |. Parser.end
            ]
        |> Parser.andThen resultToParser


apply : Line -> List ObjectAccumulator -> Result String (List ObjectAccumulator)
apply line objects =
    case line of
        -- ignored lines
        Empty ->
            Ok objects

        MaterialLibraryFileName name ->
            Ok objects

        ObjectName name ->
            Ok (emptyOA :: objects)

        UseMaterialId id ->
            Ok objects

        SmoothingGroupNumber number ->
            Ok objects

        GroupNames names ->
            Ok objects

        TextureVertex vt ->
            Ok objects

        -- object
        GeometricVertex v ->
            withLastObject objects <| \o -> Ok { o | vs = Array.push v o.vs }

        VertexNormal n ->
            withLastObject objects <| \o -> Ok { o | ns = Array.push n o.ns }

        Face faceVertices ->
            withLastObject objects (addFace faceVertices)


withLastObject : List Object -> (Object -> Result String Object) -> Result String (List Object)
withLastObject objects updateObject =
    case objects of
        [] ->
            Err "you have to declare an 'o'bject before using this"

        oldObj :: os ->
            updateObject oldObj
                |> Result.map (\newObj -> newObj :: os)



-- Object Faces


addFace : FaceVertices -> Object -> Result String Object
addFace faces object =
    let
        maybeResolvedFace =
            faces
                |> normalizeFace
                |> Maybe.Extra.traverse (resolveIndex object)
    in
    case maybeResolvedFace of
        -- TODO: more friendly message?
        Nothing ->
            Err "face index out of bounds"

        Just resolvedFace ->
            Ok { object | tris = faceToTriangles resolvedFace object.tris }


normalizeFace : FaceVertices -> List ( Int, Int )
normalizeFace face =
    case face of
        V list ->
            List.map (\v -> ( v, 0 )) list

        VT list ->
            List.map (\( v, t ) -> ( v, 0 )) list

        VN list ->
            list

        VTN list ->
            List.map (\( v, t, n ) -> ( v, n ))


resolveIndex : Object -> ( Int, Int ) -> Maybe VertexAttributes
resolveIndex object ( vIndex, nIndex ) =
    Maybe.map2 VertexAttributes
        (Array.get vIndex object.vs)
        (Array.get nIndex object.ns)


faceToTriangles : List a -> List ( a, a, a ) -> List ( a, a, a )
faceToTriangles vertices tris =
    case vertices of
        a :: b :: c :: vs ->
            facesToTriangles (b :: c :: vs) (( a, b, c ) :: tris)

        _ ->
            tris



--
{-


   vertexAttributesToString : VertexAttributes -> String
   vertexAttributesToString =
       Debug.toString


   addVertexAttributes : VertexAttributes -> MeshAccumulator -> ( MeshAccumulator, Int )
   addVertexAttributes attrs accu =
       let
           asString =
               vertexAttributesToString attrs
       in
       case Dict.get asString accu.indexByAttributes of
           Just index ->
               ( accu, index )

           Nothing ->
               ( { accu
                   | attributes = attrs :: accu.attributes
                   , indexByAttributes = Dict.insert asString accu.nextIndex
                   , nextIndex = accu.nextIndex + 1
                 }
               , accu.nextIndex
               )


   addTriangle : ( VertexAttributes, VertexAttributes, VertexAttributes ) -> MeshAccumulator -> MeshAccumulator
   addTriangle ( a, b, c ) accu =
       let
           ( accuA, indexA ) =
               addVertexAttributes a accu

           ( accuB, indexB ) =
               addVertexAttributes b accuA

           ( accuC, indexC ) =
               addVertexAttributes c accuB
       in
       { accuC | tris = ( indexA, indexB, index ) :: accuC.tris }




   addFace : List VertexAttributes -> MeshAccumulator -> MeshAccumulator
   addFace face accu =
       face
           |> faceToTriangles []
           |> List.foldl addTriangle accu




   objectToMesh : Object -> Mesh
   objectToMesh obj =
       let
           vs =
               Array.fromList (vec3 0 0 0 :: obj.geometricVertices)

           ns =
               Array.fromList (vec3 0 0 0 :: obj.normalVertices)

           resolveAttributes : ( Int, Int ) -> Maybe VertexAttributes
           resolveAttributes ( vIndex, nIndex ) =
               Maybe.map2 (\v n -> { v = v, n = n })
                   (Array.get vIndex vs)
                   (Array.get nIndex ns)

           resolveFace : List ( Int, Int ) -> Maybe (List VertexAttributes)
           resolveFace face =
               Maybe.Extra.traverse resolveAttributes face

           getGroupFaces : Group -> Maybe (List VertexAttributes)
           getGroupFaces group =
             Maybe.Extra.traverse (normalizeFace >> resolveFace)group.faces


           object.groups
             |> Maybe.Extra.traverse getGroupFaces
             |> Maybe.map List.concat





             |> List.map (normalizeFace >> resolveFace)
             |> Maybe.
             |> List.map
           addGroupFaces : Group -> MeshAccumulator -> Maybe MeshAccumulator
           addGroupFaces group accu =
             let



             in



       in
       xx


-}
