module WebglObj exposing (..)

import Array exposing (Array)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Parser exposing (Parser)
import Result.Extra
import WavefrontObject exposing (FaceVertices(..), Line(..))


-- Main types


type alias VertexAttributes =
    { v : Vec3
    , n : Vec3
    }


type alias Mesh =
    List ( VertexAttributes, VertexAttributes, VertexAttributes )


parseMesh string =
    string
        |> Parser.run meshParser


meshParser : Parser Mesh
meshParser =
    emptyAccumulator
        |> WavefrontObject.customContainerParser applyLine
        |> Parser.map (.faces >> List.foldl faceToTriangles [])



-- Temporary accumulator


type alias Accumulator =
    { vs : Array Vec3
    , ns : Array Vec3
    , faces : List (List VertexAttributes)
    }


emptyAccumulator : Accumulator
emptyAccumulator =
    { vs = Array.fromList [ vec3 0 0 0 ]
    , ns = Array.fromList [ vec3 0 0 0 ]
    , faces = []
    }



-- Apply Line


applyLine : Line -> Accumulator -> Result String Accumulator
applyLine line accu =
    case line of
        -- ignored lines
        Empty ->
            Ok accu

        MaterialLibraryFileName name ->
            Ok accu

        ObjectName name ->
            Ok accu

        UseMaterialId id ->
            Ok accu

        SmoothingGroupNumber number ->
            Ok accu

        GroupNames names ->
            Ok accu

        TextureVertex vt ->
            Ok accu

        -- object
        GeometricVertex v ->
            Ok { accu | vs = Array.push v accu.vs }

        VertexNormal n ->
            Ok { accu | ns = Array.push n accu.ns }

        Face faceVertices ->
            addFace faceVertices accu



-- Faces


addFace : FaceVertices -> Accumulator -> Result String Accumulator
addFace faces object =
    faces
        |> normalizeFace
        |> List.map (resolveIndex object)
        |> Result.Extra.combine
        |> Result.map (\resolvedFace -> { object | faces = resolvedFace :: object.faces })


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
            List.map (\( v, t, n ) -> ( v, n )) list


arrayGet : String -> Int -> Array a -> Result String a
arrayGet name index array =
    case Array.get index array of
        Just a ->
            Ok a

        Nothing ->
            Err <| name ++ " index is " ++ String.fromInt index ++ " but array length is " ++ String.fromInt (Array.length array)


resolveIndex : Accumulator -> ( Int, Int ) -> Result String VertexAttributes
resolveIndex object ( vIndex, nIndex ) =
    Result.map2 VertexAttributes
        (arrayGet "v" vIndex object.vs)
        (arrayGet "n" nIndex object.ns)


faceToTriangles : List a -> List ( a, a, a ) -> List ( a, a, a )
faceToTriangles vertices tris =
    case vertices of
        a :: b :: c :: vs ->
            faceToTriangles (b :: c :: vs) (( a, b, c ) :: tris)

        _ ->
            tris
