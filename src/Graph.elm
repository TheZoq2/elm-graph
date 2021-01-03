module Graph exposing
    ( drawGraph
    , drawHorizontalLines
    , drawLegend
    , transformToGraphCoordinates
    , GraphStyle
    )

{-|
Graph rendering library

@docs drawGraph
@docs drawHorizontalLines
@docs drawLegend
@docs transformToGraphCoordinates
-}

import Svg
import Svg.Styled exposing (..)
import Svg.Styled.Attributes exposing (..)
import String exposing (fromFloat, fromInt)
import Tuple exposing (pair)
import Css exposing (Style)


type alias GraphStyle =
    { fontStyle: Style
    , lineStyle: Style
    , gridStyle: Style
    }


{-|
-}
mapCoordinates : (Float, Float) -> (Float, Float) -> Float -> Float
mapCoordinates (fromMin, fromMax) (toMin, toMax) val =
    let
        inRange = fromMax - fromMin
        outRange = toMax - toMin
    in
        ((val - fromMin) / inRange) * outRange + toMin


{-|
-}
transformToGraphCoordinates : Bool -> Float -> (Float, Float) -> Float -> Float
transformToGraphCoordinates invert viewSize (minVal, maxVal) val =
    if invert then
        mapCoordinates (maxVal, minVal) (0, viewSize) val
    else
        mapCoordinates (minVal, maxVal) (0, viewSize) val


transformFromGraphCoordinates : Float -> (Float, Float) -> Float -> Float
transformFromGraphCoordinates viewSize (minVal, maxVal) val =
    mapCoordinates (0, viewSize) (minVal, maxVal) val


{-|
    Creates an svg object from a set of values, the size of the viewport and a range of values to display
-}
drawGraph : GraphStyle -> (Int, Int) -> (Float, Float) -> (Float, Float) -> List (Float, Float) -> Svg a
drawGraph graphStyle (viewW, viewH) (minVal, maxVal) (minTime, maxTime) data =
    let
        times = List.map Tuple.first data

        x_points =
            List.map (transformToGraphCoordinates False (toFloat viewW) (minTime, maxTime)) times
            -- List.range 0 (List.length data)
            -- |> List.map (\x -> toFloat x / toFloat (List.length data) * (toFloat viewW))

        y_points = List.map Tuple.second data

        pointsString =
            List.map (transformToGraphCoordinates True (toFloat viewH) (minVal, maxVal)) y_points
            |> List.map2 pair x_points
            |> List.map (\(x,y) -> fromFloat x ++ "," ++ fromFloat y)
            |> List.intersperse " "
            |> String.concat
    in
        polyline [css [graphStyle.lineStyle], fill "none", points pointsString] []


{-|
    Draws horizontal lines into a svg object
-}
drawHorizontalLines : GraphStyle -> (Int, Int) -> (Float, Float) -> Float -> Svg a
drawHorizontalLines graphStyle (viewW, viewH) valueRange verticalStep =
    let
        yCoords =
            List.map
                (transformToGraphCoordinates True (toFloat viewH) valueRange)
                <| getHorizontalFixpoints viewH valueRange verticalStep
    in
        List.map fromFloat yCoords
            |> List.map (\y -> line [x1 "0", x2 <| fromInt viewW, y1 y, y2 y] [])
            |> g [css [graphStyle.gridStyle]]

{-|
  Draws unit legends for the y-axis
-}
drawLegend : GraphStyle -> String -> Int -> (Float, Float) -> Float -> Svg a
drawLegend graphStyle unit viewH (min, max) verticalStep =
    let
        yValues = getHorizontalFixpoints viewH (min, max) verticalStep

        -- yValues =
        --     List.range 0 (List.length yCoords)
        --     |> List.map (\y -> toFloat y * verticalStep + min)
        yCoords =
            List.map (transformToGraphCoordinates True (toFloat viewH) (min, max))
                <| yValues
    in
        List.map2 pair yCoords yValues
        |> List.map (\(yCoord, yVal) ->
                text_ [y <| fromFloat yCoord, fontSize "10px"] [text <| fromFloat yVal ++ unit] ) 
        |> g [css [graphStyle.fontStyle]]


getHorizontalFixpoints : Int -> (Float, Float) -> Float -> List Float
getHorizontalFixpoints viewH (min, max) verticalStep =
    let
        range = max - min


        stepStart = floor (min / verticalStep)
        -- stepStart = floor (min/verticalStep)
        -- stepEnd = ceiling (max/verticalStep)
        -- stepEnd = toFloat stepStart + (range * verticalStep)
    in
        List.range stepStart (stepStart + floor (range / verticalStep))
            |> List.map toFloat
            |> List.map ((*) verticalStep)
            -- |> List.map (transformToGraphCoordinates True (toFloat viewH) (min,max))


