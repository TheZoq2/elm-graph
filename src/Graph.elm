module Graph exposing (drawGraph, drawHorizontalLines, drawLegend)

{-|
Graph rendering library

@docs drawGraph
@docs drawHorizontalLines
@docs drawLegend
-}

import Svg
import Svg exposing (..)
import Svg.Attributes exposing (..)



transformToGraphCoordinates : Bool -> Float -> (Float, Float) -> Float -> Float
transformToGraphCoordinates invert viewHeight (minVal, maxVal) val =
    let
        minMaxRange = maxVal - minVal
    in
        if invert then
            viewHeight - (viewHeight * ((val - minVal) / minMaxRange))
        else
            (viewHeight * ((val - minVal) / minMaxRange))



{-|
    Creates an svg object from a set of values, the size of the viewport and a range of values to display
-}
drawGraph : (Int, Int) -> (Float, Float) -> (Float, Float) -> List (Float, Float) -> Svg a
drawGraph (viewW, viewH) (minVal, maxVal) (minTime, maxTime) data =
    let
        times = List.map Tuple.first data

        x_points =
            List.map (transformToGraphCoordinates False (toFloat viewW) (minTime, maxTime)) times
            -- List.range 0 (List.length data)
            -- |> List.map (\x -> toFloat x / toFloat (List.length data) * (toFloat viewW))

        y_points = List.map Tuple.second data

        pointsString =
            List.map (transformToGraphCoordinates True (toFloat viewH) (minVal, maxVal)) y_points
            |> List.map2 (,) x_points
            |> List.map (\(x,y) -> toString x ++ "," ++ toString y)
            |> List.intersperse " "
            |> String.concat
    in
        polyline [fill "none", stroke "black", points pointsString] []


{-|
    Draws horizontal lines into a svg object
-}
drawHorizontalLines : (Int, Int) -> (Float, Float) -> Float -> Svg a
drawHorizontalLines (viewW, viewH) valueRange verticalStep =
    let
        yCoords = (getHorizontalFixpoints viewH valueRange verticalStep)
    in
        List.map toString yCoords
        |> List.map (\y -> line [x1 "0", x2 <| toString viewW, y1 y, y2 y] [])
        |> g [stroke "lightgray"]

{-|
  Draws unit legends for the y-axis
-}
drawLegend : String -> Int -> (Float, Float) -> Float -> Svg a
drawLegend unit viewH (min, max) verticalStep =
    let
        yCoords = (getHorizontalFixpoints viewH (min, max) verticalStep)

        yValues =
            List.range 0 (List.length yCoords)
            |> List.map (\y -> (toFloat y) * verticalStep + min)
    in
        List.map2 (,) yCoords yValues
        |> List.map (\(yCoord, yVal) ->
                text_ [y <| toString yCoord, fontSize "10px"] [text <| toString yVal ++ unit] ) 
        |> g []


getHorizontalFixpoints : Int -> (Float, Float) -> Float -> List Float
getHorizontalFixpoints viewH (min, max) verticalStep =
    let
        stepStart = (floor (min/verticalStep))
        stepEnd = (ceiling (max/verticalStep))
    in
        List.range stepStart stepEnd
        |> List.map toFloat
        |> List.map ((*) verticalStep)
        |> List.map (transformToGraphCoordinates True (toFloat viewH) (min,max))


