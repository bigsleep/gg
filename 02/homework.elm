module Main where

import Graphics.Element exposing (Element)

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)

import WebGL exposing (Drawable(..), Shader, webgl, render)

vertexShader : Shader { pv : Vec4 } {} {}
vertexShader = [glsl|
attribute vec4 pv;

void main(void)
{
    gl_Position = pv;
}
|]

fragmentShader : Shader {} {} {}
fragmentShader = [glsl|

void main(void)
{
    gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);
}
|]

positions : List Vec2
positions =
    [ vec2 -0.9 -0.9
    , vec2 0.9 -0.9
    , vec2 0.9 0.9
    , vec2 -0.9 0.9
    ]

lines : Drawable { pv : Vec4 }
lines =
    let positions' = List.map (\v -> { pv = vec4 (Vec2.getX v) (Vec2.getY v) 0 1 }) positions
        rotate ps =
            case ps of
                [] -> []
                x :: xs -> xs `List.append` [x]
    in Lines <| List.map2 (,) positions' (rotate positions')

main : Element
main = webgl (640, 480) [render vertexShader fragmentShader lines {}]
