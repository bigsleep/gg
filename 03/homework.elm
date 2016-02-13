module Main where

import Array exposing (Array)
import Graphics.Element exposing (Element)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)

import WebGL exposing (Drawable(..), Shader, webgl, render)

vertexShader : Shader { pv : Vec4 } { mc : Mat4 } {}
vertexShader = [glsl|
attribute vec4 pv;
uniform mat4 mc;

void main(void)
{
    gl_Position = mc * pv;
}
|]

fragmentShader : Shader {} { mc : Mat4 } {}
fragmentShader = [glsl|

void main(void)
{
    gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);
}
|]

positions : Array Vec3
positions =
    let a = 0.9
    in Array.fromList
    [ vec3 -a -a -a
    , vec3 a -a -a
    , vec3 a -a a
    , vec3 -a -a a
    , vec3 -a a -a
    , vec3 a a -a
    , vec3 a a a
    , vec3 -a a a
    ]

edges : List (Int, Int)
edges =
    [ (0, 1)
    , (1, 2)
    , (2, 3)
    , (3, 0)
    , (0, 4)
    , (1, 5)
    , (2, 6)
    , (3, 7)
    , (4, 5)
    , (5, 6)
    , (6, 7)
    , (7, 4)
    ]

lines : Drawable { pv : Vec4 }
lines =
    let toPoint a = { pv = vec4 (Vec3.getX a) (Vec3.getY a) (Vec3.getZ a) 1 }
        toLine (i, j) =
            case (Array.get i positions, Array.get j positions) of
                (Just a, Just b) -> Just (toPoint a, toPoint b)
                _ -> Nothing
    in Lines << List.filterMap identity << List.map toLine <| edges

windowWidth : Int
windowWidth = 680

windowHeight : Int
windowHeight = 480

mp : Mat4
mp =
    let aspect = toFloat windowWidth / toFloat windowHeight
    in Mat4.makePerspective (0.5 / pi * 180.0) aspect 1.0 15.0

mv : Mat4
mv = Mat4.makeLookAt (vec3 3.0 4.0 5.0) (vec3 0.0 0.0 0.0) (vec3 0.0 1.0 0.0)

mc : Mat4
mc = mp `Mat4.mul` mv

main : Element
main = webgl (windowWidth, windowHeight) [render vertexShader fragmentShader lines { mc = mc }]
