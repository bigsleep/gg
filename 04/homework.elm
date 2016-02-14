module Main where

import Array exposing (Array)
import Graphics.Element exposing (Element)
import Time exposing (Time)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)

import WebGL exposing (Drawable(..), Shader, webgl, render)

import Quaternion exposing (Quaternion(..))

vertexShader : Shader { pv : Vec4 } { mp : Mat4, mv : Mat4, mt : Mat4, mr : Mat4 } {}
vertexShader = [glsl|
attribute vec4 pv;
uniform mat4 mp, mv, mt, mr;

void main(void)
{
    gl_Position = mp * mv * mt * mr * pv;
}
|]

fragmentShader : Shader {} { mp : Mat4, mv : Mat4, mt : Mat4, mr : Mat4 } {}
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

type alias Timeline a =
    { initial : (Time, a)
    , terminal : (Time, a)
    , length : Int
    , timelineP2 : Array (Time, a)
    , timelineP1 : Array (Time, a)
    , timeline : Array (Time, a)
    , timelineN1 : Array (Time, a)
    }

createTimeline : a -> List (Time, a) -> Timeline a
createTimeline x xs =
    let length = List.length xs + 1
        initial = (0, x)
        terminal = Maybe.withDefault initial
            <| if length > 0
                then List.head << List.drop (length - 2) <| xs
                else Nothing
        timeline = initial :: xs
        timelineP1 = List.take length <| initial :: timeline
        timelineP2 = List.take length <| initial :: timelineP1
        timelineN1 = xs `List.append` [terminal]
    in  { initial = initial
        , terminal = terminal
        , length = length
        , timelineP2 = Array.fromList timelineP2
        , timelineP1 = Array.fromList timelineP1
        , timeline = Array.fromList (initial :: xs)
        , timelineN1 = Array.fromList timelineN1
        }

positionTimeline : Timeline Vec3
positionTimeline =
    createTimeline
    (vec3 0 0 0)
    [ (1.5 * Time.second, vec3 0.0 -2.0 -2.0)
    , (2.5 * Time.second, vec3 -1.0 -1.0 -1.0)
    , (3.5 * Time.second, vec3 -0.5 -0.5 -0.5)
    , (5.0 * Time.second, vec3 0.0 0.0 0.0)
    ]

initialQuaternion : Quaternion
initialQuaternion = Quaternion 0 0 0 1

quaternionTimeline : Timeline Quaternion
quaternionTimeline =
    createTimeline
    (Quaternion 0 0 0 1)
    []

findIndex : (a -> Bool) -> List a -> Maybe Int
findIndex f xs =
    let findIndex' g ys i =
            case ys of
                [] -> Nothing
                z::zs -> if f z
                    then Just i
                    else findIndex' g zs (i + 1)
    in findIndex' f xs 0

interpolate : (Time, Vec3) -> (Time, Vec3) -> (Time, Vec3) -> (Time, Vec3) -> Time -> Vec3
interpolate (t0, p0) (t1, p1) (t2, p2) (t3, p3) t =
    let t' = (t - t1) / (t2 - t1)
        x = catmullrom (Vec3.getX p0) (Vec3.getX p1) (Vec3.getX p2) (Vec3.getX p3) t'
        y = catmullrom (Vec3.getY p0) (Vec3.getY p1) (Vec3.getY p2) (Vec3.getY p3) t'
        z = catmullrom (Vec3.getZ p0) (Vec3.getZ p1) (Vec3.getZ p2) (Vec3.getZ p3) t'
        {-
        x = (Vec3.getX p1) + t' * (Vec3.getX p2 - Vec3.getX p1)
        y = (Vec3.getY p1) + t' * (Vec3.getY p2 - Vec3.getY p1)
        z = (Vec3.getZ p1) + t' * (Vec3.getZ p2 - Vec3.getZ p1)
        -}
    in vec3 x y z

catmullrom : Float -> Float -> Float -> Float -> Float -> Float
catmullrom x0 x1 x2 x3 t =
    let v1 = (x2 - x0) * 0.5
        v2 = (x3 - x1) * 0.5
    in (((2 * x1 - 2 * x2 + v1 + v2) * t - 3 * x1 + 3 * x2 - 2 * v1 - v2) * t + v1) * t + x1

mp : Mat4
mp =
    let aspect = toFloat windowWidth / toFloat windowHeight
    in Mat4.makePerspective (0.5 / pi * 180.0) aspect 1.0 15.0

mv : Mat4
mv = Mat4.makeLookAt (vec3 3.0 4.0 5.0) (vec3 0.0 0.0 0.0) (vec3 0.0 1.0 0.0)

mc : Mat4
mc = mp `Mat4.mul` mv

mt : Timeline Vec3 -> Time -> Mat4
mt a t =
    let f (t', _) = t' >= t
        p = case findIndex f (Array.toList a.timeline) of
            Just i -> Maybe.withDefault (snd a.initial) <|
                Maybe.map5 interpolate
                (Array.get i a.timelineP2)
                (Array.get i a.timelineP1)
                (Array.get i a.timeline)
                (Array.get i a.timelineN1)
                (Just t)

            _ -> if t <= fst a.initial
                then snd a.initial
                else snd a.terminal
    in Mat4.makeTranslate p

mr : Timeline Quaternion -> Time -> Mat4
mr _ _ = Mat4.identity

main : Signal Element
main =
    let uniform t = 
            { mp = mp
            , mv = mv
            , mt = mt positionTimeline t
            , mr = mr quaternionTimeline t 
            }
        f u = webgl (windowWidth, windowHeight) [render vertexShader fragmentShader lines u]
        sig = Signal.foldp (+) 0 <| Time.fps 30
    in (f << uniform) `Signal.map` sig
