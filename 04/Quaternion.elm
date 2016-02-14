module Quaternion
    ( Quaternion(..)
    , getX
    , getY
    , getZ
    , getW
    , dot
    , norm
    , conj
    , add
    , mul
    , makeRotateQuaternion
    , makeRotateMatrix
    ) where

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)

type Quaternion =
    Quaternion Float Float Float Float

getX : Quaternion -> Float
getX (Quaternion x _ _ _) = x

getY : Quaternion -> Float
getY (Quaternion _ y _ _) = y

getZ : Quaternion -> Float
getZ (Quaternion _ _ z _) = z

getW : Quaternion -> Float
getW (Quaternion _ _ _ w) = w

dot : Quaternion -> Float
dot (Quaternion a0 a1 a2 a3) = a0 * a0 + a1 * a1 + a2 * a2 + a3 * a3

norm : Quaternion -> Float
norm = sqrt << dot

conj : Quaternion -> Quaternion
conj (Quaternion a0 a1 a2 a3) = Quaternion -a0 -a1 -a2 a3

add : Quaternion -> Quaternion -> Quaternion
add (Quaternion a0 a1 a2 a3) (Quaternion b0 b1 b2 b3) =
    Quaternion (a0 + b0) (a1 + b1) (a2 + b2) (a3 + b3)

mul : Quaternion -> Quaternion -> Quaternion
mul (Quaternion a0 a1 a2 a3) (Quaternion b0 b1 b2 b3) =
    let c0 = a0 * b0 - a1 * b1 - a2 * b2 - a3 * b3
        c1 = a0 * b1 + a1 * b0 + a2 * b3 - a3 * b2
        c2 = a0 * b2 - a1 * b3 + a2 * b0 + a3 * b1
        c3 = a0 * b3 + a1 * b2 - a2 * b1 + a3 * b0
    in Quaternion c0 c1 c2 c3

makeRotateQuaternion : Vec3 -> Float -> Quaternion
makeRotateQuaternion v a =
    let l = Vec3.length v
        b = a * 0.5
        s = sin b / l
        x = Vec3.getX v * s
        y = Vec3.getY v * s
        z = Vec3.getZ v * s
        w = cos b
    in Quaternion x y z w

makeRotateMatrix : Quaternion -> Mat4
makeRotateMatrix q =
    let (Quaternion a0 a1 a2 a3) = q
        s = 2 / norm q
        a0a0 = a0 * a0
        a0a1 = a0 * a1
        a0a2 = a0 * a2
        a0a3 = a0 * a3
        a1a1 = a1 * a1
        a1a2 = a1 * a2
        a1a3 = a1 * a3
        a2a2 = a2 * a2
        a2a3 = a2 * a3

        v1 = vec3 (1 - s * (a1a1 + a2a2)) (s * (a0a1 - a2a3)) (s * (a0a2 + a1a3))
        v2 = vec3 (s * (a0a1 + a2a3)) (1 - s * (a0a0 + a2a2)) (s * (a1a2 - a0a3))
        v3 = vec3 (s * (a0a2 - a1a3)) (s * (a1a2 + a0a3)) (1 - s * (a0a0 + a1a1))
        
    in Mat4.makeBasis v1 v2 v3
