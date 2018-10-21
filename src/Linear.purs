module Linear where

import Prelude

import Data.Date (year)
import Math (cos, sin, tan, pi)

data Vec2 = Vec2 Number Number
dot2 :: Vec2 -> Vec2 -> Number
dot2 (Vec2 x1 y1) (Vec2 x2 y2) = x1 * x2 + y1 * y2
mul2 :: Number -> Vec2 -> Vec2
mul2 s (Vec2 x y) = Vec2 (s * x) (s * y)
neg2 :: Vec2 -> Vec2
neg2 (Vec2 x y) = Vec2 (-x) (-y)
add2 :: Vec2 -> Vec2 -> Vec2
add2 (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 + x2) (y1 + y2)
sub2 :: Vec2 -> Vec2 -> Vec2
sub2 (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 - x2) (y1 - y2)
sqrLen2 :: Vec2 -> Number
sqrLen2 x = dot2 x x

data Vec3 = Vec3 Number Number Number
dot3 :: Vec3 -> Vec3 -> Number
dot3 (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2
mul3 :: Number -> Vec3 -> Vec3
mul3 s (Vec3 x y z) = Vec3 (s * x) (s * y) (s * z)
neg3 :: Vec3 -> Vec3
neg3 (Vec3 x y z) = Vec3 (-x) (-y) (-z)
add3 :: Vec3 -> Vec3 -> Vec3
add3 (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)
sub3 :: Vec3 -> Vec3 -> Vec3
sub3 (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)
cros :: Vec3 -> Vec3 -> Vec3
cros (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (y1 * z2 - y2 * z1) (z1 * x2 - z2 * x1) (x1 * y2 - x2 * y1)
toVec4 :: Vec3 -> Vec4
toVec4 (Vec3 x1 y1 z1) = Vec4 x1 y1 z1 1.0
sqrLen3 :: Vec3 -> Number
sqrLen3 x = dot3 x x

data Vec4 = Vec4 Number Number Number Number
dot4 :: Vec4 -> Vec4 -> Number
dot4 (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = x1 * x2 + y1 * y2 + z1 * z2 + w1 * w2
mul4 :: Number -> Vec4 -> Vec4
mul4 s (Vec4 x y z w) = Vec4 (s * x) (s * y) (s * z) (s * w)
neg4 :: Vec4 -> Vec4
neg4 (Vec4 x y z w) = Vec4 (-x) (-y) (-z) (-w)
add4 :: Vec4 -> Vec4 -> Vec4
add4 (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = Vec4 (x1 + x2) (y1 + y2) (z1 + z2) (w1 + w2)
sub4 :: Vec4 -> Vec4 -> Vec4
sub4 (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = Vec4 (x1 - x2) (y1 - y2) (z1 - z2) (w1 - w2)
sqrLen4 :: Vec4 -> Number
sqrLen4 x = dot4 x x
norm :: Vec4 -> Vec4
norm (Vec4 x y z w) = Vec4 (x / w) (y / w) (z / w) 1.0
toVec2 :: Vec4 -> Vec2
toVec2 (Vec4 x y _ w) = Vec2 (x / w) (y / w)

data Mat = Mat Vec4 Vec4 Vec4 Vec4
col1 :: Mat -> Vec4
col1 (Mat (Vec4 x _ _ _) (Vec4 y _ _ _) (Vec4 z _ _ _) (Vec4 w _ _ _)) = Vec4 x y z w
col2 :: Mat -> Vec4
col2 (Mat (Vec4 _ x _ _) (Vec4 _ y _ _) (Vec4 _ z _ _) (Vec4 _ w _ _)) = Vec4 x y z w
col3 :: Mat -> Vec4
col3 (Mat (Vec4 _ _ x _) (Vec4 _ _ y _) (Vec4 _ _ z _) (Vec4 _ _ w _)) = Vec4 x y z w
col4 :: Mat -> Vec4
col4 (Mat (Vec4 _ _ _ x) (Vec4 _ _ _ y) (Vec4 _ _ _ z) (Vec4 _ _ _ w)) = Vec4 x y z w
transpose :: Mat -> Mat
transpose (Mat (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) (Vec4 x3 y3 z3 w3) (Vec4 x4 y4 z4 w4)) =
    Mat (Vec4 x1 x2 x3 z4) (Vec4 y1 y2 y3 y4) (Vec4 z1 z2 z3 z4) (Vec4 w1 w2 w3 w4)
mulVec4 :: Mat -> Vec4 -> Vec4
mulVec4 (Mat r1 r2 r3 r4) v = Vec4 (dot4 r1 v) (dot4 r2 v) (dot4 r3 v) (dot4 r4 v)
mulMat :: Mat -> Mat -> Mat
mulMat (Mat r1 r2 r3 r4) rhs =
    Mat (Vec4 (dot4 r1 c1) (dot4 r1 c2) (dot4 r1 c3) (dot4 r1 c4))
        (Vec4 (dot4 r2 c1) (dot4 r2 c2) (dot4 r2 c3) (dot4 r2 c4))
        (Vec4 (dot4 r3 c1) (dot4 r3 c2) (dot4 r3 c3) (dot4 r3 c4))
        (Vec4 (dot4 r4 c1) (dot4 r4 c2) (dot4 r4 c3) (dot4 r4 c4))
    where c1 = col1 rhs
          c2 = col2 rhs
          c3 = col3 rhs
          c4 = col4 rhs

identMat :: Mat
identMat = Mat (Vec4 1.0 0.0 0.0 0.0) (Vec4 0.0 1.0 0.0 0.0) (Vec4 0.0 0.0 1.0 0.0) (Vec4 0.0 0.0 0.0 1.0)

orthoProjMat :: Number -> Number -> Number -> Number -> Number -> Number -> Mat
orthoProjMat l r b t n f =
    Mat (Vec4 (2.0 / (r - l)) 0.0             0.0              ((r + l) / (r - l)))
        (Vec4 0.0             (2.0 / (t - b)) 0.0              ((t + b) / (t - b)))
        (Vec4 0.0             0.0             (-2.0 / (f - n)) ((f + n) / (f - n)))
        (Vec4 0.0             0.0             0.0              1.0)

persProjMat :: Number -> Number -> Number -> Number -> Number -> Number -> Mat
persProjMat l r b t n f =
    Mat (Vec4 ((2.0 * n) / (r - l)) 0.0                   ((r + l) / (r - l))  0.0)
        (Vec4 0.0                   ((2.0 * n) / (t - b)) ((t + b) / (t - b))  0.0)
        (Vec4 0.0                   0.0                   (-(f + n) / (f - n)) (-(2.0 * f * n) / (f - n)))
        (Vec4 0.0                   0.0                   (-1.0)               0.0)

invPerspectiveProjectionMat :: Number -> Number -> Number -> Number -> Number -> Number -> Mat
invPerspectiveProjectionMat l r b t n f =
    Mat (Vec4 ((r - l) / (2.0 * n)) 0.0                   0.0                        ((r + l) / (2.0 * n)))
        (Vec4 0.0                   ((t - b) / (2.0 * n)) 0.0                        ((t + b) / (2.0 * n)))
        (Vec4 0.0                   0.0                   0.0                        (-1.0))
        (Vec4 0.0                   0.0                   (-(f - n) / (2.0 * f * n)) ((f + n) / (2.0 * f * n)))

transMat :: Number -> Number -> Number -> Mat
transMat x y z =
    Mat (Vec4 1.0 0.0 0.0 x)
        (Vec4 0.0 1.0 0.0 y)
        (Vec4 0.0 0.0 1.0 z)
        (Vec4 0.0 0.0 0.0 1.0)

invTransMat :: Number -> Number -> Number -> Mat
invTransMat x y z =
    Mat (Vec4 1.0 0.0 0.0 (-x))
        (Vec4 0.0 1.0 0.0 (-y))
        (Vec4 0.0 0.0 1.0 (-z))
        (Vec4 0.0 0.0 0.0 1.0)

scaleMat :: Number -> Number -> Number -> Mat
scaleMat x y z =
    Mat (Vec4 x   0.0 0.0 0.0)
        (Vec4 0.0 y   0.0 0.0)
        (Vec4 0.0 0.0 z   0.0)
        (Vec4 0.0 0.0 0.0 1.0)

scaleMatInv :: Number -> Number -> Number -> Mat
scaleMatInv x y z =
    Mat (Vec4 (1.0 / x) 0.0       0.0       0.0)
        (Vec4 0.0       (1.0 / y) 0.0       0.0)
        (Vec4 0.0       0.0       (1.0 / z) 0.0)
        (Vec4 0.0       0.0       0.0       1.0)

viewportMat :: Number -> Number -> Mat
viewportMat width height =
    mulMat (transMat halfWidth halfHeight 0.0)
            (mulMat (scaleMat halfWidth halfHeight 1.0)
                     (scaleMat 1.0 (-1.0) 1.0))
    where halfWidth = width / 2.0
          halfHeight = height / 2.0

perspectiveMat :: Number -> Number -> Number -> Number -> Mat
perspectiveMat fov aspectRatio n f = persProjMat l r b t n f
    where halfFovRads = pi * (fov / 360.0)
          scale = (tan halfFovRads) * n
          r = aspectRatio * scale
          l = -r
          t = scale
          b = -t

data Quat = Quat Number Number Number Number
identQuat :: Quat
identQuat = Quat 1.0 0.0 0.0 0.0
rotQuat :: Vec3 -> Number -> Quat
rotQuat (Vec3 x y z) rads = Quat (cos halfRads) (x * sin halfRads) (y * sin halfRads) (z * sin halfRads)
    where halfRads = rads / 2.0

toMat :: Quat -> Mat
toMat (Quat w x y z) =
    Mat (Vec4 ((ww + xx) - (yy + zz)) (xy2 - wz2)         (xz2 + wy2)         0.0)
        (Vec4 (xy2 + wz2)             (ww - xx + yy - zz) (yz2 + wx2)         0.0)
        (Vec4 (xz2 - wy2)             (yz2 - wx2)         (ww - xx - yy + zz) 0.0)
        (Vec4 0.0                     0.0                 0.0                 1.0) where
            ww = w * w
            xx = x * x
            yy = y * y
            zz = z * z
            xy2 = 2.0 * x * y
            wz2 = 2.0 * w * z
            xz2 = 2.0 * x * z
            wy2 = 2.0 * w * y
            yz2 = 2.0 * y * z
            wx2 = 2.0 * w * x

-- qToM :: Quat -> Mat
    -- pub fn to_matrix(&self) -> Mat<S> {
    --     Mat::new(
    --         S::sub(S::sub(S::add(S::mul(self.w, self.w), S::mul(self.x, self.x)), S::mul(self.y, self.y)), S::mul(self.z, self.z)),
    --         S::sub(S::mul(S::mul(S::TWO, self.x), self.y), S::mul(S::mul(S::TWO, self.w), self.z)),
    --         S::add(S::mul(S::mul(S::TWO, self.x), self.z), S::mul(S::mul(S::TWO, self.w), self.y)),
    --         S::ZERO,

    --         S::add(S::mul(S::mul(S::TWO, self.x), self.y), S::mul(S::mul(S::TWO, self.w), self.z)),
    --         S::sub(S::add(S::sub(S::mul(self.w, self.w), S::mul(self.x, self.x)), S::mul(self.y, self.y)), S::mul(self.z, self.z)),
    --         S::add(S::mul(S::mul(S::TWO, self.y), self.z), S::mul(S::mul(S::TWO, self.w), self.x)),
    --         S::ZERO,

    --         S::sub(S::mul(S::mul(S::TWO, self.x), self.z), S::mul(S::mul(S::TWO, self.w), self.y)),
    --         S::sub(S::mul(S::mul(S::TWO, self.y), self.z), S::mul(S::mul(S::TWO, self.w), self.x)),
    --         S::add(S::sub(S::sub(S::mul(self.w, self.w), S::mul(self.x, self.x)), S::mul(self.y, self.y)), S::mul(self.z, self.z)),
    --         S::ZERO,

    --         S::ZERO,
    --         S::ZERO,
    --         S::ZERO,
    --         S::ONE,
    --     )
    -- }
