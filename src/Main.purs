module Main where

import Prelude

import Data.Array (catMaybes, foldl, fromFoldable, index, reverse) as Array
import Data.Either (Either(..))
import Data.Foldable (all) as Foldable
import Data.FoldableWithIndex (traverseWithIndex_) as FoldableWithIndex
import Data.HTTP.Method (Method(..))
import Data.Int (floor, toNumber) as Int
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Maybe (fromMaybe, maybe) as Maybe
import Data.String (Pattern(..))
import Data.String (split) as String
import Data.String.Utils (lines, words) as Utils
import Data.Traversable (class Foldable)
import Data.Traversable (traverse_) as Traversable
import Effect (Effect)
import Effect.Console (log) as Console
import Global (readFloat, readInt) as Global
import Graphics.Canvas (CanvasElement, Context2D)
import Graphics.Canvas (beginPath, clearRect, closePath, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, lineTo, moveTo, setCanvasDimensions, setStrokeStyle, stroke) as Canvas
import Linear (Mat(..), Vec2(..), Vec3(..), Vec4(..))
import Linear (dot4, mulVec4, norm, rotQuat, sub2, toVec4, toMat) as Linear
import Linear as Mat
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (eventListener, addEventListener) as EventTarget
import Web.HTML (Window)
import Web.HTML (window) as HTML
import Web.HTML.Window (toEventTarget, requestAnimationFrame, innerWidth, innerHeight) as Window
import Web.XHR.EventTypes (loadend) as EventTypes
import Web.XHR.ResponseType (string) as ResponseType
import Web.XHR.XMLHttpRequest (open, send, xmlHttpRequest, toEventTarget, response) as XMLHttpRequest

main :: Effect Unit
main = Canvas.getCanvasElementById "gameCanvas" >>= Maybe.maybe (Console.log "Canvas element not found.") startGame

startGame :: CanvasElement -> Effect Unit
startGame cnv =
    HTML.window >>= \wnd ->
    Canvas.getContext2D cnv >>= \ctx ->
    EventTarget.eventListener (\event -> resizeCanvas wnd cnv) >>= \ltr ->
    EventTarget.addEventListener (EventType "resize") ltr true (Window.toEventTarget wnd) *>
    resizeCanvas wnd cnv *>
    getFile (f wnd ctx) "./file.obj" where
        f :: Window -> Context2D -> String -> Effect Unit
        f wnd ctx str = Window.requestAnimationFrame (gameLoop wnd cnv ctx (loadWaveFront str)) wnd *> pure unit

gameLoop :: Window -> CanvasElement -> Context2D -> WaveFront -> Number -> Effect Unit
gameLoop wnd cnv ctx wf = f where
    f :: Number -> Effect Unit
    f tm =
        Canvas.getCanvasWidth cnv >>= \wd ->
        Canvas.getCanvasHeight cnv >>= \ht ->
        renderGame ctx wd ht wf tm *>
        Window.requestAnimationFrame f wnd *>
        pure unit

resizeCanvas :: Window -> CanvasElement -> Effect Unit
resizeCanvas wnd cnv =
    Window.innerWidth wnd >>= \wd ->
    Window.innerHeight wnd >>= \ht ->
    Canvas.setCanvasDimensions cnv { width: Int.toNumber wd, height: Int.toNumber ht }

renderGame :: Context2D -> Number -> Number -> WaveFront -> Number -> Effect Unit
renderGame ctx wd ht wf tm =
    Canvas.clearRect ctx { x: 0.0, y: 0.0, width: wd, height: ht } *>
    Canvas.beginPath ctx *>
    Canvas.setStrokeStyle ctx "blue" *>
    drawWaveFront ctx vp (Mat.mulMat pj mv) wf *>
    Canvas.stroke ctx where
        pj = Mat.perspectiveMat 60.0 (wd / ht) 1.0 100.0
        n = tm / 1000.0
        mv :: Mat
        mv = Mat.mulMat (Mat.transMat 0.0 0.0 (-5.0)) (Linear.toMat (Linear.rotQuat (Vec3 0.0 1.0 0.0) n))
        vp = Mat.viewportMat wd ht

type Poly2 = Array Vec2
type Poly3 = Array Vec3
type Poly4 = Array Vec4

drawPolygon :: Context2D -> Poly2 -> Effect Unit
drawPolygon ctx vs = FoldableWithIndex.traverseWithIndex_ f vs *> Canvas.closePath ctx where
    f 0 (Vec2 x y) = Canvas.moveTo ctx x y
    f _ (Vec2 x y) = Canvas.lineTo ctx x y

isPolygonClockwise :: Poly2 -> Boolean
isPolygonClockwise vs = Maybe.fromMaybe false (g <$> f 0 <*> f 1 <*> f 2) where
    f :: Int -> Maybe Vec2
    f = Array.index vs
    g :: Vec2 -> Vec2 -> Vec2 -> Boolean
    g v1 v2 v3 = x1 * y2 - x2 * y1 > 0.0 where
        Vec2 x1 y1 = Linear.sub2 v2 v1
        Vec2 x2 y2 = Linear.sub2 v3 v2

toDrawCoordinates :: Mat -> Poly4 -> Poly2
toDrawCoordinates (Mat r1 r2 _ r4) vs = map f vs where
    f :: Vec4 -> Vec2
    f v = Vec2 (Linear.dot4 r1 v / w) (Linear.dot4 r2 v / w) where
        w :: Number
        w = Linear.dot4 r4 v

toEyeCoordinates :: Mat -> Poly3 -> Poly4
toEyeCoordinates m vs = map (Linear.toVec4 >>> Linear.mulVec4 m >>> Linear.norm) vs

isClipped :: Poly4 -> Boolean
isClipped vs =
    Foldable.all (\(Vec4 x _ _ _) -> x <= -1.0) vs ||
    Foldable.all (\(Vec4 x _ _ _) -> x >= 1.0) vs ||
    Foldable.all (\(Vec4 _ x _ _) -> x <= -1.0) vs ||
    Foldable.all (\(Vec4 _ x _ _) -> x >= 1.0) vs ||
    Foldable.all (\(Vec4 _ _ x _) -> x <= -1.0) vs ||
    Foldable.all (\(Vec4 _ _ x _) -> x >= 1.0) vs

data WaveFront = WaveFront Verts TextureVerts NormalVerts Faces
type Verts = Array Vec3
type TextureVerts = Array Vec2
type NormalVerts = Array Vec3
type Faces = Array Face
type Face = Array Point3
data Point3 = Point3 Int Int Int

drawWaveFront :: Context2D -> Mat -> Mat -> WaveFront -> Effect Unit
drawWaveFront ctx drawMat eyeMat (WaveFront vs vts vns fs) = Traversable.traverse_ drawFace fs where
    drawFace face =
        let eyePoly4 = toEyeCoordinates eyeMat (Array.catMaybes (map (\(Point3 vi _ti _ni) -> Array.index vs vi) face)) in
        if isClipped eyePoly4 then pure unit else
        let viewPoly2 = toDrawCoordinates drawMat eyePoly4 in
        if isPolygonClockwise viewPoly2 then pure unit else
        drawPolygon ctx viewPoly2

getFile :: (String -> Effect Unit) -> String -> Effect Unit
getFile eff pth =
    XMLHttpRequest.xmlHttpRequest ResponseType.string >>= \rq ->
    let f e = XMLHttpRequest.response rq >>= \x -> Maybe.fromMaybe (pure unit) (eff <$> x) in
    EventTarget.eventListener f >>= \ltr ->
    EventTarget.addEventListener EventTypes.loadend ltr true (XMLHttpRequest.toEventTarget rq) *>
    XMLHttpRequest.open (Left GET) pth rq *>
    XMLHttpRequest.send rq

data LoadWaveFront = LoadWaveFront (List Vec3) (List Vec2) (List Vec3) (List Face)

loadWaveFront :: String -> WaveFront
loadWaveFront = Utils.lines >>> Array.foldl f (LoadWaveFront Nil Nil Nil Nil) >>> getWaveFront where
    f :: LoadWaveFront -> String -> LoadWaveFront
    f acc@(LoadWaveFront vs ts ns fs) ln = Maybe.fromMaybe acc m where
        m :: Maybe LoadWaveFront
        m = Array.index ws 0 >>= \w ->
            case w of
                "#" -> Nothing
                "mtllib" -> Nothing
                "usemtl" -> Nothing
                "g" -> Nothing
                "v" ->
                    rf 1 >>= \x ->
                    rf 2 >>= \y ->
                    rf 3 >>= \z ->
                    Just (LoadWaveFront (Cons (Vec3 x y z) vs) ts ns fs)
                "vt" ->
                    rf 1 >>= \x ->
                    rf 2 >>= \y ->
                    Just (LoadWaveFront vs (Cons (Vec2 x y) ts) ns fs)
                "vn" ->
                    rf 1 >>= \x ->
                    rf 2 >>= \y ->
                    rf 3 >>= \z ->
                    Just (LoadWaveFront vs ts (Cons (Vec3 x y z) ns) fs)
                "f" -> Just (LoadWaveFront vs ts ns (Cons (getFace ws) fs))
                _ -> Nothing
        ws :: Array String
        ws = Utils.words ln
        rf :: Int -> Maybe Number
        rf n = Global.readFloat <$> Array.index ws n
    getFace :: Array String -> Face
    getFace = map (String.split (Pattern "/")) >>> map g >>> Array.catMaybes where
        g :: Array String -> Maybe Point3
        g b =
            ri 0 >>= \v ->
            ri 1 >>= \t ->
            ri 2 >>= \n ->
            Just (Point3 ((Int.floor v) - 1) ((Int.floor t) - 1) ((Int.floor n) - 1)) where
                ri :: Int -> Maybe Number
                ri n = Global.readInt 10 <$> Array.index b n
    getWaveFront :: LoadWaveFront -> WaveFront
    getWaveFront (LoadWaveFront vs ts ns fs) = WaveFront (g vs) (g ts) (g ns) (g fs) where
        g :: forall f a. Foldable f => f a -> Array a
        g = Array.fromFoldable >>> Array.reverse
