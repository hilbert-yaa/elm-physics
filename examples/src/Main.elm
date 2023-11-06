module Main exposing (main)

{-| This demo implements a smooth car simulation.

  - `RayCastCar.Car` — raycast vehicle simulation algorithm,
  - `RayCastCar.Jeep` — load the 3d model using elm-obj-file.

Press arrow keys to drive, "b" to brake.

Try to add more obstacles to the demo by changing the initialWorld,
or tweak the settings in the `RayCastCar.Jeep` module!

-}

import Acceleration
import Angle
import Axis3d
import Block3d exposing (Block3d)
import Browser
import Browser.Dom
import Browser.Events
import Camera3d exposing (Camera3d)
import Color
import Common.Fps as Fps
import Direction3d
import Duration
import Frame3d
import Html exposing (Html)
import Html.Attributes
import Json.Decode
import Length exposing (Meters)
import Mass
import Physics.Body as Body exposing (Body)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.World as World exposing (World)
import Pixels exposing (Pixels)
import Plane3d
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import RaycastCar.Car as Car exposing (Wheel)
import RaycastCar.Jeep as Jeep exposing (Jeep)
import Scene3d exposing (Entity)
import Scene3d.Material
import Task
import Vector3d
import Viewpoint3d


noCmd : Model -> ( Model, Cmd Msg )
noCmd model =
    ( model, Cmd.none )


type Id
    = Obstacle (Block3d Meters BodyCoordinates)
    | Floor
    | Car (List (Wheel Id))


type alias Model =
    { dimensions : ( Quantity Int Pixels, Quantity Int Pixels )
    , fps : List Float
    , world : World Id
    , jeep : Maybe Jeep
    , firstTick : Bool
    , speeding : Float
    , steering : Float
    , braking : Bool
    }


type Msg
    = Tick Float
    | Resize Int Int
    | KeyDown Command
    | KeyUp Command
    | JeepLoaded (Result String Jeep)


type Command
    = Speed Float
    | Steer Float
    | Brake
    | Restart


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { dimensions = ( Pixels.int 0, Pixels.int 0 )
      , world = initialWorld
      , fps = []
      , jeep = Nothing
      , firstTick = True
      , speeding = 0
      , steering = 0
      , braking = False
      }
    , Cmd.batch
        [ Task.perform
            (\{ viewport } ->
                Resize (round viewport.width) (round viewport.height)
            )
            Browser.Dom.getViewport
        , Jeep.load { texture = "Jeep.png", mesh = "Jeep.obj.txt" } |> Task.attempt JeepLoaded
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize Resize
        , Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onKeyDown (keyDecoder KeyDown)
        , Browser.Events.onKeyUp (keyDecoder KeyUp)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        JeepLoaded result ->
            case result of
                Ok jeep ->
                    { model
                        | jeep = Just jeep
                        , world =
                            World.add
                                (Body.compound jeep.collider (Car (Jeep.wheels jeep))
                                    |> Body.withBehavior (Body.dynamic (Mass.kilograms 4000))
                                    |> Body.moveTo (Point3d.meters 0 0 4)
                                )
                                model.world
                    }
                        |> noCmd

                _ ->
                    model |> noCmd

        Tick dt ->
            { model
                | world =
                    case model.jeep of
                        Just jeep ->
                            model.world
                                |> World.update
                                    (\body ->
                                        case Body.data body of
                                            Car wheels ->
                                                let
                                                    ( newBody, newWheels ) =
                                                        Car.simulate
                                                            (Jeep.settings jeep)
                                                            (Duration.seconds (1 / 60))
                                                            { worldWithoutCar =
                                                                World.keepIf
                                                                    (\b ->
                                                                        case Body.data b of
                                                                            Car _ ->
                                                                                False

                                                                            _ ->
                                                                                True
                                                                    )
                                                                    model.world
                                                            , speeding = model.speeding
                                                            , steering = model.steering
                                                            , braking = model.braking
                                                            }
                                                            wheels
                                                            body
                                                in
                                                Body.withData (Car newWheels) newBody

                                            _ ->
                                                body
                                    )
                                |> World.simulate (Duration.seconds (1 / 60))

                        _ ->
                            model.world
                , fps = Fps.update dt model.fps
                , firstTick = False
            }
                |> noCmd

        Resize width height ->
            { model | dimensions = ( Pixels.pixels width, Pixels.pixels height ) } |> noCmd

        KeyDown (Steer k) ->
            { model | steering = k } |> noCmd

        KeyDown (Speed k) ->
            { model | speeding = k } |> noCmd

        KeyUp (Steer k) ->
            { model
                | steering =
                    if k == model.steering then
                        0

                    else
                        model.steering
            }
                |> noCmd

        KeyUp (Speed k) ->
            { model
                | speeding =
                    if k == model.speeding then
                        0

                    else
                        model.speeding
            }
                |> noCmd

        KeyDown Brake ->
            { model | braking = True } |> noCmd

        KeyUp Brake ->
            { model | braking = False } |> noCmd

        KeyDown Restart ->
            model |> noCmd

        KeyUp Restart ->
            init ()


view : Model -> Html Msg
view { world, fps, jeep, dimensions } =
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "top" "0"
        ]
        [ Scene3d.sunny
            { upDirection = Direction3d.z
            , sunlightDirection = Direction3d.xyZ (Angle.degrees -15) (Angle.degrees -45)
            , shadows = True
            , camera = camera world
            , dimensions = dimensions
            , background = Scene3d.transparentBackground
            , clipDepth = Length.meters 0.1
            , entities = List.map (bodyToEntity jeep) (World.bodies world)
            }
        , Fps.view fps (List.length (World.bodies world))
        ]


getCar : World Id -> Maybe (Body Id)
getCar world =
    List.filter
        (\body ->
            case Body.data body of
                Car _ ->
                    True

                _ ->
                    False
        )
        (World.bodies world)
        |> List.head


camera : World Id -> Camera3d Meters WorldCoordinates
camera world =
    let
        maybeJeep =
            getCar world

        ( carPosition, carDirection ) =
            case maybeJeep of
                Just jeep ->
                    Body.frame jeep
                        |> (\frame ->
                                ( Frame3d.originPoint frame, Frame3d.yDirection frame )
                           )

                Nothing ->
                    ( Point3d.meters 0 0 0, Direction3d.positiveX )

        eyePoint =
            carPosition
                |> Point3d.translateIn Direction3d.positiveZ (Length.meters 20)
                |> Point3d.translateIn carDirection (Length.meters -40)
    in
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.lookAt
                { eyePoint = eyePoint
                , focalPoint = carPosition
                , upDirection = Direction3d.positiveZ
                }
        , verticalFieldOfView = Angle.degrees 30
        }


bodyToEntity : Maybe Jeep -> Body Id -> Entity WorldCoordinates
bodyToEntity maybeJeep body =
    let
        id =
            Body.data body

        frame =
            Body.frame body
    in
    Scene3d.placeIn frame <|
        case id of
            Floor ->
                Scene3d.quad (Scene3d.Material.matte Color.white)
                    (Point3d.meters -100 -100 0)
                    (Point3d.meters -100 100 0)
                    (Point3d.meters 100 100 0)
                    (Point3d.meters 100 -100 0)

            Obstacle block ->
                Scene3d.blockWithShadow
                    (Scene3d.Material.nonmetal
                        { baseColor = Color.lightGray
                        , roughness = 1
                        }
                    )
                    block

            Car wheels ->
                case maybeJeep of
                    Just jeep ->
                        Scene3d.group
                            (List.foldl
                                (\wheel entities ->
                                    let
                                        position =
                                            wheel.chassisConnectionPoint

                                        { downDirection, rightDirection } =
                                            Jeep.settings jeep

                                        axisDirection =
                                            Axis3d.direction wheel.axis

                                        angle =
                                            if Quantity.greaterThan Quantity.zero (Direction3d.angleFrom rightDirection axisDirection) then
                                                identity

                                            else
                                                Frame3d.mirrorAcross Plane3d.yz

                                        newPosition =
                                            position |> Point3d.translateBy (Vector3d.withLength wheel.suspensionLength downDirection)

                                        newFrame =
                                            Frame3d.atOrigin
                                                |> angle
                                                |> Frame3d.rotateAround (Axis3d.through Point3d.origin (Direction3d.reverse rightDirection)) wheel.rotation
                                                |> Frame3d.rotateAround (Axis3d.through Point3d.origin downDirection) wheel.steering
                                                |> Frame3d.moveTo newPosition
                                    in
                                    (Scene3d.meshWithShadow jeep.material
                                        jeep.wheel
                                        jeep.wheelShadow
                                        |> Scene3d.placeIn newFrame
                                    )
                                        :: entities
                                )
                                [ Scene3d.meshWithShadow jeep.material
                                    jeep.chassis
                                    jeep.chassisShadow
                                ]
                                wheels
                            )

                    Nothing ->
                        Scene3d.nothing


initialWorld : World Id
initialWorld =
    World.empty
        |> World.withGravity (Acceleration.metersPerSecondSquared 9.8) Direction3d.negativeZ
        |> World.add (Body.plane Floor)
        |> World.add slope
        |> World.add (box (Point3d.meters 15 -15 0.5))
        |> World.add (box (Point3d.meters 15 -16.5 0.5))
        |> World.add (box (Point3d.meters 15 -18 0.5))
        |> World.add (box (Point3d.meters 15 -16 1.5))
        |> World.add (box (Point3d.meters 15 -17.5 1.5))
        |> World.add (box (Point3d.meters 15 -16.5 2.5))


{-| A slope to give a car the initial push.
-}
slope : Body Id
slope =
    let
        slopeBlock =
            Block3d.centeredOn Frame3d.atOrigin
                ( Length.meters 10
                , Length.meters 16
                , Length.meters 0.5
                )
    in
    Body.block slopeBlock (Obstacle slopeBlock)
        |> Body.rotateAround Axis3d.x (Angle.radians (pi / 16))
        |> Body.moveTo (Point3d.meters 0 -2 1.5)


box : Point3d Meters WorldCoordinates -> Body Id
box position =
    let
        boxBlock =
            Block3d.centeredOn Frame3d.atOrigin
                ( Length.meters 1
                , Length.meters 1
                , Length.meters 1
                )
    in
    Body.block boxBlock (Obstacle boxBlock)
        |> Body.withBehavior (Body.dynamic (Mass.kilograms 10))
        |> Body.moveTo position


keyDecoder : (Command -> Msg) -> Json.Decode.Decoder Msg
keyDecoder toMsg =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                case string of
                    "a" ->
                        Json.Decode.succeed (toMsg (Steer -1))

                    "d" ->
                        Json.Decode.succeed (toMsg (Steer 1))

                    "w" ->
                        Json.Decode.succeed (toMsg (Speed 1))

                    "s" ->
                        Json.Decode.succeed (toMsg (Speed -1))

                    "b" ->
                        Json.Decode.succeed (toMsg Brake)

                    "r" ->
                        Json.Decode.succeed (toMsg Restart)

                    _ ->
                        Json.Decode.fail ("Unrecognized key: " ++ string)
            )
