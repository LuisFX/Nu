namespace Elmario
open Prime
open Nu
open Nu.Declarative


[<AutoOpen>]
module Bullet =

    type BulletDispatcher () =
        inherit EntityDispatcher2d (true)

        static let [<Literal>] BulletLifeTime = 27L

        static let handleBodyCollision evt world =
            let bullet = evt.Subscriber : Entity
            let world =
                if World.getAdvancing world
                then World.destroyEntity bullet world
                else world
            (Cascade, world)

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<StaticSpriteFacet>]

        static member Properties =
            [define Entity.Size (v3 20.0f 20.0f 0.0f)
             define Entity.Presence Omnipresent
             define Entity.Density 0.1f
             define Entity.Restitution 0.5f
             define Entity.LinearDamping 0.0f
             define Entity.GravityScale 0.0f
             define Entity.Bullet true
             define Entity.BodyShape (BodySphere { Center = v3Zero; Radius = 0.5f; PropertiesOpt = None })
             define Entity.StaticImage FloppyBeard.Assets.Gameplay.PlayerBulletImage]

        override this.Register (bullet, world) =
            let world = World.monitor handleBodyCollision bullet.BodyCollisionEvent bullet world
            let world = World.schedule (World.destroyEntity bullet) (UpdateTime BulletLifeTime) bullet world
            world


// this module provides global handles to the game's key simulants.
// having a Simulants module for your game is optional, but can be nice to avoid duplicating string literals across
// the code base.
[<RequireQualifiedAccess>]
module Simulants =

    let Screen = Nu.Screen "Screen"
    let Group = Screen / "Group"
    let Elmario = Group / "Elmario"

// this is our Elm-style command type
type Command =
    | Update
    | Jump
    | Nop
    interface Nu.Command

// this is our Elm-style game dispatcher


type ElmarioDispatcher () =
    inherit GameDispatcher<unit, Message, Command> (())

    static let [<Literal>] BulletForce = 25.0f

    static let createBullet (player : Entity) world =
        let mutable playerTransform = player.GetTransform world
        let (bullet, world) = World.createEntity<BulletDispatcher> NoOverlay None player.Group world // OPTIMIZATION: NoOverlay to avoid reflection.
        let bulletPosition = playerTransform.Position + v3 (playerTransform.Size.X * 0.7f) 0.0f 0.0f
        let world = bullet.SetPosition bulletPosition world
        let world = bullet.SetElevation playerTransform.Elevation world
        (bullet, world)
    
    static let propelBullet (bullet : Entity) world =
        let world = World.playSound Constants.Audio.SoundVolumeDefault FloppyBeard.Assets.Gameplay.ShotSound world
        World.applyBodyLinearImpulse (v3 BulletForce 0.0f 0.0f) (bullet.GetPhysicsId world) world

    static let shootBullet (player : Entity) world =
        let (bullet, world) = createBullet player world
        propelBullet bullet world

    static let handleSpawnBullet evt world =
        let player = evt.Subscriber : Entity
        let world =
            if World.getAdvancing world then
                // if not (player.HasFallen world) then
                if World.getUpdateTime world % 5L = 0L
                then shootBullet player world
                    // else world
                else world
            else world
        (Cascade, world)

    // here we define the game's properties and event handling
    override this.Initialize (_, _) =
        [Game.UpdateEvent => Update
         Game.KeyboardKeyDownEvent =|> fun evt ->
             if evt.Data.KeyboardKey = KeyboardKey.Up && not evt.Data.Repeated
             then Jump :> Signal
             else Nop :> Signal]

    // here we handle the Elm-style commands
    override this.Command (_, command, _, world) =
        match command with
        | Update ->
            let physicsId = Simulants.Elmario.GetPhysicsId world
            if World.isKeyboardKeyDown KeyboardKey.Left world then
                let world =
                    if World.isBodyOnGround physicsId world
                    then World.applyBodyForce (v3 -2500.0f 0.0f 0.0f) physicsId world
                    else World.applyBodyForce (v3 -750.0f 0.0f 0.0f) physicsId world
                just world
            elif World.isKeyboardKeyDown KeyboardKey.Right world then
                let world =
                    if World.isBodyOnGround physicsId world
                    then World.applyBodyForce (v3 2500.0f 0.0f 0.0f) physicsId world
                    else World.applyBodyForce (v3 750.0f 0.0f 0.0f) physicsId world
                just world
            else just world
        | Jump ->
            let physicsId = Simulants.Elmario.GetPhysicsId world
            if World.isBodyOnGround physicsId world then
                let world = World.playSound Constants.Audio.SoundVolumeDefault (asset "Gameplay" "Jump") world
                let world = World.applyBodyForce (v3 0.0f 140000.0f 0.0f) physicsId world
                just world
            else just world
        | Nop -> just world

    // override this.Register (elmario: Entity, world: World) =
    //     let evt = elmario.UpdateEvent
    //     let world = World.monitor handleSpawnBullet evt world
    //     world

    // here we describe the content of the game including elmario, the ground he walks on, and a rock.
    override this.Content (_, _) =
        [
            Content.screen Simulants.Screen.Name Vanilla []
                [
                    Content.group
                        Simulants.Group.Name 
                        []
                        [
                            Content.sideViewCharacter
                                Simulants.Elmario.Name
                                [
                                    Entity.Position == v3 0.0f 54.0f 0.0f
                                    Entity.Size == v3 108.0f 108.0f 0.0f
                                ]
                            Content.block2d "Ground"
                                [
                                    Entity.Position == v3 0.0f -224.0f 0.0f
                                    Entity.Size == v3 768.0f 64.0f 0.0f
                                    Entity.StaticImage == asset "Gameplay" "TreeTop"
                                ]
                            Content.block2d "Rock"
                                [
                                    Entity.Position == v3 352.0f -160.0f 0.0f
                                    Entity.Size == v3 64.0f 64.0f 0.0f
                                    Entity.StaticImage == asset "Gameplay" "Rock"
                                ]
                            Content.block2d "Rock2"
                                [
                                    Entity.Position == v3 -150.0f -160.0f 0.0f
                                    Entity.Size == v3 64.0f 64.0f 0.0f
                                    Entity.StaticImage == asset "Gameplay" "Rock"
                                ]
                        ]
                ]
        ]