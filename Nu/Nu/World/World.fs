﻿// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Reflection
open System.Threading
open SDL2
open Prime
open Nu

[<RequireQualifiedAccess>]
module Nu =

    let mutable private Initialized = false

    /// Initialize the Nu game engine.
    let init nuConfig =

        // init only if needed
        if not Initialized then

            // process loading assemblies
            AppDomain.CurrentDomain.AssemblyLoad.Add (fun args ->
                Reflection.AssembliesLoaded.[args.LoadedAssembly.FullName] <- args.LoadedAssembly)
            AppDomain.CurrentDomain.add_AssemblyResolve (ResolveEventHandler (fun _ args ->
                snd (Reflection.AssembliesLoaded.TryGetValue args.Name)))

            // process existing assemblies
            for assembly in AppDomain.CurrentDomain.GetAssemblies () do
                Reflection.AssembliesLoaded.[assembly.FullName] <- assembly

            // ensure the current culture is invariate
            Thread.CurrentThread.CurrentCulture <- Globalization.CultureInfo.InvariantCulture

            // init logging
            Log.init (Some "Log.txt")

            // init math module
            Math.init ()

            // init OpenGL assertiveness
            OpenGL.Hl.InitAssert
#if DEBUG
                nuConfig.StandAlone
#else
                false
#endif

            // init simulant modules
            WorldModuleGame.init ()
            WorldModuleScreen.init ()
            WorldModuleGroup.init ()
            WorldModuleEntity.init ()

            // init simulant types
            Entity.init ()

            // init content variables
            WorldTypes.EmptyGameContent <- GameContent.empty
            WorldTypes.EmptyScreenContent <- ScreenContent.empty
            WorldTypes.EmptyGroupContent <- GroupContent.empty
            WorldTypes.EmptyEntityContent <- EntityContent.empty

            // init debug view F# reach-arounds
            WorldTypes.viewGame <- fun world -> World.viewGameProperties (world :?> World)
            WorldTypes.viewScreen <- fun screen world -> World.viewScreenProperties (screen :?> Screen) (world :?> World)
            WorldTypes.viewGroup <- fun group world -> World.viewGroupProperties (group :?> Group) (world :?> World)
            WorldTypes.viewEntity <- fun entity world -> World.viewEntityProperties (entity :?> Entity) (world :?> World)

            // init handleSubscribeAndUnsubscribeEventHook F# reach-around
            WorldTypes.handleSubscribeAndUnsubscribeEventHook <- fun subscribing eventAddress _ worldObj ->
                // here we need to update the event publish flags for entities based on whether there are subscriptions to
                // these events. These flags exists solely for efficiency reasons. We also look for subscription patterns
                // that these optimizations do not support, and warn the developer if they are invoked. Additionally, we
                // warn if the user attempts to subscribe to a Change event with a wildcard as doing so is not supported.
                let world = worldObj :?> World
                let eventNames = Address.getNames eventAddress
                let eventNamesLength = Array.length eventNames
                let world =
                    if eventNamesLength >= 5 then
                        let eventFirstName = eventNames.[0]
                        let entity = Entity (Array.skip 2 eventNames)
                        match eventFirstName with
                        | "Update" ->
#if DEBUG
                            if Array.contains (Address.head Events.Wildcard) eventNames then
                                Log.debug
                                    ("Subscribing to entity update events with a wildcard is not supported. " +
                                     "This will cause a bug where some entity update events are not published.")
#endif
                            World.updateEntityPublishUpdateFlag entity world |> snd'
#if !DISABLE_ENTITY_POST_UPDATE
                        | "PostUpdate" ->
    #if DEBUG
                            if Array.contains (Address.head Events.Wildcard) eventNames then
                                Log.debug
                                    ("Subscribing to entity post-update events with a wildcard is not supported. " +
                                     "This will cause a bug where some entity post-update events are not published.")
    #endif
                            World.updateEntityPublishPostUpdateFlag entity world |> snd'
#endif
                        | _ -> world
                    else world
                let world =
                    if eventNamesLength >= 3 then
                        match eventNames.[0] with
                        | "Change" ->
                            let world =
                                if eventNamesLength >= 6 then
                                    let entityAddress = rtoa (Array.skip 3 eventNames)
                                    let entity = Entity entityAddress
                                    match World.tryGetKeyedValueFast<UMap<Entity Address, int>> (EntityChangeCountsId, world) with
                                    | (true, entityChangeCounts) ->
                                        match entityChangeCounts.TryGetValue entityAddress with
                                        | (true, entityChangeCount) ->
                                            let entityChangeCount = if subscribing then inc entityChangeCount else dec entityChangeCount
                                            let entityChangeCounts =
                                                if entityChangeCount = 0
                                                then UMap.remove entityAddress entityChangeCounts
                                                else UMap.add entityAddress entityChangeCount entityChangeCounts
                                            let world =
                                                if entity.Exists world then
                                                    if entityChangeCount = 0 then World.setEntityPublishChangeEvents false entity world |> snd'
                                                    elif entityChangeCount = 1 then World.setEntityPublishChangeEvents true entity world |> snd'
                                                    else world
                                                else world
                                            World.addKeyedValue EntityChangeCountsId entityChangeCounts world
                                        | (false, _) ->
                                            if not subscribing then failwithumf ()
                                            let world = if entity.Exists world then World.setEntityPublishChangeEvents true entity world |> snd' else world
                                            World.addKeyedValue EntityChangeCountsId (UMap.add entityAddress 1 entityChangeCounts) world
                                    | (false, _) ->
                                        if not subscribing then failwithumf ()
                                        let config = World.getCollectionConfig world
                                        let entityChangeCounts = UMap.makeEmpty HashIdentity.Structural config
                                        let world = if entity.Exists world then World.setEntityPublishChangeEvents true entity world |> snd' else world
                                        World.addKeyedValue EntityChangeCountsId (UMap.add entityAddress 1 entityChangeCounts) world
                                else world
                            if Array.contains (Address.head Events.Wildcard) eventNames then
                                Log.debug "Subscribing to change events with a wildcard is not supported."
                            world
                        | _ -> world
                    else world
                world :> obj

            // init getEntityIs2d F# reach-around
            WorldTypes.getEntityIs2d <- fun entityObj worldObj ->
                World.getEntityIs2d (entityObj :?> Entity) (worldObj :?> World)

            // init eval F# reach-around
            // TODO: remove duplicated code with the following 4 functions...
            WorldModule.eval <- fun expr localFrame scriptContext world ->
                match expr with
                | Scripting.Unit ->
                    // OPTIMIZATION: don't bother evaluating unit
                    struct (Scripting.Unit, world)
                | _ ->
                    let oldLocalFrame = World.getLocalFrame world
                    let oldScriptContext = World.getScriptContext world
                    World.setLocalFrame localFrame world
                    let world = World.setScriptContext scriptContext world
                    ScriptingSystem.addProceduralBindings (Scripting.AddToNewFrame 1) (seq { yield struct ("self", Scripting.String (scstring scriptContext)) }) world
                    let struct (evaled, world) = World.evalInternal expr world
                    ScriptingSystem.removeProceduralBindings world
                    let world = World.setScriptContext oldScriptContext world
                    World.setLocalFrame oldLocalFrame world
                    struct (evaled, world)

            // init evalMany F# reach-around
            WorldModule.evalMany <- fun exprs localFrame scriptContext world ->
                let oldLocalFrame = World.getLocalFrame world
                let oldScriptContext = World.getScriptContext world
                World.setLocalFrame localFrame world
                let world = World.setScriptContext scriptContext world
                ScriptingSystem.addProceduralBindings (Scripting.AddToNewFrame 1) (seq { yield struct ("self", Scripting.String (scstring scriptContext)) }) world
                let struct (evaleds, world) = World.evalManyInternal exprs world
                ScriptingSystem.removeProceduralBindings world
                let world = World.setScriptContext oldScriptContext world
                World.setLocalFrame oldLocalFrame world
                struct (evaleds, world)

            // init evalWithLogging F# reach-around
            WorldModule.evalWithLogging <- fun expr localFrame scriptContext world ->
                match expr with
                | Scripting.Unit ->
                    // OPTIMIZATION: don't bother evaluating unit
                    struct (Scripting.Unit, world)
                | _ ->
                    let oldLocalFrame = World.getLocalFrame world
                    let oldScriptContext = World.getScriptContext world
                    World.setLocalFrame localFrame world
                    let world = World.setScriptContext scriptContext world
                    ScriptingSystem.addProceduralBindings (Scripting.AddToNewFrame 1) (seq { yield struct ("self", Scripting.String (scstring scriptContext)) }) world
                    let struct (evaled, world) = World.evalWithLoggingInternal expr world
                    ScriptingSystem.removeProceduralBindings world
                    let world = World.setScriptContext oldScriptContext world
                    World.setLocalFrame oldLocalFrame world
                    struct (evaled, world)

            // init evalMany F# reach-around
            WorldModule.evalManyWithLogging <- fun exprs localFrame scriptContext world ->
                let oldLocalFrame = World.getLocalFrame world
                let oldScriptContext = World.getScriptContext world
                World.setLocalFrame localFrame world
                let world = World.setScriptContext scriptContext world
                ScriptingSystem.addProceduralBindings (Scripting.AddToNewFrame 1) (seq { yield struct ("self", Scripting.String (scstring scriptContext)) }) world
                let struct (evaleds, world) = World.evalManyWithLoggingInternal exprs world
                ScriptingSystem.removeProceduralBindings world
                let world = World.setScriptContext oldScriptContext world
                World.setLocalFrame oldLocalFrame world
                struct (evaleds, world)

            // TODO: P1: implement!
            WorldModule.addSimulantScriptUnsubscription <- fun _ _ _ ->
                failwithnie ()

            // TODO: P1: implement!
            WorldModule.unsubscribeSimulantScripts <- fun _ _ ->
                failwithnie ()

            // init isSelected F# reach-around
            WorldModule.isSelected <- fun simulant world ->
                World.isSelected simulant world

            // init getScreenEcs F# reach-around
            WorldModule.getScreenEcs <- 
                World.getScreenEcs

            // init sortSubscriptionByElevation F# reach-around
            WorldModule.sortSubscriptionsByElevation <- fun subscriptions worldObj ->
                let world = worldObj :?> World
                EventSystem.sortSubscriptionsBy
                    (fun (simulant : Simulant) _ ->
                        match simulant with
                        | :? Entity as entity -> { SortElevation = entity.GetElevation world; SortHorizon = 0.0f; SortTarget = entity } :> IComparable
                        | :? Group as group -> { SortElevation = Constants.Engine.GroupSortPriority; SortHorizon = 0.0f; SortTarget = group } :> IComparable
                        | :? Screen as screen -> { SortElevation = Constants.Engine.ScreenSortPriority; SortHorizon = 0.0f; SortTarget = screen } :> IComparable
                        | :? Game | :? GlobalSimulantGeneralized -> { SortElevation = Constants.Engine.GameSortPriority; SortHorizon = 0.0f; SortTarget = Simulants.Game } :> IComparable
                        | _ -> failwithumf ())
                    subscriptions
                    world

            // init admitScreenElements F# reach-around
            WorldModule.admitScreenElements <- fun screen world ->
                let entities = World.getGroups screen world |> Seq.map (flip World.getEntitiesFlattened world) |> Seq.concat |> SegmentedList.ofSeq
                let (entities2d, entities3d) = SegmentedList.partition (fun (entity : Entity) -> entity.GetIs2d world) entities
                let oldWorld = world
                let quadtree =
                    MutantCache.mutateMutant
                        (fun () -> oldWorld.WorldExtension.Dispatchers.RebuildQuadtree oldWorld)
                        (fun quadtree ->
                            for entity in entities2d do
                                let entityState = World.getEntityState entity world
                                Quadtree.addElement entityState.Presence entityState.Bounds.Box2 entity quadtree
                            quadtree)
                        (World.getQuadtree world)
                let world = World.setQuadtree quadtree world
                let octree =
                    MutantCache.mutateMutant
                        (fun () -> oldWorld.WorldExtension.Dispatchers.RebuildOctree oldWorld)
                        (fun octree ->
                            for entity in entities3d do
                                let entityState = World.getEntityState entity world
                                let element = Octelement.make entityState.Static entityState.Light entityState.Presence entity
                                Octree.addElement entityState.Bounds element octree
                            octree)
                        (World.getOctree world)
                let world = World.setOctree octree world
                world
                
            // init evictScreenElements F# reach-around
            WorldModule.evictScreenElements <- fun screen world ->
                let entities = World.getGroups screen world |> Seq.map (flip World.getEntitiesFlattened world) |> Seq.concat |> SegmentedArray.ofSeq
                let (entities2d, entities3d) = SegmentedArray.partition (fun (entity : Entity) -> entity.GetIs2d world) entities
                let oldWorld = world
                let quadtree =
                    MutantCache.mutateMutant
                        (fun () -> oldWorld.WorldExtension.Dispatchers.RebuildQuadtree oldWorld)
                        (fun quadtree ->
                            for entity in entities2d do
                                let entityState = World.getEntityState entity world
                                Quadtree.removeElement entityState.Presence entityState.Bounds.Box2 entity quadtree
                            quadtree)
                        (World.getQuadtree world)
                let world = World.setQuadtree quadtree world
                let octree =
                    MutantCache.mutateMutant
                        (fun () -> oldWorld.WorldExtension.Dispatchers.RebuildOctree oldWorld)
                        (fun octree ->
                            for entity in entities3d do
                                let entityState = World.getEntityState entity world
                                let element = Octelement.make entityState.Static entityState.Light entityState.Presence entity
                                Octree.removeElement entityState.Bounds element octree
                            octree)
                        (World.getOctree world)
                let world = World.setOctree octree world
                world

            // init registerScreenPhysics F# reach-around
            WorldModule.registerScreenPhysics <- fun screen world ->
                let entities =
                    World.getGroups screen world |>
                    Seq.map (flip World.getEntitiesFlattened world) |>
                    Seq.concat |>
                    SegmentedList.ofSeq
                SegmentedList.fold (fun world (entity : Entity) ->
                    World.registerEntityPhysics entity world)
                    world entities

            // init unregisterScreenPhysics F# reach-around
            WorldModule.unregisterScreenPhysics <- fun screen world ->
                let entities =
                    World.getGroups screen world |>
                    Seq.map (flip World.getEntitiesFlattened world) |>
                    Seq.concat |>
                    SegmentedList.ofSeq
                SegmentedList.fold (fun world (entity : Entity) ->
                    World.unregisterEntityPhysics entity world)
                    world entities

            // init trySignal F# reach-around
            WorldModule.trySignal <- fun signalObj simulant world ->
                match simulant with
                | :? Entity as entity -> (entity.GetDispatcher world).TrySignal (signalObj, entity, world)
                | :? Group as group -> (group.GetDispatcher world).TrySignal (signalObj, group, world)
                | :? Screen as screen -> (screen.GetDispatcher world).TrySignal (signalObj, screen, world)
                | :? Game as game -> (game.GetDispatcher world).TrySignal (signalObj, game, world)
                | _ -> failwithumf ()

            // init miscellaneous F# reach-arounds
            WorldModule.register <- fun simulant world -> World.register simulant world
            WorldModule.unregister <- fun simulant world -> World.unregister simulant world
            WorldModule.destroyImmediate <- fun simulant world -> World.destroyImmediate simulant world
            WorldModule.destroy <- fun simulant world -> World.destroy simulant world

            // init scripting
            WorldScripting.init ()
            WorldBindings.init ()

            // init vsync
            Vsync.Init nuConfig.RunSynchronously

            // init event world caching
            EventSystemDelegate.setEventAddressCaching true

            // mark init flag
            Initialized <- true

[<AutoOpen; ModuleBinding>]
module WorldModule3 =

    type World with

        static member private pairWithName source =
            (getTypeName source, source)

        static member private makeDefaultGameDispatcher () =
            World.pairWithName (GameDispatcher ())

        static member private makeDefaultScreenDispatchers () =
            Map.ofList [World.pairWithName (ScreenDispatcher ())]

        static member private makeDefaultGroupDispatchers () =
            Map.ofList [World.pairWithName (GroupDispatcher ())]

        static member private makeDefaultEntityDispatchers () =
            // TODO: consider if we should reflectively generate these.
            Map.ofListBy World.pairWithName $
                [EntityDispatcher (true, false, false, false)
                 EntityDispatcher2d (false) :> EntityDispatcher
                 EntityDispatcher3d (true, false) :> EntityDispatcher
                 StaticSpriteDispatcher () :> EntityDispatcher
                 AnimatedSpriteDispatcher () :> EntityDispatcher
                 GuiDispatcher () :> EntityDispatcher
                 ButtonDispatcher () :> EntityDispatcher
                 LabelDispatcher () :> EntityDispatcher
                 TextDispatcher () :> EntityDispatcher
                 ToggleButtonDispatcher () :> EntityDispatcher
                 RadioButtonDispatcher () :> EntityDispatcher
                 FpsDispatcher () :> EntityDispatcher
                 FeelerDispatcher () :> EntityDispatcher
                 FillBarDispatcher () :> EntityDispatcher
                 BasicEmitterDispatcher2d () :> EntityDispatcher
                 EffectDispatcher2d () :> EntityDispatcher
                 BlockDispatcher2d () :> EntityDispatcher
                 BoxDispatcher2d () :> EntityDispatcher
                 SideViewCharacterDispatcher () :> EntityDispatcher
                 TileMapDispatcher () :> EntityDispatcher
                 TmxMapDispatcher () :> EntityDispatcher
                 LightDispatcher3d () :> EntityDispatcher
                 SkyBoxDispatcher () :> EntityDispatcher
                 StaticBillboardDispatcher () :> EntityDispatcher
                 StaticModelSurfaceDispatcher () :> EntityDispatcher
                 StaticModelDispatcher () :> EntityDispatcher
                 StaticModelHierarchyDispatcher () :> EntityDispatcher]

        static member private makeDefaultFacets () =
            // TODO: consider if we should reflectively generate these.
            Map.ofListBy World.pairWithName $
                [Facet false
                 ScriptFacet () :> Facet
                 StaticSpriteFacet () :> Facet
                 AnimatedSpriteFacet () :> Facet
                 TextFacet () :> Facet
                 BasicEmitter2dFacet () :> Facet
                 Effect2dFacet () :> Facet
                 RigidBodyFacet () :> Facet
                 JointFacet () :> Facet
                 TileMapFacet () :> Facet
                 TmxMapFacet () :> Facet
                 LayoutFacet () :> Facet
                 LightFacet3d () :> Facet
                 SkyBoxFacet () :> Facet
                 StaticBillboardFacet () :> Facet
                 StaticModelSurfaceFacet () :> Facet
                 StaticModelFacet () :> Facet]

        /// Update late bindings internally stored by the engine from types found in the given assemblies.
        static member updateLateBindings (assemblies : Assembly array) world =
            Content.UpdateLateBindingsCount <- inc Content.UpdateLateBindingsCount
            World.clearClipboard world // HACK: clear what's on the clipboard rather than changing its dispatcher instance.
            let pluginType =
                assemblies |>
                Array.map (fun assembly -> assembly.GetTypes ()) |>
                Array.concat |>
                Array.filter (fun ty -> ty.IsSubclassOf typeof<NuPlugin>) |>
                Array.filter (fun ty -> not ty.IsAbstract) |>
                Array.filter (fun ty -> ty.GetConstructors () |> Seq.exists (fun ctor -> ctor.GetParameters().Length = 0)) |>
                Array.head
            let plugin = Activator.CreateInstance pluginType :?> NuPlugin
            let pluginFacets = plugin.Birth<Facet> assemblies
            let pluginEntityDispatchers = plugin.Birth<EntityDispatcher> assemblies
            let pluginGroupDispatchers = plugin.Birth<GroupDispatcher> assemblies
            let pluginScreenDispatchers = plugin.Birth<ScreenDispatcher> assemblies
            let pluginGameDispatchers = plugin.Birth<GameDispatcher> assemblies
            let world =
                { world with WorldExtension = { world.WorldExtension with Plugin = plugin }}
            let world =
                Array.fold (fun world (facetName, facet) ->
                    { world with WorldExtension = { world.WorldExtension with Dispatchers = { world.WorldExtension.Dispatchers with Facets = Map.add facetName facet world.WorldExtension.Dispatchers.Facets }}})
                    world pluginFacets
            let world =
                Array.fold (fun world (entityDispatcherName, entityDispatcher) ->
                    { world with WorldExtension = { world.WorldExtension with Dispatchers = { world.WorldExtension.Dispatchers with EntityDispatchers = Map.add entityDispatcherName entityDispatcher world.WorldExtension.Dispatchers.EntityDispatchers }}})
                    world pluginEntityDispatchers
            let world =
                Array.fold (fun world (groupDispatcherName, groupDispatcher) ->
                    { world with WorldExtension = { world.WorldExtension with Dispatchers = { world.WorldExtension.Dispatchers with GroupDispatchers = Map.add groupDispatcherName groupDispatcher world.WorldExtension.Dispatchers.GroupDispatchers }}})
                    world pluginGroupDispatchers
            let world =
                Array.fold (fun world (screenDispatcherName, screenDispatcher) ->
                    { world with WorldExtension = { world.WorldExtension with Dispatchers = { world.WorldExtension.Dispatchers with ScreenDispatchers = Map.add screenDispatcherName screenDispatcher world.WorldExtension.Dispatchers.ScreenDispatchers }}})
                    world pluginScreenDispatchers
            let world =
                Array.fold (fun world (gameDispatcherName, gameDispatcher) ->
                    { world with WorldExtension = { world.WorldExtension with Dispatchers = { world.WorldExtension.Dispatchers with GameDispatchers = Map.add gameDispatcherName gameDispatcher world.WorldExtension.Dispatchers.GameDispatchers }}})
                    world pluginGameDispatchers
            let lateBindingses =
                [|Array.map (snd >> cast<LateBindings>) pluginFacets
                  Array.map (snd >> cast<LateBindings>) pluginEntityDispatchers
                  Array.map (snd >> cast<LateBindings>) pluginGroupDispatchers
                  Array.map (snd >> cast<LateBindings>) pluginScreenDispatchers
                  Array.map (snd >> cast<LateBindings>) pluginGameDispatchers|] |>
                Array.concat
            let world =
                UMap.fold (fun world simulant _ ->
                    Array.fold (fun world lateBindings -> World.updateLateBindings3 lateBindings simulant world) world lateBindingses)
                    world (World.getSimulants world)
            let world =
                UMap.fold
                    (fun world simulant _ -> World.trySynchronize false simulant world)
                    world (World.getSimulants world)
            world

        /// Make an empty world.
        static member makeEmpty (config : WorldConfig) =

            // ensure game engine is initialized
            Nu.init config.NuConfig

            // make the default plug-in
            let plugin = NuPlugin ()

            // make the world's event delegate
            let eventDelegate =
                let eventTracing = Constants.Engine.EventTracing
                let eventTracerOpt = if eventTracing then Some (Log.remark "Event") else None // NOTE: lambda expression is duplicated in multiple places...
                let eventFilter = Constants.Engine.EventFilter
                let globalSimulant = Simulants.Game
                let globalSimulantGeneralized = { GsgAddress = atoa globalSimulant.GameAddress }
                let eventConfig = if config.Imperative then Imperative else Functional
                EventSystemDelegate.make eventTracerOpt eventFilter globalSimulant globalSimulantGeneralized eventConfig

            // make the default game dispatcher
            let defaultGameDispatcher = World.makeDefaultGameDispatcher ()

            // make the world's dispatchers
            let dispatchers =
                { Facets = World.makeDefaultFacets ()
                  EntityDispatchers = World.makeDefaultEntityDispatchers ()
                  GroupDispatchers = World.makeDefaultGroupDispatchers ()
                  ScreenDispatchers = World.makeDefaultScreenDispatchers ()
                  GameDispatchers = Map.ofList [defaultGameDispatcher]
                  TryGetExtrinsic = World.tryGetExtrinsic
                  UpdateEntityInEntityTree = World.updateEntityInEntityTree
                  RebuildQuadtree = World.rebuildQuadtree
                  RebuildOctree = World.rebuildOctree }

            // make the world's subsystems
            let subsystems =
                { PhysicsEngine2d = MockPhysicsEngine.make ()
                  RendererProcess =
                    RendererInline
                        ((fun _ -> MockRenderer3d.make () :> Renderer3d),
                         (fun _ -> MockRenderer2d.make () :> Renderer2d))
                  AudioPlayer = MockAudioPlayer.make () }

            // make the world's scripting environment
            let scriptingEnv = Scripting.Env.make ()

            // make the world's ambient state
            let ambientState =
                let overlayRouter = OverlayRouter.empty
                let symbolics = Symbolics.makeEmpty ()
                AmbientState.make config.Imperative config.NuConfig.StandAlone true symbolics Overlayer.empty overlayRouter None

            // make the world's quadtree
            let quadtree = World.makeQuadtree ()

            // make the world's octree
            let octree = World.makeOctree ()

            // make the world
            let world = World.make plugin eventDelegate dispatchers subsystems scriptingEnv ambientState quadtree octree (snd defaultGameDispatcher)

            // finally, register the game
            World.registerGame world

        /// Make a default world with a default screen, group, and entity, such as for testing.
        static member makeDefault () =
            let worldConfig = WorldConfig.defaultConfig
            let world = World.makeEmpty worldConfig
            let (screen, world) = World.createScreen (Some (nameof Screen)) world
            let (group, world) = World.createGroup (Some (nameof Group)) screen world
            let world = World.createEntity DefaultOverlay (Some [|nameof Entity|]) group world |> snd
            world

        /// Attempt to make the world, returning either a Right World on success, or a Left string
        /// (with an error message) on failure.
        static member tryMake (sdlDeps : SdlDeps) config (plugin : NuPlugin) =

            // ensure game engine is initialized
            Nu.init config.NuConfig

            // attempt to create asset graph
            match AssetGraph.tryMakeFromFile Assets.Global.AssetGraphFilePath with
            | Right assetGraph ->

                // populate metadata
                Metadata.generateMetadata config.Imperative assetGraph

                // make the world's event system
                let eventSystem =
                    let eventTracing = Constants.Engine.EventTracing
                    let eventTracerOpt = if eventTracing then Some (Log.remark "Event") else None
                    let eventFilter = Constants.Engine.EventFilter
                    let globalSimulant = Simulants.Game
                    let globalSimulantGeneralized = { GsgAddress = atoa globalSimulant.GameAddress }
                    let eventConfig = if config.Imperative then Imperative else Functional
                    EventSystemDelegate.make eventTracerOpt eventFilter globalSimulant globalSimulantGeneralized eventConfig
                    
                // make plug-in facets and dispatchers
                let pluginAssemblies = [|(plugin.GetType ()).Assembly|]
                let pluginFacets = plugin.Birth<Facet> pluginAssemblies
                let pluginEntityDispatchers = plugin.Birth<EntityDispatcher> pluginAssemblies
                let pluginGroupDispatchers = plugin.Birth<GroupDispatcher> pluginAssemblies
                let pluginScreenDispatchers = plugin.Birth<ScreenDispatcher> pluginAssemblies
                let pluginGameDispatchers = plugin.Birth<GameDispatcher> pluginAssemblies

                // make the default game dispatcher
                let defaultGameDispatcher = World.makeDefaultGameDispatcher ()

                // make the world's dispatchers
                let dispatchers =
                    { Facets = Map.addMany pluginFacets (World.makeDefaultFacets ())
                      EntityDispatchers = Map.addMany pluginEntityDispatchers (World.makeDefaultEntityDispatchers ())
                      GroupDispatchers = Map.addMany pluginGroupDispatchers (World.makeDefaultGroupDispatchers ())
                      ScreenDispatchers = Map.addMany pluginScreenDispatchers (World.makeDefaultScreenDispatchers ())
                      GameDispatchers = Map.addMany pluginGameDispatchers (Map.ofList [defaultGameDispatcher])
                      TryGetExtrinsic = World.tryGetExtrinsic
                      UpdateEntityInEntityTree = World.updateEntityInEntityTree
                      RebuildQuadtree = World.rebuildQuadtree
                      RebuildOctree = World.rebuildOctree }

                // get the first game dispatcher
                let activeGameDispatcher =
                    match Array.tryHead pluginGameDispatchers with
                    | Some (_, dispatcher) -> dispatcher
                    | None -> GameDispatcher ()

                // make the world's subsystems
                let subsystems =
                    let gravityScalar = Constants.Engine.Meter2d
                    let gravity = Constants.Engine.GravityDefault * gravityScalar
                    let physicsEngine2d = AetherPhysicsEngine.make config.Imperative gravity
                    let createRenderer3d =
                        fun config ->
                            match SdlDeps.getWindowOpt sdlDeps with
                            | Some window -> GlRenderer3d.make window config :> Renderer3d
                            | None -> MockRenderer3d.make () :> Renderer3d
                    let createRenderer2d =
                        fun config ->
                            match SdlDeps.getWindowOpt sdlDeps with
                            | Some window -> GlRenderer2d.make window config :> Renderer2d
                            | None -> MockRenderer2d.make () :> Renderer2d
                    let rendererProcess =
                        if config.NuConfig.StandAlone
                        then RendererThread (createRenderer3d, createRenderer2d) :> RendererProcess
                        else RendererInline (createRenderer3d, createRenderer2d) :> RendererProcess
                    rendererProcess.Start ()
                    rendererProcess.EnqueueMessage2d (LoadRenderPackageMessage2d Assets.Default.PackageName) // enqueue default package hint
                    let audioPlayer =
                        if SDL.SDL_WasInit SDL.SDL_INIT_AUDIO <> 0u
                        then SdlAudioPlayer.make () :> AudioPlayer
                        else MockAudioPlayer.make () :> AudioPlayer
                    audioPlayer.EnqueueMessage (LoadAudioPackageMessage Assets.Default.PackageName) // enqueue default package hint
                    { PhysicsEngine2d = physicsEngine2d
                      RendererProcess = rendererProcess
                      AudioPlayer = audioPlayer }

                // attempt to make the overlayer
                let intrinsicOverlays = World.makeIntrinsicOverlays dispatchers.Facets dispatchers.EntityDispatchers
                match Overlayer.tryMakeFromFile intrinsicOverlays Assets.Global.OverlayerFilePath with
                | Right overlayer ->

                    // make the world's scripting environment
                    let scriptingEnv = Scripting.Env.make ()

                    // make the world's ambient state
                    let ambientState =
                        let overlays = Overlayer.getIntrinsicOverlays overlayer @ Overlayer.getExtrinsicOverlays overlayer
                        let overlayRoutes =
                            overlays |>
                            List.map (fun overlay -> overlay.OverlaidTypeNames |> List.map (fun typeName -> (typeName, overlay.OverlayName))) |>
                            List.concat
                        let overlayRouter = OverlayRouter.make overlayRoutes
                        let symbolics = Symbolics.makeEmpty ()
                        AmbientState.make config.Imperative config.NuConfig.StandAlone config.Advancing symbolics overlayer overlayRouter (Some sdlDeps)

                    // make the world's quadtree
                    let quadtree = World.makeQuadtree ()

                    // make the world's octree
                    let octree = World.makeOctree ()

                    // make the world
                    let world = World.make plugin eventSystem dispatchers subsystems scriptingEnv ambientState quadtree octree activeGameDispatcher

                    // add the keyed values
                    let (kvps, world) = plugin.MakeKeyedValues world
                    let world = List.fold (fun world (key, value) -> World.addKeyedValue key value world) world kvps

                    // try to load the prelude for the scripting language
                    match World.tryEvalPrelude world with
                    | Right struct (_, world) ->

                        // register the game
                        let world = World.registerGame world

#if DEBUG
                        // attempt to hookup the console if debugging
                        let world = WorldConsole.tryHookUp world |> snd
#endif

                        // fin
                        Right world
                    
                    // forward error messages
                    | Left struct (error, _) -> Left error
                | Left error -> Left error
            | Left error -> Left error

        /// Run the game engine, returning exit code upon termination.
        static member run worldConfig plugin =
            match SdlDeps.tryMake worldConfig.SdlConfig with
            | Right sdlDeps ->
                use sdlDeps = sdlDeps // bind explicitly to dispose automatically
                match World.tryMake sdlDeps worldConfig plugin with
                | Right world -> World.run4 tautology sdlDeps Live world
                | Left error -> Log.trace error; Constants.Engine.ExitCodeFailure
            | Left error -> Log.trace error; Constants.Engine.ExitCodeFailure