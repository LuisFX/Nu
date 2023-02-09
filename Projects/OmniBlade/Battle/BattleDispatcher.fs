﻿// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module BattleDispatcher =

    type Hop =
        { HopStart : Vector3
          HopStop : Vector3 }
          
    type Positioning =
        | Position of Vector3
        | Center of Vector3
        | Bottom of Vector3

    type BattleMessage =
        | Update
        | InteractDialog
        | RegularItemSelect of CharacterIndex * string
        | RegularItemCancel of CharacterIndex
        | ConsumableItemSelect of CharacterIndex * string
        | ConsumableItemCancel of CharacterIndex
        | TechItemSelect of CharacterIndex * string
        | TechItemCancel of CharacterIndex
        | ReticlesSelect of CharacterIndex * CharacterIndex
        | ReticlesCancel of CharacterIndex
        | AttackCharacter1 of CharacterIndex
        | AttackCharacter2 of CharacterIndex * CharacterIndex
        | ConsumeCharacter1 of ConsumableType * CharacterIndex
        | ConsumeCharacter2 of ConsumableType * CharacterIndex
        | TechCharacter1 of CharacterIndex * CharacterIndex * TechType
        | TechCharacter2 of CharacterIndex * CharacterIndex * TechType
        | TechCharacter3 of CharacterIndex * CharacterIndex * TechType
        | TechCharacter4 of CharacterIndex * CharacterIndex * TechType
        | TechCharacter5 of CharacterIndex * CharacterIndex * TechType
        | TechCharacter6 of CharacterIndex * CharacterIndex * TechType
        | TechCharacterAmbient of CharacterIndex * CharacterIndex * TechType
        | Nop
        interface Message

    type BattleCommand =
        | UpdateEye
        | DisplayCancel of CharacterIndex
        | DisplayHitPointsChange of CharacterIndex * int
        | DisplayBolt of int64 * CharacterIndex
        | DisplayCycloneBlur of int64 * CharacterIndex * single
        | DisplayImpactSplash of int64 * CharacterIndex
        | DisplayCut of int64 * bool * CharacterIndex
        | DisplaySlashSpike of int64 * Vector3 * CharacterIndex
        | DisplayArcaneCast of int64 * CharacterIndex
        | DisplayFire of int64 * CharacterIndex * CharacterIndex
        | DisplayFlame of int64 * CharacterIndex * CharacterIndex
        | DisplayIce of int64 * CharacterIndex
        | DisplaySnowball of int64 * CharacterIndex
        | DisplayHolyCast of int64 * CharacterIndex
        | DisplayPurify of int64 * CharacterIndex
        | DisplayCure of int64 * CharacterIndex
        | DisplayProtect of int64 * CharacterIndex
        | DisplayDimensionalCast of int64 * CharacterIndex
        | DisplayBuff of int64 * StatusType * CharacterIndex
        | DisplayDebuff of int64 * StatusType * CharacterIndex
        | DisplayConjureIfrit of int64
        | DisplayHop of Hop
        | DisplayCircle of Vector3 * single
        | PlaySound of int64 * single * AssetTag<Sound>
        | PlaySong of GameTime * GameTime * GameTime * single * Song AssetTag
        | FadeOutSong of GameTime
        interface Command

    type Screen with
        member this.GetBattle world = this.GetModelGeneric<Battle> world
        member this.SetBattle value world = this.SetModelGeneric<Battle> value world
        member this.Battle = this.ModelGeneric<Battle> ()

    type BattleDispatcher () =
        inherit ScreenDispatcher<Battle, BattleMessage, BattleCommand> (Battle.empty)

        static let displayEffect (delay : int64) size positioning effect screen world =
            World.schedule (fun world ->
                let (entity, world) = World.createEntity<EffectDispatcher2d> DefaultOverlay None Simulants.BattleScene world
                let world = entity.SetEffect effect world
                let world = entity.SetSize size world
                let world =
                    match positioning with
                    | Position position -> entity.SetPosition position world
                    | Center center -> entity.SetCenter center world
                    | Bottom bottom -> entity.SetBottom bottom world
                let world = entity.SetElevation Constants.Battle.EffectElevation world
                entity.SetSelfDestruct true world)
                delay
                screen world

        static let advanceAttack sourceIndex (targetIndexOpt : CharacterIndex option) time localTime battle =
            match Battle.tryGetCharacter sourceIndex battle with
            | Some source when source.Healthy ->
                match targetIndexOpt with
                | Some targetIndex ->
                    match Battle.tryGetCharacter targetIndex battle with
                    | Some target ->
                        match localTime with
                        | 0L ->
                            if target.Healthy
                            then withSignal (AttackCharacter1 sourceIndex) battle
                            else just (Battle.abortCharacterAction time sourceIndex battle)
                        | 15L ->
                            withSignal (AttackCharacter2 (sourceIndex, targetIndex)) battle
                        | _ when localTime > 15L && Character.getAnimationFinished time target ->
                            let target = Battle.getCharacter targetIndex battle
                            if target.Healthy then
                                let battle = Battle.updateCurrentCommandOpt (constant None) battle
                                let battle = Battle.animationCharacterPoise time sourceIndex battle
                                let battle = Battle.animationCharacterPoise time targetIndex battle
                                let battle = Battle.finishCharacterAction sourceIndex battle
                                let battle =
                                    if  (match source.CharacterType with Enemy MadMinotaur -> false | _ -> true) && // HACK: disallow countering mad minotaurs since it nerfs challenge of first battle.
                                        Battle.shouldCounter sourceIndex targetIndex battle
                                    then Battle.counterAttack sourceIndex targetIndex battle
                                    else battle
                                just battle
                            else
                                let woundCommand = CurrentCommand.make time (ActionCommand.make Wound sourceIndex (Some targetIndex))
                                let battle = Battle.updateCurrentCommandOpt (constant (Some woundCommand)) battle
                                let battle = Battle.animationCharacterPoise time sourceIndex battle
                                let battle = Battle.finishCharacterAction sourceIndex battle
                                just battle
                        | _ -> just battle
                    | None -> just (Battle.abortCharacterAction time sourceIndex battle)
                | None -> just (Battle.abortCharacterAction time sourceIndex battle)
            | Some _ | None -> just (Battle.abortCharacterAction time sourceIndex battle)

        static let advanceDefend sourceIndex time localTime battle =
            match Battle.tryGetCharacter sourceIndex battle with
            | Some source when source.Healthy ->
                match localTime with
                | 0L ->
                    let battle =
                        battle |>
                        Battle.resetCharacterActionTime sourceIndex |>
                        Battle.resetCharacterInput sourceIndex |>
                        Battle.animateCharacter time (PoiseAnimation Defending) sourceIndex |>
                        Battle.defendCharacter sourceIndex
                    let battle = Battle.updateCurrentCommandOpt (constant None) battle
                    let battle = Battle.finishCharacterAction sourceIndex battle
                    just battle
                | _ -> just battle
            | Some _ | None ->
                let battle = Battle.updateCurrentCommandOpt (constant None) battle
                just battle

        static let advanceConsume consumable sourceIndex (targetIndexOpt : CharacterIndex option) time localTime battle =
            match Battle.tryGetCharacter sourceIndex battle with
            | Some source when source.Healthy ->
                match targetIndexOpt with
                | Some targetIndex ->
                    match Battle.tryGetCharacter targetIndex battle with
                    | Some target ->
                        match localTime with
                        | 0L ->
                            if target.Healthy || consumable = Revive // HACK: should really be checked ConsumableData.
                            then withSignal (ConsumeCharacter1 (consumable, sourceIndex)) battle
                            else just (Battle.abortCharacterAction time sourceIndex battle)
                        | 30L ->
                            withSignal (ConsumeCharacter2 (consumable, targetIndex)) battle
                        | _ when localTime > 30L && Character.getAnimationFinished time target ->
                            let battle = Battle.updateCurrentCommandOpt (constant None) battle
                            let battle = Battle.animationCharacterPoise time sourceIndex battle
                            let battle = Battle.animationCharacterPoise time targetIndex battle
                            let battle = Battle.finishCharacterAction sourceIndex battle
                            just battle
                        | _ -> just battle
                    | None -> just (Battle.abortCharacterAction time sourceIndex battle)
                | None -> just (Battle.abortCharacterAction time sourceIndex battle)
            | Some _ | None -> just (Battle.abortCharacterAction time sourceIndex battle)

        static let advanceTech techType sourceIndex (targetIndexOpt : CharacterIndex option) time localTime battle =
            match targetIndexOpt with
            | Some targetIndex ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target ->
                    match (Map.tryFind techType Data.Value.Techs,  Map.tryFind techType Data.Value.TechAnimations) with
                    | (Some techData, Some techAnimationData) ->
                        ignore techData // TODO: check for target.IsWounded case if techData is affecting wounded...
                        if target.Healthy then
                            let (sigs, battle) =
                                if localTime = techAnimationData.TechStart then withSignal (TechCharacter1 (sourceIndex, targetIndex, techType)) battle
                                elif localTime = techAnimationData.TechingStart then withSignal (TechCharacter2 (sourceIndex, targetIndex, techType)) battle
                                elif localTime = techAnimationData.AffectingStart then withSignal (TechCharacter3 (sourceIndex, targetIndex, techType)) battle
                                elif localTime = techAnimationData.AffectingStop then withSignal (TechCharacter4 (sourceIndex, targetIndex, techType)) battle
                                elif localTime = techAnimationData.TechingStop then withSignal (TechCharacter5 (sourceIndex, targetIndex, techType)) battle
                                elif localTime = techAnimationData.TechStop then withSignal (TechCharacter6 (sourceIndex, targetIndex, techType)) battle
                                else just battle
                            let (sigs, battle) = (sigs @ [TechCharacterAmbient (sourceIndex, targetIndex, techType)], battle)
                            withSignals sigs battle
                        else just (Battle.abortCharacterAction time sourceIndex battle)
                    | (_, _) -> just (Battle.abortCharacterAction time sourceIndex battle)
                | None -> just (Battle.abortCharacterAction time sourceIndex battle)
            | None -> just (Battle.abortCharacterAction time sourceIndex battle)

        static let rec advanceWound targetIndexOpt time battle =
            match targetIndexOpt with
            | Some targetIndex ->
                let character = Battle.getCharacter targetIndex battle
                let (sigs, battle) =
                    if character.Ally then
                        match character.CharacterAnimationType with
                        | DamageAnimation ->
                            if Character.getAnimationFinished time character then
                                let battle = Battle.updateCurrentCommandOpt (constant None) battle
                                let battle = Battle.animateCharacterWound time targetIndex battle
                                just battle
                            else just battle
                        | PoiseAnimation _ -> // allies don't have a wound animation state but rather return to poise state
                            let battle = Battle.updateCurrentCommandOpt (constant None) battle
                            let battle = Battle.animateCharacterWound time targetIndex battle
                            just battle
                        | _ -> failwithumf ()
                    else
                        match character.CharacterAnimationType with
                        | DamageAnimation ->
                            if Character.getAnimationFinished time character then
                                let battle = Battle.animateCharacterWound time targetIndex battle
                                let playDeathSound = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.BeastDeathSound)
                                withSignal playDeathSound battle
                            else just battle
                        | WoundAnimation ->
                            if Character.getAnimationFinished time character then
                                let battle = Battle.updateCurrentCommandOpt (constant None) battle
                                let battle =
                                    if targetIndex.Enemy
                                    then Battle.removeCharacter targetIndex battle
                                    else battle
                                just battle
                            else just battle
                        | _ -> failwithumf ()
                let (sigs, battle) =
                    match battle.CurrentCommandOpt with
                    | None ->
                        let allies = battle |> Battle.getAllies |> Map.toValueList
                        let enemies = battle |> Battle.getEnemies |> Map.toValueList
                        if List.forall (fun (character : Character) -> character.Wounded) allies then
                            // lost battle
                            let battle = Battle.animateCharactersCelebrate time false battle
                            let battle = Battle.updateBattleState (constant (BattleQuitting (time, false, Set.empty))) battle
                            let (sigs2, battle) = advanceBattle time battle
                            (sigs @ sigs2, battle)
                        elif List.isEmpty enemies then
                            // won battle
                            let battle = Battle.animateCharactersCelebrate time true battle
                            let battle = Battle.updateBattleState (constant (BattleResult (time, true))) battle
                            let (sigs2, battle) = advanceBattle time battle
                            (sigs @ sigs2, battle)
                        else (sigs, battle)
                    | Some _ -> (sigs, battle)
                withSignals sigs battle
            | None -> just battle

        and advanceReady time startTime (battle : Battle) =
            let localTime = time - startTime
            let readyTime = localTime - 90L
            if localTime = inc 63L then // first frame after transitioning in
                match battle.BattleSongOpt with
                | Some battleSong -> withSignal (PlaySong (0L, Constants.Audio.FadeOutTimeDefault, 0L, Constants.Audio.SongVolumeDefault, battleSong)) battle
                | None -> just battle
            elif localTime >= 90L && localTime < 160L then
                let battle = Battle.animateCharactersReady time battle
                if readyTime = 30L
                then withSignal (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.UnsheatheSound)) battle
                else just battle
            elif localTime = 160L then
                let battle = Battle.updateBattleState (constant BattleRunning) battle
                let battle = Battle.animateCharactersPoised time battle
                let battle = Battle.populateAllyConjureCharges battle
                let battle = Battle.autoBattleEnemies battle
                just battle
            else just battle

        and advanceCurrentCommand time currentCommand battle =
            let localTime = time - currentCommand.StartTime
            match currentCommand.ActionCommand.Action with
            | Attack ->
                let source = currentCommand.ActionCommand.Source
                let targetOpt = currentCommand.ActionCommand.TargetOpt
                advanceAttack source targetOpt time localTime battle
            | Defend ->
                let source = currentCommand.ActionCommand.Source
                advanceDefend source time localTime battle
            | Tech techType ->
                let source = currentCommand.ActionCommand.Source
                let targetOpt = currentCommand.ActionCommand.TargetOpt
                advanceTech techType source targetOpt time localTime battle
            | Consume consumable ->
                let source = currentCommand.ActionCommand.Source
                let targetOpt = currentCommand.ActionCommand.TargetOpt
                advanceConsume consumable source targetOpt time localTime battle
            | Wound ->
                let targetOpt = currentCommand.ActionCommand.TargetOpt
                advanceWound targetOpt time battle

        and advanceNextCommand time nextCommand futureCommands battle =
            let command = CurrentCommand.make time nextCommand
            let sourceIndex = command.ActionCommand.Source
            let targetIndexOpt = command.ActionCommand.TargetOpt
            let source = Battle.getCharacter sourceIndex battle
            let battle =
                match command.ActionCommand.Action with
                | Attack | Defend ->
                    if source.Healthy && not (Map.containsKey Sleep source.Statuses) then
                        let targetIndexOpt = Battle.tryRetargetIfNeeded false targetIndexOpt battle
                        let command = { command with ActionCommand = { command.ActionCommand with TargetOpt = targetIndexOpt }}
                        Battle.updateCurrentCommandOpt (constant (Some command)) battle
                    else battle
                | Consume consumableType ->
                    match Data.Value.Consumables.TryGetValue consumableType with
                    | (true, consumable) ->
                        if source.Healthy && not (Map.containsKey Sleep source.Statuses) then
                            let targetIndexOpt = Battle.tryRetargetIfNeeded consumable.Revive targetIndexOpt battle
                            let command = { command with ActionCommand = { command.ActionCommand with TargetOpt = targetIndexOpt }}
                            Battle.updateCurrentCommandOpt (constant (Some command)) battle
                        else battle
                    | (false, _) -> battle
                | Tech techType ->
                    match Data.Value.Techs.TryGetValue techType with
                    | (true, _) ->
                        if source.Healthy && not (Map.containsKey Sleep source.Statuses) && not (Map.containsKey Silence source.Statuses) then
                            let targetIndexOpt = Battle.tryRetargetIfNeeded false targetIndexOpt battle // TODO: consider affecting wounded.
                            let command = { command with ActionCommand = { command.ActionCommand with TargetOpt = targetIndexOpt }}
                            Battle.updateCurrentCommandOpt (constant (Some command)) battle
                        else battle
                    | (false, _) -> battle
                | Wound ->
                    Battle.updateCurrentCommandOpt (constant (Some command)) battle
            let battle = Battle.updateActionCommands (constant futureCommands) battle
            advanceBattle time battle

        and advanceNoNextCommand (_ : int64) battle =
            let (allySignalsRev, battle) =
                Map.fold (fun (signals : Signal list, battle) allyIndex (ally : Character) ->
                    if  ally.ActionTime >= Constants.Battle.ActionTime &&
                        ally.CharacterInputState = NoInput then
                        let battle = Battle.updateCharacterInputState (constant RegularMenu) allyIndex battle
                        let playReadySound = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.ReadySound)
                        (signal playReadySound :: signals, battle)
                    else (signals, battle))
                    (just battle)
                    (Battle.getAllies battle)
            let battle =
                Map.fold (fun battle enemyIndex (enemy : Character) ->
                    if  enemy.ActionTime >= Constants.Battle.ActionTime &&
                        not (Battle.characterAppendedActionCommand enemyIndex battle) then
                        let battle =
                            match enemy.AutoBattleOpt with
                            | Some autoBattle ->
                                let actionCommand =
                                    match autoBattle.AutoTechOpt with
                                    | Some tech -> { Action = Tech tech; Source = enemyIndex; TargetOpt = Some autoBattle.AutoTarget }
                                    | None -> { Action = Attack; Source = enemyIndex; TargetOpt = Some autoBattle.AutoTarget }
                                Battle.appendActionCommand actionCommand battle
                            | None -> battle    
                        let battle = Battle.resetCharacterActionTime enemyIndex battle
                        let battle = Battle.resetCharacterInput enemyIndex battle
                        battle
                    else battle)
                    battle
                    (Battle.getEnemies battle)
            let battle =
                Battle.updateCharacters (fun character ->
                    let actionTimeDelta =
                        if character.Ally || battle.BattleSpeed = WaitSpeed
                        then Constants.Battle.AllyActionTimeDelta
                        else Constants.Battle.EnemyActionTimeDelta
                    let actionTimeDelta =
                        if Map.containsKey (Time false) character.Statuses then
                            let slowScalar =
                                if character.Ally then Constants.Battle.ActionTimeSlowScalar
                                elif character.Boss then Constants.Battle.ActionTimeSlowerScalar
                                else Constants.Battle.ActionTimeSlowestScalar
                            actionTimeDelta * slowScalar
                        elif Map.containsKey (Time true) character.Statuses then actionTimeDelta * Constants.Battle.ActionTimeHasteScalar
                        else actionTimeDelta
                    let actionTimeDelta =
                        let anyAlliesInputting = Battle.getAlliesHealthy battle |> Map.toValueList |> List.exists (fun ally -> ally.CharacterInputState <> CharacterInputState.NoInput)
                        if anyAlliesInputting then
                            match battle.BattleSpeed with
                            | SwiftSpeed -> actionTimeDelta * Constants.Battle.SwiftSpeedScalar
                            | PacedSpeed -> actionTimeDelta * Constants.Battle.PacedSpeedScalar
                            | WaitSpeed -> 0.0f
                        else actionTimeDelta * 1.0f
                    let poisoning =
                        let actionTime = character.ActionTime + actionTimeDelta
                        Map.containsKey Poison character.Statuses &&
                        character.ActionTime % 500.0f < 250.0f &&
                        actionTime % 500.0f >= 250.0f
                    let character =
                        if character.Healthy && not (Map.containsKey Sleep character.Statuses)
                        then Character.updateActionTime ((+) actionTimeDelta) character
                        else character
                    let character =
                        if character.Healthy
                        then Character.burndownStatuses actionTimeDelta character
                        else character
                    let character =
                        if character.Healthy && poisoning then
                            let poisonDrainRate =
                                if character.Ally then Constants.Battle.PoisonDrainRateMedium
                                elif character.Boss then Constants.Battle.PoisonDrainRateSlow
                                else Constants.Battle.PoisonDrainRateFast
                            let damage = single character.HitPointsMax * poisonDrainRate |> max 1.0f |> int
                            let alliesHealthy = Battle.getAlliesHealthy battle
                            Character.updateHitPoints (fun hp -> (false, max 1 (hp - damage))) false alliesHealthy character
                        else character
                    let character =
                        if character.Healthy && Character.readyForAutoBattle character then
                            let alliesHealthy = Battle.getAlliesHealthy battle
                            let alliesWounded = Battle.getAlliesWounded battle
                            let enemiesHealthy = Battle.getEnemiesHealthy battle
                            let enemiesWounded = Battle.getEnemiesWounded battle
                            Character.autoBattle alliesHealthy alliesWounded enemiesHealthy enemiesWounded character
                        else character
                    character)
                    battle
            withSignals (List.rev allySignalsRev) battle

        and advanceNoCurrentCommand time (battle : Battle) =
            match battle.ActionCommands with
            | Queue.Cons (nextCommand, futureCommands) -> advanceNextCommand time nextCommand futureCommands battle
            | Queue.Nil -> advanceNoNextCommand time battle

        and advanceRunning time (battle : Battle) =
            match battle.CurrentCommandOpt with
            | Some currentCommand -> advanceCurrentCommand time currentCommand battle
            | None -> advanceNoCurrentCommand time battle

        and advanceResults time startTime outcome (battle : Battle) =
            let localTime = time - startTime
            if localTime = 0L then
                let alliesLevelingUp =
                    battle |> Battle.getAllies |> Map.toValueList |>
                    List.filter (fun ally -> ally.HitPoints > 0) |>
                    List.filter (fun ally -> Algorithms.expPointsRemainingForNextLevel ally.ExpPoints <= battle.PrizePool.Exp)
                let textA =
                    match alliesLevelingUp with
                    | _ :: _ -> "Level up for " + (alliesLevelingUp |> List.map (fun c -> c.Name) |> String.join ", ") + "!^"
                    | [] -> "Enemies defeated!^"
                let textB =
                    alliesLevelingUp |>
                    List.choose (fun ally ->
                        let techs = Algorithms.expPointsToTechs3 ally.ExpPoints battle.PrizePool.Exp ally.ArchetypeType
                        if Set.notEmpty techs then Some (ally, techs) else None) |>
                    List.map (fun (ally, techs) ->
                        let text = techs |> Set.toList |> List.map scstring |> String.join ", "
                        ally.Name + " learned " + text + "!") |>
                    function
                    | _ :: _ as texts -> String.join "\n" texts + "^"
                    | [] -> ""
                let textC = "Gained " + string battle.PrizePool.Exp + " Exp!\nGained " + string battle.PrizePool.Gold + " Gold!"
                let textD =
                    match battle.PrizePool.Items with
                    | _ :: _ as items -> "^Found " + (items |> List.map (fun i -> ItemType.getName i) |> String.join ", ") + "!"
                    | [] -> ""
                let text = textA + textB + textC + textD
                let dialog = { DialogForm = DialogThick; DialogTokenized = text; DialogProgress = 0; DialogPage = 0; DialogPromptOpt = None; DialogBattleOpt = None }
                let battle = Battle.updateDialogOpt (constant (Some dialog)) battle
                let (sigs, battle) =
                    if outcome then
                        let battle = Battle.updateAllies (fun ally -> if ally.Healthy then Character.updateExpPoints ((+) battle.PrizePool.Exp) ally else ally) battle
                        let battle = Battle.updateInventory (fun inv -> { inv with Gold = inv.Gold + battle.PrizePool.Gold }) battle
                        let battle = Battle.updateInventory (Inventory.tryAddItems battle.PrizePool.Items >> snd) battle
                        if List.notEmpty alliesLevelingUp
                        then withSignal (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.GrowthSound)) battle
                        else just battle
                    else just battle
                (signal (FadeOutSong 360L) :: sigs, battle)
            else
                match battle.DialogOpt with
                | None -> just (Battle.updateBattleState (constant (BattleQuitting (time, outcome, battle.PrizePool.Consequents))) battle)
                | Some _ -> just battle

        and advanceCease time startTime battle =
            let localTime = time - startTime
            if localTime = 0L
            then withSignal (FadeOutSong Constants.Audio.FadeOutTimeDefault) battle
            else just battle

        and advanceBattle time (battle : Battle) : Signal list * Battle =
            match battle.BattleState with
            | BattleReady startTime -> advanceReady time startTime battle
            | BattleRunning -> advanceRunning time battle
            | BattleResult (startTime, outcome) -> advanceResults time startTime outcome battle
            | BattleQuitting (startTime, _, _) -> advanceCease time startTime battle
            | BattleQuit -> just battle

        override this.Initialize (_, _) =
            [Screen.UpdateEvent => Update
             Screen.PostUpdateEvent => UpdateEye]

        override this.Message (battle, message, _, world) =

            match message with
            | Update ->

                // advance battle
                let (signals, battle) = 
                    if World.getAdvancing world
                    then advanceBattle (World.getUpdateTime world) battle
                    else just battle

                // advance dialog
                let battle =
                    match battle.DialogOpt with
                    | Some dialog ->
                        let dialog = Dialog.advance id dialog world
                        Battle.updateDialogOpt (constant (Some dialog)) battle
                    | None -> battle

                // fin
                (signals, battle)

            | InteractDialog ->
                match battle.DialogOpt with
                | Some dialog ->
                    match Dialog.tryAdvance id dialog with
                    | (true, dialog) ->
                        let battle = Battle.updateDialogOpt (constant (Some dialog)) battle
                        just battle
                    | (false, _) ->
                        let battle = Battle.updateDialogOpt (constant None) battle
                        just battle
                | None -> just battle

            | RegularItemSelect (characterIndex, item) ->
                let battle =
                    match item with
                    | "Attack" ->
                        battle |>
                        Battle.updateCharacterInputState (constant (AimReticles (item, EnemyAim true))) characterIndex |>
                        Battle.undefendCharacter characterIndex
                    | "Defend" ->
                        let battle = Battle.updateCharacterInputState (constant NoInput) characterIndex battle
                        let command = ActionCommand.make Defend characterIndex None
                        let battle = Battle.appendActionCommand command battle
                        battle
                    | "Tech" ->
                        battle |>
                        Battle.updateCharacterInputState (constant TechMenu) characterIndex |>
                        Battle.undefendCharacter characterIndex
                    | "Consumable" ->
                        battle |>
                        Battle.updateCharacterInputState (constant ItemMenu) characterIndex |>
                        Battle.undefendCharacter characterIndex
                    | _ -> failwithumf ()
                just battle
            
            | RegularItemCancel characterIndex ->
                let battle = Battle.updateCharacterInputState (constant RegularMenu) characterIndex battle
                just battle
            
            | ConsumableItemSelect (characterIndex, item) ->
                let consumableType =
                    scvalue<ConsumableType> item
                let aimType =
                    match Data.Value.Consumables.TryGetValue consumableType with
                    | (true, consumableData) -> consumableData.AimType
                    | (false, _) -> NoAim
                let battle = Battle.updateCharacterInputState (constant (AimReticles (item, aimType))) characterIndex battle
                just battle

            | ConsumableItemCancel characterIndex ->
                let battle = Battle.updateCharacterInputState (constant RegularMenu) characterIndex battle
                just battle
            
            | TechItemSelect (characterIndex, item) ->
                let techType =
                    scvalue<TechType> item
                let aimType =
                    match Data.Value.Techs.TryGetValue techType with
                    | (true, techData) -> techData.AimType
                    | (false, _) -> NoAim
                let battle = Battle.updateCharacterInputState (constant (AimReticles (item, aimType))) characterIndex battle
                just battle
            
            | TechItemCancel characterIndex ->
                let battle = Battle.updateCharacterInputState (constant RegularMenu) characterIndex battle
                just battle

            | ReticlesSelect (sourceIndex, targetIndex) ->
                match battle.BattleState with
                | BattleRunning ->
                    let battle = Battle.confirmCharacterInput sourceIndex targetIndex battle
                    let battle = Battle.resetCharacterActionTime sourceIndex battle
                    let battle = Battle.resetCharacterInput sourceIndex battle
                    just battle
                | _ -> just battle

            | ReticlesCancel characterIndex ->
                let battle = Battle.cancelCharacterInput characterIndex battle
                just battle

            | AttackCharacter1 sourceIndex ->
                let time = World.getUpdateTime world
                let battle = Battle.animateCharacter time AttackAnimation sourceIndex battle
                let playHit = PlaySound (15L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                withSignal playHit battle

            | AttackCharacter2 (sourceIndex, targetIndex) ->
                let time = World.getUpdateTime world
                let damage = Battle.evalAttack Physical sourceIndex targetIndex battle
                let battle = Battle.updateCharacterHitPoints false false -damage targetIndex battle
                let battle = Battle.animateCharacter time DamageAnimation targetIndex battle
                let battle =
                    if Battle.getCharacterWounded targetIndex battle then
                        let battle = Battle.halveCharacterActionTime targetIndex battle
                        Battle.resetCharacterInput targetIndex battle
                    else battle
                withSignal (DisplayHitPointsChange (targetIndex, -damage)) battle

            | ConsumeCharacter1 (consumable, sourceIndex) ->
                let time = World.getUpdateTime world
                let battle = Battle.animateCharacter time CastAnimation sourceIndex battle
                let battle = Battle.updateInventory (Inventory.tryRemoveItem (Consumable consumable) >> snd) battle
                just battle

            | ConsumeCharacter2 (consumableType, targetIndex) ->
                let time = World.getUpdateTime world
                match Data.Value.Consumables.TryGetValue consumableType with
                | (true, consumableData) ->
                    if consumableData.Curative then
                        let healing = int consumableData.Scalar
                        let battle =
                            if consumableData.Techative
                            then Battle.updateCharacterTechPoints healing targetIndex battle
                            else Battle.updateCharacterHitPoints false consumableData.Revive healing targetIndex battle
                        let battle = Battle.applyCharacterStatuses consumableData.StatusesAdded consumableData.StatusesRemoved targetIndex battle
                        let battle = Battle.animateCharacter time SpinAnimation targetIndex battle
                        let displayHitPointsChange = DisplayHitPointsChange (targetIndex, healing)
                        let playHealSound = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.HealSound)
                        withSignals [displayHitPointsChange; playHealSound] battle
                    else
                        // TODO: non-curative case
                        just battle
                | (false, _) -> just battle

            | TechCharacter1 (sourceIndex, targetIndex, techType) ->
                let time = World.getUpdateTime world
                let sourcePerimeter = Battle.getCharacterPerimeter sourceIndex battle
                let targetPerimeter = Battle.getCharacterPerimeter targetIndex battle
                let effectOpt =
                    match techType with
                    | Critical | HeavyCritical | PoisonCut | PowerCut | DispelCut | DoubleCut ->
                        let hopDirection = Direction.ofVector3 (v3 (targetPerimeter.Bottom.X - sourcePerimeter.Bottom.X) 0.0f 0.0f)
                        let hopStop = targetPerimeter.Bottom - Direction.toVector3 hopDirection * Constants.Battle.StrikingDistance
                        Left (DisplayHop { HopStart = sourcePerimeter.Bottom; HopStop = hopStop } |> signal)
                    | Cyclone ->
                        Left (DisplayHop { HopStart = sourcePerimeter.Bottom; HopStop = targetPerimeter.Bottom + Constants.Battle.CharacterBottomOffset3 } |> signal)
                    | _ ->
                        match Battle.getCharacterArchetypeType sourceIndex battle with
                        | Cleric ->
                            let playCharge = PlaySound (0L, Constants.Audio.SongVolumeDefault, Assets.Field.ChargeHolySound)
                            let displayCast = DisplayHolyCast (0L, sourceIndex)
                            Right [signal playCharge; signal displayCast]
                        | Wizard ->
                            let playCharge = PlaySound (0L, Constants.Audio.SongVolumeDefault, Assets.Field.ChargeDimensionSound)
                            let displayCast = DisplayArcaneCast (0L, sourceIndex)
                            Right [playCharge; displayCast]
                        | _ ->
                            let playCharge = PlaySound (0L, Constants.Audio.SongVolumeDefault, Assets.Field.ChargeDimensionSound)
                            let displayCast = DisplayDimensionalCast (0L, sourceIndex)
                            Right [playCharge; displayCast]
                match effectOpt with
                | Left hopEffect ->
                    let battle = Battle.animateCharacter time (PoiseAnimation Poising) sourceIndex battle
                    withSignal hopEffect battle
                | Right chargeEffects ->
                    if Battle.getCharacterHealthy targetIndex battle then
                        let battle = Battle.animateCharacter time (PoiseAnimation Charging) sourceIndex battle
                        withSignals chargeEffects battle
                    else just (Battle.abortCharacterAction time sourceIndex battle)

            | TechCharacter2 (sourceIndex, targetIndex, techType) ->
                match techType with
                | Critical ->
                    let time = World.getUpdateTime world
                    let playHit = PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                    let impactSplash = DisplayImpactSplash (30L, targetIndex)
                    let battle = Battle.animateCharacter time AttackAnimation sourceIndex battle
                    withSignals [playHit; impactSplash] battle
                | Cyclone ->
                    let time = World.getUpdateTime world
                    let radius = 64.0f
                    let perimeter = Battle.getCharacterPerimeter sourceIndex battle
                    let position = perimeter.Bottom
                    let playHits =
                        [PlaySound (20L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound) |> signal
                         PlaySound (40L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound) |> signal
                         PlaySound (60L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound) |> signal
                         PlaySound (80L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound) |> signal]
                    let battle = Battle.animateCharacter time WhirlAnimation sourceIndex battle
                    let sigs = signal (DisplayCircle (position, radius)) :: signal (DisplayCycloneBlur (0L, sourceIndex, radius)) :: playHits
                    withSignals sigs battle
                | HeavyCritical ->
                    let time = World.getUpdateTime world
                    let playHit = PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                    let impactSplash = DisplayImpactSplash (30L, targetIndex) // TODO: darker impact splash to represent element.
                    let battle = Battle.animateCharacter time AttackAnimation sourceIndex battle
                    withSignals [playHit; impactSplash] battle
                | Slash ->
                    let time = World.getUpdateTime world
                    let playSlash = PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.SlashSound)
                    let playHit = PlaySound (60L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                    let perimeter = Battle.getCharacterPerimeter sourceIndex battle
                    let slashSpike = DisplaySlashSpike (10L, perimeter.Bottom, targetIndex)
                    let impactSplashes = Battle.evalTech sourceIndex targetIndex techType battle |> Triple.thd |> Map.toKeyList |> List.map (fun targetIndex -> DisplayImpactSplash (70L, targetIndex) |> signal)
                    let battle = Battle.animateCharacter time SlashAnimation sourceIndex battle
                    withSignals (signal playSlash :: signal playHit :: signal slashSpike :: impactSplashes) battle
                | PowerCut ->
                    let time = World.getUpdateTime world
                    let playHit = PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                    let cut = DisplayCut (30L, false, targetIndex)
                    let battle = Battle.animateCharacter time AttackAnimation sourceIndex battle
                    withSignals [playHit; cut] battle
                | PoisonCut ->
                    let time = World.getUpdateTime world
                    let playHit = PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                    let cut = DisplayCut (30L, false, targetIndex)
                    let battle = Battle.animateCharacter time AttackAnimation sourceIndex battle
                    withSignals [playHit; cut] battle
                | DoubleCut ->
                    let time = World.getUpdateTime world
                    let playHit = PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                    let cut = DisplayCut (30L, false, targetIndex)
                    let battle = Battle.animateCharacter time AttackAnimation sourceIndex battle
                    withSignals [playHit; cut] battle
                | DispelCut ->
                    let time = World.getUpdateTime world
                    let playHit = PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.HitSound)
                    let displayCut = DisplayCut (30L, true, targetIndex)
                    let battle = Battle.animateCharacter time AttackAnimation sourceIndex battle
                    withSignals [playHit; displayCut] battle
                | Fire ->
                    let time = World.getUpdateTime world
                    let playFire = PlaySound (60L, Constants.Audio.SoundVolumeDefault, Assets.Field.FireSound)
                    let displayFire = DisplayFire (0L, sourceIndex, targetIndex)
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    withSignals [playFire; displayFire] battle
                | TechType.Flame ->
                    let time = World.getUpdateTime world
                    let playFlame = PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.FlameSound)
                    let displayFlame = DisplayFlame (0L, sourceIndex, targetIndex)
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    withSignals [playFlame; displayFlame] battle
                | Ice ->
                    let time = World.getUpdateTime world
                    let playIce = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.IceSound)
                    let displayIce = DisplayIce (0L, targetIndex)
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    withSignals [playIce; displayIce] battle
                | Snowball ->
                    let time = World.getUpdateTime world
                    let playSnowball = PlaySound (15L, Constants.Audio.SoundVolumeDefault, Assets.Field.SnowballSound)
                    let displaySnowball = DisplaySnowball (0L, targetIndex)
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    withSignals [playSnowball; displaySnowball] battle
                | Stone ->
                    let time = World.getUpdateTime world
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    withSignal (DisplayIce (0L, targetIndex)) battle // TODO: use new sound and effect.
                | Quake ->
                    let time = World.getUpdateTime world
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    withSignal (DisplayBolt (0L, targetIndex)) battle // TODO: use new sound and effect.
                | Cure ->
                    let time = World.getUpdateTime world
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    let playCure = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.CureSound)
                    let displayCures = Battle.evalTech sourceIndex targetIndex techType battle |> Triple.thd |> Map.toKeyList |> List.map (fun targetIndex -> DisplayCure (0L, targetIndex) |> signal)
                    withSignals (signal playCure :: displayCures) battle
                | Empower ->
                    let time = World.getUpdateTime world
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    let playBuff = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.BuffSound)
                    let displayBuff = DisplayBuff (0L, Power (true, true), targetIndex)
                    withSignals [playBuff; displayBuff] battle
                | Aura ->
                    let time = World.getUpdateTime world
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    let playCure = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.CureSound)
                    let displayCures = Battle.evalTech sourceIndex targetIndex techType battle |> Triple.thd |> Map.toKeyList |> List.map (fun targetIndex -> DisplayCure (0L, targetIndex) |> signal)
                    withSignals (signal playCure :: displayCures) battle
                | Enlighten ->
                    let time = World.getUpdateTime world
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    let playBuff = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.BuffSound)
                    let displayBuff = DisplayBuff (0L, Magic (true, true), targetIndex)
                    withSignals [playBuff; displayBuff] battle
                | Protect ->
                    let time = World.getUpdateTime world
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    let playBuff = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.BuffSound)
                    let displayBuff = DisplayBuff (0L, Shield (true, true), targetIndex)
                    withSignals [playBuff; displayBuff] battle
                | Muddle ->
                    let time = World.getUpdateTime world
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    let playDebuff = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.DebuffSound)
                    let displayDebuff = DisplayDebuff (0L, Magic (false, false), targetIndex)
                    withSignals [playDebuff; displayDebuff] battle
                | Weaken ->
                    let time = World.getUpdateTime world
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    let playDebuff = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.DebuffSound)
                    let displayDebuff = DisplayDebuff (0L, Power (false, false), targetIndex)
                    withSignals [playDebuff; displayDebuff] battle
                | Slow ->
                    let time = World.getUpdateTime world
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    let playDebuff = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.DebuffSound)
                    let displayDebuff = DisplayDebuff (0L, Time false, targetIndex)
                    withSignals [playDebuff; displayDebuff] battle
                | Bolt ->
                    let time = World.getUpdateTime world
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    let playSound = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Field.ExplosionSound)
                    let displayBolt = DisplayBolt (0L, targetIndex)
                    withSignals [playSound; displayBolt] battle
                | ConjureIfrit ->
                    let time = World.getUpdateTime world
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    let playIfrit = PlaySound (10L, Constants.Audio.SoundVolumeDefault, Assets.Field.IfritSound)
                    let displayConjureIfrit = DisplayConjureIfrit 0L
                    withSignals [playIfrit; displayConjureIfrit] battle
                | Purify ->
                    let time = World.getUpdateTime world
                    let battle = Battle.animateCharacter time Cast2Animation sourceIndex battle
                    withSignal (DisplayPurify (0L, targetIndex)) battle // TODO: use new sound and effect.

            | TechCharacter3 (sourceIndex, targetIndex, techType) ->
                let time = World.getUpdateTime world
                let (_, spawnOpt, results) = Battle.evalTech sourceIndex targetIndex techType battle
                let (battle, sigs) =
                    Map.fold (fun (battle, sigs) characterIndex (cancelled, _, hitPointsChange, _, _) ->
                        if hitPointsChange < 0 && Battle.getCharacterHealthy characterIndex battle then
                            let battle = Battle.animateCharacter time DamageAnimation characterIndex battle
                            let sigs = if cancelled then signal (DisplayCancel characterIndex) :: sigs else sigs
                            (battle, sigs)
                        else (battle, sigs))
                        (battle, [])
                        results
                let battle =
                    match spawnOpt with
                    | Some spawn -> Battle.spawnEnemies time spawn battle
                    | _ -> battle
                withSignals sigs battle

            | TechCharacter4 (sourceIndex, targetIndex, techType) ->
                let results = Battle.evalTech sourceIndex targetIndex techType battle |> Triple.thd
                let (battle, sigs) =
                    Map.fold (fun (battle, sigs) _ (_, _, _, _, _) ->
                        // TODO: glow effect
                        (battle, sigs))
                        (battle, [])
                        results
                withSignals sigs battle

            | TechCharacter5 (sourceIndex, targetIndex, techType) ->
                let sourcePerimeterOriginal = Battle.getCharacterPerimeterOriginal sourceIndex battle
                let targetPerimeter = Battle.getCharacterPerimeter targetIndex battle
                let hopOpt =
                    match techType with
                    | Critical | HeavyCritical | PoisonCut | PowerCut | DispelCut | DoubleCut ->
                        let hopDirection = Direction.ofVector3 (targetPerimeter.Bottom - sourcePerimeterOriginal.Bottom)
                        let hopStart = targetPerimeter.Bottom - Direction.toVector3 hopDirection * Constants.Battle.StrikingDistance
                        Some
                            { HopStart = hopStart
                              HopStop = sourcePerimeterOriginal.Bottom }
                    | Cyclone ->
                        Some
                            { HopStart = targetPerimeter.Bottom + Constants.Battle.CharacterBottomOffset3
                              HopStop = sourcePerimeterOriginal.Bottom }
                    | _ -> None
                match hopOpt with
                | Some hop -> withSignal (DisplayHop hop) battle
                | None -> just battle

            | TechCharacter6 (sourceIndex, targetIndex, techType) ->
                let time = World.getUpdateTime world
                let (techCost, _, results) = Battle.evalTech sourceIndex targetIndex techType battle
                let (battle, sigs) =
                    Map.fold (fun (battle, sigs) characterIndex (cancelled, affectsWounded, hitPointsChange, added, removed) ->
                        let battle = Battle.updateCharacterHitPoints cancelled affectsWounded hitPointsChange characterIndex battle
                        let randomizer = if sourceIndex.Ally then StatusType.randomizeStrong else StatusType.randomizeWeak
                        let added = added |> Set.toSeq |> Seq.filter randomizer |> Set.ofSeq
                        let battle = Battle.applyCharacterStatuses added removed characterIndex battle
                        let wounded = Battle.getCharacterWounded characterIndex battle
                        let battle =
                            if wounded then
                                let battle = Battle.halveCharacterActionTime characterIndex battle
                                Battle.resetCharacterInput characterIndex battle
                            else battle
                        let sigs = if hitPointsChange <> 0 then signal (DisplayHitPointsChange (characterIndex, hitPointsChange)) :: sigs else sigs
                        let (battle, sigs) =
                            if wounded then
                                let woundCommand = ActionCommand.make Wound sourceIndex (Some characterIndex)
                                let battle = Battle.prependActionCommand woundCommand battle
                                (battle, sigs)
                            else
                                let time = World.getUpdateTime world
                                let battle = Battle.animationCharacterPoise time characterIndex battle
                                (battle, sigs)
                        (battle, sigs))
                        (battle, [])
                        results
                let battle = Battle.updateCharacterTechPoints -techCost sourceIndex battle
                let battle = Battle.updateCurrentCommandOpt (constant None) battle
                let battle = Battle.animationCharacterPoise time sourceIndex battle
                let battle = Battle.resetCharacterTechCharge sourceIndex battle
                let battle =
                    if techType.ConjureTech
                    then Battle.resetCharacterConjureCharge sourceIndex battle
                    else battle
                let battle = Battle.finishCharacterAction sourceIndex battle
                let battle =
                    if Battle.shouldCounter sourceIndex targetIndex battle
                    then Battle.counterAttack sourceIndex targetIndex battle
                    else battle
                withSignals sigs battle

            | TechCharacterAmbient (sourceIndex, _, _) ->
                if Simulants.BattleSceneRide.Exists world then
                    let battle =
                        let tags = Simulants.BattleSceneRide.GetEffectTags world
                        match Map.tryFind "Tag" tags with
                        | Some tag -> Battle.updateCharacterBottom (constant tag.Position) sourceIndex battle
                        | None -> battle
                    just battle
                else just battle

            | Nop -> just battle

        override this.Command (battle, command, screen, world) =

            match command with
            | UpdateEye ->
                let world = World.setEyeCenter2d v2Zero world
                just world
            
            | DisplayHop hop ->
                let effect = Effects.makeHopEffect hop.HopStart hop.HopStop
                let (entity, world) = World.createEntity<EffectDispatcher2d> DefaultOverlay (Some Simulants.BattleSceneRide.Surnames) Simulants.BattleScene world
                let world = entity.SetEffect effect world
                let world = entity.SetEffectOffset v3Zero world
                let world = entity.SetSelfDestruct true world
                just world

            | DisplayCircle (position, radius) ->
                let effect = Effects.makeCircleEffect radius
                let (entity, world) = World.createEntity<EffectDispatcher2d> DefaultOverlay (Some Simulants.BattleSceneRide.Surnames) Simulants.BattleScene world
                let world = entity.SetPosition position world
                let world = entity.SetEffect effect world
                let world = entity.SetSelfDestruct true world
                just world

            | DisplayCancel targetIndex ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target ->
                    let effect = Effects.makeCancelEffect ()
                    let (entity, world) = World.createEntity<EffectDispatcher2d> DefaultOverlay None Simulants.BattleScene world
                    let world = entity.SetPosition target.CenterOffset4 world
                    let world = entity.SetEffect effect world
                    let world = entity.SetElevation (Constants.Battle.GuiEffectElevation + 1.0f) world
                    let world = entity.SetSelfDestruct true world
                    just world
                | None -> just world

            | DisplayHitPointsChange (targetIndex, delta) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target ->
                    let effect = Effects.makeHitPointsChangeEffect delta
                    let (entity, world) = World.createEntity<EffectDispatcher2d> DefaultOverlay None Simulants.BattleScene world
                    let world = entity.SetPosition target.BottomOriginalOffset4 world
                    let world = entity.SetEffect effect world
                    let world = entity.SetElevation Constants.Battle.GuiEffectElevation world
                    let world = entity.SetSelfDestruct true world
                    just world
                | None -> just world

            | DisplayBolt (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 192.0f 758.0f 0.0f) (Bottom target.Bottom) (Effects.makeBoltEffect ()) screen world |> just
                | None -> just world

            | DisplayCycloneBlur (delay, targetIndex, radius) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 234.0f 234.0f 0.0f) (Center target.Center) (Effects.makeCycloneBlurEffect radius) screen world |> just
                | None -> just world

            | DisplayImpactSplash (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 192.0f 96.0f 0.0f) (Bottom target.Bottom) (Effects.makeImpactSplashEffect ()) screen world |> just
                | None -> just world

            | DisplayCut (delay, light, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 48.0f 144.0f 0.0f) (Bottom target.Bottom) (Effects.makeCutEffect light) screen world |> just
                | None -> just world
            
            | DisplaySlashSpike (delay, bottom, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target ->
                    let projection = Vector3.Normalize (target.Bottom - bottom) * single Constants.Render.VirtualResolutionX + target.Bottom
                    let effect = (Effects.makeSlashSpikeEffect bottom projection)
                    let world = displayEffect delay (v3 96.0f 96.0f 0.0f) (Bottom bottom) effect screen world
                    just world
                | None -> just world

            | DisplayArcaneCast (delay, sourceIndex) ->
                match Battle.tryGetCharacter sourceIndex battle with
                | Some source -> displayEffect delay (v3 300.0f 300.0f 0.0f) (Bottom (source.Bottom - v3 0.0f 120.0f 0.0f)) (Effects.makeArcaneCastEffect ()) screen world |> just
                | None -> just world
            
            | DisplayFire (delay, sourceIndex, targetIndex) ->
                match Battle.tryGetCharacter sourceIndex battle with
                | Some source ->
                    match Battle.tryGetCharacter targetIndex battle with
                    | Some target ->
                        let effect = Effects.makeFireEffect (source.Bottom + (v3 80.0f 80.0f 0.0f)) (target.Bottom + (v3 0.0f 20.0f 0.0f))
                        let world = displayEffect delay (v3 100.0f 100.0f 0.0f) (Bottom (source.Bottom - v3 0.0f 50.0f 0.0f)) effect screen world
                        just world
                    | None -> just world
                | None -> just world

            | DisplayFlame (delay, sourceIndex, targetIndex) ->
                match Battle.tryGetCharacter sourceIndex battle with
                | Some source ->
                    match Battle.tryGetCharacter targetIndex battle with
                    | Some target ->
                        let effect = Effects.makeFlameEffect source.CenterOffset target.CenterOffset
                        let world = displayEffect delay (v3 144.0f 144.0f 0.0f) (Bottom source.Bottom) effect screen world
                        just world
                    | None -> just world
                | None -> just world
            
            | DisplayIce (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 48.0f 48.0f 0.0f) (Bottom target.Bottom) (Effects.makeIceEffect ()) screen world |> just
                | None -> just world
            
            | DisplaySnowball (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 432.0f 432.0f 0.0f) (Bottom target.Bottom) (Effects.makeSnowballEffect ()) screen world |> just
                | None -> just world

            | DisplayHolyCast (delay, sourceIndex) ->
                match Battle.tryGetCharacter sourceIndex battle with
                | Some source -> displayEffect delay (v3 300.0f 300.0f 0.0f) (Bottom (source.Bottom - v3 0.0f 100.0f 0.0f)) (Effects.makeHolyCastEffect ()) screen world |> just
                | None -> just world
            
            | DisplayPurify (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 192.0f 192.0f 0.0f) (Bottom (target.Bottom - v3 0.0f 100.0f 0.0f)) (Effects.makePurifyEffect ()) screen world |> just
                | None -> just world

            | DisplayCure (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 48.0f 48.0f 0.0f) (Bottom target.Bottom) (Effects.makeCureEffect ()) screen world |> just
                | None -> just world
            
            | DisplayProtect (delay, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 48.0f 48.0f 0.0f) (Bottom target.Bottom) (Effects.makeProtectEffect ()) screen world |> just
                | None -> just world

            | DisplayDimensionalCast (delay, sourceIndex) ->
                match Battle.tryGetCharacter sourceIndex battle with
                | Some source -> displayEffect delay (v3 48.0f 48.0f 0.0f) (Bottom source.Bottom) (Effects.makeDimensionalCastEffect ()) screen world |> just
                | None -> just world

            | DisplayBuff (delay, statusType, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 48.0f 48.0f 0.0f) (Bottom target.Bottom) (Effects.makeBuffEffect statusType) screen world |> just
                | None -> just world

            | DisplayDebuff (delay, statusType, targetIndex) ->
                match Battle.tryGetCharacter targetIndex battle with
                | Some target -> displayEffect delay (v3 48.0f 48.0f 0.0f) (Bottom target.Bottom) (Effects.makeDebuffEffect statusType) screen world |> just
                | None -> just world

            | DisplayConjureIfrit delay ->
                displayEffect delay (v3 48.0f 48.0f 0.0f) (Position (v3 0.0f 0.0f 0.0f)) (Effects.makeConjureIfritEffect ()) screen world |> just

            | PlaySound (delay, volume, sound) ->
                let world = World.schedule (World.playSound volume sound) delay screen world
                just world

            | PlaySong (fadeIn, fadeOut, start, volume, assetTag) ->
                let world = World.playSong fadeIn fadeOut start volume assetTag world
                just world

            | FadeOutSong fade ->
                let world = World.fadeOutSong fade world
                just world

        override this.Content (battle, _) =

            [// scene group
             Content.group Simulants.BattleScene.Name []

                [// tile map
                 Content.tileMap "TileMap"
                    [Entity.Position == v3 -480.0f -270.0f 0.0f
                     Entity.Elevation == Constants.Battle.BackgroundElevation
                     Entity.TileMap := battle.TileMap
                     Entity.TileIndexOffset := battle.TileIndexOffset
                     Entity.TileIndexOffsetRange := battle.TileIndexOffsetRange]

                 // dialog
                 yield! Content.dialog "Dialog"
                    (Constants.Battle.GuiElevation + 2.0f) Nop Nop id
                    (match battle.DialogOpt with Some dialog -> Some dialog | None -> None)

                 // dialog interact button
                 Content.button "DialogInteract"
                    [Entity.Position == v3 248.0f -240.0f 0.0f; Entity.Elevation == Constants.Field.GuiElevation; Entity.Size == v3 144.0f 48.0f 0.0f
                     Entity.UpImage == Assets.Gui.ButtonShortUpImage; Entity.DownImage == Assets.Gui.ButtonShortDownImage
                     Entity.Visible := match battle.DialogOpt with Some dialog -> Dialog.canAdvance id dialog | None -> false
                     Entity.Text == "Next"
                     Entity.ClickEvent => InteractDialog]

                 // characters
                 for (index, character) in (Battle.getCharacters battle).Pairs do

                    // character
                    Content.entity<CharacterDispatcher> (CharacterIndex.toEntityName index) [Entity.Character := character]

                 // hud
                 for (index, character) in (Battle.getCharactersHudded battle).Pairs do

                    // bars
                    Content.composite (CharacterIndex.toEntityName index + "+Hud") []

                        [// health bar
                         Content.fillBar "HealthBar"
                            [Entity.MountOpt == None
                             Entity.Size == v3 48.0f 6.0f 0.0f
                             Entity.Center := character.BottomOriginalOffset
                             Entity.Elevation == Constants.Battle.GuiBackgroundElevation
                             Entity.Fill := single character.HitPoints / single character.HitPointsMax
                             Entity.FillColor := if character.Statuses.ContainsKey Poison then Color.LawnGreen.WithA 0.75f else Color.Red.WithA 0.75f
                             Entity.BorderImage == Assets.Battle.HealthBorderImage
                             Entity.BorderColor == Color.White]

                         // tech bar
                         if character.Ally then
                            Content.fillBar "TechBar"
                               [Entity.MountOpt == None
                                Entity.Size == v3 48.0f 6.0f 0.0f
                                Entity.Center := character.BottomOriginalOffset2
                                Entity.Elevation == Constants.Battle.GuiBackgroundElevation
                                Entity.Fill := single character.TechPoints / single character.TechPointsMax
                                Entity.FillColor == (color8 (byte 74) (byte 91) (byte 169) (byte 255)).WithA 0.75f
                                Entity.BorderImage == Assets.Battle.TechBorderImage
                                Entity.BorderColor == Color.White]]]

             // inputs condition
             if battle.Running then

                // inputs group
                Content.group Simulants.BattleInputs.Name []

                    [// inputs
                     for (index, ally) in (Battle.getCharactersHealthy battle).Pairs do

                        // input
                        Content.composite (CharacterIndex.toEntityName index + "+Input") []
                            [match ally.CharacterInputState with
                             | RegularMenu ->
                                Content.entity<RingMenuDispatcher> "RegularMenu"
                                    [Entity.Position := ally.CenterOffset
                                     Entity.Elevation == Constants.Battle.GuiElevation
                                     Entity.Enabled :=
                                        (let allies = battle |> Battle.getAllies |> Map.toValueList
                                         let alliesPastRegularMenu =
                                            Seq.notExists (fun (ally : Character) ->
                                                match ally.CharacterInputState with NoInput | RegularMenu -> false | _ -> true)
                                                allies
                                         alliesPastRegularMenu)
                                     Entity.RingMenu == { Items = Map.ofList [("Attack", (0, true)); ("Tech", (1, true)); ("Consumable", (2, true)); ("Defend", (3, true))]; Cancellable = false }
                                     Entity.ItemSelectEvent =|> fun evt -> RegularItemSelect (index, evt.Data) |> signal
                                     Entity.CancelEvent => RegularItemCancel index]
                             | ItemMenu ->
                                Content.entity<RingMenuDispatcher> "ConsumableMenu"
                                    [Entity.Position := ally.CenterOffset
                                     Entity.Elevation == Constants.Battle.GuiElevation
                                     Entity.RingMenu :=
                                        (let consumables =
                                            battle.Inventory |>
                                            Inventory.getConsumables |>
                                            Map.ofSeqBy (fun kvp -> (scstringm kvp.Key, (getTag kvp.Key, true)))
                                         { Items = consumables; Cancellable = true })
                                     Entity.ItemSelectEvent =|> fun evt -> ConsumableItemSelect (index, evt.Data) |> signal
                                     Entity.CancelEvent => ConsumableItemCancel index]
                             | TechMenu ->
                                Content.entity<RingMenuDispatcher> "TechMenu"
                                    [Entity.Position := ally.CenterOffset
                                     Entity.Elevation == Constants.Battle.GuiElevation
                                     Entity.RingMenu :=
                                        (let techs =
                                            ally.Techs |>
                                            Map.ofSeqBy (fun tech ->
                                                let affordable =
                                                    match Map.tryFind tech Data.Value.Techs with
                                                    | Some techData -> techData.TechCost <= ally.TechPoints && not (Map.containsKey Silence ally.Statuses)
                                                    | None -> false
                                                let castable =
                                                    if tech.ConjureTech then
                                                        match ally.ConjureChargeOpt with
                                                        | Some conjureCharge -> conjureCharge >= Constants.Battle.ChargeMax
                                                        | None -> true
                                                    else true
                                                let usable = affordable && castable
                                                (scstringm tech, (getTag tech, usable)))
                                         { Items = techs; Cancellable = true })
                                     Entity.ItemSelectEvent =|> fun evt -> TechItemSelect (index, evt.Data) |> signal
                                     Entity.CancelEvent => TechItemCancel index]
                             | AimReticles _ ->
                                Content.entity<ReticlesDispatcher> "Reticles"
                                    [Entity.Elevation == Constants.Battle.GuiElevation
                                     Entity.Reticles :=
                                        (let aimType =
                                            match Battle.tryGetCharacter index battle with
                                            | Some character -> character.CharacterInputState.AimType
                                            | None -> NoAim
                                         let characters = Battle.getTargets aimType battle
                                         let reticles =
                                            Map.map (fun _ (c : Character) ->
                                                match c.Stature with
                                                | BossStature -> c.CenterOffset2
                                                | _ -> c.CenterOffset)
                                                characters
                                         reticles)
                                     Entity.TargetSelectEvent =|> fun evt -> ReticlesSelect (index, evt.Data) |> signal
                                     Entity.CancelEvent => ReticlesCancel index]
                             | NoInput -> ()]]]